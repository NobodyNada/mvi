use crossbeam::atomic::AtomicCell;
use serde::Deserialize;
use std::{
    collections::{HashMap, HashSet},
    future::Future,
    io::Write,
    sync::Weak,
};
use thiserror::Error;

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(windows)] {
        const CORE_EXT: &str = "dll";
    } else if #[cfg(target_os = "macos")] {
        const CORE_EXT: &str = "dylib";
    } else if #[cfg(target_os = "linux")] {
        const CORE_EXT: &str = "so";
    } else {
        #[error("unsupported operating system")]
    }
}

/// The database of available cores.
#[derive(Debug, Deserialize)]
pub struct CoreDb {
    cores: Vec<CoreInfo>,
}

/// A core available from the libretro servers.
#[derive(Debug, Deserialize)]
pub struct CoreInfo {
    /// A unique string that identifies this core; the filename of the core (without extension).
    pub id: String,

    /// The user-visible name of this core.
    pub display_name: String,
    /// libretro's system identifier for this core. May not be present.
    pub system_id: Option<String>,
    /// libretro's system display name for this core.
    pub system_name: Option<String>,

    /// A list of file extensions supported by this core.
    pub supported_extensions: HashSet<String>,

    pub urls: HashMap<String, String>,
}

/// Errors that can occur while downloading cores or core info.
#[derive(Error, Debug)]
pub enum Error {
    #[error("I/O error")]
    Io(#[from] std::io::Error),

    #[error("HTTP request failed")]
    Http(#[from] reqwest::Error),

    #[error("HTTP response too large")]
    ResponseTooLarge(std::num::TryFromIntError),

    #[error("Could not parse core info file")]
    InvalidCoreInfo(#[from] serde_json::Error),

    #[error("No such core '{0}'")]
    NoSuchCore(String),

    #[error("The core '{id}' does not support your platform '{os}-{arch}'")]
    UnsupportedCore {
        id: String,
        os: &'static str,
        arch: &'static str,
    },
}

#[derive(Default, Clone, Copy, Debug)]
pub struct Progress {
    pub downloaded: usize,
    pub total: Option<usize>,
}

pub type Result<T> = std::result::Result<T, Error>;

impl CoreDb {
    const URL: &'static str = "https://mvi.nobodynada.com/cores.json";

    /// Returns the cores in the database.
    pub fn cores(&self) -> &[CoreInfo] {
        &self.cores
    }

    fn cores_dir() -> std::path::PathBuf {
        let mut path = dirs::data_local_dir().expect("no local data directory");
        path.push("mvi");
        path.push("cores");

        path
    }

    fn core_info_path() -> std::path::PathBuf {
        let mut path = Self::cores_dir();
        path.push("cores.json");
        path
    }

    pub fn find_installed_core_ids() -> Result<HashSet<String>> {
        let mut cores = HashSet::new();
        let ext = format!(".{CORE_EXT}");
        for file in std::fs::read_dir(Self::cores_dir())? {
            if let Some(name) = file?.file_name().to_str()
                && let Some(id) = name.strip_suffix(&ext) {
                    cores.insert(id.to_string());
                }
        }
        Ok(cores)
    }

    /// Loads a CoreDb from 'info.zip'.
    pub fn load_from_json(json: &[u8]) -> Result<CoreDb> {
        let mut db: CoreDb = serde_json::from_slice(json)?;
        db.cores.sort_by(|a, b| a.display_name.cmp(&b.display_name));
        Ok(db)
    }

    /// Loads a CoreDb from the cached 'info.zip'.
    pub fn load_cached() -> Result<CoreDb> {
        Self::load_from_json(&std::fs::read(Self::core_info_path())?)
    }

    /// Downloads a CoreDb from libretro servers.
    /// progress is a channel through which the asynchronous task exposes the download progress
    pub async fn download(progress: Weak<AtomicCell<Progress>>) -> Result<CoreDb> {
        let data = Self::download_with_progress(Self::URL, progress).await?;
        let db = Self::load_from_json(&data)?;
        let path = Self::core_info_path();
        std::fs::create_dir_all(path.parent().unwrap())?;
        std::fs::write(Self::core_info_path(), &data)?;
        Ok(db)
    }

    /// Downloads the given core ID from the libretro servers.
    /// - id: The core to download.
    /// - progress: A channel for reporting the download progress.
    /// - force: If true, the core will be downloaded even if a cached core is available. If false,
    ///   a cached core will be returned if present.
    pub fn download_core(
        &self,
        id: String,
        progress: Weak<AtomicCell<Progress>>,
        force: bool,
    ) -> impl Future<Output = Result<std::path::PathBuf>> + use<> {
        let url = self
            .cores
            .iter()
            .find(|c| c.id == id)
            .ok_or_else(|| Error::NoSuchCore(id.clone()))
            .and_then(|c| {
                c.urls
                    .get(&format!(
                        "{}-{}",
                        std::env::consts::OS,
                        std::env::consts::ARCH
                    ))
                    .ok_or_else(|| Error::UnsupportedCore {
                        id: id.clone(),
                        os: std::env::consts::OS,
                        arch: std::env::consts::ARCH,
                    })
            })
            .cloned();

        async move {
            let url = url?;

            let mut dst_path = Self::cores_dir();
            dst_path.push(format!("{id}.{CORE_EXT}"));

            if !force && dst_path.try_exists()? {
                return Ok(dst_path);
            }

            let data = Self::download_with_progress(&url, progress).await?;
            let mut dst = std::fs::File::create(&dst_path)?;
            dst.write_all(&data)?;

            Ok(dst_path)
        }
    }

    async fn download_with_progress(
        url: &str,
        progress: Weak<AtomicCell<Progress>>,
    ) -> Result<Vec<u8>> {
        let client = Self::client()?;
        let mut response = client.get(url).send().await?.error_for_status()?;
        let mut p = Progress {
            downloaded: 0,
            total: response
                .content_length()
                .map(|l| l.try_into().map_err(Error::ResponseTooLarge))
                .transpose()?,
        };
        if let Some(progress) = progress.upgrade() {
            progress.store(p)
        }

        let mut buf = Vec::new();
        while let Some(chunk) = response.chunk().await? {
            buf.extend_from_slice(&chunk);
            p.downloaded += chunk.len();
            if let Some(progress) = progress.upgrade() {
                progress.store(p)
            }
        }

        Ok(buf)
    }

    fn client() -> reqwest::Result<reqwest::Client> {
        // disable compression because the stuff is already compressed
        reqwest::ClientBuilder::new()
            .no_gzip()
            .no_deflate()
            .no_brotli()
            .build()
    }
}
