use crossbeam::atomic::AtomicCell;
use std::{collections::HashSet, sync::Weak};
use thiserror::Error;

use cfg_if::cfg_if;

cfg_if! {
    if #[cfg(windows)] {
        const CORE_EXT: &str = "dll";
        if #[cfg(target_arch = "x86")] {
            const CORE_URL: &str = "https://buildbot.libretro.com/nightly/windows/x86/latest";
        } else if #[cfg(target_arch = "x86_64")] {
            const CORE_URL: &str = "https://buildbot.libretro.com/nightly/windows/x86_64/latest";
        } else {
            #[error("unsupported architecture")]
        }
    } else if #[cfg(target_os = "macos")] {
        const CORE_EXT: &str = "dylib";
        cfg_if! {
            if #[cfg(target_arch = "x86_64")] {
                const CORE_URL: &str = "https://buildbot.libretro.com/nightly/apple/osx/x86_64/latest";
            } else if #[cfg(target_arch = "aarch64")] {
                const CORE_URL: &str = "https://buildbot.libretro.com/nightly/apple/osx/arm64/latest";
            } else {
                #[error("unsupported architecture")]
            }
        }
    } else if #[cfg(target_os = "linux")] {
            if #[cfg(target_arch = "x86_64")] {
                const CORE_URL: &str = "https://buildbot.libretro.com/nightly/linux/x86_64/latest";
            } else {
                #[error("unsupported architecture")]
            }
    } else {
        #[error("unsupported operating system")]
    }
}

/// The database of available cores.
#[derive(Debug)]
pub struct CoreDb {
    cores: Vec<CoreInfo>,
}

/// A core available from the libretro servers.
#[derive(Debug)]
pub struct CoreInfo {
    /// A unique string that identifies this core; the filename of the core (without extension).
    pub id: String,

    /// The user-visible name of this core.
    pub display_name: String,
    /// libretro's system identifier for this core. May not be present.
    pub system_id: Option<String>,
    /// libretro's system display name for this core.
    pub system_name: Option<String>,

    /// If true, the libretro core is marked as experimental/unstable.
    pub experimental: bool,

    /// A list of file extensions supported by this core.
    pub supported_extensions: HashSet<String>,
}

/// Errors that can occur while downloading cores or core info.
#[derive(Error, Debug)]
pub enum Error {
    #[error("I/O error")]
    IoError(#[from] std::io::Error),

    #[error("HTTP request failed")]
    HttpError(#[from] reqwest::Error),

    #[error("HTTP response too large")]
    ResponseTooLarge(std::num::TryFromIntError),

    #[error("ZIP error")]
    ZipError(#[from] zip::result::ZipError),

    #[error("Could not parse core info file {1}")]
    IniError(#[source] ini::Error, String),

    #[error("Could not parse core info file {0}: invalid field {1})")]
    InvalidCoreInfo(String, &'static str),
}

#[derive(Default, Clone, Copy, Debug)]
pub struct Progress {
    pub downloaded: usize,
    pub total: Option<usize>,
}

pub type Result<T> = std::result::Result<T, Error>;

impl CoreDb {
    const URL: &str = "https://buildbot.libretro.com/assets/frontend/info.zip";

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
        path.push("info.zip");
        path
    }

    pub fn find_installed_core_ids() -> Result<HashSet<String>> {
        let mut cores = HashSet::new();
        let ext = format!(".{CORE_EXT}");
        for file in std::fs::read_dir(Self::cores_dir())? {
            if let Some(name) = file?.file_name().to_str() {
                if let Some(id) = name.strip_suffix(&ext) {
                    cores.insert(id.to_string());
                }
            }
        }
        Ok(cores)
    }

    /// Loads a CoreDb from 'info.zip'.
    pub fn load_from_zip(data: &[u8]) -> Result<CoreDb> {
        let mut zip = zip::ZipArchive::new(std::io::Cursor::new(data))?;

        let mut cores = Vec::new();
        for i in 0..zip.len() {
            let mut file = zip.by_index(i)?;
            if !file.name().ends_with(".info") {
                // skip things that aren't core info
                continue;
            }
            if file.name() == "00_example_libretro.info"
                || file.name() == "open-source-notices.info"
            {
                // skip the example core info file, since it's not a real core and it's not parseable
                // also skip the open source notices file, which for some reason has the same
                // extension as a core info file???
                // I am curious to know why someone thought this was reasonable or sane
                continue;
            }
            let filename = file.name().to_string();
            let core = CoreInfo::parse(&mut file, filename)?;
            cores.push(core);
        }
        cores.sort_by(|a, b| a.display_name.cmp(&b.display_name));
        Ok(CoreDb { cores })
    }

    /// Loads a CoreDb from the cached 'info.zip'.
    pub fn load_cached() -> Result<CoreDb> {
        Self::load_from_zip(&std::fs::read(Self::core_info_path())?)
    }

    /// Downloads a CoreDb from libretro servers.
    /// progress is a channel through which the asynchronous task exposes the download progress
    pub async fn download(progress: Weak<AtomicCell<Progress>>) -> Result<CoreDb> {
        let data = Self::download_with_progress(Self::URL, progress).await?;
        let db = Self::load_from_zip(&data)?;
        let path = Self::core_info_path();
        std::fs::create_dir_all(path.parent().unwrap())?;
        std::fs::write(Self::core_info_path(), &data)?;
        Ok(db)
    }

    /// Downloads the given core ID from the libretro servers.
    /// - id: The core to download.
    /// - progress: A channel for reporting the download progress.
    /// - force: If true, the core will be downloaded even if a cached core is available. If false,
    /// a cached core will be returned if present.
    pub async fn download_core(
        id: String,
        progress: Weak<AtomicCell<Progress>>,
        force: bool,
    ) -> Result<std::path::PathBuf> {
        let mut dst_path = Self::cores_dir();
        dst_path.push(format!("{id}.{CORE_EXT}"));

        if !force && dst_path.try_exists()? {
            return Ok(dst_path);
        }

        let url = format!("{CORE_URL}/{id}.{CORE_EXT}.zip");
        let data = Self::download_with_progress(&url, progress).await?;

        let mut zip = zip::ZipArchive::new(std::io::Cursor::new(data))?;
        let mut file = zip.by_name(&format!("{id}.{CORE_EXT}"))?;

        std::io::copy(&mut file, &mut std::fs::File::create(&dst_path)?)?;

        Ok(dst_path)
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
        progress.upgrade().map(|progress| progress.store(p));

        let mut buf = Vec::new();
        while let Some(chunk) = response.chunk().await? {
            buf.extend_from_slice(&chunk);
            p.downloaded += chunk.len();
            progress.upgrade().map(|progress| progress.store(p));
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

impl CoreInfo {
    fn parse<R: std::io::Read>(mut r: R, filename: String) -> Result<CoreInfo> {
        let ini =
            ini::Ini::read_from(&mut r).map_err(|e| Error::IniError(e, filename.to_string()))?;

        let section = ini.general_section();
        let require = |p: &'static str| {
            section
                .get(p)
                .ok_or_else(|| Error::InvalidCoreInfo(filename.clone(), p))
        };

        Ok(CoreInfo {
            id: filename
                .strip_suffix(".info")
                .unwrap_or(&filename)
                .to_string(),
            display_name: require("display_name")?.to_string(),
            // some cores don't have a system ID, for some reason
            system_id: section.get("systemid").map(str::to_string),
            system_name: section.get("systemname").map(str::to_string),
            supported_extensions: section
                .get("supported_extensions")
                .filter(|e| !e.is_empty())
                .map(|e| e.split('|').map(str::to_string).collect())
                .unwrap_or_default(),
            experimental: match section.get("is_experimental") {
                Some("false") | None => false,
                Some("true") => true,
                Some(_) => return Err(Error::InvalidCoreInfo(filename.clone(), "is_experimental")),
            },
        })
    }
}
