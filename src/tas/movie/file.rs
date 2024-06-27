use crate::tas;
use anyhow::Context;
use flate2::bufread::{ZlibDecoder, ZlibEncoder};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, io::Read, path::PathBuf, sync::OnceLock};
use uuid::Uuid;

use super::Movie;

/// A serialized movie on disk.
///
/// A movie is stored by encoding this struct in CBOR format: https://cbor.io/
#[derive(Serialize, Deserialize)]
pub struct MovieFile {
    /// A unique identifier for this movie. This is used to cache local configuration -- such as
    /// the path to the ROM file -- that should remain the same as the user edits the movie,
    /// creates alternative versions, and collaborates with coauthors.
    pub uuid: Uuid,

    /// The core ID of the core this movie was created with.
    pub core_id: String,

    /// The filename of the ROM this movie was created with. This is only used as a display name;
    /// the user can select any ROM file they want.
    pub rom_filename: String,

    /// The SHA-256 hash of the ROM file this movie was created with.
    pub rom_sha256: [u8; 32],

    /// The attached controllers.
    pub input_devices: Vec<tas::input::InputPort>,

    /// The number of rerecords.
    #[serde(default)]
    pub rerecords: u32,

    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    /// The configured memory watches.
    pub ramwatches: Vec<super::RamWatch>,

    /// The input frames, compressed with zlib. The uncompressed size of each frame will depend on
    /// the selected input_devices.
    pub inputs: Vec<u8>,
}

impl MovieFile {
    pub fn new(movie: &Movie) -> MovieFile {
        let mut inputs = Vec::new();
        ZlibEncoder::new(movie.data.as_slice(), flate2::Compression::best())
            .read_to_end(&mut inputs)
            .expect("compression failed");
        MovieFile {
            uuid: movie.uuid,
            core_id: movie.core_id.clone(),
            rom_filename: movie.rom_filename.clone(),
            rom_sha256: movie.rom_sha256,
            input_devices: movie.input_ports.clone(),
            rerecords: movie.rerecords,
            ramwatches: movie.ramwatches.clone(),
            inputs,
        }
    }

    pub fn load<R: Read>(reader: R) -> anyhow::Result<MovieFile> {
        ciborium::from_reader(reader).context("Failed to load movie file")
    }

    pub fn decompress_inputs(&self) -> anyhow::Result<Vec<u8>> {
        let mut inputs = Vec::new();
        ZlibDecoder::new(self.inputs.as_slice()).read_to_end(&mut inputs)?;
        Ok(inputs)
    }
}

/// Stores cached information about recently edited movies.
#[derive(Serialize, Deserialize)]
pub struct MovieCache {
    /// The most recently edited movies, ordered from most to least recent
    recents: Vec<PathBuf>,

    /// The paths to the ROM files used by each movie.
    rom_paths: HashMap<Uuid, PathBuf>,
}

static PATH: OnceLock<PathBuf> = OnceLock::new();
impl MovieCache {
    fn path() -> &'static PathBuf {
        PATH.get_or_init(|| {
            let mut path = dirs::data_local_dir().unwrap();
            path.push("mvi");
            path.push("recents.json");
            path
        })
    }

    pub fn recents(&self) -> &[PathBuf] {
        &self.recents
    }

    pub fn rom_path_for_uuid(&self, uuid: Uuid) -> Option<&std::path::Path> {
        self.rom_paths.get(&uuid).map(PathBuf::as_path)
    }

    /// Loads the movie cache from disk, or returns an empty path if it could not be loaded.
    pub fn load() -> Self {
        let Ok(file) = std::fs::File::open(Self::path()) else {
            return Self::new();
        };
        let Ok(mut cache) = serde_json::from_reader::<_, MovieCache>(file) else {
            return Self::new();
        };
        // Remove recent movies that don't exist anymore.
        cache.recents.retain(|path| path.exists());
        cache
    }
    fn new() -> MovieCache {
        MovieCache {
            recents: Default::default(),
            rom_paths: Default::default(),
        }
    }

    pub fn update(
        &mut self,
        movie_path: PathBuf,
        rom_path: PathBuf,
        uuid: Uuid,
    ) -> anyhow::Result<()> {
        if let Some((i, _)) = self
            .recents
            .iter()
            .enumerate()
            .find(|(_, path)| **path == movie_path)
        {
            // If the movie is already in the recents, move it to the front.
            self.recents[0..=i].rotate_right(1);
        } else {
            // Insert the movie at the beginning of recents, keeping only 10 elements.
            self.recents.truncate(9);
            self.recents.insert(0, movie_path);
        }

        self.rom_paths.insert(uuid, rom_path);

        let path = Self::path();
        std::fs::create_dir_all(path.parent().unwrap())?;
        serde_json::to_writer(std::fs::File::create(path)?, self)
            .context("Failed to save recent movies")
    }
}
