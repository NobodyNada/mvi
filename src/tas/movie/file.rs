use crate::tas;
use anyhow::Context;
use flate2::bufread::{ZlibDecoder, ZlibEncoder};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    io::{Read, Seek},
    path::PathBuf,
    sync::OnceLock,
};
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

    /// The system ID of the core this movie targets.
    /// At the time this field was added, mvi only supported SNES cores -- therefore, we can assume
    /// SNES if this value is not present.
    #[serde(default = "default_system_id")]
    pub system_id: Option<String>,

    /// The filename of the ROM this movie was created with. This is only used as a display name;
    /// the user can select any ROM file they want.
    pub rom_filename: String,

    /// The SHA-256 hash of the ROM file this movie was created with.
    pub rom_sha256: [u8; 32],

    /// The attached controllers.
    pub input_devices: Vec<tas::input::InputPort>,

    /// Extra libretro variables fed to the libretro core.
    /// Used for sync settings.
    #[serde(default)]
    pub environment_variables: HashMap<String, String>,

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
fn default_system_id() -> Option<String> {
    Some("super_nintendo".to_string())
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
            system_id: movie.system_id.clone(),
            rom_filename: movie.rom_filename.clone(),
            rom_sha256: movie.rom_sha256,
            input_devices: movie.input_ports.clone(),
            environment_variables: movie.environment_variables.clone(),
            rerecords: movie.rerecords,
            ramwatches: movie.ramwatches.clone(),
            inputs,
        }
    }

    pub fn load<R: Read>(reader: R) -> anyhow::Result<MovieFile> {
        ciborium::from_reader(reader).context("Failed to load movie file")
    }

    // ref: https://tasvideos.org/Bizhawk/BK2Format
    // Made to be fairly resistant to errors, and also as forward compatible
    // (should the format evolve in the future) as possible.
    pub fn load_bk2<R: Read + Seek>(reader: R) -> anyhow::Result<MovieFile> {
        use anyhow::bail;
        use std::io::BufRead;
        use std::io::BufReader;
        use zip::read::ZipArchive;

        let mut archive = ZipArchive::new(reader).context("failed to decompress bk2 archive")?;
        let mut buf = String::new();

        let mut core_id = String::new();
        let mut rerecords = 0;
        let mut rom_filename = String::new();
        {
            let header = archive
                .by_name("Header.txt")
                .context("missing Header.txt")?;
            let mut r = BufReader::new(header);

            loop {
                buf.clear();
                (&mut r).take(512).read_line(&mut buf)?;
                if buf.is_empty() {
                    break;
                }
                let Some(buf) = buf.strip_suffix('\n') else {
                    // Long header line, shouldn't happen (unless bizhawk adds
                    // another option with long values), skip line.
                    eprintln!("Skipping over long line in header");
                    r.skip_until(b'\n')?;
                    continue;
                };
                let Some((key, value)) = buf.split_once(' ') else {
                    // All known header lines follow a space-separated key value
                    // format. Skip line for forward compatibility.
                    eprintln!("Skipping over invalid line {buf:?} in header");
                    continue;
                };
                match key {
                    //"Core" => core_id = value.to_owned(), // TODO
                    "GameName" => rom_filename = value.to_owned(),
                    "Platform" => match value {
                        "SNES" => core_id = "bsnes_mvi".to_owned(),
                        _ => bail!("unsupported platform {value:?}"),
                    },
                    "rerecordCount" => rerecords = value.parse::<u32>().unwrap_or(0),
                    _ => {} // Skip over unknown settings
                }
            }
        }

        let input_devices;
        let mut environment_variables = HashMap::new();
        {
            #[derive(Deserialize)]
            struct SyncSettings {
                o: serde_json::Map<String, serde_json::Value>,
            }

            #[derive(Deserialize)]
            struct Bsnes {
                #[serde(default, rename = "LeftPort")]
                left_port: u8,
                #[serde(default, rename = "RightPort")]
                right_port: u8,
                #[serde(default, rename = "Entropy")]
                entropy: u8,
                #[serde(default, rename = "Hotfixes")]
                hotfixes: bool,
                #[serde(default, rename = "FastPPU")]
                fast_ppu: bool,
                #[serde(default, rename = "FastDSP")]
                fast_dsp: bool,
                #[serde(default, rename = "FastCoprocessors")]
                fast_coprocessors: bool,
                #[serde(default, rename = "UseSGB2")]
                use_sgb2: bool,
                #[serde(default, rename = "SatellaviewCartridge")]
                satellaview_cartridge: u8,
            }

            let sync_settings = archive
                .by_name("SyncSettings.json")
                .context("missing SyncSettings.json")?;

            let sync_settings: SyncSettings =
                serde_json::from_reader(sync_settings).context("SyncSettings has invalid JSON")?;
            let sync_settings = sync_settings.o;

            match sync_settings
                .get("$type")
                .context("missing SyncSettings.o.$type")?
                .as_str()
                .context("invalid SyncSettings.o.$type")?
            {
                "BizHawk.Emulation.Cores.Nintendo.BSNES.BsnesCore+SnesSyncSettings, BizHawk.Emulation.Cores" =>
                {
                    let bs: Bsnes =
                        Bsnes::deserialize(sync_settings).context("invalid bsnes sync settings")?;

                    const JOYPAD: tas::input::InputPort =
                        tas::input::InputPort::Joypad(tas::input::Joypad::Snes);
                    input_devices = match (bs.left_port, bs.right_port) {
                        (0, 0) => Vec::new(),
                        (1, 0) => vec![JOYPAD],
                        (1, 1) => vec![JOYPAD, JOYPAD],
                        (_, _) => bail!("unsupported bsnes joypad configuration"),
                    };

                    let entropy = match bs.entropy {
                        0 => "None",
                        1 => "Low",
                        2 => "High",
                        entropy => bail!("unsupported bsnes entropy {entropy}, expected 0, 1 or 2"),
                    };
                    environment_variables.insert("bsnes_entropy".to_owned(), entropy.to_owned());

                    fn bsnes_bool(b: bool) -> String {
                        let s = if b { "ON" } else { "OFF" };
                        s.to_owned()
                    }
                    environment_variables
                        .insert("bsnes_hotfixes".to_owned(), bsnes_bool(bs.hotfixes));
                    environment_variables
                        .insert("bsnes_ppu_fast".to_owned(), bsnes_bool(bs.fast_ppu));
                    environment_variables
                        .insert("bsnes_dsp_fast".to_owned(), bsnes_bool(bs.fast_dsp));
                    environment_variables.insert(
                        "bsnes_coprocessor_delayed_sync".to_owned(),
                        bsnes_bool(bs.fast_coprocessors),
                    );

                    let sgb = if bs.use_sgb2 { "SGB2.sfc" } else { "SGB1.sfc" };
                    environment_variables.insert("bsnes_sgb_bios".to_owned(), sgb.to_owned());
                }
                sstype => bail!("unsupported sync setting type {sstype:?}"),
            }
        }

        let mut raw_inputs = Vec::new();
        {
            fn bsnes115_1player(line: &[u8], data: &mut Vec<u8>) -> anyhow::Result<()> {
                if line.len() != 17 {
                    bail!("expected line length to be 17, got {}", line.len());
                }
                let offset = data.len();
                data.resize(offset + tas::input::Joypad::Snes.frame_size(), 0);
                let frame = &mut data[offset..];
                tas::input::Joypad::Snes.write(frame, 4, line[4] != b'.'); // up
                tas::input::Joypad::Snes.write(frame, 5, line[5] != b'.'); // down
                tas::input::Joypad::Snes.write(frame, 6, line[6] != b'.'); // left
                tas::input::Joypad::Snes.write(frame, 7, line[7] != b'.'); // right
                tas::input::Joypad::Snes.write(frame, 2, line[8] != b'.'); // select
                tas::input::Joypad::Snes.write(frame, 3, line[9] != b'.'); // start
                tas::input::Joypad::Snes.write(frame, 1, line[10] != b'.'); // y
                tas::input::Joypad::Snes.write(frame, 0, line[11] != b'.'); // b
                tas::input::Joypad::Snes.write(frame, 9, line[12] != b'.'); // x
                tas::input::Joypad::Snes.write(frame, 8, line[13] != b'.'); // a
                tas::input::Joypad::Snes.write(frame, 10, line[14] != b'.'); // l
                tas::input::Joypad::Snes.write(frame, 11, line[15] != b'.'); // r
                Ok(())
            }
            fn bsnes115_2players(line: &[u8], data: &mut Vec<u8>) -> anyhow::Result<()> {
                if line.len() != 30 {
                    bail!("expected line length to be 30, got {}", line.len());
                }
                let offset = data.len();
                data.resize(offset + 2 * tas::input::Joypad::Snes.frame_size(), 0);
                let frame = &mut data[offset..];
                tas::input::Joypad::Snes.write(frame, 4, line[4] != b'.'); // up
                tas::input::Joypad::Snes.write(frame, 5, line[5] != b'.'); // down
                tas::input::Joypad::Snes.write(frame, 6, line[6] != b'.'); // left
                tas::input::Joypad::Snes.write(frame, 7, line[7] != b'.'); // right
                tas::input::Joypad::Snes.write(frame, 2, line[8] != b'.'); // select
                tas::input::Joypad::Snes.write(frame, 3, line[9] != b'.'); // start
                tas::input::Joypad::Snes.write(frame, 1, line[10] != b'.'); // y
                tas::input::Joypad::Snes.write(frame, 0, line[11] != b'.'); // b
                tas::input::Joypad::Snes.write(frame, 9, line[12] != b'.'); // x
                tas::input::Joypad::Snes.write(frame, 8, line[13] != b'.'); // a
                tas::input::Joypad::Snes.write(frame, 10, line[14] != b'.'); // l
                tas::input::Joypad::Snes.write(frame, 11, line[15] != b'.'); // r
                // TODO add player2 inputs
                Ok(())
            }

            let parse_line = match (&*core_id, input_devices.len()) {
                ("bsnes_mvi", 1) => bsnes115_1player,
                ("bsnes_mvi", 2) => bsnes115_2players,
                (core_id, input_devices_len) => bail!(
                    "unsupported core/input port configuration {core_id:?}/{input_devices_len}"
                ),
            };

            let input_log = archive
                .by_name("Input Log.txt")
                .context("missing Input Log.txt")?;
            let mut r = BufReader::new(input_log);

            // ref: https://github.com/TASEmulators/BizHawk/blob/9b4bc1e693715235fff9507f99b93c87065ed653/src/BizHawk.Client.Common/movie/bk2/Bk2Movie.InputLog.cs
            // ref: https://github.com/TASEmulators/BizHawk/blob/9b4bc1e693715235fff9507f99b93c87065ed653/src/BizHawk.Client.Common/movie/bk2/Bk2Controller.cs
            for lineno in 1_u64.. {
                buf.clear();
                (&mut r).take(512).read_line(&mut buf)?;
                if buf.is_empty() {
                    break;
                }
                if !buf.starts_with('|') {
                    // Bizhawk also ignores those lines.
                    continue;
                }
                let Some(buf) = buf.strip_suffix('\n') else {
                    // Long line, shouldn't happen in practice
                    r.skip_until(b'\n')?;
                    continue;
                };
                if let Err(err) = parse_line(buf.as_bytes(), &mut raw_inputs) {
                    eprintln!("error in Input Log at line #{lineno}: {err:#}");
                }
            }
        }

        let mut inputs = Vec::new();
        ZlibEncoder::new(raw_inputs.as_slice(), flate2::Compression::fast())
            .read_to_end(&mut inputs)
            .expect("compression failed");

        Ok(Self {
            uuid: Uuid::new_v4(), // bk2 doesn't have an equivalent identifier
            core_id,
            rom_filename,
            rom_sha256: [0; 32],
            input_devices,
            system_id: None, // TODO
            environment_variables,
            rerecords,
            ramwatches: Vec::new(),
            inputs,
        })
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
