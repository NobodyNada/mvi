use std::rc::Rc;

use anyhow::Context;
use serde::{Deserialize, Serialize};

use crate::{
    core,
    tas::edit::{Pattern, PatternBuf},
};

use super::{
    greenzone::Greenzone,
    input::{self, InputPort},
};

pub mod file;

pub struct Movie {
    pub(super) greenzone: Greenzone,
    pub(super) input_ports: Vec<input::InputPort>,
    pub(super) data: Vec<u8>,
    pub(super) default_pattern: Rc<PatternBuf>,

    pub movie_path: Option<std::path::PathBuf>,
    pub rom_path: std::path::PathBuf,
    pub uuid: uuid::Uuid,
    pub core_id: String,
    pub system_id: Option<String>,
    pub rom_filename: String,
    pub rom_sha256: [u8; 32],
    pub rerecords: u32,
    pub ramwatches: Vec<RamWatch>,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct RamWatch {
    pub name: String,
    pub address: usize,
    pub format: RamWatchFormat,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct RamWatchFormat {
    pub width: u8,
    pub hex: bool,
    pub signed: bool,
}

impl RamWatchFormat {
    #[expect(clippy::collapsible_else_if)]
    pub fn format_value(&self, mut v: u64) -> String {
        if self.signed {
            // sign-extend
            if (v >> (self.width * 8 - 1)) & 1 == 1 {
                v |= !((1 << self.width) - 1)
            }
            let v = v as i64;
            if v < 0 {
                if self.hex {
                    format!("{v:0width$x}", width = self.width as usize * 2)
                } else {
                    format!("{v}")
                }
            } else {
                if self.hex {
                    format!(" {v:0width$x}", width = self.width as usize * 2)
                } else {
                    format!(" {v}")
                }
            }
        } else {
            if self.hex {
                format!("{v:0width$x}", width = self.width as usize * 2)
            } else {
                format!("{v}")
            }
        }
    }
}

impl Movie {
    pub fn new(
        input_ports: Vec<InputPort>,
        frame_0: core::Savestate,
        core_id: String,
        system_id: Option<String>,
        rom_path: std::path::PathBuf,
        rom_sha256: [u8; 32],
    ) -> Movie {
        let default_frame = InputPort::defaults(&input_ports);
        Movie {
            greenzone: Greenzone::new(frame_0),
            input_ports,
            data: default_frame.clone(),
            default_pattern: Rc::new(PatternBuf {
                data: default_frame.clone(),
                frame_size: default_frame.len(),
                mask: None,
                autofire: None,
                autohold: None,
            }),

            movie_path: None,
            uuid: uuid::Uuid::new_v4(),
            rom_filename: rom_path
                .file_name()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_default(),
            rom_path,
            rom_sha256,
            core_id,
            system_id,
            rerecords: 0,
            ramwatches: vec![],
        }
    }

    pub fn load(
        file: file::MovieFile,
        movie_path: std::path::PathBuf,
        frame_0: core::Savestate,
        core_id: String,
        rom_path: std::path::PathBuf,
        rom_sha256: [u8; 32],
    ) -> anyhow::Result<Movie> {
        let mut default_frame = Vec::new();
        for input in &file.input_devices {
            let offset = default_frame.len();
            let len = input.frame_size();
            default_frame.resize(offset + len, 0);
            input.default(&mut default_frame[offset..(offset + len)]);
        }

        Ok(Movie {
            greenzone: Greenzone::new(frame_0),
            data: file.decompress_inputs()?,
            input_ports: file.input_devices,
            default_pattern: Rc::new(PatternBuf {
                data: default_frame.clone(),
                frame_size: default_frame.len(),
                mask: None,
                autofire: None,
                autohold: None,
            }),

            movie_path: Some(movie_path),
            uuid: file.uuid,
            rom_filename: file.rom_filename,
            rom_path,
            rom_sha256,
            core_id,
            system_id: file.system_id,
            rerecords: file.rerecords,
            ramwatches: file.ramwatches,
        })
    }

    pub fn save(&self, path: &std::path::Path) -> anyhow::Result<()> {
        ciborium::into_writer(&file::MovieFile::new(self), std::fs::File::create(path)?)
            .context("Failed to save movie file")
    }

    pub fn frame(&self, idx: u32) -> &[u8] {
        if idx < self.len() {
            let size = self.frame_size();
            &self.data[idx as usize * size..][..size]
        } else {
            // If the frame is past the end of the movie, return a blank frame.
            self.default_frame()
        }
    }

    pub(super) fn frame_mut(&mut self, idx: u32) -> &mut [u8] {
        self.greenzone.invalidate(idx);
        self.ensure_length(idx + 1);
        let size = self.frame_size();
        &mut self.data[idx as usize * size..][..size]
    }

    /// Ensures the movie is at least 'len' frames long, inserting blank frames if needed.
    pub fn ensure_length(&mut self, len: u32) {
        if len > self.len() {
            let frames_to_insert = len - self.len();
            self.data.extend(
                std::iter::repeat_n(&self.default_pattern.data, frames_to_insert as usize)
                    .flatten(),
            )
        }
    }

    pub fn default_frame(&self) -> &[u8] {
        &self.default_pattern.data
    }

    pub fn default_pattern(&self) -> Pattern {
        Pattern {
            buf: self.default_pattern.clone(),
            offset: 0,
        }
    }

    pub fn greenzone(&self) -> &Greenzone {
        &self.greenzone
    }

    pub fn frame_size(&self) -> usize {
        self.default_frame().len()
    }

    pub fn len(&self) -> u32 {
        (self.data.len() / self.frame_size()) as u32
    }

    pub fn input_ports(&self) -> &[input::InputPort] {
        &self.input_ports
    }
}
