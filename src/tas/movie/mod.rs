use anyhow::Context;

use crate::core;

use super::{greenzone::Greenzone, input};

pub mod file;

pub struct Movie {
    pub(super) greenzone: Greenzone,
    pub(super) input_ports: Vec<input::InputPort>,
    pub(super) data: Vec<u8>,
    default_frame: Vec<u8>,

    pub movie_path: Option<std::path::PathBuf>,
    pub rom_path: std::path::PathBuf,
    pub uuid: uuid::Uuid,
    pub core_id: String,
    pub rom_filename: String,
    pub rom_sha256: [u8; 32],
}

#[derive(Clone, Debug)]
pub struct Pattern {
    pub data: Vec<u8>,
}

impl Movie {
    pub fn new(
        input_ports: Vec<input::InputPort>,
        frame_0: core::Savestate,
        core_id: String,
        rom_path: std::path::PathBuf,
        rom_sha256: [u8; 32],
    ) -> Movie {
        let mut default_frame = Vec::new();
        for input in &input_ports {
            let offset = default_frame.len();
            let len = input.frame_size();
            default_frame.resize(offset + len, 0);
            input.default(&mut default_frame[offset..(offset + len)]);
        }

        Movie {
            greenzone: Greenzone::new(frame_0),
            input_ports,
            data: default_frame.clone(),
            default_frame,

            movie_path: None,
            uuid: uuid::Uuid::new_v4(),
            rom_filename: rom_path
                .file_name()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_default(),
            rom_path,
            rom_sha256,
            core_id,
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
            default_frame,

            movie_path: Some(movie_path),
            uuid: file.uuid,
            rom_filename: file.rom_filename,
            rom_path,
            rom_sha256,
            core_id,
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
                std::iter::repeat(&self.default_frame)
                    .take(frames_to_insert as usize)
                    .flatten(),
            )
        }
    }

    pub fn default_frame(&self) -> &[u8] {
        &self.default_frame
    }

    pub fn greenzone(&self) -> &Greenzone {
        &self.greenzone
    }

    pub fn frame_size(&self) -> usize {
        self.default_frame.len()
    }

    pub fn len(&self) -> u32 {
        (self.data.len() / self.frame_size()) as u32
    }

    pub fn input_ports(&self) -> &[input::InputPort] {
        &self.input_ports
    }
}

impl Pattern {
    pub fn read(&self, input_ports: &[input::InputPort], port: u32, index: u32, id: u32) -> i16 {
        let port = port as usize;
        assert!(port < input_ports.len());
        let offset: usize = input_ports[0..port]
            .iter()
            .map(crate::tas::input::InputPort::frame_size)
            .sum();
        let len = input_ports[port].frame_size();

        input_ports[port].read(&self.data[offset..(offset + len)], index, id)
    }

    pub fn write(
        &mut self,
        input_ports: &[input::InputPort],
        port: u32,
        index: u32,
        id: u32,
        value: i16,
    ) {
        let port = port as usize;
        assert!(port < input_ports.len());
        let offset: usize = input_ports[0..port]
            .iter()
            .map(crate::tas::input::InputPort::frame_size)
            .sum();
        let len = input_ports[port].frame_size();

        input_ports[port].write(&mut self.data[offset..(offset + len)], index, id, value)
    }
}
