use std::rc::Rc;

use anyhow::Context;

use crate::core;

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
    pub rom_filename: String,
    pub rom_sha256: [u8; 32],
}

#[derive(Clone, Debug)]
pub struct PatternBuf {
    pub data: Vec<u8>,
    pub frame_size: usize,
    pub mask: Option<Vec<bool>>,
    pub autofire: Option<Vec<bool>>,
    pub autohold: Option<Vec<bool>>,
}

impl PatternBuf {
    pub fn len(&self) -> usize {
        self.data.len() / self.frame_size
    }
}

#[derive(Clone, Debug)]
pub struct Pattern {
    pub buf: Rc<PatternBuf>,
    pub offset: usize,
}

impl Movie {
    pub fn new(
        input_ports: Vec<InputPort>,
        frame_0: core::Savestate,
        core_id: String,
        rom_path: std::path::PathBuf,
        rom_sha256: [u8; 32],
    ) -> Movie {
        let mut default_frame = InputPort::defaults(&input_ports);
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
                std::iter::repeat(&self.default_pattern.data)
                    .take(frames_to_insert as usize)
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

impl Pattern {
    pub fn read(&self, input_ports: &[input::InputPort], port: u32, index: u32, id: u32) -> i16 {
        let port = port as usize;
        assert!(port < input_ports.len());
        let offset: usize = input_ports[0..port]
            .iter()
            .map(crate::tas::input::InputPort::frame_size)
            .sum();
        let offset = offset + (self.offset * self.buf.frame_size) % self.buf.len();
        let len = input_ports[port].frame_size();

        input_ports[port].read(&self.buf.data[offset..(offset + len)], index, id)
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
        let offset = offset + (self.offset * self.buf.frame_size) % self.buf.len();
        let len = input_ports[port].frame_size();

        input_ports[port].write(
            &mut Rc::make_mut(&mut self.buf).data[offset..(offset + len)],
            index,
            id,
            value,
        )
    }

    pub fn apply(&self, input_ports: &[InputPort], dst: &mut [u8]) -> Pattern {
        assert_eq!(dst.len() % self.buf.frame_size, 0);

        let mut offset = self.offset;
        for dst_frame in dst.chunks_exact_mut(self.buf.frame_size) {
            let src_frame = &self.buf.data
                [((offset * self.buf.frame_size) % self.buf.data.len())..][..self.buf.frame_size];
            let autofire_value = offset % (2 * self.buf.len()) < self.buf.len();

            let mut i = 0;
            let mut device_offset = 0;
            for device in input_ports {
                let src = &src_frame[device_offset..(device_offset + device.frame_size())];
                let dst = &mut dst_frame[device_offset..(device_offset + device.frame_size())];
                for index in 0..device.num_controllers() {
                    for id in 0..device.num_inputs(index) {
                        if self.buf.mask.as_ref().map(|mask| mask[i]).unwrap_or(true) {
                            let autofire_value = autofire_value
                                & self.buf.autofire.as_ref().map(|af| af[i]).unwrap_or(false);
                            device.write(
                                dst,
                                index,
                                id,
                                device.read(src, index, id) ^ (autofire_value as i16),
                            );
                        }
                        i += 1;
                    }
                }
                device_offset += device.frame_size();
            }

            offset += 1;
        }

        Pattern {
            buf: self.buf.clone(),
            offset,
        }
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn mask(&self, input_ports: &[InputPort], port: u32, index: u32, id: u32) -> bool {
        self.buf
            .mask
            .as_ref()
            .map(|mask| mask[Self::input_index(input_ports, port, index, id)])
            .unwrap_or(true)
    }

    pub fn autofire(&self, input_ports: &[InputPort], port: u32, index: u32, id: u32) -> bool {
        self.buf
            .autofire
            .as_ref()
            .map(|af| af[Self::input_index(input_ports, port, index, id)])
            .unwrap_or(false)
    }

    pub fn autohold(&self, input_ports: &[InputPort], port: u32, index: u32, id: u32) -> bool {
        self.buf
            .autohold
            .as_ref()
            .map(|ah| ah[Self::input_index(input_ports, port, index, id)])
            .unwrap_or(false)
    }

    pub fn mask_mut(
        &mut self,
        input_ports: &[InputPort],
        port: u32,
        index: u32,
        id: u32,
    ) -> &mut bool {
        let mask = Rc::make_mut(&mut self.buf).mask.get_or_insert_with(|| {
            std::iter::repeat(true)
                .take(Self::input_count(input_ports))
                .collect()
        });
        &mut mask[Self::input_index(input_ports, port, index, id)]
    }

    pub fn autofire_mut(
        &mut self,
        input_ports: &[InputPort],
        port: u32,
        index: u32,
        id: u32,
    ) -> &mut bool {
        let af = Rc::make_mut(&mut self.buf).autofire.get_or_insert_with(|| {
            std::iter::repeat(false)
                .take(Self::input_count(input_ports))
                .collect()
        });
        &mut af[Self::input_index(input_ports, port, index, id)]
    }

    pub fn autohold_mut(
        &mut self,
        input_ports: &[InputPort],
        port: u32,
        index: u32,
        id: u32,
    ) -> &mut bool {
        let ah = Rc::make_mut(&mut self.buf).autohold.get_or_insert_with(|| {
            std::iter::repeat(false)
                .take(Self::input_count(input_ports))
                .collect()
        });
        &mut ah[Self::input_index(input_ports, port, index, id)]
    }

    fn input_index(input_ports: &[InputPort], port: u32, index: u32, id: u32) -> usize {
        input_ports[0..port as usize]
            .iter()
            .map(InputPort::total_inputs)
            .sum::<u32>() as usize
            + (0..index)
                .map(|i| input_ports[port as usize].num_inputs(i))
                .sum::<u32>() as usize
            + id as usize
    }

    fn input_count(input_ports: &[InputPort]) -> usize {
        input_ports.iter().map(InputPort::total_inputs).sum::<u32>() as usize
    }
}
