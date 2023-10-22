use crate::core;

use super::{greenzone::Greenzone, input};

pub struct Movie {
    pub(super) greenzone: Greenzone,
    pub(super) input_port: input::InputPort,
    pub(super) data: Vec<u8>,
    default_frame: Vec<u8>,
}

impl Movie {
    pub fn new(input_port: input::InputPort, frame_0: core::Savestate) -> Movie {
        let mut default_frame = Vec::new();
        default_frame.resize(input_port.frame_size(), 0);
        input_port.default(&mut default_frame);

        Movie {
            greenzone: Greenzone::new(frame_0),
            input_port,
            data: default_frame.clone(),
            default_frame,
        }
    }

    pub fn frame(&self, idx: u32) -> &[u8] {
        if idx < self.len() {
            let size = self.input_port.frame_size();
            &self.data[idx as usize * size..][..size]
        } else {
            // If the frame is past the end of the movie, return a blank frame.
            self.default_frame()
        }
    }

    pub(super) fn frame_mut(&mut self, idx: u32) -> &mut [u8] {
        self.greenzone.invalidate(idx);
        self.ensure_length(idx + 1);
        let size = self.input_port.frame_size();
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
        self.input_port.frame_size()
    }

    pub fn len(&self) -> u32 {
        (self.data.len() / self.input_port.frame_size()) as u32
    }

    pub fn input_port(&self) -> input::InputPort {
        self.input_port
    }
}
