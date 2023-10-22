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
        let size = self.input_port.frame_size();
        &self.data[idx as usize * size..][..size]
    }

    pub(super) fn frame_mut(&mut self, idx: u32) -> &mut [u8] {
        self.greenzone.invalidate(idx);
        let size = self.input_port.frame_size();
        &mut self.data[idx as usize * size..][..size]
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
