pub enum InputPort {
    Joypad(Joypad),
}

impl InputPort {
    /// The number of controllers attached to this input port.
    pub fn num_controllers(&self) -> u32 {
        match self {
            Self::Joypad(_) => 1,
        }
    }

    /// The number of bytes of memory needed to store one frames' worth of input data.
    pub fn frame_size(&self) -> usize {
        match self {
            Self::Joypad(j) => j.frame_size(),
        }
    }

    pub fn read(&self, data: &[u8], index: u32, id: u32) -> i16 {
        match self {
            Self::Joypad(j) => {
                assert_eq!(index, 0);
                j.read(data, id) as i16
            }
        }
    }

    pub fn write(&self, data: &mut [u8], index: u32, id: u32, value: i16) {
        match self {
            Self::Joypad(j) => {
                assert_eq!(index, 0);
                j.write(data, id, value != 0)
            }
        }
    }

    /// Constructs a default, "empty" frame of input.
    pub fn default(&self, buf: &mut [u8]) {
        assert_eq!(buf.len(), self.frame_size());
        match self {
            Self::Joypad(j) => j.default(buf),
        }
    }
}

pub enum Joypad {
    Snes,
}

impl Joypad {
    pub fn buttons(&self) -> &[&str] {
        match self {
            Joypad::Snes => &["B", "Y", "s", "S", "↑", "↓", "←", "→", "A", "X", "L", "R"],
        }
    }

    pub fn frame_size(&self) -> usize {
        (self.buttons().len() + 7) / 8
    }

    pub fn read(&self, data: &[u8], id: u32) -> bool {
        assert!((id as usize) < self.buttons().len());
        match self {
            Joypad::Snes => ((data[id as usize / 8] >> (id as usize % 8)) & 1) != 0,
        }
    }

    pub fn write(&self, data: &mut [u8], id: u32, value: bool) {
        assert!((id as usize) < self.buttons().len());
        match self {
            Joypad::Snes => data[id as usize / 8] |= (value as u8) << (id as usize % 8),
        }
    }

    pub fn default(&self, buf: &mut [u8]) {
        assert_eq!(buf.len(), self.frame_size());
        buf.fill(0)
    }
}
