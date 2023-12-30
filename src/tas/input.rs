use serde::{Deserialize, Serialize};

#[derive(Copy, Clone, Debug, Serialize, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum InputPort {
    Joypad(Joypad),
}

impl InputPort {
    /// Returns an iterator over all supported input devices.
    pub fn all() -> impl Iterator<Item = InputPort> {
        Joypad::all().map(InputPort::Joypad)
    }

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
        assert!(index < self.num_controllers());
        match self {
            Self::Joypad(j) => j.read(data, id) as i16,
        }
    }

    pub fn write(&self, data: &mut [u8], index: u32, id: u32, value: i16) {
        assert!(index < self.num_controllers());
        match self {
            Self::Joypad(j) => j.write(data, id, value != 0),
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

impl std::fmt::Display for InputPort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Joypad(j) => write!(f, "{j} joypad"),
        }
    }
}

#[derive(Copy, Clone, Debug, Serialize, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Joypad {
    Snes,
}

impl Joypad {
    /// Returns an iterator over all support joypads.
    pub fn all() -> impl Iterator<Item = Joypad> {
        [Joypad::Snes].into_iter()
    }

    pub fn buttons(&self) -> &'static [&'static str] {
        match self {
            Joypad::Snes => &["B", "Y", "s", "S", "↑", "↓", "←", "→", "A", "X", "L", "R"],
        }
    }

    pub fn frame_size(&self) -> usize {
        (self.buttons().len() + 7) / 8
    }

    pub fn read(&self, data: &[u8], id: u32) -> bool {
        if (id as usize) >= self.buttons().len() {
            return false;
        }
        match self {
            Joypad::Snes => ((data[id as usize / 8] >> (id as usize % 8)) & 1) != 0,
        }
    }

    pub fn write(&self, data: &mut [u8], id: u32, value: bool) {
        assert!((id as usize) < self.buttons().len());
        match self {
            Joypad::Snes => {
                let byte = &mut data[id as usize / 8];
                let mask = 1 << (id as usize % 8);
                if value {
                    *byte |= mask;
                } else {
                    *byte &= !mask;
                }
            }
        }
    }

    pub fn default(&self, buf: &mut [u8]) {
        assert_eq!(buf.len(), self.frame_size());
        buf.fill(0)
    }
}

impl std::fmt::Display for Joypad {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Joypad::Snes => "SNES",
            }
        )
    }
}
