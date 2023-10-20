use std::collections::BTreeMap;

use crate::core::Savestate;

pub struct Greenzone {
    states: BTreeMap<u32, Savestate>,
}

impl Greenzone {
    pub fn new(frame_0: Savestate) -> Greenzone {
        Greenzone {
            states: [(0, frame_0)].into_iter().collect(),
        }
    }

    pub fn invalidate(&mut self, after: u32) {
        self.states.retain(|f, _| *f <= after);
    }

    pub fn save(&mut self, frame: u32, state: Savestate) {
        self.states.insert(frame, state);
    }

    pub fn restore(&self, frame: u32) -> (u32, &Savestate) {
        let (&frame, state) = self
            .states
            .range(0..=frame)
            .next_back()
            .expect("should always have a power-on state");
        (frame, state)
    }

    pub fn states(&self) -> &BTreeMap<u32, Savestate> {
        &self.states
    }
}
