use std::collections::BTreeMap;

use crate::core::{DeltaSavestate, Savestate, SavestateRef};

pub struct Greenzone {
    full_states: BTreeMap<u32, Savestate>,
    delta_states: BTreeMap<u32, DeltaSavestate>,
    full_memory_used: usize,
    delta_memory_used: usize,
}

impl Greenzone {
    pub fn new(frame_0: Savestate) -> Greenzone {
        Greenzone {
            full_memory_used: frame_0.size(),
            delta_memory_used: 0,
            full_states: [(0, frame_0)].into_iter().collect(),
            delta_states: BTreeMap::new(),
        }
    }

    fn memory_threshold(&self) -> usize {
        100_000_000
    }

    pub fn invalidate(&mut self, after: u32) {
        self.full_states.retain(|f, s| {
            if *f <= after {
                true
            } else {
                self.full_memory_used -= s.size();
                false
            }
        });
        self.delta_states.retain(|f, s| {
            if *f <= after {
                true
            } else {
                self.delta_memory_used -= s.size();
                false
            }
        });
    }

    pub fn save(&mut self, frame: u32, state: Savestate) {
        self.full_memory_used += state.size();

        // Remove an old delta frame, if there was one.
        if let Some(old) = self.delta_states.remove(&frame) {
            self.delta_memory_used -= old.size();
        }

        // Reparent any delta savestates which should now reference this savestate.
        let (_, old_parent) = self
            .full_states
            .range(0..=frame.max(1))
            .last()
            .expect("will always have a parent because frame 0 is guaranteed to be present");
        let next_frame = self.full_states.range(frame + 1..).next().map(|(k, _v)| k);
        for (_, delta) in self
            .delta_states
            .range_mut(frame..*next_frame.unwrap_or(&u32::MAX))
        {
            self.delta_memory_used -= delta.size();
            delta.reparent(old_parent, &state);
            self.delta_memory_used += delta.size();
        }

        // Remove an old frame, if there was one.
        if let Some(old) = self.full_states.insert(frame, state) {
            self.full_memory_used -= old.size();
        }
    }

    /// If memory usage is above memory_threshold, removes/delta-compresses savestates
    /// so that total memory usage is below 3/4 * memory_threshold.
    /// The cursor frame is used to help determine priority: frames nearer to this point are less
    /// likely to be pruned.
    pub fn gc(&mut self, cursor_frame: u32) {
        let priority = |frame| {
            cursor_frame
                .abs_diff(frame)
                .checked_shr(frame.trailing_zeros())
                .unwrap_or(0)
        };
        let log_priority = |frame| priority(frame).checked_ilog2().unwrap_or(0) as usize;

        if self.full_memory_used + self.delta_memory_used > self.memory_threshold() {
            print!("Garbage-collecting savestates.\nBefore: ");
            self.print_usage();

            // The target amount of memory for full states
            let full_threshold = self.memory_threshold() / 4;
            let delta_threshold = self.memory_threshold() / 2;

            // Find a priority threshold above which frames will get deleted.
            let mut full_histogram = [0usize; 32];
            for (frame, state) in &self.full_states {
                full_histogram[log_priority(*frame)] += state.size();
            }
            full_histogram
                .iter_mut()
                .scan(0, |total, s| {
                    *s += *total;
                    *total = *s;
                    Some(())
                })
                .for_each(std::mem::drop);
            assert_eq!(*full_histogram.last().unwrap(), self.full_memory_used);

            // Find full savestates that should be downgraded to delta savestates.
            let frames_to_downgrade = self
                .full_states
                .keys()
                .copied()
                .filter(|frame| {
                    *frame != 0 && full_histogram[log_priority(*frame)] >= full_threshold
                })
                .collect::<Vec<_>>();

            for frame in frames_to_downgrade {
                let state = self.full_states.remove(&frame).unwrap();
                let (_, parent) = self.full_states.range(0..frame).last().expect(
                    "will always have a parent because frame 0 is guaranteed to be present",
                );
                let next_frame = self.full_states.range(frame..).next().map(|(k, _v)| k);

                // We need to reparent any delta states who were depending on this state.
                for (_, delta) in self
                    .delta_states
                    .range_mut(frame..*next_frame.unwrap_or(&u32::MAX))
                {
                    self.delta_memory_used -= delta.size();
                    delta.reparent(&state, parent);
                    self.delta_memory_used += delta.size();
                }

                let delta = state.to_delta(parent);
                self.full_memory_used -= state.size();
                self.delta_memory_used += delta.size();

                assert!(self.delta_states.insert(frame, delta).is_none());
            }

            // Now find delta frames that should be removed.
            let mut delta_histogram = [0usize; 32];
            for (frame, state) in &self.delta_states {
                delta_histogram[log_priority(*frame)] += state.size();
            }

            delta_histogram
                .iter_mut()
                .scan(0, |total, s| {
                    *s += *total;
                    *total = *s;
                    Some(())
                })
                .for_each(std::mem::drop);
            assert_eq!(*delta_histogram.last().unwrap(), self.delta_memory_used);

            self.delta_states.retain(|frame, state| {
                if delta_histogram[log_priority(*frame)] < delta_threshold {
                    true
                } else {
                    self.delta_memory_used -= state.size();
                    false
                }
            });

            print!("After: ");
            self.print_usage();
        }
    }

    fn print_usage(&self) {
        println!(
            "{}MB ({} states)",
            (self.full_memory_used + self.delta_memory_used) / 1_000_000,
            self.full_states.len() + self.delta_states.len()
        );
        println!(
            "    full: {}MB ({} states)",
            self.full_memory_used / 1_000_000,
            self.full_states.len()
        );
        println!(
            "    delta: {}MB ({} states)",
            self.delta_memory_used / 1_000_000,
            self.delta_states.len()
        );
    }

    pub fn contains(&self, frame: u32) -> bool {
        self.full_states.contains_key(&frame) || self.delta_states.contains_key(&frame)
    }

    pub fn restore(&self, frame: u32) -> (u32, SavestateRef<'_>) {
        let (&full_frame, full_state) = self
            .full_states
            .range(0..=frame)
            .next_back()
            .expect("should always have a power-on state");

        if let Some((&delta_frame, delta_state)) =
            self.delta_states.range(full_frame..=frame).next_back()
        {
            (delta_frame, SavestateRef::Delta(delta_state, full_state))
        } else {
            (full_frame, SavestateRef::Full(full_state))
        }
    }
}
