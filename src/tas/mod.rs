use std::{rc::Rc, time::Instant};

use crate::core::{self, Core};

use self::movie::{Movie, Pattern};

mod greenzone;
pub mod input;
pub mod movie;

pub struct Tas {
    core: Core,

    // Playback state
    /// The frame the user expects to be visible on the screen.
    /// Normally, this will lag behind next_emulator_frame by 1. However, next_emulator_frame may
    /// have a lower value if the emulator is currently behind, e.g. to needing to catch up after
    /// loading a state from the greenzone.
    playback_cursor: u32,
    /// The next frame the emulator will render.
    next_emulator_frame: u32,
    run_mode: RunMode,
    last_host_frame: Instant,
    core_frame_fraction: f32,

    // Editor state
    selected_frame: u32,
    selection_locked: bool,

    movie: Movie,

    undo_history: Vec<Action>,
    undo_index: usize,
    repeatable_action: Option<Action>,

    /// The earliest frame that was recorded and then invalidated.
    /// If we record over this frame, that's a rerecord.
    earliest_invalidated_frame: Option<u32>,
}

#[derive(Debug)]
pub enum RunMode {
    Running {
        stop_at: Option<u32>,
        record_mode: RecordMode,
    },
    Paused,
}

impl Default for RunMode {
    fn default() -> Self {
        Self::Paused
    }
}

#[derive(Debug)]
pub enum RecordMode {
    ReadOnly,
    Insert { pattern: Pattern, action: Action },
    Overwrite { pattern: Pattern, action: Action },
}

/// An action in the undo history. Actions can be undone, redone, or repeated.
#[derive(Debug, Clone)]
pub struct Action {
    /// The location of the edit.
    pub cursor: u32,

    /// What the edit was.
    pub kind: ActionKind,
}

impl Action {
    /// Returns true if this action is blank.
    fn is_empty(&self) -> bool {
        match &self.kind {
            ActionKind::Insert(frames)
            | ActionKind::Delete(frames)
            | ActionKind::Apply {
                previous: frames, ..
            } => frames.is_empty(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ActionKind {
    /// Insert the given frames at the cursor.
    Insert(Vec<u8>),
    /// Delete the given frames at the cursor.
    Delete(Vec<u8>),
    /// Apply a given pattern to the given contents, starting at the cursor.
    Apply { pattern: Pattern, previous: Vec<u8> },
}

impl Tas {
    pub fn new(mut core: Core) -> Tas {
        let input_ports = vec![input::InputPort::Joypad(input::Joypad::Snes)];

        let frame_0 = core.save_state(None);

        let movie = Movie::new(
            input_ports,
            frame_0.compress(),
            core.id.clone(),
            core.rom_path.clone(),
            core.rom_sha256,
        );
        Tas {
            playback_cursor: 0,
            next_emulator_frame: 0,
            run_mode: RunMode::Paused,
            last_host_frame: Instant::now(),
            core_frame_fraction: 0.,

            selected_frame: 0,
            selection_locked: true,
            undo_history: Vec::new(),
            undo_index: 0,
            repeatable_action: None,
            earliest_invalidated_frame: None,

            movie,
            core,
        }
    }

    pub fn load(
        mut core: Core,
        file: movie::file::MovieFile,
        movie_path: std::path::PathBuf,
    ) -> anyhow::Result<Tas> {
        let frame_0 = core.save_state(None);
        let movie = Movie::load(
            file,
            movie_path,
            frame_0.compress(),
            core.id.clone(),
            core.rom_path.clone(),
            core.rom_sha256,
        )?;

        Ok(Tas {
            playback_cursor: 0,
            next_emulator_frame: 0,
            run_mode: RunMode::Paused,
            last_host_frame: Instant::now(),
            core_frame_fraction: 0.,

            selected_frame: 0,
            selection_locked: true,
            undo_history: Vec::new(),
            undo_index: 0,
            repeatable_action: None,
            earliest_invalidated_frame: None,

            movie,
            core,
        })
    }

    pub fn selected_frame(&self) -> u32 {
        self.selected_frame
    }

    pub fn playback_frame(&self) -> u32 {
        self.playback_cursor
    }

    pub fn movie(&self) -> &Movie {
        &self.movie
    }

    pub fn set_movie_path(&mut self, path: Option<std::path::PathBuf>) {
        self.movie.movie_path = path;
    }

    pub fn run_guest_frame(
        &mut self,
        audio_callback: &mut impl FnMut(&[core::AudioFrame]),
    ) -> &core::Frame {
        if self
            .earliest_invalidated_frame
            .is_some_and(|e| self.next_emulator_frame > e)
        {
            println!("rerecord");
            self.movie.rerecords += 1;
            self.earliest_invalidated_frame = None;
        }
        let frame = if self.next_emulator_frame < self.movie.len() {
            self.movie.frame(self.next_emulator_frame)
        } else {
            self.movie.default_frame()
        };
        self.core
            .run_frame(frame, &self.movie.input_ports, audio_callback);
        self.next_emulator_frame += 1;
        self.movie.greenzone.save(
            self.next_emulator_frame,
            self.core
                .save_state(Some((frame, &self.movie.input_ports)))
                .compress(),
        );
        if self.playback_cursor < self.next_emulator_frame - 1 {
            let n = self.next_emulator_frame - self.playback_cursor - 1;
            self.playback_cursor += n;
            if self.selection_locked {
                self.selected_frame += n;
            }
        }
        &self.core.frame
    }

    pub fn run_host_frame(
        &mut self,
        mut audio_callback: impl FnMut(&[core::AudioFrame]),
    ) -> &core::Frame {
        // Determine how many guest frames have elapsed since the last host frame
        let time = Instant::now();
        let host_frame_duration = time - self.last_host_frame;
        self.last_host_frame = time;
        self.core_frame_fraction +=
            host_frame_duration.as_secs_f32() * self.core.av_info.timing.fps as f32;

        // Don't try to skip more than one guest frame
        self.core_frame_fraction = self.core_frame_fraction.clamp(0., 2.);

        // If we're behind where playback should be, seek to catch up
        while self.playback_cursor >= self.next_emulator_frame && self.core_frame_fraction >= 1. {
            self.run_guest_frame(&mut audio_callback);
            self.core_frame_fraction -= 1.;
        }

        if self.core_frame_fraction < 1. {
            return &self.core.frame;
        }

        let mut run_mode = std::mem::replace(&mut self.run_mode, RunMode::Paused);

        let result = match &mut run_mode {
            RunMode::Paused => &self.core.frame,
            RunMode::Running {
                stop_at,
                record_mode,
            } => {
                while self.core_frame_fraction >= 1. {
                    if let Some(stop) = stop_at {
                        if self.playback_cursor >= *stop {
                            run_mode = RunMode::Paused;
                            break;
                        }
                    }

                    // If we're recording, write the user input.
                    match record_mode {
                        RecordMode::ReadOnly => {
                            // We're not recording, nothing to do.
                        }
                        RecordMode::Insert { pattern, action } => {
                            // Insert a blank frame, and apply the pattern to it.
                            self.insert_blank(self.playback_cursor + 1, 1);
                            *pattern = self.apply(pattern, self.playback_cursor + 1, 1);

                            // Update the undo history.
                            let ActionKind::Insert(frames) = &mut action.kind else {
                                unreachable!("unexpected action kind");
                            };
                            frames.extend_from_slice(self.frame(self.playback_cursor + 1));
                        }
                        RecordMode::Overwrite { pattern: _, action } => {
                            // Update the undo history.
                            let ActionKind::Apply { pattern, previous } = &mut action.kind else {
                                unreachable!("unexpected action kind");
                            };
                            let old_len = previous.len();
                            previous.extend_from_slice(self.frame(self.playback_cursor + 1));

                            // Apply the pattern to the current frame.
                            *pattern = self.apply(pattern, self.playback_cursor + 1, 1);

                            pattern.expand(
                                self.movie().input_ports(),
                                (previous.len() / pattern.buf.frame_size) as u32,
                            );
                            Rc::make_mut(&mut pattern.buf).data[old_len..]
                                .copy_from_slice(self.frame(self.playback_cursor + 1));
                        }
                    }
                    assert!(self.next_emulator_frame == self.playback_cursor + 1);

                    self.run_guest_frame(&mut audio_callback);
                    self.core_frame_fraction -= 1.;
                }

                &self.core.frame
            }
        };

        self.run_mode = run_mode;
        self.movie.greenzone.gc(self.playback_cursor);

        result
    }

    pub fn run_mode(&self) -> &RunMode {
        &self.run_mode
    }

    pub fn run_mode_mut(&mut self) -> &mut RunMode {
        &mut self.run_mode
    }

    pub fn set_run_mode(&mut self, mode: RunMode) {
        let old_mode = std::mem::replace(&mut self.run_mode, mode);

        // If we had pending undo actions, commit them.
        if let RunMode::Running { record_mode, .. } = old_mode {
            match record_mode {
                RecordMode::ReadOnly => {}
                RecordMode::Insert { action, .. } | RecordMode::Overwrite { action, .. } => {
                    self.push_undo(action)
                }
            }
        }
    }

    pub fn set_input(&mut self, pattern: &Pattern) {
        let mut mode = std::mem::take(self.run_mode_mut());
        match &mut mode {
            RunMode::Paused => {
                _ = self.apply(pattern, self.selected_frame(), 1);
            }
            RunMode::Running {
                stop_at: _,
                record_mode:
                    RecordMode::Insert { pattern: p, .. } | RecordMode::Overwrite { pattern: p, .. },
            } => {
                *p = Pattern {
                    buf: pattern.buf.clone(),
                    offset: p.offset,
                }
            }
            _ => {}
        };
        self.set_run_mode(mode);
    }

    pub fn apply(&mut self, pattern: &Pattern, index: u32, len: usize) -> Pattern {
        self.ensure_length(index + len as u32);
        self.invalidate(index);
        let frame_size = self.movie.frame_size();
        pattern.apply(
            &self.movie.input_ports,
            &mut self.movie.data[index as usize * frame_size..][..len * frame_size],
        )
    }

    pub fn av_info(&self) -> libretro_ffi::retro_system_av_info {
        self.core.av_info
    }

    /// Invalidates the greenzone after the specified index.
    /// In other words: the savestate at the beginning of this frame is valid, but this frame's
    /// input may have changed.
    pub fn invalidate(&mut self, after: u32) {
        let invalidated = self.movie.greenzone.invalidate(after);
        if invalidated {
            if self.earliest_invalidated_frame.is_none()
                || self.earliest_invalidated_frame.unwrap() > after
            {
                self.earliest_invalidated_frame = Some(after);
            }
        }

        if self.next_emulator_frame > after {
            let (f, state) = self.movie.greenzone.restore(after);
            self.next_emulator_frame = f;
            self.core.restore_state(state);
        }
    }

    pub fn frame(&self, idx: u32) -> &[u8] {
        self.movie.frame(idx)
    }

    pub fn frame_mut(&mut self, idx: u32) -> &mut [u8] {
        self.invalidate(idx);
        self.movie.frame_mut(idx)
    }

    pub fn ensure_length(&mut self, len: u32) {
        self.movie.ensure_length(len);
    }

    /// Inserts a frame of input before the specified index.
    pub fn insert(&mut self, idx: u32, buf: &[u8]) {
        let size = self.movie.frame_size();
        assert_eq!(buf.len() % size, 0);
        self.invalidate(idx);

        let insert_idx = idx as usize * size;
        self.movie.ensure_length(idx);
        self.movie
            .data
            .splice(insert_idx..insert_idx, buf.iter().cloned());
    }

    /// Inserts a blank frame before the specified index.
    pub fn insert_blank(&mut self, idx: u32, len: usize) {
        self.invalidate(idx);
        self.movie.ensure_length(idx);

        let size = self.movie.frame_size();
        let insert_idx = idx as usize * size;
        self.movie.data.splice(
            insert_idx..insert_idx,
            self.movie
                .default_pattern
                .data
                .iter()
                .cloned()
                .cycle()
                .take(len * size),
        );
    }

    pub fn delete(&mut self, frames: impl std::ops::RangeBounds<u32>) {
        let size = self.movie().frame_size();
        let range = (
            match frames.start_bound() {
                std::ops::Bound::Unbounded => std::ops::Bound::Unbounded,
                std::ops::Bound::Included(frame) => {
                    std::ops::Bound::Included(*frame as usize * size)
                }
                std::ops::Bound::Excluded(frame) => {
                    std::ops::Bound::Excluded(*frame as usize * size + (size - 1))
                }
            },
            match frames.end_bound() {
                std::ops::Bound::Unbounded => std::ops::Bound::Unbounded,
                std::ops::Bound::Included(frame) => {
                    std::ops::Bound::Included(*frame as usize * size + (size - 1))
                }
                std::ops::Bound::Excluded(frame) => {
                    std::ops::Bound::Excluded(*frame as usize * size)
                }
            },
        );
        let start = match frames.start_bound() {
            std::ops::Bound::Unbounded => 0,
            std::ops::Bound::Excluded(frame) => frame + 1,
            std::ops::Bound::Included(frame) => *frame,
        };

        let action = Action {
            cursor: start,
            kind: ActionKind::Delete(self.movie.data.drain(range).collect()),
        };
        self.push_undo(action);
        self.invalidate(start);
    }

    pub fn seek_to(&mut self, frame: u32) {
        if self.playback_cursor != frame {
            self.playback_cursor = frame;
            let (f, state) = self.movie.greenzone.restore(frame);
            self.next_emulator_frame = f;
            self.core.restore_state(state);
        }
    }

    pub fn select(&mut self, frame: u32) {
        if frame < self.selected_frame {
            self.select_prev(self.selected_frame - frame);
        } else if frame > self.selected_frame {
            self.select_next(frame - self.selected_frame);
        }
    }

    pub fn select_next(&mut self, n: u32) {
        let n = n.min(self.movie.len().saturating_sub(self.selected_frame() + 1));
        self.selected_frame += n;
        if self.selection_locked {
            self.seek_to(self.playback_cursor + n);
        }
        self.run_mode = RunMode::Paused;
    }
    pub fn select_prev(&mut self, n: u32) {
        let n = n.min(self.selected_frame);
        self.selected_frame -= n;
        if self.selection_locked {
            self.seek_to(self.playback_cursor.saturating_sub(n));
        }
        self.run_mode = RunMode::Paused;
    }

    pub fn push_undo(&mut self, action: Action) {
        if !action.is_empty() {
            self.undo_history.truncate(self.undo_index);
            self.undo_history.push(action);
            self.undo_index = self.undo_history.len();
        }
    }

    pub fn push_repeatable(&mut self, action: Action) {
        self.push_undo(action.clone());
        self.repeatable_action = Some(action);
    }

    pub fn undo(&mut self, action: &Action) {
        self.invalidate(action.cursor);
        let start = action.cursor as usize * self.movie.frame_size();
        match &action.kind {
            ActionKind::Insert(frames) => {
                let end = start + frames.len();
                self.movie.data.drain(start..end);
            }
            ActionKind::Delete(frames) => {
                self.movie.data.splice(start..start, frames.iter().cloned());
            }
            ActionKind::Apply {
                pattern: _,
                previous,
            } => {
                self.movie.data[start..start + previous.len()].copy_from_slice(previous);
            }
        }
        self.select(action.cursor.min(self.movie.len().saturating_sub(1)));
    }

    pub fn undo_latest(&mut self) {
        if self.undo_index != 0 {
            self.undo_index -= 1;
            let undo_history = std::mem::take(&mut self.undo_history);
            self.undo(&undo_history[self.undo_index]);
            self.undo_history = undo_history;
        }
    }

    pub fn redo(&mut self, action: &Action) {
        self.invalidate(action.cursor);
        let start = action.cursor as usize * self.movie.frame_size();
        match &action.kind {
            ActionKind::Insert(frames) => {
                self.movie.data.splice(start..start, frames.iter().cloned());
            }
            ActionKind::Delete(frames) => {
                let end = start + frames.len();
                self.movie.data.drain(start..end);
            }
            ActionKind::Apply { pattern, previous } => {
                pattern.apply(
                    &self.movie.input_ports,
                    &mut self.movie.data[start..start + previous.len()],
                );
            }
        }
        self.select(action.cursor.min(self.movie.len().saturating_sub(1)));
    }

    pub fn redo_latest(&mut self) {
        if self.undo_index < self.undo_history.len() {
            let undo_history = std::mem::take(&mut self.undo_history);
            self.redo(&undo_history[self.undo_index]);
            self.undo_history = undo_history;
            self.undo_index += 1;
        }
    }

    pub fn repeat(&mut self) {
        if let Some(mut action) = self.repeatable_action.take() {
            action.cursor = self.selected_frame;
            self.redo(&action);
            self.push_undo(action.clone());
            self.repeatable_action = Some(action);
        };
    }
}
