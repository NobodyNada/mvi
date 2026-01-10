use std::time::Instant;

use crate::core::{self, Core, trace};
use crate::tas::edit::{Action, ActionHandle};

use self::edit::Pattern;
use self::movie::Movie;

pub mod edit;
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

    /// Whether the trace debugger is enabled.
    trace_enabled: bool,

    trace: Option<trace::Trace>,
}

#[derive(Debug, Default)]
pub enum RunMode {
    Running {
        stop_at: Option<u32>,
        record_mode: RecordMode,
    },
    #[default]
    Paused,
}

#[derive(Debug)]
pub enum RecordMode {
    ReadOnly,
    Insert {
        pattern: Pattern,
        action: ActionHandle,
    },
    Overwrite {
        pattern: Pattern,
        action: ActionHandle,
    },
}

impl Tas {
    pub fn new(mut core: Core, system_id: Option<String>) -> Tas {
        let input_ports = vec![input::InputPort::Joypad(input::Joypad::Snes)];

        let frame_0 = core.save_state(None);

        let movie = Movie::new(
            input_ports,
            frame_0.compress(),
            core.id.clone(),
            system_id,
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
            trace_enabled: false,
            trace: None,
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
            trace_enabled: false,
            trace: None,
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

    pub fn read_ram_watch(&self, watch: &movie::RamWatch) -> Option<u64> {
        let mut v = 0;
        for offset in 0..watch.format.width as usize {
            v |= (self.core.read_memory_byte(watch.address + offset)? as u64) << (offset * 8);
        }
        Some(v)
    }

    pub fn ramwatches_mut(&mut self) -> &mut Vec<movie::RamWatch> {
        &mut self.movie.ramwatches
    }

    pub fn set_movie_path(&mut self, path: Option<std::path::PathBuf>) {
        self.movie.movie_path = path;
    }

    pub fn can_trace(&self) -> bool {
        self.core.trace_fields.is_some()
    }

    pub fn trace_enabled(&self) -> bool {
        self.trace_enabled
    }

    pub fn set_trace_enabled(&mut self, enabled: bool) {
        assert!(!enabled || self.can_trace());
        self.trace_enabled = enabled;
    }

    pub fn trace_fields(&self) -> Option<&[trace::Field]> {
        self.core
            .trace_fields
            .as_deref()
            .map(|f| f.fields.as_slice())
    }

    pub fn trace(&self) -> Option<&trace::Trace> {
        self.trace.as_ref()
    }

    pub fn run_guest_frame(
        &mut self,
        audio_callback: &mut impl FnMut(&[core::AudioFrame]),
    ) -> &core::Frame {
        self.trace = None;
        if self
            .earliest_invalidated_frame
            .is_some_and(|e| self.next_emulator_frame > e)
        {
            self.movie.rerecords += 1;
            self.earliest_invalidated_frame = None;
        }
        let frame = if self.next_emulator_frame < self.movie.len() {
            self.movie.frame(self.next_emulator_frame)
        } else {
            self.movie.default_frame()
        };
        if self.trace_enabled() {
            self.core.begin_trace();
        }
        self.core
            .run_frame(frame, &self.movie.input_ports, audio_callback);
        if self.trace_enabled() {
            self.trace = Some(self.core.end_trace());
        }
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
                    self.core_frame_fraction -= 1.;
                    if let Some(stop) = stop_at
                        && self.playback_cursor >= *stop
                    {
                        run_mode = RunMode::Paused;
                        break;
                    }

                    // If we're recording, write the user input.
                    match record_mode {
                        RecordMode::ReadOnly => {
                            // We're not recording, nothing to do.
                        }
                        RecordMode::Insert { pattern, action } => {
                            let mut frame = self.movie().default_frame().to_owned();
                            *pattern = pattern.apply(self.movie().input_ports(), &mut frame);
                            action.insert(self, self.playback_cursor + 1, &frame);
                        }
                        RecordMode::Overwrite { pattern, action } => {
                            let mut frame = self.frame(self.playback_cursor + 1).to_owned();
                            *pattern = pattern.apply(self.movie().input_ports(), &mut frame);
                            action.replace_frame(self, self.playback_cursor + 1, &frame);
                        }
                    }
                    assert!(self.next_emulator_frame == self.playback_cursor + 1);

                    self.run_guest_frame(&mut audio_callback);
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

    pub fn set_run_mode(&mut self, mode: RunMode) {
        // TODO: we used to force-commit the undo action here
        // do we still want to do that?
        self.run_mode = mode;
    }

    pub fn set_input(&mut self, pattern: &Pattern) {
        let RunMode::Running {
            stop_at: _,
            record_mode:
                RecordMode::Insert { pattern: p, .. } | RecordMode::Overwrite { pattern: p, .. },
        } = &mut self.run_mode
        else {
            panic!("Not recording")
        };
        *p = Pattern {
            buf: pattern.buf.clone(),
            offset: p.offset,
        };
    }

    pub fn av_info(&self) -> libretro_ffi::retro_system_av_info {
        self.core.av_info
    }

    /// Invalidates the greenzone after the specified index.
    /// In other words: the savestate at the beginning of this frame is valid, but this frame's
    /// input may have changed.
    pub fn invalidate(&mut self, after: u32) {
        let invalidated = self.movie.greenzone.invalidate(after);
        if invalidated
            && (self.earliest_invalidated_frame.is_none()
                || self.earliest_invalidated_frame.unwrap() > after)
        {
            self.earliest_invalidated_frame = Some(after);
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

    pub fn ensure_length(&mut self, len: u32) {
        self.movie.ensure_length(len);
    }

    pub fn start_repeatable(&mut self) -> ActionHandle {
        ActionHandle::new(true)
    }

    pub fn start_nonrepeatable(&mut self) -> ActionHandle {
        ActionHandle::new(false)
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

    pub fn seek_to_selected(&mut self) {
        let n = self.playback_cursor.saturating_sub(self.selected_frame);
        self.seek_to(self.selected_frame);
        if self.selection_locked {
            self.selected_frame -= n;
        }
    }

    fn push_undo(&mut self, action: Action) {
        self.synchronize_undo();
        self.undo_history.truncate(self.undo_index);
        self.undo_history.push(action);
        self.undo_index = self.undo_history.len();
    }

    fn synchronize_undo(&mut self) {
        if let Some(top) = self.undo_history.last()
            && self.undo_history.len() == self.undo_index
        {
            if top.is_empty() {
                self.undo_index -= 1;
                self.undo_history.pop();
            } else if top.repeatable {
                self.repeatable_action = Some(top.clone());
            }
        }
    }

    pub fn undo_latest(&mut self) {
        self.synchronize_undo();
        if self.undo_index != 0 {
            self.undo_index -= 1;
            let undo_history = std::mem::take(&mut self.undo_history);
            undo_history[self.undo_index].undo(self);
            self.undo_history = undo_history;
        }
    }

    pub fn redo_latest(&mut self) {
        if self.undo_index < self.undo_history.len() {
            let undo_history = std::mem::take(&mut self.undo_history);
            undo_history[self.undo_index].redo(self);
            self.undo_history = undo_history;
            self.undo_index += 1;
        }
    }

    pub fn repeat(&mut self) {
        self.synchronize_undo();
        if let Some(mut action) = self.repeatable_action.take() {
            action.cursor = self.selected_frame;
            action.redo(self);
            self.push_undo(action.clone());
            self.repeatable_action = Some(action);
        };
    }
}
