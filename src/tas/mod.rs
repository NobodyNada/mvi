use std::{rc::Rc, time::Instant};

use crate::core::{self, Core};

use self::movie::Movie;

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
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum RecordMode {
    ReadOnly,
    Insert(Rc<movie::Pattern>),
    Overwrite(Rc<movie::Pattern>),
}

impl Tas {
    pub fn new(mut core: Core) -> Tas {
        let input_ports = vec![input::InputPort::Joypad(input::Joypad::Snes)];

        let frame_0 = core.save_state();

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

            movie,
            core,
        }
    }

    pub fn load(
        mut core: Core,
        file: movie::file::MovieFile,
        movie_path: std::path::PathBuf,
    ) -> anyhow::Result<Tas> {
        let frame_0 = core.save_state();
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
        let frame = if self.next_emulator_frame < self.movie.len() {
            self.movie.frame(self.next_emulator_frame)
        } else {
            self.movie.default_frame()
        };
        self.core
            .run_frame(frame, &self.movie.input_ports, audio_callback);
        self.next_emulator_frame += 1;
        self.movie
            .greenzone
            .save(self.next_emulator_frame, self.core.save_state().compress());
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

        let result = match &run_mode {
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

                    match record_mode {
                        RecordMode::ReadOnly => {}
                        RecordMode::Insert(pattern) => {
                            assert!(pattern.data.len() == self.movie.frame_size());
                            self.insert(self.playback_cursor + 1, &pattern.data);
                        }
                        RecordMode::Overwrite(pattern) => {
                            assert!(pattern.data.len() == self.movie.frame_size());
                            self.movie
                                .frame_mut(self.playback_cursor + 1)
                                .copy_from_slice(&pattern.data);
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
        self.run_mode = mode;
    }

    pub fn set_input(&mut self, pattern: &Rc<movie::Pattern>) {
        let mut mode = std::mem::take(self.run_mode_mut());
        match &mut mode {
            RunMode::Paused => self
                .frame_mut(self.selected_frame())
                .copy_from_slice(&pattern.data),
            RunMode::Running {
                stop_at: _,
                record_mode: RecordMode::Insert(p) | RecordMode::Overwrite(p),
            } => *p = pattern.clone(),
            _ => {}
        };
        self.set_run_mode(mode);
    }

    pub fn av_info(&self) -> libretro_ffi::retro_system_av_info {
        self.core.av_info
    }

    /// Invalidates the greenzone after the specified index.
    /// In other words: the savestate at the beginning of this frame is valid, but this frame's
    /// input may have changed.
    pub fn invalidate(&mut self, after: u32) {
        self.movie.greenzone.invalidate(after);
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

    pub fn seek_to(&mut self, frame: u32) {
        if self.playback_cursor != frame {
            self.playback_cursor = frame;
            let (f, state) = self.movie.greenzone.restore(frame);
            self.next_emulator_frame = f;
            self.core.restore_state(state);
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
}
