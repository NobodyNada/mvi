use std::collections::HashMap;

use winit::event::{ModifiersState, VirtualKeyCode};

use crate::{
    tas::{self, movie::Pattern, Tas},
    ui::piano_roll::ScrollLock,
};

use super::piano_roll::PianoRoll;

pub struct Keybinds {
    mode: Mode,

    input_bindings: HashMap<VirtualKeyCode, u32>,
}

pub enum Mode {
    Normal { count: u32 },
    Insert(Pattern),
    Replace(Pattern),
    Z,
}

// shortcut for defining control characters
const fn control(c: char) -> char {
    let c = c.to_ascii_uppercase();
    assert!((c >= '@' && c < '_'), "not a valid control character");
    (c as u8 & !0x40) as char
}

impl Keybinds {
    pub fn new() -> Keybinds {
        Self {
            mode: Mode::Normal { count: 0 },
            input_bindings: HashMap::from([
                (VirtualKeyCode::A, 0),
                (VirtualKeyCode::Q, 1),
                (VirtualKeyCode::W, 2),
                (VirtualKeyCode::P, 3),
                (VirtualKeyCode::K, 4),
                (VirtualKeyCode::J, 5),
                (VirtualKeyCode::H, 6),
                (VirtualKeyCode::L, 7),
                (VirtualKeyCode::D, 8),
                (VirtualKeyCode::F, 9),
                (VirtualKeyCode::E, 10),
                (VirtualKeyCode::R, 11),
            ]),
        }
    }

    pub fn key_down(
        &mut self,
        key: VirtualKeyCode,
        modifiers: ModifiersState,
        tas: &mut Tas,
        _piano_roll: &mut PianoRoll,
    ) {
        const NONE: ModifiersState = ModifiersState::empty();
        const SHIFT: ModifiersState = ModifiersState::SHIFT;
        match (&mut self.mode, modifiers, key) {
            (Mode::Normal { count: count @ 1.. }, NONE, VirtualKeyCode::Down) => {
                tas.select_next(std::mem::take(count))
            }
            (Mode::Normal { count: count @ 1.. }, NONE, VirtualKeyCode::Up) => {
                tas.select_next(std::mem::take(count))
            }
            (_, NONE, VirtualKeyCode::Down) => tas.select_next(1),
            (_, NONE, VirtualKeyCode::Up) => tas.select_next(1),
            (_, NONE, VirtualKeyCode::Escape) => {
                self.mode = Mode::Normal { count: 0 };
                tas.set_run_mode(tas::RunMode::Paused);
            }
            (Mode::Normal { .. }, NONE, VirtualKeyCode::I | VirtualKeyCode::A)
            | (Mode::Normal { .. }, SHIFT, VirtualKeyCode::R) => {
                let pattern = Pattern {
                    data: tas.movie().default_frame().to_vec(),
                };
                let is_replace = key == VirtualKeyCode::R;
                let is_append = key == VirtualKeyCode::A;
                let mode = match tas.run_mode() {
                    tas::RunMode::Paused => tas::RunMode::Paused,
                    tas::RunMode::Running {
                        stop_at,
                        record_mode: _,
                    } => tas::RunMode::Running {
                        stop_at: *stop_at,
                        record_mode: if is_replace {
                            tas::RecordMode::Overwrite(pattern.clone())
                        } else {
                            tas::RecordMode::Insert(pattern.clone())
                        },
                    },
                };
                tas.set_run_mode(mode);
                tas.ensure_length(tas.selected_frame() + 1);
                if is_replace {
                    self.mode = Mode::Replace(pattern);
                } else {
                    // If the movie only has one blank frame, overwrite it insead of inserting
                    if tas.movie().len() > 1 || tas.frame(0) != tas.movie().default_frame() {
                        if is_append {
                            tas.insert(tas.selected_frame() + 1, &pattern.data);
                        } else {
                            tas.insert(tas.selected_frame(), &pattern.data);
                        }
                    }
                    self.mode = Mode::Insert(pattern);
                }
            }
            (Mode::Insert(pattern) | Mode::Replace(pattern), NONE, VirtualKeyCode::Return) => {
                if let tas::RunMode::Paused = tas.run_mode() {
                    #[allow(clippy::unnecessary_to_owned)] // false-positive
                    tas.insert(
                        tas.selected_frame() + 1,
                        &tas.movie().default_frame().to_vec(),
                    );
                    tas.select_next(1);
                    tas.set_input(pattern);
                }
            }
            (Mode::Insert(pattern) | Mode::Replace(pattern), NONE, k) => {
                if let Some(id) = self.input_bindings.get(&k) {
                    if pattern.read(tas.movie().input_ports(), 0, 0, *id) != 1 {
                        pattern.write(&tas.movie().input_ports(), 0, 0, *id, 1);
                        tas.set_input(pattern);
                    }
                }
            }
            _ => {}
        }
    }

    pub fn key_up(
        &mut self,
        key: VirtualKeyCode,
        modifiers: winit::event::ModifiersState,
        tas: &mut Tas,
        _piano_roll: &mut PianoRoll,
    ) {
        const NONE: ModifiersState = ModifiersState::empty();
        #[allow(clippy::single_match)]
        match (&mut self.mode, modifiers, key) {
            (Mode::Insert(pattern) | Mode::Replace(pattern), NONE, k) => {
                if let Some(id) = self.input_bindings.get(&k) {
                    if pattern.read(tas.movie().input_ports(), 0, 0, *id) != 0 {
                        pattern.write(tas.movie().input_ports(), 0, 0, *id, 0);
                        tas.set_input(pattern);
                    }
                }
            }
            _ => {}
        }
    }

    pub fn toggle_playback(&mut self, tas: &mut Tas) {
        let mode = match tas.run_mode() {
            tas::RunMode::Running {
                stop_at: _,
                record_mode: _,
            } => tas::RunMode::Paused,
            tas::RunMode::Paused => tas::RunMode::Running {
                stop_at: None,
                record_mode: match &self.mode {
                    Mode::Normal { .. } | Mode::Z => tas::RecordMode::ReadOnly,
                    Mode::Insert(pattern) => tas::RecordMode::Insert(pattern.clone()),
                    Mode::Replace(pattern) => tas::RecordMode::Overwrite(pattern.clone()),
                },
            },
        };
        tas.set_run_mode(mode);
    }

    pub fn input_char(&mut self, c: char, tas: &mut Tas, piano_roll: &mut PianoRoll) {
        const CD: char = control('d');
        const CU: char = control('u');
        const CF: char = control('B');
        const CB: char = control('F');
        match (&mut self.mode, c) {
            (Mode::Normal { .. }, 'z') => self.mode = Mode::Z,
            (Mode::Z, 't') => piano_roll.set_scroll_lock(ScrollLock::Top),
            (Mode::Z, 'z') => piano_roll.set_scroll_lock(ScrollLock::Center),
            (Mode::Z, 'b') => piano_roll.set_scroll_lock(ScrollLock::Bottom),
            (Mode::Z, _) => self.mode = Mode::Normal { count: 0 },

            (Mode::Normal { count }, d @ '0'..='9') => {
                *count *= 10;
                *count += d.to_digit(10).unwrap();
                *count = (*count).min(1_000_000);
            }
            (_, ' ') => self.toggle_playback(tas),
            (Mode::Normal { count }, 'j') => tas.select_next(std::mem::take(count).max(1)),
            (Mode::Normal { count }, 'k') => tas.select_prev(std::mem::take(count).max(1)),
            (_, CD) => tas.select_next(piano_roll.screen_size() / 2),
            (_, CU) => tas.select_prev(piano_roll.screen_size() / 2),
            (_, CF) => tas.select_next(piano_roll.screen_size()),
            (_, CB) => tas.select_prev(piano_roll.screen_size()),

            _ => {}
        }
    }
}
