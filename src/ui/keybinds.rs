use crate::{tas::Tas, ui::piano_roll::ScrollEvent};

use super::piano_roll::PianoRoll;

pub struct Keybinds {}

// shortcut for defining control characters
const fn control(c: char) -> char {
    let c = c.to_ascii_uppercase();
    assert!((c >= '@' && c < '_'), "not a valid control character");
    (c as u8 & !0x40) as char
}

impl Keybinds {
    pub fn new() -> Keybinds {
        Self {}
    }

    pub fn input_key(
        &mut self,
        key: &winit::event::VirtualKeyCode,
        _modifiers: &winit::event::ModifiersState,
        tas: &mut Tas,
        _piano_roll: &mut PianoRoll,
    ) {
        match key {
            winit::event::VirtualKeyCode::Down => tas.select_next(1),
            winit::event::VirtualKeyCode::Up => tas.select_next(1),
            _ => {}
        }
    }

    pub fn input_char(&mut self, c: char, tas: &mut Tas, piano_roll: &mut PianoRoll) {
        const CD: char = control('d');
        const CU: char = control('u');
        const CF: char = control('B');
        const CB: char = control('F');
        match c {
            ' ' => tas.toggle_playback(),
            'j' => tas.select_next(1),
            'k' => tas.select_prev(1),
            CD => tas.select_next(piano_roll.screen_size() / 2),
            CU => tas.select_prev(piano_roll.screen_size() / 2),
            CF => tas.select_next(piano_roll.screen_size()),
            CB => tas.select_prev(piano_roll.screen_size()),

            _ => {}
        }
    }
}
