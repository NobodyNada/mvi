use std::rc::Rc;

use imgui::{ListClipper, Ui};

use crate::tas::{
    input::InputPort,
    movie::{Pattern, PatternBuf},
    Action, ActionKind, Tas,
};

use super::keybinds;

pub struct PianoRoll {
    last_selection: u32,
    screen_size: u32,

    scroll_lock: ScrollLock,
    pending_scroll_lock: Option<ScrollLock>,

    drag_mode: Option<DragMode>,
}

#[derive(Clone, Copy)]
pub enum ScrollLock {
    Top,
    Center,
    Bottom,
}

#[derive(PartialEq, Eq)]
pub enum DragMode {
    Playback,
    Selection,
    Input {
        index: u32,
        start: u32,
        end: u32,
        autofire: bool,
    },
}

impl ScrollLock {
    fn ratio(&self) -> f32 {
        match self {
            ScrollLock::Top => 0.1,
            ScrollLock::Center => 0.5,
            ScrollLock::Bottom => 0.9,
        }
    }
}

macro_rules! color {
    [$r:expr, $g:expr, $b:expr] => {
        [$r as f32 / 256., $g as f32 / 256., $b as f32 / 256., 1.]
    };
}
impl PianoRoll {
    const FRAMENO_COLOR: [f32; 4] = color![116, 128, 160];
    const SELECTED_FRAMENO_COLOR: [f32; 4] = color![0, 0, 0];
    const UNPRESSED_COLOR: [f32; 4] = color![45, 49, 55];
    const PRESSED_COLOR: [f32; 4] = color![255, 255, 255];
    const SELECT_HIGHLIGHT: [f32; 4] = color![128, 128, 128];
    const GREENZONE_HIGHLIGHT: [f32; 4] = color![16, 32, 16];

    pub fn new() -> PianoRoll {
        PianoRoll {
            last_selection: 0,
            screen_size: 30,
            scroll_lock: ScrollLock::Center,
            pending_scroll_lock: None,
            drag_mode: None,
        }
    }

    pub fn screen_size(&self) -> u32 {
        self.screen_size
    }

    pub fn set_scroll_lock(&mut self, event: ScrollLock) {
        self.pending_scroll_lock = Some(event);
    }

    pub fn draw(&mut self, ui: &Ui, tas: &mut Tas, keybinds: &keybinds::Keybinds) {
        if !ui.is_mouse_down(imgui::MouseButton::Left) {
            #[expect(clippy::single_match)]
            match self.drag_mode.take() {
                Some(DragMode::Input {
                    index,
                    start,
                    end,
                    autofire,
                }) => {
                    // Create an undo action toggling all inputs
                    // between 'start' and 'end' inclusive.
                    let len = start.abs_diff(end) + 1;
                    let frame_size = tas.movie().frame_size();
                    let mut previous = Vec::with_capacity(frame_size * len as usize);
                    let mut pattern_buf = Vec::with_capacity(frame_size * len as usize);
                    // TODO: multi-input support
                    let input_port = tas.movie().input_ports()[0];

                    for i in start.min(end)..=start.max(end) {
                        if autofire && (i.abs_diff(start)) % 2 != 0 {
                            continue;
                        }
                        let frame = tas.movie().frame(i);
                        let frame_start = previous.len();
                        pattern_buf.extend_from_slice(frame);
                        previous.extend_from_slice(frame);
                        input_port.write(
                            &mut previous[frame_start..],
                            0,
                            index,
                            (input_port.read(frame, 0, index) == 0) as i16,
                        );
                    }

                    let mut mask = Vec::new();
                    mask.resize(
                        tas.movie()
                            .input_ports()
                            .iter()
                            .map(InputPort::total_inputs)
                            .sum::<u32>() as usize,
                        false,
                    );
                    mask[index as usize] = true; // TODO: multi-input support

                    tas.push_undo(Action {
                        cursor: start.min(end),
                        kind: ActionKind::Apply {
                            pattern: Pattern {
                                buf: Rc::new(PatternBuf {
                                    data: pattern_buf,
                                    frame_size,
                                    mask: Some(mask),
                                    autofire: None,
                                    autohold: None,
                                }),
                                offset: 0,
                            },
                            previous,
                        },
                    })
                }
                _ => {}
            }
        }

        let autofire_pressed =
            ui.is_key_down(imgui::Key::LeftAlt) || ui.is_key_down(imgui::Key::RightAlt);

        let padding = unsafe { ui.style().window_padding };
        let padding_token = ui.push_style_var(imgui::StyleVar::WindowPadding([0., 0.]));
        ui.window("Piano Roll")
            .size([256., ui.window_size()[1]], imgui::Condition::FirstUseEver)
            .build(|| {
                std::mem::drop(padding_token);
                ui.child_window("piano_roll_inputs")
                    .size([0., -ui.text_line_height_with_spacing() - padding[1]])
                    .flags(imgui::WindowFlags::NO_SCROLL_WITH_MOUSE)
                    .build(|| {
                        let rows = tas.movie().len();

                        let _style = ui.push_style_var(imgui::StyleVar::ItemSpacing([0., 0.]));
                        if tas.selected_frame() != self.last_selection
                            || self.pending_scroll_lock.is_some()
                        {
                            self.scroll_lock =
                                self.pending_scroll_lock.take().unwrap_or(self.scroll_lock);
                            ui.set_scroll_from_pos_y_with_ratio(
                                ui.cursor_start_pos()[1]
                                    + (tas.selected_frame() as f64
                                        * ui.text_line_height_with_spacing() as f64)
                                        as f32,
                                self.scroll_lock.ratio(),
                            );
                        }

                        if ui.is_window_hovered() {
                            ui.set_scroll_y(
                                ui.scroll_y()
                                    - ui.io().mouse_wheel * ui.text_line_height_with_spacing() * 3.,
                            );
                        }

                        let clipper = ListClipper::new(rows.try_into().unwrap()).begin(ui);

                        // TODO: multi-input support
                        let input_port = tas.movie().input_ports()[0];
                        let buttons = match input_port {
                            InputPort::Joypad(j) => j.buttons(),
                        };
                        let number_column_width =
                            rows.max(1000).saturating_sub(1).ilog10() as usize + 1;

                        self.screen_size =
                            (ui.window_size()[1] / ui.text_line_height_with_spacing()) as u32;

                        for row in clipper.iter() {
                            let row = row as u32;

                            let frame_rect = [
                                [ui.window_pos()[0], ui.cursor_screen_pos()[1]],
                                [
                                    ui.window_pos()[0] + ui.window_size()[0],
                                    ui.cursor_screen_pos()[1] + ui.text_line_height_with_spacing(),
                                ],
                            ];

                            let (highlight, frameno_color) = if row == tas.selected_frame() {
                                (Some(Self::SELECT_HIGHLIGHT), Self::SELECTED_FRAMENO_COLOR)
                            } else if tas.movie().greenzone().contains(row) {
                                (Some(Self::GREENZONE_HIGHLIGHT), Self::FRAMENO_COLOR)
                            } else {
                                (None, Self::FRAMENO_COLOR)
                            };

                            if let Some(highlight) = highlight {
                                ui.get_window_draw_list()
                                    .add_rect(
                                        ui.cursor_screen_pos(),
                                        [
                                            ui.cursor_screen_pos()[0] + ui.content_region_max()[0],
                                            ui.cursor_screen_pos()[1]
                                                + ui.text_line_height_with_spacing(),
                                        ],
                                        highlight,
                                    )
                                    .filled(true)
                                    .build();
                            }

                            let marker = if row == tas.playback_frame() {
                                '>'
                            } else {
                                ' '
                            };

                            ui.text_colored(frameno_color, format!("{marker}"));
                            if ui.is_item_clicked()
                                || self.drag_mode == Some(DragMode::Playback)
                                    && ui.is_mouse_hovering_rect(frame_rect[0], frame_rect[1])
                            {
                                self.drag_mode = Some(DragMode::Playback);
                                tas.seek_to(row);
                            }

                            ui.same_line();
                            ui.text_colored(
                                frameno_color,
                                format!("{row:width$} ", width = number_column_width),
                            );
                            if ui.is_item_clicked()
                                || self.drag_mode == Some(DragMode::Selection)
                                    && ui.is_mouse_hovering_rect(frame_rect[0], frame_rect[1])
                            {
                                self.drag_mode = Some(DragMode::Selection);
                                tas.select(row);
                            }

                            for (index, text) in buttons.iter().enumerate() {
                                let mut autofire = false;
                                if row == tas.selected_frame() {
                                    if let keybinds::Mode::Insert { pattern, .. }
                                    | keybinds::Mode::Replace { pattern, .. } = keybinds.mode()
                                    {
                                        autofire = pattern.autofire(
                                            tas.movie().input_ports(),
                                            0,
                                            0,
                                            index as u32,
                                        );
                                    }
                                }
                                let frame = &tas.frame(row)[0..input_port.frame_size()];
                                let pressed = if autofire {
                                    (ui.time() * 10.) as u64 % 2 != 0
                                } else {
                                    input_port.read(frame, 0, index as u32) != 0
                                };
                                let color = if pressed {
                                    Self::PRESSED_COLOR
                                } else {
                                    Self::UNPRESSED_COLOR
                                };

                                ui.same_line();
                                ui.text_colored(color, text);

                                if ui.is_item_clicked() {
                                    self.drag_mode = Some(DragMode::Input {
                                        index: index as u32,
                                        start: row,
                                        end: row,
                                        autofire: autofire_pressed,
                                    });
                                    let frame = &mut tas.frame_mut(row)[0..input_port.frame_size()];
                                    input_port.write(frame, 0, index as u32, !pressed as i16);
                                } else if let Some(DragMode::Input {
                                    index,
                                    start,
                                    end,
                                    autofire,
                                }) = &mut self.drag_mode
                                {
                                    #[expect(clippy::comparison_chain)]
                                    if ui.is_mouse_hovering_rect(frame_rect[0], frame_rect[1])
                                        && row != *end
                                    {
                                        // Determine inputs to toggle.

                                        // Is the user adding new rows to the selection?
                                        let expanding = if row > *start {
                                            row > *end
                                        } else if row < *start {
                                            row < *end
                                        } else {
                                            false
                                        };

                                        // Is the user removing rows from the selection?
                                        // (Note that it's possible for both expanding and
                                        //  contracting to be true. If the selection crosses the
                                        //  start pointer in one frame, the user is contracting the
                                        //  side they left and expanding the side they entered.
                                        let contracting = if *end > *start {
                                            row < *end
                                        } else if *end < *start {
                                            row > *end
                                        } else {
                                            false
                                        };

                                        // We need to toggle from the previously hovered row to the
                                        // newly hovered row. Whether these bounds are inclusive or
                                        // exclusive depends on whether we're expanding or
                                        // contracting the selection:
                                        //
                                        // If expanding, we want to include the current row, so
                                        //  that dragged-over rows are added to the selection.
                                        // If not, don't include the current row -- if the user
                                        //  drags away from a row, we want to deselect the row
                                        //  they dragged away from, not the row they dragged to.
                                        //
                                        // If contracting, we want to include the previous row.
                                        //  If not, we want to exclude the previous row.

                                        let min;
                                        let max;
                                        if *end < row {
                                            if contracting {
                                                min = *end
                                            } else {
                                                min = *end + 1;
                                            }
                                            if expanding {
                                                max = row + 1;
                                            } else {
                                                max = row;
                                            }
                                        } else {
                                            if contracting {
                                                max = *end + 1;
                                            } else {
                                                max = *end;
                                            }
                                            if expanding {
                                                min = row;
                                            } else {
                                                min = row + 1;
                                            }
                                        };
                                        for i in min..max {
                                            if i == *start
                                                || (*autofire && (i.abs_diff(*start)) % 2 != 0)
                                            {
                                                continue;
                                            }
                                            let frame =
                                                &mut tas.frame_mut(i)[0..input_port.frame_size()];
                                            input_port.write(
                                                frame,
                                                0,
                                                *index,
                                                (input_port.read(frame, 0, *index) == 0) as i16,
                                            );
                                        }
                                        *end = row;
                                    }

                                    if *autofire != autofire_pressed {
                                        *autofire = autofire_pressed;
                                        for i in (*start).min(*end)..=(*start).max(*end) {
                                            if i.abs_diff(*start) % 2 != 0 {
                                                let frame = &mut tas.frame_mut(i)
                                                    [0..input_port.frame_size()];
                                                input_port.write(
                                                    frame,
                                                    0,
                                                    *index,
                                                    (input_port.read(frame, 0, *index) == 0) as i16,
                                                );
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    });

                let (highlight, text) = match keybinds.mode() {
                    keybinds::Mode::Normal { .. } => ([0., 1., 0.5], "NORMAL"),
                    keybinds::Mode::Insert { .. } => ([0., 0.5, 1.], "INSERT"),
                    keybinds::Mode::Replace { .. } => ([0.75, 0., 0.25], "REPLACE"),
                };
                ui.set_cursor_pos([ui.cursor_pos()[0], ui.cursor_pos()[1] + padding[1]]);
                ui.get_window_draw_list()
                    .add_rect(
                        ui.cursor_screen_pos(),
                        [
                            ui.cursor_screen_pos()[0] + ui.content_region_max()[0],
                            ui.window_pos()[1] + ui.window_size()[1],
                        ],
                        highlight,
                    )
                    .filled(true)
                    .build();
                ui.set_cursor_pos([ui.cursor_pos()[0] + padding[0], ui.cursor_pos()[1]]);
                ui.text_colored([0., 0., 0., 1.], text);
            });
        self.last_selection = tas.selected_frame();
    }
}
