use imgui::{ListClipper, Ui};

use crate::tas::{input::InputPort, Tas};

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
    Input { index: u32, last: u32 },
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

    pub fn draw(&mut self, ui: &Ui, tas: &mut Tas) {
        if !ui.is_mouse_down(imgui::MouseButton::Left) {
            self.drag_mode = None;
        }

        ui.window("Piano Roll")
            .size([256., 768.], imgui::Condition::FirstUseEver)
            .build(|| {
                let rows = tas.movie().len();

                let _style = ui.push_style_var(imgui::StyleVar::ItemSpacing([0., 0.]));
                if tas.selected_frame() != self.last_selection || self.pending_scroll_lock.is_some()
                {
                    self.scroll_lock = self.pending_scroll_lock.take().unwrap_or(self.scroll_lock);
                    ui.set_scroll_from_pos_y_with_ratio(
                        ui.cursor_start_pos()[1]
                            + (tas.selected_frame() as f64
                                * ui.text_line_height_with_spacing() as f64)
                                as f32,
                        self.scroll_lock.ratio(),
                    );
                }

                let clipper = ListClipper::new(rows.try_into().unwrap()).begin(ui);

                let input_port = tas.movie().input_port();
                let buttons = match input_port {
                    InputPort::Joypad(j) => j.buttons(),
                };
                let number_column_width = rows.max(1000).saturating_sub(1).ilog10() as usize + 1;

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
                                    ui.cursor_screen_pos()[0] + ui.window_size()[0],
                                    ui.cursor_screen_pos()[1] + ui.text_line_height_with_spacing(),
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
                        match row.cmp(&tas.selected_frame()) {
                            std::cmp::Ordering::Less => tas.select_prev(tas.selected_frame() - row),
                            std::cmp::Ordering::Greater => {
                                tas.select_next(row - tas.selected_frame())
                            }
                            std::cmp::Ordering::Equal => {}
                        }
                    }

                    for (index, text) in buttons.iter().enumerate() {
                        let frame = tas.frame(row);
                        let pressed = input_port.read(frame, 0, index as u32);
                        let color = if pressed != 0 {
                            Self::PRESSED_COLOR
                        } else {
                            Self::UNPRESSED_COLOR
                        };

                        ui.same_line();
                        ui.text_colored(color, text);

                        if ui.is_item_clicked() {
                            self.drag_mode = Some(DragMode::Input {
                                index: index as u32,
                                last: row,
                            });
                            let frame = tas.frame_mut(row);
                            input_port.write(frame, 0, index as u32, (pressed == 0) as i16);
                        } else if let Some(DragMode::Input { index, last }) = &mut self.drag_mode {
                            if ui.is_mouse_hovering_rect(frame_rect[0], frame_rect[1])
                                && row != *last
                            {
                                // toggle all inputs between last and row,
                                // inclusive of row but exclusive of last
                                let min = std::cmp::min(row, *last + 1);
                                let max = std::cmp::max(row + 1, *last);
                                for i in min..max {
                                    let frame = tas.frame_mut(i);
                                    input_port.write(
                                        frame,
                                        0,
                                        *index,
                                        (input_port.read(frame, 0, *index) == 0) as i16,
                                    );
                                }
                                *last = row;
                            }
                        }
                    }
                }
            });
        self.last_selection = tas.selected_frame();
    }
}
