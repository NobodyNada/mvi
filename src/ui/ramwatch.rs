use imgui::Ui;

use crate::tas::{Tas, movie};

pub struct RamWatch {
    pub opened: bool,
    editor: Editor,
    dragged_item: Option<usize>,
}

struct Editor {
    state: EditorState,
    should_focus: bool,
    name: String,
    address: String,
    format: movie::RamWatchFormat,
}

#[derive(Copy, Clone, PartialEq)]
enum EditorState {
    Closed,
    Add,
    Edit(usize),
}

impl Editor {
    fn init_from_ram_watch(&mut self, watch: &movie::RamWatch) {
        self.name = watch.name.clone();
        self.address = format!("{:#04X}", watch.address);
        self.format = watch.format.clone();
    }

    fn to_ram_watch(&self) -> Option<movie::RamWatch> {
        let name = self.name.trim().to_string();
        if name.is_empty() {
            return None;
        }
        let address =
            usize::from_str_radix(self.address.strip_prefix("0x").unwrap_or(&self.address), 16)
                .ok()?;
        Some(movie::RamWatch {
            name,
            address,
            format: self.format.clone(),
        })
    }
}

impl Default for Editor {
    fn default() -> Self {
        Editor {
            state: EditorState::Closed,
            should_focus: false,
            name: String::new(),
            address: String::new(),
            format: movie::RamWatchFormat {
                width: 1,
                hex: true,
                signed: false,
            },
        }
    }
}

impl RamWatch {
    pub fn new() -> RamWatch {
        RamWatch {
            opened: true,
            editor: Editor::default(),
            dragged_item: None,
        }
    }

    pub fn draw(&mut self, ui: &Ui, tas: &mut Tas) -> bool {
        let ignore_events;

        // Draw the editor, if it's open.
        const ADD_POPUP: &str = "Add Memory Watch";
        if self.editor.state != EditorState::Closed {
            // Don't allow keydown events while the editor is open!
            ignore_events = true;
            super::Ui::set_popup_size(ui, [256., 256.]);
            match ui
                .modal_popup_config(ADD_POPUP)
                .resizable(false)
                .begin_popup()
            {
                Some(_token) => {
                    // Focus on the name field when the editor is first opened.
                    if self.editor.should_focus {
                        self.editor.should_focus = false;
                        ui.set_keyboard_focus_here();
                    }
                    ui.input_text("Name", &mut self.editor.name).build();

                    ui.input_text("Address", &mut self.editor.address).build();

                    let group = ui.begin_group();
                    ui.columns(2, "ram_watch_editor", false);

                    ui.text("Size");
                    ui.radio_button("8 bits", &mut self.editor.format.width, 1);
                    ui.radio_button("16 bits", &mut self.editor.format.width, 2);
                    ui.radio_button("32 bits", &mut self.editor.format.width, 3);
                    ui.radio_button("64 bits", &mut self.editor.format.width, 4);

                    ui.next_column();

                    ui.text("Format");
                    ui.radio_button("Hex", &mut self.editor.format.hex, true);
                    ui.radio_button("Decimal", &mut self.editor.format.hex, false);

                    ui.checkbox("Signed", &mut self.editor.format.signed);
                    group.end();

                    ui.columns(1, "ram_watch_confirm", false);
                    let button_width = ui.calc_text_size("  Cancel  OK  ")[0];
                    ui.set_cursor_pos([
                        ui.window_size()[0] - button_width,
                        ui.window_size()[1] - ui.text_line_height_with_spacing() * 2.,
                    ]);

                    if ui.button("Cancel") || ui.is_key_pressed(imgui::Key::Escape) {
                        ui.close_current_popup();
                        self.editor.state = EditorState::Closed;
                    }
                    ui.same_line();

                    let watch = self.editor.to_ram_watch();
                    ui.enabled(watch.is_some(), || {
                        if ui.button("OK")
                            || (watch.is_some() && ui.is_key_pressed(imgui::Key::Enter))
                        {
                            ui.close_current_popup();
                            let watch = watch.unwrap();
                            match self.editor.state {
                                EditorState::Closed => unreachable!(),
                                EditorState::Add => tas.ramwatches_mut().push(watch),
                                EditorState::Edit(i) => tas.ramwatches_mut()[i] = watch,
                            }
                            self.editor.state = EditorState::Closed;
                        }
                    });
                }
                _ => {
                    ui.open_popup(ADD_POPUP);
                    self.editor.should_focus = true;
                }
            }
        } else {
            ignore_events = false;
        }

        // Draw the RAM watch view.
        if self.opened {
            ui.window("Memory Watch")
                .opened(&mut self.opened)
                .size([128., 448.], imgui::Condition::FirstUseEver)
                .build(|| {
                    if let Some(_token) = ui.begin_table("ram_watches", 3) {
                        // 3 columns: name, value, delete
                        ui.table_setup_column("Name");
                        ui.table_setup_column_with(imgui::TableColumnSetup {
                            name: "Value",
                            flags: imgui::TableColumnFlags::WIDTH_FIXED,
                            init_width_or_weight: ui.calc_text_size("00000000")[0],
                            ..Default::default()
                        });
                        ui.table_setup_column_with(imgui::TableColumnSetup {
                            name: " ",
                            flags: imgui::TableColumnFlags::WIDTH_FIXED,
                            init_width_or_weight: ui.calc_text_size(" ")[0],
                            ..Default::default()
                        });
                        ui.table_headers_row();

                        // Which item had its delete button clicked.
                        let mut item_to_delete = None;

                        // Which item is currently hovered.
                        let mut hovered_item = None;
                        for (i, watch) in tas.movie().ramwatches.iter().enumerate() {
                            ui.table_next_column();

                            // Is this row hovered?
                            let row_rect = unsafe {
                                [
                                    [
                                        ui.cursor_screen_pos()[0],
                                        ui.cursor_screen_pos()[1]
                                            - ui.style().item_spacing[1] * 0.5,
                                    ],
                                    [
                                        ui.window_pos()[0] + ui.window_content_region_max()[0],
                                        ui.cursor_screen_pos()[1]
                                            + ui.text_line_height()
                                            + ui.style().item_spacing[1] * 0.5,
                                    ],
                                ]
                            };
                            let mouse_pos = ui.io().mouse_pos;
                            let row_hovered = (row_rect[0][0]..row_rect[1][0])
                                .contains(&mouse_pos[0])
                                && (row_rect[0][1]..row_rect[1][1]).contains(&mouse_pos[1]);
                            if row_hovered {
                                hovered_item = Some(i);
                            }

                            ui.text(&watch.name);

                            ui.table_next_column();
                            if let Some(v) = tas.read_ram_watch(watch) {
                                ui.text(watch.format.format_value(v));
                            }

                            ui.table_next_column();

                            let delete_text = if row_hovered { "x" } else { " " };
                            ui.text(delete_text);
                            if ui.is_item_clicked() {
                                item_to_delete = Some(i);
                            } else if row_hovered {
                                // If the delete button was NOT clicked, handle dragging items
                                // around.
                                if self.dragged_item.is_some()
                                    && ui.is_mouse_dragging(imgui::MouseButton::Left)
                                {
                                    // We're dragging, so draw a line
                                    ui.get_foreground_draw_list()
                                        .add_line(
                                            row_rect[0],
                                            [row_rect[1][0], row_rect[0][1]],
                                            (0., 0., 1.),
                                        )
                                        .build();
                                } else if ui.is_mouse_clicked(imgui::MouseButton::Left) {
                                    // Note that we consider a drag to start on a click event, but
                                    // we don't actually draw visual guides until the item has been
                                    // dragged some distance.
                                    self.dragged_item = Some(i);
                                }

                                if ui.is_mouse_double_clicked(imgui::MouseButton::Left) {
                                    // On double-click, edit the current item.
                                    self.editor.state = EditorState::Edit(i);
                                    self.editor.init_from_ram_watch(watch);
                                    self.dragged_item = None;
                                }
                            }
                            ui.table_next_row();
                        }

                        // Now that we've drawn all the items, handle updates.
                        if let Some(src) = self.dragged_item {
                            if !ui.is_mouse_down(imgui::MouseButton::Left) {
                                if let Some(dst) = hovered_item {
                                    if src < dst {
                                        tas.ramwatches_mut()[src..=dst].rotate_left(1);
                                    } else {
                                        tas.ramwatches_mut()[dst..=src].rotate_right(1);
                                    }
                                }
                                self.dragged_item = None;
                            }
                        } else if let Some(i) = item_to_delete {
                            tas.ramwatches_mut().remove(i);
                        }

                        // Draw the add button.
                        ui.table_next_column();
                        if ui
                            .selectable_config("Add...")
                            .flags(imgui::SelectableFlags::SPAN_ALL_COLUMNS)
                            .build()
                        {
                            self.editor.state = EditorState::Add;
                        }
                    }
                });
        }

        ignore_events
    }

    pub fn menu(&mut self, ui: &Ui, tas: &mut Tas) -> anyhow::Result<()> {
        ui.checkbox("Memory Watch", &mut self.opened);
        if ui.menu_item("Import watches...")
            && let Some(path) = rfd::FileDialog::new()
                .add_filter("JSON file", &["json"])
                .set_title("Select a memory watch file")
                .pick_file()
        {
            *tas.ramwatches_mut() = serde_json::from_reader(std::fs::File::open(path)?)?;
        }
        if ui.menu_item("Export watches...")
            && let Some(path) = rfd::FileDialog::new()
                .add_filter("JSON file", &["json"])
                .set_title("Select a memory watch file")
                .set_file_name("ram_watch")
                .save_file()
        {
            serde_json::to_writer_pretty(std::fs::File::create(path)?, tas.ramwatches_mut())?;
        }

        Ok(())
    }
}
