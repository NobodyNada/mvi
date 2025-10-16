use anyhow::Context;

use crate::ui;

use super::*;

// TODO: conflict detection

pub struct KeybindEditor {
    config: KeybindConfiguration,
    default: KeybindConfiguration,

    recording: Option<RecordTarget>,
    recorded_keys: Vec<(Key, ModifiersState)>,
    append: bool,
}

enum RecordTarget {
    Controller {
        port: usize,
        device: tas::input::InputPort,
        index: usize,
    },
    Global(String),
    Normal(String),
    Insert(String),
}

impl KeybindEditor {
    /// Initializes the keybind editor.
    pub fn new(bindings: &mut Keybinds) -> Self {
        // Load an empty keybinding configuration to populate the defaults.
        let mut default_bindings = Default::default();
        bindings.register_keybinds(&mut default_bindings);
        KeybindEditor {
            default: default_bindings,
            // Reload the real keybinding configuration.
            config: bindings.load_config(),
            recorded_keys: Vec::new(),
            recording: None,
            append: false,
        }
    }

    /// Saves the changes made in the keybind editor.
    pub fn apply(mut self, bindings: &mut Keybinds) -> anyhow::Result<()> {
        bindings.register_keybinds(&mut self.config);
        Keybinds::save_config(&self.config).context("Could not save keybinds to disk")
    }

    // Handles user keypresses while the editor is open.
    pub fn key_down(&mut self, key: Key, modifiers: ModifiersState) {
        // Are we recording?
        match self.recording {
            // Yes, we're recording a controller input. Controller inputs currently don't support
            // sequences, so just replace the binding.
            Some(RecordTarget::Controller { .. }) => self.recorded_keys = vec![(key, modifiers)],

            // Yes, we're recording an action. Add the new keypress to the action's sequence.
            Some(_) => self.recorded_keys.push((key, modifiers)),

            // No, do nothing.
            _ => {}
        }
    }

    /// Renders the keybind editor.
    pub fn draw(&mut self, ui: &imgui::Ui, modifiers: ModifiersState) -> Option<bool> {
        // If we're recording, draw the recording modal.
        if let Some(target) = &self.recording {
            const ID: &str = "Record Keybind";
            ui::Ui::set_popup_size(ui, [300., 100.]);
            match ui.begin_modal_popup(ID) { Some(_token) => {
                match target {
                    // What's the name of the thing we're recording?
                    RecordTarget::Global(s) | RecordTarget::Normal(s) | RecordTarget::Insert(s) => {
                        ui.text(format!("Enter keybind for '{s}'"));
                    }
                    RecordTarget::Controller {
                        port: _,
                        device,
                        index,
                    } => {
                        let tas::input::InputPort::Joypad(joypad) = device;
                        ui.text(format!(
                            "Enter keybind for '{device} {}'",
                            joypad.buttons()[*index]
                        ))
                    }
                };
                Self::draw_binding(ui, &self.recorded_keys);

                if ui.button("Save") {
                    // Write the keybind to the target.
                    let mut apply = |target: &mut Vec<_>| {
                        if self.append {
                            target.push(std::mem::take(&mut self.recorded_keys));
                        } else {
                            *target = vec![std::mem::take(&mut self.recorded_keys)];
                        }
                    };
                    match self.recording.take().unwrap() {
                        RecordTarget::Global(target) => {
                            apply(self.config.global_bindings.entry(target).or_default())
                        }
                        RecordTarget::Normal(target) => {
                            apply(self.config.normal_bindings.entry(target).or_default())
                        }
                        RecordTarget::Insert(target) => {
                            apply(self.config.insert_bindings.entry(target).or_default())
                        }
                        RecordTarget::Controller {
                            port,
                            device,
                            index,
                        } => {
                            let mut keys = std::mem::take(&mut self.recorded_keys);
                            assert!(keys.len() <= 1);
                            let key = keys.drain(..).next().map(|(k, _)| k);
                            self.config.controller_bindings[port]
                                .get_mut(&device)
                                .unwrap()[index] = key
                        }
                    }
                    ui.close_current_popup();
                }
                ui.same_line();
                if ui.button("Clear") {
                    self.recorded_keys = Vec::new();
                }
                ui.same_line();
                if ui.button("Cancel") {
                    self.recorded_keys = Vec::new();
                    self.recording = None;
                    ui.close_current_popup();
                }
            } _ => {
                ui.open_popup(ID);
            }}
        }

        if let Some(_token) = ui
            .child_window("keybind_list")
            .size([0., -ui.text_line_height_with_spacing() * 6.])
            .begin()
        {
            // Draw the controller binding table.
            if ui.collapsing_header("Controllers", imgui::TreeNodeFlags::FRAMED) {
                for (port_index, port) in self.config.controller_bindings.iter_mut().enumerate() {
                    for (device, buttons) in port {
                        let tas::input::InputPort::Joypad(joypad) = device;
                        let device_name = format!("{device}");
                        if ui.collapsing_header(&device_name, imgui::TreeNodeFlags::BULLET)
                            && let Some(_token) = ui.begin_table(device_name, 2) {
                                ui.table_setup_column("Button");
                                ui.table_setup_column("Key Binding");
                                ui.table_headers_row();

                                for (i, key) in buttons.iter_mut().enumerate() {
                                    ui.table_next_column();

                                    let selected = ui
                                        .selectable_config(format!("{}##{i}", joypad.buttons()[i]))
                                        .span_all_columns(true)
                                        .allow_double_click(true)
                                        .build();
                                    if selected
                                        && ui.is_mouse_double_clicked(imgui::MouseButton::Left)
                                    {
                                        if modifiers.control_key() {
                                            *key = None;
                                        } else if modifiers.alt_key() {
                                            *key = self.default.controller_bindings[port_index]
                                                [device][i]
                                                .clone();
                                        } else {
                                            self.recording = Some(RecordTarget::Controller {
                                                port: port_index,
                                                device: *device,
                                                index: i,
                                            });
                                        }
                                    }
                                    ui.table_next_column();
                                    if let Some(key) = key {
                                        Self::draw_binding(
                                            ui,
                                            &[(key.clone(), ModifiersState::empty())],
                                        );
                                    }
                                }
                            }
                    }
                }
            }

            // Draw each of the action binding tables.
            for (label, bindings, default, record_target) in [
                (
                    "Global bindings",
                    &mut self.config.global_bindings,
                    &self.default.global_bindings,
                    RecordTarget::Global as fn(String) -> RecordTarget,
                ),
                (
                    "Normal mode",
                    &mut self.config.normal_bindings,
                    &self.default.normal_bindings,
                    RecordTarget::Normal as fn(String) -> RecordTarget,
                ),
                (
                    "Insert/replace mode",
                    &mut self.config.insert_bindings,
                    &self.default.insert_bindings,
                    RecordTarget::Insert as fn(String) -> RecordTarget,
                ),
            ] {
                if ui.collapsing_header(label, imgui::TreeNodeFlags::FRAMED)
                    && let Some(_token) = ui.begin_table(label, 2) {
                        ui.table_setup_column("Action");
                        ui.table_setup_column("Key Binding");
                        ui.table_headers_row();

                        for (action, bindings) in bindings.iter_mut() {
                            ui.table_next_column();
                            let selected = ui
                                .selectable_config(action)
                                .span_all_columns(true)
                                .allow_double_click(true)
                                .build();
                            if selected && ui.is_mouse_double_clicked(imgui::MouseButton::Left) {
                                if modifiers.control_key() {
                                    *bindings = Vec::new();
                                } else if modifiers.alt_key() {
                                    // Restore default binding
                                    *bindings = default.get(action).unwrap_or(&Vec::new()).clone();
                                } else {
                                    self.append = modifiers.shift_key();
                                    self.recording = Some(record_target(action.clone()));
                                }
                            }

                            ui.table_next_column();
                            Self::draw_bindings(ui, bindings);

                            ui.table_next_row();
                        }
                    }
            }
        }

        ui.text("Double-click to replace bindings");
        ui.text("Shift+double-click to add a binding");
        ui.text("Control+double-click to clear bindings");
        ui.text("Alt+double-click to reset to default");

        if ui.button("Save") {
            return Some(true);
        }
        ui.same_line();
        if ui.button("Cancel") {
            return Some(false);
        }
        None
    }

    fn draw_bindings(ui: &imgui::Ui, bindings: &[Vec<(Key, ModifiersState)>]) {
        let mut bindings = bindings.iter().peekable();
        if bindings.peek().is_none() {
            ui.text("");
        }
        while let Some(binding) = bindings.next() {
            Self::draw_binding(ui, binding);
            if bindings.peek().is_some() {
                ui.same_line_with_spacing(0., 0.);
                ui.text_colored([0.5, 0.5, 0.5, 1.], "; ");
                ui.same_line_with_spacing(0., 0.);
            }
        }
    }

    fn draw_binding(ui: &imgui::Ui, binding: &[(Key, ModifiersState)]) {
        let mut keys = binding.iter().peekable();
        if keys.peek().is_none() {
            ui.text("");
        }
        while let Some((key, mods)) = keys.next() {
            if mods.control_key() {
                ui.text("Ctrl-");
                ui.same_line_with_spacing(0., 0.);
            }
            if mods.shift_key() {
                ui.text("Shift-");
                ui.same_line_with_spacing(0., 0.);
            }
            if mods.super_key() {
                ui.text("Super-");
                ui.same_line_with_spacing(0., 0.);
            }
            if mods.alt_key() {
                ui.text("Alt-");
                ui.same_line_with_spacing(0., 0.);
            }
            match key {
                Key::Character(s) => ui.text(s),
                Key::Named(k) => ui.text(format!("{k:?}")),
                _ => ui.text(format! {"{key:?}"}),
            };
            if keys.peek().is_some() {
                ui.same_line_with_spacing(0., 0.);
                ui.text(" ");
                ui.same_line_with_spacing(0., 0.);
            }
        }
    }
}
