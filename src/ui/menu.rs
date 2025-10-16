use std::collections::HashMap;
use std::collections::HashSet;

use imgui::ItemHoveredFlags;
use sha2::Digest;

use super::*;

#[expect(clippy::type_complexity)]
pub(super) struct CoreSelector {
    extension: Option<String>,
    show_non_matching: bool,
    selected_core_id: Option<String>,
    downloaded_cores: HashSet<String>,
    callback: Box<dyn FnOnce(&mut Ui, &str) + Send>,
}

pub(super) struct HashMismatch {
    movie_file: tas::movie::file::MovieFile,
    movie_path: std::path::PathBuf,
    rom_path: std::path::PathBuf,
    expected_hash: String,
    actual_hash: String,
}

impl Ui {
    pub(super) fn draw_menu(&mut self, ui: &imgui::Ui) {
        ui.menu_bar(|| {
            ui.menu("File", || {
                if ui.menu_item("New Movie...") {
                    self.load_rom();
                }
                if ui.menu_item("Open Movie...") {
                    if let Some(path) = rfd::FileDialog::new()
                        .add_filter("mvi movie file", &["mvi"])
                        .set_title("Select a movie file")
                        .pick_file()
                    {
                        self.open_movie(path)
                    }
                }
                if !self.movie_cache.recents().is_empty() {
                    ui.separator();
                    ui.text_disabled("Recent Movies");
                    for path in self.movie_cache.recents() {
                        let label = format!(
                            "{} ({})",
                            path.file_name().unwrap_or_default().to_string_lossy(),
                            path.parent()
                                .map(|p| p.to_string_lossy())
                                .unwrap_or_default()
                        );

                        if ui.menu_item(label) {
                            self.open_movie(path.clone());
                            break;
                        }
                    }
                }
                ui.separator();
                ui.enabled(self.tas.is_some(), || {
                    let mut save_as = false;
                    let mut save_path = None;
                    if ui.menu_item("Save") {
                        save_path = self.tas.as_ref().unwrap().movie().movie_path.clone();
                        save_as = save_path.is_none();
                    }
                    if ui.menu_item("Save As...") || save_as {
                        let rom_name = &self.tas.as_ref().unwrap().movie().rom_filename;
                        let rom_name = rom_name
                            .rsplit_once(".")
                            .map(|(name, _ext)| name)
                            .unwrap_or(rom_name);

                        save_path = rfd::FileDialog::new()
                            .add_filter("mvi movie file", &["mvi"])
                            .set_file_name(format!("{rom_name}.mvi"))
                            .save_file();
                    }
                    if let Some(path) = save_path {
                        self.handle_error(|ui| {
                            let tas = ui.tas.as_mut().unwrap();
                            tas.movie().save(&path)?;
                            tas.set_movie_path(Some(path.clone()));
                            ui.movie_cache.update(
                                path,
                                tas.movie().rom_path.clone(),
                                tas.movie().uuid,
                            )?;
                            Ok(())
                        });
                    }
                });
            });
            ui.menu("Config", || {
                if ui.menu_item("Keybinds...") {
                    self.keybind_editor = Some(keybinds::KeybindEditor::new(&mut self.keybinds));
                }
            });
            if self.tas.is_some() {
                ui.menu("Debug", || {
                    self.handle_error(|s| {
                        let tas = s.tas.as_mut().unwrap();
                        s.ramwatch.menu(ui, tas)?;

                        ui.separator();
                        ui.enabled(tas.can_trace(), || {
                            if ui.menu_item("Trace Debugger...") {
                                if let Some(t) = &mut s.trace_debuger {
                                    t.focus();
                                } else {
                                    s.trace_debuger = Some(debug::trace::TraceDebugger::new());
                                }
                            }
                        });
                        if !tas.can_trace()
                            && ui.is_item_hovered_with_flags(ItemHoveredFlags::ALLOW_WHEN_DISABLED)
                        {
                            ui.tooltip_text("The current core does not support tracing.");
                        }
                        Ok(())
                    });
                });
            }
        });
    }

    // Creating Movies

    fn load_rom(&mut self) {
        let Some(db) = &self.core_db else { return };

        // Group extensions by system ID to create filters
        #[derive(Debug)]
        struct SystemExtensions<'a> {
            system_name: Option<&'a String>,
            supported_extensions: HashSet<&'a String>,
        }
        let mut extensions_by_system_id = HashMap::new();
        for core in db.cores() {
            extensions_by_system_id
                .entry(core.system_id.as_ref().or(core.system_name.as_ref()))
                .or_insert_with(|| SystemExtensions {
                    system_name: core.system_name.as_ref().or(core.system_id.as_ref()),
                    supported_extensions: HashSet::new(),
                })
                .supported_extensions
                .extend(&core.supported_extensions);
        }

        let mut extensions_by_system_id = extensions_by_system_id.into_iter().collect::<Vec<_>>();
        extensions_by_system_id.sort_by_key(|(s, _)| *s);

        let mut file_picker = rfd::FileDialog::new().set_title("Select ROM");
        for (
            _,
            SystemExtensions {
                system_name,
                supported_extensions,
            },
        ) in extensions_by_system_id
        {
            let name = system_name.map(|s| &**s).unwrap_or("Other");
            let mut extensions: Vec<_> = supported_extensions.into_iter().collect();
            extensions.sort();
            file_picker = file_picker.add_filter(name, &extensions)
        }

        let Some(rom_path) = file_picker.pick_file() else {
            return;
        };

        let extension = rom_path
            .extension()
            .and_then(|e| e.to_str())
            .map(str::to_string);

        self.handle_error(|ui| {
            ui.select_core(extension, move |ui, core_id| {
                let cid = core_id.to_string();
                ui.download_core(core_id, move |ui, core_path| {
                    let system_id = ui
                        .core_db
                        .as_ref()
                        .unwrap()
                        .cores()
                        .iter()
                        .find(|core| core.id == cid)
                        .unwrap()
                        .system_id
                        .clone();
                    ui.handle_error(|ui| {
                        ui.load_game(&core_path?, &rom_path, |core| Ok(Tas::new(core, system_id)))
                    })
                })
            })
            .map_err(Into::into)
        });
    }

    pub(super) fn draw_core_selector(&mut self, ui: &imgui::Ui) -> bool {
        let Some(core_selector) = &mut self.core_selector else {
            return false;
        };
        let db = self
            .core_db
            .as_ref()
            .expect("Cannot select cores with no core database");

        const ID: &str = "Select Core";
        Ui::set_popup_position(ui);
        Ui::set_popup_size(ui, ui.window_size());
        match ui.begin_modal_popup(ID) { Some(_token) => {
            if let Some(_token) = ui
                .child_window("core_list")
                .size([0., -ui.text_line_height_with_spacing() * 4.])
                .begin()
            {
                if let Some(_token) = ui.begin_table(
                    "Available cores",
                    2 + core_selector.show_non_matching as usize,
                ) {
                    ui.table_setup_column("Core Name");
                    ui.table_setup_column_with(imgui::TableColumnSetup {
                        name: "Downloaded",
                        flags: imgui::TableColumnFlags::WIDTH_FIXED,
                        init_width_or_weight: ui.calc_text_size("Downloaded")[0],
                        ..Default::default()
                    });
                    if core_selector.show_non_matching {
                        ui.table_setup_column_with(imgui::TableColumnSetup {
                            name: "Supports ROM",
                            flags: imgui::TableColumnFlags::WIDTH_FIXED,
                            init_width_or_weight: ui.calc_text_size("Supports ROM")[0],
                            ..Default::default()
                        });
                    }
                    ui.table_headers_row();

                    for core in db.cores() {
                        let supports_rom = core_selector
                            .extension
                            .as_ref()
                            .map(|e| core.supported_extensions.contains(e))
                            .unwrap_or(true);

                        if !supports_rom && !core_selector.show_non_matching {
                            continue;
                        }

                        ui.table_next_column();
                        let selected = ui
                            .selectable_config(&core.display_name)
                            .flags(imgui::SelectableFlags::SPAN_ALL_COLUMNS)
                            .selected(core_selector.selected_core_id.as_ref() == Some(&core.id))
                            .allow_double_click(true)
                            .build();
                        if selected {
                            core_selector.selected_core_id = Some(core.id.clone());
                            if ui.is_mouse_double_clicked(imgui::MouseButton::Left) {
                                let core_selector = self.core_selector.take().unwrap();
                                (core_selector.callback)(
                                    self,
                                    &core_selector.selected_core_id.unwrap(),
                                );
                                return true;
                            }
                        }

                        ui.table_next_column();
                        if core_selector.downloaded_cores.contains(&core.id) {
                            ui.text("Yes")
                        }

                        if core_selector.show_non_matching {
                            ui.table_next_column();
                            if supports_rom {
                                ui.text("Yes");
                            }
                        }

                        ui.table_next_row();
                    }
                }
            }

            ui.new_line();

            if let Some(extension) = &core_selector.extension {
                ui.checkbox(
                    format!("Show cores that don't support the selected ROM (\".{extension}\")"),
                    &mut core_selector.show_non_matching,
                );
            }

            let button_width = ui.calc_text_size("  Cancel  Select  ")[0];
            ui.same_line_with_pos(ui.window_size()[0] - button_width);
            if ui.button("Cancel") {
                ui.close_current_popup();
                self.core_selector = None;
                return true;
            }
            ui.same_line();
            ui.enabled(core_selector.selected_core_id.is_some(), || {
                if ui.button("Select") {
                    let core_selector = self.core_selector.take().unwrap();
                    (core_selector.callback)(self, &core_selector.selected_core_id.unwrap());
                }
            });
        } _ => {
            ui.open_popup(ID);
        }}

        true
    }

    fn select_core<F: FnOnce(&mut Ui, &str) + Send + 'static>(
        &mut self,
        extension: Option<String>,
        callback: F,
    ) -> Result<(), core::info::Error> {
        self.core_selector = Some(CoreSelector {
            extension,
            show_non_matching: false,
            selected_core_id: None,
            downloaded_cores: core::info::CoreDb::find_installed_core_ids()?,
            callback: Box::new(callback),
        });

        Ok(())
    }

    fn download_core<
        F: FnOnce(&mut Ui, core::info::Result<std::path::PathBuf>) + Send + 'static,
    >(
        &mut self,
        id: &str,
        callback: F,
    ) {
        assert!(self.download_progress.is_none());

        let (tx, rx) = std::sync::mpsc::sync_channel(1);
        let progress: Arc<AtomicCell<core::info::Progress>> = Arc::default();

        self.download_progress = Some(Download {
            item: DownloadItem::Core {
                id: id.to_string(),
                rx,
            },
            progress: progress.clone(),
        });

        let id = id.to_string();
        let download =
            self.core_db
                .as_ref()
                .unwrap()
                .download_core(id, Arc::downgrade(&progress), false);

        self.tokio
            .spawn(async move { tx.try_send((download.await, Box::new(callback))).unwrap() });
    }

    // Opening Movies

    fn open_movie(&mut self, path: std::path::PathBuf) {
        // **1.** Deserialize the movie file from disk
        self.handle_error(|ui| {
            let file = tas::movie::file::MovieFile::load(std::fs::File::open(&path)?)?;
            ui.open_movie_file(file, path, true)
        });
    }

    fn open_movie_file(
        &mut self,
        file: tas::movie::file::MovieFile,
        movie_path: std::path::PathBuf,
        allow_cached: bool,
    ) -> anyhow::Result<()> {
        // **2.** Select a ROM file (unless we have a cached movie file.)
        let cached_path = if allow_cached {
            self.movie_cache
                .rom_path_for_uuid(file.uuid)
                .filter(|p| p.exists())
                .map(|p| p.to_owned())
        } else {
            None
        };

        let rom_path = cached_path.or_else(|| {
            // No cached file, prompt the user
            // Filter files by extensions supported by the movie's core
            let mut extensions = self
                .core_db
                .as_ref()
                .unwrap()
                .cores()
                .iter()
                .find(|core| core.id == file.core_id)
                .map(|core| {
                    core.supported_extensions
                        .iter()
                        .cloned()
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();

            // Also include the extension of the ROM filename associated with the movie
            if let Some((_, e)) = file.rom_filename.rsplit_once(".") {
                if !extensions.iter().any(|ext| ext == e) {
                    extensions.push(e.to_string());
                }
            }
            extensions.sort();

            rfd::FileDialog::new()
                .add_filter("ROM files", &extensions)
                .set_title(format!("Locate ROM '{}'", file.rom_filename))
                .pick_file()
        });

        let Some(rom_path) = rom_path else {
            // No cached ROM was available, and the user did not select one.
            return Ok(());
        };

        // **3.** See if its hash matches.
        let rom_data = std::fs::read(&rom_path)?;
        let hash: [u8; 32] = sha2::Sha256::digest(&rom_data).into();

        if hash == file.rom_sha256 {
            // The hash matched, proceed to load the core
            self.open_movie_with_rom(file, movie_path, rom_path);
        } else {
            // The hash did not match, ask the user what the would like to do
            fn to_hex_string(hash: [u8; 32]) -> String {
                let mut result = String::with_capacity(hash.len() * 2);
                for byte in hash {
                    for digit in [byte >> 4, byte & 0xf] {
                        result.push(char::from_digit(digit as u32, 16).unwrap());
                    }
                }
                result
            }
            self.hash_mismatch = Some(HashMismatch {
                expected_hash: to_hex_string(file.rom_sha256),
                actual_hash: to_hex_string(hash),
                movie_file: file,
                rom_path,
                movie_path,
            })
        }
        Ok(())
    }

    pub(super) fn draw_hash_mismatch(&mut self, ui: &imgui::Ui) -> bool {
        if let Some(mismatch) = self.hash_mismatch.as_mut() {
            const ID: &str = "Hash mismatch";
            match ui.begin_modal_popup(ID) { Some(_token) => {
                ui.text(format!(
                    "The selected ROM '{}' is not the same ROM that the movie was created with.",
                    mismatch.rom_path.to_string_lossy()
                ));

                ui.text("ROM hash: ");
                ui.same_line();
                ui.text(&mismatch.actual_hash);

                ui.text("Expected hash: ");
                ui.same_line();
                ui.text(&mismatch.expected_hash);

                if ui.button("Select another ROM") {
                    ui.close_current_popup();
                    let mismatch = self.hash_mismatch.take().unwrap();
                    self.handle_error(|ui| {
                        ui.open_movie_file(mismatch.movie_file, mismatch.movie_path, false)
                    })
                }
                if ui.button("Load ROM anyway") {
                    ui.close_current_popup();
                    let mismatch = self.hash_mismatch.take().unwrap();
                    self.open_movie_with_rom(
                        mismatch.movie_file,
                        mismatch.movie_path,
                        mismatch.rom_path,
                    )
                }
                if ui.button("Cancel") {
                    ui.close_current_popup();
                    self.hash_mismatch = None;
                }
            } _ => {
                ui.open_popup(ID);
            }}
            return true;
        }
        false
    }

    fn open_movie_with_rom(
        &mut self,
        file: tas::movie::file::MovieFile,
        movie_path: std::path::PathBuf,
        rom_path: std::path::PathBuf,
    ) {
        self.download_core(&file.core_id.clone(), move |ui, core_path| {
            ui.handle_error(move |ui| {
                let core_path = core_path?;

                let uuid = file.uuid;

                ui.load_game(&core_path, &rom_path, |core| {
                    Tas::load(core, file, movie_path.clone())
                })?;
                ui.movie_cache.update(movie_path, rom_path, uuid)
            });
        })
    }
}
