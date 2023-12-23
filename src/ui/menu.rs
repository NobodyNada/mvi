use std::collections::HashSet;

use std::collections::HashMap;

use super::*;

impl Ui {
    pub(super) fn draw_menu(&mut self, ui: &imgui::Ui) {
        ui.menu_bar(|| {
            ui.menu("File", || {
                if ui.menu_item("Load ROM...") {
                    self.load_rom();
                }
            });
        });
    }

    pub(super) fn draw_core_selector(&mut self, ui: &imgui::Ui) {
        let Some(core_selector) = &mut self.core_selector else {
            return;
        };
        let db = self
            .core_db
            .as_ref()
            .expect("Cannot select cores with no core database");

        const ID: &str = "Select Core";
        Ui::set_popup_position(ui);
        Ui::set_popup_size(ui, ui.window_size());
        if let Some(_) = ui.begin_modal_popup(ID) {
            if let Some(_) = ui
                .child_window("core_list")
                .size([0., -ui.text_line_height_with_spacing() * 4.])
                .begin()
            {
                if let Some(_) = ui.begin_table(
                    "Available cores",
                    2 + core_selector.show_experimental as usize
                        + core_selector.show_non_matching as usize,
                ) {
                    ui.table_setup_column("Core Name");
                    ui.table_setup_column_with(imgui::TableColumnSetup {
                        name: "Downloaded",
                        flags: imgui::TableColumnFlags::WIDTH_FIXED,
                        init_width_or_weight: ui.calc_text_size("Downloaded")[0],
                        ..Default::default()
                    });
                    if core_selector.show_experimental {
                        ui.table_setup_column_with(imgui::TableColumnSetup {
                            name: "Stable",
                            flags: imgui::TableColumnFlags::WIDTH_FIXED,
                            init_width_or_weight: ui.calc_text_size("Stable")[0],
                            ..Default::default()
                        });
                    }
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
                        if core.experimental && !core_selector.show_experimental {
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
                                return;
                            }
                        }

                        ui.table_next_column();
                        if core_selector.downloaded_cores.contains(&core.id) {
                            ui.text("Yes")
                        }

                        if core_selector.show_experimental {
                            ui.table_next_column();
                            if !core.experimental {
                                ui.text("Yes");
                            }
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

            ui.checkbox(
                "Show experimental cores",
                &mut core_selector.show_experimental,
            );
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
                return;
            }
            ui.same_line();
            ui.enabled(core_selector.selected_core_id.is_some(), || {
                if ui.button("Select") {
                    let core_selector = self.core_selector.take().unwrap();
                    (core_selector.callback)(self, &core_selector.selected_core_id.unwrap());
                }
            });
        } else {
            ui.open_popup(ID);
        }
    }

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
                ui.download_core(core_id, move |ui, core_path| {
                    ui.handle_error(|ui| -> anyhow::Result<()> {
                        let core_path = core_path?;
                        ui.tas = None;
                        ui.framebuffer = None;
                        let core = unsafe { core::Core::load(&core_path, &rom_path)? };
                        ui.tas = Some(Tas::new(core)?);
                        anyhow::Result::Ok(())
                    })
                })
            })
        });
    }

    fn select_core<F: FnOnce(&mut Ui, &str) + Send + 'static>(
        &mut self,
        extension: Option<String>,
        callback: F,
    ) -> Result<(), core::info::Error> {
        self.core_selector = Some(CoreSelector {
            extension,
            show_experimental: false,
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
        self.tokio.spawn(async move {
            tx.try_send((
                core::info::CoreDb::download_core(id, Arc::downgrade(&progress), false).await,
                Box::new(callback),
            ))
            .unwrap()
        });
    }
}
