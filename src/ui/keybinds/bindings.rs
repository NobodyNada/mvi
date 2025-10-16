use super::*;

use crate::ui::piano_roll;

impl Keybinds {
    /// Applies the given keybind configuration, populating any missing entries with their default
    /// values.
    pub(super) fn register_keybinds(&mut self, config: &mut KeybindConfiguration) {
        self.global_bindings = InputTrie::new();
        self.normal_bindings = InputTrie::new();
        self.insert_bindings = InputTrie::new();
        self.controller_bindings.clear();

        let global = RefCell::new((
            &mut config.global_bindings,
            &mut self.global_bindings,
            &|ctx: &Context| match ctx.keybinds.mode {
                Mode::Normal { count } => Some(count),
                _ => None,
            },
        ));
        let normal = RefCell::new((
            &mut config.normal_bindings,
            &mut self.normal_bindings,
            &|ctx: &Context| {
                let Mode::Normal { count } = ctx.keybinds.mode else {
                    unreachable!()
                };
                count
            },
        ));
        let insert = RefCell::new((
            &mut config.insert_bindings,
            &mut self.insert_bindings,
            &|ctx: &Context| match &ctx.keybinds.mode {
                Mode::Insert { pattern, action: _ } | Mode::Replace { pattern, action: _ } => {
                    pattern.clone()
                }
                _ => unreachable!(),
            },
        ));

        // Registers key sequences to trigger the given action.
        // `into`: The mode to register the action into.
        // `label`: The name of the action.
        // `default`: The key sequence to be used if none is specified.
        // `action`: A callback to run when the keybind is triggered.
        fn register_multiple<ModeState>(
            into: &RefCell<(
                &mut BTreeMap<String, Vec<Vec<(Key, ModifiersState)>>>,
                &mut InputTrie,
                &'static impl Fn(&Context) -> ModeState,
            )>,
            label: &str,
            default: Vec<Vec<(Key, ModifiersState)>>,
            mut action: impl FnMut(Context, ModeState) + 'static,
        ) {
            let into = &mut *into.borrow_mut();
            let state_handler = into.2;
            let bindings = into.0.entry(label.to_string()).or_insert(default);

            let handler = move |context: Context<'_>| {
                let state = state_handler(&context);
                action(context, state)
            };
            let handler = Rc::new(RefCell::new(handler));
            for binding in bindings {
                into.1.add(binding, handler.clone());
            }
        }
        // Convenience function for registering key sequences where the default is a single key
        // sequence rather than a list.
        fn register<ModeState>(
            into: &RefCell<(
                &mut BTreeMap<String, Vec<Vec<(Key, ModifiersState)>>>,
                &mut InputTrie,
                &'static impl Fn(&Context) -> ModeState,
            )>,
            label: &str,
            default: Vec<(Key, ModifiersState)>,
            action: impl FnMut(Context, ModeState) + 'static,
        ) {
            register_multiple(into, label, vec![default], action)
        }
        // Convenience function for converting a character to a key.
        fn c(c: char) -> Key {
            Key::Character(winit::keyboard::SmolStr::new(c.encode_utf8(&mut [0; 4])))
        }
        // Convenience function for converting a character to a key.
        fn s(s: &str) -> Vec<(Key, ModifiersState)> {
            s.chars()
                .map(|c_| (c(c_), ModifiersState::empty()))
                .collect()
        }

        register(&normal, "Insert", s("i"), |ctx, _| {
            ctx.keybinds.start_insert(ctx.tas, false, false, false);
        });
        register(&normal, "Append", s("a"), |ctx, _| {
            ctx.keybinds.start_insert(ctx.tas, false, true, false);
        });
        register(
            &normal,
            "Insert (autohold)",
            vec![(c('i'), ModifiersState::SHIFT)],
            |ctx, _| {
                ctx.keybinds.start_insert(ctx.tas, false, false, true);
            },
        );
        register(
            &normal,
            "Append (autohold)",
            vec![(c('a'), ModifiersState::SHIFT)],
            |ctx, _| {
                ctx.keybinds.start_insert(ctx.tas, false, true, true);
            },
        );
        register(
            &normal,
            "Replace",
            vec![(c('r'), ModifiersState::SHIFT)],
            |ctx, _| {
                ctx.keybinds.start_insert(ctx.tas, true, false, false);
            },
        );
        register(
            &global,
            "Normal mode",
            vec![(Key::Named(NamedKey::Escape), ModifiersState::empty())],
            |ctx, _| {
                let old_mode = std::mem::replace(&mut ctx.keybinds.mode, Mode::Normal { count: 0 });
                match old_mode {
                    Mode::Insert {
                        action: Some(action),
                        ..
                    }
                    | Mode::Replace {
                        action: Some(action),
                        ..
                    } => {
                        ctx.tas.push_repeatable(action);
                    }
                    _ => {}
                }
                ctx.tas.set_run_mode(tas::RunMode::Paused);
            },
        );
        register(
            &global,
            "Toggle playback",
            vec![(Key::Named(NamedKey::Space), ModifiersState::empty())],
            |ctx, _| ctx.keybinds.toggle_playback(ctx.tas),
        );

        fn next_frame(ctx: Context<'_>, should_insert: bool) {
            ctx.tas.set_run_mode(tas::RunMode::Paused);
            if should_insert {
                #[expect(clippy::unnecessary_to_owned)] // clippy false-positive
                ctx.tas.insert(
                    ctx.tas.selected_frame() + 1,
                    &ctx.tas.movie().default_frame().to_vec(),
                );
            }
            ctx.tas.select_next(1);
            let (Mode::Insert { pattern, action } | Mode::Replace { pattern, action }) =
                &mut ctx.keybinds.mode
            else {
                unreachable!()
            };
            if should_insert {
                pattern.offset += 1;

                // If we have an apply action in insert mode, push it
                if let Some(Action {
                    kind: tas::ActionKind::Apply { .. },
                    ..
                }) = action
                {
                    ctx.tas.push_repeatable(action.take().unwrap());
                }

                // If we have no action, create an empty insert action
                let action = action.get_or_insert(Action {
                    cursor: ctx.tas.selected_frame(),
                    kind: tas::ActionKind::Insert(Vec::new()),
                });

                let tas::ActionKind::Insert(frames) = &mut action.kind else {
                    unreachable!("unexpected action kind");
                };
                let offset = frames.len();
                frames.extend_from_slice(ctx.tas.movie().default_frame());
                pattern.apply(ctx.tas.movie().input_ports(), &mut frames[offset..]);

                ctx.tas.set_input(pattern);
            } else {
                pattern.offset += 1;

                // If we have an insert action in replace mode, push it
                if let Some(Action {
                    kind: tas::ActionKind::Insert { .. },
                    ..
                }) = action
                {
                    ctx.tas.push_repeatable(action.take().unwrap());
                }

                // If we have no action, create an empty replace action
                let a = action.get_or_insert(Action {
                    cursor: ctx.tas.selected_frame(),
                    kind: tas::ActionKind::Apply {
                        pattern: ctx.tas.movie().default_pattern(),
                        previous: Vec::new(),
                    },
                });

                let tas::ActionKind::Apply {
                    pattern: p,
                    previous,
                } = &mut a.kind
                else {
                    unreachable!("unexpected action kind");
                };
                let old_len = previous.len();
                previous.extend_from_slice(ctx.tas.frame(ctx.tas.selected_frame()));
                p.expand(
                    ctx.tas.movie().input_ports(),
                    (old_len / p.buf.frame_size + 1) as u32,
                );
                let buf = Rc::make_mut(&mut p.buf);
                pattern.apply(ctx.tas.movie().input_ports(), &mut buf.data[old_len..]);

                ctx.tas.set_input(pattern);
            }
        }
        register_multiple(
            &insert,
            "Next frame (replace)",
            vec![vec![(
                Key::Named(NamedKey::ArrowDown),
                ModifiersState::empty(),
            )]],
            |ctx, _| next_frame(ctx, false),
        );
        register_multiple(
            &insert,
            "Next frame (insert)",
            vec![vec![(Key::Named(NamedKey::Enter), ModifiersState::empty())]],
            |ctx, _| next_frame(ctx, true),
        );
        register_multiple(
            &insert,
            "Previous frame",
            vec![vec![(
                Key::Named(NamedKey::ArrowUp),
                ModifiersState::empty(),
            )]],
            |ctx, _| {
                ctx.tas.set_run_mode(tas::RunMode::Paused);
                if let tas::RunMode::Paused = ctx.tas.run_mode() {
                    ctx.tas.select_prev(1);
                    match &mut ctx.keybinds.mode {
                        Mode::Insert { pattern, action: _ }
                        | Mode::Replace { pattern, action: _ } => {
                            if pattern.offset == 0 {
                                pattern.offset = pattern.len() * 2 - 1;
                            } else {
                                pattern.offset -= 1;
                            }
                            ctx.tas.set_input(pattern);
                        }
                        _ => unreachable!(),
                    }
                    match &mut ctx.keybinds.mode {
                        Mode::Insert { action, .. } | Mode::Replace { action, .. } => {
                            if let Some(action) = action.take() {
                                ctx.tas.push_repeatable(action);
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            },
        );
        register(&normal, "Delete", s("x"), |ctx, count| {
            let start = ctx.tas.selected_frame();
            let end = (start + count.max(1)).min(ctx.tas.movie().len());
            ctx.tas.delete(start..end);
        });
        register_multiple(
            &normal,
            "Next frame",
            vec![
                vec![(c('j'), ModifiersState::empty())],
                vec![(Key::Named(NamedKey::ArrowDown), ModifiersState::empty())],
            ],
            |ctx, count| {
                ctx.tas.select_next(count.max(1));
            },
        );
        register_multiple(
            &normal,
            "Previous frame",
            vec![
                vec![(c('k'), ModifiersState::empty())],
                vec![(Key::Named(NamedKey::ArrowUp), ModifiersState::empty())],
            ],
            |ctx, count| {
                ctx.tas.select_prev(count.max(1));
            },
        );
        register_multiple(
            &global,
            "Page down",
            vec![
                vec![(Key::Named(NamedKey::PageDown), ModifiersState::empty())],
                vec![(c('f'), ModifiersState::CONTROL)],
            ],
            |ctx, _| {
                ctx.tas.select_next(ctx.piano_roll.screen_size());
            },
        );
        register_multiple(
            &global,
            "Page up",
            vec![
                vec![(Key::Named(NamedKey::PageUp), ModifiersState::empty())],
                vec![(c('b'), ModifiersState::CONTROL)],
            ],
            |ctx, _| {
                ctx.tas.select_prev(ctx.piano_roll.screen_size());
            },
        );
        register(
            &global,
            "Half-page down",
            vec![(c('d'), ModifiersState::CONTROL)],
            |ctx, _| {
                ctx.tas.select_next(ctx.piano_roll.screen_size() / 2);
            },
        );
        register(
            &global,
            "Half-page up",
            vec![(c('u'), ModifiersState::CONTROL)],
            |ctx, _| {
                ctx.tas.select_prev(ctx.piano_roll.screen_size() / 2);
            },
        );
        register(&normal, "Scroll top", s("zt"), |ctx, _| {
            ctx.piano_roll.set_scroll_lock(piano_roll::ScrollLock::Top)
        });
        register(&normal, "Scroll middle", s("zz"), |ctx, _| {
            ctx.piano_roll
                .set_scroll_lock(piano_roll::ScrollLock::Center)
        });
        register(&normal, "Scroll bottom", s("zb"), |ctx, _| {
            ctx.piano_roll
                .set_scroll_lock(piano_roll::ScrollLock::Bottom)
        });

        register(&normal, "Undo", s("u"), |ctx, _| {
            ctx.tas.set_run_mode(tas::RunMode::Paused);
            ctx.tas.undo_latest();
        });
        register(
            &normal,
            "Redo",
            vec![(c('r'), ModifiersState::CONTROL)],
            |ctx, _| {
                ctx.tas.set_run_mode(tas::RunMode::Paused);
                ctx.tas.redo_latest();
            },
        );
        register(&normal, "Repeat", s("."), |ctx, _| {
            ctx.tas.set_run_mode(tas::RunMode::Paused);
            ctx.tas.repeat();
        });

        // Initialize controller bindings.
        if config.controller_bindings.is_empty() {
            const DEFAULT_CONTROLLER_LAYOUT: &str = "aqwpkjhldfer";
            config.controller_bindings = vec![[(
                tas::input::InputPort::Joypad(tas::input::Joypad::Snes),
                DEFAULT_CONTROLLER_LAYOUT
                    .chars()
                    .map(|c| {
                        Some(Key::Character(winit::keyboard::SmolStr::new(
                            c.encode_utf8(&mut [0; 4]),
                        )))
                    })
                    .collect(),
            )]
            .into_iter()
            .collect()];
        }

        self.controller_bindings
            .resize(config.controller_bindings.len(), HashMap::new());
        for (port_index, port_config) in config.controller_bindings.iter_mut().enumerate() {
            for device in tas::input::InputPort::all() {
                // TODO: non-joypad/multitap support
                let tas::input::InputPort::Joypad(joypad) = device;
                let bindings = port_config
                    .entry(device)
                    .or_insert_with(|| std::iter::repeat_n(None, joypad.buttons().len()).collect());
                assert_eq!(bindings.len(), joypad.buttons().len());

                self.controller_bindings[port_index].insert(
                    device,
                    bindings
                        .iter()
                        .enumerate()
                        .flat_map(|(i, key)| key.clone().map(|key| (key, i as u32)))
                        .collect(),
                );
            }
        }
    }
}
