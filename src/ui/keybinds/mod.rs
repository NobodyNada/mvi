#![expect(clippy::type_complexity)]

use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use smallvec::SmallVec;
use winit::keyboard::{Key, ModifiersState, NamedKey};

use crate::tas::{self, Tas, edit::ActionHandle, edit::Pattern};

use super::piano_roll::PianoRoll;

use serde::{Deserialize, Serialize};

mod bindings;
mod editor;

pub use editor::KeybindEditor;

/// The current keybind-processing state.
#[derive(Debug)]
pub struct Keybinds {
    /// The current mode (normal, insert, etc.)
    mode: Mode,

    /// All bindings mapping a key to a controller input.
    ///     controller_bindings[port_index][port_type][key] = button_index
    controller_bindings: Vec<HashMap<tas::input::InputPort, HashMap<Key, u32>>>,

    /// Keybindings that apply in all modes.
    global_bindings: InputTrie,

    /// Keybindings that apply only in normal mode.
    normal_bindings: InputTrie,

    /// Keybindings that apply only in insert or replace mode.
    insert_bindings: InputTrie,
}

/// The current editor mode.
#[derive(Debug)]
pub enum Mode {
    Normal {
        count: u32,
    },
    Insert {
        pattern: Pattern,
        action: ActionHandle,
    },
    Replace {
        pattern: Pattern,
        action: ActionHandle,
    },
}

/// The keybind configuration in a form convenient for editing and serializing.
///
/// For processing keybinds, it's convenient to represent them as mappings from key to action.
/// However, for editing, saving, and loading keybinds, we represent them as mappings from action
/// to key.
#[derive(Debug, Default, Serialize, Deserialize)]
struct KeybindConfiguration {
    /// The controller bindings.
    ///     controller_bindings[port_index][port_type][button_index] = key
    #[serde(with = "serialize_controller_bindings")]
    controller_bindings: Vec<BTreeMap<tas::input::InputPort, Vec<Option<Key>>>>,

    /// The bindings that apply in all modes.
    ///     global_bindings[action] = [binding1, binding2, binding3, ...]
    /// where each binding is [(key1, mods1), (key2, mods2), ...]
    global_bindings: BTreeMap<String, Vec<Vec<(Key, ModifiersState)>>>,
    normal_bindings: BTreeMap<String, Vec<Vec<(Key, ModifiersState)>>>,
    insert_bindings: BTreeMap<String, Vec<Vec<(Key, ModifiersState)>>>,
}

/// Context needed by keybind callbacks.
struct Context<'a> {
    keybinds: &'a mut Keybinds,
    tas: &'a mut Tas,
    piano_roll: &'a mut PianoRoll,
}

/// Keybinds are represented using a trie: https://en.wikipedia.org/wiki/Trie
/// Each node of the trie is either a branch node with edges corresponding to keypresses that lead
/// to child nodes, or a leaf node corresponding to an action. A sequence of keypresses is
/// interpereted by walking the trie by following edges corresponding to pressed keys until
/// reaching an action.
#[derive(Debug)]
struct InputTrie {
    /// The root node of the trie.
    root: Rc<RefCell<InputNode>>,

    /// The position of the currently buffered sequence of keys, or None if the pressed keys do not
    /// lead to any existing actions.
    current: Option<Rc<RefCell<InputNode>>>,
}

enum InputNode {
    /// This is a terminal node corresponding to an action.
    Action(Rc<RefCell<dyn FnMut(Context)>>),
    /// This is a branch node, with children for each possible keypress.
    Branch(HashMap<Key, SmallVec<[(ModifiersState, Rc<RefCell<InputNode>>); 3]>>),
}

static CONFIG_PATH: std::sync::OnceLock<std::path::PathBuf> = std::sync::OnceLock::new();
impl Keybinds {
    /// Creates the keybinds, loading the user's configuration from disk.
    pub fn new() -> Keybinds {
        let mut kb = Self {
            mode: Mode::Normal { count: 0 },
            controller_bindings: Vec::new(),
            global_bindings: InputTrie::new(),
            normal_bindings: InputTrie::new(),
            insert_bindings: InputTrie::new(),
        };
        kb.load_config();
        kb
    }

    /// The path to the keybind configuration file.
    fn config_path() -> &'static std::path::Path {
        CONFIG_PATH.get_or_init(|| {
            let mut path = dirs::config_dir().unwrap();
            path.push("mvi");
            path.push("keybinds.json");
            path
        })
    }

    /// Loads the keybind configuration from disk. The configuration is applied, missing entries
    /// are populated with their default values, and the resulting configuration is returned.
    fn load_config(&mut self) -> KeybindConfiguration {
        let path = Self::config_path();
        let mut config = KeybindConfiguration::default();
        if let Ok(file) = std::fs::File::open(path)
            && let Ok(c) = serde_json::from_reader(file)
        {
            config = c;
        }

        self.register_keybinds(&mut config);
        config
    }

    /// Saves the given keybind configuration to disk.
    fn save_config(config: &KeybindConfiguration) -> anyhow::Result<()> {
        let path = Self::config_path();
        std::fs::create_dir_all(path.parent().unwrap())?;
        let file = std::fs::File::create(path)?;
        serde_json::to_writer(file, config)?;

        Ok(())
    }

    /// Returns the current editor mode.
    pub fn mode(&self) -> &Mode {
        &self.mode
    }

    /// Returns the controller (port, index, button) corresponding to the given keypress.
    fn get_input_binding(
        &self,
        input_ports: &[tas::input::InputPort],
        key: &Key,
    ) -> Option<(u32, u32, u32)> {
        self.controller_bindings
            .iter()
            .enumerate()
            .find_map(|(port, ports)| {
                input_ports
                    .get(port)
                    .and_then(|port| ports.get(port))
                    .and_then(|bindings| bindings.get(key))
                    // TODO: handle index
                    .map(|id| (port as u32, 0, *id))
            })
    }

    /// Handles a keyboard input.
    pub fn key_down(
        &mut self,
        key: Key,
        modifiers: ModifiersState,
        tas: &mut Tas,
        piano_roll: &mut PianoRoll,
    ) {
        let input = self.get_input_binding(tas.movie().input_ports(), &key);

        // The numeric value of this key, if it's a digit.
        let digit = if let Key::Character(c) = &key {
            if c.len() == 1 {
                c.chars().next().unwrap().to_digit(10)
            } else {
                None
            }
        } else {
            None
        };

        if let (Mode::Normal { count }, Some(digit)) = (&mut self.mode, digit) {
            // If we already have a count, numeric keys update the count instead of performing
            // actions.
            if *count != 0 {
                *count = (*count * 10 + digit).min(1_000_000);
                return;
            }
        }

        // Walk a step in the active tries according to the pressed key.
        let global = self.global_bindings.process(&key, modifiers);
        let modal = match self.mode {
            Mode::Normal { .. } => self.normal_bindings.process(&key, modifiers),
            Mode::Insert { .. } | Mode::Replace { .. } => {
                self.insert_bindings.process(&key, modifiers)
            }
        };

        // Did any actions match?
        match (&mut self.mode, input, modal, global) {
            // In insert mode, controller bindings get priority over all else
            (
                Mode::Insert { pattern, action } | Mode::Replace { pattern, action },
                Some((port, index, id)),
                _,
                _,
            ) => {
                let mut input_changed = false;
                if modifiers.alt_key() {
                    // Toggle autofire
                    *pattern.autofire_mut(tas.movie().input_ports(), port, index, id) ^= true;
                    input_changed = true;
                } else {
                    if pattern.autofire(tas.movie().input_ports(), port, index, id) {
                        // Disable autofire
                        *pattern.autofire_mut(tas.movie().input_ports(), port, index, id) = false;
                    }
                    if modifiers.shift_key() {
                        // Toggle autohold
                        *pattern.autohold_mut(tas.movie().input_ports(), port, index, id) ^= true;
                    } else if pattern.autohold(tas.movie().input_ports(), port, index, id) {
                        // Disable autohold
                        *pattern.autohold_mut(tas.movie().input_ports(), port, index, id) = false;
                    }
                    if pattern.read(tas.movie().input_ports(), port, 0, id) != 1 {
                        pattern.write(tas.movie().input_ports(), port, index, id, 1);
                        input_changed = true;
                    }
                }
                if input_changed {
                    if let tas::RunMode::Paused = tas.run_mode() {
                        let mut frame = tas.frame(tas.selected_frame()).to_owned();
                        pattern.apply(tas.movie().input_ports(), &mut frame);
                        action.replace_frame(tas, tas.selected_frame(), &frame);
                    } else {
                        tas.set_input(pattern);
                    }
                }
                self.reset_bindings();
                return;
            }
            // Modal actions get priority over global actions
            (_, _, Some(action), _) | (_, _, _, Some(action)) => {
                action.borrow_mut()(Context {
                    keybinds: self,
                    tas,
                    piano_roll,
                });
                self.reset_bindings();
                return;
            }
            _ => (),
        };

        let modal_bindings = match self.mode {
            Mode::Normal { .. } => &mut self.normal_bindings,
            Mode::Insert { .. } | Mode::Replace { .. } => &mut self.insert_bindings,
        };
        // If nothing matches at all, reset all the tries.
        if self.global_bindings.current.is_none() && modal_bindings.current.is_none() {
            // If a numeric key was pressed in normal mode, and it was not mapped to anything else,
            // set the count.
            if let (Mode::Normal { .. }, Some(digit)) = (&self.mode, digit) {
                self.reset_bindings();
                self.mode = Mode::Normal { count: digit };
            } else {
                self.reset_bindings();
            }
        }
    }

    /// Handles a key release input.
    pub fn key_up(
        &mut self,
        key: Key,
        _modifiers: ModifiersState,
        tas: &mut Tas,
        _piano_roll: &mut PianoRoll,
    ) {
        if let Some((port, index, id)) = self.get_input_binding(tas.movie().input_ports(), &key)
            && let Mode::Insert { pattern, action } | Mode::Replace { pattern, action } =
                &mut self.mode
            && !pattern.autofire(tas.movie().input_ports(), port, index, id)
            && !pattern.autohold(tas.movie().input_ports(), port, index, id)
            && pattern.read(tas.movie().input_ports(), port, index, id) != 0
        {
            pattern.write(tas.movie().input_ports(), port, index, id, 0);
            if let tas::RunMode::Paused = tas.run_mode() {
                let mut frame = tas.frame(tas.selected_frame()).to_owned();
                pattern.apply(tas.movie().input_ports(), &mut frame);
                action.replace_frame(tas, tas.selected_frame(), &frame);
            } else {
                tas.set_input(pattern);
            }
        }
    }

    /// Resets the state, clearing any pending keys and switching to normal mode.
    pub fn reset(&mut self) {
        self.mode = Mode::Normal { count: 0 };
        self.reset_bindings();
    }

    fn reset_bindings(&mut self) {
        self.global_bindings.reset();
        self.insert_bindings.reset();
        self.normal_bindings.reset();
        if let Mode::Normal { count } = &mut self.mode {
            *count = 0;
        }
    }

    fn toggle_playback(&mut self, tas: &mut Tas) {
        let mode = match tas.run_mode() {
            tas::RunMode::Running { .. } => tas::RunMode::Paused,
            tas::RunMode::Paused => tas::RunMode::Running {
                stop_at: None,
                record_mode: match &mut self.mode {
                    // If paused, start the emulator in the appropriate recording mode.
                    Mode::Normal { .. } => tas::RecordMode::ReadOnly,
                    Mode::Insert { pattern, .. } => {
                        tas.seek_to_selected();
                        tas::RecordMode::Insert {
                            pattern: pattern.clone(),
                            action: tas.start_repeatable(),
                        }
                    }
                    Mode::Replace { pattern, .. } => {
                        tas.seek_to_selected();
                        tas::RecordMode::Overwrite {
                            pattern: pattern.clone(),
                            action: tas.start_repeatable(),
                        }
                    }
                },
            },
        };
        tas.set_run_mode(mode);
    }

    fn start_insert(&mut self, tas: &mut Tas, is_replace: bool, is_append: bool, autohold: bool) {
        let mut pattern = tas.movie().default_pattern();
        if autohold {
            Rc::make_mut(&mut pattern.buf)
                .data
                .copy_from_slice(tas.frame(tas.selected_frame()));
            let input_ports = tas.movie().input_ports();
            for (port, device) in input_ports.iter().enumerate() {
                for index in 0..device.num_controllers() {
                    for id in 0..device.num_inputs(index) {
                        if device.read(tas.movie().frame(tas.selected_frame()), index, id) != 0 {
                            *pattern.autohold_mut(input_ports, port as u32, index, id) = true;
                        }
                    }
                }
            }
        }
        let mode = match tas.run_mode() {
            tas::RunMode::Paused => tas::RunMode::Paused,
            tas::RunMode::Running {
                stop_at,
                record_mode: _,
            } => tas::RunMode::Running {
                stop_at: *stop_at,
                record_mode: if is_replace {
                    tas::RecordMode::Overwrite {
                        pattern: pattern.clone(),
                        action: tas.start_repeatable(),
                    }
                } else {
                    tas::RecordMode::Insert {
                        pattern: pattern.clone(),
                        action: tas.start_repeatable(),
                    }
                },
            },
        };
        tas.set_run_mode(mode);
        tas.ensure_length(tas.selected_frame() + 1);
        if is_replace {
            self.mode = Mode::Replace {
                pattern: pattern.clone(),
                action: tas.start_repeatable(),
            };
        } else {
            // If the movie only has one blank frame, overwrite it insead of inserting
            let mut action = tas.start_repeatable();
            if tas.movie().len() > 1 || tas.frame(0) != tas.movie().default_frame() {
                let mut frame = tas.movie().default_frame().to_owned();
                pattern.apply(tas.movie().input_ports(), &mut frame);
                if is_append {
                    tas.select_next(1);
                }
                action.insert(tas, tas.selected_frame(), &frame);
            }
            self.mode = Mode::Insert { pattern, action };
        }
    }
}

impl InputTrie {
    /// Creates an empty InputTrie.
    fn new() -> InputTrie {
        let root = Rc::new(RefCell::new(InputNode::Branch(HashMap::new())));
        InputTrie {
            current: Some(root.clone()),
            root,
        }
    }

    /// Adds a binding to this trie.
    fn add(
        &mut self,
        binding: &[(Key, ModifiersState)],
        action: Rc<RefCell<dyn FnMut(Context)>>,
    ) -> bool {
        self.root.borrow_mut().add(binding, action)
    }

    /// Resets the current state of the trie (clearing any pending keys).
    fn reset(&mut self) {
        self.current = Some(self.root.clone())
    }

    /// Advances the trie's current state according to the given keypress.
    fn process(
        &mut self,
        key: &Key,
        modifiers: ModifiersState,
    ) -> Option<Rc<RefCell<dyn FnMut(Context)>>> {
        let (result, next) = match &self.current {
            None => (None, None),
            Some(state) => match state.borrow().process(key, modifiers) {
                None => (None, None),
                Some(state) => match &*state.borrow() {
                    InputNode::Branch(_) => (None, Some(state.clone())),
                    InputNode::Action(a) => (Some(a.clone()), None),
                },
            },
        };
        self.current = next;
        result
    }
}

impl std::fmt::Debug for InputNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Action(_) => write!(f, "<action>"),
            Self::Branch(b) => b.fmt(f),
        }
    }
}

impl InputNode {
    /// Adds a binding to the trie. Returns true if successful,
    /// or false in case of conflict.
    fn add(
        &mut self,
        binding: &[(Key, ModifiersState)],
        action: Rc<RefCell<dyn FnMut(Context)>>,
    ) -> bool {
        match self {
            // If this node is an action, we have a conflict.
            Self::Action(_) => false,
            Self::Branch(mappings) => {
                // This node is a branch. Try to add the binding to this branch.
                let Some((key, mods)) = binding.first() else {
                    // If we're trying to add an action here, and this is a branch,
                    // we have a conflict.
                    return false;
                };
                let remainder = &binding[1..];

                let mappings = mappings.entry(key.clone()).or_default();
                #[expect(clippy::collapsible_else_if)]
                if let Some(conflict) = mappings.iter_mut().find(|(m, _)| m == mods) {
                    if remainder.is_empty() {
                        // We already have something mapped to this key, and we're trying to put an
                        // action here -- we have a conflict.
                        false
                    } else {
                        // We have something mapped to this key, but it's a branch and this key is
                        // not the end of the sequence. Recursively add the remainder of the
                        // sequence to the new branch.
                        conflict.1.borrow_mut().add(remainder, action)
                    }
                } else {
                    if remainder.is_empty() {
                        // We have an action, and there is no conflict.
                        mappings.push((*mods, Rc::new(RefCell::new(InputNode::Action(action)))));
                        true
                    } else {
                        // There is nothing here in the tree already, but we have more keys to
                        // process. Recursively create a series of branches leading to the action,
                        // and put it in the tree here.
                        let mut node = InputNode::Branch(HashMap::new());
                        assert!(node.add(remainder, action));
                        mappings.push((*mods, Rc::new(RefCell::new(node))));
                        true
                    }
                }
            }
        }
    }

    /// If this node is a branch, returns the child node corresponding to the given keypress.
    fn process(&self, key: &Key, modifiers: ModifiersState) -> Option<Rc<RefCell<InputNode>>> {
        match self {
            Self::Action(_) => None,
            Self::Branch(mappings) => {
                if let Some(mappings) = mappings.get(key) {
                    mappings
                        .iter()
                        .filter(|(mods, _)| *mods == modifiers)
                        .map(|(_, node)| node.clone())
                        .next()
                } else {
                    None
                }
            }
        }
    }
}

/// A custom serde serializer/deserializer to serialize (device, binding) key-value pairs as an
/// array of tuples instead of a JSON dictionary.
mod serialize_controller_bindings {
    use serde::{
        de::{Deserializer, SeqAccess, Visitor},
        ser::{Serialize, SerializeSeq, Serializer},
    };

    use super::*;

    pub(super) fn serialize<S: Serializer>(
        bindings: &Vec<BTreeMap<tas::input::InputPort, Vec<Option<Key>>>>,
        s: S,
    ) -> Result<S::Ok, S::Error> {
        let mut s = s.serialize_seq(Some(bindings.len()))?;
        for port in bindings {
            s.serialize_element(&BindingSerializer(port))?;
        }
        s.end()
    }

    struct BindingSerializer<'a>(&'a BTreeMap<tas::input::InputPort, Vec<Option<Key>>>);

    impl Serialize for BindingSerializer<'_> {
        fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
            let mut s = s.serialize_seq(Some(self.0.len()))?;
            for (device, bindings) in self.0 {
                s.serialize_element(&(device, bindings))?;
            }
            s.end()
        }
    }

    pub(super) fn deserialize<'de, D: Deserializer<'de>>(
        d: D,
    ) -> Result<Vec<BTreeMap<tas::input::InputPort, Vec<Option<Key>>>>, D::Error> {
        struct SeqVisitor;
        impl<'de> Visitor<'de> for SeqVisitor {
            type Value = Vec<BTreeMap<tas::input::InputPort, Vec<Option<Key>>>>;
            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "an array of arrays of (device, bindings) tuples")
            }

            fn visit_seq<D>(self, mut d: D) -> Result<Self::Value, D::Error>
            where
                D: SeqAccess<'de>,
            {
                let mut result = Vec::new();
                while let Some(bindings) = d.next_element::<BindingDeserializer>()? {
                    result.push(bindings.0);
                }
                Ok(result)
            }
        }

        d.deserialize_seq(SeqVisitor)
    }
    struct BindingDeserializer(BTreeMap<tas::input::InputPort, Vec<Option<Key>>>);

    impl<'de> Deserialize<'de> for BindingDeserializer {
        fn deserialize<D: Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
            Ok(BindingDeserializer(d.deserialize_seq(SeqVisitor)?))
        }
    }
    struct SeqVisitor;
    impl<'de> Visitor<'de> for SeqVisitor {
        type Value = BTreeMap<tas::input::InputPort, Vec<Option<Key>>>;
        fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "an array of (device, bindings) tuples")
        }

        fn visit_seq<D>(self, mut d: D) -> Result<Self::Value, D::Error>
        where
            D: SeqAccess<'de>,
        {
            let mut result = BTreeMap::new();
            while let Some((device, bindings)) = d.next_element()? {
                result.insert(device, bindings);
            }
            Ok(result)
        }
    }
}
