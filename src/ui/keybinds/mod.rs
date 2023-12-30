use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use smallvec::SmallVec;
use winit::keyboard::{Key, ModifiersState, NamedKey};

use crate::tas::{self, movie::Pattern, Tas};

use super::piano_roll::PianoRoll;

use serde::{Deserialize, Serialize};

mod bindings;
mod editor;

pub use editor::KeybindEditor;

#[derive(Debug)]
pub struct Keybinds {
    mode: Mode,

    controller_bindings: Vec<HashMap<tas::input::InputPort, HashMap<Key, u32>>>,
    global_bindings: InputTrie,
    normal_bindings: InputTrie,
    insert_bindings: InputTrie,
}

#[derive(Debug)]
pub enum Mode {
    Normal { count: u32 },
    Insert(Rc<Pattern>),
    Replace(Rc<Pattern>),
}

#[derive(Debug, Default, Serialize, Deserialize)]
struct KeybindConfiguration {
    #[serde(with = "serialize_controller_bindings")]
    controller_bindings: Vec<BTreeMap<tas::input::InputPort, Vec<Option<Key>>>>,
    global_bindings: BTreeMap<String, Vec<Vec<(Key, ModifiersState)>>>,
    normal_bindings: BTreeMap<String, Vec<Vec<(Key, ModifiersState)>>>,
    insert_bindings: BTreeMap<String, Vec<Vec<(Key, ModifiersState)>>>,
}

struct Context<'a> {
    keybinds: &'a mut Keybinds,
    tas: &'a mut Tas,
    piano_roll: &'a mut PianoRoll,
}

#[derive(Debug)]
struct InputTrie {
    root: Rc<RefCell<InputNode>>,
    current: Option<Rc<RefCell<InputNode>>>,
}

enum InputNode {
    Action(Rc<RefCell<dyn FnMut(Context)>>),
    Branch(HashMap<Key, SmallVec<[(ModifiersState, Rc<RefCell<InputNode>>); 3]>>),
}

static CONFIG_PATH: std::sync::OnceLock<std::path::PathBuf> = std::sync::OnceLock::new();
impl Keybinds {
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

    fn config_path() -> &'static std::path::Path {
        CONFIG_PATH.get_or_init(|| {
            let mut path = dirs::config_dir().unwrap();
            path.push("mvi");
            path.push("keybinds.json");
            path
        })
    }

    fn load_config(&mut self) -> KeybindConfiguration {
        let path = Self::config_path();
        let mut config = KeybindConfiguration::default();
        if let Ok(file) = std::fs::File::open(path) {
            if let Ok(c) = serde_json::from_reader(file) {
                config = c;
            }
        }

        self.register_keybinds(&mut config);
        config
    }

    fn save_config(config: &KeybindConfiguration) -> anyhow::Result<()> {
        let path = Self::config_path();
        std::fs::create_dir_all(path.parent().unwrap())?;
        let file = std::fs::File::create(path)?;
        serde_json::to_writer(file, config)?;

        Ok(())
    }

    pub fn mode(&self) -> &Mode {
        &self.mode
    }

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
                    .and_then(|bindings| bindings.get(&key))
                    // TODO: handle index
                    .map(|id| (port as u32, 0, *id))
            })
    }

    pub fn key_down(
        &mut self,
        key: Key,
        modifiers: ModifiersState,
        tas: &mut Tas,
        piano_roll: &mut PianoRoll,
    ) {
        let key = Self::normalize(key);
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

        let global = self.global_bindings.process(&key, modifiers);
        let modal = match self.mode {
            Mode::Normal { .. } => self.normal_bindings.process(&key, modifiers),
            Mode::Insert { .. } | Mode::Replace { .. } => {
                self.insert_bindings.process(&key, modifiers)
            }
        };

        match (&mut self.mode, input, modal, global) {
            // In insert mode, controller bindings get priority over all else
            (Mode::Insert(pattern) | Mode::Replace(pattern), Some((port, index, id)), _, _) => {
                if pattern.read(tas.movie().input_ports(), port, 0, id) != 1 {
                    Rc::make_mut(pattern).write(&tas.movie().input_ports(), port, index, id, 1);
                    tas.set_input(pattern);
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
            Mode::Insert(..) | Mode::Replace(..) => &mut self.insert_bindings,
        };
        if self.global_bindings.current.is_none() && modal_bindings.current.is_none() {
            // If nothing matched, and a numeric key was pressed in normal mode,
            // reset bindings and set the count.
            if let (Mode::Normal { .. }, Some(digit)) = (&self.mode, digit) {
                self.reset_bindings();
                self.mode = Mode::Normal { count: digit };
            } else {
                self.reset_bindings();
            }
            return;
        }
    }

    pub fn key_up(
        &mut self,
        key: Key,
        _modifiers: ModifiersState,
        tas: &mut Tas,
        _piano_roll: &mut PianoRoll,
    ) {
        let key = Self::normalize(key);
        if let Some((port, index, id)) = self.get_input_binding(tas.movie().input_ports(), &key) {
            if let Mode::Insert(pattern) | Mode::Replace(pattern) = &mut self.mode {
                if pattern.read(tas.movie().input_ports(), port, index, id) != 0 {
                    Rc::make_mut(pattern).write(&tas.movie().input_ports(), port, index, id, 0);
                    tas.set_input(pattern);
                }
            }
        }
    }

    fn normalize(key: Key) -> Key {
        match key {
            Key::Character(c) => Key::Character(c.to_lowercase().into()),
            k => k,
        }
    }

    fn reset_bindings(&mut self) {
        self.global_bindings.reset();
        self.insert_bindings.reset();
        self.normal_bindings.reset();
        if let Mode::Normal { count } = &mut self.mode {
            *count = 0;
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
                    Mode::Normal { .. } => tas::RecordMode::ReadOnly,
                    Mode::Insert(pattern) => tas::RecordMode::Insert(pattern.clone()),
                    Mode::Replace(pattern) => tas::RecordMode::Overwrite(pattern.clone()),
                },
            },
        };
        tas.set_run_mode(mode);
    }

    fn start_insert(&mut self, tas: &mut Tas, is_replace: bool, is_append: bool) {
        let pattern = Rc::new(Pattern {
            data: tas.movie().default_frame().to_vec(),
        });
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
                    tas.select_next(1);
                } else {
                    tas.insert(tas.selected_frame(), &pattern.data);
                }
            }
            self.mode = Mode::Insert(pattern);
        }
    }
}

impl InputTrie {
    fn new() -> InputTrie {
        let root = Rc::new(RefCell::new(InputNode::Branch(HashMap::new())));
        InputTrie {
            current: Some(root.clone()),
            root,
        }
    }
    fn add(
        &mut self,
        binding: &[(Key, ModifiersState)],
        action: Rc<RefCell<dyn FnMut(Context)>>,
    ) -> bool {
        self.root.borrow_mut().add(binding, action)
    }

    fn reset(&mut self) {
        self.current = Some(self.root.clone())
    }

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
    fn add(
        &mut self,
        binding: &[(Key, ModifiersState)],
        action: Rc<RefCell<dyn FnMut(Context)>>,
    ) -> bool {
        match self {
            Self::Action(_) => false,
            Self::Branch(mappings) => {
                let Some((key, mods)) = binding.first() else {
                    return false;
                };
                let remainder = &binding[1..];

                let mappings = mappings.entry(key.clone()).or_default();
                if let Some(conflict) = mappings.iter_mut().find(|(m, _)| m == mods) {
                    if remainder.is_empty() {
                        false
                    } else {
                        conflict.1.borrow_mut().add(remainder, action)
                    }
                } else {
                    if remainder.is_empty() {
                        mappings.push((*mods, Rc::new(RefCell::new(InputNode::Action(action)))));
                        true
                    } else {
                        let mut node = InputNode::Branch(HashMap::new());
                        assert!(node.add(remainder, action));
                        mappings.push((*mods, Rc::new(RefCell::new(node))));
                        true
                    }
                }
            }
        }
    }

    fn process(&self, key: &Key, modifiers: ModifiersState) -> Option<Rc<RefCell<InputNode>>> {
        match self {
            Self::Action(_) => None,
            Self::Branch(mappings) => {
                if let Some(mappings) = mappings.get(&key) {
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

    impl<'a> Serialize for BindingSerializer<'a> {
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
