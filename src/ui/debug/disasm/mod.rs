use std::{collections::HashMap, sync::LazyLock};

use crate::core::trace;

mod snes;

pub trait Disassembler {
    fn disassemble(&mut self, entry: trace::Entry<'_>) -> String;
}

#[allow(clippy::type_complexity)]
pub static DISASSEMBLERS: LazyLock<HashMap<&'static str, fn() -> Box<dyn Disassembler + 'static>>> =
    LazyLock::new(|| {
        [disassembler("super_nintendo", || {
            Box::new(snes::SnesDisassembler::new())
        })]
        .into_iter()
        .collect()
    });

pub fn get_disassembler(system_id: &str) -> Option<Box<dyn Disassembler>> {
    DISASSEMBLERS.get(system_id).map(|f| f())
}

// this function is just to simplify type inference
fn disassembler(
    name: &'static str,
    f: fn() -> Box<dyn Disassembler>,
) -> (&'static str, fn() -> Box<dyn Disassembler>) {
    (name, f)
}
