use crate::core::Core;

/// Extension trait for [`rhai::Engine`].
pub trait EngineExt {
    /// Register the `mem` rhai module.
    ///
    /// This module contains functions to interact with the guest's memory.
    fn register_mem_module(&mut self);
}

impl EngineExt for rhai::Engine {
    fn register_mem_module(&mut self) {
        self.register_static_module("mem", mem().into());
    }
}

type RhaiResult<T> = Result<T, Box<rhai::EvalAltResult>>;

fn mem() -> rhai::Module {
    let mut module = rhai::Module::new();
    rhai::FuncRegistration::new("u8")
        .with_volatility(true)
        .set_into_module(
            &mut module,
            |ctx: rhai::NativeCallContext, address: i64| -> RhaiResult<u8> {
                let c: &Core = ctx.tag().unwrap().clone().cast();
                usize::try_from(address)
                    .ok()
                    .and_then(|address| c.read_memory_byte(address))
                    .ok_or_else(|| format!("out of bounds read at {address}").into())
            },
        );
    rhai::FuncRegistration::new("u16le")
        .with_volatility(true)
        .set_into_module(
            &mut module,
            |ctx: rhai::NativeCallContext, address: i64| -> RhaiResult<u16> {
                let c: &Core = ctx.tag().unwrap().clone().cast();
                let res = usize::try_from(address)
                    .ok()
                    .and_then(|address| c.read_memory_le(address, 2))
                    .ok_or_else(|| format!("out of bounds read of 2 bytes at {address}"))?;
                Ok(res as u16)
            },
        );
    rhai::FuncRegistration::new("u32le")
        .with_volatility(true)
        .set_into_module(
            &mut module,
            |ctx: rhai::NativeCallContext, address: i64| -> RhaiResult<u32> {
                let c: &Core = ctx.tag().unwrap().clone().cast();
                let res = usize::try_from(address)
                    .ok()
                    .and_then(|address| c.read_memory_le(address, 4))
                    .ok_or_else(|| format!("out of bounds read of 4 bytes at {address}"))?;
                Ok(res as u32)
            },
        );
    rhai::FuncRegistration::new("u64le")
        .with_volatility(true)
        .set_into_module(
            &mut module,
            |ctx: rhai::NativeCallContext, address: i64| -> RhaiResult<u64> {
                let c: &Core = ctx.tag().unwrap().clone().cast();
                usize::try_from(address)
                    .ok()
                    .and_then(|address| c.read_memory_le(address, 8))
                    .ok_or_else(|| format!("out of bounds read of 8 bytes at {address}").into())
            },
        );

    module
}
