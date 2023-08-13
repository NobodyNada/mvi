#![allow(dead_code)]

use std::{ffi::c_void, sync::Mutex};

use anyhow::{anyhow, bail, ensure, Result};
use libloading::Library;
use libretro_ffi::*;

#[cfg(unix)]
use libloading::os::unix::Symbol;

#[cfg(windows)]
use libloading::os::windows::Symbol;

pub struct Core {
    library: Library,
    symbols: CoreSymbols,
}

static CORE_LOADED: Mutex<bool> = Mutex::new(false);
impl Core {
    pub unsafe fn load(path: &str) -> Result<Core> {
        unsafe {
            let mut lock = CORE_LOADED.lock().unwrap();
            assert!(!*lock, "Only one core can be loaded at once");

            let library = libloading::Library::new(path)?;
            let symbols = CoreSymbols::new(&library)?;
            let result = Self { library, symbols };
            ensure!(
                result.retro_api_version() == RETRO_API_VERSION,
                "core declares incompatible libretro version"
            );

            *lock = true;
            Ok(result)
        }
    }
}

impl Core {
    pub fn retro_api_version(&self) -> u32 {
        unsafe { (self.symbols.retro_api_version.unwrap())() }
    }
}

impl Drop for Core {
    fn drop(&mut self) {
        let mut lock = CORE_LOADED.lock().unwrap();
        *lock = false;
    }
}

struct CoreSymbols {
    retro_api_version: Symbol<retro_api_version_t>,

    retro_set_environment: Symbol<retro_set_environment_t>,
    retro_set_video_refresh: Symbol<retro_set_video_refresh_t>,
    retro_set_audio_sample: Symbol<retro_set_audio_sample_t>,
    retro_set_audio_sample_batch: Symbol<retro_set_audio_sample_batch_t>,
    retro_set_input_poll: Symbol<retro_set_input_poll_t>,
    retro_set_input_state: Symbol<retro_set_input_state_t>,

    retro_init: Symbol<retro_init_t>,
    retro_deinit: Symbol<retro_deinit_t>,

    retro_get_system_info: Symbol<retro_get_system_info_t>,
    retro_get_system_av_info: Symbol<retro_get_system_av_info_t>,

    retro_set_controller_port_device: Symbol<retro_set_controller_port_device_t>,

    retro_reset: Symbol<retro_reset_t>,
    retro_run: Symbol<retro_run_t>,

    retro_serialize_size: Symbol<retro_serialize_size_t>,
    retro_serialize: Symbol<retro_serialize_t>,
    retro_unserialize: Symbol<retro_unserialize_t>,

    retro_load_game: Symbol<retro_load_game_t>,
    retro_unload_game: Symbol<retro_unload_game_t>,

    retro_get_region: Symbol<retro_get_region_t>,

    retro_get_memory_data: Symbol<retro_get_memory_data_t>,
    retro_get_memory_size: Symbol<retro_get_memory_size_t>,
}

impl CoreSymbols {
    fn new(library: &Library) -> Result<Self> {
        unsafe {
            Ok(CoreSymbols {
                retro_api_version: Self::load_symbol(library, "retro_api_version")?,
                retro_set_environment: Self::load_symbol(library, "retro_set_environment")?,
                retro_set_video_refresh: Self::load_symbol(library, "retro_set_video_refresh")?,
                retro_set_audio_sample: Self::load_symbol(library, "retro_set_audio_sample")?,
                retro_set_audio_sample_batch: Self::load_symbol(
                    library,
                    "retro_set_audio_sample_batch",
                )?,
                retro_set_input_poll: Self::load_symbol(library, "retro_set_input_poll")?,
                retro_set_input_state: Self::load_symbol(library, "retro_set_input_state")?,
                retro_init: Self::load_symbol(library, "retro_init")?,
                retro_deinit: Self::load_symbol(library, "retro_deinit")?,
                retro_get_system_info: Self::load_symbol(library, "retro_get_system_info")?,
                retro_get_system_av_info: Self::load_symbol(library, "retro_get_system_av_info")?,
                retro_set_controller_port_device: Self::load_symbol(
                    library,
                    "retro_set_controller_port_device",
                )?,
                retro_reset: Self::load_symbol(library, "retro_reset")?,
                retro_run: Self::load_symbol(library, "retro_run")?,
                retro_serialize_size: Self::load_symbol(library, "retro_serialize_size")?,
                retro_serialize: Self::load_symbol(library, "retro_serialize")?,
                retro_unserialize: Self::load_symbol(library, "retro_unserialize")?,
                retro_load_game: Self::load_symbol(library, "retro_load_game")?,
                retro_unload_game: Self::load_symbol(library, "retro_unload_game")?,
                retro_get_region: Self::load_symbol(library, "retro_get_region")?,
                retro_get_memory_data: Self::load_symbol(library, "retro_get_memory_data")?,
                retro_get_memory_size: Self::load_symbol(library, "retro_get_memory_size")?,
            })
        }
    }

    unsafe fn load_symbol<T>(library: &Library, symbol: &str) -> Result<Symbol<Option<T>>> {
        library
            .get(symbol.as_bytes())
            .map(|s: libloading::Symbol<Option<T>>| s.into_raw())
            .map_err(|e| anyhow!("error loading symbol '{symbol}': {e:?}"))
            .and_then(|s| {
                if s.is_some() {
                    Ok(s)
                } else {
                    bail!("Core does not declare symbol '{symbol}'")
                }
            })
    }
}
