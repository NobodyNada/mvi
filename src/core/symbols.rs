use super::*;
use anyhow::anyhow;
#[cfg(unix)]
use libloading::os::unix::Symbol;

#[cfg(windows)]
use libloading::os::windows::Symbol;

pub struct CoreSymbols {
    pub retro_api_version: RequiredSymbol<retro_api_version_t>,

    pub retro_set_environment: RequiredSymbol<retro_set_environment_t>,
    pub retro_set_video_refresh: RequiredSymbol<retro_set_video_refresh_t>,
    pub retro_set_audio_sample: RequiredSymbol<retro_set_audio_sample_t>,
    pub retro_set_audio_sample_batch: RequiredSymbol<retro_set_audio_sample_batch_t>,
    pub retro_set_input_poll: RequiredSymbol<retro_set_input_poll_t>,
    pub retro_set_input_state: RequiredSymbol<retro_set_input_state_t>,

    pub retro_init: RequiredSymbol<retro_init_t>,
    pub retro_deinit: RequiredSymbol<retro_deinit_t>,

    pub retro_get_system_info: RequiredSymbol<retro_get_system_info_t>,
    pub retro_get_system_av_info: RequiredSymbol<retro_get_system_av_info_t>,

    pub retro_set_controller_port_device: RequiredSymbol<retro_set_controller_port_device_t>,

    pub retro_reset: RequiredSymbol<retro_reset_t>,
    pub retro_run: RequiredSymbol<retro_run_t>,

    pub retro_serialize_size: RequiredSymbol<retro_serialize_size_t>,
    pub retro_serialize: RequiredSymbol<retro_serialize_t>,
    pub retro_unserialize: RequiredSymbol<retro_unserialize_t>,

    pub retro_load_game: RequiredSymbol<retro_load_game_t>,
    pub retro_unload_game: RequiredSymbol<retro_unload_game_t>,

    pub retro_get_region: RequiredSymbol<retro_get_region_t>,

    pub retro_get_memory_data: RequiredSymbol<retro_get_memory_data_t>,
    pub retro_get_memory_size: RequiredSymbol<retro_get_memory_size_t>,
}

// The retro_*_t types are annoyingly optional, let's make a wrapper to unwrap them for us
pub struct RequiredSymbol<T: OptionSymbol>(Symbol<T::Unwrapped>);
impl<T: OptionSymbol> Deref for RequiredSymbol<T> {
    type Target = T::Unwrapped;
    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

pub trait OptionSymbol {
    type Unwrapped;
}
impl<S> OptionSymbol for Option<S> {
    type Unwrapped = S;
}

impl CoreSymbols {
    pub fn new(library: &Library) -> Result<Self> {
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

    unsafe fn load_symbol<T>(library: &Library, symbol: &str) -> Result<RequiredSymbol<Option<T>>> {
        unsafe {
            library
                .get(symbol.as_bytes())
                .map(|s: libloading::Symbol<Option<T>>| s.into_raw())
                .map_err(|e| anyhow!("error loading symbol '{symbol}': {e:?}"))
                .map(|s| Some(RequiredSymbol(s.lift_option()?)))
                .and_then(|s| s.ok_or_else(|| anyhow!("Core does not declare symbol '{symbol}")))
        }
    }
}
