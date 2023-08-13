#include "libretro.h"

typedef typeof(retro_api_version) retro_api_version_t;

typedef typeof(retro_set_environment) retro_set_environment_t;
typedef typeof(retro_set_video_refresh) retro_set_video_refresh_t;
typedef typeof(retro_set_audio_sample) retro_set_audio_sample_t;
typedef typeof(retro_set_audio_sample_batch) retro_set_audio_sample_batch_t;
typedef typeof(retro_set_input_poll) retro_set_input_poll_t;
typedef typeof(retro_set_input_state) retro_set_input_state_t;

typedef typeof(retro_init) retro_init_t;
typedef typeof(retro_deinit) retro_deinit_t;

typedef typeof(retro_get_system_info) retro_get_system_info_t;
typedef typeof(retro_get_system_av_info) retro_get_system_av_info_t;

typedef typeof(retro_set_controller_port_device) retro_set_controller_port_device_t;

typedef typeof(retro_reset) retro_reset_t;
typedef typeof(retro_run) retro_run_t;

typedef typeof(retro_serialize_size) retro_serialize_size_t;
typedef typeof(retro_serialize) retro_serialize_t;
typedef typeof(retro_unserialize) retro_unserialize_t;

typedef typeof(retro_load_game) retro_load_game_t;
typedef typeof(retro_unload_game) retro_unload_game_t;

typedef typeof(retro_get_region) retro_get_region_t;

typedef typeof(retro_get_memory_data) retro_get_memory_data_t;
typedef typeof(retro_get_memory_size) retro_get_memory_size_t;
