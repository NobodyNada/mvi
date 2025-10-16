#![allow(dead_code)]

use sha2::Digest;
use std::{
    ffi::c_void,
    ops::{Deref, DerefMut},
    ptr::null_mut,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, Mutex, RwLock,
    },
};

use anyhow::{ensure, Result};
use libloading::Library;
use libretro_ffi::*;

pub mod info;
pub mod trace;

mod savestate;
pub use savestate::*;

mod symbols;
use symbols::CoreSymbols;

use crate::tas::input::InputPort;

pub struct Core {
    pub system_info: retro_system_info,
    pub av_info: retro_system_av_info,
    pub frame: Frame,
    pub id: String,
    pub rom_path: std::path::PathBuf,
    pub rom_sha256: [u8; 32],
    pub trace_fields: Option<Arc<trace::Fields>>,
    instance_id: usize,
    savestate_buffer: Box<[u8]>,
}
unsafe impl Send for Core {}

#[expect(clippy::type_complexity)]
pub struct CoreImpl {
    library: Library,
    pixel_format: PixelFormat,
    memory_map: Vec<retro_memory_descriptor>,
    frame: Frame,
    input: *const [u8],
    input_ports: *const [InputPort],
    audio_callback: Option<*mut dyn FnMut(&[AudioFrame])>,
    trace_context: *mut retro_trace_ctx_t,
    trace_buffer: Vec<u8>,
}

// The input_callback cannot be safely sent across threads, but we are careful to never do so.
unsafe impl Send for CoreImpl {}

pub struct Frame {
    pub width: usize,
    pub height: usize,
    pub buffer: Vec<[u8; 4]>,
}

impl Frame {
    pub fn blank(av_info: &retro_system_av_info) -> Self {
        let width = av_info.geometry.base_width as usize;
        let height = av_info.geometry.base_height as usize;
        Frame {
            width,
            height,
            buffer: std::iter::repeat_n([0, 0, 0, 255], width * height).collect(),
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct AudioFrame {
    pub l: i16,
    pub r: i16,
}

static CORE: Mutex<Option<CoreImpl>> = Mutex::new(None);
static SYMBOLS: RwLock<Option<CoreSymbols>> = RwLock::new(None);
static ID: AtomicUsize = AtomicUsize::new(0);
impl Core {
    pub unsafe fn load(path: &std::path::Path, game_path: &std::path::Path) -> Result<Core> {
        unsafe {
            CoreImpl::load(path)?;

            let mut system_info = std::mem::MaybeUninit::uninit();
            (symbols().retro_get_system_info)(system_info.as_mut_ptr());
            let system_info = system_info.assume_init();

            let rom = std::fs::read(game_path)?;
            let rom_sha256 = sha2::Sha256::digest(&rom).into();

            let game_info = if system_info.need_fullpath {
                retro_game_info {
                    path: game_path.as_os_str().as_encoded_bytes().as_ptr().cast(),
                    data: std::ptr::null(),
                    size: 0,
                    meta: std::ptr::null(),
                }
            } else {
                let (data, size) = (rom.as_ptr().cast(), rom.len());
                retro_game_info {
                    path: std::ptr::null(),
                    data,
                    size,
                    meta: std::ptr::null(),
                }
            };

            ensure!(
                (symbols().retro_load_game)(&game_info),
                "Failed to load game"
            );

            let mut av_info = std::mem::MaybeUninit::uninit();
            (symbols().retro_get_system_av_info)(av_info.as_mut_ptr());
            let av_info = av_info.assume_init();

            let savestate_buffer: Box<[u8]> =
                std::iter::repeat_n(0, (symbols().retro_serialize_size)()).collect();

            let trace_ctx = lock().trace_context;
            let trace_fields = if !trace_ctx.is_null() {
                let mut trace_fields = vec![];
                let mut field = (*trace_ctx).fields;
                while !(*field).name.is_null() {
                    trace_fields.push(trace::Field {
                        name: std::ffi::CStr::from_ptr((*field).name)
                            .to_str()
                            .unwrap()
                            .to_string(),
                        offset: (*field).offset,
                        size: (*field).len,
                        field_type: trace::FieldType::from((*field).flags),
                    });
                    field = field.add(1);
                }
                Some(Arc::new(trace::Fields {
                    fields: trace_fields,
                    end: (*field).offset,
                }))
            } else {
                None
            };

            let core = Core {
                system_info,
                id: path.file_stem().unwrap().to_string_lossy().into_owned(),
                rom_path: game_path.to_owned(),
                rom_sha256,
                trace_fields,
                av_info: dbg!(av_info),
                frame: Frame::blank(&av_info),
                instance_id: ID.fetch_add(1, Ordering::Relaxed),
                savestate_buffer,
            };
            Ok(core)
        }
    }

    pub fn lock(&mut self) -> impl DerefMut<Target = CoreImpl> {
        lock()
    }

    pub fn read_memory_byte(&self, addr: usize) -> Option<u8> {
        lock().get_memory_byte(addr).copied()
    }

    pub fn run_frame(
        &mut self,
        input: &[u8],
        input_ports: &[crate::tas::input::InputPort],
        mut audio_callback: impl FnMut(&[AudioFrame]),
    ) {
        let run = *symbols().retro_run;
        unsafe {
            let mut core = self.lock();
            core.input = input as *const [u8];
            core.input_ports = input_ports as *const [InputPort];
            core.audio_callback = Some(std::ptr::addr_of_mut!(audio_callback) as *mut _);
            std::mem::drop(core);

            run();

            let mut core = self.lock();
            core.input = std::ptr::slice_from_raw_parts(std::ptr::null(), 0);
            core.input_ports = std::ptr::slice_from_raw_parts(std::ptr::null(), 0);
            core.audio_callback = None;
        }

        std::mem::swap(&mut self.frame, &mut lock().frame);
    }

    /// Saves the current state of the emulated system.
    pub fn save_state(
        &mut self,
        input: Option<(&[u8], &[crate::tas::input::InputPort])>,
    ) -> SavestateBuffer<'_> {
        unsafe {
            if let Some(input) = input {
                let mut core = self.lock();
                core.input = input.0 as *const [u8];
                core.input_ports = input.1 as *const [InputPort];
                std::mem::drop(core);
            }

            assert!((symbols().retro_serialize)(
                self.savestate_buffer.as_mut_ptr().cast(),
                self.savestate_buffer.len()
            ));

            let mut core = self.lock();
            core.input = std::ptr::slice_from_raw_parts(std::ptr::null(), 0);
            core.input_ports = std::ptr::slice_from_raw_parts(std::ptr::null(), 0);
        }
        SavestateBuffer::new(&self.savestate_buffer, self.instance_id)
    }

    /// Restores a savestate.
    /// Returns a reference to the temporary buffer containing the decompressed state.
    pub fn restore_state(&mut self, state: SavestateRef) -> SavestateBuffer<'_> {
        assert_eq!(state.core_id(), self.instance_id);
        state.decompress(&mut self.savestate_buffer);

        unsafe {
            assert!((symbols().retro_unserialize)(
                self.savestate_buffer.as_ptr().cast(),
                self.savestate_buffer.len()
            ));
        }

        SavestateBuffer::new(&self.savestate_buffer, self.instance_id)
    }

    /// Restores a delta savestate, given its parent savestate.
    /// Returns a reference to the temporary buffer containing the decompressed state.
    pub fn restore_delta(&mut self, state: &DeltaSavestate) -> SavestateBuffer<'_> {
        assert_eq!(state.core_id, self.instance_id);
        state.decompress(&mut self.savestate_buffer);

        unsafe {
            assert!((symbols().retro_unserialize)(
                self.savestate_buffer.as_ptr().cast(),
                self.savestate_buffer.len()
            ));
        }

        SavestateBuffer::new(&self.savestate_buffer, self.instance_id)
    }
}
impl Drop for Core {
    fn drop(&mut self) {
        let mut lock = CORE.lock().unwrap();
        *lock = None;
    }
}

struct CoreGuard<T: Deref<Target = Option<CoreImpl>>>(T);
impl<T: Deref<Target = Option<CoreImpl>>> Deref for CoreGuard<T> {
    type Target = CoreImpl;
    fn deref(&self) -> &Self::Target {
        self.0.deref().as_ref().unwrap()
    }
}
impl<T: DerefMut<Target = Option<CoreImpl>>> DerefMut for CoreGuard<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.deref_mut().as_mut().unwrap()
    }
}
struct SymbolsGuard<T: Deref<Target = Option<CoreSymbols>>>(T);
impl<T: Deref<Target = Option<CoreSymbols>>> Deref for SymbolsGuard<T> {
    type Target = CoreSymbols;
    fn deref(&self) -> &Self::Target {
        self.0.deref().as_ref().unwrap()
    }
}

fn lock() -> impl DerefMut<Target = CoreImpl> {
    CoreGuard(CORE.lock().unwrap())
}
fn symbols() -> impl Deref<Target = CoreSymbols> {
    SymbolsGuard(SYMBOLS.read().unwrap())
}

#[repr(u32)]
#[derive(Clone, Copy, Debug)]
enum PixelFormat {
    XRGB1555 = retro_pixel_format_RETRO_PIXEL_FORMAT_0RGB1555,
    XRGB8888 = retro_pixel_format_RETRO_PIXEL_FORMAT_XRGB8888,
    RGB565 = retro_pixel_format_RETRO_PIXEL_FORMAT_RGB565,
}

impl PixelFormat {
    fn stride(&self) -> usize {
        match self {
            PixelFormat::XRGB1555 | PixelFormat::RGB565 => 2,
            PixelFormat::XRGB8888 => 4,
        }
    }

    unsafe fn read(&self, buf: *const c_void) -> [u8; 4] {
        let pixel: u32 = match self {
            PixelFormat::XRGB8888 => buf.cast::<u32>().read(),
            PixelFormat::XRGB1555 | PixelFormat::RGB565 => buf.cast::<u16>().read() as u32,
        };
        self.convert(pixel)
    }

    #[allow(clippy::identity_op)]
    #[rustfmt::skip]
    /// Converts a pixel from this format to R8G8B8A8.
    fn convert(&self, pixel: u32) -> [u8; 4] {
        match self {
            PixelFormat::XRGB1555 => {
                let r = ((pixel >> 10) & 0x1f) as u8;
                let g = ((pixel >>  5) & 0x1f) as u8;
                let b = ((pixel >>  0) & 0x1f) as u8;
                [
                    (r << 3) | (r & 0x7),
                    (g << 3) | (g & 0x7),
                    (b << 3) | (b & 0x7),
                    0xff
                ]
            }
            PixelFormat::XRGB8888 => [
                (pixel >> 16) as u8,
                (pixel >> 8) as u8,
                (pixel >> 0) as u8,
                0xff,
            ],
            PixelFormat::RGB565 => {
                let r = ((pixel >> 11) & 0x1f) as u8;
                let g = ((pixel >>  5) & 0x3f) as u8;
                let b = ((pixel >>  0) & 0x1f) as u8;
                [
                    (r << 3) | (r & 0x7),
                    (g << 2) | (g & 0x3),
                    (b << 3) | (b & 0x7),
                    0xff
                ]
            }
        }
    }
}
impl TryFrom<retro_pixel_format> for PixelFormat {
    type Error = ();
    fn try_from(value: retro_pixel_format) -> std::result::Result<Self, Self::Error> {
        match value {
            x if x == Self::XRGB1555 as retro_pixel_format => Ok(Self::XRGB1555),
            x if x == Self::XRGB8888 as retro_pixel_format => Ok(Self::XRGB8888),
            x if x == Self::RGB565 as retro_pixel_format => Ok(Self::RGB565),
            _ => Err(()),
        }
    }
}

impl CoreImpl {
    unsafe fn load(path: &std::path::Path) -> Result<()> {
        let mut lock = CORE.lock().unwrap();
        assert!(lock.is_none(), "Only one core can be loaded at once");

        let library = libloading::Library::new(path)?;
        let symbols = CoreSymbols::new(&library)?;
        *SYMBOLS.write().unwrap() = Some(symbols);
        let result = CoreImpl {
            library,
            pixel_format: PixelFormat::XRGB1555,
            frame: Frame {
                width: 0,
                height: 0,
                buffer: Vec::new(),
            },
            memory_map: vec![],
            input: std::ptr::slice_from_raw_parts(std::ptr::null(), 0),
            input_ports: std::ptr::slice_from_raw_parts(std::ptr::null(), 0),
            audio_callback: None,
            trace_context: null_mut(),
            trace_buffer: vec![],
        };
        ensure!(
            result.retro_api_version() == RETRO_API_VERSION,
            "core declares incompatible libretro version"
        );

        *lock = Some(result);
        std::mem::drop(lock);

        let symbols = SYMBOLS.read().unwrap();
        let symbols = symbols.as_ref().unwrap();
        (symbols.retro_set_environment)(Some(environment_callback));
        (symbols.retro_set_video_refresh)(Some(video_refresh_callback));
        (symbols.retro_set_audio_sample)(Some(audio_sample_callback));
        (symbols.retro_set_audio_sample_batch)(Some(audio_samples_callback));
        (symbols.retro_set_input_poll)(Some(input_poll_callback));
        (symbols.retro_set_input_state)(Some(input_state_callback));

        (symbols.retro_init)();
        Ok(())
    }

    pub fn retro_api_version(&self) -> u32 {
        unsafe { (symbols().retro_api_version)() }
    }

    pub fn get_memory_map(&self) -> &[retro_memory_descriptor] {
        &self.memory_map
    }

    pub fn get_memory_byte(&mut self, addr: usize) -> Option<&mut u8> {
        for mapping in &self.memory_map {
            if (addr & mapping.select) == (mapping.start & mapping.select) {
                // Ignore disconnected address lines of the mapping:
                // For each set bit of `disconnect`, shift all higher bits of `addr` to the right.
                let mut disconnect = mapping.disconnect;
                let mut addr = addr;
                while disconnect != 0 {
                    let bit = disconnect.trailing_zeros();
                    let low_bits = addr & ((1 << bit) - 1);
                    let high_bits = addr & !((1 << (bit + 1)) - 1);
                    addr = low_bits | (high_bits >> 1);

                    disconnect &= !(1 << bit);
                    disconnect >>= 1;
                }

                addr %= mapping.len;

                unsafe { return mapping.ptr.add(mapping.offset + addr).cast::<u8>().as_mut() }
            }
        }
        None
    }

    fn video_refresh_callback(
        &mut self,
        data: *const c_void,
        width: u32,
        height: u32,
        pitch: usize,
    ) {
        let (width, height) = (width as usize, height as usize);
        self.frame.width = width;
        self.frame.height = height;

        let buf = &mut self.frame.buffer;
        buf.clear();
        buf.reserve(width * height);

        unsafe {
            for y in 0..height {
                let line = data.add(y * pitch);
                for x in 0..width {
                    let pixel = line.add(x * self.pixel_format.stride());
                    buf.push(self.pixel_format.read(pixel));
                }
            }
        }
    }

    fn audio_callback(&mut self, data: &[AudioFrame]) {
        unsafe {
            if let Some(cb) = self.audio_callback {
                (*cb)(data)
            }
        }
    }

    fn set_pixel_format(&mut self, format: retro_pixel_format) -> bool {
        let Ok(pixel_format) = PixelFormat::try_from(format) else {
            return false;
        };
        self.pixel_format = dbg!(pixel_format);
        true
    }

    fn set_memory_maps(&mut self, map: retro_memory_map) -> bool {
        if map.num_descriptors == 0 {
            self.memory_map = vec![];
        } else {
            let map = unsafe {
                std::slice::from_raw_parts(map.descriptors, map.num_descriptors as usize)
            };
            self.memory_map = map.to_vec();
        }
        true
    }

    fn set_trace_context(&mut self, ctx: *mut retro_trace_ctx_t) -> bool {
        self.trace_context = ctx;
        true
    }

    fn no_input_callback(_port: u32, _device: u32, _index: u32, _id: u32) -> i16 {
        unreachable!()
    }
}

unsafe extern "C" fn environment_callback(cmd: u32, data: *mut c_void) -> bool {
    let mut core = lock();
    match cmd {
        RETRO_ENVIRONMENT_SET_PIXEL_FORMAT => core.set_pixel_format(*data.cast()),
        RETRO_ENVIRONMENT_SET_MEMORY_MAPS => core.set_memory_maps(*data.cast()),
        RETRO_ENVIRONMENT_SET_TRACE_CONTEXT => core.set_trace_context(data.cast()),
        _ => false,
    }
}

unsafe extern "C" fn video_refresh_callback(
    data: *const c_void,
    width: u32,
    height: u32,
    pitch: usize,
) {
    lock().video_refresh_callback(data, width, height, pitch)
}

unsafe extern "C" fn audio_sample_callback(l: i16, r: i16) {
    lock().audio_callback(&[AudioFrame { l, r }]);
}

unsafe extern "C" fn audio_samples_callback(data: *const i16, frames: usize) -> usize {
    lock().audio_callback(std::slice::from_raw_parts(data.cast(), frames));
    frames
}

unsafe extern "C" fn input_poll_callback() {}

unsafe extern "C" fn input_state_callback(port: u32, _device: u32, index: u32, id: u32) -> i16 {
    let core = lock();
    assert!(!core.input.is_null());
    assert!(!core.input_ports.is_null());

    let input = &*core.input;
    let input_ports = &*core.input_ports;
    let port = port as usize;

    if port < input_ports.len() {
        let offset: usize = input_ports[0..port]
            .iter()
            .map(crate::tas::input::InputPort::frame_size)
            .sum();
        let len = input_ports[port].frame_size();

        input_ports[port].read(&input[offset..(offset + len)], index, id)
    } else {
        0
    }
}
