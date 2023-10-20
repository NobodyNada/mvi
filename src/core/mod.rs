#![allow(dead_code)]

use std::{
    ffi::c_void,
    ops::{Deref, DerefMut},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Mutex, RwLock,
    },
};

use anyhow::{ensure, Result};
use libloading::Library;
use libretro_ffi::*;

mod symbols;
use symbols::CoreSymbols;

pub struct Core {
    pub system_info: retro_system_info,
    pub av_info: retro_system_av_info,
    pub frame: Frame,
    id: usize,
    savestate_size: usize,
}
unsafe impl Send for Core {}

pub struct CoreImpl {
    library: Library,
    rom_buf: Option<Vec<u8>>,
    pixel_format: PixelFormat,
    frame: Frame,
}

pub struct Frame {
    pub width: usize,
    pub height: usize,
    pub buffer: Vec<[u8; 4]>,
}

pub struct Savestate {
    state: Box<[u8]>,
    core_id: usize,
}

impl Frame {
    pub fn blank(av_info: &retro_system_av_info) -> Self {
        let width = av_info.geometry.base_width as usize;
        let height = av_info.geometry.base_height as usize;
        Frame {
            width,
            height,
            buffer: std::iter::repeat([0, 0, 0, 255])
                .take(width * height)
                .collect(),
        }
    }
}

static CORE: Mutex<Option<CoreImpl>> = Mutex::new(None);
static SYMBOLS: RwLock<Option<CoreSymbols>> = RwLock::new(None);
static ID: AtomicUsize = AtomicUsize::new(0);
impl Core {
    pub unsafe fn load(path: &str, game_path: &str) -> Result<Core> {
        unsafe {
            CoreImpl::load(path)?;

            let mut system_info = std::mem::MaybeUninit::uninit();
            (symbols().retro_get_system_info)(system_info.as_mut_ptr());
            let system_info = system_info.assume_init();

            let game_info = if system_info.need_fullpath {
                retro_game_info {
                    path: game_path.as_bytes().as_ptr().cast(),
                    data: std::ptr::null(),
                    size: 0,
                    meta: std::ptr::null(),
                }
            } else {
                let buf = std::fs::read(game_path)?;
                let (data, size) = (buf.as_ptr().cast(), buf.len());
                lock().rom_buf = Some(buf);
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

            let core = Core {
                system_info,
                av_info: dbg!(av_info),
                frame: Frame::blank(&av_info),
                id: ID.fetch_add(1, Ordering::Relaxed),
                savestate_size: (symbols().retro_serialize_size)(),
            };
            Ok(core)
        }
    }

    pub fn lock(&mut self) -> impl DerefMut<Target = CoreImpl> {
        lock()
    }

    pub fn run_frame(&mut self) {
        let run = *symbols().retro_run;
        unsafe {
            run();
        }

        std::mem::swap(&mut self.frame, &mut lock().frame);
    }

    pub fn save_state(&mut self) -> Savestate {
        unsafe {
            let state =
                std::alloc::alloc(std::alloc::Layout::array::<u8>(self.savestate_size).unwrap());
            assert!((symbols().retro_serialize)(
                state.cast(),
                self.savestate_size
            ));
            Savestate {
                state: Box::from_raw(std::ptr::slice_from_raw_parts_mut(
                    state,
                    self.savestate_size,
                )),
                core_id: self.id,
            }
        }
    }

    pub fn restore_state(&mut self, state: &Savestate) {
        assert_eq!(state.core_id, self.id);
        unsafe {
            assert!((symbols().retro_unserialize)(
                state.state.as_ptr().cast(),
                state.state.len()
            ));
        }
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
    unsafe fn load(path: &str) -> Result<()> {
        let mut lock = CORE.lock().unwrap();
        assert!(lock.is_none(), "Only one core can be loaded at once");

        let library = libloading::Library::new(path)?;
        let symbols = CoreSymbols::new(&library)?;
        *SYMBOLS.write().unwrap() = Some(symbols);
        let result = CoreImpl {
            library,
            rom_buf: None,
            pixel_format: PixelFormat::XRGB1555,
            frame: Frame {
                width: 0,
                height: 0,
                buffer: Vec::new(),
            },
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

    fn audio_callback(&mut self, _data: &[AudioFrame]) {}

    fn set_pixel_format(&mut self, format: retro_pixel_format) -> bool {
        let Ok(pixel_format) = PixelFormat::try_from(format) else {
            return false;
        };
        self.pixel_format = dbg!(pixel_format);
        true
    }
}

unsafe extern "C" fn environment_callback(cmd: u32, data: *mut c_void) -> bool {
    let mut core = lock();
    match cmd {
        RETRO_ENVIRONMENT_SET_PIXEL_FORMAT => core.set_pixel_format(*data.cast()),
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

unsafe extern "C" fn input_state_callback(port: u32, device: u32, index: u32, id: u32) -> i16 {
    0
}

#[repr(C)]
#[derive(Clone, Copy, Debug)]
struct AudioFrame {
    l: i16,
    r: i16,
}
