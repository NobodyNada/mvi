use std::sync::{Arc, mpsc};

use anyhow::{Error, Result, anyhow, bail};
use crossbeam::atomic::AtomicCell;
use winit::{keyboard::NamedKey, platform::modifier_supplement::KeyEventExtModifierSupplement};
use zerocopy::IntoBytes;

use crate::{
    core::{self, Frame},
    tas::{self, Tas},
};

use self::{audio::AudioWriter, backend::render::Renderer};

mod audio;
mod backend;
mod debug;
mod keybinds;
mod menu;
mod piano_roll;
mod ramwatch;

pub struct Ui {
    tas: Option<Tas>,
    keybinds: keybinds::Keybinds,
    piano_roll: piano_roll::PianoRoll,
    ramwatch: ramwatch::RamWatch,
    framebuffer: Option<Framebuffer>,
    audio: Option<AudioWriter>,

    core_db: Option<core::info::CoreDb>,
    movie_cache: tas::movie::file::MovieCache,

    tokio: tokio::runtime::Runtime,

    modifiers: winit::keyboard::ModifiersState,

    // Modal window state
    download_progress: Option<Download>,
    core_selector: Option<menu::CoreSelector>,
    hash_mismatch: Option<menu::HashMismatch>,
    keybind_editor: Option<keybinds::KeybindEditor>,
    reported_error: Option<ReportedError>,

    trace_debuger: Option<debug::trace::TraceDebugger>,

    ignore_events: bool,
}

struct Download {
    item: DownloadItem,
    progress: Arc<AtomicCell<core::info::Progress>>,
}

#[expect(clippy::type_complexity)]
enum DownloadItem {
    CoreDb(mpsc::Receiver<core::info::Result<core::info::CoreDb>>),
    Core {
        id: String,
        rx: mpsc::Receiver<(
            core::info::Result<std::path::PathBuf>,
            Box<dyn FnOnce(&mut Ui, core::info::Result<std::path::PathBuf>) + Send + 'static>,
        )>,
    },
}

struct ReportedError {
    error: Error,
    is_fatal: bool,
}

impl DownloadItem {
    fn name(&self) -> String {
        match self {
            DownloadItem::CoreDb(_) => "core database".to_string(),
            DownloadItem::Core { id, .. } => format!("core '{id}'"),
        }
    }
}

pub fn run() -> Result<()> {
    let tokio = tokio::runtime::Runtime::new()?;
    // Load the core database from disk, or start a download if it's not present
    let core_db;
    let download_progress;

    match core::info::CoreDb::load_cached() {
        Ok(db) => {
            core_db = Some(db);
            download_progress = None;
        }
        Err(core::info::Error::Io(e)) if e.kind() == std::io::ErrorKind::NotFound => {
            core_db = None;

            let (tx, rx) = std::sync::mpsc::sync_channel(1);
            let progress: Arc<AtomicCell<core::info::Progress>> = Arc::default();

            download_progress = Some(Download {
                item: DownloadItem::CoreDb(rx),
                progress: progress.clone(),
            });

            tokio.spawn(async move {
                tx.try_send(core::info::CoreDb::download(Arc::downgrade(&progress)).await)
                    .unwrap()
            });
        }
        Err(e) => bail!(e),
    }

    let (event_tx, event_rx) = std::sync::mpsc::channel();
    backend::run(
        move || {
            move |event, _imgui, _window| {
                _ = event_tx.send(event);
            }
        },
        move || {
            let mut ui = Ui {
                tas: None,
                keybinds: keybinds::Keybinds::new(),
                piano_roll: piano_roll::PianoRoll::new(),
                ramwatch: ramwatch::RamWatch::new(),
                framebuffer: None,
                audio: None,
                core_db,
                movie_cache: tas::movie::file::MovieCache::load(),
                tokio,

                download_progress,
                reported_error: None,
                core_selector: None,
                hash_mismatch: None,
                keybind_editor: None,
                trace_debuger: None,

                modifiers: Default::default(),
                ignore_events: false,
            };

            move |imgui, renderer| {
                while let Ok(e) = event_rx.try_recv() {
                    ui.handle_event(e);
                }
                ui.render(imgui, renderer);
            }
        },
    )
}

impl Ui {
    fn render(&mut self, ui: &mut imgui::Ui, renderer: &mut Renderer) {
        // Render the UI

        let mut ignore_events = false;

        let flags =
        // No borders etc for top-level window
        imgui::WindowFlags::NO_DECORATION | imgui::WindowFlags::NO_MOVE
        // Show menu bar
        | imgui::WindowFlags::MENU_BAR
        // Don't raise window on focus (as it'll clobber floating windows)
        | imgui::WindowFlags::NO_BRING_TO_FRONT_ON_FOCUS | imgui::WindowFlags::NO_NAV_FOCUS
        // Don't want the dock area's parent to be dockable!
        | imgui::WindowFlags::NO_DOCKING
        ;
        let _token = unsafe {
            let viewport = imgui::sys::igGetMainViewport().as_ref().unwrap();
            let _padding = ui.push_style_var(imgui::StyleVar::WindowPadding([0., 0.]));
            let _rounding = ui.push_style_var(imgui::StyleVar::WindowRounding(0.));
            let _border = ui.push_style_var(imgui::StyleVar::WindowBorderSize(0.));
            ui.window("mvi")
                .position(<[f32; 2]>::from(viewport.Pos), imgui::Condition::Always)
                .size([viewport.Size.x, viewport.Size.y], imgui::Condition::Always)
                .flags(flags)
                .begin()
        };

        let dockspace_id =
            unsafe { std::mem::transmute::<imgui::Id, u32>(ui.new_id_str("MviDockspace")) };

        unsafe {
            let dockspace_needs_setup = imgui::sys::igDockBuilderGetNode(dockspace_id).is_null();

            let menu_height =
                imgui::sys::ImGuiWindow_MenuBarHeight(imgui::sys::igGetCurrentWindowRead());
            imgui::sys::igDockSpace(
                dockspace_id,
                [ui.window_size()[0], ui.window_size()[1] - menu_height].into(),
                imgui::sys::ImGuiDockNodeFlags_PassthruCentralNode as i32,
                std::ptr::null(),
            );
            if dockspace_needs_setup {
                let (mut left, mut right) = (0, 0);
                imgui::sys::igDockBuilderSplitNode(
                    dockspace_id,
                    imgui::sys::ImGuiDir_Left,
                    0.2,
                    &mut left,
                    &mut right,
                );

                imgui::sys::igDockBuilderDockWindow(
                    std::ffi::CString::new("Piano Roll")
                        .unwrap()
                        .as_bytes_with_nul()
                        .as_ptr()
                        .cast(),
                    left,
                );
                imgui::sys::igDockBuilderDockWindow(
                    std::ffi::CString::new("Game View")
                        .unwrap()
                        .as_bytes_with_nul()
                        .as_ptr()
                        .cast(),
                    right,
                );
            }
        }

        self.draw_menu(ui);
        ignore_events |= self.draw_core_selector(ui);
        ignore_events |= self.draw_hash_mismatch(ui);

        if let Some(error) = &self.reported_error {
            ignore_events = true;
            const ID: &str = "Error";
            match ui
                .modal_popup_config(ID)
                .flags(imgui::WindowFlags::ALWAYS_AUTO_RESIZE)
                .begin_popup()
            {
                Some(_token) => {
                    ui.text(format!("{}", error.error));
                    if ui.collapsing_header("Details", imgui::TreeNodeFlags::FRAMED) {
                        ui.text(format!("{:?}", error.error));
                    }

                    #[expect(clippy::collapsible_else_if)]
                    if error.is_fatal {
                        if ui.button("Exit") {
                            std::process::exit(1);
                        }
                    } else {
                        if ui.button("Close") {
                            ui.close_current_popup();
                        }
                    }
                }
                _ => {
                    ui.open_popup(ID);
                }
            }
            if error.is_fatal {
                return;
            }
        }

        if self.download_progress.is_some() {
            ignore_events = true;
            let mut completed = false;
            match &self.download_progress.as_ref().unwrap().item {
                DownloadItem::CoreDb(rx) => match rx.try_recv() {
                    Ok(Ok(db)) => {
                        completed = true;
                        self.core_db = Some(db);
                    }
                    Ok(Err(e)) => {
                        completed = true;
                        self.report_error(
                            anyhow!(e).context("Failed to download core database"),
                            true,
                        );
                    }
                    Err(mpsc::TryRecvError::Empty) => {}
                    Err(mpsc::TryRecvError::Disconnected) => panic!("downloader is dead?"),
                },
                DownloadItem::Core { id: _, rx } => match rx.try_recv() {
                    Ok((path, callback)) => {
                        completed = true;
                        callback(self, path);
                    }
                    Err(mpsc::TryRecvError::Empty) => {}
                    Err(mpsc::TryRecvError::Disconnected) => panic!("downloader is dead?"),
                },
            }

            let download = self.download_progress.as_ref().unwrap();
            const ID: &str = "Download progress";
            Ui::set_popup_position(ui);
            match ui.begin_modal_popup(ID) {
                Some(_token) => {
                    if completed {
                        ui.close_current_popup();
                    }
                    ui.text(format!("Downloading {}...", download.item.name()));
                    let progress = download.progress.load();
                    let (fraction, text) = match progress.total {
                        Some(total) => (
                            progress.downloaded as f32 / total as f32,
                            format!(
                                "{} / {} kB",
                                progress.downloaded / 1000,
                                total.div_ceil(1000)
                            ),
                        ),
                        None => (0., format!("{} kB", progress.downloaded)),
                    };
                    imgui::ProgressBar::new(fraction)
                        .overlay_text(text)
                        .build(ui);
                }
                _ => {
                    ui.open_popup(ID);
                }
            }

            if completed {
                self.download_progress = None;
            }
        }

        if let Some(editor) = self.keybind_editor.as_mut() {
            ignore_events = true;
            const ID: &str = "Keybind Editor";
            Ui::set_popup_position(ui);
            Ui::set_popup_size(ui, ui.window_size());
            match ui.begin_modal_popup(ID) {
                Some(_token) => {
                    if let Some(save) = editor.draw(ui, self.modifiers) {
                        let editor = self.keybind_editor.take().unwrap();
                        if save {
                            self.handle_error(|ui| editor.apply(&mut ui.keybinds));
                        }
                    }
                }
                _ => {
                    ui.open_popup(ID);
                }
            }
        }

        if self.tas.is_some() {
            ui.window("Game View")
                .size([512., 448.], imgui::Condition::FirstUseEver)
                .build(|| {
                    let tas = self.tas.as_mut().unwrap();
                    let av = tas.av_info();
                    let aspect_ratio = av.geometry.aspect_ratio;
                    let available_size = ui.content_region_avail();
                    let (w, h) = [
                        (available_size[0], available_size[0] / aspect_ratio),
                        (available_size[1] * aspect_ratio, available_size[1]),
                    ]
                    .into_iter()
                    .filter(|(w, h)| w <= &available_size[0] && h <= &available_size[1])
                    .max_by(|(w1, _), (w2, _)| w1.partial_cmp(w2).unwrap())
                    .unwrap();

                    let (_, framebuffer) = self.run_frame(renderer).unwrap();

                    imgui::Image::new(framebuffer.texture.id, [w, h]).build(ui)
                });

            ignore_events |= self.ramwatch.draw(ui, self.tas.as_mut().unwrap());
            self.piano_roll
                .draw(ui, self.tas.as_mut().unwrap(), &self.keybinds);

            if let Some(trace) = &mut self.trace_debuger
                && !trace.draw(ui, self.tas.as_mut().unwrap())
            {
                self.trace_debuger = None;
            }
        }

        self.ignore_events = ignore_events;
    }

    fn handle_event(&mut self, event: winit::event::WindowEvent) {
        match event {
            winit::event::WindowEvent::ModifiersChanged(m) => self.modifiers = m.state(),
            winit::event::WindowEvent::KeyboardInput {
                event: e @ winit::event::KeyEvent { state, .. },
                ..
            } => {
                let key = e.key_without_modifiers();
                if let winit::keyboard::Key::Named(
                    NamedKey::Control | NamedKey::Shift | NamedKey::Super | NamedKey::Alt,
                ) = key
                {
                    // These are handled by ModifiersChanged
                    return;
                }
                match state {
                    winit::event::ElementState::Pressed => {
                        if let Some(editor) = self.keybind_editor.as_mut() {
                            editor.key_down(key, self.modifiers);
                        } else if let Some(tas) = &mut self.tas
                            && !self.ignore_events
                        {
                            self.keybinds
                                .key_down(key, self.modifiers, tas, &mut self.piano_roll)
                        }
                    }
                    winit::event::ElementState::Released => {
                        if self.keybind_editor.is_none()
                            && let Some(tas) = &mut self.tas
                        {
                            self.keybinds
                                .key_up(key, self.modifiers, tas, &mut self.piano_roll)
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn set_popup_position(ui: &imgui::Ui) {
        let window_center = [
            ui.window_pos()[0] + ui.window_size()[0] / 2.,
            ui.window_pos()[1] + ui.window_size()[1] / 2.,
        ];
        unsafe {
            imgui::sys::igSetNextWindowPos(
                window_center.into(),
                imgui::Condition::Always as i32,
                [0.5, 0.5].into(),
            );
        }
    }

    fn set_popup_size(_ui: &imgui::Ui, size: [f32; 2]) {
        unsafe { imgui::sys::igSetNextWindowSize(size.into(), imgui::Condition::Always as i32) }
    }

    fn load_game<F: FnOnce(core::Core) -> Result<Tas>>(
        &mut self,
        core_path: &std::path::Path,
        game_path: &std::path::Path,
        tas: F,
    ) -> Result<()> {
        self.keybinds.reset();
        self.tas = None;
        self.framebuffer = None;
        let core = unsafe { core::Core::load(core_path, game_path)? };
        self.audio = Some(AudioWriter::new(core.av_info.timing.sample_rate as usize)?);
        self.tas = Some(tas(core)?);
        Ok(())
    }

    fn run_frame(&mut self, renderer: &mut Renderer) -> Option<(&Frame, &mut Framebuffer)> {
        let tas = self.tas.as_mut()?;
        tas.set_trace_enabled(self.trace_debuger.is_some());
        let frame = tas.run_host_frame(|samples| {
            if let Some(writer) = self.audio.as_mut() {
                writer.write(samples);
            }
        });

        if let Some(framebuffer) = &self.framebuffer
            && (framebuffer.width != frame.width || framebuffer.height != frame.height)
        {
            self.framebuffer = None;
        }

        let size = wgpu::Extent3d {
            width: frame.width as u32,
            height: frame.height as u32,
            depth_or_array_layers: 1,
        };
        // Create the framebuffer, if it does not already exist
        let framebuffer = self.framebuffer.get_or_insert_with(|| {
            let image = renderer
                .device()
                .create_texture(&wgpu::wgt::TextureDescriptor {
                    label: Some("Emulator Framebuffer"),
                    size,
                    mip_level_count: 1,
                    sample_count: 1,
                    dimension: wgpu::TextureDimension::D2,
                    format: wgpu::TextureFormat::Rgba8UnormSrgb,
                    usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
                    view_formats: &[],
                });
            let texture = renderer.create_texture(&image).unwrap();

            Framebuffer {
                image,
                texture,
                width: frame.width,
                height: frame.height,
            }
        });

        // Upload the frame to the GPU
        renderer.queue().write_texture(
            wgpu::TexelCopyTextureInfo {
                texture: &framebuffer.image,
                mip_level: 0,
                origin: Default::default(),
                aspect: wgpu::TextureAspect::All,
            },
            frame.buffer.as_bytes(),
            wgpu::TexelCopyBufferLayout {
                bytes_per_row: Some(framebuffer.width as u32 * 4),
                ..Default::default()
            },
            size,
        );

        Some((frame, framebuffer))
    }

    fn report_error(&mut self, error: Error, is_fatal: bool) {
        if self.reported_error.is_none()
            || is_fatal && !self.reported_error.as_ref().unwrap().is_fatal
        {
            self.reported_error = Some(ReportedError { error, is_fatal });
        }
    }

    fn handle_error<F: FnOnce(&mut Ui) -> anyhow::Result<()>>(&mut self, f: F) {
        match f(self) {
            Ok(()) => {}
            Err(e) => self.report_error(e, false),
        }
    }
}

struct Framebuffer {
    image: wgpu::Texture,
    texture: Arc<backend::render::Texture>,
    width: usize,
    height: usize,
}
