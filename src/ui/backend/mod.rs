use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    rc::Rc,
    sync::{mpsc, Arc, Condvar, Mutex},
};

use anyhow::Result;
use imgui::{
    BackendFlags, Context, FontConfig, FontGlyphRanges, FontSource, Id, PlatformViewportBackend,
    Ui, ViewportFlags,
};
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use vk::swapchain::Surface;
use vulkano as vk;
use winit::{
    dpi::{LogicalPosition, LogicalSize, PhysicalPosition, PhysicalSize},
    event::{Event, WindowEvent},
    event_loop::EventLoopBuilder,
    window::{Window, WindowBuilder},
};

pub mod render;

const WINDOW_TITLE: &str = "mvi";

// TODO: the bottom window of a vertically stacked configuration is offset

pub fn run(
    mut event_callback: impl FnMut(&Event<()>, &mut Context, &Window) + Send,
    mut render_callback: impl FnMut(&mut Ui, &mut render::Renderer) + Send,
) -> Result<()> {
    // Create the application event loop
    let event_loop = EventLoopBuilder::with_user_event().build();

    // Define our Vulkan configuration
    let config = vulkano_util::context::VulkanoConfig {
        instance_create_info: vk::instance::InstanceCreateInfo {
            application_name: Some(WINDOW_TITLE.to_string()),
            enabled_extensions: vk::instance::InstanceExtensions {
                // Enable debugging features
                ext_debug_utils: true,
                ..Default::default()
            },
            // Allow us to use devices that do not fully support
            // the Vulkan specification, such as Apple graphics hardware.
            enumerate_portability: true,
            ..Default::default()
        },
        device_features: vk::device::Features {
            image_view_format_swizzle: true,
            ..Default::default()
        },

        // Use our callback to print debug messages.
        debug_create_info: Some(
            vk::instance::debug::DebugUtilsMessengerCreateInfo::user_callback(std::sync::Arc::new(
                debug,
            )),
        ),

        // Print the name of the automatically-selected device.
        print_device_name: true,
        ..Default::default()
    };

    // Create a Vulkan instance and device with our configuration.
    let vk_context = vulkano_util::context::VulkanoContext::new(config);
    let vk_instance = vk_context.instance().clone();

    // Create a window with a Vulkan surface.
    let window = WindowBuilder::new()
        .with_title(WINDOW_TITLE)
        .build(&event_loop)?;
    let window = Arc::new(window);
    let surface =
        vulkano_win::create_surface_from_handle(window.clone(), vk_instance.clone()).unwrap();

    // Launch a thread to do our work -- handling events, updating the UI, running the emulation
    // core, and rendering the screen. This way, we can tie the core loop to display refresh
    // without blocking the main thread & preventing OS libraries from doing UI things.
    //
    // There is a wart: windows can only be created in response to an event. In order to deal with
    // that, the worker thread can send the event loop thread a request to create a window (via a
    // user event). The event loop thread will respond by setting a condition variable.
    #[derive(Debug)]
    enum UserEvent {
        CreateWindow {
            request: Box<WindowBuilder>,
            response: Arc<(Condvar, Mutex<Option<CreateWindowResponse>>)>,
        },
        Exit,
    }
    #[derive(Debug)]
    struct CreateWindowResponse {
        window: Arc<Window>,
        surface: Arc<Surface>,
    }

    let (event_tx, event_rx) = mpsc::channel::<Event<'static, ()>>();
    let event_proxy = event_loop.create_proxy();

    std::thread::scope(|scope| {
        std::thread::Builder::new()
            .name("Rendering Thread".to_string())
            .spawn_scoped(scope, move || {
                // Initialize and configure imgui.
                let mut imgui = Context::create();
                imgui.io_mut().config_flags.insert(
                    imgui::ConfigFlags::DOCKING_ENABLE | imgui::ConfigFlags::VIEWPORTS_ENABLE,
                );

                let mut platform = WinitPlatform::init(&mut imgui);
                platform.attach_window(imgui.io_mut(), &window, HiDpiMode::Default);
                imgui.platform_io_mut().monitors.replace_from_slice(
                    &window
                        .available_monitors()
                        .map(|monitor| imgui::PlatformMonitor {
                            main_pos: monitor.position().cast::<f32>().into(),
                            work_pos: monitor.position().cast::<f32>().into(),
                            main_size: monitor.size().cast::<f32>().into(),
                            work_size: monitor.size().cast::<f32>().into(),
                            dpi_scale: monitor.scale_factor() as f32,
                        })
                        .collect::<Vec<_>>(),
                );

                // Define our font.
                const ARROW_GLYPHS: [u32; 3] = ['←' as u32, '↓' as u32, 0]; // ←→↑↓
                let font_config = FontConfig {
                    oversample_v: 1,
                    oversample_h: 1,
                    ..Default::default()
                };
                imgui.fonts().add_font(&[
                    FontSource::DefaultFontData {
                        config: Some(font_config.clone()),
                    },
                    FontSource::TtfData {
                        data: include_bytes!("./arrows.ttf"),
                        size_pixels: 13.,
                        config: Some(FontConfig {
                            glyph_ranges: FontGlyphRanges::from_slice(&ARROW_GLYPHS),
                            ..font_config
                        }),
                    },
                ]);

                // Create and initialize our renderer with a Vulkan surface.
                let mut renderer = render::Renderer::new(vk_context, &surface, &mut imgui).unwrap();

                let mut target = render::Target::new(surface);

                let main_viewport = imgui.main_viewport_mut();
                ViewportBackend::initialize_viewport_data(main_viewport);
                ViewportBackend::data(main_viewport).pos = platform
                    .scale_pos_from_winit(
                        &window,
                        window
                            .inner_position()
                            .unwrap_or_default()
                            .to_logical(window.scale_factor()),
                    )
                    .cast::<f32>()
                    .into();
                let mut viewport_windows = HashMap::<Id, (Arc<Window>, render::Target)>::new();
                let (viewport_backend, viewport_events) = ViewportBackend::new();
                imgui.set_platform_backend(viewport_backend);
                imgui.set_renderer_backend(RendererViewportBackend {});

                imgui.io_mut().backend_flags.insert(
                    BackendFlags::PLATFORM_HAS_VIEWPORTS | BackendFlags::RENDERER_HAS_VIEWPORTS,
                );

                // We're done initializing, start the render loop!
                'main_loop: loop {
                    // Handle viewport events first.
                    for viewport_event in viewport_events.borrow_mut().drain(..) {
                        match viewport_event {
                            ViewportEvent::Create(id) => {
                                let viewport = imgui.viewport_by_id_mut(id).unwrap();
                                let window_builder = WindowBuilder::new()
                                    .with_title(WINDOW_TITLE)
                                    .with_decorations(
                                        !viewport.flags.contains(ViewportFlags::NO_DECORATION),
                                    );

                                let response =
                                    Arc::<(Condvar, Mutex<Option<CreateWindowResponse>>)>::default(
                                    );
                                event_proxy
                                    .send_event(UserEvent::CreateWindow {
                                        request: Box::new(window_builder),
                                        response: response.clone(),
                                    })
                                    .unwrap();
                                let CreateWindowResponse { window, surface } = response
                                    .0
                                    .wait_while(response.1.lock().unwrap(), |w| w.is_none())
                                    .unwrap()
                                    .take()
                                    .unwrap();

                                viewport.dpi_scale = window.scale_factor().round() as f32;
                                viewport_windows.insert(id, (window, render::Target::new(surface)));
                            }
                            ViewportEvent::Destroy(id) => {
                                viewport_windows.remove(&id);
                            }
                            ViewportEvent::Show(id) => viewport_windows[&id].0.set_visible(true),
                            ViewportEvent::SetPos(id, pos) => {
                                let window = &viewport_windows[&id].0;
                                window.set_outer_position(platform.scale_pos_for_winit(
                                    window,
                                    LogicalPosition::<f32>::from(pos).cast(),
                                ));
                                window.request_redraw();
                            }
                            ViewportEvent::SetSize(id, size) => {
                                let window = &viewport_windows[&id].0;
                                window.set_inner_size(
                                    LogicalSize::<f32>::from(size).to_physical::<f32>(
                                        imgui.viewport_by_id(id).unwrap().dpi_scale as f64,
                                    ),
                                );
                            }
                            ViewportEvent::SetFocus(id) => viewport_windows[&id].0.focus_window(),
                            ViewportEvent::SetTitle(id, title) => {
                                viewport_windows[&id].0.set_title(&title)
                            }
                            ViewportEvent::SetAlpha(_, _) => {}
                        }
                    }

                    for event in event_rx.try_iter() {
                        if let Event::WindowEvent {
                            window_id: _,
                            event:
                                WindowEvent::Resized(PhysicalSize {
                                    width: u32::MAX,
                                    height: u32::MAX,
                                }),
                        } = event
                        {
                            // https://github.com/rust-windowing/winit/issues/2876
                            continue;
                        }
                        event_callback(&event, &mut imgui, &window);

                        if !matches!(event, Event::WindowEvent { .. }) {
                            platform.handle_event(imgui.io_mut(), &window, &event);
                        }

                        #[allow(clippy::single_match)]
                        match event {
                            Event::WindowEvent {
                                window_id,
                                event: e,
                            } => {
                                let main_window = &window;
                                let main_window_id = window.id();
                                let (viewport, window, target) = if window_id == main_window_id {
                                    (imgui.main_viewport().id, &*window, &mut target)
                                } else if let Some((viewport, (window, target))) = viewport_windows
                                    .iter_mut()
                                    .find(|(_v, (w, _t))| window_id == w.id())
                                {
                                    (*viewport, &**window, target)
                                } else {
                                    // the event is not for us
                                    return;
                                };

                                let viewport = imgui.viewport_by_id_mut(viewport).unwrap();

                                match e {
                                    WindowEvent::Resized(_new_size) => {
                                        let new_size = window.inner_size();
                                        target.invalidate();
                                        let new_size = platform
                                            .scale_size_from_winit(
                                                window,
                                                new_size.to_logical(window.scale_factor()),
                                            )
                                            .cast::<f32>()
                                            .into();
                                        ViewportBackend::data(viewport).size = new_size;
                                        viewport.platform_request_resize = true;
                                        if window_id == main_window_id {
                                            imgui.io_mut().display_size = new_size;
                                        }
                                    }
                                    WindowEvent::Moved(new_pos) => {
                                        let new_pos = window.inner_position().unwrap_or(new_pos);
                                        ViewportBackend::data(viewport).pos = platform
                                            .scale_pos_from_winit(
                                                window,
                                                new_pos.to_logical(window.scale_factor()),
                                            )
                                            .cast::<f32>()
                                            .into();
                                        viewport.platform_request_move = true;
                                    }
                                    WindowEvent::Focused(focused) => {
                                        ViewportBackend::data(viewport).focused = focused;
                                    }
                                    WindowEvent::CloseRequested => {
                                        viewport.platform_request_close = true;
                                        if window.id() == main_window_id {
                                            break 'main_loop;
                                        }
                                    }
                                    WindowEvent::CursorMoved { position, .. } => {
                                        let window_pos =
                                            window.inner_position().unwrap_or_default();
                                        let physical = PhysicalPosition::new(
                                            position.x + window_pos.x as f64,
                                            position.y + window_pos.y as f64,
                                        );
                                        let logical: LogicalPosition<f64> =
                                            physical.to_logical(viewport.dpi_scale as f64);
                                        imgui.io_mut().add_mouse_pos_event([
                                            logical.x as f32,
                                            logical.y as f32,
                                        ])
                                    }
                                    e => {
                                        // yikes...
                                        platform.handle_event(
                                            imgui.io_mut(),
                                            main_window,
                                            &Event::WindowEvent::<()> {
                                                window_id: main_window.id(),
                                                event: e,
                                            },
                                        );
                                    }
                                }
                            }
                            _ => {}
                        }
                    }

                    // Now that we've cleared all events, render a frame.
                    let ui = imgui.frame();
                    render_callback(ui, &mut renderer);
                    ui.end_frame_early();

                    platform.prepare_render(ui, &window);
                    imgui.update_platform_windows();
                    let mut frame = renderer.begin();

                    frame.render(&mut target, imgui.render()).unwrap();

                    for (viewport, (_w, target)) in &mut viewport_windows {
                        let viewport = imgui.viewport_by_id_mut(*viewport).unwrap();
                        frame.render(target, viewport.draw_data()).unwrap();
                    }

                    frame.wait().unwrap();
                }

                event_proxy.send_event(UserEvent::Exit).unwrap();
            })
            .unwrap();

        // Run the event loop.
        event_loop.run(move |event, window_target, control_flow| {
            control_flow.set_wait();

            match event {
                Event::UserEvent(e) => match e {
                    UserEvent::Exit => control_flow.set_exit_with_code(0),
                    UserEvent::CreateWindow { request, response } => {
                        let window = Arc::new(request.build(window_target).unwrap());
                        let surface = vulkano_win::create_surface_from_handle(
                            window.clone(),
                            vk_instance.clone(),
                        )
                        .unwrap();
                        *response.1.lock().unwrap() =
                            Some(CreateWindowResponse { window, surface });
                        response.0.notify_one();
                    }
                },
                e => {
                    if let Some(event) = e.map_nonuser_event().unwrap().to_static() {
                        _ = event_tx.send(event);
                    }
                }
            }
        });
    })
}

/// This callback will be invoked whenever the Vulkan API generates a debug message.
fn debug(msg: &vulkano::instance::debug::Message<'_>) {
    use vk::instance::debug::DebugUtilsMessageSeverity as Severity;
    use vk::instance::debug::DebugUtilsMessageType as Type;

    let severity = if msg.severity.contains(Severity::ERROR) {
        "ERROR"
    } else if msg.severity.contains(Severity::WARNING) {
        "WARNING"
    } else if msg.severity.contains(Severity::INFO) {
        "INFO"
    } else if msg.severity.contains(Severity::VERBOSE) {
        "VERBOSE"
    } else {
        "DEBUG"
    };

    let ty = if msg.ty.contains(Type::VALIDATION) {
        " [VALIDATION]"
    } else if msg.ty.contains(Type::PERFORMANCE) {
        " [PERF]"
    } else {
        ""
    };

    eprintln!("[{severity}]{ty} {}", msg.description);
}

struct ViewportBackend {
    pending_events: Rc<RefCell<VecDeque<ViewportEvent>>>,
}

#[derive(Clone, Copy)]
struct ViewportData {
    pos: [f32; 2],
    size: [f32; 2],
    focused: bool,
}

#[derive(Debug)]
enum ViewportEvent {
    Create(Id),
    Destroy(Id),
    Show(Id),
    SetPos(Id, [f32; 2]),
    SetSize(Id, [f32; 2]),
    SetFocus(Id),
    SetTitle(Id, String),
    SetAlpha(Id, f32),
}

impl ViewportBackend {
    fn new() -> (Self, Rc<RefCell<VecDeque<ViewportEvent>>>) {
        let result = Self {
            pending_events: Default::default(),
        };
        let queue = result.pending_events.clone();
        (result, queue)
    }

    fn event(&mut self, event: ViewportEvent) {
        self.pending_events.borrow_mut().push_back(event);
    }

    fn initialize_viewport_data(viewport: &mut imgui::Viewport) {
        viewport.platform_user_data = Box::into_raw(Box::new(ViewportData {
            pos: viewport.pos,
            size: viewport.size,
            focused: true,
        }))
        .cast()
    }

    unsafe fn destroy_viewport_data(viewport: &mut imgui::Viewport) {
        std::mem::drop(Box::from_raw(
            viewport.platform_user_data.cast::<ViewportData>(),
        ));
        viewport.platform_user_data = std::ptr::null_mut();
    }

    fn data(viewport: &mut imgui::Viewport) -> &mut ViewportData {
        unsafe {
            viewport
                .platform_user_data
                .cast::<ViewportData>()
                .as_mut()
                .unwrap()
        }
    }
}

impl PlatformViewportBackend for ViewportBackend {
    fn create_window(&mut self, viewport: &mut imgui::Viewport) {
        self.event(ViewportEvent::Create(viewport.id));
        Self::initialize_viewport_data(viewport);
    }

    fn destroy_window(&mut self, viewport: &mut imgui::Viewport) {
        self.event(ViewportEvent::Destroy(viewport.id));
        unsafe { Self::destroy_viewport_data(viewport) }
    }

    fn show_window(&mut self, viewport: &mut imgui::Viewport) {
        self.event(ViewportEvent::Show(viewport.id));
    }

    fn set_window_pos(&mut self, viewport: &mut imgui::Viewport, pos: [f32; 2]) {
        self.event(ViewportEvent::SetPos(viewport.id, pos));
        Self::data(viewport).pos = pos;
    }

    fn get_window_pos(&mut self, viewport: &mut imgui::Viewport) -> [f32; 2] {
        Self::data(viewport).pos
    }

    fn set_window_size(&mut self, viewport: &mut imgui::Viewport, size: [f32; 2]) {
        self.event(ViewportEvent::SetSize(viewport.id, size));
        Self::data(viewport).size = size;
    }

    fn get_window_size(&mut self, viewport: &mut imgui::Viewport) -> [f32; 2] {
        Self::data(viewport).size
    }

    fn set_window_focus(&mut self, viewport: &mut imgui::Viewport) {
        self.event(ViewportEvent::SetFocus(viewport.id));
        Self::data(viewport).focused = true;
    }

    fn get_window_focus(&mut self, viewport: &mut imgui::Viewport) -> bool {
        Self::data(viewport).focused
    }

    fn get_window_minimized(&mut self, _viewport: &mut imgui::Viewport) -> bool {
        // winit 0.27 does not expose this information yet
        false
    }

    fn set_window_title(&mut self, viewport: &mut imgui::Viewport, title: &str) {
        self.event(ViewportEvent::SetTitle(viewport.id, title.to_owned()));
    }

    fn set_window_alpha(&mut self, viewport: &mut imgui::Viewport, alpha: f32) {
        self.event(ViewportEvent::SetAlpha(viewport.id, alpha));
    }

    fn update_window(&mut self, _viewport: &mut imgui::Viewport) {}

    fn render_window(&mut self, _viewport: &mut imgui::Viewport) {}

    fn swap_buffers(&mut self, _viewport: &mut imgui::Viewport) {}

    fn create_vk_surface(
        &mut self,
        _viewport: &mut imgui::Viewport,
        _instance: u64,
        _out_surface: &mut u64,
    ) -> i32 {
        unimplemented!()
    }
}

struct RendererViewportBackend {}
impl imgui::RendererViewportBackend for RendererViewportBackend {
    fn create_window(&mut self, _viewport: &mut imgui::Viewport) {}
    fn destroy_window(&mut self, _viewport: &mut imgui::Viewport) {}
    fn set_window_size(&mut self, _viewport: &mut imgui::Viewport, _size: [f32; 2]) {}
    fn render_window(&mut self, _viewport: &mut imgui::Viewport) {}
    fn swap_buffers(&mut self, _viewport: &mut imgui::Viewport) {}
}
