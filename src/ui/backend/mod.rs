use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    rc::Rc,
    sync::Arc,
};

use anyhow::Result;
use imgui::{BackendFlags, Context, Id, PlatformViewportBackend, Ui, ViewportFlags};
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use vulkano as vk;
use winit::{
    dpi::{LogicalPosition, LogicalSize, PhysicalPosition, PhysicalSize},
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::{Window, WindowBuilder},
};

mod render;

const WINDOW_TITLE: &str = "mvi";

pub fn run(
    mut event_callback: impl FnMut(&Event<()>, &mut Context, &Window, &mut ControlFlow) + 'static,
    mut render_callback: impl FnMut(&mut Ui) + 'static,
) -> Result<()> {
    // Create the application event loop
    let event_loop = EventLoop::new();

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

    // Create a window with a Vulkan surface
    let window = WindowBuilder::new()
        .with_title(WINDOW_TITLE)
        .build(&event_loop)?;
    let window = Arc::new(window);
    let surface = vulkano_win::create_surface_from_handle(window.clone(), vk_instance.clone())?;

    // Initialize and configure imgui.
    let mut imgui = Context::create();
    imgui
        .io_mut()
        .config_flags
        .insert(imgui::ConfigFlags::DOCKING_ENABLE | imgui::ConfigFlags::VIEWPORTS_ENABLE);

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

    // Create and initialize our rendering loop.
    let mut renderer = render::Renderer::new(vk_context, &surface, &mut imgui)?;

    let mut target = render::Target::new(surface);

    ViewportBackend::initialize_viewport_data(imgui.main_viewport_mut());
    ViewportBackend::data(imgui.main_viewport_mut()).pos = platform
        .scale_pos_from_winit(
            &window,
            window
                .outer_position()
                .unwrap_or_default()
                .to_logical(window.scale_factor()),
        )
        .cast::<f32>()
        .into();
    let mut viewport_windows = HashMap::<Id, (Arc<Window>, render::Target)>::new();
    let (viewport_backend, viewport_events) = ViewportBackend::new();
    imgui.set_platform_backend(viewport_backend);
    imgui.set_renderer_backend(RendererViewportBackend {});

    imgui
        .io_mut()
        .backend_flags
        .insert(BackendFlags::PLATFORM_HAS_VIEWPORTS | BackendFlags::RENDERER_HAS_VIEWPORTS);

    event_loop.run(move |event, window_target, control_flow| {
        dbg!(&event);
        control_flow.set_poll();

        // Handle viewport events first.
        for viewport_event in viewport_events.borrow_mut().drain(..) {
            match viewport_event {
                ViewportEvent::Create(id) => {
                    let viewport = imgui.viewport_by_id_mut(id).unwrap();
                    let window = WindowBuilder::new()
                        .with_title(WINDOW_TITLE)
                        .with_decorations(!viewport.flags.contains(ViewportFlags::NO_DECORATION))
                        .build(window_target)
                        .unwrap();
                    viewport.dpi_scale = window.scale_factor().round() as f32;

                    let window = Arc::new(window);
                    let surface = vulkano_win::create_surface_from_handle(
                        window.clone(),
                        vk_instance.clone(),
                    )
                    .unwrap();
                    viewport_windows.insert(id, (window, render::Target::new(surface)));
                }
                ViewportEvent::Destroy(id) => {
                    viewport_windows.remove(&id);
                }
                ViewportEvent::Show(id) => viewport_windows[&id].0.set_visible(true),
                ViewportEvent::SetPos(id, pos) => {
                    let window = &viewport_windows[&id].0;
                    dbg!("request move");
                    window.set_outer_position(
                        platform
                            .scale_pos_for_winit(window, LogicalPosition::<f32>::from(pos).cast())
                            .to_physical::<f64>(window.scale_factor()),
                    );
                    window.request_redraw();
                }
                ViewportEvent::SetSize(id, size) => {
                    let window = &viewport_windows[&id].0;
                    window.set_inner_size(
                        LogicalSize::<f32>::from(size)
                            .to_physical::<f32>(imgui.viewport_by_id(id).unwrap().dpi_scale as f64)
                            .to_logical::<f32>(window.scale_factor()),
                    );
                }
                ViewportEvent::SetFocus(id) => viewport_windows[&id].0.focus_window(),
                ViewportEvent::SetTitle(id, title) => viewport_windows[&id].0.set_title(&title),
                ViewportEvent::SetAlpha(_, _) => {}
            }
        }

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
            return;
        }

        if !matches!(event, Event::WindowEvent { .. }) {
            platform.handle_event(imgui.io_mut(), &window, &event);
        }
        event_callback(&event, &mut imgui, &window, control_flow);

        match event {
            Event::WindowEvent {
                window_id,
                event: e,
            } => {
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
                    WindowEvent::Resized(new_size) => {
                        target.invalidate();
                        dbg!("winit resize");
                        ViewportBackend::data(viewport).size = platform
                            .scale_size_from_winit(
                                window,
                                new_size.to_logical(window.scale_factor()),
                            )
                            .cast::<f32>()
                            .into();
                        viewport.platform_request_resize = true;
                    }
                    WindowEvent::Moved(new_pos) => {
                        dbg!("winit move");
                        ViewportBackend::data(viewport).pos = platform
                            .scale_pos_from_winit(window, new_pos.to_logical(window.scale_factor()))
                            .cast::<f32>()
                            .into();
                        viewport.platform_request_move = true;
                    }
                    WindowEvent::Focused(focused) => {
                        ViewportBackend::data(viewport).focused = focused;
                    }
                    WindowEvent::CloseRequested => {
                        viewport.platform_request_close = true;
                    }
                    WindowEvent::CursorMoved { position, .. } => {
                        let window_pos = window.outer_position().unwrap_or_default();
                        let physical = PhysicalPosition::new(
                            position.x + window_pos.x as f64,
                            position.y + window_pos.y as f64,
                        );
                        let logical: LogicalPosition<f64> =
                            physical.to_logical(viewport.dpi_scale as f64);
                        imgui
                            .io_mut()
                            .add_mouse_pos_event([logical.x as f32, logical.y as f32])
                    }
                    _ => {}
                }

                if !matches!(e, WindowEvent::CursorMoved { .. }) {
                    // yikes...
                    platform.handle_event(
                        imgui.io_mut(),
                        window,
                        &Event::WindowEvent::<()> {
                            window_id: window.id(),
                            event: e,
                        },
                    );
                }
            }
            winit::event::Event::MainEventsCleared => {
                window.request_redraw();
            }
            Event::RedrawRequested(_) => {
                let ui = imgui.frame();
                ui.dockspace_over_main_viewport();
                render_callback(ui);
                ui.end_frame_early();

                platform.prepare_render(ui, &window);
                imgui.update_platform_windows();

                renderer.render(&mut target, imgui.render()).unwrap();

                for (viewport, (_w, target)) in &mut viewport_windows {
                    let viewport = imgui.viewport_by_id_mut(*viewport).unwrap();
                    renderer.render(target, viewport.draw_data()).unwrap();
                }
            }
            _ => {}
        }
    });
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
        dbg!("create {}", viewport.id);
        self.event(ViewportEvent::Create(viewport.id));
        Self::initialize_viewport_data(viewport);
    }

    fn destroy_window(&mut self, viewport: &mut imgui::Viewport) {
        dbg!("destroy {}", viewport.id);
        self.event(ViewportEvent::Destroy(viewport.id));
        unsafe { Self::destroy_viewport_data(viewport) }
    }

    fn show_window(&mut self, viewport: &mut imgui::Viewport) {
        self.event(ViewportEvent::Show(viewport.id));
    }

    fn set_window_pos(&mut self, viewport: &mut imgui::Viewport, pos: [f32; 2]) {
        dbg!("move {}", viewport.id);
        self.event(ViewportEvent::SetPos(viewport.id, pos));
        Self::data(viewport).pos = pos;
    }

    fn get_window_pos(&mut self, viewport: &mut imgui::Viewport) -> [f32; 2] {
        Self::data(viewport).pos
    }

    fn set_window_size(&mut self, viewport: &mut imgui::Viewport, size: [f32; 2]) {
        dbg!("resize {}", viewport.id);
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
    fn create_window(&mut self, viewport: &mut imgui::Viewport) {}
    fn destroy_window(&mut self, viewport: &mut imgui::Viewport) {}
    fn set_window_size(&mut self, viewport: &mut imgui::Viewport, size: [f32; 2]) {}
    fn render_window(&mut self, viewport: &mut imgui::Viewport) {}
    fn swap_buffers(&mut self, viewport: &mut imgui::Viewport) {}
}
