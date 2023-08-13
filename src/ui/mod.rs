use anyhow::Result;
use winit::event::{Event, WindowEvent};

mod backend;

#[allow(clippy::collapsible_match, clippy::single_match)]
pub fn run() -> Result<()> {
    backend::run(
        |event, _imgui, window, control_flow| match event {
            Event::WindowEvent { window_id, event } if *window_id == window.id() => match event {
                WindowEvent::CloseRequested => control_flow.set_exit(),
                _ => {}
            },
            _ => {}
        },
        |ui| ui.show_demo_window(&mut true),
    )
}
