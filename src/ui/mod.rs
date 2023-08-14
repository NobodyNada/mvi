use anyhow::Result;

mod backend;

#[allow(clippy::collapsible_match, clippy::single_match)]
pub fn run() -> Result<()> {
    backend::run(
        |event, _imgui, _window| match event {
            _ => {}
        },
        |ui| ui.show_demo_window(&mut true),
    )
}
