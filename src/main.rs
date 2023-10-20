use anyhow::Result;

mod core;
mod tas;
mod ui;

fn main() -> Result<()> {
    ui::run()
}
