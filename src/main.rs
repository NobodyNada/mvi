use anyhow::Result;

mod core;
mod ui;

fn main() -> Result<()> {
    ui::run()
}
