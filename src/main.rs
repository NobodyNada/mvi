use anyhow::Result;

mod core;
mod ui;

fn main() -> Result<()> {
    unsafe {
        //let core = core::Core::load("cores/bsnes2014_accuracy_libretro.dylib")?;
    }
    ui::run()
}
