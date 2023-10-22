use anyhow::Result;

mod core;
mod tas;
mod ui;

fn main() -> Result<()> {
    std::panic::set_hook(Box::new(|panic_info| {
        eprintln!("{panic_info}");
        eprintln!("{}", std::backtrace::Backtrace::capture());
        std::process::exit(1)
    }));
    ui::run()
}
