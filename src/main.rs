use anyhow::Result;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

mod core;
mod tas;
mod ui;

fn main() -> Result<()> {
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer())
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .init();
    std::panic::set_hook(Box::new(|panic_info| {
        eprintln!("{panic_info}");
        eprintln!("{}", std::backtrace::Backtrace::capture());
        std::process::exit(1)
    }));
    ui::run()
}
