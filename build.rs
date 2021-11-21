use anyhow::Result;
use vergen::{Config, TimeZone, vergen};

fn main() -> Result<()> {
    // Generate the default 'cargo:' instruction output
    let mut config = Config::default();
    *config.build_mut().timezone_mut() = TimeZone::Local;
    println!("cargo:rerun-if-changed=src");
    vergen(config)
}