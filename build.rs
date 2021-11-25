use anyhow::Result;
use vergen::{Config, TimeZone, vergen};

fn main() -> Result<()> {
    // Generate the default 'cargo:' instruction output
    let mut config = Config::default();
    *config.build_mut().timezone_mut() = TimeZone::Local;
    println!("cargo:rerun-if-changed=src");

    println!("cargo:rustc-cfg=debug_deopt_print");
    //println!("cargo:rustc-cfg=debug_on_call_print");


    vergen(config)
}