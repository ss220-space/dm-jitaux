use std::process::{Command};
use anyhow::Result;
use vergen::{Config, TimeZone, vergen};

fn main() -> Result<()> {
    // Generate the default 'cargo:' instruction output
    let mut config = Config::default();
    *config.build_mut().timezone_mut() = TimeZone::Local;
    println!("cargo:rerun-if-changed=src");

    if std::env::var("OUT_DIR").unwrap().contains("release-dist") {
        println!("cargo:rustc-cfg=rotate_logs");
        println!("cargo:rustc-env=DMJIT_LOG_PREFIX=data/logs/");
    } else {
        println!("cargo:rustc-cfg=debug_deopt_print");
        println!("cargo:rustc-env=DMJIT_LOG_PREFIX=");
    }
    //println!("cargo:rustc-cfg=debug_on_call_print");

    let mut child = Command::new("llvm-as")
        .arg("src/runtime.ll")
        .arg("-o")
        .arg("target/runtime.bc")
        .spawn().unwrap();

    assert_eq!(child.wait().unwrap().success(), true);


    vergen(config)
}