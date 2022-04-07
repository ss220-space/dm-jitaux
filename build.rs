use std::process::{Command};
use anyhow::Result;
use vergen::{Config, TimeZone, vergen};


fn llvm_config(param: &str) -> String {
    let config_path = std::env::var("DEP_LLVM_12_CONFIG_PATH").unwrap();
    let output = Command::new(config_path).arg(param).output().unwrap();

    return String::from_utf8(output.stdout).unwrap()
}

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

    cc::Build::new()
        .include(llvm_config("--includedir").trim_end())
        .include("src/")
        .file("src/sectionMemoryManagerBindings.cpp")
        .cpp(true)
        .compile("dmjit-cpp");

    let mut child = Command::new("llvm-as")
        .arg("src/runtime.ll")
        .arg("-o")
        .arg("target/runtime.bc")
        .spawn().unwrap();

    assert_eq!(child.wait().unwrap().success(), true);


    vergen(config)
}