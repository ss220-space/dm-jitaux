use std::process::{Command, ExitStatus};
use std::fs::File;
use std::io::Write;
use std::{io, fs};
use std::path::Path;


pub fn run_hook_test(files: Vec<&str>) {
    fs::remove_file(Path::new(TEST_WORK_DIR).join("result.txt")).ok();
    let mut all_files = vec!("common.dm", "hook.dm");
    all_files.extend(files);
    assert!(compile_files(all_files).unwrap().success());
    assert!(copy_hook_lib().is_ok());
    assert!(run_dm().unwrap().success());
}

const TEST_WORK_DIR: &str = "tests/tmp/";
const TEST_DATA_DIR: &str = "tests/testData/";

pub fn test_result() -> String {
    let path = Path::new(TEST_WORK_DIR).join("result.txt");
    let res = fs::read_to_string(path).unwrap();
    res.replace("\r\n","\n")
}

fn lib_path() -> &'static Path {
    if cfg!(target_os = "windows") {
        Path::new("target/i686-pc-windows-msvc/debug/dm_jitaux.dll")
    } else {
        Path::new("target/i686-unknown-linux-gnu/debug/libdm_jitaux.so")
    }
}

fn copy_hook_lib() -> io::Result<()> {
    let lib_path = lib_path();
    std::fs::copy(lib_path.clone(), Path::new(TEST_WORK_DIR).join(lib_path.file_name().unwrap().to_str().unwrap()))?;
    io::Result::Ok(())
}

fn cmd_dm() -> Command {
    if cfg!(target_os = "windows") {
        Command::new(format!("{}\\bin\\dm.exe", byond_path()))
    } else {
        let mut cmd = Command::new(format!("{}/bin/byondexec", byond_path()));
        cmd.arg(format!("{}/bin/DreamMaker", byond_path()));
        cmd
    }
}

fn compile_files(files: Vec<&str>) -> io::Result<ExitStatus> {
    let test_dme_path = Path::new(TEST_WORK_DIR).join("test.dme");
    std::fs::create_dir_all(test_dme_path.parent().unwrap())?;
    let mut dme = File::create(test_dme_path.clone())?;

    writeln!(dme, "#define DMJIT_LIB \"{}\"", lib_path().file_name().unwrap().to_str().unwrap())?;

    for file_name in files {
        let src = Path::new(TEST_DATA_DIR).join(file_name);
        let target = Path::new(TEST_WORK_DIR).join(file_name);
        std::fs::create_dir_all(target.parent().unwrap())?;
        std::fs::copy(src, target)?;
        writeln!(dme, "#include \"{}\"", file_name)?;
    }


    cmd_dm()
        .arg(test_dme_path.to_str().unwrap())
        .spawn()
        .unwrap()
        .wait()
}

fn byond_path() -> String {
    let byond_path = std::env::var("BYOND_PATH");
    if cfg!(target_os = "windows") {
        byond_path.unwrap_or("C:\\Program Files (x86)\\BYOND".to_string())
    } else {
        byond_path.unwrap()
    }
}

fn cmd_dreamdaemon() -> Command {
    if cfg!(target_os = "windows") {
        Command::new(format!("{}\\bin\\dreamdaemon.exe", byond_path()))
    } else {
        let mut cmd = Command::new(format!("{}/bin/byondexec", byond_path()));
        cmd.arg(format!("{}/bin/DreamDaemon", byond_path()));
        cmd
    }
}

fn run_dm() -> io::Result<ExitStatus> {
    cmd_dreamdaemon()
        .arg(Path::new(TEST_WORK_DIR).join("test.dmb").to_str().unwrap())
        .arg("-trusted")
        .spawn()?
        .wait()
}
