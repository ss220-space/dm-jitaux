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
    let res = fs::read_to_string(path);
    assert!(res.is_ok());
    res.unwrap().replace("\r\n","\n")
}

fn copy_hook_lib() -> io::Result<()> {
    std::fs::copy(Path::new("target/i686-pc-windows-msvc/debug/hook_test.dll"), Path::new(TEST_WORK_DIR).join("hook_test.dll"))?;
    io::Result::Ok(())
}

fn compile_files(files: Vec<&str>) -> io::Result<ExitStatus> {
    let test_dme_path = Path::new(TEST_WORK_DIR).join("test.dme");
    std::fs::create_dir_all(test_dme_path.parent().unwrap())?;
    let mut dme = File::create(test_dme_path.clone())?;

    for file_name in files {
        let src = Path::new(TEST_DATA_DIR).join(file_name);
        let target = Path::new(TEST_WORK_DIR).join(file_name);
        std::fs::create_dir_all(target.parent().unwrap())?;
        std::fs::copy(src, target)?;
        write!(dme, "#include \"{}\"\n", file_name)?;
    }


    Command::new("C:\\Program Files (x86)\\BYOND\\bin\\dm.exe")
        .args(&[test_dme_path.to_str().unwrap()])
        .spawn()
        .unwrap()
        .wait()
}

fn run_dm() -> io::Result<ExitStatus> {
    Command::new("C:\\Program Files (x86)\\BYOND\\bin\\dreamdaemon.exe")
        .args(&[Path::new(TEST_WORK_DIR).join("test.dmb").to_str().unwrap(), "-trusted"])
        .spawn()?
        .wait()
}
