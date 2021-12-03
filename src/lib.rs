#![feature(core_intrinsics)]
#![feature(once_cell)]
#![feature(asm)]

mod compile;
mod pads;
mod dmir;
mod codegen;
mod ref_count;
mod variable_termination_pass;
mod dmir_annotate;

#[cfg(feature = "test_time")]
mod time;

#[cfg(feature = "bench_utils")]
mod bench_utils;

#[cfg(feature = "tools")]
mod tools;

#[cfg(feature = "test_utils")]
mod test_utils;


#[macro_use]
extern crate auxtools;
extern crate log;

use auxtools::{hook, CompileTimeHook, StringRef, raw_types, DMResult, Runtime};
use auxtools::Value;
use auxtools::Proc;
use auxtools::inventory;
use auxtools::hooks::call_counts;


use log::LevelFilter;
use std::panic::{UnwindSafe, catch_unwind};


pub struct DisassembleEnv;

impl dmasm::disassembler::DisassembleEnv for DisassembleEnv {
    fn get_string_data(&mut self, index: u32) -> Option<Vec<u8>> {
        unsafe {
            Some(
                StringRef::from_id(raw_types::strings::StringId(index))
                    .data()
                    .to_vec(),
            )
        }
    }

    fn get_variable_name(&mut self, index: u32) -> Option<Vec<u8>> {
        unsafe {
            Some(
                StringRef::from_variable_id(raw_types::strings::VariableId(index))
                    .data()
                    .to_vec(),
            )
        }
    }

    fn get_proc_name(&mut self, index: u32) -> Option<String> {
        Proc::from_id(raw_types::procs::ProcId(index)).map(|x| x.path)
    }

    fn value_to_string_data(&mut self, tag: u32, data: u32) -> Option<Vec<u8>> {
        unsafe {
            let value = Value::new(std::mem::transmute(tag as u8), std::mem::transmute(data));
            match value.to_dmstring() {
                Ok(s) => Some(s.data().to_vec()),
                _ => None,
            }
        }
    }
}


pub enum BoxResult<T, E> {
    /// Contains the success value
    Ok(T),

    /// Contains the error value
    Err(E),
}

pub fn guard<F: FnOnce() -> DMResult + UnwindSafe>(f: F) -> DMResult {
    let res = catch_unwind(move ||
        match f() {
            Ok(value) => BoxResult::Ok(value),
            Err(err) => BoxResult::Err(err)
        }
    );
    match res {
        Ok(BoxResult::Ok(value)) => Ok(value),
        Ok(BoxResult::Err(err)) => {
            log::error!("Hook error: {}", err.message);
            Err(err)
        },
        Err(err) => {
            log::error!("Hook panic: {:?}", err);
            Result::Err(Runtime::new(format!("Panic over boundary {:?}", err)))
        }
    }
}

#[hook("/proc/dmjit_dump_call_count")]
pub fn dump_call_count() {
    log::info!("Dump call count");
    if let Some(mut vec) = call_counts() {
        vec.sort_by_key(|h| -(h.count as i32));
        log::info!("Total {} procs", vec.len());
        for count in vec {
            log::info!("{}\t{}", count.count, count.proc.path);
        }
    }
    Ok(Value::null())
}

#[hook("/proc/dmjit_hook_log_init")]
pub fn log_init() {
    macro_rules! ver_string {
        () => {
            format!("{}-{} built on {}", env!("VERGEN_GIT_SEMVER"), env!("VERGEN_CARGO_PROFILE"), env!("VERGEN_BUILD_TIMESTAMP"))
        };
    }



    simple_logging::log_to_file("dmjit.log", LevelFilter::Debug).unwrap();
    log_panics::init();
    log::info!("Log startup, {}", ver_string!());

    for hook in inventory::iter::<CompileTimeHook> {
        log::info!("Hooked {}", hook.proc_path)
    }

    pads::deopt::initialize_deopt();
    pads::debug::init();

    Value::from_string(format!("dmJIT init success, {}", ver_string!()))
}




#[hook("/proc/dmjit_toggle_hooks")]
pub fn toggle_hooks() {
    unsafe {
        auxtools::hooks::ENABLE_CHAD_HOOKS = !auxtools::hooks::ENABLE_CHAD_HOOKS;
        return Ok(Value::from(auxtools::hooks::ENABLE_CHAD_HOOKS))
    }

}

#[hook("/proc/dmjit_toggle_call_counts")]
pub fn toggle_call_counts() {
    unsafe {
        auxtools::hooks::ENABLE_CALL_COUNTS = !auxtools::hooks::ENABLE_CALL_COUNTS;
        return Ok(Value::from(auxtools::hooks::ENABLE_CALL_COUNTS))
    }
}