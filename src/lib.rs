#![feature(core_intrinsics)]
#![feature(once_cell)]
#![feature(asm)]

mod compile;
mod pads;
mod dmir;
mod codegen;
mod ref_count;


#[macro_use]
extern crate auxtools;
extern crate log;

use auxtools::{hook, CompileTimeHook, StringRef, raw_types, DMResult, Runtime};
use auxtools::Value;
use auxtools::Proc;
use auxtools::inventory;
use auxtools::hooks::call_counts;


use log::LevelFilter;
use std::collections::HashMap;
use std::borrow::{BorrowMut, Borrow};
use std::panic::{UnwindSafe, catch_unwind};
use dmasm::{format_disassembly, Instruction};
use std::process::exit;
use dmasm::operands::Variable;


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

#[hook("/proc/dump_call_count")]
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

pub fn var_desc(v: &Variable) -> String {
    match v {
        Variable::Null => "Null",
        Variable::World => "World",
        Variable::Usr => "Usr",
        Variable::Src => "Src",
        Variable::Args => "Args",
        Variable::Dot => "Dot",
        Variable::Cache => "Cache",
        Variable::CacheKey => "CacheKey",
        Variable::CacheIndex => "CacheIndex",
        Variable::Arg(_) => "Arg",
        Variable::Local(_) => "Local",
        Variable::Global(_) => "Global",
        Variable::SetCache(a, b) => return format!("SetCache({}, {})", var_desc(a.borrow()), var_desc(b.borrow())),
        Variable::Initial(_) => "Initial",
        Variable::IsSaved(_) => "IsSaved",
        Variable::Field(_) => "Field",
        Variable::StaticVerb(_) => "StaticVerb",
        Variable::DynamicVerb(_) => "DynamicVerb",
        Variable::StaticProc(_) => "StaticProc",
        Variable::DynamicProc(_) => "DynamicProc",
    }.to_string()
}

#[hook("/proc/dump_opcode_count")]
pub fn dump_opcode_count() {
    log::info!("[DOC] Dump opcode counts");
    if let Some(mut vec) = call_counts() {
        vec.sort_by_key(|h| h.count);
        log::info!("[DOC] Total {} procs", vec.len());

        let mut env = DisassembleEnv {};

        let mut map: HashMap<String, u64> = HashMap::new();

        for count in vec {
            unsafe {
                let (nodes, res) = dmasm::disassembler::disassemble(count.proc.bytecode(), &mut env);
                for node in nodes {
                    match node {
                        dmasm::Node::Instruction(insn, _) => {

                            let mut name = insn.op_name();
                            match insn {
                                Instruction::Call(v, _) => {
                                    name = format!("{} {}", name, var_desc(&v))
                                },
                                Instruction::CallStatement(v, _) => {
                                    name = format!("{} {}", name, var_desc(&v))
                                },
                                Instruction::GetVar(v) => {
                                    name = format!("{} {}", name, var_desc(&v))
                                }
                                Instruction::SetVar(v) => {
                                    name = format!("{} {}", name, var_desc(&v))
                                }
                                _ => {}
                            }

                            *map.entry(name).or_default().borrow_mut() += u64::from(count.count)
                        }
                        _ => {}
                    }
                }
            }
        }

        let mut res = map.iter().collect::<Vec<_>>();
        res.sort_by_key(|e| -(*e.1 as i64));
        for (op, count) in res {
            log::info!("[DOC] {}\t{}", count, op);
        }
    }
    Ok(Value::null())
}

#[hook("/proc/hook_log_init")]
pub fn log_init() {
    simple_logging::log_to_file("hook.log", LevelFilter::Debug).unwrap();
    log_panics::init();
    log::info!("Log startup");

    for hook in inventory::iter::<CompileTimeHook> {
        log::info!("Hooked {}", hook.proc_path)
    }

    pads::deopt::initialize_deopt();

    Value::from_string("dm-jitaux init success")
}

#[hook("/proc/dump_opcodes")]
pub fn dump_opcodes(list: Value) {
    if let Ok(name) = list.as_list()?.get(Value::from(1))?.as_string() {

        let mut override_id = 0;
        loop {
            if let Some(proc) = Proc::find_override(name.clone(), override_id) {
                let mut env = DisassembleEnv {};

                let bytecode = unsafe { proc.bytecode() };

                let (nodes, _error) = dmasm::disassembler::disassemble(bytecode, &mut env);

                log::info!("override_id: {}, proc.path: {}", override_id, proc.path);
                log::info!("{:?}", unsafe { &*proc.entry });
                log::info!("{}", format_disassembly(&nodes, None));
                override_id += 1;
            } else {
                if override_id == 0 {
                    log::error!("Function not found {}", name);
                }
                break
            }
        }
    } else {
        log::error!("Not a str {}, {}", list, list.to_string()?)
    }
    Ok(Value::null())
}


#[hook("/proc/exit_test")]
pub fn exit_test() {
    exit(0);
}

#[hook("/proc/toggle_dm_jitaux_hooks")]
pub fn toggle_dm_jitaux_hooks() {
    unsafe {
        auxtools::hooks::ENABLE_CHAD_HOOKS = !auxtools::hooks::ENABLE_CHAD_HOOKS;
        Ok(Value::from(auxtools::hooks::ENABLE_CHAD_HOOKS))
    }

}

#[hook("/proc/toggle_dm_jitaux_call_counts")]
pub fn toggle_dm_jitaux_call_counts() {
    unsafe {
        auxtools::hooks::ENABLE_CALL_COUNTS = !auxtools::hooks::ENABLE_CALL_COUNTS;
        Ok(Value::from(auxtools::hooks::ENABLE_CALL_COUNTS))
    }
}