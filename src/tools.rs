use std::borrow::{Borrow, BorrowMut};
use std::collections::HashMap;
use auxtools::{DMResult, Proc, Value};
use dmasm::Instruction;
use dmasm::format_disassembly;
use dmasm::operands::Variable;
use crate::{call_counts, DisassembleEnv};
use crate::compile::PROC_META;

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
        Variable::SetCache(a, b) => {
            return format!(
                "SetCache({}, {})",
                var_desc(a.borrow()),
                var_desc(b.borrow())
            );
        }
        Variable::Initial(_) => "Initial",
        Variable::IsSaved(_) => "IsSaved",
        Variable::Field(_) => "Field",
        Variable::StaticVerb(_) => "StaticVerb",
        Variable::DynamicVerb(_) => "DynamicVerb",
        Variable::StaticProc(_) => "StaticProc",
        Variable::DynamicProc(_) => "DynamicProc",
    }
    .to_string()
}

#[hook("/proc/dmjit_dump_opcode_count")]
pub fn dump_opcode_count() -> DMResult {
    log::info!("[DOC] Dump opcode counts");
    if let Some(mut vec) = call_counts() {
        vec.sort_by_key(|h| h.count);
        log::info!("[DOC] Total {} procs", vec.len());

        let mut env = DisassembleEnv {};

        let mut map: HashMap<String, u64> = HashMap::new();

        for count in vec {
            unsafe {
                let (nodes, res) =
                    dmasm::disassembler::disassemble(count.proc.bytecode(), &mut env);
                for node in nodes {
                    match node {
                        dmasm::Node::Instruction(insn, _) => {
                            let mut name = insn.op_name();
                            match insn {
                                Instruction::Call(v, _) => {
                                    name = format!("{} {}", name, var_desc(&v))
                                }
                                Instruction::CallStatement(v, _) => {
                                    name = format!("{} {}", name, var_desc(&v))
                                }
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
    DMResult::Ok(Value::null())
}

#[hook("/proc/dmjit_dump_opcodes")]
pub fn dump_opcodes(list: Value) -> DMResult {
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
                log::info!("{:?}", proc.parameter_names());
                override_id += 1;
            } else {
                if override_id == 0 {
                    log::error!("Function not found {}", name);
                }
                break;
            }
        }
    } else {
        log::error!("Not a str {}, {}", list, list.to_string()?)
    }
    DMResult::Ok(Value::null())
}

#[hook("/proc/dmjit_dump_deopts")]
fn dump_deopts() -> DMResult {
    for meta in unsafe { &PROC_META } {
        let proc_id = meta.proc_id;
        for point in meta.deopt_point_map.as_ref() {
            match point {
                Option::Some(point) if point.hits > 0 =>
                    log::info!("{:?} {}@{:X} -- {}", proc_id, Proc::from_id(proc_id).map_or("_unk".to_string(), |proc| proc.path), point.origin.bytecode_offset, point.hits),
                _ => {}
            }
        }
    }
    DMResult::Ok(Value::null())
}