use std::collections::HashMap;
use auxtools::hooks::call_counts;
use auxtools::Value;
use dmasm::Instruction;
use dmasm::operands::Variable;
use std::borrow::{BorrowMut, Borrow};
use crate::DisassembleEnv;

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

#[hook("/proc/dmjit_dump_opcode_count")]
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