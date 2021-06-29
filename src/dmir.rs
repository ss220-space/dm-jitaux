use auxtools::raw_types::procs::ProcId;
use auxtools::raw_types::values::ValueTag;
use dmasm::{Instruction, Node, DebugData};
use dmasm::operands::{Variable, ValueOpRaw};
use std::ffi::CString;
use std::borrow::Borrow;
use auxtools::Proc;
use auxtools::raw_types::strings::StringId;

#[derive(Debug)]
pub enum DMIR {
    GetLocal(u32),
    SetLocal(u32),
    GetSrc,
    GetArg(u32),
    SetCache,
    GetCacheField(u32),
    PushCache,
    FloatAdd,
    FloatMul,
    FloatTg,
    FloatAbs,
    PushInt(i32),
    PushVal(dmasm::operands::ValueOpRaw),
    Pop,
    Ret,
    Test,
    JZ(String),
    EnterBlock(String),
    Deopt(u32, ProcId),
    CheckTypeDeopt(u32, ValueTag, Box<DMIR>),
    CallProcById(ProcId, u8, u32),
    CallProcByName(StringId, u8, u32),
    End
}



fn get_string_id(str: &Vec<u8>) -> StringId {
    let mut id = auxtools::raw_types::strings::StringId(0);
    unsafe {
        // obtain id of string from BYOND
        auxtools::raw_types::funcs::get_string_id(&mut id, CString::from_vec_unchecked(str.clone()).as_ptr());
    }

    return id;
}

// GetVar cache = src; cache["oxygen"]
// ->
// GetVar src
// SetCache
// GetCacheField cache["oxygen"]

// decode Variable nesing in GetVar
fn decode_get_var(vr: &Variable, out: &mut Vec<DMIR>) {
    match vr {
        Variable::Src => out.push(DMIR::GetSrc),

        // Decode complex SetCache chains
        //  GetVar cache = src; cache["oxygen"]
        //  ->
        //  GetVar src
        //  SetCache
        //  GetCacheField cache["oxygen"]
        Variable::SetCache(first, second) => {
            decode_get_var(first.borrow(), out);
            out.push(DMIR::SetCache);
            decode_get_var(second.borrow(), out);
        }
        // Just cache["STR"]
        Variable::Field(str) => {
            // gen DMIR
            out.push(DMIR::GetCacheField(get_string_id(&(str.0)).0));
        }
        Variable::Local(idx) => out.push(DMIR::GetLocal(idx.clone())),
        Variable::Arg(idx) => out.push(DMIR::GetArg(idx.clone())),
        _ => panic!("Not supported")
    }
}

// decode Variable nesing in SetVar
fn decode_set_var(vr: &Variable, out: &mut Vec<DMIR>) {
    match vr {
        Variable::Local(idx) => out.push(DMIR::SetLocal(idx.clone())),
        _ => panic!("Not supported")
    }
}


fn decode_call(vr: &Variable, arg_count: u32, out: &mut Vec<DMIR>) {
    match vr {
        Variable::SetCache(a, b) => {
            decode_call(a.borrow(), arg_count, out);
            out.push(DMIR::SetCache);
            decode_call(b.borrow(), arg_count, out);
        }
        Variable::StaticVerb(_) => panic!("Unsupported: {:?}", vr.clone()),
        Variable::DynamicVerb(_) => panic!("Unsupported: {:?}", vr.clone()),
        Variable::StaticProc(proc_id) => {
            let name_id = unsafe { &*Proc::find(&proc_id.0).unwrap().entry }.name;
            out.push(DMIR::PushCache);
            out.push(DMIR::CallProcByName(name_id, 2, arg_count))
        }
        Variable::DynamicProc(name) => {
            out.push(DMIR::PushCache);
            out.push(DMIR::CallProcByName(get_string_id(&(name.0)), 2, arg_count))
        }
        _ => decode_get_var(vr, out)
    }
}

pub fn decode_byond_bytecode(nodes: Vec<Node<DebugData>>, proc: Proc) -> Result<Vec<DMIR>, ()> {

    // output for intermediate operations sequence
    let mut irs = vec![];

    // will set to false if some unsupported operation found
    let mut supported = true;

    // generate DMIR sequence for each instruction in dm-asm
    for nd in nodes {
        match nd {
            // if node contains instruction
            dmasm::Node::Instruction(insn, data) => {
                match insn {
                    // skip debug info for now
                    Instruction::DbgFile(_f) => {}
                    Instruction::DbgLine(_ln) => {},
                    Instruction::GetVar(vr) => {
                        decode_get_var(&vr, &mut irs)
                    }
                    Instruction::SetVar(vr) => {
                        decode_set_var(&vr, &mut irs)
                    },
                    Instruction::Add => {
                        irs.push(DMIR::CheckTypeDeopt(0, ValueTag::Number, Box::new(DMIR::Deopt(data.offset, proc.id))));
                        irs.push(DMIR::CheckTypeDeopt(1, ValueTag::Number, Box::new(DMIR::Deopt(data.offset, proc.id))));
                        irs.push(DMIR::FloatAdd)
                    }
                    Instruction::Mul => {
                        irs.push(DMIR::FloatMul)
                    }
                    Instruction::Tg => {
                        irs.push(DMIR::FloatTg)
                    }
                    Instruction::Abs => {
                        irs.push(DMIR::FloatAbs)
                    }
                    Instruction::CallGlob(arg_count, callee) => {
                        if callee.0 == "/dm_jitaux_deopt" {
                            irs.push(DMIR::Deopt(data.offset, proc.id));
                            irs.push(DMIR::EnterBlock(format!("post_deopt_{}", data.offset)));
                        } else {
                            irs.push(DMIR::PushVal(ValueOpRaw { tag: ValueTag::Null as u8, data: 0 }));
                            let id = Proc::find(callee.0).unwrap().id;
                            irs.push(DMIR::CallProcById(id, 2, arg_count))
                        }
                    }
                    Instruction::Call(var, arg_count) => {
                        decode_call(&var, arg_count, &mut irs);
                    }
                    Instruction::CallStatement(var, arg_count) => {
                        decode_call(&var, arg_count, &mut irs);
                        irs.push(DMIR::Pop)
                    }
                    Instruction::Ret => {
                        irs.push(DMIR::Ret)
                    }
                    Instruction::End => {
                        irs.push(DMIR::End)
                    }
                    Instruction::Test => {
                        irs.push(DMIR::Test)
                    }
                    Instruction::Jz(lbl) => {
                        irs.push(DMIR::JZ(lbl.0))
                    }
                    Instruction::PushInt(i32) => {
                        irs.push(DMIR::PushInt(i32))
                    }
                    Instruction::PushVal(op) => {
                        irs.push(DMIR::PushVal(op.raw.unwrap()))
                    }
                    Instruction::Pop => {
                        irs.push(DMIR::Pop)
                    }
                    _ => {
                        log::info!("Unsupported insn {}", insn);
                        supported = false;
                    }
                }
            }
            dmasm::Node::Label(lbl) => {
                //log::info!("{}:", lbl)
                irs.push(DMIR::EnterBlock(lbl))
            }
            _ => {}
        }
    }

    if !supported {
        return Err(());
    }


    return Ok(irs);
}