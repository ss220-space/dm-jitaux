use auxtools::raw_types::procs::ProcId;
use auxtools::raw_types::values::ValueTag;
use dmasm::{Instruction, Node, DebugData};
use dmasm::operands::Variable;
use std::ffi::CString;
use std::borrow::Borrow;
use auxtools::Proc;

pub enum DMIR {
    GetLocal(u32),
    SetLocal(u32),
    GetSrc,
    GetArg(u32),
    SetCache,
    GetCacheField(u32),
    FloatAdd,
    FloatMul,
    FloatTg,
    PushInt(i32),
    PushVal(dmasm::operands::ValueOpRaw),
    Pop,
    Ret,
    Test,
    JZ(String),
    EnterBlock(String),
    Deopt(u32, ProcId),
    CheckTypeDeopt(u32, ValueTag, Box<DMIR>),
    End
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
            let mut id = auxtools::raw_types::strings::StringId(0);
            unsafe {
                // obtain id of string from BYOND
                auxtools::raw_types::funcs::get_string_id(&mut id, CString::from_vec_unchecked(str.0.clone()).as_ptr());
            }
            // gen DMIR
            out.push(DMIR::GetCacheField(id.0));
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
                    Instruction::CallGlob(_arg_count, callee) => {
                        if callee.0 == "/dm_jitaux_deopt" {
                            irs.push(DMIR::Deopt(data.offset, proc.id));
                            irs.push(DMIR::EnterBlock(format!("post_deopt_{}", data.offset)));
                        } else {
                            supported = false
                        }
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
