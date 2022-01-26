use std::borrow::Borrow;
use std::ffi::CString;

use auxtools::Proc;
use auxtools::raw_types::procs::ProcId;
use auxtools::raw_types::strings::StringId;
use auxtools::raw_types::values::ValueTag;
use dmasm::{DebugData, Instruction, Node};
use dmasm::operands::{Value, ValueOpRaw, Variable};
use inkwell::FloatPredicate;

use crate::dmir::DMIR::{CheckTypeDeopt, EnterBlock};

#[derive(Debug)]
pub enum DMIR {
    GetLocal(u32),
    SetLocal(u32),
    GetSrc,
    GetArg(u32),
    SetArg(u32),
    SetCache,
    GetCacheField(u32),
    SetCacheField(u32),
    PushCache,
    ValueTagSwitch(ValueLocation, Box<Vec<(ValueTagPredicate, String)>>),
    FloatAdd,
    FloatSub,
    FloatMul,
    FloatDiv,
    FloatCmp(inkwell::FloatPredicate),
    FloatAbs,
    RoundN,
    ListCheckSizeDeopt(ValueLocation, ValueLocation, Box<DMIR>),
    ListIndexedGet,
    ListIndexedSet,
    ListAssociativeGet,
    ListAssociativeSet,
    PushInt(i32),
    PushVal(dmasm::operands::ValueOpRaw),
    PushTestFlag, // Push test flag value as Number
    Pop,
    Ret,
    Not,
    Test,
    TestEqual,
    IsNull,
    JZ(String),
    Dup, // Duplicate last value on stack
    DupX1, // Duplicate top value and insert one slot back ..., a, b -> ..., b, a, b
    Swap, // Swap values on stack top: ..., b, a -> ..., a, b
    TestInternal,        // Perform Test and write internal_test_flag
    JZInternal(String),  // Jump based on internal_test_flag
    JNZInternal(String), // Jump based on internal_test_flag
    EnterBlock(String),
    Jmp(String),
    Deopt(u32, ProcId),
    CheckTypeDeopt(u32, ValueTagPredicate, Box<DMIR>), // Doesn't consume stack value for now
    CallProcById(ProcId, u8, u32),
    CallProcByName(StringId, u8, u32),
    IncRefCount { target: RefOpDisposition, op: Box<DMIR> },
    DecRefCount { target: RefOpDisposition, op: Box<DMIR> },
    Nop,
    UnsetLocal(u32),
    UnsetCache,
    End
}

#[derive(Clone, Debug)]
pub enum ValueTagPredicate {
    Any,
    None,
    Tag(ValueTag),
    Union(Vec<ValueTagPredicate>)
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum RefOpDisposition {
    DupPost(ValueLocation), // Read value before op, dup, execute op, execute ref count operation
    Post(ValueLocation),    // Read value after op, execute ref count operation
    Pre(ValueLocation)      // Read value before op, execute ref count operation, execute op
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ValueLocation {
    Stack(u8),
    Cache,
    Local(u32)
}

macro_rules! value_tag_pred {
    (@any) => ({ ValueTagPredicate::Any });
    (@nothing) => ({ ValueTagPredicate::Nothing });
    ($tag:expr) => ({ ValueTagPredicate::Tag($tag) });
    (@union $($tag:expr),+) => ({ ValueTagPredicate::Union(vec![$(value_tag_pred!($tag)),+]) });
}

macro_rules! check_type_deopt {
    (@$slot:literal !is $pred:expr => $deopt:expr) => {
        DMIR::CheckTypeDeopt($slot, $pred, Box::new($deopt))
    };
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
        Variable::Null => out.push(DMIR::PushVal(ValueOpRaw { tag: 0x0, data: 0 })), // TODO: make special instruction to allow no ref-counting over that Null value
        _ => panic!("decode_get_var: Not supported {:?}", vr)
    }
}

// decode Variable nesing in SetVar
fn decode_set_var(vr: &Variable, out: &mut Vec<DMIR>) {
    match vr {
        Variable::Local(idx) => out.push(DMIR::SetLocal(idx.clone())),
        Variable::SetCache(first, second) => {
            decode_get_var(first.borrow(), out);
            out.push(DMIR::SetCache);
            decode_set_var(second.borrow(), out);
        }
        Variable::Field(str) => {
            out.push(DMIR::SetCacheField(get_string_id(&(str.0)).0))
        }
        Variable::Arg(idx) => out.push(DMIR::SetArg(idx.clone())),
        _ => panic!("decode_set_var: Not supported {:?}", vr)
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
            let name_id = unsafe { &*Proc::find(&proc_id.path).unwrap().entry }.name;
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

fn decode_cmp(op: FloatPredicate, data: &DebugData, proc: &Proc, out: &mut Vec<DMIR>) {
    out.push(check_type_deopt!(
        @0 !is value_tag_pred!(@union ValueTag::Number, ValueTag::Null)
        => DMIR::Deopt(data.offset, proc.id)
    ));
    out.push(check_type_deopt!(
        @1 !is value_tag_pred!(@union ValueTag::Number, ValueTag::Null)
        => DMIR::Deopt(data.offset, proc.id)
    ));
    out.push(DMIR::FloatCmp(op));
}

fn gen_push_null(out: &mut Vec<DMIR>) {
    out.push(DMIR::PushVal(ValueOpRaw { tag: ValueTag::Null as u8, data: 0 }));
}

fn build_float_bin_op_deopt(action: DMIR, data: &DebugData, proc: &Proc, out: &mut Vec<DMIR>) {
    out.push(CheckTypeDeopt(0, ValueTagPredicate::Tag(ValueTag::Number), Box::new(DMIR::Deopt(data.offset, proc.id))));
    out.push(CheckTypeDeopt(1, ValueTagPredicate::Tag(ValueTag::Number), Box::new(DMIR::Deopt(data.offset, proc.id))));
    out.push(action);
}

fn decode_float_aug_instruction(var: &Variable, action: DMIR, data: &DebugData, proc: &Proc, out: &mut Vec<DMIR>) {
    decode_get_var(var, out);
    out.push(DMIR::Swap);
    build_float_bin_op_deopt(action, data, proc, out);
    decode_set_var(var, out);
}

fn decode_switch(value: ValueLocation, switch_id: &mut u32, cases: Vec<(ValueTagPredicate, Vec<DMIR>)>, out: &mut Vec<DMIR>) {
    let switch_exit = format!("switch_{}_exit", switch_id);
    let (predicates, blocks): (Vec<_>, Vec<_>) = cases.into_iter().unzip();
    out.push(DMIR::ValueTagSwitch(value, Box::new(
        predicates.into_iter().enumerate().map(
            |(index, predicate)| (predicate, format!("switch_{}_case_{}", switch_id, index))
        ).collect()
    )));
    let mut case_counter = 0;
    for mut instructions in blocks {
        out.push(EnterBlock(format!("switch_{}_case_{}", switch_id, case_counter)));
        out.append(&mut instructions);
        if !matches!(out.last(), Option::Some(DMIR::End)) {
            out.push(DMIR::Jmp(switch_exit.clone()));
        }
        case_counter += 1;
    }
    out.push(EnterBlock(switch_exit));
    *switch_id += 1;
}

pub fn decode_byond_bytecode(nodes: Vec<Node<DebugData>>, proc: Proc) -> Result<Vec<DMIR>, ()> {
    // output for intermediate operations sequence
    let mut irs = vec![];

    // will set to false if some unsupported operation found
    let mut supported = true;

    // needed for generating fallthrough jumps on EnterBlock
    let mut block_ended = false;

    let mut switch_counter = 0;

    macro_rules! type_switch {
        (@stack $n:literal, $(($($check:tt)+) => $body:expr),+) => ({
            let cases = vec![
                $((value_tag_pred!($($check)+), $body)),+
            ];
            let mut block = Vec::new();
            decode_switch(ValueLocation::Stack($n), &mut switch_counter, cases, &mut block);
            block
        });
    }

    // generate DMIR sequence for each instruction in dm-asm
    for nd in nodes {
        match nd {
            // if node contains instruction
            dmasm::Node::Instruction(insn, data) => {
                macro_rules! deopt {
                    () => { DMIR::Deopt(data.offset, proc.id) };
                    (@type_switch) => ( vec![deopt!(), DMIR::End] );
                }
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
                        irs.append(
                            &mut type_switch!(
                                @stack 1,
                                (ValueTag::Number) =>
                                    type_switch!(
                                        @stack 0,
                                        (@union ValueTag::Number, ValueTag::Null) => vec![DMIR::FloatAdd],
                                        (@any) => deopt!(@type_switch)
                                    ),
                                (@any) => deopt!(@type_switch)
                            )
                        );
                    }
                    Instruction::Sub => {
                        irs.append(
                            &mut type_switch!(
                                @stack 1,
                                (ValueTag::Number) =>
                                    type_switch!(
                                        @stack 0,
                                        (@union ValueTag::Number, ValueTag::Null) => vec![DMIR::FloatSub],
                                        (@any) => deopt!(@type_switch)
                                    ),
                                (@any) => deopt!(@type_switch)
                            )
                        );
                    }
                    Instruction::Mul => {
                        irs.push(CheckTypeDeopt(
                            1,
                            value_tag_pred!(ValueTag::Number),
                            Box::new(DMIR::Deopt(data.offset, proc.id))
                        ));
                        irs.push(DMIR::FloatMul);
                    }
                    Instruction::Div => {
                        build_float_bin_op_deopt(DMIR::FloatDiv, &data, &proc, &mut irs);
                    }
                    Instruction::RoundN => {
                        irs.push(DMIR::RoundN);
                    }
                    Instruction::Tg => {
                        decode_cmp(FloatPredicate::UGT, &data, &proc, &mut irs);
                    }
                    Instruction::Tl => {
                        decode_cmp(FloatPredicate::ULT, &data, &proc, &mut irs);
                    }
                    Instruction::Tge => {
                        decode_cmp(FloatPredicate::UGE, &data, &proc, &mut irs);
                    }
                    Instruction::Tle => {
                        decode_cmp(FloatPredicate::ULE, &data, &proc, &mut irs);
                    }
                    Instruction::Teq => {
                        irs.push(DMIR::DupX1);
                        irs.push(DMIR::Swap);
                        irs.push(DMIR::TestEqual);
                    }
                    Instruction::Not => {
                        irs.push(DMIR::Not)
                    }
                    Instruction::Abs => {
                        irs.push(DMIR::FloatAbs)
                    }
                    Instruction::ListGet => {
                        irs.push(CheckTypeDeopt(
                            1,
                            value_tag_pred!(ValueTag::List),
                            Box::new(DMIR::Deopt(data.offset, proc.id))
                        ));
                        irs.append(
                            &mut type_switch!(
                                @stack 0,
                                (ValueTag::Number) => vec![DMIR::ListCheckSizeDeopt(ValueLocation::Stack(1), ValueLocation::Stack(0), Box::new(DMIR::Deopt(data.offset, proc.id))), DMIR::ListIndexedGet],
                                (@any) => vec![DMIR::ListAssociativeGet]
                            )
                        );
                    }
                    Instruction::ListSet => {
                        irs.push(CheckTypeDeopt(
                            1,
                            value_tag_pred!(ValueTag::List),
                            Box::new(DMIR::Deopt(data.offset, proc.id))
                        ));
                        irs.append(
                            &mut type_switch!(
                                @stack 0,
                                (ValueTag::Number) => vec![DMIR::ListCheckSizeDeopt(ValueLocation::Stack(1), ValueLocation::Stack(0), Box::new(DMIR::Deopt(data.offset, proc.id))), DMIR::ListIndexedSet],
                                (@any) => vec![DMIR::ListAssociativeSet]
                            )
                        );
                    }
                    Instruction::CallGlob(arg_count, callee) => {
                        match callee.path.as_ref() {
                            "/dm_jitaux_deopt" => {
                                irs.push(DMIR::Deopt(data.offset, proc.id));
                                gen_push_null(&mut irs);
                            }
                            _ => {
                                gen_push_null(&mut irs);
                                let id = Proc::find(callee.path).unwrap().id;
                                irs.push(DMIR::CallProcById(id, 2, arg_count))
                            }
                        }
                    }
                    Instruction::Call(var, arg_count) => {
                        decode_call(&var, arg_count, &mut irs);
                    }
                    Instruction::CallStatement(var, arg_count) => {
                        decode_call(&var, arg_count, &mut irs);
                    }
                    Instruction::Ret => {
                        irs.push(DMIR::Ret);
                        block_ended = true;
                    }
                    Instruction::End => {
                        irs.push(DMIR::End);
                        block_ended = true;
                    }
                    Instruction::Test => {
                        irs.push(DMIR::Test)
                    }
                    Instruction::Jz(lbl) => {
                        irs.push(DMIR::JZ(lbl.0))
                    }
                    Instruction::Jmp(lbl) => {
                        irs.push(DMIR::Jmp(lbl.0));
                        block_ended = true;
                    }
                    Instruction::JmpAnd(lbl) => {
                        irs.push(DMIR::Dup);
                        irs.push(DMIR::TestInternal);
                        irs.push(DMIR::JZInternal(lbl.0));
                        irs.push(DMIR::Pop);
                    }
                    Instruction::JmpOr(lbl) => {
                        irs.push(DMIR::Dup);
                        irs.push(DMIR::TestInternal);
                        irs.push(DMIR::JNZInternal(lbl.0));
                        irs.push(DMIR::Pop);
                    }
                    Instruction::PushInt(i32) => {
                        irs.push(DMIR::PushInt(i32))
                    }
                    Instruction::PushVal(op) => {
                        match op.value {
                            Value::Number(value) => {
                                irs.push(DMIR::PushVal(ValueOpRaw {
                                    tag: ValueTag::Number as u8,
                                    data: unsafe { std::mem::transmute(value) }
                                }))
                            }
                            _ => {
                                irs.push(DMIR::PushVal(op.raw.unwrap()))
                            }
                        }

                    }
                    Instruction::GetFlag => {
                        irs.push(DMIR::PushTestFlag);
                    }
                    Instruction::Pop => {
                        irs.push(DMIR::Pop)
                    }
                    Instruction::AugAdd(var) => {
                        decode_float_aug_instruction(&var, DMIR::FloatAdd, &data, &proc, &mut irs)
                    }
                    Instruction::AugSub(var) => {
                        decode_float_aug_instruction(&var, DMIR::FloatSub, &data, &proc, &mut irs)
                    }
                    Instruction::IsNull => {
                        irs.push(DMIR::IsNull)
                    }
                    _ => {
                        log::info!("Unsupported insn {}", insn);
                        supported = false;
                    }
                }
            }
            dmasm::Node::Label(lbl) => {
                //log::info!("{}:", lbl)
                if !block_ended {
                    irs.push(DMIR::Jmp(lbl.clone()));
                }
                block_ended = false;
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
