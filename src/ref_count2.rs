use std::cmp::max;
use RefOpDisposition::{Post, Pre};
use crate::dfa::{analyze_and_dump_dfa, OperationEffect};
use crate::dmir::{DMIR, RefOpDisposition, ValueLocation};
use crate::dmir::DMIR::IncRefCount;
use crate::dmir::RefOpDisposition::DupPost;
use crate::dmir::ValueLocation::{Argument, Cache, Local, Stack};
use crate::ref_count2::RefCountOp::{Dec, Inc};

pub fn generate_ref_count_operations2(
    ir: &mut Vec<DMIR>,
    parameter_count: usize,
    data_flow_info: Vec<OperationEffect>
) {

}

enum RefCountOp {
    Inc(RefOpDisposition),
    Dec(RefOpDisposition)
}

fn analyze_instruction(instruction: &DMIR, stack_size: u8) {
    match instruction {
        DMIR::GetLocal(_) => {
            Inc(Post(Stack(stack_size)));
        }
        DMIR::SetLocal(local_idx) => {
            Dec(Pre(Local(*local_idx)));
        }
        DMIR::GetSrc => {
            Inc(Post(Stack(stack_size)));
        }
        DMIR::GetArg(_) => {
            Inc(Post(Stack(stack_size)));
        }
        DMIR::SetArg(arg_idx) => {
            Dec(Pre(Argument(*arg_idx)));
        }
        DMIR::SetCache => {
            Dec(Pre(Cache))
        }
        DMIR::GetCacheField(_) => {
            Inc(Post(Stack(stack_size)))
        }
        DMIR::SetCacheField(_) => {
            Dec(DupPost(Stack(stack_size)))
        }
        DMIR::PushCache => {
            Dec(DupPost(Stack(stack_size)))
        }
        DMIR::ValueTagSwitch(_, _) => {}
        DMIR::FloatAdd => {}
        DMIR::FloatSub => {}
        DMIR::FloatMul => {}
        DMIR::FloatDiv => {}
        DMIR::FloatCmp(_) => {}
        DMIR::FloatAbs => {}
        DMIR::FloatInc => {}
        DMIR::FloatDec => {}
        DMIR::BitAnd => {}
        DMIR::BitOr => {}
        DMIR::RoundN => {}
        DMIR::ListCheckSizeDeopt(_, _, _) => {}
        DMIR::ListCopy => {
            Dec(DupPost(Stack(stack_size)))
        }
        DMIR::ListAddSingle | DMIR::ListSubSingle => {
            Dec(DupPost(Stack(stack_size - 2)));
            Dec(DupPost(Stack(stack_size - 1)));
        }
        DMIR::ListIndexedGet | DMIR::ListAssociativeGet => {
            Dec(DupPost(Stack(stack_size - 2)));
            Dec(DupPost(Stack(stack_size - 1)));
            Inc(Post(Stack(stack_size)));
        }
        DMIR::ListIndexedSet | DMIR::ListAssociativeSet => {
            Dec(DupPost(Stack(stack_size - 3)));
            Dec(DupPost(Stack(stack_size - 2)));
        }
        DMIR::NewVectorList(_) => {}
        DMIR::NewAssocList(_, _) => {}
        DMIR::ArrayIterLoadFromList(_) => {

        }
        DMIR::ArrayIterLoadFromObject(_) => {

        }
        DMIR::IterAllocate => {}
        DMIR::IterPop => {}
        DMIR::IterPush => {}
        DMIR::IterNext => {
            Inc(Post(Stack(stack_size)))
        }
        DMIR::GetStep => {

        }
        DMIR::PushInt(_) => {}
        DMIR::PushVal(_) => {}
        DMIR::PushTestFlag => {}
        DMIR::SetTestFlag(_) => {}
        DMIR::Pop => {}
        DMIR::Ret => {}
        DMIR::Not => {}
        DMIR::Test => {}
        DMIR::TestEqual => {}
        DMIR::TestIsDMEntity => {}
        DMIR::IsSubtypeOf => {}
        DMIR::JZ(_) => {}
        DMIR::Dup => {}
        DMIR::DupX1 => {}
        DMIR::DupX2 => {}
        DMIR::Swap => {}
        DMIR::SwapX1 => {}
        DMIR::TestInternal => {}
        DMIR::JZInternal(_) => {}
        DMIR::JNZInternal(_) => {}
        DMIR::EnterBlock(_) => {}
        DMIR::Jmp(_) => {}
        DMIR::InfLoopCheckDeopt(_) => {}
        DMIR::Deopt(_, _) => {}
        DMIR::CheckTypeDeopt(_, _, _) => {}
        DMIR::CallProcById(_, _, _) => {}
        DMIR::CallProcByName(_, _, _) => {}
        DMIR::NewDatum(_) => {}
        DMIR::IncRefCount { .. } => {}
        DMIR::DecRefCount { .. } => {}
        DMIR::Nop => {}
        DMIR::UnsetLocal(local_idx) => {
            Dec(Pre(Local(*local_idx)))
        }
        DMIR::UnsetCache => {
            Dec(Pre(Cache))
        }
        DMIR::End => {}
    }
}