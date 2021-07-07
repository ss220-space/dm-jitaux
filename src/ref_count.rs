use crate::dmir::DMIR;
use std::rc::Rc;

/// Denotes different types of value sources
#[derive(Debug)]
enum RValue {
    ProduceSource(usize), // We may or may not inc ref count
    ConsumeSource(usize), // We should dec ref count on consume
    Phi(usize, Vec<Rc<RValue>>)
}

/// Denotes value drains
#[derive(Debug)]
enum RValueDrain {
    ConsumeDrain(usize, Rc<RValue>), // We may or may not dec ref count
    RequireDrain(usize, Rc<RValue>), // We should be able to dec ref count
}

pub fn generate_ref_count_operations(ir: &mut Vec<DMIR>) {

    let mut drains: Vec<RValueDrain> = Vec::new();

    let mut values: Vec<Rc<RValue>> = Vec::new();

    let mut stack: Vec<Rc<RValue>> = Vec::new();
    let mut cache: Option<Rc<RValue>> = None;

    for (pos, op) in ir.iter().enumerate() {
        match op {
            DMIR::GetLocal(_) => {}
            DMIR::SetLocal(_) => {}
            DMIR::GetSrc => {}
            DMIR::GetArg(_) => {}
            DMIR::SetCache => {
                cache = Some(stack.pop().unwrap());
            }
            DMIR::GetCacheField(_) => {
                let value = Rc::new(RValue::ProduceSource(pos));
                values.push(value.clone());
                stack.push(value);
            }
            DMIR::SetCacheField(_) => {}
            DMIR::PushCache => {
                stack.push(cache.unwrap().clone());
            }
            DMIR::FloatAdd => {}
            DMIR::FloatSub => {}
            DMIR::FloatMul => {}
            DMIR::FloatCmp(_) => {}
            DMIR::FloatAbs => {}
            DMIR::PushInt(_) => {}
            DMIR::PushVal(_) => {}
            DMIR::PushTestFlag => {}
            DMIR::Pop => {
                let value = stack.pop().unwrap();
                drains.push(RValueDrain::ConsumeDrain(pos, value));
            }
            DMIR::Ret => {
                let value = stack.pop().unwrap();
                drains.push(RValueDrain::RequireDrain(pos, value));
            }
            DMIR::Test => {
                let value = stack.pop().unwrap();
                drains.push(RValueDrain::ConsumeDrain(pos, value));
            }
            DMIR::JZ(_) => {}
            DMIR::Dup => {}
            DMIR::Swap => {}
            DMIR::TestJZ(_) => {}
            DMIR::TestJNZ(_) => {}
            DMIR::EnterBlock(_) => {

            }
            DMIR::Deopt(_, _) => {}
            DMIR::CheckTypeDeopt(_, _, _) => {}
            DMIR::CallProcById(_, _, _) => {}
            DMIR::CallProcByName(_, _, _) => {}
            DMIR::End => {}
        }
    }

    log::debug!("{:?}", drains);
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn internal() {

        let mut dmir = vec!(
            DMIR::GetSrc,
            DMIR::SetCache,
            DMIR::GetCacheField(0),
            DMIR::Pop,
            DMIR::End
        );

        generate_ref_count_operations(&mut dmir)
    }
}