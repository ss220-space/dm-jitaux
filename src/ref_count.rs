use crate::dmir::DMIR;
use std::rc::Rc;
use crate::ref_count::RValueDrain::{ConsumeDrain, MoveDrain};
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;
use std::borrow::{BorrowMut, Borrow};
use crate::ref_count::RValue::Phi;
use std::cell::RefCell;
use inkwell::debug_info::DWARFSourceLanguage::Haskell;
use std::hash::{Hash, Hasher};
use std::ops::Deref;

/// Denotes different types of value sources
#[derive(Debug, Eq, PartialEq)]
enum RValue {
    ProduceSource(usize), // We may or may not inc ref count
    MovedInSource(usize), // We should dec ref count on consume
    Phi(u32, RefCell<Vec<Rc<RValue>>>)
}

impl Hash for RValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            RValue::ProduceSource(pos) => state.write_usize(pos),
            RValue::MovedInSource(pos) => state.write_usize(pos),
            Phi(pos, _) => state.write_u32(pos)
        }
    }
}

/// Denotes value drains
#[derive(Debug, Eq, Hash, PartialEq, Clone)]
enum RValueDrain {
    ConsumeDrain(usize, Rc<RValue>), // We may or may not dec ref count
    MoveDrain(usize, Rc<RValue>), // We should be able to dec ref count
}

struct BasicBlockNodes {
    stack_phi: Vec<Rc<RValue>>,
    cache_phi: Option<Rc<RValue>>,
    locals_phi: HashMap<u32, Rc<RValue>>,
}

impl RValue {
    fn new_phi(phi_id: &mut u32, first_incoming: &Rc<RValue>) -> Rc<RValue> {
        Rc::new(Phi(*phi_id, RefCell::new(vec![first_incoming.clone()])))
    }
}

fn merge_block<'a>(
    phi_id: &mut u32,
    blocks: &mut HashMap<&'a str, BasicBlockNodes>,
    lbl: &'a str,
    stack: &Vec<Rc<RValue>>,
    cache: &Option<Rc<RValue>>
) {
    match blocks.entry(lbl) {
        Entry::Occupied(mut entry) => {
            let mut v = entry.get_mut();
            for (i, value) in stack.iter().enumerate() {
                match &*v.stack_phi[i] {
                    Phi(_, incoming) => {
                        incoming.borrow_mut().push(value.clone())
                    }
                    _ => {}
                }
            }
        }
        Entry::Vacant(entry) => {
            entry.insert(
                BasicBlockNodes {
                    stack_phi: stack.iter().map(|f| RValue::new_phi(phi_id, f)).collect(),
                    cache_phi: cache.clone().map(|f| RValue::new_phi(phi_id, &f)),
                    locals_phi: Default::default()
                }
            );
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
enum Decision {
    Keep,
    Remove,
    Undecided
}

fn rvalue_dfs(value: Rc<RValue>, visited: &mut HashSet<Rc<RValue>>) {
    if visited.contains(value.as_ref()) {
        return;
    }
    visited.insert(value.clone());
    match value.as_ref() {
        RValue::ProduceSource(_) => {}
        RValue::MovedInSource(_) => {}
        Phi(_, incoming) => {
            for value in incoming.borrow().iter() {
                rvalue_dfs(value.clone(), visited);
            }
        }
    }
}

pub fn generate_ref_count_operations(ir: &mut Vec<DMIR>) {

    let mut drains: Vec<RValueDrain> = Vec::new();

    let mut values: Vec<Rc<RValue>> = Vec::new();

    let mut stack: Vec<Rc<RValue>> = Vec::new();
    let mut cache: Option<Rc<RValue>> = None;

    let mut blocks: HashMap<&str, BasicBlockNodes> = HashMap::new();
    let mut phi_id: u32 = 0;

    let mut block_ended = false;

    for (pos, op) in ir.iter().enumerate() {
        match op {
            DMIR::GetLocal(_) => {}
            DMIR::SetLocal(_) => {}
            DMIR::GetSrc => {
                let value = Rc::new(RValue::ProduceSource(pos));
                values.push(value.clone());
                stack.push(value);
            }
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
                stack.push(cache.as_ref().unwrap().clone());
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
                drains.push(RValueDrain::MoveDrain(pos, value));
            }
            DMIR::Test => {
                let value = stack.pop().unwrap();
                drains.push(RValueDrain::ConsumeDrain(pos, value));
            }
            DMIR::JZ(lbl) => {
                merge_block(&mut phi_id, &mut blocks, lbl.as_str(), &stack, &cache)
            }
            DMIR::Dup => {
                let value = stack.pop().unwrap();
                stack.push(value.clone());
                stack.push(value)
            }
            DMIR::Swap => {}
            DMIR::TestJZ(_) => {}
            DMIR::TestJNZ(_) => {}
            DMIR::EnterBlock(lbl) => {
                let lbl_str = lbl.as_str();
                if !block_ended {
                    merge_block(&mut phi_id, &mut blocks, lbl_str, &stack, &cache);
                } else {
                    block_ended = false;
                }
                let block = blocks.get(lbl_str).unwrap();
                stack.clear();
                stack.append(&mut block.stack_phi.clone());
                cache = block.cache_phi.clone();
            }
            DMIR::Deopt(_, _) => {}
            DMIR::CheckTypeDeopt(_, _, _) => {}
            DMIR::CallProcById(_, _, arg_count) => {
                stack.pop(); // drop src
                for _ in 0..arg_count.clone() {
                    let value = stack.pop().unwrap();
                    drains.push(MoveDrain(pos, value))
                }
            }
            DMIR::CallProcByName(_, _, arg_count) => {}
            DMIR::End => {
                if let Some(cache) = cache.as_ref() {
                    drains.push(ConsumeDrain(pos, cache.clone()))
                }
            }
            DMIR::Not => {}
            DMIR::TestEqual => {}
            DMIR::IsNull => {}
            DMIR::DupX1 => {}
            DMIR::Jmp(lbl) => {
                merge_block(&mut phi_id, &mut blocks, lbl.as_str(), &stack, &cache);
                block_ended = true;
            }
        }
    }


    let mut sources_by_drain: HashMap<RValueDrain, _> = HashMap::new();
    let mut decision_by_source: HashMap<Rc<RValue>, Decision> = HashMap::new();
    let mut decision_by_drain: HashMap<&RValueDrain, Decision> = HashMap::new();


    for drain in drains.iter() {
        let mut sources = HashSet::new();
        match drain {
            ConsumeDrain(_, value) => {
                rvalue_dfs(value.clone(), &mut sources)
            },
            MoveDrain(_, value) => {
                rvalue_dfs(value.clone(), &mut sources)
            }
        }

        sources_by_drain.insert(drain.clone(), sources);
    }

    let mut pending = Vec::new();

    // Add initial constraints
    'outer: for (drain, sources) in &sources_by_drain {
        match drain {
            MoveDrain(_, _) => {
                for source in sources {
                    decision_by_source.insert(source.clone(), Decision::Keep);
                }
                decision_by_drain.insert(drain, Decision::Keep);
            }
            _ => {
                for source in sources {
                    match &**source {
                        RValue::MovedInSource(_) => {
                            decision_by_drain.insert(drain, Decision::Keep);
                            continue'outer;
                        }
                        _ => {}
                    }
                }

                pending.push(drain);
            }
        }
    }

    // iterate constraints until fixpoint
    let mut has_changes = false;
    loop {
        for drain in pending.iter() {
            let mut decision = Decision::Undecided;
            for source in sources_by_drain.get(&drain).unwrap() {
                match decision_by_source.get(source).unwrap_or(&Decision::Undecided) {
                    Decision::Keep => {
                        decision = Decision::Keep;
                    }
                    _ => {}
                }
            }

            if decision != Decision::Undecided {
                has_changes = true;
                for source in sources_by_drain.get(&drain).unwrap() {
                    decision_by_source.insert(source.clone(), decision.clone());
                }
                decision_by_drain.insert(drain, decision);
            }
        }
        if !has_changes {
            break
        }
    }

    // pending now is only unreachable, let's remove them
    for drain in pending {
        let decision = Decision::Remove;
        for source in sources_by_drain.get(&drain).unwrap() {
            decision_by_source.insert(source.clone(), decision.clone());
        }
        decision_by_drain.insert(drain, decision);
    }


    // build annotations
    let mut annotations: HashMap<_, Vec<String>> = HashMap::new();

    for drain in &drains {
        let pos = match drain {
            ConsumeDrain(pos, _) => pos,
            MoveDrain(pos, _) => pos
        };

        let decision = decision_by_drain.get(drain);
        let annotation = format!("dec: {:?}", decision);
        annotations.entry(pos.clone())
            .and_modify(|entry| entry.push(annotation.clone()) )
            .or_insert_with(|| vec![annotation] );
    }

    for value in &values {
        let pos = match &**value {
            RValue::ProduceSource(pos) => pos,
            RValue::MovedInSource(pos) => pos,
            Phi(_, _) => continue
        };

        let decision = decision_by_source.get(value);
        let annotation = format!("inc: {:?}", decision);
        annotations.entry(pos.clone())
            .and_modify(|entry| entry.push(annotation.clone()) )
            .or_insert_with(|| vec![annotation] );
    }

    // render annotations
    for (pos, dmir) in ir.iter().enumerate() {
        if let Some(annotations) = annotations.get(&pos) {
            println!("{:?} // {}", dmir, annotations.join(", "))
        } else {
            println!("{:?}", dmir)
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_ref() {

        let mut dmir = vec!(
            DMIR::GetSrc,
            DMIR::SetCache,
            DMIR::GetCacheField(0),
            DMIR::Pop,
            DMIR::End
        );

        generate_ref_count_operations(&mut dmir);

        panic!("")
    }

    #[test]
    fn simple_phi() {

        let mut dmir = vec!(
            DMIR::GetSrc,
            DMIR::SetCache,
            DMIR::GetCacheField(0),
            DMIR::GetCacheField(1),
            DMIR::Test,
            DMIR::JZ("lbl".to_string()),
            DMIR::Pop,
            DMIR::GetCacheField(2),
            DMIR::EnterBlock("lbl".to_string()),
            DMIR::Test,
            DMIR::End
        );

        generate_ref_count_operations(&mut dmir);

        panic!("")
    }

    #[test]
    fn loop_phi() {

        let mut dmir = vec!(
            DMIR::GetSrc,
            DMIR::SetCache,
            DMIR::GetCacheField(0),
            DMIR::EnterBlock("lbl".to_string()),
            DMIR::GetCacheField(1),
            DMIR::Test,
            DMIR::JZ("lbl".to_string()),
            DMIR::Pop,
            DMIR::End
        );

        generate_ref_count_operations(&mut dmir);

        panic!("")
    }

    #[test]
    fn non_remove_ret() {

        let mut dmir = vec!(
            DMIR::GetSrc,
            DMIR::SetCache,
            DMIR::GetCacheField(0),
            DMIR::Ret,
            DMIR::End
        );

        generate_ref_count_operations(&mut dmir);

        panic!("")
    }

    #[test]
    fn non_remove_ret_phi() {

        let mut dmir = vec!(
            DMIR::GetSrc,
            DMIR::SetCache,
            DMIR::GetCacheField(0),
            DMIR::Test,
            DMIR::JZ("lbl".to_string()),
            DMIR::GetCacheField(1),
            DMIR::Jmp("end".to_string()),
            DMIR::EnterBlock("lbl".to_string()),
            DMIR::GetCacheField(2),
            DMIR::EnterBlock("end".to_string()),
            DMIR::Ret,
            DMIR::End
        );

        generate_ref_count_operations(&mut dmir);

        panic!("")
    }
}