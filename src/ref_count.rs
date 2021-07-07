use crate::dmir::{DMIR, RefOpDisposition};
use crate::ref_count::RValueDrain::{ConsumeDrain, MoveOutDrain};
use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;
use std::borrow::{Borrow};
use crate::ref_count::RValue::Phi;
use std::cell::{RefCell};
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use typed_arena::Arena;
use crate::dmir::ValueLocation;

/// Denotes different types of value sources
#[derive(Eq)]
enum RValue<'t> {
    ProduceSource(usize, IncRefOp), // on-demand ref count increment, example: getting value from variable, which already have non-zero ref count, and by this can be eliminated
    UncountedSource(usize), // produces value, that doesn't need to be counted, example: results of numeric operations
    MovedInSource(usize), // moves-in value with already incremented ref count, example: return result of other proc call
    Phi(u32, RefCell<Vec<&'t RValue<'t>>>) // combines values in results of branching
}

impl<'t> PartialEq for RValue<'t> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RValue::ProduceSource(a, a_loc), RValue::ProduceSource(b, b_loc)) => a == b && a_loc == b_loc,
            (RValue::UncountedSource(a), RValue::UncountedSource(b)) => a == b,
            (RValue::MovedInSource(a), RValue::MovedInSource(b)) => a == b,
            (Phi(a, _), Phi(b, _)) => a == b,
            _ => false
        }
    }
}

impl<'t> Hash for RValue<'t> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            RValue::ProduceSource(pos, _) => state.write_usize(pos),
            RValue::MovedInSource(pos) => state.write_usize(pos),
            RValue::UncountedSource(pos) => state.write_usize(pos),
            Phi(pos, _) => state.write_u32(pos),
        }
    }
}

/// Denotes value drains
#[derive(Debug, Eq, Hash, PartialEq, Clone)]
enum RValueDrain<'t> {
    ConsumeDrain(usize, &'t RValue<'t>, DecRefOp), // decrement ref count on-demand, example: pop value off stack should dec ref count if value had incremented ref count before
    MoveOutDrain(usize, &'t RValue<'t>), // moves-out value to somewhere else, meaning value must have incremented ref count, example: return from proc
}

struct BasicBlockNodes<'t> {
    stack_phi: Vec<&'t RValue<'t>>,
    cache_phi: Option<&'t RValue<'t>>,
    locals_phi: HashMap<u32, &'t RValue<'t>>,
}

impl<'t> RValue<'t> {
    fn new_phi(phi_id: &mut u32, first_incoming: &'t RValue<'t>) -> RValue<'t> {
        Phi(*phi_id, RefCell::new(vec![first_incoming]))
    }
}

macro_rules! mk_value {
    ( $self:expr, $x:expr ) => (
        {
            let value = $self.values_arena.alloc($x);
            $self.values.push(value);
            value
        }
    )
}



#[derive(Clone, Eq, PartialEq, Debug)]
enum Decision {
    Keep,
    Remove,
    Undecided
}

fn rvalue_dfs<'t>(value: &'t RValue<'t>, visited: &mut HashSet<&'t RValue<'t>>) {
    if !visited.insert(value) {
        return;
    }
    match value {
        RValue::ProduceSource(_, _) => {}
        RValue::MovedInSource(_) => {}
        RValue::UncountedSource(_) => {}
        Phi(_, incoming) => {
            for value in incoming.borrow().iter() {
                rvalue_dfs(*value, visited);
            }
        }
    }
}

struct Analyzer<'t> {
    values_arena: &'t Arena<RValue<'t>>,
    stack: Vec<&'t RValue<'t>>,
    cache: Option<&'t RValue<'t>>,
    drains: Vec<RValueDrain<'t>>,
    values: Vec<&'t RValue<'t>>,
    locals: HashMap<u32, &'t RValue<'t>>,
    blocks: HashMap<String, BasicBlockNodes<'t>>,
    phi_id: u32,
    block_ended: bool
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum IncRefOp {
    Post(ValueLocation),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum DecRefOp {
    Pre(ValueLocation), DupPost(ValueLocation)
}


impl<'t> Analyzer<'t> {

    fn merge_block(
        stack: &Vec<&'t RValue<'t>>,
        cache: &Option<&'t RValue<'t>>,
        locals: &HashMap<u32, &'t RValue<'t>>,
        values_arena: &'t Arena<RValue<'t>>,
        phi_id: &mut u32,
        blocks: &mut HashMap<String, BasicBlockNodes<'t>>,
        lbl: String,
    ) {
        match blocks.entry(lbl) {
            Entry::Occupied(mut entry) => {
                let mut v = entry.get_mut();
                for (i, value) in stack.iter().enumerate() {
                    match v.stack_phi.get_mut(i).unwrap() {
                        Phi(_, incoming) => {
                            incoming.borrow_mut().push(value);
                        }
                        _ => {}
                    }
                }
                for (idx, value) in locals {
                    match v.locals_phi.get_mut(idx).unwrap() {
                        Phi(_, incoming) => {
                            incoming.borrow_mut().push(value);
                        }
                        _ => {}
                    }
                }
            }
            Entry::Vacant(entry) => {
                entry.insert(
                    BasicBlockNodes {
                        stack_phi: stack.iter().map(|f| {
                            let node = RValue::new_phi(phi_id, f);
                            &*values_arena.alloc(node)
                        }).collect(),
                        cache_phi: cache.as_ref().map(|f|
                            &*values_arena.alloc(RValue::new_phi(phi_id, f))
                        ),
                        locals_phi: locals.iter().map(|(idx, v)| {
                            let node = RValue::new_phi(phi_id, v);
                            (idx.clone(), &*values_arena.alloc(node))
                        }).collect()
                    }
                );
            }
        }
    }

    #[allow(unused_assignments, unused_variables)]
    fn analyze_instruction(&mut self, pos: usize, ir: &DMIR) {

        let mut stack_write_pos: u8 = 0;
        let mut stack_read_pos: u8 = 0;

        macro_rules! make_read {
            (@stack) => ({
                let value = self.stack.pop().unwrap();
                let location = ValueLocation::Stack(stack_read_pos);
                stack_read_pos += 1;
                (value, location)
            });
            (@cache) => ({
                let value = self.cache.unwrap();
                let location = ValueLocation::Cache;
                (value, location)
            });
        }

        macro_rules! make_write_location {
            (@stack) => ({
                let location = ValueLocation::Stack(stack_write_pos);
                stack_write_pos += 1;
                location
            });
            (@cache) => {
                ValueLocation::Cache
            };
            (@local $idx:expr) => {
                ValueLocation::Local($idx)
            };
        }

        macro_rules! make_write {
            ($value:expr, @stack) => ({
                self.stack.push(mk_value!(self, $value));
            });
        }

        macro_rules! single_effect {
            (@consume @$kind:ident $($idx:expr)?) => ({
                let (value, location) = make_read!(@$kind $($idx)?);
                self.drains.push(RValueDrain::ConsumeDrain(pos, value, DecRefOp::DupPost(location)));
            });
            (@move_out @$kind:ident $($idx:expr)?) => ({
                let (value, location) = make_read!(@$kind $($idx)?);
                self.drains.push(RValueDrain::MoveOutDrain(pos, value));
            });
            (@move_in @$kind:ident $($idx:expr)?) => ({
                make_write!(RValue::MovedInSource(pos), @$kind $($idx)?);
            });
            (@produce_uncounted @$kind:ident $($idx:expr)?) => ({
                make_write!(RValue::UncountedSource(pos), @$kind $($idx)?);
            });
            (@produce @$kind:ident $($idx:expr)?) => ({
                make_write!(RValue::ProduceSource(pos, IncRefOp::Post(make_write_location!(@$kind $($idx)?))), @$kind $($idx)?);
            });
        }

        macro_rules! op_effect {
            ($(@$action:ident @$kind:ident $($idx:expr)?),+) => {
                $( single_effect!(@$action @$kind $($idx)?) );+
            };
        }

        macro_rules! unset_locals_and_cache {
            () => {
                if let Some(cache) = self.cache.as_ref() {
                    self.drains.push(ConsumeDrain(pos, cache, DecRefOp::Pre(ValueLocation::Cache)))
                }
                for (idx, value) in self.locals.clone() {
                    self.drains.push(ConsumeDrain(pos, value, DecRefOp::Pre(ValueLocation::Local(idx))))
                }
            }
        }

        match ir {
            DMIR::GetLocal(idx) => {
                op_effect!(
                    @produce @stack
                )
            }
            DMIR::SetLocal(idx) => {
                let value = self.stack.pop().unwrap();
                let old = self.locals.insert(idx.clone(), value);
                if let Some(old) = old {
                    self.drains.push(RValueDrain::ConsumeDrain(pos, old, DecRefOp::Pre(ValueLocation::Local(idx.clone()))))
                }
            }
            DMIR::GetSrc => {
                op_effect!(
                    @produce @stack
                );
            }
            DMIR::GetArg(_) => {
                op_effect!(
                    @produce @stack
                );
            }
            DMIR::SetCache => {
                let prev = self.cache.replace(self.stack.pop().unwrap());
                if let Some(prev) = prev {
                    self.drains.push(RValueDrain::ConsumeDrain(pos, prev, DecRefOp::Pre(ValueLocation::Cache)))
                }
            }
            DMIR::GetCacheField(_) => {
                op_effect!(
                    @move_in @stack
                );
            }
            DMIR::SetCacheField(_) => {
                op_effect!(
                    @consume @stack
                );
            }
            DMIR::PushCache => {
                op_effect!(
                    @produce @stack
                );
            }
            DMIR::FloatAdd | DMIR::FloatSub | DMIR::FloatMul => {
                op_effect!(
                    @consume @stack,
                    @consume @stack,
                    @produce_uncounted @stack
                );
            }
            DMIR::FloatCmp(_) => {
                op_effect!(
                    @consume @stack,
                    @consume @stack,
                    @produce_uncounted @stack
                );
            }
            DMIR::FloatAbs => {
                op_effect!(
                    @consume @stack,
                    @produce_uncounted @stack
                );
            }
            DMIR::PushInt(_) => {
                op_effect!(@produce_uncounted @stack);
            }
            DMIR::PushVal(_) => {
                op_effect!(@produce @stack);
            }
            DMIR::PushTestFlag => {
                op_effect!(@produce_uncounted @stack);
            }
            DMIR::Pop => {
                op_effect!(@consume @stack);
            }
            DMIR::Ret => {
                unset_locals_and_cache!();
                op_effect!(@move_out @stack);
            }
            DMIR::Test => {
                op_effect!(@consume @stack);
            }
            DMIR::JZ(lbl) => {
                Analyzer::merge_block(
                    &self.stack,
                    &self.cache,
                    &self.locals,
                    &self.values_arena,
                    &mut self.phi_id,
                    &mut self.blocks,
                    lbl.to_string()
                )
            }
            DMIR::Dup => {
                let value = self.stack.pop().unwrap();
                self.stack.push(value);
                op_effect!(@produce @stack);
            }
            DMIR::Swap => {
                let a = self.stack.pop().unwrap();
                let b = self.stack.pop().unwrap();
                self.stack.push(a);
                self.stack.push(b);
            }
            DMIR::TestInternal => {
                op_effect!(@consume @stack);
            }
            DMIR::JZInternal(_) => {}
            DMIR::JNZInternal(_) => {}
            DMIR::EnterBlock(lbl) => {
                let lbl_str = lbl.to_string();
                if !self.block_ended {
                    Analyzer::merge_block(
                        &self.stack,
                        &self.cache,
                        &self.locals,
                        &self.values_arena,
                        &mut self.phi_id,
                        &mut self.blocks,
                        lbl_str
                    );
                } else {
                    self.block_ended = false;
                }
                let block = self.blocks.get(lbl).unwrap();
                self.stack.clear();
                self.stack.append(&mut block.stack_phi.clone());
                self.locals.clear();
                for (idx, value) in block.locals_phi.clone() {
                    self.locals.insert(idx.clone(), value);
                }
                self.cache = block.cache_phi.clone();
            }
            DMIR::Deopt(_, _) => {}
            DMIR::CheckTypeDeopt(_, _, _) => {}
            DMIR::CallProcById(_, _, arg_count) | DMIR::CallProcByName(_, _, arg_count) => {
                op_effect!(@consume @stack);
                for _ in 0..arg_count.clone() {
                    op_effect!(@move_out @stack);
                }
                op_effect!(@move_in @stack);
            }
            DMIR::End => {
                assert!(self.stack.is_empty());
                unset_locals_and_cache!();
            }
            DMIR::Not => {
                op_effect!(
                    @consume @stack,
                    @produce @stack
                );
            }
            DMIR::TestEqual => {
                op_effect!(
                    @consume @stack,
                    @consume @stack
                );
            }
            DMIR::IsNull => {
                op_effect!(
                    @consume @stack,
                    @produce @stack
                );
            }
            DMIR::DupX1 => {
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();

                self.stack.push(b);
                self.stack.push(a);
                op_effect!(@produce @stack)
            }
            DMIR::Jmp(lbl) => {
                Analyzer::merge_block(
                    &self.stack,
                    &self.cache,
                    &self.locals,
                    self.values_arena.borrow(),
                    &mut self.phi_id,
                    &mut self.blocks,
                    lbl.to_string()
                );
                self.block_ended = true;
            }
            DMIR::IncRefCount { .. } => panic!(),
            DMIR::DecRefCount { .. } => panic!()
        }


    }

    fn analyze_instructions(&mut self, ir: &Vec<DMIR>) {
        for (pos, op) in ir.iter().enumerate() {
            self.analyze_instruction(pos, op)
        }
    }
}

impl<'t> Debug for RValue<'t> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        describe_source(self, f, &mut HashSet::new())
    }
}

fn describe_source(src: &RValue, fmt: &mut Formatter<'_>, phi_stack: &mut HashSet<u32>) -> std::fmt::Result {
    match src {
        RValue::ProduceSource(pos, _) => write!(fmt, "ProduceSource({})", pos),
        RValue::UncountedSource(pos) => write!(fmt, "UncountedSource({})", pos),
        RValue::MovedInSource(pos) => write!(fmt, "MovedInSource({})", pos),
        Phi(id, sources) => {
            if phi_stack.insert(id.clone()) {
                write!(fmt, "Phi({}, [", id)?;
                for (idx, source) in sources.borrow().iter().enumerate() {
                    if idx != 0 {
                        write!(fmt, ", ")?;
                    }
                    describe_source(source, fmt, phi_stack)?
                }
                write!(fmt, "]")
            } else {
                write!(fmt, "Phi({}, ...)", id)
            }
        }
    }
}

pub fn generate_ref_count_operations(ir: &mut Vec<DMIR>) {

    let arena = Arena::new();
    let mut analyzer = Analyzer {
        values_arena: &arena,
        stack: vec![],
        cache: None,
        drains: vec![],
        values: vec![],
        locals: Default::default(),
        blocks: Default::default(),
        phi_id: 0,
        block_ended: false
    };

    analyzer.analyze_instructions(ir);

    log::debug!("ref count analyzed");

    let mut sources_by_drain: HashMap<&RValueDrain, _> = HashMap::new();
    let mut decision_by_source: HashMap<&RValue, Decision> = HashMap::new();
    let mut decision_by_drain: HashMap<&RValueDrain, Decision> = HashMap::new();


    for drain in analyzer.drains.iter() {
        let mut sources = HashSet::new();
        match drain {
            ConsumeDrain(_, value, _) => {
                rvalue_dfs(value, &mut sources)
            },
            MoveOutDrain(_, value) => {
                rvalue_dfs(value, &mut sources)
            }
        }

        sources_by_drain.insert(drain, sources);
    }

    let mut pending = Vec::new();

    // Add initial constraints
    'outer: for (drain, sources) in &sources_by_drain {
        match drain {
            MoveOutDrain(_, _) => {
                for source in sources {
                    decision_by_source.insert(source, Decision::Keep);
                }
                decision_by_drain.insert(drain, Decision::Keep);
            }
            _ => {
                for source in sources {
                    match source {
                        RValue::MovedInSource(_) => {
                            decision_by_drain.insert(drain, Decision::Keep);
                            continue'outer;
                        }
                        _ => {}
                    }
                }

                pending.push(*drain);
            }
        }
    }

    // iterate constraints until fixpoint
    let mut has_changes = false;
    loop {
        pending = pending.iter().filter_map(|drain| {
            let mut decision = Decision::Undecided;
            for source in sources_by_drain.get(drain).unwrap() {
                match decision_by_source.get(source).unwrap_or(&Decision::Undecided) {
                    Decision::Keep => {
                        decision = Decision::Keep;
                    }
                    _ => {}
                }
            }

            if decision != Decision::Undecided {
                has_changes = true;
                for source in sources_by_drain.get(drain).unwrap() {
                    decision_by_source.insert(source, decision.clone());
                }
                decision_by_drain.insert(drain, decision.clone());
                log::debug!("Fix {:?} into {:?}", drain, decision);
                None
            } else {
                Some(*drain)
            }
        }).collect();
        if !has_changes || pending.is_empty() {
            break
        }
        has_changes = false;
    }

    // pending now is only unreachable, let's remove them
    for drain in pending {
        let decision = Decision::Remove;
        for source in sources_by_drain.get(drain).unwrap() {
            decision_by_source.insert(source, decision.clone());
        }
        decision_by_drain.insert(drain, decision);
    }


    // build annotations
    let mut annotations: HashMap<_, Vec<String>> = HashMap::new();

    for drain in &analyzer.drains {
        let (pos, source) = match drain {
            ConsumeDrain(pos, val, _) => (pos, format!("{:?}", val)),
            MoveOutDrain(pos, val) => (pos, format!("{:?}", val))
        };

        let decision = decision_by_drain.get(drain);
        let annotation = format!("dec: {:?} (from: {})", decision.unwrap(), source);
        annotations.entry(pos.clone())
            .and_modify(|entry| entry.push(annotation.clone()) )
            .or_insert_with(|| vec![annotation] );
    }

    for value in &analyzer.values {
        let pos = match *value {
            RValue::ProduceSource(pos, _) => pos,
            RValue::MovedInSource(pos) => pos,
            RValue::UncountedSource(pos) => pos,
            Phi(_, _) => continue,
        };

        let decision = decision_by_source.get(value);
        let annotation = format!("inc: {:?}", decision.unwrap());
        annotations.entry(pos.clone())
            .and_modify(|entry| entry.push(annotation.clone()) )
            .or_insert_with(|| vec![annotation] );
    }

    for drain in analyzer.drains.iter() {
        if decision_by_drain[drain] != Decision::Keep {
            continue;
        }
        match drain {
            ConsumeDrain(pos, _, op) => {
                let element = ir.get_mut(pos.clone()).unwrap();
                match element {
                    _ => {
                        let mut tmp = DMIR::End;
                        std::mem::swap(element, &mut tmp);
                        *element = match op {
                            DecRefOp::Pre(loc) => {
                                DMIR::DecRefCount {
                                    target: RefOpDisposition::Pre(loc.clone()),
                                    op: Box::new(tmp)
                                }
                            }
                            DecRefOp::DupPost(loc) => {
                                DMIR::DecRefCount {
                                    target: RefOpDisposition::DupPost(loc.clone()),
                                    op: Box::new(tmp)
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    for source in analyzer.values.iter() {
        if decision_by_source[source] != Decision::Keep {
            continue;
        }
        match &**source {
            RValue::ProduceSource(pos, op) => {
                let element = ir.get_mut(pos.clone()).unwrap();
                match element {
                    _ => {
                        let mut tmp = DMIR::End;
                        std::mem::swap(element, &mut tmp);
                        match op {
                            IncRefOp::Post(loc) => {
                                *element = DMIR::IncRefCount {
                                    target: RefOpDisposition::Post(loc.clone()),
                                    op: Box::new(tmp)
                                }
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    log::debug!("Generated:");

    for (pos, dmir) in ir.iter().enumerate() {
        if let Some(annotations) = annotations.get(&pos) {
            log::debug!("{}: {:?} // {}", pos, dmir, annotations.join(", "))
        } else {
            log::debug!("{}: {:?}", pos, dmir)
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
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
    #[ignore]
    fn simple_phi() {

        /*
        if src.some_var1
            src.some_var0
        else
            src.some_var2
         */
        let mut dmir = vec!(
            DMIR::GetSrc,
            DMIR::SetCache, // cache = src
            DMIR::GetCacheField(0), // s[0] = cache["some_var0"]
            DMIR::GetCacheField(1), // s[1] = cache["some_var1"]
            DMIR::Test, // if s[1]
            DMIR::JZ("lbl".to_string()), // goto lbl
            DMIR::Pop, // else pop s[0]
            DMIR::GetCacheField(2), // s[0] = cache["some_var2"]
            DMIR::EnterBlock("lbl".to_string()), // lbl:
            DMIR::Test, // s[0] is either some_var0 or some_var2
            DMIR::End //
        );

        generate_ref_count_operations(&mut dmir);

        panic!("") // TODO: some asserts
    }

    #[test]
    #[ignore]
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

        panic!("") // TODO: some asserts
    }

    #[test]
    #[ignore]
    fn non_remove_ret() {

        let mut dmir = vec!(
            DMIR::GetSrc,
            DMIR::SetCache,
            DMIR::GetCacheField(0),
            DMIR::Ret,
            DMIR::End
        );

        generate_ref_count_operations(&mut dmir);

        panic!("") // TODO: some asserts
    }

    #[test]
    #[ignore]
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

        panic!("") // TODO: some asserts
    }
}