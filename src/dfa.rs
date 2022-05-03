use std::cell::RefCell;
use std::collections::HashMap;
use typed_arena::Arena;
use crate::dmir::{DMIR, ValueLocation};
use ValueLocation::*;
use FlowVariableExpression::*;

/// This module provides data-flow analysis capability

#[derive(Clone)]
struct InterpreterState<'t> {
    arguments: Vec<&'t FlowVariable<'t>>,
    stack: Vec<&'t FlowVariable<'t>>,
    locals: HashMap<u32, &'t FlowVariable<'t>>,
    cache: Option<&'t FlowVariable<'t>>,
}

impl InterpreterState<'_> {
    fn new() -> Self {
        Self {
            arguments: vec![],
            stack: vec![],
            locals: Default::default(),
            cache: None,
        }
    }
}

enum FlowVariableExpression<'t> {
    /// Re-assign of different variable
    Variable(&'t FlowVariable<'t>),
    /// Merge of different variables in SSA
    Phi(RefCell<Vec<&'t FlowVariable<'t>>>),
    /// Opaque read, for example return of call
    In,
}

struct FlowVariable<'t> {
    location: ValueLocation,
    expression: FlowVariableExpression<'t>,
}

use std::hash::{Hash, Hasher};
use itertools::Itertools;
use crate::cfa::{ControlFlowAnalyzer, ControlFlowGraph};
use crate::dmir_annotate::Annotator;
use crate::ref_count::ref_identity;
ref_identity!(FlowVariable<'_>);

enum FlowVariableConsume<'t> {
    /// Consume without using variable
    Unset(&'t FlowVariable<'t>),
    /// Generic opaque write, for example write to global variable
    Out(&'t FlowVariable<'t>),
}

struct OperationEffect<'t> {
    variables: Vec<&'t FlowVariable<'t>>,
    consumes: Vec<FlowVariableConsume<'t>>,
}

struct DataFlowAnalyzer<'t, 'graph> {
    arena: Arena<FlowVariable<'t>>,
    control_flow_graph: &'graph ControlFlowGraph<'graph>
}

impl<'t, 'graph> DataFlowAnalyzer<'t, 'graph> {
    fn new(control_flow_graph: &'graph ControlFlowGraph<'graph>) -> Self {
        Self {
            arena: Arena::new(),
            control_flow_graph
        }
    }

    fn update_state<'q>(variable: &'q FlowVariable<'q>, state: &mut InterpreterState<'q>) {
        match &variable.location {
            Stack(idx) => {
                if *idx as usize == state.stack.len() {
                    state.stack.push(variable)
                } else {
                    state.stack[*idx as usize] = variable;
                }
            }
            Cache => {
                state.cache = Option::Some(variable);
            }
            Local(idx) => {
                assert!(state.locals.insert(*idx, variable).is_none());
            }
            Argument(idx) => {
                state.arguments[*idx as usize] = variable;
            }
        }
    }

    fn analyze(&'t mut self, instructions: &Vec<DMIR>, argument_count: u32) -> Vec<OperationEffect<'t>> {
        let mut result = Vec::new();
        let mut state = InterpreterState::new();
        let mut blocks = HashMap::new();

        for idx in 0..argument_count {
            state.arguments.push(
                self.arena.alloc(
                    FlowVariable {
                        location: ValueLocation::Argument(idx),
                        expression: FlowVariableExpression::In
                    }
                )
            );
        }
        result.push(OperationEffect {
            variables: state.arguments.clone(),
            consumes: vec![]
        });

        for instruction in instructions {
            result.push(self.analyze_instruction(&mut state, &mut blocks, instruction));
        }

        return result;
    }

    fn epilogue_effect(state: &mut InterpreterState<'t>, effect: &mut OperationEffect<'t>) {
        for (_, var) in state.locals.drain() {
            effect.consumes.push(FlowVariableConsume::Unset(var));
        }
        for var in state.arguments.drain(..) {
            effect.consumes.push(FlowVariableConsume::Unset(var));
        }
        if let Some(var) = &state.cache.take() {
            effect.consumes.push(FlowVariableConsume::Unset(var));
        }
    }

    fn merge_phi(target: &FlowVariable<'t>, source: &'t FlowVariable<'t>) {
        match &target.expression {
            Phi(sources) => {
                sources.borrow_mut().push(source);
            }
            _ => panic!("unexpected target for Phi merging")
        }
    }
    fn make_phi(&'t self, source: &'t FlowVariable<'t>) -> &'t FlowVariable<'t> {
        self.arena.alloc(
            FlowVariable {
                location: source.location.clone(),
                expression: FlowVariableExpression::Phi(RefCell::new(vec![source]))
            }
        )
    }

    fn block_has_single_predecessor(&self, label: &str) -> bool {
        return self.control_flow_graph.nodes.get(label).unwrap().inbound.borrow().len() < 2;
    }

    fn merge_block(
        &'t self,
        current_state: &InterpreterState<'t>,
        blocks: &mut HashMap<String, InterpreterState<'t>>,
        label: &str
    ) {
        let has_single_predecessor = self.block_has_single_predecessor(label);
        if has_single_predecessor {
            assert!(blocks.insert(label.to_owned(), current_state.clone()).is_none())
        } else {
            blocks.entry(label.to_owned())
                .and_modify(|state| {
                    state.arguments.iter().zip(current_state.arguments.iter()).for_each(|(target, source)| Self::merge_phi(target, source));
                    state.stack.iter().zip(current_state.stack.iter()).for_each(|(target, source)| Self::merge_phi(target, source));
                    assert_eq!(state.locals.len(), current_state.locals.len());
                    for (idx, var) in current_state.locals.iter() {
                        Self::merge_phi(state.locals[idx], var);
                    }
                    state.cache.iter().zip(current_state.cache.iter()).for_each(|(target, source)| Self::merge_phi(target, source));
                })
                .or_insert_with(|| {
                    InterpreterState {
                        arguments: current_state.arguments.iter().map(|var| self.make_phi(var)).collect(),
                        stack: current_state.stack.iter().map(|var| self.make_phi(var)).collect(),
                        locals: current_state.locals.iter().map(|(idx, var)| (*idx, self.make_phi(var))).collect(),
                        cache: current_state.cache.map(|var| self.make_phi(var))
                    }
                });
        }
    }

    fn analyze_instruction<'q>(
        &'t self,
        state: &'q mut InterpreterState<'t>,
        blocks: &'q mut HashMap<String, InterpreterState<'t>>,
        instruction: &'q DMIR
    ) -> OperationEffect<'t> {
        let mut effect = OperationEffect { variables: vec![], consumes: vec![] };

        macro_rules! mk_var {
            ($loc:expr => $expr:expr) => {
                let v = self.arena.alloc(
                    FlowVariable {
                        location: $loc,
                        expression: $expr
                    }
                );
                Self::update_state(v, state);
                effect.variables.push(v);
            };
        }
        macro_rules! stack_top {
            () => { stack_top!(0) };
            ($loc:expr) => { ValueLocation::Stack((state.stack.len() - $loc) as u8) };
        }
        macro_rules! unset {
            ($var:expr) => {
                effect.consumes.push(
                    FlowVariableConsume::Unset($var)
                )
            };
        }
        macro_rules! out {
            ($var:expr) => {
                effect.consumes.push(
                    FlowVariableConsume::Out($var)
                )
            };
        }

        match instruction {
            DMIR::GetLocal(idx) => {
                mk_var!(stack_top!() => Variable(state.locals[idx]));
            }
            DMIR::SetLocal(idx) => {
                if let Some(var) = state.locals.remove(idx) {
                    unset!(var)
                }
                mk_var!(Local(*idx) => Variable(state.stack.pop().unwrap()));
            }
            DMIR::GetSrc => {
                mk_var!(stack_top!() => In);
            }
            DMIR::GetArg(idx) => {
                mk_var!(stack_top!() => Variable(state.arguments[*idx as usize]));
            }
            DMIR::SetArg(idx) => {
                unset!(state.arguments[*idx as usize]);
                mk_var!(Argument(*idx) => Variable(state.stack.pop().unwrap()));
            }
            DMIR::SetCache => {
                if let Some(var) = state.cache.take() {
                    unset!(var)
                }
                mk_var!(Cache => Variable(state.stack.pop().unwrap()));
            }
            DMIR::GetCacheField(_) => {
                out!(state.cache.unwrap());
                mk_var!(stack_top!() => In);
            }
            DMIR::SetCacheField(_) => {
                out!(state.cache.unwrap());
                out!(state.stack.pop().unwrap());
            }
            DMIR::PushCache => {
                mk_var!(stack_top!() => Variable(state.cache.unwrap()));
            }
            DMIR::ValueTagSwitch(_, cases) => {
                for (_, label) in cases {
                    self.merge_block(
                        state,
                        blocks,
                        label
                    );
                }
            }
            DMIR::RoundN | DMIR::FloatCmp(_) | DMIR::FloatAdd | DMIR::FloatSub | DMIR::FloatMul | DMIR::FloatDiv => {
                out!(state.stack.pop().unwrap());
                out!(state.stack.pop().unwrap());
                mk_var!(stack_top!() => In);
            }
            DMIR::FloatAbs => {
                out!(state.stack.pop().unwrap());
                mk_var!(stack_top!() => In);
            }
            DMIR::FloatInc | DMIR::FloatDec => {
                out!(state.stack.pop().unwrap());
                mk_var!(stack_top!() => In);
            }
            DMIR::BitAnd | DMIR::BitOr => {
                out!(state.stack.pop().unwrap());
                out!(state.stack.pop().unwrap());
                mk_var!(stack_top!() => In);
            }
            DMIR::ListCheckSizeDeopt(_, _, _) => {}
            DMIR::ListCopy => {
                out!(state.stack.pop().unwrap());
                mk_var!(stack_top!() => In);
            }
            DMIR::ListSubSingle | DMIR::ListAddSingle => {
                out!(state.stack.pop().unwrap());
                out!(state.stack.pop().unwrap());
            }
            DMIR::ListAssociativeGet | DMIR::ListIndexedGet => {
                out!(state.stack.pop().unwrap());
                out!(state.stack.pop().unwrap());
                mk_var!(stack_top!() => In);
            }
            DMIR::ListAssociativeSet | DMIR::ListIndexedSet => {
                out!(state.stack.pop().unwrap());
                out!(state.stack.pop().unwrap());
                out!(state.stack.pop().unwrap());
            }
            DMIR::NewVectorList(size) => {
                for _ in 0..*size {
                    out!(state.stack.pop().unwrap());
                }
                mk_var!(stack_top!() => In);
            }
            DMIR::NewAssocList(size, _) => {
                for _ in 0..*size {
                    out!(state.stack.pop().unwrap());
                    out!(state.stack.pop().unwrap());
                }
                mk_var!(stack_top!() => In);
            }
            DMIR::ArrayIterLoadFromObject(_) | DMIR::ArrayIterLoadFromList(_) => {
                out!(state.stack.pop().unwrap());
            }
            DMIR::IterAllocate => {}
            DMIR::IterPop => {}
            DMIR::IterPush => {}
            DMIR::IterNext => {
                mk_var!(stack_top!() => In);
            }
            DMIR::GetStep => {
                out!(state.stack.pop().unwrap());
                out!(state.stack.pop().unwrap());
                mk_var!(stack_top!() => In);
            }
            DMIR::PushInt(_) | DMIR::PushVal(_) => {
                mk_var!(stack_top!() => In);
            }
            DMIR::PushTestFlag => {
                mk_var!(stack_top!() => In);
            }
            DMIR::SetTestFlag(_) => {}
            DMIR::Pop => {
                unset!(state.stack.pop().unwrap());
            }
            DMIR::Ret => {
                out!(state.stack.pop().unwrap());
                Self::epilogue_effect(state, &mut effect);
            }
            DMIR::Not => {
                out!(state.stack.pop().unwrap());
                mk_var!(stack_top!() => In);
            }
            DMIR::Test => {
                out!(state.stack.pop().unwrap());
            }
            DMIR::TestEqual => {
                out!(state.stack.pop().unwrap());
                out!(state.stack.pop().unwrap());
            }
            DMIR::TestIsDMEntity => {
                out!(state.stack.pop().unwrap());
            }
            DMIR::IsSubtypeOf => {
                out!(state.stack.pop().unwrap());
                out!(state.stack.pop().unwrap());
                mk_var!(stack_top!() => In);
            }
            DMIR::JZ(label) => {
                self.merge_block(
                    state,
                    blocks,
                    label
                );
            }
            DMIR::Dup => {
                let stack_top = state.stack.pop().unwrap();
                mk_var!(stack_top!() => Variable(stack_top));
                mk_var!(stack_top!() => Variable(stack_top));
            }
            DMIR::DupX1 => {
                let b = state.stack.pop().unwrap();
                let a = state.stack.pop().unwrap();
                mk_var!(stack_top!() => Variable(b));
                mk_var!(stack_top!() => Variable(a));
                mk_var!(stack_top!() => Variable(b));
            }
            DMIR::DupX2 => {
                let c = state.stack.pop().unwrap();
                let b = state.stack.pop().unwrap();
                let a = state.stack.pop().unwrap();
                mk_var!(stack_top!() => Variable(c));
                mk_var!(stack_top!() => Variable(a));
                mk_var!(stack_top!() => Variable(b));
                mk_var!(stack_top!() => Variable(c));
            }
            DMIR::Swap => {
                let b = state.stack.pop().unwrap();
                let a = state.stack.pop().unwrap();

                mk_var!(stack_top!() => Variable(b));
                mk_var!(stack_top!() => Variable(a));
            }
            DMIR::SwapX1 => {
                let c = state.stack.pop().unwrap();
                let b = state.stack.pop().unwrap();
                let a = state.stack.pop().unwrap();

                mk_var!(stack_top!() => Variable(b));
                mk_var!(stack_top!() => Variable(c));
                mk_var!(stack_top!() => Variable(a));
            }
            DMIR::TestInternal => {
                out!(state.stack.pop().unwrap());
            }
            DMIR::Jmp(label) | DMIR::JZInternal(label) | DMIR::JNZInternal(label) => {
                self.merge_block(state, blocks, label);
            }
            DMIR::EnterBlock(lbl) => {
                let block = blocks.get(lbl).expect("Block not initialized");
                // replace state completely
                *state = block.clone();
                if !self.block_has_single_predecessor(lbl) {
                    effect.variables.append(
                        &mut block.arguments.clone()
                    );
                    effect.variables.extend(
                        block.stack.iter()
                    );
                    effect.variables.extend(
                        block.locals.values()
                    );
                    if let Some(var) = block.cache {
                        effect.variables.push(var);
                    };
                }
            }
            DMIR::InfLoopCheckDeopt(_) => {}
            DMIR::Deopt(_, _) => {}
            DMIR::CheckTypeDeopt(_, _, _) => {}
            DMIR::CallProcByName(_, _, arg_count) | DMIR::CallProcById(_, _, arg_count) => {
                let src = state.stack.pop().unwrap();
                out!(src);
                for _ in 0..*arg_count {
                    out!(state.stack.pop().unwrap());
                }
                mk_var!(stack_top!() => In);
            }
            DMIR::IncRefCount { .. } => {} // todo
            DMIR::DecRefCount { .. } => {} // todo
            DMIR::Nop => {}
            DMIR::UnsetLocal(idx) => unset!(state.locals.remove(idx).unwrap()),
            DMIR::UnsetCache => unset!(state.cache.take().unwrap()),
            DMIR::End => {
                Self::epilogue_effect(state, &mut effect);
            }
        }
        effect
    }
}

pub fn analyze_and_dump_dfa(instructions: &Vec<DMIR>, argument_count: u32) {
    let cfa = ControlFlowAnalyzer::new();
    let graph = cfa.analyze(&instructions);

    let mut analyzer = DataFlowAnalyzer::new(&graph);

    let r = analyzer.analyze(
        &instructions,
        argument_count
    );

    let mut next_var_id = 0u32;
    let mut var_ids = HashMap::new();
    for effect in &r {
        for variable in &effect.variables {
            if !var_ids.contains_key(variable) {
                var_ids.insert(variable, next_var_id);
                next_var_id += 1
            }
        }
    }

    let mut annotator = Annotator::new();


    for (idx, effect) in r.iter().skip(1).enumerate() {
        for consume in &effect.consumes {
            let str = match consume {
                FlowVariableConsume::Unset(var) => {
                    format!("unset %{}", var_ids[var])
                }
                FlowVariableConsume::Out(var) => {
                    format!("out %{}", var_ids[var])
                }
            };
            annotator.add(idx, str)
        }
        for variable in &effect.variables {
            let str = match &variable.expression {
                FlowVariableExpression::Variable(other) => format!("%{} = %{}", var_ids[&variable], var_ids[other]),
                FlowVariableExpression::Phi(other) => format!("%{} = φ[{}]", var_ids[&variable], other.borrow().iter().map(|var| format!("%{}", var_ids[var])).join(", ")),
                FlowVariableExpression::In => format!("%{} = in", var_ids[&variable])
            };
            annotator.add(idx, str)
        }
    }

    annotator.dump_annotated(&instructions);
}

mod tests {
    use std::borrow::Borrow;
    use std::collections::HashMap;
    use auxtools::raw_types::procs::ProcId;
    use dmasm::format;
    use dmasm::operands::ValueOpRaw;
    use itertools::Itertools;
    use libc::fopen;
    use log::LevelFilter;
    use crate::cfa::ControlFlowAnalyzer;
    use crate::dfa::{analyze_and_dump_dfa, DataFlowAnalyzer, FlowVariable, FlowVariableConsume, FlowVariableExpression};
    use crate::dmir::DMIR;
    use crate::dmir_annotate::Annotator;

    #[test]
    fn test_dfa_unbalanced_wtf() {
        simple_logging::log_to_stderr(LevelFilter::Trace);


        fn push_null() -> DMIR {
            DMIR::PushVal(ValueOpRaw { tag: 0, data: 0 })
        }
        let instructions = vec![
            push_null(),
            DMIR::CallProcById(ProcId(0), 0, 0),
            DMIR::SetLocal(0),
            DMIR::GetLocal(0),
            DMIR::SetLocal(1),
            push_null(),
            DMIR::SetLocal(0),
            DMIR::GetLocal(1),
            push_null(),
            DMIR::CallProcById(ProcId(1), 0, 1),
            DMIR::Pop,
            DMIR::End,
        ];

        analyze_and_dump_dfa(&instructions, 0);
        //panic!();
    }
}