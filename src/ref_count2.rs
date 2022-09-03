use std::borrow::Borrow;
use std::cell::RefCell;
use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::env::var;
use itertools::Itertools;
use log::log;
use typed_arena::Arena;
use FlowVariableConsume::{Out, Unset};
use RefCountOp::ProduceUncounted;
use RefOpDisposition::{Post, Pre};
use crate::cfa::ControlFlowAnalyzer;
use crate::dfa;
use crate::dfa::{analyze_and_dump_dfa, DataFlowAnalyzer, DataFlowAnalyzerState, DFValueLocation, dump_dfa, FlowVariable, FlowVariableConsume, FlowVariableExpression, OperationEffect};
use crate::dmir::{DMIR, RefOpDisposition, ValueLocation};
use crate::dmir::DMIR::{DecRefCount, IncRefCount};
use crate::dmir::RefOpDisposition::DupPost;
use crate::dmir::ValueLocation::{Argument, Cache, Local, Stack};
use crate::inventory::iter;
use crate::ref_count2::RefCountOp::{Dec, Inc, Instruction, Split};


enum RefCountOpNode<'t> {
    Source(&'t FlowVariable<'t>),
    Op(&'t RefCountOpNode<'t>, usize)
}

pub fn generate_ref_count_operations2(
    ir: &mut Vec<DMIR>,
    parameter_count: usize,
) {

    let cfa = ControlFlowAnalyzer::new();
    let graph = cfa.analyze(ir);

    let mut analyzer = DataFlowAnalyzer::new(&graph);

    let (_, mut dfa_state) = analyzer.enter_function(parameter_count as u32);

    let mut ops = vec![];
    for (idx, instruction) in ir.iter().enumerate() {
        analyze_instruction(idx, instruction, &mut dfa_state, &mut ops);
    }

    let mut var_idx = VarIdx { var_id: Default::default(), next: 0 };


    // let arena = typed_arena::Arena::new();

    let mut ref_op_node_by_var = HashMap::new();

    fn bind_node_to_var<'t>(
        var: &'t FlowVariable<'t>,
        nodes: &mut HashMap<&'t FlowVariable<'t>, Vec<usize>>,
        idx: usize
    ) {
        let vec = nodes.entry(var).or_insert(Default::default());
        vec.push(idx);
    }

    for (idx, op) in ops.iter().enumerate() {
        match op {
            Instruction(_, effect) => {
                for var in effect.variables.iter() {
                    bind_node_to_var(unwrap_var(var), &mut ref_op_node_by_var, idx)
                }
                for consume in effect.consumes.iter() {
                    match consume {
                        Unset(var) => {
                            bind_node_to_var(unwrap_var(var), &mut ref_op_node_by_var, idx)
                        }
                        Out(var) => {
                            bind_node_to_var(unwrap_var(var), &mut ref_op_node_by_var, idx)
                        }
                    }
                }
            }
            Inc(var) => {
                bind_node_to_var(unwrap_var(var), &mut ref_op_node_by_var, idx)
            }
            Dec(var) => {
                bind_node_to_var(unwrap_var(var), &mut ref_op_node_by_var, idx)
            }
            ProduceUncounted(var) => {
                bind_node_to_var(unwrap_var(var), &mut ref_op_node_by_var, idx)
            }
            Split(block, vars) => {
                for var in vars.iter() {
                    bind_node_to_var(unwrap_var(var), &mut ref_op_node_by_var, idx)
                }
            }
        }
    }

    for key in ref_op_node_by_var.keys().sorted_by_key(|a| var_idx.var_id(a)) {
        let v = &ref_op_node_by_var[key];
        log::debug!("------- Operation for variable {}:", var_idx.var_id(key));
        for insn in v.iter() {
            print_instruction(&ops[*insn], ir, &mut var_idx)
        }
    }

    log::debug!("============================");

    for op in &ops {
        print_instruction(op, ir, &mut var_idx)
    }


    //
    //
    // let mut operations_by_ref = HashMap::new();
    //
    // for (idx, instruction) in ir.iter().enumerate() {
    //     log::trace!("inx({}): {:?}", idx, instruction);
    //     let effect = analyze_instruction(instruction);
    //     let pre_stack_size = data_flow_info[idx].stack_size;
    //     let post_stack_size = data_flow_info[idx + 1].stack_size;
    //
    //     let variables =
    //         data_flow_info[idx + 1].variables.iter().map(|var| (var.location.clone(), *var)).collect::<HashMap<_, _>>();
    //
    //     let consumes =
    //         data_flow_info[idx + 1].consumes.iter().map(|consume| (consume.var().location.clone(), *consume)).collect::<HashMap<_, _>>();
    //
    //     for ref_count_op in effect.operations.iter() {
    //         log::trace!("op: {:?}", ref_count_op);
    //         match ref_count_op {
    //             Inc(disposition) => {
    //
    //                 let location = disposition.to_df_location(pre_stack_size, post_stack_size);
    //
    //                 log::trace!("location is: {:?}", location);
    //                 operations_by_ref
    //                     .entry(FlowNodeRef::Variable(variables[&location]))
    //                     .or_insert(vec![]).push(ref_count_op.clone())
    //             }
    //             Dec(disposition) => {
    //                 let location = disposition.to_df_location(pre_stack_size, post_stack_size);
    //
    //                 if let Some(consume) = consumes.get(&location) {
    //                     log::trace!("location is: {:?}", location);
    //                     operations_by_ref
    //                         .entry(FlowNodeRef::Consume(consume))
    //                         .or_insert(vec![]).push(ref_count_op.clone())
    //                 }
    //             }
    //         }
    //     }
    // }


}

fn print_instruction<'t>(op:&RefCountOp<'t>, ir: &Vec<DMIR>, var_idx: &mut VarIdx<'t>){
    match op {
        Instruction(idx, effect) => {
            let vars = effect.variables.iter().map(|var| format!("v{}", var_idx.var_id(var))).join(", ");
            if !effect.consumes.is_empty() {
                let reads = effect.consumes.iter().filter(|v|
                    matches!(v, FlowVariableConsume::Out(_))
                ).map(|v| {
                    match v {
                        Out(var) => {
                            format!("v{}", var_idx.var_id(var))
                        }
                        _ => panic!("unexpected")
                    }
                }).join(", ");
                log::debug!("// Reads: {}", reads);
            }
            log::debug!("Instruction({}): {:?} -- {}", idx, ir[*idx], vars);
        }
        Inc(var) => {
            log::debug!("Inc(v{})", var_idx.var_id(var))
        }
        Dec(var) => {
            log::debug!("Dec(v{})", var_idx.var_id(var))
        }
        ProduceUncounted(var) => {
            log::debug!("Produce(v{})", var_idx.var_id(var))
        }
        Split(name, vars) => {
            let v = vars.iter().map(|var| format!("v{}", var_idx.var_id(var))).join(", ");
            log::debug!("Split({}) -- {}", name, v);
        }
    }
}


fn unwrap_var<'t>(var: &'t FlowVariable<'t>) -> &'t FlowVariable<'t> {
    match &var.expression {
        FlowVariableExpression::Variable(other) => {
            unwrap_var(other)
        }
        FlowVariableExpression::Phi(_) => {
            var
        }
        FlowVariableExpression::In => { var }
    }
}

struct VarIdx<'t> {
    var_id: HashMap<&'t FlowVariable<'t>, usize>,
    next: usize
}

impl <'t> VarIdx<'t> {
    fn var_id(&mut self, var: &'t FlowVariable<'t>) -> usize {
        let next_id = &mut self.next;
        let v = unwrap_var(var);
        *self.var_id.entry(v).or_insert_with(|| {
            let id = *next_id;
            *next_id += 1;
            id
        })
    }
}

impl<'t> FlowVariableConsume<'t> {
    fn var(&self) -> &'t FlowVariable<'t> {
        match self {
            Unset(var) => *var,
            Out(var) => *var
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
enum FlowNodeRef<'t> {
    Variable(&'t FlowVariable<'t>),
    Consume(&'t FlowVariableConsume<'t>)
}

impl RefOpDisposition {
    fn to_df_location(&self, pre_stack_size: usize, post_stack_size: usize) -> DFValueLocation {
        match self {
            DupPost(location) => location.to_df_location(pre_stack_size),
            Post(location) => location.to_df_location(post_stack_size),
            Pre(location) => location.to_df_location(pre_stack_size)
        }
    }
}

impl ValueLocation {
    fn to_df_location(&self, stack_size: usize) -> DFValueLocation {
        match self {
            Stack(position) => DFValueLocation::Stack(stack_size as u8 - 1 - *position),
            Cache => DFValueLocation::Cache,
            Local(idx) => DFValueLocation::Local(*idx),
            Argument(idx) => DFValueLocation::Argument(*idx),
        }
    }
}


enum RefCountOp<'t> {
    Instruction(usize, OperationEffect<'t>),
    Inc(&'t FlowVariable<'t>),
    Dec(&'t FlowVariable<'t>),
    ProduceUncounted(&'t FlowVariable<'t>),
    Split(String, Vec<&'t FlowVariable<'t>>),
}


fn analyze_instruction<'t>(
    ir_idx: usize,
    instruction: &DMIR,
    dfa: &mut DataFlowAnalyzerState<'t, '_>,
    out: &mut Vec<RefCountOp<'t>>
) {
    macro_rules! instruction_body {
        () => {
            out.push(Instruction(ir_idx, dfa.analyze_instruction(instruction)));
        };
    }

    match instruction {
        DMIR::GetLocal(idx) => {
            instruction_body!();
            out.push(Inc(dfa.stack_top(0)));
        }
        DMIR::SetLocal(idx) => {
            if let Some(local) = dfa.state().locals.get(idx) {
                out.push(Dec(local));
            }
            instruction_body!();
        },
        DMIR::GetSrc => {
            instruction_body!();
            out.push(Inc(dfa.stack_top(0)));
        },
        DMIR::GetArg(_) => {
            instruction_body!();
            out.push(Inc(dfa.stack_top(0)));
        },
        DMIR::SetArg(idx) => {
            if let Some(argument) = dfa.state().arguments.get(*idx as usize) {
                out.push(Dec(argument));
            }
            instruction_body!();
        }
        DMIR::FloatAdd | DMIR::FloatSub |
        DMIR::FloatMul | DMIR::FloatDiv => {
            let a = dfa.stack_top(0);
            let b = dfa.stack_top(1);
            instruction_body!();
            out.push(Dec(a));
            out.push(Dec(b));
            out.push(ProduceUncounted(dfa.stack_top(0)));
        },
        DMIR::SetCache => {
            if let Some(cache) = dfa.state().cache {
                out.push(Dec(cache));
            }
            instruction_body!();
        }
        DMIR::GetCacheField(_) => {
            instruction_body!();
            out.push(Inc(dfa.stack_top(0)));
        }
        DMIR::SetCacheField(_) => {
            // TODO Q?
            let value = dfa.stack_top(0);
            instruction_body!();
            out.push(Dec(value));
        }
        DMIR::PushCache => {
            instruction_body!();
            out.push(Inc(dfa.stack_top(0)));
        }
        DMIR::PushVal(_) => {
            instruction_body!();
            out.push(Inc(dfa.stack_top(0)));
        }

        DMIR::CallProcById(_, _, _) | DMIR::CallProcByName(_, _, _) => {
            let stack_top = dfa.stack_top(0);
            instruction_body!();
            out.push(Dec(stack_top))
        }
        DMIR::Pop => {
            out.push(Dec(dfa.stack_top(0)));
            instruction_body!();
        }
        DMIR::End => {
            if let Some(cache) = &dfa.state().cache {
                out.push(Dec(cache));
            }
            for (_, local) in dfa.state().locals.iter() {
                out.push(Dec(local));
            }
            for arg in dfa.state().arguments.iter() {
                out.push(Dec(arg));
            }
            instruction_body!();
        }
        DMIR::UnsetCache => {
            if let Some(cache) = &dfa.state().cache {
                out.push(Dec(cache));
            }
            instruction_body!();
        }
        DMIR::UnsetLocal(idx) => {
            out.push(Dec(&dfa.state().locals[idx]));
            instruction_body!();
        }
        DMIR::JZ(block) |
        DMIR::JZInternal(block) |
        DMIR::JNZInternal(block) => {
            let mut vars: Vec<&FlowVariable> = vec![];
            if let Some(cache) = &dfa.state().cache {
                vars.push(cache);
            }
            for (_, local) in dfa.state().locals.iter() {
                vars.push(local);
            }
            for arg in dfa.state().arguments.iter() {
                vars.push(arg);
            }
            out.push(Split(block.clone(), vars));
            instruction_body!();
        }
        _ => {
            instruction_body!();
        }
    }
}