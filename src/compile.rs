use std::cmp::max;
use std::marker::PhantomPinned;
use std::mem::transmute_copy;
use std::pin::Pin;
use std::ptr::NonNull;

use auxtools::{Proc, Value};
use auxtools::DMResult;
use auxtools::raw_types::procs::ProcId;
use auxtools::raw_types::values::ValueTag;
use dmasm::format_disassembly;
use inkwell::attributes::AttributeLoc;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::OptimizationLevel;
use inkwell::passes::PassManager;
use inkwell::values::AnyValue;

use crate::{ByondProcFunc, chad_hook_by_id, DisassembleEnv, dmir, guard};
use crate::codegen::CodeGen;
use crate::dmir::DMIR;
use crate::ref_count::generate_ref_count_operations;
use crate::variable_termination_pass::variable_termination_pass;

#[hook("/proc/dmjit_compile_proc")]
pub fn compile_and_call(proc_name: auxtools::Value) -> DMResult {
    guard(|| {
        LLVM_CONTEXT.with(|val| {
            let mut override_id = 0;
            let base_proc = match proc_name.raw.tag {
                ValueTag::String => {
                    Proc::find(proc_name.as_string().unwrap())
                }
                ValueTag::ProcId => {
                    Proc::from_id(ProcId(unsafe { proc_name.raw.data.id }))
                }
                _ => Option::None
            };

            let name = if let Some(base_proc) = base_proc {
                base_proc.path
            } else {
                return
            };

            loop {
                if let Some(proc) = Proc::find_override(&name, override_id) {
                    compile_proc(
                        unsafe { val.context_ref.as_ref() },
                        val.module.as_ref().unwrap(),
                        val.execution_engine.as_ref().unwrap(),
                        proc
                    );
                    override_id += 1;
                } else {
                    break
                }
            }
        });

        DMResult::Ok(Value::null())
    })
}


#[hook("/proc/dmjit_install_compiled")]
pub fn install_hooks() -> DMResult {
    guard(|| {
        let mut installed: Vec<String> = vec!();
        LLVM_CONTEXT.with(|val| {
            let execution_engine = val.execution_engine.as_ref().unwrap();
            let module = val.module.as_ref().unwrap();

            log::info!("Module {}", module.print_to_string().to_string());

            let mut curr_function = module.get_first_function();
            while curr_function.is_some() {
                if let Some(func_value) = curr_function {
                    let name = func_value.get_name().to_str().unwrap();

                    if !name.starts_with("<intrinsic>/") && func_value.get_intrinsic_id() == 0 {
                        log::info!("installing {}", name);
                        installed.push(name.to_string());
                        let func: ByondProcFunc = unsafe {
                            transmute_copy(&execution_engine.get_function_address(name).unwrap())
                        };

                        log::info!("target is {} at {:?}", name, func as (*mut ()));

                        let proc_id_attrib = func_value.get_string_attribute(AttributeLoc::Function, "proc_id").unwrap();
                        // TODO: cleanup
                        let proc_id = auxtools::raw_types::procs::ProcId(proc_id_attrib.get_string_value().to_string_lossy().parse::<u32>().unwrap());
                        let proc = Proc::from_id(proc_id).unwrap();
                        chad_hook_by_id(proc.id, func);
                    }
                }

                curr_function = curr_function.unwrap().get_next_function();
            }
        });


        Value::from_string(installed.join(", "))
    })
}


struct ModuleContext<'ctx> {
    context: Context,
    context_ref: NonNull<Context>,
    module: Option<Module<'ctx>>,
    execution_engine: Option<ExecutionEngine<'ctx>>,
    _pin: PhantomPinned
}

impl<'ctx> Drop for ModuleContext<'ctx> {
    fn drop(&mut self) {
        self.execution_engine = Option::None;
        self.module = Option::None;
        drop(self.context_ref);
    }
}

impl ModuleContext<'static> {

    fn new() -> Pin<Box<ModuleContext<'static>>> {
        let s = ModuleContext {
            context: Context::create(),
            context_ref: NonNull::dangling(),
            module: None,
            execution_engine: None,
            _pin: PhantomPinned
        };

        let mut boxed = Box::pin(s);

        let context_ref = NonNull::from(&boxed.context);

        let module = unsafe { context_ref.as_ref() }.create_module("dmir");
        let execution_engine = module.create_jit_execution_engine(OptimizationLevel::Default).unwrap();


        log::info!("Initialize ModuleContext");

        unsafe {
            let mut_ref: Pin<&mut Self> = Pin::as_mut(&mut boxed);
            let ctx = Pin::get_unchecked_mut(mut_ref);
            ctx.module = Some(module);
            ctx.execution_engine = Some(execution_engine);
            ctx.context_ref = context_ref;
        }

        boxed
    }
}

thread_local! {
    static LLVM_CONTEXT: Pin<Box<ModuleContext<'static>>> = ModuleContext::new()
}

fn compile_proc<'ctx>(
    context: &'static Context,
    module: &'ctx Module<'static>,
    execution_engine: &'ctx ExecutionEngine<'static>,
    proc: auxtools::Proc
) {
    log::info!("Trying compile {}", proc.path);

    // Take bytecode of proc to compile
    let bc = unsafe { proc.bytecode() };

    let mut env = DisassembleEnv {};
    // disassemble DM asm
    let (nodes, res) = dmasm::disassembler::disassemble(bc, &mut env);

    log::debug!("{}", format_disassembly(&nodes, None));

    if let Some(res) = res {
        panic!("{:?}", res);
    }

    let mut irs = dmir::decode_byond_bytecode(nodes, proc.clone()).unwrap();

    log::debug!("DMIR created");
    variable_termination_pass(&mut irs);
    log::debug!("variable_termination_pass done");
    generate_ref_count_operations(&mut irs);
    log::debug!("ref_count_pass done");

    fn compute_max_sub_call_arg_count(ir: &DMIR, max_sub_call_arg_count: &mut u32) {
        match ir {
            DMIR::CallProcById(_, _, arg_count) | DMIR::CallProcByName(_, _, arg_count) => {
                *max_sub_call_arg_count = max(*max_sub_call_arg_count, *arg_count);
            }
            DMIR::IncRefCount { target: _, op } |
            DMIR::DecRefCount { target: _, op } |
            DMIR::InfLoopCheckDeopt(op) |
            DMIR::ListCheckSizeDeopt(_, _, op) |
            DMIR::CheckTypeDeopt(_, _, op) => {
                compute_max_sub_call_arg_count(op, max_sub_call_arg_count);
            }
            _ => {}
        }
    }

    let mut max_sub_call_arg_count = 0;
    for ir in &irs {
        compute_max_sub_call_arg_count(ir, &mut max_sub_call_arg_count);
    }
    // Prepare LLVM internals for code-generation
    let mut code_gen = CodeGen::create(
        context,
        &module,
        context.create_builder(),
        execution_engine,
        proc.parameter_names().len() as u32,
        proc.local_names().len() as u32
    );
    code_gen.init_builtins();

    let func = code_gen.create_jit_func(proc.path.as_str());

    let proc_id_attr = context.create_string_attribute("proc_id", format!("{}", proc.id.0).as_str());
    func.add_attribute(AttributeLoc::Function, proc_id_attr);

    code_gen.emit_prologue(func, max_sub_call_arg_count);
    // Emit LLVM IR nodes from DMIR
    for ir in irs {
        log::debug!("emit: {:?}", &ir);
        code_gen.emit(&ir, func);
    }

    log::info!("{}", func.print_to_string().to_string());
    let verify = func.verify(true);

    if let Err(err) = code_gen.module.verify() {
        log::error!("err: {}", err.to_string());
    }

    log::info!("Compiled {}, installing {}", verify, proc.path);

    let fpm = PassManager::create(code_gen.module);

    fpm.add_early_cse_mem_ssa_pass();
    fpm.add_loop_idiom_pass();
    fpm.add_licm_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_scalar_repl_aggregates_pass();
    fpm.add_bit_tracking_dce_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();

    fpm.initialize();

    let res = fpm.run_on(&func);
    log::info!("OPT -- {}\n{}", res, func.print_to_string().to_string());

}
