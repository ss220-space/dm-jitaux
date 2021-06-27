use auxtools::{Value, Proc};
use dmasm::{Instruction, format_disassembly, DebugData};
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine};
use inkwell::builder::Builder;
use inkwell::module::{Module, Linkage};
use inkwell::values::{IntValue, FunctionValue, PointerValue, AnyValue, StructValue, BasicValueEnum};
use inkwell::types::StructType;
use std::collections::HashMap;
use dmasm::operands::Variable;
use std::borrow::Borrow;
use std::ffi::{CString, CStr, c_void};
use inkwell::{OptimizationLevel, AddressSpace, IntPredicate, FloatPredicate};
use crate::{DisassembleEnv, guard};
use inkwell::basic_block::BasicBlock;
use std::mem::transmute_copy;
use std::collections::hash_map::Entry;
use inkwell::passes::PassManager;
use std::ptr::{NonNull, drop_in_place};
use std::marker::PhantomPinned;
use std::pin::Pin;
use inkwell::AddressSpace::Generic;
use inkwell::debug_info::{DILocation, DebugInfoBuilder, DWARFSourceLanguage};
use auxtools::raw_types::values::{ValueTag, ValueData};
use auxtools::raw_types::procs::{ExecutionContext, ProcInstance, ProcId};
use auxtools::raw_types::funcs::CURRENT_EXECUTION_CONTEXT;
use auxtools::raw_types::strings::StringId;
use crate::deopt::deopt;

#[hook("/proc/compile_proc")]
pub fn compile_and_call(proc_name: auxtools::Value) {
    guard(|| {
        LLVM_CONTEXT.with(|val| {
            compile_proc(
                unsafe { val.context_ref.as_ref() },
                val.module.as_ref().unwrap(),
                val.execution_engine.as_ref().unwrap(),
                Proc::find(proc_name.as_string().unwrap()).unwrap()
            );
        });

        Result::Ok(Value::null())
    })
}


#[hook("/proc/install_compiled")]
pub fn install_hooks() {
    guard(|| {
        let mut installed: Vec<String> = vec!();
        LLVM_CONTEXT.with(|val| {
            let execution_engine = val.execution_engine.as_ref().unwrap();
            let module = val.module.as_ref().unwrap();

            log::info!("Module {}", module.print_to_string().to_string());

            let mut curr_function = module.get_first_function();
            while curr_function.is_some() {
                if let Some(func) = curr_function {
                    let name = func.get_name().to_str().unwrap();

                    if !name.starts_with("<intrinsic>/") {
                        log::info!("installing {}", name);
                        installed.push(name.to_string());
                        let func: auxtools::hooks::ByondProcFunc = unsafe {
                            transmute_copy(&execution_engine.get_function_address(name).unwrap())
                        };

                        log::info!("target is {} at {:?}", name, func as (*mut ()));

                        auxtools::hooks::chad_hook(name, func).unwrap();
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

struct CodeGen<'ctx, 'a> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: &'a ExecutionEngine<'ctx>,
    stack_loc: Vec<PointerValue<'ctx>>,
    locals: HashMap<u32, PointerValue<'ctx>>,
    cache: Option<StructValue<'ctx>>,
    val_type: StructType<'ctx>,
    test_res: Option<IntValue<'ctx>>,
    block_map: HashMap<String, BasicBlock<'ctx>>,
    block_ended: bool
}


thread_local! {
    static LLVM_CONTEXT: Pin<Box<ModuleContext<'static>>> = ModuleContext::new()
}

struct MetaValue<'ctx> {
    tag: IntValue<'ctx>,
    data: BasicValueEnum<'ctx>
}

impl MetaValue<'_> {
    fn new<'a>(tag: IntValue<'a>, data: BasicValueEnum<'a>) -> MetaValue<'a> {
        return MetaValue {
            tag,
            data
        }
    }

    fn with_tag<'a>(tag: auxtools::raw_types::values::ValueTag, data: BasicValueEnum<'a>, code_gen: &mut CodeGen<'a, '_>) -> MetaValue<'a> {
        let tag = code_gen.context.i8_type().const_int(tag as u64, false);
        return Self::new(tag, data)
    }
}

impl<'ctx> CodeGen<'ctx, '_> {

    fn create<'a>(context: &'ctx Context, module: &'a Module<'ctx>, builder: Builder<'ctx>, execution_engine: &'a ExecutionEngine<'ctx>) -> CodeGen<'ctx, 'a> {
        CodeGen {
            context,
            module,
            builder,
            execution_engine,
            stack_loc: vec![],
            locals: HashMap::new(),
            cache: None,
            // type of BYOND operands, actually a struct { type: u8, value: u32 }
            val_type: context.struct_type(&[context.i8_type().into(), context.i32_type().into()], false),
            test_res: None,
            block_map: HashMap::new(),
            block_ended: false
        }
    }

    fn dbg(&mut self, str: &str) {
        let ptr = self.builder.build_global_string_ptr(str, "dbg_str").as_pointer_value();
        self.builder.build_call(self.module.get_function("<intrinsic>/debug").unwrap(), &[ptr.into()], "call_dbg");
    }

    fn dbg_val(&mut self, val: StructValue<'ctx>) {
        self.builder.build_call(self.module.get_function("<intrinsic>/debug_val").unwrap(), &[val.into()], "call_dbg");
    }


    fn emit_load_meta_value(&mut self, from: StructValue<'ctx>) -> MetaValue<'ctx> {
        let tag = self.builder.build_extract_value(from, 0, "get_tag").unwrap().into_int_value();
        let data = self.builder.build_extract_value(from, 1, "get_data").unwrap().into_int_value();
        return MetaValue::new(tag, data.into());
    }

    fn emit_store_meta_value(&mut self, from: MetaValue<'ctx>) -> StructValue<'ctx> {
        let out_val = self.val_type.const_zero();
        let out_val = self.builder.build_insert_value(out_val, from.tag, 0, "set_tag").unwrap().into_struct_value();
        let out_val = self.builder.build_insert_value(out_val, from.data, 1, "set_data").unwrap().into_struct_value();

        return out_val;
    }

    fn emit_bin_op<F>(&mut self, op: F)
        where F : FnOnce(MetaValue<'ctx>, MetaValue<'ctx>, &mut Self) -> MetaValue<'ctx>
    {
        let first = self.stack_loc.pop().unwrap();
        let second = self.stack_loc.pop().unwrap();

        let first_struct = self.builder.build_load(first, "first").into_struct_value();
        let first_meta = self.emit_load_meta_value(first_struct);

        let second_struct = self.builder.build_load(second, "second").into_struct_value();
        let second_meta = self.emit_load_meta_value(second_struct);

        let result_meta = op(first_meta, second_meta, self);

        let out = self.builder.build_alloca(self.val_type, "out");
        let out_val = self.emit_store_meta_value(result_meta);
        self.builder.build_store(out, out_val);

        self.stack_loc.push(out);
    }

    fn const_tag(&mut self, value_tag: ValueTag) -> IntValue<'ctx> {
        return self.context.i8_type().const_int(value_tag as u64, false).into()
    }

    fn emit_to_number_or_zero(&mut self, func: FunctionValue<'ctx>, value: MetaValue<'ctx>) -> MetaValue<'ctx> {
        let iff_number = self.context.append_basic_block(func,"iff_number");
        let next = self.context.append_basic_block(func, "next");

        let out_f32_ptr = self.builder.build_alloca(self.context.f32_type(), "second_f32_store");
        self.builder.build_store(out_f32_ptr, self.context.f32_type().const_zero());

        let number_tag = self.const_tag(ValueTag::Number);

        self.builder.build_conditional_branch(
            self.builder.build_int_compare(IntPredicate::EQ, value.tag, number_tag, "check_number"),
            iff_number,
            next
        );
        {
            self.builder.position_at_end(iff_number);
            let number_value = self.builder.build_bitcast(value.data, self.context.f32_type(), "cast_float");
            self.builder.build_store(out_f32_ptr, number_value);
            self.builder.build_unconditional_branch(next);
        };

        self.builder.position_at_end(next);
        let out_f32 = self.builder.build_load(out_f32_ptr, "out_f32").into_float_value();

        return MetaValue::with_tag(ValueTag::Number, out_f32.into(), self);
    }

    fn emit(&mut self, ir: DMIR, func: FunctionValue<'ctx>) {
        match ir {

            // Load src onto stack
            DMIR::GetSrc => {
                // self.dbg("GetSrc");

                let p = self.builder.build_alloca(self.val_type, "Src");
                self.builder.build_store(p, func.get_nth_param(1).unwrap());
                self.stack_loc.push(p)
            },
            // Set cache to stack top
            DMIR::SetCache => {
                let arg = self.stack_loc.pop().unwrap();
                self.cache = Some(self.builder.build_load(arg, "load_cache").into_struct_value());
            },
            // Read field from cache e.g cache["oxygen"] where "oxygen" is interned string at name_id
            DMIR::GetCacheField(name_id) => {
                log::debug!("gen get_variable {}", name_id);
                let get_var_func = self.module.get_function("<intrinsic>/get_variable").unwrap();

                log::debug!("get_var func {}", get_var_func.print_to_string().to_string());

                let receiver_value = self.cache.unwrap();

                let out = self.builder.build_alloca(self.val_type, "get_cache_field_out");
                self.builder.build_call(get_var_func, &[out.into(), receiver_value.into(), self.context.i32_type().const_int(name_id as u64, false).into()], "get_cache_field");

                // self.dbg("GetVar");
                // self.dbg_val(self.builder.build_load(out, "load_dbg").into_struct_value());

                self.stack_loc.push(out);
            },
            // Read two values from stack, add them together, put result to stack
            DMIR::FloatAdd => {
                self.emit_bin_op(|first, second, code_gen| {
                    let first_f32 = code_gen.builder.build_bitcast(first.data, code_gen.context.f32_type(), "first_f32").into_float_value();
                    let second_f32 = code_gen.builder.build_bitcast(second.data, code_gen.context.f32_type(), "second_f32").into_float_value();

                    let result_value = code_gen.builder.build_float_add(first_f32, second_f32, "add");
                    let result_i32 = code_gen.builder.build_bitcast(result_value, code_gen.context.i32_type(), "result_i32").into_int_value();

                    MetaValue::with_tag(ValueTag::Number, result_i32.into(), code_gen)
                })
            },
            DMIR::FloatMul => {
                self.emit_bin_op(|first, second, code_gen| {
                    let first_f32 = code_gen.emit_to_number_or_zero(func, first).data.into_float_value();
                    let second_f32 = code_gen.builder.build_bitcast(second.data, code_gen.context.f32_type(), "first_f32").into_float_value();

                    let result_value = code_gen.builder.build_float_mul(first_f32, second_f32, "mul");
                    let result_i32 = code_gen.builder.build_bitcast(result_value, code_gen.context.i32_type(), "result_i32").into_int_value();
                    MetaValue::with_tag(ValueTag::Number, result_i32.into(), code_gen)
                });
            }
            DMIR::FloatTg => {
                self.emit_bin_op(|first, second, code_gen| {
                    let first_f32 = code_gen.builder.build_bitcast(first.data, code_gen.context.f32_type(), "first_f32").into_float_value();
                    let second_f32 = code_gen.builder.build_bitcast(second.data, code_gen.context.f32_type(), "second_f32").into_float_value();

                    let result_value = code_gen.builder.build_float_compare(FloatPredicate::OGT,second_f32, first_f32, "test_greater");
                    let result_f32 = code_gen.builder.build_unsigned_int_to_float(result_value, code_gen.context.f32_type(), "bool_to_f32");
                    let result_i32 = code_gen.builder.build_bitcast(result_f32, code_gen.context.i32_type(), "f32_as_i32").into_int_value();

                    MetaValue::with_tag(ValueTag::Number, result_i32.into(), code_gen)
                })
            }
            DMIR::PushInt(val) => {
                let out = self.builder.build_alloca(self.val_type, "push_int");

                let mut out_val = self.builder.build_load(out, "load_out").into_struct_value();
                out_val = self.builder.build_insert_value(out_val, self.context.i8_type().const_int(0x2a, false), 0, "store_number_tag").unwrap().into_struct_value();

                let result_i32 = self.builder.build_bitcast(
                    self.context.f32_type().const_float(val as f64).const_cast(self.context.f32_type()),
                    self.context.i32_type(),
                    "load_value"
                ).into_int_value();

                out_val = self.builder.build_insert_value(out_val, result_i32, 1, "store_value").unwrap().into_struct_value();


                self.builder.build_store(out, out_val);
                self.stack_loc.push(out);

                // self.dbg("PushInt");
                // self.dbg_val(out_val);
            },
            DMIR::PushVal(op) => {
                let out = self.builder.build_alloca(self.val_type, "push_val");

                let result = MetaValue::new(
                    self.context.i8_type().const_int(op.tag as u64, false),
                    self.context.i32_type().const_int(op.data as u64, false).into()
                );

                let result_val = self.emit_store_meta_value(result);
                self.builder.build_store(out, result_val);
                self.stack_loc.push(out);
            }
            DMIR::Pop => {
                self.stack_loc.pop();
            }
            // Return stack top from proc
            DMIR::Ret => {

                // self.dbg("Ret");
                let result = self.stack_loc.pop().unwrap();
                let value = self.builder.build_load(result, "result").into_struct_value();
                // self.dbg_val(value);

                let out = func.get_nth_param(0).unwrap().into_pointer_value();
                self.builder.build_store(out, value);
                self.block_ended = true;
                self.builder.build_return(None);

            },
            // Set indexed local to stack top
            DMIR::SetLocal(idx) => {
                let ptr = self.stack_loc.pop().unwrap();
                self.locals.insert(idx, ptr);
            }
            // Push indexed local value to stack
            DMIR::GetLocal(idx) => {
                let ptr = self.locals.get(&idx).unwrap().clone();
                self.stack_loc.push(ptr);
            }
            DMIR::GetArg(idx) => {
                let args_pointer = func.get_nth_param(2).unwrap().into_pointer_value();
                let ptr_int = self.context.ptr_sized_int_type(self.execution_engine.get_target_data(), Some(Generic));
                let args_pointer_int = self.builder.build_ptr_to_int(args_pointer, ptr_int, "args_ptr_to_int");
                let size_of = self.val_type.size_of().unwrap();
                let offset_int = size_of.const_mul(size_of.get_type().const_int(idx as u64, false));
                let result_ptr_int = self.builder.build_int_add(args_pointer_int, self.builder.build_int_cast(offset_int, ptr_int, "conv"), "add_offset");
                self.stack_loc.push(self.builder.build_int_to_ptr(result_ptr_int, self.val_type.ptr_type(Generic), "final_ptr"));
            }
            DMIR::Test => {
                let ptr = self.stack_loc.pop().unwrap();
                let value = self.builder.build_load(ptr, "read_stack").into_struct_value();
                let value_type = self.builder.build_extract_value(value, 0, "type").unwrap().into_int_value();

                // is_true on byond is valueTag != 0x0 && (valueTag != 0x2A || valueData != 0.0), where 0x2A is tag for Number
                let ne_null = self.builder.build_int_compare(IntPredicate::NE, value_type, self.context.i8_type().const_zero(), "check_null");
                let ne_num = self.builder.build_int_compare(IntPredicate::NE, value_type, self.context.i8_type().const_int(0x2a, false), "check_number");

                let value_data_i32 = self.builder.build_extract_value(value, 1, "data").unwrap().into_int_value();
                let value_data = self.builder.build_bitcast(value_data_i32, self.context.f32_type(), "cast_f32").into_float_value();
                let ne_zero = self.builder.build_float_compare(FloatPredicate::ONE, value_data, self.context.f32_type().const_zero(), "check_zero");
                let res = self.builder.build_and(ne_null, self.builder.build_or(ne_num, ne_zero, "ne_num or ne_zero"), "ne_null and (ne_num or ne_zero)");

                self.test_res = Some(res);
            }
            DMIR::JZ(lbl) => {
                // this is our next block
                let next = self.context.append_basic_block(func, "next");

                let block = match self.block_map.entry(lbl.clone()) {
                    Entry::Occupied(o) => o.into_mut(),
                    Entry::Vacant(v) => v.insert(self.context.append_basic_block(func, lbl.clone().as_str()))
                };


                self.builder.build_conditional_branch(
                    self.builder.build_not(self.test_res.unwrap(), "jz"), // if test_res == false
                    block.clone(),
                    next
                );

                self.builder.position_at_end(next);
            }
            DMIR::EnterBlock(lbl) => {
                let block = match self.block_map.entry(lbl.clone()) {
                    Entry::Occupied(o) => o.into_mut(),
                    Entry::Vacant(v) => v.insert(self.context.append_basic_block(func, lbl.clone().as_str()))
                };
                if !self.block_ended {
                    self.builder.build_unconditional_branch(block.clone());
                }
                self.block_ended = false;
                self.builder.position_at_end(block.clone());
            }
            DMIR::End => {
                if !self.block_ended {
                    self.builder.build_return(None);
                }
            }
            DMIR::Deopt(offset, proc_id) => {
                let out_stack_type = self.val_type.array_type(self.stack_loc.len() as u32);
                let stack_out_ptr = self.builder.build_alloca(out_stack_type,"out_stack");
                let mut stack_out_array = out_stack_type.const_zero();
                for (pos, loc) in self.stack_loc.iter().enumerate() {
                    let load_stack = self.builder.build_load(loc.clone(), "load_value_stack_loc");
                    stack_out_array = self.builder.build_insert_value(stack_out_array, load_stack, pos as u32, "store_value_from_stack").unwrap().into_array_value();
                }
                self.builder.build_store(stack_out_ptr, stack_out_array);


                let local_names = Proc::from_id(proc_id).unwrap().local_names();

                let out_locals_type = self.val_type.array_type(local_names.len() as u32);
                let locals_out_ptr = self.builder.build_alloca(out_locals_type,"out_locals");
                let mut locals_out_array = out_locals_type.const_zero();
                for (pos, loc) in self.locals.iter() {
                    let load_local = self.builder.build_load(loc.clone(), "load_value_local");
                    locals_out_array = self.builder.build_insert_value(locals_out_array, load_local, pos.clone(), "store_value_from_local").unwrap().into_array_value();
                }
                self.builder.build_store(locals_out_ptr, locals_out_array);

                let parameter_count = Proc::from_id(proc_id).unwrap().parameter_names();

                /*
                out: *mut auxtools::raw_types::values::Value,
                proc_id: auxtools::raw_types::procs::ProcId,
                proc_flags: u8,
                offset: u32,
                test_flag: bool,
                stack: *const auxtools::raw_types::values::Value,
                stack_size: u32,
                cached_datum: auxtools::raw_types::values::Value,
                src: auxtools::raw_types::values::Value,
                args: *const auxtools::raw_types::values::Value,
                args_count: u32,
                locals: *const auxtools::raw_types::values::Value,
                locals_count: u32
                 */
                self.builder.build_call(self.module.get_function("<intrinsic>/deopt").unwrap(), &[
                    func.get_nth_param(0).unwrap().into(),
                    self.context.i32_type().const_int(proc_id.0 as u64, false).into(),
                    self.context.i8_type().const_int(2, false).into(),
                    self.context.i32_type().const_int(offset as u64, false).into(),
                    self.test_res.unwrap_or(self.context.bool_type().const_int(0, false)).into(),
                    self.builder.build_bitcast(stack_out_ptr, self.val_type.ptr_type(Generic), "cast").into(),
                    self.context.i32_type().const_int(self.stack_loc.len() as u64, false).into(),
                    self.cache.unwrap_or(self.val_type.const_zero()).into(),
                    func.get_nth_param(1).unwrap().into(),
                    func.get_nth_param(2).unwrap().into(),
                    self.context.i32_type().const_int(parameter_count.len() as u64, false).into(),
                    self.builder.build_bitcast(locals_out_ptr, self.val_type.ptr_type(Generic), "cast").into(),
                    self.context.i32_type().const_int(local_names.len() as u64, false).into()
                ], "call_deopt");


                self.builder.build_return(None);
                self.block_ended = true;
            }
            _ => {}
        }
    }
}

enum DMIR {
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


#[no_mangle]
pub extern "C" fn debug(str: *mut i8) {
    log::debug!("dbg ptr: {:?}", str);
    log::debug!("dbg: {}", unsafe { CStr::from_ptr(str) }.to_str().unwrap());
}

#[no_mangle]
pub extern "C" fn debug_val(val: auxtools::raw_types::values::Value) {
    log::debug!("dbg: {:?}", val)
}


fn compile_proc<'ctx>(context: &'static Context, module: &'ctx Module<'static>, execution_engine: &'ctx ExecutionEngine<'static>, proc: auxtools::Proc) {
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
                        irs.push(DMIR::FloatAdd)
                    }
                    Instruction::Mul => {
                        irs.push(DMIR::FloatMul)
                    }
                    Instruction::Tg => {
                        irs.push(DMIR::FloatTg)
                    }
                    Instruction::CallGlob(arg_count, callee) => {
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
        return;
    }

    // Prepare LLVM internals for code-generation
    let mut code_gen = CodeGen::create(context, &module, context.create_builder(), execution_engine);

    if code_gen.module.get_function("<intrinsic>/get_variable").is_none() {
        let get_variable_signature = context.i8_type().fn_type(&[code_gen.val_type.ptr_type(AddressSpace::Generic).into(), code_gen.val_type.into(), code_gen.context.i32_type().into()], false);
        let get_variable_func = code_gen.module.add_function("<intrinsic>/get_variable", get_variable_signature, Some(Linkage::External));
        code_gen.execution_engine.add_global_mapping(&get_variable_func, auxtools::raw_types::funcs::get_variable as usize);

        let debug_func_sig = context.void_type().fn_type(&[code_gen.context.i8_type().ptr_type(AddressSpace::Generic).into()], false);
        let debug_func = code_gen.module.add_function("<intrinsic>/debug", debug_func_sig, Some(Linkage::External));
        code_gen.execution_engine.add_global_mapping(&debug_func, debug as usize);


        let debug_val_func_sig = context.void_type().fn_type(&[code_gen.val_type.into()], false);
        let debug_val_func = code_gen.module.add_function("<intrinsic>/debug_val", debug_val_func_sig, Some(Linkage::External));
        code_gen.execution_engine.add_global_mapping(&debug_val_func,  debug_val as usize);


        /*
          out: *mut auxtools::raw_types::values::Value,
          proc_id: auxtools::raw_types::procs::ProcId,
          proc_flags: u8,
          offset: u32,
          test_flag: bool,
          stack: *const auxtools::raw_types::values::Value,
          stack_size: u32,
          cached_datum: auxtools::raw_types::values::Value,
          src: auxtools::raw_types::values::Value,
          args: *const auxtools::raw_types::values::Value,
          args_count: u32,
          locals: *const auxtools::raw_types::values::Value,
          locals_count: u32
         */
        let deopt_func_sig = context.void_type().fn_type(&[
            code_gen.val_type.ptr_type(AddressSpace::Generic).into(),
            context.i32_type().into(),
            context.i8_type().into(),
            context.i32_type().into(),
            context.bool_type().into(),
            code_gen.val_type.ptr_type(AddressSpace::Generic).into(),
            context.i32_type().into(),
            code_gen.val_type.into(),
            code_gen.val_type.into(),
            code_gen.val_type.ptr_type(AddressSpace::Generic).into(),
            context.i32_type().into(),
            code_gen.val_type.ptr_type(AddressSpace::Generic).into(),
            context.i32_type().into()
        ], false);
        let deopt_func = code_gen.module.add_function("<intrinsic>/deopt", deopt_func_sig, Some(Linkage::External));
        code_gen.execution_engine.add_global_mapping(&deopt_func,  deopt as usize);
    }

    // each function in our case should be void *(ret, src)
    let ptr = code_gen.val_type.ptr_type(AddressSpace::Generic);
    let ft = context.void_type().fn_type(&[ptr.into(), code_gen.val_type.into(), ptr.into()], false);

    // register function in LLVM
    let func = code_gen.module.add_function(&proc.path, ft, None);



    // create start basic block for our function
    let block = context.append_basic_block(func, "base");
    code_gen.builder.position_at_end(block);
    code_gen.dbg(format!("llvm function call {}", proc.path).as_str());

    // Emit LLVM IR nodes from DMIR
    for ir in irs {
        code_gen.emit(ir, func);
    }

    log::info!("{}", func.print_to_string().to_string());
    let verify = func.verify(true);

    if let Err(err) = code_gen.module.verify() {
        log::error!("err: {}", err.to_string());
    }

    log::info!("Compiled {}, installing {}", verify, proc.path);

    let fpm = PassManager::create(code_gen.module);

    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();

    fpm.initialize();

    let res = fpm.run_on(&func);
    log::info!("OPT -- {}\n{}", res, func.print_to_string().to_string());

}
