use inkwell::{IntPredicate, FloatPredicate, AddressSpace};
use inkwell::context::Context;
use inkwell::module::{Module, Linkage};
use inkwell::builder::Builder;
use inkwell::values::{PointerValue, StructValue, IntValue, BasicValueEnum, FunctionValue, AnyValue};
use std::collections::HashMap;
use inkwell::types::StructType;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::basic_block::BasicBlock;
use auxtools::raw_types::values::ValueTag;
use inkwell::AddressSpace::Generic;
use std::collections::hash_map::Entry;
use auxtools::Proc;
use crate::dmir::DMIR;
use std::borrow::Borrow;
use crate::pads;

pub struct CodeGen<'ctx, 'a> {
    context: &'ctx Context,
    pub(crate) module: &'a Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: &'a ExecutionEngine<'ctx>,
    stack_loc: Vec<PointerValue<'ctx>>,
    locals: HashMap<u32, PointerValue<'ctx>>,
    cache: Option<StructValue<'ctx>>,
    val_type: StructType<'ctx>,
    test_res: Option<IntValue<'ctx>>,
    block_map: HashMap<String, BasicBlock<'ctx>>,
    block_ended: bool,
}


const INTRINSIC_CALL_PROC_BY_ID: &str = "<intrinsic>/call_proc_by_id";
const INTRINSIC_CALL_PROC_BY_NAME: &str = "<intrinsic>/call_proc_by_name";
const INTRINSIC_DEOPT: &str = "<intrinsic>/deopt";

struct MetaValue<'ctx> {
    tag: IntValue<'ctx>,
    data: BasicValueEnum<'ctx>,
}

impl MetaValue<'_> {
    fn new<'a>(tag: IntValue<'a>, data: BasicValueEnum<'a>) -> MetaValue<'a> {
        return MetaValue {
            tag,
            data,
        };
    }

    fn with_tag<'a>(tag: auxtools::raw_types::values::ValueTag, data: BasicValueEnum<'a>, code_gen: &CodeGen<'a, '_>) -> MetaValue<'a> {
        let tag = code_gen.context.i8_type().const_int(tag as u64, false);
        return Self::new(tag, data);
    }
}

impl<'ctx> CodeGen<'ctx, '_> {
    pub fn init_builtins(&self) {
        if self.module.get_function("<intrinsic>/get_variable").is_none() {
            let get_variable_signature = self.context.i8_type().fn_type(&[self.val_type.ptr_type(AddressSpace::Generic).into(), self.val_type.into(), self.context.i32_type().into()], false);
            let get_variable_func = self.module.add_function("<intrinsic>/get_variable", get_variable_signature, Some(Linkage::External));
            self.execution_engine.add_global_mapping(&get_variable_func, auxtools::raw_types::funcs::get_variable as usize);

            let debug_func_sig = self.context.void_type().fn_type(&[self.context.i8_type().ptr_type(AddressSpace::Generic).into()], false);
            let debug_func = self.module.add_function("<intrinsic>/debug", debug_func_sig, Some(Linkage::External));
            self.execution_engine.add_global_mapping(&debug_func, pads::debug::handle_debug as usize);


            let debug_val_func_sig = self.context.void_type().fn_type(&[self.val_type.into()], false);
            let debug_val_func = self.module.add_function("<intrinsic>/debug_val", debug_val_func_sig, Some(Linkage::External));
            self.execution_engine.add_global_mapping(&debug_val_func, pads::debug::handle_debug_val as usize);


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
            let deopt_func_sig = self.context.void_type().fn_type(&[
                self.val_type.ptr_type(AddressSpace::Generic).into(),
                self.context.i32_type().into(),
                self.context.i8_type().into(),
                self.context.i32_type().into(),
                self.context.bool_type().into(),
                self.val_type.ptr_type(AddressSpace::Generic).into(),
                self.context.i32_type().into(),
                self.val_type.into(),
                self.val_type.into(),
                self.val_type.ptr_type(AddressSpace::Generic).into(),
                self.context.i32_type().into(),
                self.val_type.ptr_type(AddressSpace::Generic).into(),
                self.context.i32_type().into()
            ], false);
            let deopt_func = self.module.add_function(INTRINSIC_DEOPT, deopt_func_sig, Some(Linkage::External));
            self.execution_engine.add_global_mapping(&deopt_func, pads::deopt::handle_deopt as usize);


            {

                let call_proc_by_id_sig = self.context.i8_type().fn_type(
                    &[
                        self.val_type.ptr_type(AddressSpace::Generic).into(), //out: *mut values::Value,
                        self.val_type.into(), //usr: values::Value,
                        self.context.i32_type().into(), //proc_type: u32,
                        self.context.i32_type().into(), //proc_id: procs::ProcId,
                        self.context.i32_type().into(), //unk_0: u32,
                        self.val_type.into(), //src: values::Value,
                        self.val_type.ptr_type(AddressSpace::Generic).into(), //args: *const values::Value,
                        self.context.i32_type().into(), //args_count_l: usize,
                        self.context.i32_type().into(), //unk_1: u32,
                        self.context.i32_type().into(), //unk_2: u32,
                    ],
                    false
                );
                let call_proc_by_id_func = self.module.add_function(INTRINSIC_CALL_PROC_BY_ID, call_proc_by_id_sig, None);
                self.execution_engine.add_global_mapping(&call_proc_by_id_func, auxtools::raw_types::funcs::call_proc_by_id as usize)
            }

            {
                let call_proc_by_name_sig = self.context.i8_type().fn_type(&[
                    self.val_type.ptr_type(AddressSpace::Generic).into(), //out: *mut values::Value,
                    self.val_type.into(), //usr: values::Value,
                    self.context.i32_type().into(), //proc_type: u32,
                    self.context.i32_type().into(), //proc_name: strings::StringId,
                    self.val_type.into(), //src: values::Value,
                    self.val_type.ptr_type(AddressSpace::Generic).into(), //args: *mut values::Value,
                    self.context.i32_type().into(), //args_count_l: usize,
                    self.context.i32_type().into(), //unk_0: u32,
                    self.context.i32_type().into(), //unk_1: u32,
                ], false);
                let call_proc_by_name_func = self.module.add_function(INTRINSIC_CALL_PROC_BY_NAME, call_proc_by_name_sig, None);
                self.execution_engine.add_global_mapping(&call_proc_by_name_func, auxtools::raw_types::funcs::call_datum_proc_by_name as usize)
            }
        }
    }

    pub fn create<'a>(context: &'ctx Context, module: &'a Module<'ctx>, builder: Builder<'ctx>, execution_engine: &'a ExecutionEngine<'ctx>) -> CodeGen<'ctx, 'a> {

        // type of BYOND operands, actually a struct { type: u8, value: u32 }
        let val_type = context.opaque_struct_type("DMValue");
        val_type.set_body(&[context.i8_type().into(), context.i32_type().into()], false);

        CodeGen {
            context,
            module,
            builder,
            execution_engine,
            stack_loc: vec![],
            locals: HashMap::new(),
            cache: None,
            val_type,
            test_res: None,
            block_map: HashMap::new(),
            block_ended: false,
        }
    }

    pub fn create_jit_func(&self, name: &str) -> FunctionValue<'ctx> {
        // each function in our case should be void *(ret, src)
        let ptr = self.val_type.ptr_type(AddressSpace::Generic);
        let ft = self.context.void_type().fn_type(
            &[
                ptr.into(),
                self.val_type.into(),
                self.val_type.into(),
                ptr.into()
            ],
            false,
        );

        // register function in LLVM
        let func = self.module.add_function(name, ft, None);

        // create start basic block for our function
        let block = self.context.append_basic_block(func, "base");
        self.builder.position_at_end(block);
        self.dbg(format!("llvm function call {}", name).as_str());

        return func;
    }

    fn dbg(&self, str: &str) {
        let ptr = self.builder.build_global_string_ptr(str, "dbg_str").as_pointer_value();
        self.builder.build_call(self.module.get_function("<intrinsic>/debug").unwrap(), &[ptr.into()], "call_dbg");
    }

    fn dbg_val(&self, val: StructValue<'ctx>) {
        self.builder.build_call(self.module.get_function("<intrinsic>/debug_val").unwrap(), &[val.into()], "call_dbg");
    }


    fn emit_load_meta_value(&self, from: StructValue<'ctx>) -> MetaValue<'ctx> {
        let tag = self.builder.build_extract_value(from, 0, "get_tag").unwrap().into_int_value();
        let data = self.builder.build_extract_value(from, 1, "get_data").unwrap().into_int_value();
        return MetaValue::new(tag, data.into());
    }

    fn emit_store_meta_value(&self, from: MetaValue<'ctx>) -> StructValue<'ctx> {
        let out_val = self.val_type.const_zero();
        let out_val = self.builder.build_insert_value(out_val, from.tag, 0, "set_tag").unwrap().into_struct_value();
        let out_val = self.builder.build_insert_value(out_val, from.data, 1, "set_data").unwrap().into_struct_value();

        return out_val;
    }

    fn emit_bin_op<F>(&mut self, op: F)
        where F: FnOnce(MetaValue<'ctx>, MetaValue<'ctx>, &mut Self) -> MetaValue<'ctx>
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

    fn const_tag(&self, value_tag: ValueTag) -> IntValue<'ctx> {
        return self.context.i8_type().const_int(value_tag as u64, false).into();
    }

    fn emit_to_number_or_zero(&self, func: FunctionValue<'ctx>, value: MetaValue<'ctx>) -> MetaValue<'ctx> {
        let iff_number = self.context.append_basic_block(func, "iff_number");
        let next = self.context.append_basic_block(func, "next");

        let out_f32_ptr = self.builder.build_alloca(self.context.f32_type(), "second_f32_store");
        self.builder.build_store(out_f32_ptr, self.context.f32_type().const_zero());

        let number_tag = self.const_tag(ValueTag::Number);

        self.builder.build_conditional_branch(
            self.builder.build_int_compare(IntPredicate::EQ, value.tag, number_tag, "check_number"),
            iff_number,
            next,
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

    fn emit_store_args_from_stack(&mut self, arg_count: u32) -> PointerValue<'ctx> {
        let out_stack_type = self.val_type.array_type(arg_count as u32);
        let args = self.builder.build_alloca(out_stack_type, "out_stack");
        let mut stack_out_array = out_stack_type.const_zero();
        for idx in 0..arg_count {
            let arg = self.stack_loc.pop().unwrap();
            let load_stack = self.builder.build_load(arg, "load_arg_loc");
            stack_out_array = self.builder.build_insert_value(stack_out_array, load_stack, idx as u32, "store_value_from_stack").unwrap().into_array_value();
        }
        self.builder.build_store(args, stack_out_array);
        return self.builder.build_bitcast(args, self.val_type.ptr_type(Generic), "cast_to_ptr").into_pointer_value();
    }

    // Actual return type is i1/bool
    fn emit_check_is_true(&self, value: MetaValue<'ctx>) -> IntValue<'ctx> {
        let is_null = self.builder.build_int_compare(IntPredicate::EQ, value.tag, self.const_tag(ValueTag::Null), "is_null");
        let is_x22 = self.builder.build_int_compare(IntPredicate::EQ, value.tag, self.context.i8_type().const_int(0x22, false), "is_x22");

        let is_false_tag = self.builder.build_or(is_null, is_x22, "check_false_tag");

        let is_num = self.builder.build_int_compare(IntPredicate::EQ, value.tag, self.const_tag(ValueTag::Number), "is_num");
        let is_str = self.builder.build_int_compare(IntPredicate::EQ, value.tag, self.const_tag(ValueTag::String), "is_str");

        let not_num_or_not_zero = self.builder.build_or(
            self.builder.build_not(is_num, "!is_num"),
            self.builder.build_float_compare(
                FloatPredicate::UNE,
                self.builder.build_bitcast(value.data, self.context.f32_type(), "cast_f32").into_float_value(),
                self.context.f32_type().const_zero(),
                "check_ne_zero"
            ),
            "not_num_or_not_zero"
        );

        let not_str_or_not_zero = self.builder.build_or(
            self.builder.build_not(is_str, "!is_str"),
            self.builder.build_int_compare(IntPredicate::NE, value.data.into_int_value(), self.context.i32_type().const_zero(), "check_ne_zero"),
            "not_str_or_not_zero"
        );


        // = !(is_null || is_x22) && (!is_num || value != 0.0) && (!is_str || value != 0)
        return self.builder.build_and(
            self.builder.build_not(is_false_tag, "not_(null_or_x22)"),
            self.builder.build_and(
                not_num_or_not_zero,
                not_str_or_not_zero,
                "is_true_data"
            ),
            "check_is_true"
        )
    }

    pub fn emit(&mut self, ir: &DMIR, func: FunctionValue<'ctx>) {
        match ir {

            // Load src onto stack
            DMIR::GetSrc => {
                // self.dbg("GetSrc");

                let p = self.builder.build_alloca(self.val_type, "Src");
                self.builder.build_store(p, func.get_nth_param(1).unwrap());
                self.stack_loc.push(p)
            }
            // Set cache to stack top
            DMIR::SetCache => {
                let arg = self.stack_loc.pop().unwrap();
                self.cache = Some(self.builder.build_load(arg, "load_cache").into_struct_value());
            }
            DMIR::PushCache => {
                let cache = self.cache.unwrap();
                let out_ptr = self.builder.build_alloca(self.val_type, "push_cache");
                self.builder.build_store(out_ptr, cache);
                self.stack_loc.push(out_ptr);
            }
            // Read field from cache e.g cache["oxygen"] where "oxygen" is interned string at name_id
            DMIR::GetCacheField(name_id) => {
                log::debug!("gen get_variable {}", name_id);
                let get_var_func = self.module.get_function("<intrinsic>/get_variable").unwrap();

                log::debug!("get_var func {}", get_var_func.print_to_string().to_string());

                let receiver_value = self.cache.unwrap();

                let out = self.builder.build_alloca(self.val_type, "get_cache_field_out");
                self.builder.build_call(get_var_func, &[out.into(), receiver_value.into(), self.context.i32_type().const_int(name_id.clone() as u64, false).into()], "get_cache_field");

                // self.dbg("GetVar");
                // self.dbg_val(self.builder.build_load(out, "load_dbg").into_struct_value());

                self.stack_loc.push(out);
            }
            // Read two values from stack, add them together, put result to stack
            DMIR::FloatAdd => {
                self.emit_bin_op(|first, second, code_gen| {
                    let first_f32 = code_gen.builder.build_bitcast(first.data, code_gen.context.f32_type(), "first_f32").into_float_value();
                    let second_f32 = code_gen.builder.build_bitcast(second.data, code_gen.context.f32_type(), "second_f32").into_float_value();

                    let result_value = code_gen.builder.build_float_add(first_f32, second_f32, "add");
                    let result_i32 = code_gen.builder.build_bitcast(result_value, code_gen.context.i32_type(), "result_i32").into_int_value();

                    MetaValue::with_tag(ValueTag::Number, result_i32.into(), code_gen)
                })
            }
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

                    let result_value = code_gen.builder.build_float_compare(FloatPredicate::OGT, second_f32, first_f32, "test_greater");
                    let result_f32 = code_gen.builder.build_unsigned_int_to_float(result_value, code_gen.context.f32_type(), "bool_to_f32");
                    let result_i32 = code_gen.builder.build_bitcast(result_f32, code_gen.context.i32_type(), "f32_as_i32").into_int_value();

                    MetaValue::with_tag(ValueTag::Number, result_i32.into(), code_gen)
                })
            }
            DMIR::FloatAbs => {
                let arg_ptr = self.stack_loc.pop().unwrap();
                let arg = self.emit_load_meta_value(self.builder.build_load(arg_ptr, "load_arg").into_struct_value());
                let arg_num = self.emit_to_number_or_zero(func, arg);
                let fabs_type = self.context.f32_type().fn_type(&[self.context.f32_type().into()], false);
                let fabs = self.module.add_function("llvm.fabs.f32", fabs_type, None);
                let result = self.builder.build_call(fabs, &[arg_num.data.into_float_value().into()], "abs").try_as_basic_value().left().unwrap().into_float_value();

                let out_ptr = self.builder.build_alloca(self.val_type, "abs_out");
                let result_i32 = self.builder.build_bitcast(result, self.context.i32_type(), "cast_result").into_int_value();
                let meta_result = MetaValue::with_tag(ValueTag::Number, result_i32.into(), self);
                self.builder.build_store(out_ptr, self.emit_store_meta_value(meta_result));


                self.stack_loc.push(out_ptr);
            }
            DMIR::CallProcById(proc_id, proc_call_type, arg_count) => {
                let src_ptr = self.stack_loc.pop().unwrap();
                let src = self.builder.build_load(src_ptr, "load_src").into_struct_value();

                let args = self.emit_store_args_from_stack(arg_count.clone());

                let call_proc_by_id = self.module.get_function(INTRINSIC_CALL_PROC_BY_ID).unwrap();

                let usr = func.get_nth_param(2).unwrap().into_struct_value(); // TODO: Proc can change self usr

                let out = self.builder.build_alloca(self.val_type, "out");

                self.builder.build_call(
                    call_proc_by_id,
                    &[
                        out.into(), // out: *mut values::Value,
                        usr.into(), // usr: values::Value,
                        self.context.i32_type().const_int(proc_call_type.clone() as u64, false).into(), // proc_type: u32,
                        self.context.i32_type().const_int(proc_id.0 as u64, false).into(), // proc_id: procs::ProcId,
                        self.context.i32_type().const_int(0, false).into(),  // unk_0: u32,
                        src.into(), // src: values::Value,
                        args.into(), // args: *const values::Value,
                        self.context.i32_type().const_int(arg_count.clone() as u64, false).into(), // args_count_l: usize,
                        self.context.i32_type().const_int(0, false).into(), // unk_1: u32,
                        self.context.i32_type().const_int(0, false).into(), // unk_2: u32,
                    ],
                    "call_proc_by_id",
                );

                self.stack_loc.push(out);
            }
            DMIR::CallProcByName(string_id, proc_call_type, arg_count) => {
                let src_ptr = self.stack_loc.pop().unwrap();
                let src = self.builder.build_load(src_ptr, "load_src").into_struct_value();

                let args = self.emit_store_args_from_stack(arg_count.clone());

                let call_proc_by_name = self.module.get_function(INTRINSIC_CALL_PROC_BY_NAME).unwrap();

                let usr = func.get_nth_param(2).unwrap().into_struct_value(); // TODO: Proc can change self usr

                let out = self.builder.build_alloca(self.val_type, "out");

                self.builder.build_call(
                    call_proc_by_name,
                    &[
                        out.into(), //out: *mut values::Value,
                        usr.into(), //usr: values::Value,
                        self.context.i32_type().const_int(proc_call_type.clone() as u64, false).into(), //proc_type: u32,
                        self.context.i32_type().const_int(string_id.0 as u64, false).into(), //proc_name: strings::StringId,
                        src.into(), //src: values::Value,
                        args.into(), //args: *mut values::Value,
                        self.context.i32_type().const_int(arg_count.clone() as u64, false).into(), //args_count_l: usize,
                        self.context.i32_type().const_int(0, false).into(), //unk_0: u32,
                        self.context.i32_type().const_int(0, false).into(), //unk_1: u32,
                    ],
                    "call_proc_by_name",
                );

                self.stack_loc.push(out);
            }
            DMIR::PushInt(val) => {
                let out = self.builder.build_alloca(self.val_type, "push_int");

                let mut out_val = self.builder.build_load(out, "load_out").into_struct_value();
                out_val = self.builder.build_insert_value(out_val, self.context.i8_type().const_int(0x2a, false), 0, "store_number_tag").unwrap().into_struct_value();

                let result_i32 = self.builder.build_bitcast(
                    self.context.f32_type().const_float(val.clone() as f64).const_cast(self.context.f32_type()),
                    self.context.i32_type(),
                    "load_value",
                ).into_int_value();

                out_val = self.builder.build_insert_value(out_val, result_i32, 1, "store_value").unwrap().into_struct_value();


                self.builder.build_store(out, out_val);
                self.stack_loc.push(out);

                // self.dbg("PushInt");
                // self.dbg_val(out_val);
            }
            DMIR::PushVal(op) => {
                let out = self.builder.build_alloca(self.val_type, "push_val");

                let result = MetaValue::new(
                    self.context.i8_type().const_int(op.tag as u64, false),
                    self.context.i32_type().const_int(op.data as u64, false).into(),
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
            }
            // Set indexed local to stack top
            DMIR::SetLocal(idx) => {
                let ptr = self.stack_loc.pop().unwrap();
                self.locals.insert(idx.clone(), ptr);
            }
            // Push indexed local value to stack
            DMIR::GetLocal(idx) => {
                let ptr = self.locals.get(&idx).unwrap().clone();
                self.stack_loc.push(ptr);
            }
            DMIR::GetArg(idx) => {
                let args_pointer = func.get_nth_param(3).unwrap().into_pointer_value();
                let ptr_int = self.context.ptr_sized_int_type(self.execution_engine.get_target_data(), Some(Generic));
                let args_pointer_int = self.builder.build_ptr_to_int(args_pointer, ptr_int, "args_ptr_to_int");
                let size_of = self.val_type.size_of().unwrap();
                let offset_int = size_of.const_mul(size_of.get_type().const_int(idx.clone() as u64, false));
                let result_ptr_int = self.builder.build_int_add(args_pointer_int, self.builder.build_int_cast(offset_int, ptr_int, "conv"), "add_offset");
                self.stack_loc.push(self.builder.build_int_to_ptr(result_ptr_int, self.val_type.ptr_type(Generic), "final_ptr"));
            }
            DMIR::Test => {
                let ptr = self.stack_loc.pop().unwrap();
                let value = self.builder.build_load(ptr, "read_stack").into_struct_value();
                let res = self.emit_check_is_true(self.emit_load_meta_value(value));

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
                    next,
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
                let stack_out_ptr = self.builder.build_alloca(out_stack_type, "out_stack");
                let mut stack_out_array = out_stack_type.const_zero();
                for (pos, loc) in self.stack_loc.iter().enumerate() {
                    let load_stack = self.builder.build_load(loc.clone(), "load_value_stack_loc");
                    stack_out_array = self.builder.build_insert_value(stack_out_array, load_stack, pos as u32, "store_value_from_stack").unwrap().into_array_value();
                }
                self.builder.build_store(stack_out_ptr, stack_out_array);


                let local_names = Proc::from_id(proc_id.clone()).unwrap().local_names();

                let out_locals_type = self.val_type.array_type(local_names.len() as u32);
                let locals_out_ptr = self.builder.build_alloca(out_locals_type, "out_locals");
                let mut locals_out_array = out_locals_type.const_zero();
                for (pos, loc) in self.locals.iter() {
                    let load_local = self.builder.build_load(loc.clone(), "load_value_local");
                    locals_out_array = self.builder.build_insert_value(locals_out_array, load_local, pos.clone(), "store_value_from_local").unwrap().into_array_value();
                }
                self.builder.build_store(locals_out_ptr, locals_out_array);

                let parameter_count = Proc::from_id(proc_id.clone()).unwrap().parameter_names();

                self.builder.build_call(
                    self.module.get_function(INTRINSIC_DEOPT).unwrap(),
                    &[
                        func.get_nth_param(0).unwrap().into(), //out: *mut auxtools::raw_types::values::Value,
                        self.context.i32_type().const_int(proc_id.0 as u64, false).into(), //proc_id: auxtools::raw_types::procs::ProcId,
                        self.context.i8_type().const_int(2, false).into(), //proc_flags: u8,
                        self.context.i32_type().const_int(offset.clone() as u64, false).into(), //offset: u32,
                        self.test_res.unwrap_or(self.context.bool_type().const_int(0, false)).into(), //test_flag: bool,
                        self.builder.build_bitcast(stack_out_ptr, self.val_type.ptr_type(Generic), "cast").into(), //stack: *const auxtools::raw_types::values::Value,
                        self.context.i32_type().const_int(self.stack_loc.len() as u64, false).into(), //stack_size: u32,
                        self.cache.unwrap_or(self.val_type.const_zero()).into(), //cached_datum: auxtools::raw_types::values::Value,
                        func.get_nth_param(1).unwrap().into(), //src: auxtools::raw_types::values::Value,
                        func.get_nth_param(3).unwrap().into(), //args: *const auxtools::raw_types::values::Value,
                        self.context.i32_type().const_int(parameter_count.len() as u64, false).into(), //args_count: u32,
                        self.builder.build_bitcast(locals_out_ptr, self.val_type.ptr_type(Generic), "cast").into(), //locals: *const auxtools::raw_types::values::Value,
                        self.context.i32_type().const_int(local_names.len() as u64, false).into(), //locals_count: u32
                    ],
                    "call_deopt",
                );


                self.builder.build_return(None);
                self.block_ended = true;
            }
            DMIR::CheckTypeDeopt(stack_pos, tag, deopt) => {
                let stack_size = self.stack_loc.len();
                let stack_value_ptr = self.stack_loc[stack_size - 1 - (stack_pos.clone() as usize)];
                let stack_value = self.builder.build_load(stack_value_ptr, "load_value_to_check").into_struct_value();
                let actual_tag = self.emit_load_meta_value(stack_value).tag;

                let next_block = self.context.append_basic_block(func, "next");
                let deopt_block = self.context.append_basic_block(func, "deopt");


                self.builder.build_conditional_branch(
                    self.builder.build_int_compare(IntPredicate::EQ, actual_tag, self.const_tag(tag.clone()), "check_tag"),
                    next_block,
                    deopt_block,
                );

                self.builder.position_at_end(deopt_block);
                self.emit(deopt.borrow(), func);

                self.builder.position_at_end(next_block);
                self.block_ended = false;
            }
            _ => {}
        }
    }
}
