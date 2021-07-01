use inkwell::{IntPredicate, FloatPredicate, AddressSpace};
use inkwell::context::Context;
use inkwell::module::{Module, Linkage};
use inkwell::builder::Builder;
use inkwell::values::{PointerValue, StructValue, IntValue, BasicValueEnum, FunctionValue, AnyValue, PhiValue};
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
    stack_loc: Vec<StructValue<'ctx>>,
    locals: HashMap<u32, PointerValue<'ctx>>,
    cache: Option<StructValue<'ctx>>,
    val_type: StructType<'ctx>,
    test_res: Option<IntValue<'ctx>>,
    block_map: BlockMap<'ctx>,
    block_ended: bool,
}

type BlockMap<'ctx> = HashMap<String, LabelBlockInfo<'ctx>>;

const INTRINSIC_CALL_PROC_BY_ID: &str = "<intrinsic>/call_proc_by_id";
const INTRINSIC_CALL_PROC_BY_NAME: &str = "<intrinsic>/call_proc_by_name";
const INTRINSIC_DEOPT: &str = "<intrinsic>/deopt";
const INTRINSIC_SET_VARIABLE: &str = "<intrinsic>/set_variable";


struct LabelBlockInfo<'ctx> {
    block: BasicBlock<'ctx>,
    phi: Vec<PhiValue<'ctx>>
}

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


struct StackManager<'ctx, 'a, 'b> {
    code_gen: &'a mut CodeGen<'ctx, 'b>
}


impl<'ctx> StackManager<'ctx, '_, '_> {
    fn push(&mut self, value: StructValue<'ctx>) {
        self.code_gen.stack_loc.push(value);
    }

    fn pop(&mut self) -> StructValue<'ctx> {
        return self.code_gen.stack_loc.pop().unwrap();
    }
}

impl<'ctx, 'b> CodeGen<'ctx, 'b> {
    fn stack(&mut self) -> StackManager<'ctx, '_, 'b> {
        return StackManager {
            code_gen: self
        }
    }
}

struct BlockBuilder<'ctx, 'a> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    val_type: &'a StructType<'ctx>,
    block_map: &'a mut BlockMap<'ctx>
}

impl<'ctx, 'a> BlockBuilder<'ctx, 'a> {
    fn emit_postponed_block(
        &'a mut self, func: FunctionValue<'ctx>, lbl: &String, stack_size: usize
    ) -> &'a LabelBlockInfo<'ctx> {
        match self.block_map.entry(lbl.clone()) {
            Entry::Vacant(v) => {
                let current_block = self.builder.get_insert_block().unwrap();
                let new_block = self.context.append_basic_block(func, lbl);

                self.builder.position_at_end(new_block);
                let mut phi: Vec<PhiValue<'ctx>> = Vec::new();
                for _ in 0..stack_size {
                    phi.push(self.builder.build_phi(self.val_type.clone(), "stack_loc"));
                }
                self.builder.position_at_end(current_block);

                let target_info =
                    LabelBlockInfo {
                        block: new_block,
                        phi
                    };
                v.insert(target_info)
            }
            Entry::Occupied(v) => {
                v.into_mut()
            }
        }
    }

    fn emit_jump_target_block(&'a mut self, stack_loc: &Vec<StructValue<'ctx>>, func: FunctionValue<'ctx>, lbl: &String) -> &'a LabelBlockInfo<'ctx> {
        let current_block = self.builder.get_insert_block().unwrap();
        let stack = stack_loc;
        let label_block_info = self.emit_postponed_block(func, lbl, stack.len());
        for (idx, loc) in stack.iter().enumerate() {
            label_block_info.phi[idx].add_incoming(&[(loc, current_block)]);
        }

        label_block_info
    }
}

impl<'ctx> CodeGen<'ctx, '_> {
    pub fn init_builtins(&self) {
        if self.module.get_function("<intrinsic>/get_variable").is_none() {
            let get_variable_signature = self.context.i8_type().fn_type(&[self.val_type.ptr_type(AddressSpace::Generic).into(), self.val_type.into(), self.context.i32_type().into()], false);
            let get_variable_func = self.module.add_function("<intrinsic>/get_variable", get_variable_signature, Some(Linkage::External));
            self.execution_engine.add_global_mapping(&get_variable_func, auxtools::raw_types::funcs::get_variable as usize);

            {
                let set_variable_sig = self.context.i8_type().fn_type(
                    &[
                        self.val_type.into(), // datum: values::Value
                        self.context.i32_type().into(), // index: strings::StringId
                        self.val_type.into() // value: values::Value
                    ],
                    false
                );

                let set_variable_func = self.module.add_function(INTRINSIC_SET_VARIABLE, set_variable_sig, Some(Linkage::External));
                self.execution_engine.add_global_mapping(&set_variable_func, auxtools::raw_types::funcs::set_variable as usize)
            }

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

        // TODO: Cleanup
        let val_type =
            if let Some(val_type) = module.get_struct_type("DMValue") {
                val_type
            } else {
                // type of BYOND operands, actually a struct { type: u8, value: u32 }
                let val_type = context.opaque_struct_type("DMValue");
                val_type.set_body(&[context.i8_type().into(), context.i32_type().into()], false);
                val_type
            };

        CodeGen {
            context,
            module,
            builder,
            execution_engine,
            stack_loc: Vec::new(),
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
        let first_struct = self.stack().pop();
        let second_struct = self.stack().pop();

        let first_meta = self.emit_load_meta_value(first_struct);
        let second_meta = self.emit_load_meta_value(second_struct);

        let result_meta = op(first_meta, second_meta, self);

        let out_val = self.emit_store_meta_value(result_meta);

        self.stack().push(out_val);
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
            let arg = self.stack().pop();
            stack_out_array = self.builder.build_insert_value(stack_out_array, arg, idx as u32, "store_value_from_stack").unwrap().into_array_value();
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

    fn emit_conditional_jump(&mut self, func: FunctionValue<'ctx>, lbl: &String, condition: IntValue<'ctx>) {
        let next = self.context.append_basic_block(func, "next");

        let mut block_builder = BlockBuilder {
            context: self.context,
            builder: &self.builder,
            val_type: &self.val_type,
            block_map: &mut self.block_map
        };
        let target = block_builder.emit_jump_target_block(&self.stack_loc, func, lbl);
        let target_block = target.block.clone();

        self.builder.build_conditional_branch(
            condition,
            target_block,
            next,
        );

        self.builder.position_at_end(next);
    }

    fn emit_boolean_to_number(&self, bool: IntValue<'ctx>) -> MetaValue<'ctx> {
        let bool_f32 = self.builder.build_unsigned_int_to_float(bool, self.context.f32_type(), "to_f32");
        let res_i32 = self.builder.build_bitcast(bool_f32, self.context.i32_type(), "to_i32").into_int_value();
        return MetaValue::with_tag(ValueTag::Number, res_i32.into(), self);
    }

    pub fn emit(&mut self, ir: &DMIR, func: FunctionValue<'ctx>) {
        match ir {

            // Load src onto stack
            DMIR::GetSrc => {
                // self.dbg("GetSrc");
                self.stack().push(func.get_nth_param(1).unwrap().into_struct_value());
            }
            // Set cache to stack top
            DMIR::SetCache => {
                let arg = self.stack().pop();
                self.cache = Some(arg);
            }
            DMIR::PushCache => {
                let cache = self.cache.unwrap();
                self.stack().push(cache);
            }
            // Read field from cache e.g cache["oxygen"] where "oxygen" is interned string at name_id
            DMIR::GetCacheField(name_id) => {
                let get_var_func = self.module.get_function("<intrinsic>/get_variable").unwrap();

                let receiver_value = self.cache.unwrap();

                let out = self.builder.build_alloca(self.val_type, "field_get_result");
                self.builder.build_call(get_var_func, &[out.into(), receiver_value.into(), self.context.i32_type().const_int(name_id.clone() as u64, false).into()], "get_cache_field");

                let out_value = self.builder.build_load(out, "out_value").into_struct_value();
                self.stack().push(out_value);
                // self.dbg("GetVar");
                // self.dbg_val(self.builder.build_load(out, "load_dbg").into_struct_value());
            }
            DMIR::SetCacheField(name_id) => {
                let value = self.stack().pop();

                let set_var_func = self.module.get_function(INTRINSIC_SET_VARIABLE).unwrap();
                let receiver_value = self.cache.unwrap();

                self.builder.build_call(
                    set_var_func,
                    &[
                        receiver_value.into(),
                        self.context.i32_type().const_int(name_id.clone() as u64, false).into(),
                        value.into()
                    ],
                    "set_cache_field"
                );
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
            DMIR::FloatSub => {
                self.emit_bin_op(|first, second, code_gen| {
                    let first_f32 = code_gen.builder.build_bitcast(first.data, code_gen.context.f32_type(), "first_f32").into_float_value();
                    let second_f32 = code_gen.builder.build_bitcast(second.data, code_gen.context.f32_type(), "second_f32").into_float_value();

                    let result_value = code_gen.builder.build_float_sub(second_f32, first_f32, "sub");
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
            DMIR::FloatCmp(predicate) => {
                self.emit_bin_op(|first, second, code_gen| {
                    let first_f32 = code_gen.builder.build_bitcast(first.data, code_gen.context.f32_type(), "first_f32").into_float_value();
                    let second_f32 = code_gen.builder.build_bitcast(second.data, code_gen.context.f32_type(), "second_f32").into_float_value();

                    let result_value = code_gen.builder.build_float_compare(predicate.clone(), second_f32, first_f32, "test_pred");
                    let result_f32 = code_gen.builder.build_unsigned_int_to_float(result_value, code_gen.context.f32_type(), "bool_to_f32");
                    let result_i32 = code_gen.builder.build_bitcast(result_f32, code_gen.context.i32_type(), "f32_as_i32").into_int_value();

                    MetaValue::with_tag(ValueTag::Number, result_i32.into(), code_gen)
                })
            }
            DMIR::FloatAbs => {
                let arg_value = self.stack().pop();
                let arg = self.emit_load_meta_value(arg_value);
                let arg_num = self.emit_to_number_or_zero(func, arg);
                let fabs_type = self.context.f32_type().fn_type(&[self.context.f32_type().into()], false);

                let fabs_name = "llvm.fabs.f32";

                let fabs =
                    if let Some(func) = self.module.get_function(fabs_name) {
                        func
                    } else {
                        self.module.add_function(fabs_name, fabs_type, None)
                    };
                let result = self.builder.build_call(fabs, &[arg_num.data.into_float_value().into()], "abs").try_as_basic_value().left().unwrap().into_float_value();

                let result_i32 = self.builder.build_bitcast(result, self.context.i32_type(), "cast_result").into_int_value();
                let meta_result = MetaValue::with_tag(ValueTag::Number, result_i32.into(), self);
                let result_value = self.emit_store_meta_value(meta_result);
                self.stack().push(result_value);
            }
            DMIR::CallProcById(proc_id, proc_call_type, arg_count) => {
                let src = self.stack().pop();

                let args = self.emit_store_args_from_stack(arg_count.clone());

                let call_proc_by_id = self.module.get_function(INTRINSIC_CALL_PROC_BY_ID).unwrap();

                let usr = func.get_nth_param(2).unwrap().into_struct_value(); // TODO: Proc can change self usr

                let out = self.builder.build_alloca(self.val_type, "call_result");

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

                let out_value = self.builder.build_load(out, "call_result_value").into_struct_value();
                self.stack().push(out_value);
            }
            DMIR::CallProcByName(string_id, proc_call_type, arg_count) => {
                let src = self.stack().pop();

                let args = self.emit_store_args_from_stack(arg_count.clone());

                let call_proc_by_name = self.module.get_function(INTRINSIC_CALL_PROC_BY_NAME).unwrap();

                let usr = func.get_nth_param(2).unwrap().into_struct_value(); // TODO: Proc can change self usr

                let out = self.builder.build_alloca(self.val_type, "call_result");

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

                let out_value = self.builder.build_load(out, "call_result_value").into_struct_value();
                self.stack().push(out_value);
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

                self.stack().push(out_val);

                // self.dbg("PushInt");
                // self.dbg_val(out_val);
            }
            DMIR::PushVal(op) => {

                let result = MetaValue::new(
                    self.context.i8_type().const_int(op.tag as u64, false),
                    self.context.i32_type().const_int(op.data as u64, false).into(),
                );

                let result_val = self.emit_store_meta_value(result);
                self.stack().push(result_val);
            }
            DMIR::Pop => {
                self.stack().pop();
            }
            // Return stack top from proc
            DMIR::Ret => {

                // self.dbg("Ret");
                let value = self.stack().pop();
                // self.dbg_val(value);

                let out = func.get_nth_param(0).unwrap().into_pointer_value();
                self.builder.build_store(out, value);
                self.block_ended = true;
                self.builder.build_return(None);
            }
            // Set indexed local to stack top
            DMIR::SetLocal(idx) => {
                let value = self.stack().pop();
                let local = self.builder.build_alloca(self.val_type, "local");
                self.builder.build_store(local, value);
                self.locals.insert(idx.clone(), local);
            }
            // Push indexed local value to stack
            DMIR::GetLocal(idx) => {
                let ptr = self.locals.get(&idx).unwrap().clone();
                let value = self.builder.build_load(ptr, "load_local").into_struct_value();
                self.stack().push(value);
            }
            DMIR::GetArg(idx) => {
                let args_pointer = func.get_nth_param(3).unwrap().into_pointer_value();
                let ptr_int = self.context.ptr_sized_int_type(self.execution_engine.get_target_data(), Some(Generic));
                let args_pointer_int = self.builder.build_ptr_to_int(args_pointer, ptr_int, "args_ptr_to_int");
                let size_of = self.val_type.size_of().unwrap();
                let offset_int = size_of.const_mul(size_of.get_type().const_int(idx.clone() as u64, false));
                let result_ptr_int = self.builder.build_int_add(args_pointer_int, self.builder.build_int_cast(offset_int, ptr_int, "conv"), "add_offset");
                let arg_ptr = self.builder.build_int_to_ptr(result_ptr_int, self.val_type.ptr_type(Generic), "final_ptr");
                let arg_value = self.builder.build_load(arg_ptr, "load_arg").into_struct_value();
                self.stack().push(arg_value)
            }
            DMIR::IsNull => {
                let value = self.stack().pop();
                let meta = self.emit_load_meta_value(value);
                let const_null_tag = self.const_tag(ValueTag::Null);
                let result = self.builder.build_int_compare(IntPredicate::EQ, meta.tag, const_null_tag, "check_null");
                let meta_value = self.emit_boolean_to_number(result);
                let result_value = self.emit_store_meta_value(meta_value);

                self.stack().push(result_value);
            }
            DMIR::Test => {
                let value = self.stack().pop();
                let res = self.emit_check_is_true(self.emit_load_meta_value(value));

                self.test_res = Some(res);
            }
            DMIR::Not => {
                let value = self.stack().pop();
                let result = self.emit_check_is_true(self.emit_load_meta_value(value));
                let invert = self.builder.build_not(result, "not");
                let meta_value = self.emit_boolean_to_number(invert);
                let result_value = self.emit_store_meta_value(meta_value);

                self.stack().push(result_value);
            }
            DMIR::PushTestFlag => {
                let test_value_f32 = self.builder.build_signed_int_to_float(self.test_res.unwrap(), self.context.f32_type(), "test_flag_to_f32");
                let test_value_i32 = self.builder.build_bitcast(test_value_f32, self.context.i32_type(), "test_value_i32");
                let test_value = self.emit_store_meta_value(MetaValue::with_tag(ValueTag::Number, test_value_i32, self));
                self.stack().push(test_value)
            }
            DMIR::JZ(lbl) => {
                self.emit_conditional_jump(func, lbl, self.builder.build_not(self.test_res.unwrap(), "jz"))
            }
            DMIR::Dup => {
                let value = self.stack().pop();
                self.stack().push(value);
                self.stack().push(value);
            }
            DMIR::Swap => {
                let a = self.stack().pop();
                let b = self.stack().pop();
                self.stack().push(b);
                self.stack().push(a);
            }
            DMIR::TestJZ(lbl) => {
                let arg_value = self.stack().pop();
                let arg = self.emit_load_meta_value(arg_value);

                let arg_is_true = self.emit_check_is_true(arg);
                let if_arg_false = self.builder.build_not(arg_is_true, "jz");
                self.emit_conditional_jump(func, lbl, if_arg_false)
            }
            DMIR::TestJNZ(lbl) => {
                let arg_value = self.stack().pop();
                let arg = self.emit_load_meta_value(arg_value);

                let arg_is_true = self.emit_check_is_true(arg);
                self.emit_conditional_jump(func, lbl, arg_is_true)
            }
            DMIR::EnterBlock(lbl) => {

                let mut block_builder = BlockBuilder {
                    context: self.context,
                    builder: &self.builder,
                    val_type: &self.val_type,
                    block_map: &mut self.block_map
                };
                let target = if !self.block_ended {
                    block_builder.emit_jump_target_block(&self.stack_loc, func, lbl)
                } else {
                    self.block_map.get(lbl).unwrap()
                };

                self.stack_loc.clear();
                for phi in target.phi.iter() {
                    self.stack_loc.push(phi.as_basic_value().into_struct_value());
                }

                if !self.block_ended {
                    self.builder.build_unconditional_branch(target.block);
                }
                self.block_ended = false;
                self.builder.position_at_end(target.block);
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
                    stack_out_array = self.builder.build_insert_value(stack_out_array, loc.clone(), pos as u32, "store_value_from_stack").unwrap().into_array_value();
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
                let post_deopt_block = self.context.append_basic_block(func, "post_deopt");
                self.builder.position_at_end(post_deopt_block);
            }
            DMIR::CheckTypeDeopt(stack_pos, tag, deopt) => {
                let stack_value = self.stack_loc[self.stack_loc.len() - 1 - (stack_pos.clone() as usize)];
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
                self.builder.build_unconditional_branch(next_block);

                self.builder.position_at_end(next_block);
            }
            _ => {}
        }
    }
}
