use std::borrow::Borrow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

use auxtools::Proc;
use auxtools::raw_types::values::ValueTag;
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use inkwell::AddressSpace::Generic;
use inkwell::attributes::AttributeLoc;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::{Linkage, Module};
use inkwell::types::StructType;
use inkwell::values::{AnyValue, BasicValueEnum, FloatValue, FunctionValue, IntValue, PhiValue, PointerValue, StructValue};

use crate::dmir::{DMIR, RefOpDisposition, ValueLocation, ValueTagPredicate};
use crate::pads;

pub struct CodeGen<'ctx, 'a> {
    context: &'ctx Context,
    pub(crate) module: &'a Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: &'a ExecutionEngine<'ctx>,
    stack_loc: Vec<StructValue<'ctx>>,
    locals: HashMap<u32, StructValue<'ctx>>,
    cache: Option<StructValue<'ctx>>,
    val_type: StructType<'ctx>,
    test_res: Option<IntValue<'ctx>>,
    internal_test_flag: Option<IntValue<'ctx>>,
    block_map: BlockMap<'ctx>,
    block_ended: bool,
    parameter_count: u32,
    local_count: u32,
    args: Option<PointerValue<'ctx>>
}

type BlockMap<'ctx> = HashMap<String, LabelBlockInfo<'ctx>>;

const INTRINSIC_CALL_PROC_BY_ID: &str = "<intrinsic>/call_proc_by_id";
const INTRINSIC_CALL_PROC_BY_NAME: &str = "<intrinsic>/call_proc_by_name";
const INTRINSIC_DEOPT: &str = "<intrinsic>/deopt";
const INTRINSIC_SET_VARIABLE: &str = "<intrinsic>/set_variable";
const INTRINSIC_GET_VARIABLE: &str = "<intrinsic>/get_variable";

const INTRINSIC_INC_REF_COUNT: &str = "<intrinsic>/inc_ref_count";
const INTRINSIC_DEC_REF_COUNT: &str = "<intrinsic>/dec_ref_count";


struct LabelBlockInfo<'ctx> {
    block: BasicBlock<'ctx>,
    phi: Vec<PhiValue<'ctx>>,
    cache_phi: Option<PhiValue<'ctx>>,
    locals_phi: HashMap<u32, PhiValue<'ctx>>
}

#[derive(Clone)]
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

macro_rules! decl_type {
    ($code_gen:ident DMValue) => (
        $code_gen.val_type
    );
    ($code_gen:ident i8_ptr) => (
        $code_gen.context.i8_type().ptr_type(AddressSpace::Generic)
    );
    ($code_gen:ident $arg:ident) => (
        $code_gen.context.$arg()
    );
}

macro_rules! decl_sig {
    ($code_gen:ident ($($arg:tt),+) -> $result:tt) => (
        decl_type!($code_gen $result).fn_type(&[$(decl_type!($code_gen $arg).into()),+], false)
    );
}

macro_rules! decl_intrinsic {
    ($code_gen:ident $name:literal $($sig:tt)+) => ({
        let function =
            if let Some(func) = $code_gen.module.get_function($name) {
                func
            } else {
                let t = decl_sig!($code_gen $($sig)+);
                $code_gen.module.add_function($name, t, None)
            };
        function
    });
}

struct BlockBuilder<'ctx, 'a> {
    context: &'ctx Context,
    builder: &'a Builder<'ctx>,
    val_type: &'a StructType<'ctx>,
    block_map: &'a mut BlockMap<'ctx>
}

impl<'ctx, 'a> BlockBuilder<'ctx, 'a> {

    fn emit_jump_target_block(
        &'a mut self,
        stack_loc: &Vec<StructValue<'ctx>>,
        locals: &HashMap<u32, StructValue<'ctx>>,
        cache_data: &Option<StructValue<'ctx>>,
        func: FunctionValue<'ctx>,
        lbl: &String
    ) -> &'a LabelBlockInfo<'ctx> {
        let current_block = self.builder.get_insert_block().unwrap();
        let stack = stack_loc;

        let val_type = self.val_type;
        let builder = self.builder;

        let label_block_info = match self.block_map.entry(lbl.clone()) {
            Entry::Occupied(v) => {
                v.into_mut()
            }
            Entry::Vacant(v) => {
                let current_block = builder.get_insert_block().unwrap();
                let new_block = self.context.append_basic_block(func, lbl);

                builder.position_at_end(new_block);
                let mut phi: Vec<PhiValue<'ctx>> = Vec::new();
                for _ in 0..stack.len() {
                    phi.push(builder.build_phi(val_type.clone(), "stack_loc"));
                }

                let mut locals_phi = HashMap::new();
                for (idx, _) in locals {
                    locals_phi.insert(idx.clone(), builder.build_phi(val_type.clone(), "local_phi"));
                }

                let cache_phi = if cache_data.is_some() {
                    Option::Some(builder.build_phi(val_type.clone(), "cache_phi"))
                } else {
                    Option::None
                };

                builder.position_at_end(current_block);



                let target_info =
                    LabelBlockInfo {
                        block: new_block,
                        phi,
                        locals_phi,
                        cache_phi
                    };
                v.insert(target_info)
            }
        };


        assert_eq!(stack.len(), label_block_info.phi.len(), "stack unaligned");
        for (idx, loc) in stack.iter().enumerate() {
            label_block_info.phi[idx].add_incoming(&[(loc, current_block)]);
        }

        for (idx, local) in locals {
            let local_phi = label_block_info.locals_phi[&idx];
            local_phi.add_incoming(&[(local, current_block)]);
        }

        if let Some(cache) = cache_data {
            label_block_info.cache_phi.unwrap().add_incoming(&[(cache, current_block)])
        }


        label_block_info
    }
}

impl<'ctx> CodeGen<'ctx, '_> {
    pub fn init_builtins(&self) {
        if self.module.get_function(INTRINSIC_GET_VARIABLE).is_none() {
            {
                let get_variable_signature = self.context.i8_type().fn_type(
                    &[
                        self.val_type.ptr_type(AddressSpace::Generic).into(),
                        self.val_type.into(),
                        self.context.i32_type().into()
                    ],
                    false
                );
                let get_variable_func = self.module.add_function(INTRINSIC_GET_VARIABLE, get_variable_signature, Some(Linkage::External));
                get_variable_func.add_attribute(AttributeLoc::Function, self.context.create_enum_attribute(41, 1));
                self.execution_engine.add_global_mapping(&get_variable_func, auxtools::raw_types::funcs::get_variable as usize);
            }

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

            let list_copy_func_sig = self.val_type.fn_type(&[self.val_type.into()], false);
            let list_copy_func = self.module.add_function("<intrinsic>/list_copy", list_copy_func_sig, Some(Linkage::External));
            self.execution_engine.add_global_mapping(&list_copy_func, pads::lists::list_copy as usize);

            let list_remove_func_sig = self.context.void_type().fn_type(&[self.val_type.into(), self.val_type.into()], false);
            let list_remove_func = self.module.add_function("<intrinsic>/list_remove", list_remove_func_sig, Some(Linkage::External));
            self.execution_engine.add_global_mapping(&list_remove_func, pads::lists::list_remove as usize);

            let list_append_func_sig = self.context.void_type().fn_type(&[self.val_type.into(), self.val_type.into()], false);
            let list_append_func = self.module.add_function("<intrinsic>/list_append", list_append_func_sig, Some(Linkage::External));
            self.execution_engine.add_global_mapping(&list_append_func, pads::lists::list_append as usize);

            let list_indexed_get_func_sig = self.val_type.fn_type(&[self.val_type.into(), self.context.i32_type().into()], false);
            let list_indexed_get_func = self.module.add_function("<intrinsic>/list_indexed_get", list_indexed_get_func_sig, Some(Linkage::External));
            self.execution_engine.add_global_mapping(&list_indexed_get_func, pads::lists::list_indexed_get as usize);

            let list_indexed_set_func_sig = self.context.void_type().fn_type(&[self.val_type.into(), self.context.i32_type().into(), self.val_type.into()], false);
            let list_indexed_set_func = self.module.add_function("<intrinsic>/list_indexed_set", list_indexed_set_func_sig, Some(Linkage::External));
            self.execution_engine.add_global_mapping(&list_indexed_set_func, pads::lists::list_indexed_set as usize);

            let list_associative_get_func_sig = self.val_type.fn_type(&[self.val_type.into(), self.val_type.into()], false);
            let list_associative_get_func = self.module.add_function("<intrinsic>/list_associative_get", list_associative_get_func_sig, Some(Linkage::External));
            self.execution_engine.add_global_mapping(&list_associative_get_func, pads::lists::list_associative_get as usize);

            let list_associative_set_func_sig = self.context.void_type().fn_type(&[self.val_type.into(), self.val_type.into(), self.val_type.into()], false);
            let list_associative_set_func = self.module.add_function("<intrinsic>/list_associative_set", list_associative_set_func_sig, Some(Linkage::External));
            self.execution_engine.add_global_mapping(&list_associative_set_func, pads::lists::list_associative_set as usize);

            let list_check_size_func_sig = self.context.bool_type().fn_type(&[self.val_type.into(), self.context.i32_type().into()], false);
            let list_check_size_func = self.module.add_function("<intrinsic>/list_check_size", list_check_size_func_sig, Some(Linkage::External));
            self.execution_engine.add_global_mapping(&list_check_size_func, pads::lists::list_check_size as usize);
            
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

            {
                let inc_ref_count_sig = self.context.i8_type().fn_type(&[
                    self.val_type.into()
                ], false);
                let inc_ref_count_func = self.module.add_function(INTRINSIC_INC_REF_COUNT, inc_ref_count_sig, None);
                self.execution_engine.add_global_mapping(&inc_ref_count_func, auxtools::raw_types::funcs::inc_ref_count as usize)
            }

            {
                let dec_ref_count_sig = self.context.i8_type().fn_type(&[
                    self.val_type.into()
                ], false);
                let dec_ref_count_func = self.module.add_function(INTRINSIC_DEC_REF_COUNT, dec_ref_count_sig, None);
                self.execution_engine.add_global_mapping(&dec_ref_count_func, auxtools::raw_types::funcs::dec_ref_count as usize)
            }
        }
    }

    pub fn create<'a>(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: Builder<'ctx>,
        execution_engine: &'a ExecutionEngine<'ctx>,
        parameter_count: u32,
        local_count: u32,
    ) -> CodeGen<'ctx, 'a> {

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
            internal_test_flag: None,
            block_map: HashMap::new(),
            block_ended: false,
            parameter_count,
            local_count,
            args: Option::None
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
                ptr.into(),
                self.context.i32_type().into()
            ],
            false,
        );

        // register function in LLVM
        let func = self.module.add_function(name, ft, None);

        // create start basic block for our function
        let block = self.context.append_basic_block(func, "base");
        self.builder.position_at_end(block);
        if cfg!(debug_on_call_print) {
            self.dbg(format!("llvm function call {}", name).as_str());
        }

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

    fn emit_to_number_or_zero(&self, func: FunctionValue<'ctx>, value: MetaValue<'ctx>) -> FloatValue<'ctx> {
        let before = self.builder.get_insert_block().unwrap();
        let iff_number = self.context.append_basic_block(func, "iff_number");
        let next = self.context.append_basic_block(func, "next");

        let zero_value = self.context.f32_type().const_zero();

        let number_tag = self.const_tag(ValueTag::Number);

        self.builder.build_conditional_branch(
            self.builder.build_int_compare(IntPredicate::EQ, value.tag, number_tag, "check_number"),
            iff_number,
            next,
        );
        let number_value = {
            self.builder.position_at_end(iff_number);
            let r = self.builder.build_bitcast(value.data, self.context.f32_type(), "cast_float").into_float_value();
            self.builder.build_unconditional_branch(next);
            r
        };

        self.builder.position_at_end(next);
        let out_phi = self.builder.build_phi(self.context.f32_type(), "to_number_or_zero_res");
        out_phi.add_incoming(&[(&zero_value, before), (&number_value, iff_number)]);

        return out_phi.as_basic_value().into_float_value()
    }

    fn emit_null_to_zero(&mut self, func: FunctionValue<'ctx>, value: MetaValue<'ctx>) -> MetaValue<'ctx> {
        let tag_check = self.emit_tag_predicate_comparison(value.tag, &ValueTagPredicate::Tag(ValueTag::Null));

        let iff_null_block = self.context.append_basic_block(func, "iff_null");
        let next_block = self.context.append_basic_block(func, "next_block");
        let prev_block = self.builder.get_insert_block().unwrap();

        let original_value_packed = self.emit_store_meta_value(value);

        self.builder.build_conditional_branch(
            tag_check,
            iff_null_block,
            next_block
        );

        self.builder.position_at_end(iff_null_block);
        let zero_i32 = self.builder.build_bitcast(self.context.f32_type().const_zero(), self.context.i32_type(), "zero_to_int").into_int_value();
        let literal_zero = MetaValue::with_tag(ValueTag::Number, zero_i32.into(), self);
        let literal_zero_packed = self.emit_store_meta_value(literal_zero);
        self.builder.build_unconditional_branch(next_block);

        self.builder.position_at_end(next_block);
        let phi = self.builder.build_phi(self.val_type, "merge_result");
        phi.add_incoming(&[(&original_value_packed, prev_block), (&literal_zero_packed, iff_null_block)]);

        self.emit_load_meta_value(phi.as_basic_value().into_struct_value())
    }

    fn emit_store_args_from_stack(&mut self, arg_count: u32) -> PointerValue<'ctx> {
        let out_stack_type = self.val_type.array_type(arg_count as u32);
        let args = self.builder.build_alloca(out_stack_type, "out_stack");
        let mut stack_out_array = out_stack_type.const_zero();
        for idx in (0..arg_count).rev() {
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
        let target = block_builder.emit_jump_target_block(&self.stack_loc, &self.locals, &self.cache, func, lbl);
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

    fn emit_inc_ref_count(&self, value: StructValue<'ctx>) {
        let func = self.module.get_function(INTRINSIC_INC_REF_COUNT).unwrap();
        self.builder.build_call(func, &[value.into()], "call_inc_ref_count");
    }

    fn emit_dec_ref_count(&self, value: StructValue<'ctx>) {
        let func = self.module.get_function(INTRINSIC_DEC_REF_COUNT).unwrap();
        self.builder.build_call(func, &[value.into()], "call_dec_ref_count");
    }

    fn emit_read_value_location(&self, location: &ValueLocation) -> StructValue<'ctx> {
        match location {
            ValueLocation::Stack(rel) => {
                self.stack_loc[self.stack_loc.len() - 1 - (*rel as usize)]
            }
            ValueLocation::Cache => {
                self.cache.unwrap()
            }
            ValueLocation::Local(idx) => {
                self.locals.get(idx).unwrap().clone()
            }
        }
    }

    fn emit_load_argument(&self, idx: u32) -> StructValue<'ctx> {
        let arg_ptr = self.emit_load_argument_pointer(idx);
        let arg_value = self.builder.build_load(arg_ptr, "load_arg").into_struct_value();
        return arg_value
    }

    fn emit_load_argument_pointer(&self, idx: u32) -> PointerValue<'ctx> {
        let args_pointer = self.args.unwrap();
        let ptr_int = self.context.ptr_sized_int_type(self.execution_engine.get_target_data(), Some(Generic));
        let args_pointer_int = self.builder.build_ptr_to_int(args_pointer, ptr_int, "args_ptr_to_int");
        let size_of = self.val_type.size_of().unwrap();
        let offset_int = size_of.const_mul(size_of.get_type().const_int(idx.clone() as u64, false));
        let result_ptr_int = self.builder.build_int_add(args_pointer_int, self.builder.build_int_cast(offset_int, ptr_int, "conv"), "add_offset");
        self.builder.build_int_to_ptr(result_ptr_int, self.val_type.ptr_type(Generic), "final_ptr")
    }

    fn emit_epilogue(&self) {
        let parameter_count = self.parameter_count;
        for param in 0..parameter_count {
            let arg = self.emit_load_argument(param);
            self.emit_dec_ref_count(arg);
        }
    }

    fn emit_tag_predicate_comparison(&self, actual_tag: IntValue<'ctx>, predicate: &ValueTagPredicate) -> IntValue<'ctx> {
        match predicate {
            ValueTagPredicate::Any => self.context.bool_type().const_int(1, false),
            ValueTagPredicate::None => self.context.bool_type().const_int(0, false),
            ValueTagPredicate::Tag(tag) => self.builder.build_int_compare(IntPredicate::EQ, actual_tag, self.const_tag(tag.clone()), "check_tag"),
            ValueTagPredicate::Union(tags) => {
                let mut last_op = self.context.bool_type().const_int(1, false);
                for pred in tags.iter() {
                    last_op = self.builder.build_or(
                        last_op,
                        self.emit_tag_predicate_comparison(actual_tag, pred),
                        "or"
                    );
                }
                last_op
            }
        }

    }

    pub fn emit_prologue(&mut self, func: FunctionValue<'ctx>) {
        let val_ptr_type = self.val_type.ptr_type(AddressSpace::Generic);

        self.builder.build_store(func.get_nth_param(0).unwrap().into_pointer_value(), self.val_type.const_zero()); // initialize out

        let caller_arg_count = func.get_nth_param(4).unwrap().into_int_value();
        let args_ptr = func.get_nth_param(3).unwrap().into_pointer_value();
        let ptr_int = self.context.ptr_sized_int_type(self.execution_engine.get_target_data(), Some(Generic));
        let args_ptr_int = self.builder.build_ptr_to_int(args_ptr, ptr_int, "arg_ptr_int");

        let copy_args_block = self.context.append_basic_block(func, "copy_args");
        let copy_arg_check_block = self.context.append_basic_block(func, "copy_arg_check");
        let copy_arg_block = self.context.append_basic_block(func, "copy_arg");
        let end_block = self.context.append_basic_block(func, "end_prologue");

        let param_count_const = self.context.i32_type().const_int(self.parameter_count as u64, false);

        let entry_block = self.builder.get_insert_block().unwrap();

        // if args.count < parameter_count
        self.builder.build_conditional_branch(
            self.builder.build_int_compare(IntPredicate::ULT, caller_arg_count, param_count_const, "check_arg_count"),
            copy_args_block,
            end_block
        );

        self.builder.position_at_end(copy_args_block);
        let copied_args = self.builder.build_alloca(self.val_type.array_type(self.parameter_count), "arg_array_copy");
        self.builder.build_store(copied_args, self.val_type.array_type(self.parameter_count).const_zero());
        let copied_args_cast = self.builder.build_pointer_cast(copied_args, val_ptr_type, "copied_args_ptr_cast");
        self.builder.build_unconditional_branch(copy_arg_check_block);


        // while (i < arg_count)
        //      copied_args[i] = *(args + i)
        //      i++
        self.builder.position_at_end(copy_arg_check_block);
        let phi_i = self.builder.build_phi(self.context.i32_type(), "i");
        phi_i.add_incoming(&[(&self.context.i32_type().const_zero(), copy_args_block)]);

        let i = phi_i.as_basic_value().into_int_value();
        self.builder.build_conditional_branch(
            self.builder.build_int_compare(IntPredicate::ULT, i, caller_arg_count, "check_i_lt_arg_count"),
            copy_arg_block,
            end_block
        );
        self.builder.position_at_end(copy_arg_block);

        let arg_src_ptr = self.builder.build_int_to_ptr(
            self.builder.build_int_add(args_ptr_int, i, "args_plus_i"),
            self.val_type.ptr_type(Generic),
            "arg_ptr"
        );
        let arg_value = self.builder.build_load(arg_src_ptr, "load_arg");
        let arg_dst_ptr = self.builder.build_int_to_ptr(
            self.builder.build_int_add(
                self.builder.build_ptr_to_int(copied_args, ptr_int, "to_ptr"),
                i,
                "copy_args_plus_i"
            ),
            self.val_type.ptr_type(Generic),
            "copy_arg_ptr"
        );
        self.builder.build_store(arg_dst_ptr, arg_value);
        let next_i = self.builder.build_int_add(i, self.context.i32_type().const_int(1, false), "i_plus_1");
        phi_i.add_incoming(&[(&next_i, copy_arg_block)]);
        self.builder.build_unconditional_branch(copy_arg_check_block);

        self.builder.position_at_end(end_block);
        let args_ptr_phi = self.builder.build_phi(val_ptr_type, "args_ptr");
        args_ptr_phi.add_incoming(&[(&args_ptr, entry_block), (&copied_args_cast, copy_arg_check_block)]);

        self.args = Option::Some(args_ptr_phi.as_basic_value().into_pointer_value())
    }

    pub fn emit(&mut self, ir: &DMIR, func: FunctionValue<'ctx>) {

        match ir {
            DMIR::Nop => {}
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
                let get_var_func = self.module.get_function(INTRINSIC_GET_VARIABLE).unwrap();

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
            DMIR::ValueTagSwitch(location, cases) => {
                let value = self.emit_read_value_location(location);
                let meta_value = self.emit_load_meta_value(value);
                let mut jumps = Vec::new();
                let mut default = Option::None;
                for (predicate, block) in cases.as_ref() {
                    let mut predicates = Vec::new();
                    fn to_linear(predicate: &ValueTagPredicate, out: &mut Vec<ValueTagPredicate>) {
                        match predicate {
                            ValueTagPredicate::Union(values) => {
                                for i in values {
                                    to_linear(i, out)
                                }
                            }
                            _ => { out.push(predicate.clone()) }
                        }
                    }
                    to_linear(predicate, &mut predicates);

                    for predicate in predicates {
                        let mut block_builder = BlockBuilder {
                            context: self.context,
                            builder: &self.builder,
                            val_type: &self.val_type,
                            block_map: &mut self.block_map,
                        };
                        let target = block_builder.emit_jump_target_block(&self.stack_loc, &self.locals, &self.cache, func, block).block;
                        if let ValueTagPredicate::Tag(t) = predicate {
                            jumps.push((self.const_tag(t), target))
                        } else if matches!(predicate, ValueTagPredicate::Any) {
                            default = Option::Some(target)
                        }
                    }
                }
                self.builder.build_switch(meta_value.tag, default.unwrap(), &jumps);
                self.block_ended = true;
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
                    let second_f32 = code_gen.emit_to_number_or_zero(func, second);

                    let result_value = code_gen.builder.build_float_sub(second_f32, first_f32, "sub");
                    let result_i32 = code_gen.builder.build_bitcast(result_value, code_gen.context.i32_type(), "result_i32").into_int_value();

                    MetaValue::with_tag(ValueTag::Number, result_i32.into(), code_gen)
                })
            }
            DMIR::FloatMul => {
                self.emit_bin_op(|first, second, code_gen| {
                    let first_f32 = code_gen.emit_to_number_or_zero(func, first);
                    let second_f32 = code_gen.builder.build_bitcast(second.data, code_gen.context.f32_type(), "first_f32").into_float_value();

                    let result_value = code_gen.builder.build_float_mul(first_f32, second_f32, "mul");
                    let result_i32 = code_gen.builder.build_bitcast(result_value, code_gen.context.i32_type(), "result_i32").into_int_value();
                    MetaValue::with_tag(ValueTag::Number, result_i32.into(), code_gen)
                });
            }
            DMIR::FloatDiv => {
                self.emit_bin_op(|first, second, code_gen| {
                    let first_f32 = code_gen.builder.build_bitcast(first.data, code_gen.context.f32_type(), "first_f32").into_float_value();
                    let second_f32 = code_gen.builder.build_bitcast(second.data, code_gen.context.f32_type(), "second_f32").into_float_value();

                    let result_value = code_gen.builder.build_float_div(second_f32, first_f32, "div");
                    let result_i32 = code_gen.builder.build_bitcast(result_value, code_gen.context.i32_type(), "result_i32").into_int_value();
                    MetaValue::with_tag(ValueTag::Number, result_i32.into(), code_gen)
                })
            }
            DMIR::FloatCmp(predicate) => {
                self.emit_bin_op(|first, second, code_gen| {
                    let first_not_null = code_gen.emit_null_to_zero(func, first);
                    let second_not_null = code_gen.emit_null_to_zero(func, second);
                    let first_f32 = code_gen.builder.build_bitcast(first_not_null.data, code_gen.context.f32_type(), "first_f32").into_float_value();
                    let second_f32 = code_gen.builder.build_bitcast(second_not_null.data, code_gen.context.f32_type(), "second_f32").into_float_value();

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

                let fabs = decl_intrinsic!(self "llvm.fabs.f32" (f32_type) -> f32_type);

                let result = self.builder.build_call(fabs, &[arg_num.into()], "abs").try_as_basic_value().left().unwrap().into_float_value();

                let result_i32 = self.builder.build_bitcast(result, self.context.i32_type(), "cast_result").into_int_value();
                let meta_result = MetaValue::with_tag(ValueTag::Number, result_i32.into(), self);
                let result_value = self.emit_store_meta_value(meta_result);
                self.stack().push(result_value);
            }
            DMIR::RoundN => {
                self.emit_bin_op(|first, second, code_gen| {
                    let first_f32 = code_gen.emit_to_number_or_zero(func, first.clone());
                    let second_f32 = code_gen.emit_to_number_or_zero(func, second.clone());
                    // first_f32: round to
                    // second_f32: value to round
                    let divided = code_gen.builder.build_float_div(second_f32, first_f32, "div");
                    let divided_f64 =
                        code_gen.builder.build_float_ext(divided, code_gen.context.f64_type(), "extend_to_double");

                    let value_to_floor = code_gen.builder.build_float_add(divided_f64, code_gen.context.f64_type().const_float(0.5), "add_const");

                    let floor = decl_intrinsic!(code_gen "llvm.floor.f64" (f64_type) -> f64_type);
                    let discrete_counts = code_gen.builder.build_call(floor, &[value_to_floor.into()], "call_floor").try_as_basic_value().left().unwrap().into_float_value();
                    let discrete_counts_f32 = code_gen.builder.build_float_trunc(discrete_counts, code_gen.context.f32_type(), "trunc_to_float");
                    let result = code_gen.builder.build_float_mul(discrete_counts_f32, first_f32, "mul");
                    let result_i32 = code_gen.builder.build_bitcast(result, code_gen.context.i32_type(), "cast_result").into_int_value();

                    MetaValue::with_tag(ValueTag::Number, result_i32.into(), code_gen)
                })
            }
            DMIR::ListCopy => {
                let list_struct = self.stack().pop();

                let list_copy = self.module.get_function("<intrinsic>/list_copy").unwrap();

                let result = self.builder.build_call(
                    list_copy,
                    &[
                        list_struct.into()
                    ], "list_copy").as_any_value_enum().into_struct_value();

                self.stack().push(result);
            }
            DMIR::ListAddSingle => {
                let value_struct = self.stack().pop();
                let list_struct = self.stack().pop();

                let list_append = self.module.get_function("<intrinsic>/list_append").unwrap();

                self.builder.build_call(
                    list_append,
                    &[
                        list_struct.into(),
                        value_struct.into()
                    ], "list_append");
            }
            DMIR::ListSubSingle => {
                let value_struct = self.stack().pop();
                let list_struct = self.stack().pop();

                let list_remove = self.module.get_function("<intrinsic>/list_remove").unwrap();

                self.builder.build_call(
                    list_remove,
                    &[
                        list_struct.into(),
                        value_struct.into()
                    ], "list_remove");
            }
            DMIR::ListIndexedGet => {
                let index_struct = self.stack().pop();
                let list_struct = self.stack().pop();

                let index_meta = self.emit_load_meta_value(index_struct);


                let index_f32 = self.builder.build_bitcast(index_meta.data, self.context.f32_type(), "cast_to_float").into_float_value();

                let index_i32 = self.builder.build_float_to_signed_int(index_f32, self.context.i32_type(), "float_to_int");

                let list_indexed_get = self.module.get_function("<intrinsic>/list_indexed_get").unwrap();

                let result = self.builder.build_call(
                    list_indexed_get,
                    &[
                        list_struct.into(),
                        index_i32.into()
                    ], "list_indexed_get").as_any_value_enum().into_struct_value();

                self.stack().push(result);
            }
            DMIR::ListIndexedSet => {
                let index_struct = self.stack().pop();
                let list_struct = self.stack().pop();
                let value_struct = self.stack().pop();

                let index_meta = self.emit_load_meta_value(index_struct);


                let index_f32 = self.builder.build_bitcast(index_meta.data, self.context.f32_type(), "cast_to_float").into_float_value();

                let index_i32 = self.builder.build_float_to_signed_int(index_f32, self.context.i32_type(), "float_to_int");

                let list_indexed_set = self.module.get_function("<intrinsic>/list_indexed_set").unwrap();

                self.builder.build_call(
                    list_indexed_set,
                    &[
                        list_struct.into(),
                        index_i32.into(),
                        value_struct.into()
                    ], "list_indexed_set");
            }
            DMIR::ListAssociativeGet => {
                let index_struct = self.stack().pop();
                let list_struct = self.stack().pop();

                let list_indexed_get = self.module.get_function("<intrinsic>/list_associative_get").unwrap();

                let result = self.builder.build_call(
                    list_indexed_get,
                    &[
                        list_struct.into(),
                        index_struct.into()
                    ], "list_associative_get").as_any_value_enum().into_struct_value();

                self.stack().push(result);
            }
            DMIR::ListAssociativeSet => {
                let index_struct = self.stack().pop();
                let list_struct = self.stack().pop();
                let value_struct = self.stack().pop();

                let list_indexed_set = self.module.get_function("<intrinsic>/list_associative_set").unwrap();

                self.builder.build_call(
                    list_indexed_set,
                    &[
                        list_struct.into(),
                        index_struct.into(),
                        value_struct.into()
                    ], "list_associative_set");
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
                // val is int, so convert it to float as all values in byond are floats, then bit-cast to store it within DMValue
                let value = self.builder.build_bitcast(
                    self.context.f32_type().const_float((*val) as f64),
                    self.context.i32_type(),
                    "f32_to_i32"
                );
                let out_val = self.val_type.const_named_struct(
                    &[
                        self.const_tag(ValueTag::Number).into(),
                        value.into(),
                    ]
                );

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
                self.emit_epilogue();
                self.builder.build_return(None);
            }
            // Set indexed local to stack top
            DMIR::SetLocal(idx) => {
                let value = self.stack().pop();
                self.locals.insert(idx.clone(), value);
            }
            // Push indexed local value to stack
            DMIR::GetLocal(idx) => {
                let value = self.locals.get(&idx).unwrap().clone();
                self.stack().push(value);
            }
            DMIR::GetArg(idx) => {
                let arg = self.emit_load_argument(idx.clone());
                self.stack().push(arg);
            }
            DMIR::SetArg(idx) => {
                let new_value = self.stack().pop();
                let arg_ptr = self.emit_load_argument_pointer(idx.clone());
                self.builder.build_store(arg_ptr, new_value);
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
            DMIR::TestEqual => {
                let second = self.stack().pop();
                let first = self.stack().pop();

                let first_meta = self.emit_load_meta_value(first);
                let second_meta = self.emit_load_meta_value(second);

                let const_tag_num = self.const_tag(ValueTag::Number);

                let is_number = self.builder.build_int_compare(IntPredicate::EQ, first_meta.tag, const_tag_num, "is_number");

                let total_eq =
                    self.builder.build_and(
                        self.builder.build_int_compare(IntPredicate::EQ, first_meta.tag, second_meta.tag, "check_tag"),
                        self.builder.build_int_compare(
                            IntPredicate::EQ,
                            first_meta.data.into_int_value(),
                            second_meta.data.into_int_value(),
                            "check_data"
                        ),
                        "total_eq"
                    );

                let first_f32 = self.builder.build_bitcast(first_meta.data, self.context.f32_type(), "first_f32").into_float_value();
                let second_f32 = self.builder.build_bitcast(second_meta.data, self.context.f32_type(), "second_f32").into_float_value();

                let result = self.builder.build_select(
                    is_number,
                    self.builder.build_float_compare(FloatPredicate::UEQ, first_f32, second_f32, "compare_num"),
                    total_eq,
                    "select_compare"
                ).into_int_value();

                self.test_res = Some(result);
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
            DMIR::DupX1 => {
                let b = self.stack().pop();
                let a = self.stack().pop();

                self.stack().push(b);
                self.stack().push(a);
                self.stack().push(b);
            }
            DMIR::Swap => {
                let a = self.stack().pop();
                let b = self.stack().pop();
                self.stack().push(a);
                self.stack().push(b);
            }
            DMIR::TestInternal => {
                let arg_value = self.stack().pop();
                let arg = self.emit_load_meta_value(arg_value);

                self.internal_test_flag = Some(self.emit_check_is_true(arg));
            }
            DMIR::JZInternal(lbl) => {
                let if_arg_false = self.builder.build_not(self.internal_test_flag.unwrap(), "jz");
                self.emit_conditional_jump(func, lbl, if_arg_false)
            }
            DMIR::JNZInternal(lbl) => {
                self.emit_conditional_jump(func, lbl, self.internal_test_flag.unwrap())
            }
            DMIR::Jmp(lbl) => {
                let mut block_builder = BlockBuilder {
                    context: self.context,
                    builder: &self.builder,
                    val_type: &self.val_type,
                    block_map: &mut self.block_map
                };
                let target = block_builder.emit_jump_target_block(&self.stack_loc, &self.locals, &self.cache, func, lbl);
                self.builder.build_unconditional_branch(target.block);
                self.block_ended = true;
            }
            DMIR::EnterBlock(lbl) => {
                assert!(self.block_ended, "no direct fallthrough allowed");
                let target = self.block_map.get(lbl).unwrap();

                self.test_res = None;
                self.stack_loc.clear();
                self.locals.clear();
                for phi in target.phi.iter() {
                    self.stack_loc.push(phi.as_basic_value().into_struct_value());
                }

                for (idx, phi) in target.locals_phi.iter() {
                    self.locals.insert(idx.clone(), phi.as_basic_value().into_struct_value());
                }

                if let Some(cache) = target.cache_phi {
                    self.cache = Option::Some(cache.as_basic_value().into_struct_value())
                }

                if !self.block_ended {
                    self.builder.build_unconditional_branch(target.block);
                }
                self.block_ended = false;
                self.builder.position_at_end(target.block);
            }
            DMIR::End => {
                if !self.block_ended {
                    self.emit_epilogue();
                    let out = func.get_nth_param(0).unwrap().into_pointer_value();
                    self.builder.build_store(out, self.val_type.const_zero());
                    self.builder.build_return(None);
                }
                self.block_ended = true;
            }
            DMIR::Deopt(offset, proc_id) => {
                let out_stack_type = self.val_type.array_type(self.stack_loc.len() as u32);
                let stack_out_ptr = self.builder.build_alloca(out_stack_type, "out_stack");
                let mut stack_out_array = out_stack_type.const_zero();
                for (pos, loc) in self.stack_loc.iter().enumerate() {
                    stack_out_array = self.builder.build_insert_value(stack_out_array, loc.clone(), pos as u32, "store_value_from_stack").unwrap().into_array_value();
                }
                self.builder.build_store(stack_out_ptr, stack_out_array);

                let local_count = self.local_count;

                let out_locals_type = self.val_type.array_type(local_count as u32);
                let locals_out_ptr = self.builder.build_alloca(out_locals_type, "out_locals");
                let mut locals_out_array = out_locals_type.const_zero();
                for (idx, loc) in &self.locals {
                    locals_out_array = self.builder.build_insert_value(locals_out_array, loc.clone(), *idx, "store_value_from_local").unwrap().into_array_value();
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
                        self.builder.build_pointer_cast(stack_out_ptr, self.val_type.ptr_type(Generic), "cast").into(), //stack: *const auxtools::raw_types::values::Value,
                        self.context.i32_type().const_int(self.stack_loc.len() as u64, false).into(), //stack_size: u32,
                        self.cache.unwrap_or(self.val_type.const_zero()).into(), //cached_datum: auxtools::raw_types::values::Value,
                        func.get_nth_param(1).unwrap().into(), //src: auxtools::raw_types::values::Value,
                        self.args.unwrap().into(), //args: *const auxtools::raw_types::values::Value,
                        self.context.i32_type().const_int(parameter_count.len() as u64, false).into(), //args_count: u32,
                        self.builder.build_pointer_cast(locals_out_ptr, self.val_type.ptr_type(Generic), "cast").into(), //locals: *const auxtools::raw_types::values::Value,
                        self.context.i32_type().const_int(local_count as u64, false).into(), //locals_count: u32
                    ],
                    "call_deopt",
                );

                self.builder.build_return(None);
                let post_deopt_block = self.context.append_basic_block(func, "post_deopt");
                self.builder.position_at_end(post_deopt_block);
            }
            DMIR::CheckTypeDeopt(stack_pos, predicate, deopt) => {
                let stack_value = self.stack_loc[self.stack_loc.len() - 1 - (stack_pos.clone() as usize)];
                let actual_tag = self.emit_load_meta_value(stack_value).tag;

                let next_block = self.context.append_basic_block(func, "next");
                let deopt_block = self.context.append_basic_block(func, "deopt");


                self.builder.build_conditional_branch(
                    self.emit_tag_predicate_comparison(actual_tag, predicate),
                    next_block,
                    deopt_block,
                );

                self.builder.position_at_end(deopt_block);
                if cfg!(debug_deopt_print) {
                    self.dbg(format!("CheckType({}, {:?}, {:?}) failed: ", stack_pos, predicate, deopt).as_str());
                    self.dbg_val(stack_value);
                }
                self.emit(deopt.borrow(), func);
                self.builder.build_unconditional_branch(next_block);

                self.builder.position_at_end(next_block);
            }
            DMIR::ListCheckSizeDeopt(list, index, deopt) =>{
                let list_struct = self.emit_read_value_location(list);
                let index_struct = self.emit_read_value_location(index);

                let index_meta = self.emit_load_meta_value(index_struct);


                let index_f32 = self.builder.build_bitcast(index_meta.data, self.context.f32_type(), "cast_to_float").into_float_value();

                let index_i32 = self.builder.build_float_to_signed_int(index_f32, self.context.i32_type(), "float_to_int");

                let list_check_size = self.module.get_function("<intrinsic>/list_check_size").unwrap();

                let result = self.builder.build_call(
                    list_check_size,
                    &[
                        list_struct.into(),
                        index_i32.into()
                    ], "list_check_size").as_any_value_enum().into_int_value();

                let next_block = self.context.append_basic_block(func, "next");
                let deopt_block = self.context.append_basic_block(func, "deopt");

                self.builder.build_conditional_branch(
                    result,
                    next_block,
                    deopt_block,
                );

                self.builder.position_at_end(deopt_block);
                if cfg!(debug_deopt_print) {
                    self.dbg(format!("CheckListSize({:?}, {:?}, {:?}) failed: ", list, index, deopt).as_str());
                    self.dbg_val(list_struct);
                }
                self.emit(deopt.borrow(), func);
                self.builder.build_unconditional_branch(next_block);

                self.builder.position_at_end(next_block);
            }
            DMIR::IncRefCount { target, op } => {
                if self.block_ended {
                    return;
                }
                match target {
                    RefOpDisposition::DupPost(location) => {
                        let value = self.emit_read_value_location(location);
                        self.emit(op, func);
                        self.emit_inc_ref_count(value);
                    }
                    RefOpDisposition::Post(location) => {
                        self.emit(op, func);
                        let value = self.emit_read_value_location(location);
                        self.emit_inc_ref_count(value);
                    }
                    RefOpDisposition::Pre(location) => {
                        let value = self.emit_read_value_location(location);
                        self.emit_inc_ref_count(value);
                        self.emit(op, func);
                    }
                }
            }
            DMIR::DecRefCount { target, op } => {
                if self.block_ended {
                    return;
                }
                match target {
                    RefOpDisposition::DupPost(location) => {
                        let value = self.emit_read_value_location(location);
                        self.emit(op, func);
                        self.emit_dec_ref_count(value);
                    }
                    RefOpDisposition::Post(location) => {
                        self.emit(op, func);
                        let value = self.emit_read_value_location(location);
                        self.emit_dec_ref_count(value);
                    }
                    RefOpDisposition::Pre(location) => {
                        let value = self.emit_read_value_location(location);
                        self.emit_dec_ref_count(value);
                        self.emit(op, func);
                    }
                }
            }
            DMIR::UnsetCache => {
                self.cache = Option::None;
            }
            DMIR::UnsetLocal(idx) => {
                self.locals.remove(idx);
            }
            _ => {}
        }
    }
}
