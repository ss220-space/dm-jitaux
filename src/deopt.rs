extern crate libc;

use auxtools::raw_types::funcs::CURRENT_EXECUTION_CONTEXT;
use auxtools::raw_types::values::{ValueData, ValueTag};
use auxtools::raw_types::strings::StringId;
use auxtools::raw_types::procs::{ExecutionContext, ProcInstance};
use std::ffi::c_void;

pub(crate) static mut EXECUTE_INSTRUCTION: *const c_void = std::ptr::null();

unsafe fn call_execute_instruction(context: *mut ExecutionContext) {
    asm!(
        "mov eax, {0}",
        "jmp ecx",
        in(reg) context, in("ecx") EXECUTE_INSTRUCTION
    );
}

#[no_mangle]
pub extern "C" fn deopt(
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
) {

    log::debug!("Deopt called entry {:?}", proc_id);
    unsafe {
        let context = {
            let size = std::mem::size_of::<ExecutionContext>() as libc::size_t;
            let r = libc::malloc(size);
            libc::memset(r, 0, size);
            r as *mut ExecutionContext
        };

        let proc = {
            let size = std::mem::size_of::<ProcInstance>() as libc::size_t;
            let r = libc::malloc(size);
            libc::memset(r, 0, size);
            r as *mut ProcInstance
        };


        (*proc).proc = proc_id;
        (*proc).flags = proc_flags;
        (*proc).mega_hack = 0;
        (*proc).usr = (*(**CURRENT_EXECUTION_CONTEXT).proc_instance).usr; // TODO?
        (*proc).src = src;
        (*proc).context = context;
        (*proc).arglist_idx = ValueData { id: 0 }; // TODO?
        (*proc).callback = None;
        (*proc).callback_value = 0;

        (*proc).args_count = args_count;
        (*proc).args = put_to_data_store(&mut (*proc).data_store, args, args_count);
        (*proc).time_to_resume = 0.;


        (*context).proc_instance = proc;
        (*context).parent_context = (*CURRENT_EXECUTION_CONTEXT);
        (*context).filename = StringId(0xffff);
        (*context).line = 0;
        let (bytecode_ptr, _) = auxtools::Proc::from_id(proc_id).unwrap().bytecode_mut_ptr();
        (*context).bytecode = bytecode_ptr;
        (*context).bytecode_offset = offset as u16;

        (*context).test_flag = test_flag;
        (*context).cached_datum = cached_datum;
        (*context).dmvalue_0x20 = auxtools::raw_types::values::Value {
            tag: ValueTag::Null,
            data: ValueData {
                id: 0x0
            }
        };
        let mut cached_values = [auxtools::raw_types::values::Value { tag: ValueTag::Null, data: ValueData { id: 0x0 } }; 64];

        let mut dmvalue_ptr_2c: auxtools::raw_types::values::Value = auxtools::raw_types::values::Value { tag: ValueTag::Null, data: ValueData { id: 0x0 } };

        (*context).cached_values = (&mut cached_values) as *mut auxtools::raw_types::values::Value;
        (*context).dmvalue_ptr_2c = (&mut dmvalue_ptr_2c) as *mut auxtools::raw_types::values::Value;

        (*context).locals = put_to_data_store(&mut (*proc).data_store, locals, locals_count);
        (*context).stack = put_to_data_store(&mut (*proc).data_store, stack, stack_size);
        (*context).locals_count = locals_count as u16;
        (*context).stack_size = stack_size as u16;

        (*context).iterator_stack = std::ptr::null_mut(); // TODO

        /*
        TODO
        pub current_iterator: *mut values::Value,
        pub iterator_allocated: u32,
        pub iterator_length: u32,
        pub iterator_index: u32,
        pub iterator_filter_type: values::Value,
        pub iterator_unk1: u32,
         */
        (*context).jnz_lchk_loop_count = 10;

        /*
        TODO
        pub iterator_smth: u8,
         */

        (*context).not_suspended = true;

        /*
        pub unk_9: u8,
	    pub is_measure_some_time_4: bool,
	    */
        (*context).call_in_progress = false;
        /*
	    pub unk_10: u8,
	    pub unk_11: u8,
	    pub unk_12: u8,
         */

        (*context).throw_handler = std::ptr::null_mut();
        /*
	    pub some_time3: Timeval,
	    pub some_time2: Timeval,
	    pub some_time: Timeval,
	    pub some_time4: Timeval,
         */

        (*CURRENT_EXECUTION_CONTEXT) = context;

        // TODO EXEC INSN

        log::debug!("Deopt called: context {:?}, proc {:?}", *context, *proc);

        call_execute_instruction(context);

        *out = (*context).dot;
    };
}

unsafe fn put_to_data_store(
    data_store: *mut auxtools::raw_types::procs::ArgAndLocalStore,
    data: *const auxtools::raw_types::values::Value,
    count: u32
) -> *mut auxtools::raw_types::values::Value {

    let data_size = std::mem::size_of::<auxtools::raw_types::values::Value>() * (count as usize);

    (*data_store).external_arg_count += count;
    let r = libc::malloc(data_size);
    libc::memcpy(r as *mut libc::c_void, data as *mut libc::c_void, data_size);

    return r as *mut auxtools::raw_types::values::Value;
}