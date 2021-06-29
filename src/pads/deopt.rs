extern crate libc;

use auxtools::raw_types::funcs::CURRENT_EXECUTION_CONTEXT;
use auxtools::raw_types::values::{ValueData, ValueTag, Value};
use auxtools::raw_types::strings::StringId;
use auxtools::raw_types::procs::{ExecutionContext, ProcInstance, ProcId};
use auxtools::sigscan;

static mut DO_CALL: Option<extern "cdecl" fn(*mut ProcInstance) -> Value> = Option::None;


#[no_mangle]
pub extern "C" fn handle_deopt(
    out: *mut Value,
    proc_id: ProcId,
    proc_flags: u8,
    offset: u32,
    test_flag: bool,
    stack: *const Value,
    stack_size: u32,
    cached_datum: Value,
    src: Value,
    args: *const Value,
    args_count: u32,
    locals: *const Value,
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

        let usr_value = (*(**CURRENT_EXECUTION_CONTEXT).proc_instance).usr; // TODO?

        // usr will be dec on DoCall exit, make sure not to lost it
        auxtools::raw_types::funcs::inc_ref_count(usr_value.clone());

        (*proc).usr = usr_value;
        // src will be dec on DoCall exit, make sure not to lost it
        auxtools::raw_types::funcs::inc_ref_count(src.clone());

        (*proc).src = src;
        (*proc).context = context;
        (*proc).arglist_idx = ValueData { id: 0 }; // TODO?
        (*proc).callback = std::ptr::null();
        (*proc).callback_value = 0;

        (*proc).args_count = args_count;
        (*proc).args = put_to_data_store(&mut (*proc).data_store, args, args_count);
        (*proc).time_to_resume = 0.;

        (*proc).data_store.internal_arg_count = 10; // hack to avoid complex logic with putting data into

        (*context).proc_instance = proc;
        (*context).parent_context = std::ptr::null_mut();

        (*context).filename = StringId(0xffff);
        (*context).line = 0;
        let (bytecode_ptr, _) = auxtools::Proc::from_id(proc_id).unwrap().bytecode_mut_ptr();
        (*context).bytecode = bytecode_ptr;
        (*context).bytecode_offset = offset as u16;

        (*context).test_flag = test_flag;

        // cached datum will be dec on DoCall exit, make sure not to lost it
        auxtools::raw_types::funcs::inc_ref_count(cached_datum.clone());

        (*context).cached_datum = cached_datum;
        (*context).dmvalue_0x20 = Value {
            tag: ValueTag::Null,
            data: ValueData {
                id: 0x0
            }
        };
        let mut cached_values = [Value { tag: ValueTag::Null, data: ValueData { id: 0x0 } }; 64];

        let mut dmvalue_ptr_2c: Value = Value { tag: ValueTag::Null, data: ValueData { id: 0x0 } };

        (*context).cached_values = (&mut cached_values) as *mut Value;
        (*context).dmvalue_ptr_2c = (&mut dmvalue_ptr_2c) as *mut Value;

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

        log::debug!("Deopt called: context {:?}, proc {:?}", *context, *proc);

        *out = DO_CALL.unwrap()(proc);

        log::debug!("Deopt return: {:?}", *out);
    };
}

unsafe fn put_to_data_store(
    data_store: *mut auxtools::raw_types::procs::ArgAndLocalStore,
    data: *const Value,
    count: u32
) -> *mut Value {

    let data_size = std::mem::size_of::<Value>() * ((count + 1) as usize);

    (*data_store).external_arg_count += count;
    let r = libc::malloc(data_size); // TODO: ensure no leaks!
    libc::memcpy(r as *mut libc::c_void, data as *mut libc::c_void, data_size);

    return r as *mut Value;
}

pub fn initialize_deopt() {
    let scanner = auxtools::sigscan::Scanner::for_module(auxtools::BYONDCORE).unwrap();

    let mut do_call_byond = std::ptr::null();
    {
        if cfg!(windows) {
            let res = scanner.find(signature!("53 8B DC 83 EC 08 83 E4 F8 83 C4 04"));

            if let Some(ptr) = res {
                do_call_byond = ptr as *const std::ffi::c_void;
            }
        }

        if cfg!(unix) {
            panic!("TODO")
        }

        if do_call_byond.is_null() {
            panic!("Failed to find do_call");
        }
    }

    unsafe {
        DO_CALL = Option::Some(
            std::mem::transmute(do_call_byond)
        );
    };
}