extern crate libc;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::Shr;
use std::panic::catch_unwind;
use auxtools::raw_types::funcs::CURRENT_EXECUTION_CONTEXT;
use auxtools::raw_types::values::{ValueData, ValueTag, Value};
use auxtools::raw_types::strings::StringId;
use auxtools::raw_types::procs::{ExecutionContext, ProcInstance, ProcId};
use auxtools::sigscan;
use crate::compile::{STACK_MAP_INDEX, StackMapIndex};
use crate::stack_map::{LocationType, StkMapRecord};

static mut DO_CALL: Option<extern "cdecl" fn(*mut ProcInstance) -> Value> = Option::None;

pub struct DeoptId(pub u64);

impl DeoptId {
    pub fn new(proc_id: ProcId, site_id: u32) -> Self {
        let value: u64 =
            ((proc_id.0 as u64) << 32) | (site_id as u64);
        Self(value)
    }
    fn proc_id(&self) -> ProcId {
        ProcId((self.0 >> 32) as u32)
    }
    fn site_id(&self) -> u32 {
        (self.0 & 0xFFFF_FFFF) as u32
    }
}

#[cfg(unix)]
fn do_call_trampoline(proc_instance: *mut ProcInstance) -> Value {
    unsafe {
        let addr = DO_CALL.unwrap();
        let mut result: Value = Value { tag: ValueTag::Null, data: ValueData { id: 0 } };
        asm!(
            "call {}",
            in(reg) addr,
            inout("eax") &mut result => _,
            in("edx") proc_instance,
            clobber_abi("cdecl")
        );
        result
    }
}

#[cfg(windows)]
fn do_call_trampoline(proc_instance: *mut ProcInstance) -> Value {
    *out = DO_CALL.unwrap()(proc);
}

thread_local! {
    pub static DEOPT_COUNT: RefCell<HashMap<(ProcId, u32), u32>> = RefCell::new(HashMap::new());
}

#[naked]
#[no_mangle]
pub unsafe extern "C" fn handle_deopt_entry() {
    asm!(
        "sub esp, 0xc", // realign stack
        "push esp", // store esp first before messing with stack
        "push eax",
        "push edx",
        "push ecx",
        "push ebx",
        "push esi",
        "push edi",
        "push ebp",
        "call {}",
        "add esp, 0x10", // drop non-restore registers
        "pop ecx", // restore registers that we have to maintain
        "pop edx",
        "pop eax",
        "pop esp",
        "add esp, 0xc", // de-align stack back
        "ret",
        sym handle_deopt_bridge,
        options(noreturn)
    )
}

#[repr(C)]
#[derive(Debug)]
struct Frame {
    ebp: u32,
    edi: u32,
    esi: u32,
    ebx: u32,
    ecx: u32,
    edx: u32,
    eax: u32,
    esp: u32,
    pad0: [u8; 0xc],
    ret: u32,
    id: u64
}

impl Frame {
    fn reg_value(&self, number: u16) -> u32 {
        match number {
            0 => self.eax,
            1 => self.ecx,
            2 => self.edx,
            3 => self.ebx,
            4 => panic!("esp not stable"),
            5 => self.ebp,
            6 => self.esi,
            7 => self.edi,
            _ => panic!("unexpected reg_num: {}", number)
        }
    }
}


#[derive(Debug)]
struct ByondFrame {
    offset: u32,
    out: *mut Value,
    src: Value,
    test_res: bool,
    arg_count: u32,
    args: *mut Value,
    cache: Value,
    stack: Vec<Value>,
    locals: Vec<Value>
}

impl ByondFrame {
    unsafe fn new(
        frame: &Frame,
        stack_map_index: &StackMapIndex,
        stack_map_record: &StkMapRecord,
    ) -> Self {
        let mut idx = 0;
        let offset = Self::read_local_as(frame, stack_map_record, &mut idx, stack_map_index);
        let out = Self::read_local_as(frame, stack_map_record, &mut idx, stack_map_index);
        let src = Self::read_value(frame, stack_map_record, &mut idx, stack_map_index);
        let test_res = Self::read_local_as(frame, stack_map_record, &mut idx, stack_map_index);
        let arg_count = Self::read_local_as(frame, stack_map_record, &mut idx, stack_map_index);
        let args = Self::read_local_as(frame, stack_map_record, &mut idx, stack_map_index);
        let cache = Self::read_value(frame, stack_map_record, &mut idx, stack_map_index);
        let stack = Self::read_value_list(frame, stack_map_record, &mut idx, stack_map_index);
        let locals = Self::read_value_list(frame, stack_map_record, &mut idx, stack_map_index);

        Self {
            offset,
            out,
            src,
            test_res,
            arg_count,
            args,
            cache,
            stack,
            locals
        }
    }

    unsafe fn read_value_list(frame: &Frame, stack_map_record: &StkMapRecord, idx: &mut usize, stack_map_index: &StackMapIndex) -> Vec<Value> {
        let count: u32 = Self::read_local_as(frame, stack_map_record, idx, stack_map_index);
        let mut vec = Vec::with_capacity(count as usize);
        for _ in 0..count {
            vec.push(Self::read_value(frame, stack_map_record, idx, stack_map_index));
        }
        vec
    }

    unsafe fn read_value(frame: &Frame, stack_map_record: &StkMapRecord, idx: &mut usize, stack_map_index: &StackMapIndex) -> Value {
        let tag = Self::read_local_as::<u8>(frame, stack_map_record, idx, stack_map_index);
        let value = Self::read_local_as::<u32>(frame, stack_map_record, idx, stack_map_index);
        Value {
            tag: std::mem::transmute(tag),
            data: std::mem::transmute(value)
        }
    }

    unsafe fn read_local_as<T: Copy + Sized + Debug>(frame: &Frame, stack_map_record: &StkMapRecord, idx: &mut usize, stack_map_index: &StackMapIndex) -> T {
        let local = &stack_map_record.locations[*idx];
        *idx += 1;

        let r = match local.loc_type {
            LocationType::Register => {
                let reg = frame.reg_value(local.dwarf_reg_num);
                let value = reg.shr(local.offset_or_const as u8);
                *(((&value) as *const u32) as *const T)
            }
            LocationType::Direct => {
                *(local.offset_or_const as *const T)
            }
            LocationType::Indirect => {
                let ptr = (frame.reg_value(local.dwarf_reg_num) as *mut u8).offset(local.offset_or_const as isize);
                *(ptr as *const T)
            }
            LocationType::Constant => {
                let value = local.offset_or_const;
                *(((&value) as *const u32) as *const T)
            }
            LocationType::ConstantIndex => {
                let value = stack_map_index.constants[local.offset_or_const as usize].large_const;
                *(((&value) as *const u64) as *const T)
            }
        };
        log::trace!("{:?} -> {:?}", local, r);
        return r;
    }
}

extern "C" fn handle_deopt_bridge(
    frame: Frame
) {
    log::trace!("Deopt called");
    let res = catch_unwind(move || {
        let deopt_id = DeoptId(frame.id);
        log::trace!("Deopt entry: {:?}, site: {}", deopt_id.proc_id(), deopt_id.site_id());

        log::trace!("Deopt frame: {:?}", frame);
        let stack_map_index = unsafe { STACK_MAP_INDEX.as_ref().unwrap() };
        let record = stack_map_index.records_by_id.get(&frame.id).unwrap();
        log::trace!("Deopt record: {:?}", record);
        let byond_frame = unsafe { ByondFrame::new(&frame, stack_map_index, record) };
        log::trace!("Byond Frame: {:?}", byond_frame);



        handle_deopt_internal(
            byond_frame.out,
            deopt_id.proc_id(),
            2,
            byond_frame.offset,
            byond_frame.test_res,
            byond_frame.stack,
            byond_frame.cache,
            byond_frame.src,
            byond_frame.args,
            byond_frame.arg_count,
            byond_frame.locals
        );

        log::trace!("id: {} ret: {:x} eax: {:x} edx: {:x} ecx: {:x} ebx: {:x} esi: {:x} edi: {:x} ebp: {:x} esp: {:x}",
            frame.id,
            frame.ret,
            frame.eax,
            frame.edx,
            frame.ecx,
            frame.ebx,
            frame.esi,
            frame.edi,
            frame.ebp,
            frame.esp
        );
    });
    if let Result::Err(e) = res {
        log::error!("deopt panic: {:?}", e);
    }
}

fn handle_deopt_internal(
    out: *mut Value,
    proc_id: ProcId,
    proc_flags: u8,
    offset: u32,
    test_flag: bool,
    stack: Vec<Value>,
    cached_datum: Value,
    src: Value,
    args: *const Value,
    args_count: u32,
    locals: Vec<Value>,
) {
    let deopt_count = DEOPT_COUNT.with(|deopt_data| {
        *deopt_data.borrow_mut().entry((proc_id, offset))
            .and_modify(|prev| *prev += 1)
            .or_insert(1)
    });

    let log_deopt =
        if cfg!(deopt_print_debug) {
            true
        } else {
            deopt_count < 10
        };

    if log_deopt { log::debug!("Deopt called entry {:?}", proc_id); }
    unsafe {
        let context = {
            let size = std::mem::size_of::<ExecutionContext>() as libc::size_t;
            let r = libc::malloc(size);
            libc::memset(r, 0, size);
            r as *mut ExecutionContext
        };

        let mut proc_data = std::mem::zeroed::<ProcInstance>();
        let proc = &mut proc_data;

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
        (*proc).data_store.internal_arg_count = 0;

        (*proc).args = put_to_data_store(&mut (*proc).data_store, args, args_count);
        (*proc).time_to_resume = 0.;


        (*context).proc_instance = proc;
        (*context).parent_context = std::ptr::null_mut();

        (*context).filename = StringId(0xffff);
        (*context).line = 0;
        let (bytecode_ptr, _) = auxtools::Proc::from_id(proc_id).unwrap().bytecode_mut_ptr();
        (*context).bytecode = bytecode_ptr;
        (*context).bytecode_offset = offset as u16;

        (*context).test_flag = test_flag;

        (*context).cached_datum = cached_datum;
        (*context).dmvalue_0x20 = Value {
            tag: ValueTag::Null,
            data: ValueData {
                id: 0x0
            },
        };
        let mut cached_values = [Value { tag: ValueTag::Null, data: ValueData { id: 0x0 } }; 64];

        let mut dmvalue_ptr_2c: Value = Value { tag: ValueTag::Null, data: ValueData { id: 0x0 } };

        (*context).cached_values = (&mut cached_values) as *mut Value;
        (*context).dmvalue_ptr_2c = (&mut dmvalue_ptr_2c) as *mut Value;

        (*context).locals = put_to_data_store(&mut (*proc).data_store, locals.as_ptr(), locals.len() as u32);
        (*context).stack = put_to_data_store(&mut (*proc).data_store, stack.as_ptr(), stack.len() as u32);
        (*context).locals_count = locals.len() as u16;
        (*context).stack_size = stack.len() as u16;

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

        if log_deopt { log::debug!("Deopt called: context {:?}, proc {:?}", *context, *proc); }

        *out = do_call_trampoline(proc);

        if log_deopt { log::debug!("Deopt return: {:?}", *out); }
    };
}

unsafe fn put_to_data_store(
    data_store: *mut auxtools::raw_types::procs::ArgAndLocalStore,
    data: *const Value,
    count: u32,
) -> *mut Value {
    let data_size = std::mem::size_of::<Value>() * (count as usize);

    let dest: *mut Value;

    if (*data_store).internal_arg_count + count < 0xb {
        let arg_count = (*data_store).internal_arg_count;
        (*data_store).external_arg_count = 0;
        (*data_store).internal_arg_count += count;
        dest = &mut (*data_store).data_store[arg_count as usize];
    } else {
        let pad_count = (count & 0xffff_fff8) + 8;
        (*data_store).external_arg_count = pad_count;
        dest = libc::malloc(std::mem::size_of::<Value>() * pad_count as usize) as *mut Value;
    }

    libc::memcpy(dest as *mut libc::c_void, data as *mut libc::c_void, data_size);

    return dest;
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
            let res = scanner.find(signature!("55 89 e5 57 56 53 81 ec 1c 08 00 00"));

            if let Some(ptr) = res {
                do_call_byond = ptr as *const std::ffi::c_void;
            }
        }

        if do_call_byond.is_null() {
            panic!("Failed to find do_call");
        }
    }

    unsafe {
        DO_CALL = Option::Some(std::mem::transmute(do_call_byond));
    };
}