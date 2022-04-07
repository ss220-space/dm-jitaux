extern crate libc;

use std::convert::TryFrom;
use std::fmt::Debug;
use std::ops::Shr;
use std::panic::catch_unwind;
use std::ptr::slice_from_raw_parts;
use num_enum::TryFromPrimitive;

use auxtools::raw_types::funcs::{CURRENT_EXECUTION_CONTEXT, inc_ref_count};
use auxtools::raw_types::values::{ValueData, ValueTag, Value};
use auxtools::raw_types::strings::StringId;
use auxtools::raw_types::procs::{ExecutionContext, ProcInstance, ProcId};
use auxtools::sigscan;
use crate::compile::{PROC_META};
use crate::dmir::ValueLocation;
use crate::proc_meta::{ProcMetaId};
use crate::stack_map::{Constant, Location, LocationType, StkMapRecord};

static mut DO_CALL: Option<extern "cdecl" fn(*mut ProcInstance) -> Value> = Option::None;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct DeoptId(pub u64);

impl DeoptId {
    pub fn new(proc_mid: ProcMetaId, site_id: u32) -> Self {
        let value: u64 =
            ((proc_mid.0 as u64) << 32) | (site_id as u64);
        Self(value)
    }
    pub fn proc_mid(&self) -> ProcMetaId {
        ProcMetaId((self.0 >> 32) as u32)
    }
    pub fn index(&self) -> u32 {
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
    unsafe { return (DO_CALL).unwrap()(proc_instance); }
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
    fn reg_value(&self, register: &Register) -> u32 {
        match register {
            Register::EAX => self.eax,
            Register::ECX => self.ecx,
            Register::EDX => self.edx,
            Register::EBX => self.ebx,
            Register::ESP => self.esp + 0xc + 4 + 8, // 0xc - alignment, 4 - return address, 8 - id
            Register::EBP => self.ebp,
            Register::ESI => self.esi,
            Register::EDI => self.edi,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DeoptPointMeta {
    pub hits: u64,
    pub origin: DeoptPointOrigin,
    stack_map_locations: Box<[StackMapAccessor]>
}

impl DeoptPointMeta {
    pub fn parse_stack_map_record(origin: DeoptPointOrigin, record: &StkMapRecord, constants: &Vec<Constant>) -> DeoptPointMeta {
        let stack_map_locations = record.locations.iter().map(|location| StackMapAccessor::new(location, constants)).collect::<Vec<_>>();

        return Self {
            hits: 0,
            origin,
            stack_map_locations: stack_map_locations.into_boxed_slice()
        }
    }
}

#[derive(Debug, Clone)]
pub struct DeoptPointOrigin {
    pub bytecode_offset: u32,
    pub inc_ref_count_locations: Box<[ValueLocation]>
}

#[derive(Debug, Clone)]
enum StackMapAccessor {
    Register { register: Register, bit_offset: u8 },
    Direct { address: u32 },
    Indirect { register: Register, address_offset: i32 },
    Constant { value: u32 },
    LargeConstant { value: u64 }
}

impl StackMapAccessor {
    fn new(location: &Location, constants: &Vec<Constant>) -> Self {
        match location.loc_type {
            LocationType::Register =>
                Self::Register { register: Register::try_from(location.dwarf_reg_num).unwrap(), bit_offset: u8::try_from(location.offset_or_const).unwrap() },
            LocationType::Direct =>
                Self::Direct { address: location.offset_or_const },
            LocationType::Indirect =>
                Self::Indirect { register: Register::try_from(location.dwarf_reg_num).unwrap(), address_offset: location.offset_or_const as i32 },
            LocationType::Constant =>
                Self::Constant { value: location.offset_or_const },
            LocationType::ConstantIndex =>
                Self::LargeConstant { value: constants[location.offset_or_const as usize].large_const }
        }
    }
}

#[repr(u16)]
#[derive(Debug, TryFromPrimitive, Clone)]
enum Register {
    EAX = 0x0,
    ECX = 0x1,
    EDX = 0x2,
    EBX = 0x3,
    ESP = 0x4,
    EBP = 0x5,
    ESI = 0x6,
    EDI = 0x7
}

#[derive(Debug)]
struct ByondFrame {
    out: *mut Value,
    src: Value,
    test_res: bool,
    arg_count: u32,
    caller_arg_count: u32,
    original_args: *mut Value,
    cache: Value,
    args: Vec<Value>,
    stack: Vec<Value>,
    locals: Vec<Value>
}

impl ByondFrame {
    unsafe fn read_from_frame(
        frame: &Frame,
        deopt_meta: &DeoptPointMeta,
    ) -> Self {
        let mut idx = 0;
        let out = Self::read_frame_location_as(frame, deopt_meta, &mut idx);
        let src = Self::read_value(frame, deopt_meta, &mut idx);
        let test_res = Self::read_frame_location_as(frame, deopt_meta, &mut idx);
        let cache = Self::read_value(frame, deopt_meta, &mut idx);
        let arg_count = Self::read_frame_location_as(frame, deopt_meta, &mut idx);
        let caller_arg_count = Self::read_frame_location_as(frame, deopt_meta, &mut idx);
        let original_args = Self::read_frame_location_as(frame, deopt_meta, &mut idx);
        let args = Self::read_value_list(frame, deopt_meta, &mut idx);
        let stack = Self::read_value_list(frame, deopt_meta, &mut idx);
        let locals = Self::read_value_list(frame, deopt_meta, &mut idx);

        Self {
            out,
            src,
            test_res,
            arg_count,
            caller_arg_count,
            original_args,
            cache,
            args,
            stack,
            locals
        }
    }

    unsafe fn read_value_list(frame: &Frame, deopt_meta: &DeoptPointMeta, idx: &mut usize) -> Vec<Value> {
        let count: u32 = Self::read_frame_location_as(frame, deopt_meta, idx);
        let mut vec = Vec::with_capacity(count as usize);
        for _ in 0..count {
            vec.push(Self::read_value(frame, deopt_meta, idx));
        }
        vec
    }

    unsafe fn read_value(frame: &Frame, deopt_meta: &DeoptPointMeta, idx: &mut usize) -> Value {
        let tag = Self::read_frame_location_as::<u8>(frame, deopt_meta, idx);
        let value = Self::read_frame_location_as::<u32>(frame, deopt_meta, idx);
        Value {
            tag: std::mem::transmute(tag),
            data: std::mem::transmute(value)
        }
    }

    unsafe fn read_frame_location_as<T: Copy + Sized + Debug>(frame: &Frame, deopt_meta: &DeoptPointMeta, idx: &mut usize) -> T {
        let accessor = &deopt_meta.stack_map_locations[*idx];
        *idx += 1;

        let r = match accessor {
            StackMapAccessor::Register { register, bit_offset} => {
                let reg = frame.reg_value(register);
                let value = reg.shr(*bit_offset as u8);
                *(((&value) as *const u32) as *const T)
            }
            StackMapAccessor::Direct { address } => {
                *(*address as *const T)
            }
            StackMapAccessor::Indirect { register, address_offset } => {
                let ptr = (frame.reg_value(register) as *mut u8).offset(*address_offset as isize);
                *(ptr as *const T)
            }
            StackMapAccessor::Constant { value } => {
                *(((value) as *const u32) as *const T)
            }
            StackMapAccessor::LargeConstant { value } => {
                *(((value) as *const u64) as *const T)
            }
        };
        log::trace!("{:?} -> {:?}", accessor, r);
        return r;
    }
}

extern "C" fn handle_deopt_bridge(
    frame: Frame
) {
    log::trace!("Deopt called");
    let res = catch_unwind(move || {
        let deopt_id = DeoptId(frame.id);
        log::trace!("Deopt entry: {:?}, site: {}", deopt_id.proc_mid(), deopt_id.index());

        log::trace!("Deopt frame: {:?}", frame);
        let proc_meta = unsafe { &mut PROC_META[deopt_id.proc_mid().0 as usize] };

        log::trace!("ProcMeta: {:?}", proc_meta);

        let deopt_meta = proc_meta.deopt_point_map[deopt_id.index() as usize].as_mut().unwrap();
        log::trace!("DeoptPointMeta: {:?}", deopt_meta);

        let byond_frame = unsafe { ByondFrame::read_from_frame(&frame, deopt_meta) };
        log::trace!("ByondFrame: {:?}", byond_frame);


        handle_deopt_internal(
            proc_meta.proc_id,
            deopt_meta,
            2,
            byond_frame,
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

fn inc_ref_counts(locations: &[ValueLocation], frame: &ByondFrame) {
    for location in locations {
        let value = match location {
            ValueLocation::Stack(rel) => {
                frame.stack[frame.stack.len() - 1 - (*rel as usize)]
            }
            ValueLocation::Cache => {
                frame.cache
            }
            ValueLocation::Local(idx) => {
                frame.locals[*idx as usize]
            }
            ValueLocation::Argument(idx) => {
                frame.args[*idx as usize]
            }
        };
        unsafe { inc_ref_count(value); }
    }
}

fn handle_deopt_internal(
    proc_id: ProcId,
    deopt_point_meta: &mut DeoptPointMeta,
    proc_flags: u8,
    mut frame: ByondFrame,
) {
    let offset = deopt_point_meta.origin.bytecode_offset;

    deopt_point_meta.hits += 1;
    let deopt_count = deopt_point_meta.hits;

    let log_deopt =
        if cfg!(deopt_print_debug) {
            true
        } else {
            deopt_count < 10
        };

    if log_deopt { log::debug!("Deopt called entry {:?}", proc_id); }
    unsafe {

        // prepare ref-counts
        inc_ref_counts(deopt_point_meta.origin.inc_ref_count_locations.as_ref(), &frame);

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
        auxtools::raw_types::funcs::inc_ref_count(frame.src);

        (*proc).src = frame.src;
        (*proc).context = context;
        (*proc).arglist_idx = ValueData { id: 0 }; // TODO?
        (*proc).callback = std::ptr::null();
        (*proc).callback_value = 0;

        let original_args = slice_from_raw_parts(frame.original_args, frame.caller_arg_count as usize);

        for i in frame.arg_count..frame.caller_arg_count {
            frame.args.push((&*original_args)[i as usize])
        }

        (*proc).args_count = frame.args.len() as u32;
        (*proc).data_store.internal_arg_count = 0;

        (*proc).args = put_to_data_store(&mut (*proc).data_store, frame.args.as_ptr(), frame.args.len() as u32);
        (*proc).time_to_resume = 0.;


        (*context).proc_instance = proc;
        (*context).parent_context = std::ptr::null_mut();

        (*context).filename = StringId(0xffff);
        (*context).line = 0;
        let (bytecode_ptr, _) = auxtools::Proc::from_id(proc_id).unwrap().bytecode_mut_ptr();
        (*context).bytecode = bytecode_ptr;
        (*context).bytecode_offset = offset as u16;

        (*context).test_flag = frame.test_res;

        (*context).cached_datum = frame.cache;
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

        (*context).locals = put_to_data_store(&mut (*proc).data_store, frame.locals.as_ptr(), frame.locals.len() as u32);
        (*context).stack = put_to_data_store(&mut (*proc).data_store, frame.stack.as_ptr(), frame.stack.len() as u32);
        (*context).locals_count = frame.locals.len() as u16;
        (*context).stack_size = frame.stack.len() as u16;

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

        *frame.out = do_call_trampoline(proc);

        if log_deopt { log::debug!("Deopt return: {:?}", *frame.out); }
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