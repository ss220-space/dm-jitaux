use std::ffi::CStr;

use auxtools::{sigscan, Value};
use auxtools::raw_types::values::{ValueData, ValueTag};
use once_cell::sync::Lazy;
use crate::pads::{byond_imports, find_by_reference};

use crate::pads::lists::get_list;

#[no_mangle]
pub extern "C" fn handle_debug(str: *mut i8) {
    log::debug!("dbg: {}", unsafe { CStr::from_ptr(str) }.to_str().unwrap());
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct ValueRaw {
    pub tag: u8,
    pub data: ValueData,
}

#[no_mangle]
pub extern "C" fn handle_debug_val(val: ValueRaw) {
    log::debug!("dbg: {:#X} {}", val.tag, unsafe { val.data.id })
}

byond_imports!(
    var DATUM_ARRAY_PTR: *mut *mut u8
        = find_by_reference!(
            unix    => "8b 15 >?? ?? ?? ?? 8b 14 82 85 d2 74 ad 8b 4a 18",
            windows => "a1 >?? ?? ?? ?? 89 0c b0 33 c0 c7 01 ff ff 00 00 89 41 08 66 89"
        );
);

pub fn init() {
    init_byond_imports();
}

pub fn get_ref_count(value: Value) -> u32 {
    unsafe {
        log::debug!("{:?}", value.raw.tag);
        log::debug!("{:?}", value);
        return if value.raw.tag == ValueTag::Datum {
            let datum_ptr = *(DATUM_ARRAY_PTR.add(value.raw.data.id as usize));
            let res = *(datum_ptr.add(0x18) as *mut u32);
            log::debug!("datum ref count: {}", res);
            res
        } else if value.raw.tag == ValueTag::List {
            let res = (*get_list(value.raw)).refcount;
            log::debug!("list ref count: {}", res);
            res
        } else {
            log::debug!("unknown value tag, cant get ref count");
            0
        };
    }
}