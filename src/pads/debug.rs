use std::ffi::CStr;
use auxtools::{sigscan, Value};

#[no_mangle]
pub extern "C" fn handle_debug(str: *mut i8) {
    log::debug!("dbg: {}", unsafe { CStr::from_ptr(str) }.to_str().unwrap());
}

#[no_mangle]
pub extern "C" fn handle_debug_val(val: auxtools::raw_types::values::Value) {
    log::debug!("dbg: {:?}, {} -- {:?}", val.tag, unsafe { val.data.id }, val)
}

pub static mut DATUM_ARRAY_PTR : *mut *mut *mut u8 = std::ptr::null_mut();

pub fn init() {
    let scanner = auxtools::sigscan::Scanner::for_module(auxtools::BYONDCORE).unwrap();

    unsafe {
        DATUM_ARRAY_PTR = *((scanner.find(signature!("8b 15 ?? ?? ?? ?? 8b 14 82 85 d2 74 ad 8b 4a 18")).unwrap()).add(2) as *mut *mut *mut *mut u8);
    }
}

pub fn get_datum_ref_count(datum: Value) -> u32 {
    unsafe {

        let array_ptr = *DATUM_ARRAY_PTR;
        log::debug!("datum array ptr: {:?}", array_ptr);

        log::debug!("{:?}", datum.raw.tag);
        log::debug!("{:?}", datum);
        log::debug!("datum ptr ptr: {:?}", array_ptr.add(datum.raw.data.id as usize));
        let datum_ptr = *(array_ptr.add(datum.raw.data.id as usize));
        log::debug!("datum ptr: {:?}", datum_ptr);
        let res = *(datum_ptr.add(0x10) as *mut u32);
        log::debug!("datum ref count: {}", res);
        return res
    }
}