use std::ffi::CStr;

#[no_mangle]
pub extern "C" fn handle_debug(str: *mut i8) {
    log::debug!("dbg: {}", unsafe { CStr::from_ptr(str) }.to_str().unwrap());
}

#[no_mangle]
pub extern "C" fn handle_debug_val(val: auxtools::raw_types::values::Value) {
    log::debug!("dbg: {:?}, {} -- {:?}", val.tag, unsafe { val.data.id }, val)
}
