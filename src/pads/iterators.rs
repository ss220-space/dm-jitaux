use auxtools::raw_types::funcs::dec_ref_count;
use auxtools::raw_types::values::Value;
use libc::{c_void, free};
use crate::pads::lists::get_list;
use crate::pads::{byond_imports, find_by_call, find_by_reference};
use once_cell::sync::Lazy;

byond_imports!(
    fn ITER_LOAD_FROM_OBJECT_PTR: extern "cdecl" fn(Value, *mut *mut Value, *mut u32, *mut u32, u32)
        = find_by_call!(
            unix    => "8b 50 60 89 54 24 14 8d 50 50 89 54 24 10 8d 50 4c 83 c0 48 89 54 24 0c 8b 55 90 89 44 24 08 8b 45 8c 89 54 24 04 89 04 24 e8 >?? ?? ?? ??",
            windows => "ff 71 ?? 8d 41 ?? 50 8d 41 ?? 50 8d 41 ?? 50 ff b5 ?? ?? ?? ?? ff b5 ?? ?? ?? ?? e8 >?? ?? ?? ?? a1 ?? ?? ?? ?? 83 c4 ??"
        );
);

pub(crate) fn init() {
    init_byond_imports();
}

#[no_mangle]
pub extern "C" fn load_array_iter_from_list(list: Value, out_array: &mut *mut Value, out_array_size: &mut u32, out_length: &mut u32) {
    unsafe {
        let data = get_list(list);
        *out_array = (*data).vector_part;
        *out_array_size = (*data).allocated;
        *out_length = (*data).length;
        (*data).vector_part = std::ptr::null_mut();
        (*data).allocated = 0;
        (*data).length = 0;
    };
}

#[no_mangle]
pub extern "C" fn load_array_iter_from_object(obj: Value, out_array: &mut *mut Value, out_array_size: &mut u32, out_length: &mut u32, bitmask: u32) {
    unsafe {
        log::trace!("load_array_iter_from_object, {:?}, before: {:?}, {}, {}", *ITER_LOAD_FROM_OBJECT_PTR as *mut u8, *out_array, *out_array_size, *out_length);
        *out_array = std::ptr::null_mut();
        *out_array_size = 0;
        *out_length = 0;
        ITER_LOAD_FROM_OBJECT_PTR(obj, out_array, out_array_size, out_length, bitmask);
        log::trace!("load_array_iter_from_object result, {:?}, {}, {}", *out_array, *out_array_size, *out_length)
    };
}

#[no_mangle]
pub extern "C" fn iter_unref(array: *mut Value, array_length: u32) {
    unsafe {
        for index in 0..array_length {
            dec_ref_count(*array.offset(index as isize));
        }
    };
}

#[no_mangle]
pub extern "C" fn iter_free(array: *mut Value) {
    unsafe {
        free(array as *mut c_void)
    };
}

#[derive(Debug)]
pub struct ByondIter {
    pub kind: u8,
    pub array: *mut Value,
    pub allocated: u32,
    pub length: u32,
    pub index: u32,
    pub filter_flags: u32,
    pub filter_type: Value
}