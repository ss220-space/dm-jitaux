use std::ptr::null_mut;
use auxtools::raw_types::funcs::dec_ref_count;

use auxtools::raw_types::lists::{AssociativeListEntry, List};
use auxtools::raw_types::values::{Value, ValueData, ValueTag};
use auxtools::sigscan;

pub static mut GLOB_LIST_ARRAY_PTR: *mut *mut *mut List = std::ptr::null_mut();
pub static mut UNSET_ASSOC_LIST_PTR: Option<extern "cdecl" fn(*mut *mut AssociativeListEntry, Value)> = Option::None;
pub static mut ASSOC_LIST_SET_PTR: Option<extern "cdecl" fn(u32, Value, Value)> = Option::None;
pub static mut ASSOC_FIND_NODE_BY_KEY_PTR: Option<extern "cdecl" fn(*mut AssociativeListEntry, Value) -> *mut AssociativeListEntry> = Option::None;

pub fn init() {
    let scanner = auxtools::sigscan::Scanner::for_module(auxtools::BYONDCORE).unwrap();

    macro_rules! get_call_pointer {
        ($signature:literal, $shift:literal) => ({
            let pointer = scanner.find(signature!($signature)).unwrap();
            Option::Some(std::mem::transmute(pointer.add($shift).offset(4).offset(*(pointer.add($shift) as *const isize))))
        });
    }

    unsafe {
        GLOB_LIST_ARRAY_PTR = *((scanner.find(signature!("8b 15 ?? ?? ?? ?? 8b 14 82 85 d2 74 a7 83 42 10 01 8b 83 f4 00 00 00")).unwrap()).add(2) as *mut *mut *mut *mut List);
        UNSET_ASSOC_LIST_PTR = get_call_pointer!("e8 ?? ?? ?? ?? 39 5d d0 72 e0 8b 7d c8 8b 5d c4 8b 75 d0", 1);
        ASSOC_LIST_SET_PTR = get_call_pointer!("8b 55 14 89 44 24 0c 89 7c 24 04 89 54 24 10 8b 55 c8 89 54 24 08 e8 ?? ?? ?? ??", 23);
        ASSOC_FIND_NODE_BY_KEY_PTR = get_call_pointer!("e8 ?? ?? ?? ?? 85 c0 89 c3 0f 84 b8 fe ff ff 8b 40 08 8b 53 0c", 1);
    }
}

pub unsafe fn unset_assoc_list(list: *mut List, value: Value) {
    UNSET_ASSOC_LIST_PTR.unwrap()(&mut (*list).assoc_part, value)
}

pub fn get_list(list: Value) -> *mut List {
    unsafe {
        let array_ptr = *GLOB_LIST_ARRAY_PTR;
        return *(array_ptr.add(list.data.list.0 as usize));
    }
}

pub fn list_check_size(list: Value, index: i32) -> bool {
    unsafe {
        let list = get_list(list);
        return index > 0 && index as u32 <= (*list).length;
    }
}


pub fn list_indexed_get(list: Value, index: i32) -> Value {
    unsafe {
        let list = get_list(list);
        return *(*list).vector_part.offset((index - 1) as isize);
    }
}

pub fn list_indexed_set(list: Value, index: i32, value: Value) {
    unsafe {
        let list = get_list(list);
        let value_ptr = (*list).vector_part.offset((index - 1) as isize);
        unset_assoc_list(list, *value_ptr);
        dec_ref_count(*value_ptr);
        *(value_ptr) = value;
    }
}

pub fn list_associative_get(list: Value, index: Value) -> Value {
    unsafe {
        let node = ASSOC_FIND_NODE_BY_KEY_PTR.unwrap()((*get_list(list)).assoc_part, index);
        if node == null_mut() {
            return Value { tag: ValueTag::Null, data: ValueData { id: 0 } };
        }
        return (*node).value;
    }
}

pub fn list_associative_set(list: Value, index: Value, value: Value) {
    unsafe {
        ASSOC_LIST_SET_PTR.unwrap()(list.data.list.0, index, value)
    };
}