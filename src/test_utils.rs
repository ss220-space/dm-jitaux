use std::process::exit;
use std::ptr::null_mut;
use auxtools::raw_types::lists::AssociativeListEntry;
use auxtools::{DMResult, Value};
use crate::pads;

#[hook("/proc/dmjit_exit_test")]
pub fn exit_test() -> DMResult {
    #[cfg(windows)]
    {
        unsafe { winapi::um::winuser::PostQuitMessage(0); }
        DMResult::Ok(Value::null())
    }
    #[cfg(unix)]
    {
        exit(0);
    }
}

#[hook("/proc/dmjit_get_ref_count")]
pub fn get_ref_count(arg: Value) -> DMResult {
    return DMResult::Ok(Value::from(pads::debug::get_ref_count(arg.clone())))
}

#[hook("/proc/dmjit_print_list_content")]
pub fn test(l: auxtools::Value) {
    let list = pads::lists::get_list(l.clone().raw);
    unsafe {
        log::debug!("List ID {} {:?}", l.raw.data.list.0, (*list));
        log::debug!("Vector part");
        for i in 0..(*list).length {
            log::debug!("{:?}", pads::lists::list_indexed_get(l.clone().raw, (i+1) as i32));
        }
        log::debug!("Assoc part");
        fn debug_assoc_part(entry: *mut AssociativeListEntry) -> String {
            unsafe {
                return if entry != null_mut() {
                    format!("K: {} V: {} RB:{:?} L:[{}] R:[{}]", (*entry).key, (*entry).value, (*entry).color, debug_assoc_part((*entry).left), debug_assoc_part((*entry).right))
                } else {
                    "NULL".to_string()
                };
            }
        }
        log::debug!("{}", debug_assoc_part((*list).assoc_part));
    }
    Ok(auxtools::Value::null())
}