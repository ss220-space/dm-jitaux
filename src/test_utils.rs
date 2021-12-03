use std::process::exit;
use auxtools::Value;
use crate::pads;

#[hook("/proc/dmjit_exit_test")]
pub fn exit_test() {
    exit(0);
}

#[hook("/proc/dmjit_get_datum_ref_count")]
pub fn get_datum_ref_count(arg: Value) {
    return Ok(Value::from(pads::debug::get_datum_ref_count(arg.clone())))
}