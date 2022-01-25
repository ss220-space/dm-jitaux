use std::collections::HashMap;
use std::time::Instant;
use auxtools::{DMResult, Value};


static mut TIME_MAP: Option<HashMap<String, Instant>> = Option::None;

#[hook("/proc/dmjit_mark_time")]
pub fn mark_time(name: Value) -> DMResult {
    let time = Instant::now();
    unsafe {
        TIME_MAP.get_or_insert_with(|| HashMap::new()).insert(
            name.to_string()?,
            time
        );
    }
    DMResult::Ok(Value::null())
}


#[hook("/proc/dmjit_report_time")]
pub fn report_time(name: Value) -> DMResult {
    let time = Instant::now();
    let before = unsafe {
        TIME_MAP.get_or_insert_with(|| HashMap::new()).remove(
            name.to_string()?.as_str()
        )
    };
    if let Some(before) = before {
        Value::from_string(time.duration_since(before).as_secs_f64().to_string())
    } else {
        DMResult::Ok(Value::null())
    }
}