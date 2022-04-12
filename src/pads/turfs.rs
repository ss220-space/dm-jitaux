use auxtools::raw_types::values::{Value, ValueData, ValueTag};
use crate::pads::{byond_imports, find_by_call};

byond_imports!(
    #[cfg(windows)]
    fn GET_STEP: extern "cdecl" fn(*mut Value, Value, u8) -> *mut Value
        = find_by_call!(
            windows => "8d 85 d8 f8 ff ff 50 8d 89 78 03 00 00 e8 >?? ?? ?? ?? ff 70 04 ff 30"
        );
    #[cfg(unix)]
    fn GET_STEP: extern "cdecl" fn(u32, Value, u8) -> Value
        = find_by_call!(
            unix    => "a1 ?? ?? ?? ?? 89 54 24 0c 8d 95 f0 f8 ff ff 89 14 24 05 78 03 00 00 89 44 24 04 e8 >?? ?? ?? ??"
        );
);

pub(crate) fn init() {
    init_byond_imports();
}

pub fn get_step(value: Value, direction: u8) -> Value {
    unsafe {
        #[cfg(windows)]
        {
            let mut ret = Value {
                tag: ValueTag::Null,
                data: ValueData { id: 0 },
            };
            return *GET_STEP(&mut ret, value, direction);
        }
        #[cfg(unix)]
        {
            return GET_STEP(0, value, direction);
        }
    }
}