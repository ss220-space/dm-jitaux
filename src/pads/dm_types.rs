use auxtools::raw_types::values::Value;
use crate::pads::{byond_imports, find_by_call};

byond_imports!(
    fn IS_DM_ENTITY: extern "cdecl" fn(Value) -> bool
        = find_by_call!(
            unix    => "89 04 24 e8 ?? ?? ?? ?? 8b 3d ?? ?? ?? ?? e9 ?? ?? ?? ?? 8b 45 8c 8b 55 90 89 04 24 89 54 24 04 e8 >?? ?? ?? ??",
            windows => "b9 ff ff 00 00 66 01 48 42 66 8b 40 42 0f b7 c8 a1 ?? ?? ?? ?? 8b 40 3c ff 74 c8 04 ff 34 c8 8d 85 e0 fd ff ff 50 e8 ?? ?? ?? ?? 8a 85 e0 fd ff ff 83 c4 0c 3c 03 74 08 3c 02 0f ?? ?? ?? ?? ?? ff b5 e4 fd ff ff ff b5 e0 fd ff ff e8 >?? ?? ?? ?? 83 c4 08 84 c0"
        );
    fn IS_SUBTYPE_OF: extern "cdecl" fn(Value, Value) -> bool
        = find_by_call!(
            unix    => "8b b5 c8 f8 ff ff 89 54 24 08 89 4c 24 0c 89 7c 24 04 89 34 24 e8 >?? ?? ?? ?? 0f b6 c0 66 89 85 d6 f8 ff ff 8d bd f0 f8 ff ff df 85 d6 f8 ff ff d9 5c 24 04 89 3c 24",
            windows => "ff b5 e0 fd ff ff ff 74 ca fc ff 74 ca f8 e8 >?? ?? ?? ?? 83 c4 10 0f b6 c0 66 0f 6e c0 51"
        );
    fn CREATE_DATUM_BY_TYPE: extern "cdecl" fn(Value, i32, i32) -> i32
        = find_by_call!(
            unix    => "c7 44 24 0c ff ff 00 00 89 54 24 04 89 04 24 e8 >?? ?? ?? ?? 89 34 24 c7 44 24 08 21 00 00 00 89 44 24 04",
            windows => "83 c4 08 eb 24 6a 21 68 ff ff 00 00 ff 76 04 ff 75 f8 ff 75 fc e8 >?? ?? ?? ?? 83 c4 10"
        );
);

pub(crate) fn init() {
    init_byond_imports();
}

pub extern "cdecl" fn is_dm_entity(value: Value) -> bool {
    unsafe {
        return IS_DM_ENTITY(value);
    }
}

pub extern "cdecl" fn is_subtype_of(t: Value, value: Value) -> bool {
    unsafe {
        return IS_SUBTYPE_OF(t, value);
    }
}

pub extern "cdecl" fn create_datum(usr: Value, t: i32, unk: i32) -> i32 {
    unsafe {
        return CREATE_DATUM_BY_TYPE(usr, t, unk);
    }
}