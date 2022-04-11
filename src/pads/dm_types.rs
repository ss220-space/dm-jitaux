use auxtools::raw_types::values::Value;
use auxtools::sigscan;
use once_cell::sync::Lazy;
use crate::pads::{byond_imports, find_by_call};

byond_imports!(
    fn IS_DM_ENTITY: extern "cdecl" fn(Value) -> bool
        = find_by_call!(
            unix    => "89 04 24 e8 ?? ?? ?? ?? 8b 3d ?? ?? ?? ?? e9 ?? ?? ?? ?? 8b 45 8c 8b 55 90 89 04 24 89 54 24 04 e8 >?? ?? ?? ??",
            windows => "b9 ff ff 00 00 66 01 48 42 66 8b 40 42 0f b7 c8 a1 ?? ?? ?? ?? 8b 40 3c ff 74 c8 04 ff 34 c8 8d 85 e0 fd ff ff 50 e8 ?? ?? ?? ?? 8a 85 e0 fd ff ff 83 c4 0c 3c 03 74 08 3c 02 0f ?? ?? ?? ?? ?? ff b5 e4 fd ff ff ff b5 e0 fd ff ff e8 >?? ?? ?? ?? 83 c4 08 84 c0"
        );
);

pub(crate) fn init() {
    init_byond_imports();
}

pub fn is_dm_entity(value: Value) -> bool {
    unsafe {
        return IS_DM_ENTITY(value);
    }
}