pub struct ExSignature {
    pub data: &'static [Option<u8>],
    pub marker_pos: u32
}

use auxtools::sigscan::Scanner;
use itertools::Itertools;
use once_cell::sync::Lazy;
use dmjit_macro::ex_signature;

pub(crate) static SCANNER: Lazy<Scanner> = Lazy::new(|| {
    auxtools::sigscan::Scanner::for_module(auxtools::BYONDCORE).unwrap()
});

pub fn find_by_call(scanner: &Scanner, signature: ExSignature) -> *mut u8 {
    let positions = scanner.find_all(signature.data);
    let marker_pos = signature.marker_pos;
    let result = positions.iter()
        .map(|pointer| unsafe {
            pointer.add(marker_pos as usize).offset(4).offset(*(pointer.add(marker_pos as usize) as *const isize))
        })
        .dedup()
        .collect::<Vec<_>>();
    if result.len() == 1 {
        return result.into_iter().next().unwrap();
    }
    panic!("Failed to perform signature search: found matches at {:#?}", positions);
}

pub fn find_by_reference(scanner: &Scanner, signature: ExSignature) -> *mut u8 {
    let positions = scanner.find_all(signature.data);
    let marker_pos = signature.marker_pos;
    let result = positions.iter()
        .map(|pointer| unsafe {
            pointer.add(marker_pos as usize)
        })
        .dedup()
        .collect::<Vec<_>>();
    if result.len() == 1 {
        return result.into_iter().next().unwrap();
    }
    panic!("Failed to perform signature search: found matches at {:#?}", positions);
}