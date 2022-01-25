/*
LLVM Stack-map structure

Header {
  uint8  : Stack Map Version (current version is 3)
  uint8  : Reserved (expected to be 0)
  uint16 : Reserved (expected to be 0)
}
uint32 : NumFunctions
uint32 : NumConstants
uint32 : NumRecords
StkSizeRecord[NumFunctions] {
  uint64 : Function Address
  uint64 : Stack Size
  uint64 : Record Count
}
Constants[NumConstants] {
  uint64 : LargeConstant
}
StkMapRecord[NumRecords] {
  uint64 : PatchPoint ID
  uint32 : Instruction Offset
  uint16 : Reserved (record flags)
  uint16 : NumLocations
  Location[NumLocations] {
    uint8  : Register | Direct | Indirect | Constant | ConstantIndex
    uint8  : Reserved (expected to be 0)
    uint16 : Location Size
    uint16 : Dwarf RegNum
    uint16 : Reserved (expected to be 0)
    int32  : Offset or SmallConstant
  }
  uint32 : Padding (only if required to align to 8 byte)
  uint16 : Padding
  uint16 : NumLiveOuts
  LiveOuts[NumLiveOuts]
    uint16 : Dwarf RegNum
    uint8  : Reserved
    uint8  : Size in Bytes
  }
  uint32 : Padding (only if required to align to 8 byte)
}
 */

use std::io::Cursor;
use binrw::BinRead;

#[derive(BinRead, Debug)]
#[br(assert(version == 3))]
pub struct Header {
    pub version: u8,
    _r0: u8, // Reserved
    _r1: u16 // Reserved
}

#[derive(BinRead, Debug)]
pub struct StkSizeRecord {
    pub address: u64,
    pub stack_size: u64,
    pub record_count: u64
}

#[derive(BinRead, Debug)]
pub struct Constant {
    pub large_const: u64
}

#[derive(BinRead, Debug)]
pub struct LiveOut {
    pub dwarf_reg_num: u16,
    _r0: u8,
    pub size: u8
}


#[derive(BinRead, Debug)]
#[repr(u8)]
#[br(repr = u8)]
pub enum LocationType {
    Register = 0x1,
    Direct = 0x2,
    Indirect = 0x3,
    Constant = 0x4,
    ConstantIndex = 0x5
}

#[derive(BinRead, Debug)]
pub struct Location {
    pub loc_type: LocationType,
    _r0: u8, // Reserved
    pub loc_size: u16,
    pub dwarf_reg_num: u16,
    _r1: u16, // Reserved
    pub offset_or_const: u32
}

#[derive(BinRead, Debug)]
pub struct StkMapRecord {
    pub id: u64,
    pub instruction_offset: u32,
    _r0: u16, // Reserved
    _num_locations: u16,
    #[br(count = _num_locations, align_after = 8)]
    pub locations: Vec<Location>,
    _pad1: u16, // Padding
    _num_live_outs: u16,
    #[br(count = _num_live_outs, align_after = 8)]
    pub live_outs: Vec<LiveOut>
}

#[derive(BinRead, Debug)]
pub struct StackMap {
    pub header: Header,
    _num_functions: u32,
    _num_constants: u32,
    _num_records: u32,

    #[br(count = _num_functions)]
    pub stack_records: Vec<StkSizeRecord>,
    #[br(count = _num_constants)]
    pub constants: Vec<Constant>,
    #[br(count = _num_records)]
    pub map_records: Vec<StkMapRecord>,
}


/// Read LLVM StackMap from raw memory
pub fn read_stack_map(address: *const u8, size: usize) -> StackMap {
    unsafe {
        let mut cursor: Cursor<&[u8]> = Cursor::new(std::slice::from_raw_parts(address as *const u8, size));
        StackMap::read(&mut cursor).unwrap()
    }
}