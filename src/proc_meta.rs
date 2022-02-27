use std::collections::HashMap;
use auxtools::raw_types::procs::ProcId;
use itertools::{Itertools};
use crate::dmir::ValueLocation;
use crate::pads::deopt::{DeoptId, DeoptPointMeta, DeoptPointOrigin};
use crate::stack_map::{Constant, StackMap, StkMapRecord};

/// Sequential ID for each JIT-ed proc, for efficient storage
#[derive(Debug, Clone, Hash, Ord, PartialOrd, Eq, PartialEq)]
pub struct ProcMetaId(pub u32);

impl ProcMetaId {
    fn next(self) -> Self {
        Self(self.0 + 1)
    }
}

/// Metadata structure used by dmJIT
#[derive(Debug, Clone)]
pub struct ProcMeta {
    pub mid: ProcMetaId,
    pub proc_id: ProcId,
    pub deopt_point_map: Box<[Option<DeoptPointMeta>]>,
}

impl ProcMeta {
    fn stub() -> Self {
        Self {
            mid: ProcMetaId(0),
            proc_id: ProcId(0),
            deopt_point_map: vec![].into_boxed_slice()
        }
    }
}


/// Intermediate storage
pub struct ProcMetaModuleBuilder {
    next_mid: ProcMetaId,
    meta_builders: Vec<ProcMetaBuilder>,
}

#[derive(Debug)]
pub struct ProcMetaUpdateTransaction(Vec<ProcMeta>);

impl ProcMetaUpdateTransaction {
    /// Commit transaction to storage, rebuilding MID order
    pub fn commit_transaction_to(&mut self, storage: &mut Vec<ProcMeta>) {
        for proc_meta in self.0.drain(..) {
            let target_index = proc_meta.mid.0 as usize;
            if target_index >= storage.len() {
                storage.resize(target_index + 1, ProcMeta::stub())
            }
            storage[target_index] = proc_meta
        }
    }
}

impl ProcMetaModuleBuilder {
    pub fn new() -> Self {
        Self {
            next_mid: ProcMetaId(0),
            meta_builders: vec![],
        }
    }

    pub fn build_update_transaction(
        &mut self,
        stack_map: Option<StackMap>
    ) -> ProcMetaUpdateTransaction {

        let (record_index, constants) = if let Some(StackMap { map_records, constants, .. }) = stack_map {
            (map_records.into_iter().map(|record| (DeoptId(record.id), record)).collect::<HashMap<_, _>>(), constants)
        } else {
            (HashMap::new(), Vec::new())
        };

        ProcMetaUpdateTransaction(
            self.meta_builders.drain(..)
            .map(|builder| builder.build(&record_index, &constants))
            .collect_vec()
        )
    }

    fn next_mid(&mut self) -> ProcMetaId {
        let r = self.next_mid.clone();
        let next = r.clone().next();
        self.next_mid = next;
        r
    }

    pub fn create_meta_builder(&mut self, proc_id: ProcId) -> &mut ProcMetaBuilder {
        let meta_builder = ProcMetaBuilder {
            mid: self.next_mid(),
            proc_id,
            deopt_points: vec![]
        };

        self.meta_builders.push(meta_builder);
        self.meta_builders.last_mut().unwrap()
    }
}


pub struct ProcMetaBuilder {
    mid: ProcMetaId,
    proc_id: ProcId,
    deopt_points: Vec<DeoptPointOrigin>
}

impl ProcMetaBuilder {
    pub fn build(self, stack_map_record_index: &HashMap<DeoptId, StkMapRecord>, constants: &Vec<Constant>) -> ProcMeta {
        let mid = self.mid;
        let deopt_points = self.deopt_points.into_iter().enumerate().map(|(idx, deopt_point)| {
            let deopt_point_id = DeoptId::new(mid.clone(), idx as u32);
            stack_map_record_index.get(&deopt_point_id).map(|record| DeoptPointMeta::parse_stack_map_record(deopt_point, record, constants))
        }).collect_vec();

        ProcMeta {
            mid,
            proc_id: self.proc_id,
            deopt_point_map: deopt_points.into_boxed_slice()
        }
    }

    pub fn add_deopt_point(&mut self, bytecode_offset: u32, inc_ref_count_locations: &Vec<ValueLocation>) -> DeoptId {
        let deopt_id = DeoptId::new(self.mid.clone(), self.deopt_points.len() as u32);
        self.deopt_points.push(
            DeoptPointOrigin {
                bytecode_offset,
                inc_ref_count_locations: inc_ref_count_locations.clone().into_boxed_slice(),
            }
        );
        return deopt_id;
    }
}








