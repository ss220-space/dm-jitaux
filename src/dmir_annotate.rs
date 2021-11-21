use std::collections::HashMap;
use crate::dmir::DMIR;

pub struct Annotator {
    annotations: HashMap<usize, Vec<String>>
}

impl Annotator {
    pub fn new() -> Self {
        Annotator {
            annotations: Default::default()
        }
    }
    pub fn add(&mut self, pos: usize, annotation: String) {
        self.annotations.entry(pos)
            .and_modify(|entry| entry.push(annotation.clone()) )
            .or_insert_with(|| vec![annotation] );
    }
    pub fn dump_annotated(&self, ir: &Vec<DMIR>) {
        for (pos, dmir) in ir.iter().enumerate() {
            if let Some(annotations) = self.annotations.get(&pos) {
                log::debug!("{}: {:?} // {}", pos, dmir, annotations.join(", "))
            } else {
                log::debug!("{}: {:?}", pos, dmir)
            }
        }
    }
    pub fn clear(&mut self) {
        self.annotations.clear()
    }
}