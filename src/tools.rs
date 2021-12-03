use auxtools::hooks::call_counts;
use auxtools::raw_types::procs::ProcId;
use auxtools::{Proc, Value};
use dmasm::format_disassembly;
use dmasm::operands::Variable;
use dmasm::{Instruction, Node};
use itertools::Itertools;
use serde::Serialize;
use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::ffi::CString;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::Path;
use typed_arena::Arena;

use crate::DisassembleEnv;

pub fn var_desc(v: &Variable) -> String {
    match v {
        Variable::Null => "Null",
        Variable::World => "World",
        Variable::Usr => "Usr",
        Variable::Src => "Src",
        Variable::Args => "Args",
        Variable::Dot => "Dot",
        Variable::Cache => "Cache",
        Variable::CacheKey => "CacheKey",
        Variable::CacheIndex => "CacheIndex",
        Variable::Arg(_) => "Arg",
        Variable::Local(_) => "Local",
        Variable::Global(_) => "Global",
        Variable::SetCache(a, b) => {
            return format!(
                "SetCache({}, {})",
                var_desc(a.borrow()),
                var_desc(b.borrow())
            );
        }
        Variable::Initial(_) => "Initial",
        Variable::IsSaved(_) => "IsSaved",
        Variable::Field(_) => "Field",
        Variable::StaticVerb(_) => "StaticVerb",
        Variable::DynamicVerb(_) => "DynamicVerb",
        Variable::StaticProc(_) => "StaticProc",
        Variable::DynamicProc(_) => "DynamicProc",
    }
    .to_string()
}

#[hook("/proc/dmjit_dump_opcode_count")]
pub fn dump_opcode_count() {
    log::info!("[DOC] Dump opcode counts");
    if let Some(mut vec) = call_counts() {
        vec.sort_by_key(|h| h.count);
        log::info!("[DOC] Total {} procs", vec.len());

        let mut env = DisassembleEnv {};

        let mut map: HashMap<String, u64> = HashMap::new();

        for count in vec {
            unsafe {
                let (nodes, res) =
                    dmasm::disassembler::disassemble(count.proc.bytecode(), &mut env);
                for node in nodes {
                    match node {
                        dmasm::Node::Instruction(insn, _) => {
                            let mut name = insn.op_name();
                            match insn {
                                Instruction::Call(v, _) => {
                                    name = format!("{} {}", name, var_desc(&v))
                                }
                                Instruction::CallStatement(v, _) => {
                                    name = format!("{} {}", name, var_desc(&v))
                                }
                                Instruction::GetVar(v) => {
                                    name = format!("{} {}", name, var_desc(&v))
                                }
                                Instruction::SetVar(v) => {
                                    name = format!("{} {}", name, var_desc(&v))
                                }
                                _ => {}
                            }

                            *map.entry(name).or_default().borrow_mut() += u64::from(count.count)
                        }
                        _ => {}
                    }
                }
            }
        }

        let mut res = map.iter().collect::<Vec<_>>();
        res.sort_by_key(|e| -(*e.1 as i64));
        for (op, count) in res {
            log::info!("[DOC] {}\t{}", count, op);
        }
    }
    Ok(Value::null())
}

#[hook("/proc/dmjit_dump_opcodes")]
pub fn dump_opcodes(list: Value) {
    if let Ok(name) = list.as_list()?.get(Value::from(1))?.as_string() {
        let mut override_id = 0;
        loop {
            if let Some(proc) = Proc::find_override(name.clone(), override_id) {
                let mut env = DisassembleEnv {};

                let bytecode = unsafe { proc.bytecode() };

                let (nodes, _error) = dmasm::disassembler::disassemble(bytecode, &mut env);

                log::info!("override_id: {}, proc.path: {}", override_id, proc.path);
                log::info!("{:?}", unsafe { &*proc.entry });
                log::info!("{}", format_disassembly(&nodes, None));
                log::info!("{:?}", proc.parameter_names());
                override_id += 1;
            } else {
                if override_id == 0 {
                    log::error!("Function not found {}", name);
                }
                break;
            }
        }
    } else {
        log::error!("Not a str {}, {}", list, list.to_string()?)
    }
    Ok(Value::null())
}

#[derive(Debug)]
struct ProcEntry<'t> {
    id: ProcId,
    calls: RefCell<HashMap<u32, ProcCallTarget<'t>>>,
}

#[derive(Debug, Eq, PartialEq, Hash)]
enum ProcCallTarget<'t> {
    Static(&'t ProcEntry<'t>),
    Dynamic(String, Vec<&'t ProcEntry<'t>>),
    Unresolved(String),
}

impl PartialEq<Self> for ProcEntry<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id.eq(&other.id)
    }
}

impl Eq for ProcEntry<'_> {}

impl Hash for ProcEntry<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

struct CallHierarchyScanner<'t> {
    arena: Arena<ProcEntry<'t>>,
}

struct CallHierarchyScannerState<'t> {
    container: &'t CallHierarchyScanner<'t>,
    entries: HashMap<ProcId, &'t ProcEntry<'t>>,
    dynamic_cache: HashMap<String, Vec<auxtools::proc::Proc>>,
}

impl<'t> CallHierarchyScannerState<'t> {
    fn new(container: &'t CallHierarchyScanner<'t>) -> Self {
        Self {
            container,
            entries: Default::default(),
            dynamic_cache: Default::default(),
        }
    }

    fn scan(&mut self, proc_id: ProcId) -> &'t ProcEntry<'t> {
        if self.entries.contains_key(&proc_id) {
            return self.entries.get(&proc_id).unwrap();
        }
        let entry = self.container.arena.alloc(ProcEntry {
            id: proc_id.clone(),
            calls: RefCell::default(),
        });

        self.entries.insert(proc_id, entry);

        let mut env = DisassembleEnv {};
        let proc = auxtools::proc::Proc::from_id(proc_id).unwrap();

        let (instructions, _) =
            unsafe { dmasm::disassembler::disassemble(&proc.bytecode(), &mut env) };

        for node in instructions {
            match node {
                Node::Comment(_) => {}
                Node::Label(_) => {}
                Node::Instruction(instruction, data) => match instruction {
                    Instruction::Call(variable, _) | Instruction::CallStatement(variable, _) => {
                        entry.calls.borrow_mut().insert(
                            data.offset,
                            self.decode_var_access(&variable)
                                .unwrap_or_else(|| ProcCallTarget::Unresolved("Call".to_string())),
                        );
                    }
                    Instruction::CallParent | Instruction::CallParentArgs(_) => {
                        let call = if let Some(parent) = Self::nearest_parent(&proc) {
                            ProcCallTarget::Static(self.scan(parent))
                        } else {
                            ProcCallTarget::Unresolved("CallParent".to_owned())
                        };
                        entry.calls.borrow_mut().insert(data.offset, call);
                    }
                    Instruction::CallSelf | Instruction::CallSelfArgs(_) => {
                        entry
                            .calls
                            .borrow_mut()
                            .insert(data.offset, ProcCallTarget::Static(entry));
                    }
                    Instruction::CallGlob(_, proc) | Instruction::CallGlobalArgList(proc) => {
                        let called = self.scan(ProcId(proc.id.unwrap()));
                        entry
                            .calls
                            .borrow_mut()
                            .insert(data.offset, ProcCallTarget::Static(called));
                    }
                    Instruction::GetVar(variable)
                    | Instruction::SetVar(variable)
                    | Instruction::SetVarExpr(variable) => {
                        if let Some(call) = self.decode_var_access(&variable) {
                            entry.calls.borrow_mut().insert(data.offset, call);
                        }
                    }
                    Instruction::CallPath(_) => {
                        entry.calls.borrow_mut().insert(
                            data.offset,
                            ProcCallTarget::Unresolved("CallPath".to_owned()),
                        );
                    }
                    Instruction::CallName(_) => {
                        entry.calls.borrow_mut().insert(
                            data.offset,
                            ProcCallTarget::Unresolved("CallName".to_owned()),
                        );
                    }
                    Instruction::CallLib(_) | Instruction::CallLibArgList => {
                        entry.calls.borrow_mut().insert(
                            data.offset,
                            ProcCallTarget::Unresolved("CallLib".to_owned()),
                        );
                    }
                    _ => {}
                },
            }
        }
        return entry;
    }

    fn decode_var_access(&mut self, acc: &Variable) -> Option<ProcCallTarget<'t>> {
        match acc {
            Variable::SetCache(a, b) => {
                assert!(self.decode_var_access(a.borrow()).is_none());
                self.decode_var_access(b.borrow())
            }
            Variable::StaticProc(proc) | Variable::StaticVerb(proc) => {
                let called = self.scan(ProcId(proc.id.unwrap()));
                Option::Some(ProcCallTarget::Static(called))
            }
            Variable::DynamicProc(str) | Variable::DynamicVerb(str) => {
                let chars = unsafe { CString::from_vec_unchecked(str.0.clone()) };
                let str = chars.to_string_lossy();
                let name = str.to_string();

                let proc_entries = match self.dynamic_cache.entry(name.clone()) {
                    Entry::Occupied(entry) => entry.get().clone(),
                    Entry::Vacant(entry) => {
                        let procs = auxtools::proc::get_for_dynamic_call(&name);
                        log::debug!("Try resolve '{}', got: {}", str, procs.len());
                        entry.insert(procs).clone()
                    }
                };
                Option::Some(ProcCallTarget::Dynamic(
                    name,
                    proc_entries.iter().map(|proc| self.scan(proc.id)).collect(),
                ))
            }
            _ => Option::None,
        }
    }

    fn nearest_parent(proc: &auxtools::proc::Proc) -> Option<ProcId> {
        let override_id = proc.override_id();
        if override_id != 0 {
            return Option::Some(
                auxtools::proc::get_proc_override(&proc.path, override_id - 1)
                    .unwrap()
                    .id,
            );
        }

        let (type_path, proc_name) = proc
            .path
            .rsplit_once("/")
            .unwrap_or(("", proc.path.as_str()));
        let mut path = type_path;
        while !path.is_empty() {
            if let Some(idx) = path.rfind("/") {
                let super_type_path = &path[..idx];
                let proc_path = format!("{}/{}", super_type_path, proc_name);
                log::debug!("Try to resolve parent call: {}", proc_path);
                if let Some(proc) = auxtools::proc::get_proc(proc_path) {
                    return Option::Some(
                        *CallHierarchyScannerState::find_all_override_ids(proc)
                            .last()
                            .unwrap(),
                    );
                }
                path = super_type_path;
            } else {
                break;
            }
        }

        return Option::None;
    }

    fn call_hierarchy(&mut self, proc_id: ProcId) -> Vec<&'t ProcEntry<'t>> {
        let proc = auxtools::proc::Proc::from_id(proc_id).unwrap();
        let override_ids = Self::find_all_override_ids(proc);

        return override_ids.iter().map(|id| self.scan(*id)).collect();
    }
}

impl CallHierarchyScannerState<'_> {
    fn find_all_override_ids(proc: auxtools::proc::Proc) -> Vec<ProcId> {
        let mut override_ids = vec![];
        let mut index = 0u32;
        loop {
            let other = auxtools::proc::get_proc_override(&proc.path, index);
            if let Some(proc) = other {
                override_ids.push(proc.id)
            } else {
                break;
            }
            index += 1
        }
        return override_ids;
    }
}

impl<'t> CallHierarchyScanner<'t> {
    fn new() -> Self {
        let arena = Arena::new();
        Self { arena }
    }

    fn call_hierarchy(&'t self, proc_id: ProcId) -> Vec<&'t ProcEntry<'t>> {
        let mut state = CallHierarchyScannerState::new(self);
        return state.call_hierarchy(proc_id);
    }
}

#[derive(Serialize)]
struct ReportProcData {
    name: String,
    opcodes: HashMap<String, u32>,
    performance: Option<ReportPerformanceData>,
}

#[derive(Serialize, Clone)]
struct ReportPerformanceData {
    self_cpu: f32,
    total_cpu: f32,
    real_time: f32,
    overtime: f32,
    calls: u64,
}

#[derive(Serialize)]
struct ReportProcReference {
    kind: String,
    title: String,
    refs: Vec<u32>,
}

#[derive(Serialize)]
struct Report {
    proc_data: HashMap<u32, ReportProcData>,
    proc_ref: HashMap<u32, Vec<ReportProcReference>>,
    roots: Vec<u32>,
}

fn write_out_hierarchy(
    proc_data_by_id: &mut HashMap<u32, ReportProcData>,
    proc_references: &mut HashMap<u32, Vec<ReportProcReference>>,
    entry: &ProcEntry<'_>,
    processed: &mut HashSet<ProcId>,
    profile: &HashMap<String, ReportPerformanceData>,
) {
    let proc = auxtools::proc::Proc::from_id(entry.id).unwrap();

    let mut opcodes = HashMap::new();
    let mut env = DisassembleEnv {};
    let (nodes, _) = dmasm::disassembler::disassemble(unsafe { proc.bytecode() }, &mut env);
    for node in nodes {
        match node {
            Node::Instruction(instruction, _) => {
                let mut name = instruction.op_name();
                match instruction {
                    Instruction::Call(v, _) => name = format!("{} {}", name, var_desc(&v)),
                    Instruction::CallStatement(v, _) => name = format!("{} {}", name, var_desc(&v)),
                    Instruction::GetVar(v) => name = format!("{} {}", name, var_desc(&v)),
                    Instruction::SetVar(v) => name = format!("{} {}", name, var_desc(&v)),
                    _ => {}
                }
                *opcodes.entry(name).or_insert(0u32) += 1;
            }
            Node::Comment(_) => {}
            Node::Label(_) => {}
        }
    }

    if !processed.insert(entry.id) {
        return;
    }

    proc_data_by_id.insert(
        proc.id.0,
        ReportProcData {
            name:
                if proc.override_id() != 0 {
                    format!("{}.{}", proc.path, proc.override_id())
                } else {
                    proc.path.clone()
                },
            opcodes,
            performance: profile.get(proc.path.as_str()).cloned(),
        },
    );

    let mut references = vec![];

    for (_, call) in entry.calls.borrow().iter() {
        match call {
            ProcCallTarget::Static(entry) => {
                write_out_hierarchy(proc_data_by_id, proc_references, entry, processed, profile);
                references.push(ReportProcReference {
                    kind: "static".to_string(),
                    title: "".to_string(),
                    refs: vec![entry.id.0],
                });
            }
            ProcCallTarget::Dynamic(name, entries) => {
                for entry in entries.iter() {
                    write_out_hierarchy(proc_data_by_id, proc_references, entry, processed, profile);
                }
                references.push(ReportProcReference {
                    kind: "dynamic".to_string(),
                    title: format!("{} ({})", name, entries.len()),
                    refs: entries.iter().map(|entry| entry.id.0).collect(),
                });
            }
            ProcCallTarget::Unresolved(reason) => {
                references.push(ReportProcReference {
                    kind: "unresolved".to_string(),
                    title: reason.clone(),
                    refs: vec![],
                });
            }
        }
    }
    proc_references.insert(proc.id.0, references);
}

fn parse_performance_profile(report_file: &Path) -> HashMap<String, ReportPerformanceData> {
    let input = File::open(report_file).unwrap();
    let reader = BufReader::new(input);

    let mut result = HashMap::new();

    for line in reader.lines().skip(3) {
        if let Ok(line) = line {
            if let Some((name, self_cpu, total_cpu, real_time, overtime, calls)) =
            line.split_whitespace().collect_tuple()
            {
                result
                    .insert(
                        name.to_string(),
                        ReportPerformanceData {
                            self_cpu: self_cpu.parse().unwrap(),
                            total_cpu: total_cpu.parse().unwrap(),
                            real_time: real_time.parse().unwrap(),
                            overtime: overtime.parse().unwrap(),
                            calls: calls.parse().unwrap(),
                        },
                    );
            }
        }
    }

    return result;
}

#[hook("/proc/dmjit_call_hierarchy")]
fn hook_call_hierarchy(proc_name: Value) {
    let name = proc_name.as_string()?;
    let scanner = CallHierarchyScanner::new();
    if let Some(proc) = auxtools::proc::get_proc(&name) {
        let procs = scanner.call_hierarchy(proc.id);
        let path = std::path::Path::new("data.js");
        let f = File::create(path).unwrap();
        let mut writer = BufWriter::new(f);

        let mut report = Report {
            proc_data: Default::default(),
            proc_ref: Default::default(),
            roots: procs.iter().map(|entry| entry.id.0).collect(),
        };

        let profile = parse_performance_profile(
            std::path::Path::new("/home/semoro/Downloads/Profile_results_total_time.txt")
        );

        for proc in procs {
            write_out_hierarchy(
                &mut report.proc_data,
                &mut report.proc_ref,
                proc,
                &mut Default::default(),
                &profile,
            );
        }

        write!(writer, "data = ").unwrap();
        serde_json::to_writer(&mut writer, &report).unwrap();
    } else {
        log::info!(target: "call_hierarchy", "Proc {} not found", name);
        return Ok(Value::from(false));
    }

    Ok(Value::from(true))
}
