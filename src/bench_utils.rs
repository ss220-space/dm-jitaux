use std::time::Duration;
use auxtools::{DMResult, Value};
use criterion::Criterion;

static mut HARNESS: Option<Criterion> = Option::None;

#[hook("/proc/dmjit_bench_iterate")]
fn run_benchmark(group: Value, name: Value) -> DMResult {

    run_benchmark_internal(&group, &name, args)?;

    DMResult::Ok(Value::null())
}

#[hook("/proc/dmjit_bench_init")]
fn init_benchmark() -> DMResult {
    let harness = Criterion::default()
        .measurement_time(Duration::from_secs(10));
    unsafe { HARNESS = Some(harness); }

    DMResult::Ok(Value::null())
}

#[hook("/proc/dmjit_bench_finish")]
fn finish_benchmark() -> DMResult {
    let harness = unsafe { HARNESS.as_mut().unwrap() };
    harness.final_summary();

    DMResult::Ok(Value::null())
}

fn run_benchmark_internal(group: &Value, name: &Value, args: &Vec<Value>) -> DMResult {

    let c = unsafe { HARNESS.as_mut().unwrap() };
    let args = args.iter().collect::<Vec<&Value>>();
    log::debug!("benchmark proc is {}", name.as_string()?);
    let proc_name_sanitized = name.as_string()?.replace("/", "_");
    if let Some(proc) = auxtools::proc::get_proc(name.as_string()?) {
        c.benchmark_group(group.as_string()?)
            .bench_function(proc_name_sanitized, |bencher| {
            bencher.iter(|| proc.call(&args[2..]).unwrap());
        });
    } else {
        panic!("Proc not found");
    }

    DMResult::Ok(Value::null())
}