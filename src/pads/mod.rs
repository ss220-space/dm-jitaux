use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;


pub(crate) mod deopt;
pub(crate) mod debug;
pub(crate) mod lists;


pub(crate) fn init() {
	deopt::initialize_deopt();
	debug::init();
	lists::init();
}

pub(crate) fn bind_runtime_externals(module: &Module, execution_engine: &ExecutionEngine) {
	macro_rules! runtime_export {
		($func:expr) => ({
			runtime_export!($func, stringify!($func))
		});
		($func:expr, $name:expr) => ({
			let target = module.get_function(&concat!("dmir.runtime.", $name).replace("::", ".")).unwrap();
			execution_engine.add_global_mapping(&target, $func as usize);
			log::debug!("runtime_export: dmir.runtime.{} -> {:#X}", $name, $func as usize);
		});
	}


	runtime_export!(deopt::handle_deopt_entry, "deopt");

	runtime_export!(debug::handle_debug);
	runtime_export!(debug::handle_debug_val);

	use lists::*;
	runtime_export!(list_indexed_get);
	runtime_export!(list_indexed_set);
	runtime_export!(list_associative_get);
	runtime_export!(list_associative_set);
	runtime_export!(list_copy);
	runtime_export!(list_check_size);
	runtime_export!(list_append);
	runtime_export!(list_remove);

	use auxtools::raw_types::funcs::inc_ref_count;
	use auxtools::raw_types::funcs::dec_ref_count;
	use auxtools::raw_types::funcs::get_variable;
	use auxtools::raw_types::funcs::set_variable;
	use auxtools::raw_types::funcs::call_datum_proc_by_name as call_proc_by_name;
	use auxtools::raw_types::funcs::call_proc_by_id;
	runtime_export!(inc_ref_count);
	runtime_export!(dec_ref_count);
	runtime_export!(get_variable);
	runtime_export!(set_variable);
	runtime_export!(call_proc_by_name);
	runtime_export!(call_proc_by_id);
}