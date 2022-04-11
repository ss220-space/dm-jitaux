use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;


pub(crate) mod deopt;
pub(crate) mod debug;
pub(crate) mod lists;
pub(crate) mod dm_types;
mod signature_utils;

macro_rules! byond_imports {
    ($($(#[cfg($att:meta)])? $kind:tt $name:ident:$t:ty = $body:expr;)+) => {
        $(
            $(#[cfg($att)])?
            byond_imports!($kind $name $t = $body);
        )+
        fn init_byond_imports() {
            $(
                $(#[cfg($att)])?
                {
                    $name.init();
                }
            )+
        }
    };
	(fn $name:ident $t:ty = $body:expr) => {
		static $name: crate::pads::signature_utils::DynamicBoundFunction<$t> = $body;
	};
	(var $name:ident $t:ty = $body:expr) => {
		static $name: crate::pads::signature_utils::DynamicBoundVariable<$t> = $body;
	};
}

pub(crate) use byond_imports;

macro_rules! find_by {
    ($func:ident, $($att:meta => $signature:literal),+) => ({
		$(
			#[cfg($att)]
			unsafe {
                use crate::pads::signature_utils::ExSignature;
                std::mem::transmute(
					crate::pads::signature_utils::$func(
						&crate::pads::signature_utils::SCANNER,
						dmjit_macro::ex_signature!($signature)
					)
				)
            }
		)+
	});
}
pub(crate) use find_by;

macro_rules! find_by_call {
	($($rest:tt)+) => ({
		crate::pads::signature_utils::DynamicBoundFunction::new(|| crate::pads::find_by!(find_by_call, $($rest)+) )
	});
}
pub(crate) use find_by_call;

macro_rules! find_by_reference {
	($($rest:tt)+) => ({
		crate::pads::signature_utils::DynamicBoundVariable::new(|| crate::pads::find_by!(find_by_reference, $($rest)+) )
	});
}
pub(crate) use find_by_reference;

pub(crate) fn init() {
	deopt::initialize_deopt();
	debug::init();
	lists::init();
	dm_types::init();
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

	use dm_types::*;
	runtime_export!(is_dm_entity);

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