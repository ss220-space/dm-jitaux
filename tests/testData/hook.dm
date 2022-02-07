#define DMJIT_NATIVE CRASH("dm-jit not loaded")

/proc/dmjit_hook_main_init()
	world.log << call(DMJIT_LIB, "auxtools_init")()
	world.log << dmjit_hook_log_init()

// INIT
/proc/dmjit_hook_log_init()
	DMJIT_NATIVE

// DEBUG
/proc/dmjit_hook_call(src)
	DMJIT_NATIVE

// DEBUG Re-enter
/proc/dmjit_on_test_call()
	DMJIT_NATIVE

// Dump call counts
/proc/dmjit_dump_call_count()
	DMJIT_NATIVE

// Dump opcode use counts
/proc/dmjit_dump_opcode_count()
	DMJIT_NATIVE

/proc/dmjit_dump_opcodes(name)
	DMJIT_NATIVE

/proc/dmjit_hook_compile()
	DMJIT_NATIVE

/proc/auxtools_stack_trace(msg)
	CRASH(msg)

/proc/dmjit_compile_proc(name)
    DMJIT_NATIVE

/proc/dmjit_install_compiled()
    DMJIT_NATIVE

/proc/dmjit_toggle_hooks()
    DMJIT_NATIVE

/proc/dmjit_toggle_call_counts()
    DMJIT_NATIVE

/proc/dmjit_get_ref_count(arg)
    DMJIT_NATIVE

/proc/dmjit_mark_time(name)
    DMJIT_NATIVE

/proc/dmjit_report_time(name)
    DMJIT_NATIVE

/proc/dmjit_dump_deopts()
    DMJIT_NATIVE

/proc/dmjit_print_list_content(list)
    DMJIT_NATIVE


// This function is treated as intrinsic in dmJIT, if calling proc JIT-ed and not yet deopt-ed, returns TRUE
/proc/dmjit_is_optimized()
    return FALSE