#define DMJIT_NATIVE CRASH("dm-jit not loaded")

/proc/hook_main_init()
	world.log << call(DMJIT_LIB, "auxtools_init")()
	world.log << hook_log_init()
// INIT
/proc/hook_log_init()
	DMJIT_NATIVE

// DEBUG
/proc/hook_call(src)
	DMJIT_NATIVE

// DEBUG Re-enter
/proc/on_test_call()
	DMJIT_NATIVE

// Dump call counts
/proc/dump_call_count()
	DMJIT_NATIVE

// Dump opcode use counts
/proc/dump_opcode_count()
	DMJIT_NATIVE

/proc/dump_opcodes(name)
	DMJIT_NATIVE

/proc/hook_compile()
	DMJIT_NATIVE

/proc/auxtools_stack_trace(msg)
	CRASH(msg)

/proc/compile_proc(name)
    DMJIT_NATIVE

/proc/install_compiled()
    DMJIT_NATIVE

/proc/toggle_dm_jitaux_hooks()
    DMJIT_NATIVE

/proc/toggle_dm_jitaux_call_counts()
    DMJIT_NATIVE

/proc/get_datum_ref_count(arg)
    DMJIT_NATIVE