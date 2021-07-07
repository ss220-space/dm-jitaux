/proc/hook_main_init()
	world.log << call(DMJIT_LIB, "auxtools_init")()
	hook_log_init()
// INIT
/proc/hook_log_init()
	CRASH()

// DEBUG
/proc/hook_call(src)
	CRASH()

// DEBUG Re-enter
/proc/on_test_call()
	CRASH()

// Dump call counts
/proc/dump_call_count()
	CRASH()

// Dump opcode use counts
/proc/dump_opcode_count()
	CRASH()

/proc/dump_opcodes(name)
	CRASH()

/proc/hook_compile()
	CRASH()

/proc/auxtools_stack_trace(msg)
	CRASH(msg)

/proc/compile_proc(name)
    CRASH()

/proc/install_compiled()
    CRASH()

/proc/toggle_dm_jitaux_hooks()
    CRASH()

/proc/toggle_dm_jitaux_call_counts()
    CRASH()

/proc/get_datum_ref_count(arg)
    CRASH()