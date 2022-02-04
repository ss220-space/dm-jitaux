#define WRITE_RESULT(T) file("result.txt") << "[__FILE__]:[__LINE__]:[T]"
#define CHECK_INSTALL_COMPILED WRITE_RESULT(dmjit_install_compiled())
#define RES(T) WRITE_RESULT(format_value(T))
#define CHECK_LEAK(v) out_leak(before##v, dmjit_get_ref_count(v))
#define RES_CHECK_LEAK(v) WRITE_RESULT(CHECK_LEAK(v))
#define MARK_REF_COUNT(v) var/before##v = dmjit_get_ref_count(v)
#define compile_proc(v) dmjit_compile_proc(v)

/world/New()
    dmjit_hook_main_init()
    world.log = file("world.txt")
    do_test()
    dmjit_exit_test()

/proc/dmjit_exit_test()
    CRASH("hook not loaded")

/proc/out_leak(before, after)
    if (after == before) {
        return "OK"
    } else {
        return "NOT_OK([before] != [after])"
    }

/proc/format_value(T)
    if(isnull(T))
        return "null"
    if(istext(T))
        return "[json_encode(T)]"
    return "[T]"