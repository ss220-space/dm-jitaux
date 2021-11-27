#define CHECK_INSTALL_COMPILED file("result.txt") << dmjit_install_compiled()
#define WRITE_RESULT(T) file("result.txt") << T
#define RES(T) WRITE_RESULT(T)
#define CHECK_LEAK(v) out_leak(before##v, dmjit_get_datum_ref_count(v))
#define MARK_REF_COUNT(v) var/before##v = dmjit_get_datum_ref_count(v)
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