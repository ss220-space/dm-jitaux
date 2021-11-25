#define CHECK_INSTALL_COMPILED file("result.txt") << install_compiled()
#define WRITE_RESULT(T) file("result.txt") << T
#define RES(T) WRITE_RESULT(T)
#define CHECK_LEAK(v) out_leak(before##v, get_datum_ref_count(v))
#define MARK_REF_COUNT(v) var/before##v = get_datum_ref_count(v)

/world/New()
    hook_main_init()
    world.log = file("world.txt")
    do_test()
    exit_test()

/proc/exit_test()
    CRASH("hook not loaded")

/proc/out_leak(before, after)
    if (after == before) {
        return "OK"
    } else {
        return "NOT_OK([before] != [after])"
    }