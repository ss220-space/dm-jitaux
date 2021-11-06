#define CHECK_LEAK(v) out_leak(before##v, get_datum_ref_count(v))
#define MARK_REF_COUNT(v) var/before##v = get_datum_ref_count(v)


/proc/out_leak(before, after)
    if (after == before) {
        return "OK"
    } else {
        return "NOT_OK([before] != [after])"
    }


/proc/do_test()
    compile_proc("/proc/receive_datum")
    compile_proc("/proc/access_datum")
    compile_proc("/proc/pass_datum")
    compile_proc("/proc/store_restore_datum")
    CHECK_INSTALL_COMPILED // RES: /receive_datum, /access_datum, /pass_datum, /store_restore_datum

    var/datum/base/dt_local = new

    MARK_REF_COUNT(dt_local)
    RES(CHECK_LEAK(dt_local)) // RES: OK

    receive_datum(dt_local)
    RES(CHECK_LEAK(dt_local)) // RES: OK

    access_datum(dt_local)
    RES(CHECK_LEAK(dt_local)) // RES: OK

    pass_datum(dt_local)
    RES(CHECK_LEAK(dt_local)) // RES: OK

    store_restore_datum(dt_local)
    RES(CHECK_LEAK(dt_local)) // RES: OK

/datum/base
    var/dt_next = null

/proc/receive_datum(arg)
    return arg

/proc/access_datum(var/datum/base/arg)
    arg.dt_next = 1

/proc/pass_datum(arg)
    return just_ret(arg)

/proc/store_restore_datum(var/datum/base/arg)
    arg.dt_next = arg
    var/q = arg.dt_next
    arg.dt_next = 1
    return arg

/proc/just_ret(arg)
    return arg