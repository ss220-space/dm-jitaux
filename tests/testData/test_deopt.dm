/proc/do_test()
    compile_proc(/proc/do_deopt)
    CHECK_INSTALL_COMPILED // RES: /do_deopt

    RES(do_deopt(2, 1)) // RES: 3

/proc/do_deopt(n, d)
    var/q = n + d
    dm_jitaux_deopt()
    if (n > 0)
        return q
    return 2

// Intrinsic to emulate deopt
/proc/dm_jitaux_deopt()