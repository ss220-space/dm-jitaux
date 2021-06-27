/proc/do_test()
    compile_proc("/proc/cond_false")
    compile_proc("/proc/cond_true")
    CHECK_INSTALL_COMPILED // RES: /cond_false, /cond_true

    RES(cond_true()) // RES: 1
    RES(cond_false()) // RES: 0

/proc/cond_true()
    var/c = 1
    if (c)
        return 1
    return 0

/proc/cond_false()
    var/c = 0
    if (c)
        return 1
    return 0