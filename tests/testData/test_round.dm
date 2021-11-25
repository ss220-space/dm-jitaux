/proc/do_test()
    compile_proc("/proc/do_round")
    CHECK_INSTALL_COMPILED // RES: /do_round

    RES(do_round(2, 10)) // RES: 0
    RES(do_round(null, 10)) // RES: 0
    RES(do_round(11, 10)) // RES: 10
    RES(do_round(7, 10)) // RES: 10



/proc/do_round(a, b)
    return round(a, b)