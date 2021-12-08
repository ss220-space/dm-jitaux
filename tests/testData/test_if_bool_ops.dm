/proc/do_test()
    compile_proc(/proc/do_check)
    CHECK_INSTALL_COMPILED // RES: /do_check

    RES(do_check(3, 2)) // RES: 1
    RES(do_check(3, 1)) // RES: 1
    RES(do_check(3, 3)) // RES: 0
    RES(do_check(0.5, 0.1)) // RES: 0

/proc/do_check(a, b)
    if (a > b && (a > 1 || b > 1))
        return 1
    return 0
