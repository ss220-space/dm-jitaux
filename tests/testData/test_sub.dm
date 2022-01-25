/proc/do_test()
    compile_proc(/proc/sub)
    CHECK_INSTALL_COMPILED // RES: /sub

    RES(sub(2, 10, 12)) // RES: -20
    RES(sub(2, list(), 2)) // RES: null
    RES(sub(2, "test", 2)) // RES: null
    RES(sub(2, 2.2, 2.4)) // RES: -2.6

/proc/sub(a, b, c)
    return a - b - c