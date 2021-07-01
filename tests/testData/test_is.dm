/proc/do_test()
    compile_proc("/proc/is_null")
    CHECK_INSTALL_COMPILED // RES: /is_null

    RES(is_null(null)) // RES: 1
    RES(is_null(1)) // RES: 0

/proc/is_null(a)
    return isnull(a)
