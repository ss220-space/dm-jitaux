/proc/do_test()
    compile_proc(/proc/get_abs)
    CHECK_INSTALL_COMPILED // RES: /get_abs

    RES(get_abs(-10)) // RES: 10
    RES(get_abs(-1.1)) // RES: 1.1
    RES(get_abs(10)) // RES: 10



/proc/get_abs(num)
    return abs(num)