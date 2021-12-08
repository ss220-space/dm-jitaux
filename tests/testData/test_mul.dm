/proc/do_test()
    compile_proc(/proc/mul)
    CHECK_INSTALL_COMPILED // RES: /mul

    RES(mul(2, 10)) // RES: 20
    RES(mul(2, list())) // RES: 0
    RES(mul(2, "test")) // RES: 0
    RES(mul(2, 2.2)) // RES: 4.4



/proc/mul(a, b)
    return a * b