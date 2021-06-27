/proc/do_test()
    compile_proc("/proc/sum")
    CHECK_INSTALL_COMPILED // RES: /sum

    RES(sum(1, 10)) // RES: 11
    RES(sum(15, 0.5)) // RES: 15.5

/proc/sum(a, b)
    return a + b