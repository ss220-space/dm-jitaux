/proc/do_test()
    compile_proc("/proc/sum")
    CHECK_INSTALL_COMPILED // RES: /sum

    RES(sum(2, 10, 12)) // RES: 24
    RES(sum(2, list(), 2)) // RES:
    RES(sum(2, "test", 2)) // RES:
    RES(sum(2, 2.2, 2.4)) // RES: 6.6

/proc/sum(a, b, c)
    return a + b + c