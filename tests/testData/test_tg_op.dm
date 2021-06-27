/proc/do_test()
    compile_proc("/proc/greater")
    CHECK_INSTALL_COMPILED // RES: /greater

    RES(greater(1, 2)) // RES: b
    RES(greater(2, 1)) // RES: a
    RES(greater(2, 2)) // RES: eq

/proc/greater(a, b)
    if (a > b)
        return "a"
    if (b > a)
        return "b"
    return "eq"