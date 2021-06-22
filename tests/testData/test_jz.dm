/proc/do_test()
    compile_proc("/proc/cond_false")
    compile_proc("/proc/cond_true")
    install_compiled()

    file("result.txt") << cond_true()
    file("result.txt") << cond_false()

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