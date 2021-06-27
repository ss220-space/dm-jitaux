/proc/do_test()
    compile_proc("/proc/do_deopt")
    install_compiled()

    file("result.txt") << do_deopt(2, 0)

/proc/do_deopt(n, d)
    var/q = n + d
    if (n < 0)
        return q
    return 2
