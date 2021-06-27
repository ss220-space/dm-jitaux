/proc/do_test()
    compile_proc("/proc/do_deopt")
    install_compiled()

    file("result.txt") << do_deopt(2)

/proc/do_deopt(n)
    var/q = n + 1
    if (n < 0)
        return q
    return 2
