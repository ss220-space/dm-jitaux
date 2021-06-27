/proc/do_test()
    compile_proc("/proc/do_deopt")
    file("result.txt") << install_compiled()

    file("result.txt") << do_deopt(2, 0)

/proc/do_deopt(n, d)
    var/q = n + d
    dm_jitaux_deopt()
    if (n > 0)
        return q
    return 2

// Intrinsic to emulate deopt
/proc/dm_jitaux_deopt()