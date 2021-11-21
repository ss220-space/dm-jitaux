/proc/do_test()
    compile_proc("/proc/conditional_var_set")
    compile_proc("/proc/conditional_var_set2")
    CHECK_INSTALL_COMPILED // RES: /conditional_var_set, /conditional_var_set2

    RES(conditional_var_set(10)) // RES: 10
    RES(conditional_var_set2(10)) // RES: 10

/proc/conditional_var_set(c)
    var/w
    if (c)
        var/q = c
        w = q
    else
        w = c
    return w

/proc/conditional_var_set2(c)
    var/w
    if (c)
        var/q = c
        if (c)
            return c
        w = q
    else
        w = c
    return w