/proc/do_test()
    compile_proc(/proc/greater)
    compile_proc(/proc/greater_or_eq)
    compile_proc(/proc/lower)
    compile_proc(/proc/lower_or_eq)
    CHECK_INSTALL_COMPILED // RES: /greater, /greater_or_eq, /lower, /lower_or_eq

    RES(greater(2, null)) // RES: "a"
    RES(greater(null, null)) // RES: "not a"
    RES(greater(null, 2)) // RES: "not a"

    RES(greater_or_eq(2, null)) // RES: "a"
    RES(greater_or_eq(null, null)) // RES: "a"
    RES(greater_or_eq(null, 2)) // RES: "not a"

    RES(lower(null, 2)) // RES: "a"
    RES(lower(null, null)) // RES: "not a"
    RES(lower(2, null)) // RES: "not a"

    RES(lower_or_eq(null, 2)) // RES: "a"
    RES(lower_or_eq(null, null)) // RES: "a"
    RES(lower_or_eq(2, null)) // RES: "not a"


/proc/greater(a, b)
    if (a > b)
        return "a"
    return "not a"

/proc/greater_or_eq(a, b)
    if (a >= b)
        return "a"
    return "not a"

/proc/lower(a, b)
    if (a < b)
        return "a"
    return "not a"

/proc/lower_or_eq(a, b)
    if (a <= b)
        return "a"
    return "not a"

/proc/not(a)
    if (!a)
        return "not a"
    return "a"