/proc/do_test()
    compile_proc(/proc/and)
    compile_proc(/proc/or)
    CHECK_INSTALL_COMPILED // RES: /and, /or

    RES(and(15, 3)) // RES: 3
    RES(and(14, 3)) // RES: 2
    RES(or(2, 8)) // RES: 10
    RES(or(2, 10)) // RES: 10

    RES(and(15, null)) // RES: 0
    RES(and(null, 3)) // RES: 0
    RES(or(null, 8)) // RES: 8
    RES(or(2, null)) // RES: 2

/proc/and(a, b)
    return a & b

/proc/or(a, b)
    return a | b