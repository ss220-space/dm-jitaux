/proc/do_test()
    compile_proc(/proc/test_while)
    compile_proc(/proc/test_for_classic)
    compile_proc(/proc/test_for_range)
    compile_proc(/proc/test_for_range_step)
    compile_proc(/proc/test_for_range_counter)
    compile_proc(/proc/test_for_range_step_counter)
    CHECK_INSTALL_COMPILED // RES: /test_while, /test_for_classic, /test_for_range, /test_for_range_step, /test_for_range_counter, /test_for_range_step_counter

    RES(test_while(10)) // RES: 100
    RES(test_for_classic(10)) // RES: 100
    RES(test_for_range(10)) // RES: 100
    RES(test_for_range_step(20)) // RES: 200
    RES(test_for_range_counter(10)) // RES: 10
    RES(test_for_range_step_counter(20)) // RES: 20
    RES(test_deopted(10)) // RES: 100
    RES(test_while(5)) // RES: 25
    RES(test_for_classic(5)) // RES: 25
    RES(test_for_range(5)) // RES: 25
    RES(test_for_range_step(10)) // RES: 50
    RES(test_for_range_counter(5)) // RES: 5
    RES(test_for_range_step_counter(10)) // RES: 10
    RES(test_deopted(5)) // RES: 25

/proc/test_for_classic(c)
    var/ret = 0
    var/i
    for(i=1, i<=c, i++)
        ret += c
    return ret

/proc/test_for_range(c)
    var/ret = 0
    for(var/i in 1 to c)
        ret += c
    return ret

/proc/test_for_range_step(c)
    var/ret = 0
    for(var/i in 1 to c step 2)
        ret += c
    return ret

/proc/test_for_range_counter(c)
    var/ret = 0
    for(var/i in 1 to c)
        ret = c
    return ret

/proc/test_for_range_step_counter(c)
    var/ret = 0
    for(var/i in 1 to c step 2)
        ret = c
    return ret

/proc/test_while(c)
    var/ret = 0
    var/i = 1
    while(i <= c)
        ret += c
        i += 1
    return ret

/proc/test_deopted(c)
    var/ret = 0
    var/i = 1
    while(i <= c)
        ret += c
        i += 1
    return ret
