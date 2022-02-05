/proc/do_test()
    compile_proc(/proc/test_while)
    CHECK_INSTALL_COMPILED // RES: /test_while, /test_for_classic, /test_for_range, /test_inc, /test_dec

    RES(test_inc()) // RES: 3
    RES(test_dec()) // RES: 7

    RES(test_while(10000000)) // RES: 100
    RES(test_for_classic(10)) // RES: 100
    RES(test_for_range(10)) // RES: 100
    RES(test_deopted(10)) // RES: 100
    RES(test_while(5)) // RES: 25
    RES(test_for_classic(5)) // RES: 25
    RES(test_for_range(5)) // RES: 25
    RES(test_deopted(5)) // RES: 25

/proc/test_inc()
    var/ret = 0
    ret++
    ret++
    ret++
    return ret

/proc/test_dec()
    var/ret = 10
    ret--
    ret--
    ret--
    return ret

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
