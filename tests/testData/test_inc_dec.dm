/proc/do_test()
    compile_proc(/proc/test_inc)
    compile_proc(/proc/test_dec)
    CHECK_INSTALL_COMPILED // RES: /test_inc, /test_dec

    RES(test_inc()) // RES: 3
    RES(test_dec()) // RES: 7

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