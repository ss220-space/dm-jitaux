/proc/do_test()
    compile_proc(/proc/list_add)
    CHECK_INSTALL_COMPILED // RES: /list_add

    RES((list() + 1)[1]) // RES: 1
    RES((list(5) + 2)[1]) // RES: 5
    RES((list(5) + 2)[2]) // RES: 2
    RES((list(5,9,99,33,88,42) + 3)[7]) // RES: 3

    RES(list_add(list(),1)[1]) // RES: 1
    RES(list_add(list(5),2)[1]) // RES: 5
    RES(list_add(list(5),2)[2]) // RES: 2
    RES(list_add(list(5,9,99,33,88,42),3)[7]) // RES: 3

    var/datum/base/dt_local = new
    var/list/test = list()
    var/list/test1 = list()

    MARK_REF_COUNT(dt_local)
    MARK_REF_COUNT(test)
    MARK_REF_COUNT(test1)

    RES_CHECK_LEAK(dt_local) // RES: OK
    RES_CHECK_LEAK(test) // RES: OK

    test1 = list_add_deopted(test, dt_local)

    RES_CHECK_LEAK(dt_local) // RES: NOT_OK(3 != 4)
    RES_CHECK_LEAK(test) // RES: OK
    RES_CHECK_LEAK(test1) // RES: OK

    test = list()
    test1 = list()

    RES_CHECK_LEAK(dt_local) // RES: OK
    RES_CHECK_LEAK(test) // RES: OK

    test1 = list_add(test, dt_local)

    RES_CHECK_LEAK(dt_local) // RES: NOT_OK(3 != 4)
    RES_CHECK_LEAK(test) // RES: OK
    RES_CHECK_LEAK(test1) // RES: OK

/datum/base
    var/dt_next = null

/proc/list_add(a, b)
    return a + b

/proc/list_add_deopted(a, b)
    return a + b