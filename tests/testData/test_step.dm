/proc/do_test()
    compile_proc(/proc/test_step)
    CHECK_INSTALL_COMPILED // RES: /test_step

    var/obj/item/item = new /obj/item()

    item.loc = locate(2, 2, 1)

    RES(test_step(locate(2, 2, 1), 0) == locate(2, 2, 1)) // RES: 1
    RES(test_step(locate(2, 2, 1), 1) == locate(2, 2, 1)) // RES: 0
    RES(test_step(locate(2, 2, 1), 1) == locate(2, 3, 1)) // RES: 1
    RES(test_step(locate(2, 2, 1), 2) == locate(2, 1, 1)) // RES: 1
    RES(test_step(locate(2, 2, 1), 4) == locate(3, 2, 1)) // RES: 1
    RES(test_step(locate(2, 2, 1), 8) == locate(1, 2, 1)) // RES: 1

    MARK_REF_COUNT(item)
    RES(test_step(item, 8) == locate(1, 2, 1)) // RES: 1
    RES_CHECK_LEAK(item) // RES: OK

/proc/test_step(a, b)
    return get_step(a, b)

/turf/ground

/obj/item