/proc/do_test()
    compile_proc(/proc/new_datum)
    CHECK_INSTALL_COMPILED // RES: /new_datum

    RES(new_datum(2)) // RES: 2
    RES(new_datum(8)) // RES: 8

/proc/new_datum(v)
    var/datum/test/t = new /datum/test(v)
    return t.a

/datum/test
    var/a = 2

    New(v)
        a = v