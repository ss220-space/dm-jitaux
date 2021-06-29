/proc/do_test()
    compile_proc("/datum/no_neg/proc/static_call")
    CHECK_INSTALL_COMPILED // RES: /datum/no_neg/static_call
    var/datum/no_neg/s = new
    RES(s.static_call(-1)) // RES: 0
    RES(s.static_call(-10)) // RES: 0

/datum/no_neg

/datum/no_neg/proc/static_call(num)
    if (0 > num)
        return static_call(num + 1)
    return num