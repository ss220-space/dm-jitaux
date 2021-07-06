/proc/do_test()
    compile_proc("/datum/no_neg/proc/static_call")
    compile_proc("/datum/no_neg/proc/static_call_statement")
    CHECK_INSTALL_COMPILED // RES: /datum/no_neg/static_call, /datum/no_neg/static_call_statement
    var/datum/no_neg/s = new
    RES(s.static_call(-1)) // RES: 0
    RES(s.static_call(-10)) // RES: 0

    s.v = -10
    s.static_call_statement()
    RES(s.v) // RES: 0

/datum/no_neg
    var/v = 0

/datum/no_neg/proc/static_call(num)
    if (0 > num)
        return static_call(num + 1)
    return num

/datum/no_neg/proc/static_call_statement()
    if (0 > v)
        v += 1
        static_call_statement()
        return v
    return v