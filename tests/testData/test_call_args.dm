/proc/do_test()
    compile_proc("/datum/base/proc/dynamic_call")
    CHECK_INSTALL_COMPILED // RES: /datum/base/dynamic_call

    var/datum/base/a = new
    RES(a.dynamic_call(1, 2)) // RES: -1

/datum/base

/datum/base/proc/non_reflexive(a, b)
    return a - b
/datum/base/proc/dynamic_call(a, b)
    return non_reflexive(a, b)