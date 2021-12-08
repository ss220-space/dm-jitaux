/proc/do_test()
    compile_proc(/datum/base/proc/dynamic_call)
    CHECK_INSTALL_COMPILED // RES: /datum/base/dynamic_call

    var/datum/base/a = new
    RES(a.dynamic_call(1)) // RES: 2

    var/datum/base/override/b = new
    RES(b.dynamic_call(1)) // RES: 3

/datum/base

/datum/base/proc/call_me(num)
    return num + 1
/datum/base/proc/dynamic_call(num)
    return call_me(num)

/datum/base/override

/datum/base/override/call_me(num)
    return num + 2