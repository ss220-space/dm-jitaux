/proc/do_test()
    compile_proc(/proc/iter_list)
    compile_proc(/proc/iter_list_deopt)
    CHECK_INSTALL_COMPILED // RES: /iter_list, /iter_list_deopt

    var/list/data[1000]
    for(var/i in 1 to 1000)
        data[i] = i

    RES(num2text(iter_list(data), 10)) // RES: "500504160"
    RES(num2text(iter_list_deopt(data), 10)) // RES: "500504160"

/datum/some
    var/s = 1

/proc/iter_list(var/list/data)
    var/sum = 0
    for (var/i in data)
        for (var/j in data)
            sum += j
        sum += i
    return sum

/proc/iter_list_deopt(var/list/data)
    var/sum = 0
    for (var/i in data)
        for (var/j in data)
            dm_jitaux_deopt()
            sum += j
        sum += i
    return sum