/proc/do_test()
    compile_proc(/datum/simple/proc/update)
    compile_proc(/proc/list_aug_add)
    CHECK_INSTALL_COMPILED // RES: /datum/simple/update, /list_aug_add

    var/datum/simple/s = new
    s.update()
    RES(s.a) // RES: 2
    RES(s.b) // RES: 0
    RES(json_encode(s.c)) // RES: "[42,33]"

    var/list/l = list(11)
    list_aug_add(l, 42)
    RES(json_encode(l)) // RES: "[11,42]"

/datum/simple
    var/a = 1
    var/b = 1
    var/c = list(42)
/datum/simple/proc/update()
    a += 1
    b -= 1
    c += 33

/proc/list_aug_add(l, v)
    l += v