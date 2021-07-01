/proc/do_test()
    compile_proc("/datum/simple/update")
    CHECK_INSTALL_COMPILED // RES: /datum/simple/update

    var/datum/simple/s = new
    s.update()
    RES(s.a) // RES: 2
    RES(s.b) // RES: 0



/datum/simple
    var/a = 1
    var/b = 1
/datum/simple/proc/update()
    a += 1
    b -= 1