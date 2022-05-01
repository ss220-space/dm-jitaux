/proc/do_test()
    compile_proc(/proc/iter_turf_mob_obj)
    compile_proc(/proc/iter_turf_mob)
    compile_proc(/proc/iter_turf_obj)
    compile_proc(/proc/iter_turf_anything)
    CHECK_INSTALL_COMPILED // RES: /iter_turf_mob_obj, /iter_turf_mob, /iter_turf_obj, /iter_turf_anything

    var/obj/water/water = new /obj/water()
    var/turf/burning_ground/tile = locate(1, 1, 1)
    var/mob/player/player = new /mob/player()

    player.loc = tile
    water.loc = tile

    RES(iter_turf_mob_obj(tile)) // RES: 2
    RES(player.health) // RES: 99
    RES(water.temperature) // RES: 21

    RES(iter_turf_obj(tile)) // RES: 1
    RES(player.health) // RES: 99
    RES(water.temperature) // RES: 22

    RES(iter_turf_mob(tile)) // RES: 1
    RES(player.health) // RES: 98
    RES(water.temperature) // RES: 22

    RES(iter_turf_anything(tile)) // RES: 2
    RES(player.health) // RES: 97
    RES(water.temperature) // RES: 23

/turf/burning_ground

/atom/proc/on_heat()
    return

/obj/water
    var/temperature = 20

/obj/water/on_heat()
    temperature += 1

/mob/player
    var/health = 100

/mob/player/on_heat()
    health -= 1

/proc/iter_turf_mob_obj(var/turf/burning_ground/tile)
    var/count = 0
    for (var/atom/subj as (mob|obj) in tile)
        subj.on_heat()
        count += 1
    return count

/proc/iter_turf_mob(var/turf/burning_ground/tile)
    var/count = 0
    for (var/atom/subj as mob in tile)
        subj.on_heat()
        count += 1
    return count

/proc/iter_turf_obj(var/turf/burning_ground/tile)
    var/count = 0
    for (var/atom/subj as obj in tile)
        subj.on_heat()
        count += 1
    return count

/proc/iter_turf_anything(var/turf/burning_ground/tile)
    var/count = 0
    for (var/atom/subj as anything in tile)
        subj.on_heat()
        count += 1
    return count
