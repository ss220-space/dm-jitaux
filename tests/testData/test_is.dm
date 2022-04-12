/proc/do_test()
    compile_proc(/proc/is_null)
    compile_proc(/proc/is_text)
    compile_proc(/proc/is_num)
    compile_proc(/proc/is_turf)
    compile_proc(/proc/is_obj)
    compile_proc(/proc/is_mob)
    compile_proc(/proc/is_area)
    compile_proc(/proc/is_loc)
    compile_proc(/proc/is_movable)
    compile_proc(/proc/is_type)
    CHECK_INSTALL_COMPILED // RES: /is_null, /is_text, /is_num, /is_turf, /is_obj, /is_mob, /is_area, /is_loc, /is_movable, /is_type

    RES(is_null(null)) // RES: 1
    RES(is_null(1)) // RES: 0

    RES(is_text("tex")) // RES: 1
    RES(is_text(1)) // RES: 0

    RES(is_num(1)) // RES: 1
    RES(is_num("test")) // RES: 0

    var/obj/water/water = new /obj/water()
    var/turf/burning_ground/tile = locate(1, 1, 1)
    var/mob/player/player = new /mob/player()

    RES(is_turf(tile)) // RES: 1
    RES(is_turf("test")) // RES: 0
    RES(is_turf(player)) // RES: 0

    RES(is_obj(water)) // RES: 1
    RES(is_obj("test")) // RES: 0
    RES(is_obj(tile)) // RES: 0

    RES(is_mob(player)) // RES: 1
    RES(is_mob("test")) // RES: 0
    RES(is_mob(tile)) // RES: 0

    RES(is_area(tile.loc)) // RES: 1
    RES(is_area("test")) // RES: 0
    RES(is_area(tile)) // RES: 0

    RES(is_loc(tile.loc)) // RES: 1
    RES(is_loc(tile)) // RES: 1
    RES(is_loc(water)) // RES: 1
    RES(is_loc(player)) // RES: 1
    RES(is_loc("tile")) // RES: 0

    RES(is_movable(tile.loc)) // RES: 0
    RES(is_movable(tile)) // RES: 0
    RES(is_movable(water)) // RES: 1
    RES(is_movable(player)) // RES: 1
    RES(is_movable("tile")) // RES: 0

    var/obj/items/category/test/test = new /obj/items/category/test()

    RES(is_type("string", /obj/items)) // RES: 0
    RES(is_type(test, /obj/items/category/test)) // RES: 1
    RES(is_type(test, /obj/items/category)) // RES: 1
    RES(is_type(test, /obj/items)) // RES: 1
    RES(is_type(test, /obj/water)) // RES: 0
    RES(is_type(water, /obj/items)) // RES: 0

/proc/is_null(a)
    return isnull(a)

/proc/is_text(a)
    return istext(a)

/proc/is_num(a)
    return isnum(a)

/proc/is_turf(a)
    return isturf(a)

/proc/is_obj(a)
    return isobj(a)

/proc/is_mob(a)
    return ismob(a)

/proc/is_area(a)
    return isarea(a)

/proc/is_loc(a)
    return isloc(a)

/proc/is_movable(a)
    return ismovable(a)

/proc/is_type(a, b)
    return istype(a, b)

/turf/burning_ground

/obj/water

/obj/items

/obj/items/category

/obj/items/category/test

/mob/player