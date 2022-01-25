/proc/do_test()
    compile_proc(/datum/gas_mixture/share)
    CHECK_INSTALL_COMPILED // RES: /datum/gas_mixture/share, /datum/gas_mixture/share.1

    var/datum/gas_mixture/cell_2 = new
    cell_2.oxygen = 100
    cell_2.oxygen_archived = 100
    cell_2.temperature = T20C
    cell_2.temperature_archived = T20C

    var/datum/gas_mixture/cell_1 = new
    cell_1.oxygen = 10
    cell_1.oxygen_archived = 10
    cell_1.temperature = T20C
    cell_1.temperature_archived = T20C


    MARK_REF_COUNT(cell_1)
    MARK_REF_COUNT(cell_2)

    RES(cell_1.share(cell_2)) // RES: -87.6988
    RES_CHECK_LEAK(cell_1) // RES: OK
    RES_CHECK_LEAK(cell_2) // RES: OK

    RES(cell_1.oxygen) // RES: 28
    cell_1.archive()
    cell_2.archive()

    RES(cell_1.last_share) // RES: 18

    RES(cell_1.share(cell_2)) // RES: -52.6193
    RES_CHECK_LEAK(cell_1) // RES: OK
    RES_CHECK_LEAK(cell_2) // RES: OK

    RES(cell_1.last_share) // RES: 10.8

    RES(cell_1.oxygen) // RES: 38.8