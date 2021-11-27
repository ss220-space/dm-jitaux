/proc/do_test()
    compile_proc("/datum/gas_mixture/share")
    compile_proc("/datum/gas_mixture/total_moles")
    CHECK_INSTALL_COMPILED // RES: /datum/gas_mixture/share, /datum/gas_mixture/share.1, /datum/gas_mixture/total_moles

    var/datum/gas_mixture/cell_2 = new
    cell_2.oxygen = 100
    cell_2.oxygen_archived = 100
    cell_2.temperature = 1000
    cell_2.temperature_archived = 1000

    var/datum/gas_mixture/cell_1 = new
    cell_1.oxygen = 10
    cell_1.oxygen_archived = 10
    cell_1.temperature = T20C
    cell_1.temperature_archived = T20C

    cell_1.archive()
    cell_2.archive()
    cell_1.share(cell_2)
    cell_1.archive()
    cell_2.archive()

    dmjit_bench_init()

    dmjit_bench_iterate("gas_mixture", "/proc/total_moles_jit", cell_1)
    dmjit_bench_iterate("gas_mixture", "/proc/gas_share_jit", cell_1, cell_2)
    dmjit_toggle_hooks() // Disable JIT-ed functions
    dmjit_bench_iterate("gas_mixture", "/proc/total_moles_byond", cell_1)
    dmjit_bench_iterate("gas_mixture", "/proc/gas_share_byond", cell_1, cell_2)

    dmjit_bench_finish()
    RES("")


/proc/total_moles_jit(var/datum/gas_mixture/cell)
    cell.total_moles()

/proc/total_moles_byond(var/datum/gas_mixture/cell)
    cell.total_moles()

/proc/gas_share_jit(var/datum/gas_mixture/cell_1, var/datum/gas_mixture/cell_2)
    cell_2.oxygen = 100
    cell_2.oxygen_archived = 100
    cell_2.temperature = 1000
    cell_2.temperature_archived = 1000

    cell_1.oxygen = 10
    cell_1.oxygen_archived = 10
    cell_1.temperature = T20C
    cell_1.temperature_archived = T20C

    cell_1.share(cell_2)


/proc/gas_share_byond(var/datum/gas_mixture/cell_1, var/datum/gas_mixture/cell_2)
    cell_2.oxygen = 100
    cell_2.oxygen_archived = 100
    cell_2.temperature = 1000
    cell_2.temperature_archived = 1000

    cell_1.oxygen = 10
    cell_1.oxygen_archived = 10
    cell_1.temperature = T20C
    cell_1.temperature_archived = T20C

    cell_1.share(cell_2)