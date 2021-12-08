/proc/do_test()
    compile_proc(/datum/gas_mixture/proc/total_moles)
    CHECK_INSTALL_COMPILED // RES: /datum/gas_mixture/total_moles

    var/datum/gas_mixture/mix = new
    var/moles = mix.total_moles() + mix.total_moles() + mix.total_moles()
    RES(moles) // RES: 63


/datum/gas_mixture
	var/oxygen = 1
	var/nitrogen = 2
	var/carbon_dioxide = 3
	var/sleepeing_agent = 4
	var/agent_b = 5
	var/toxins = 6

/datum/gas_mixture/proc/total_moles()
	var/moles = oxygen + nitrogen + carbon_dioxide + sleepeing_agent + agent_b + toxins
	return moles