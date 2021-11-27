#define R_IDEAL_GAS_EQUATION	8.31	//kPa*L/(K*mol)
#define ONE_ATMOSPHERE			101.325	//kPa
#define TCMB					2.7		// -270.3degC
#define TCRYO					265		// -48.15degC
#define T0C						273.15	// 0degC
#define T20C					293.15	// 20degC

#define MOLES_CELLSTANDARD		(ONE_ATMOSPHERE*CELL_VOLUME/(T20C*R_IDEAL_GAS_EQUATION))	//moles in a 2.5 m^3 cell at 101.325 Pa and 20 degC

#define CELL_VOLUME				2500	//liters in a cell
#define MINIMUM_TEMPERATURE_DELTA_TO_CONSIDER		0.5		//Minimum temperature difference before the gas temperatures are just set to be equal
#define MINIMUM_TEMPERATURE_TO_MOVE					(T20C+100)			//or this (or both, obviously)
#define MINIMUM_AIR_RATIO_TO_SUSPEND				0.005	//Minimum ratio of air that must move to/from a tile to suspend group processing
#define MINIMUM_AIR_TO_SUSPEND						(MOLES_CELLSTANDARD*MINIMUM_AIR_RATIO_TO_SUSPEND)	//Minimum amount of air that has to move before a group processing can be suspended
#define MINIMUM_MOLES_DELTA_TO_MOVE					(MOLES_CELLSTANDARD*MINIMUM_AIR_RATIO_TO_SUSPEND) //Either this must be active

#define OPEN_HEAT_TRANSFER_COEFFICIENT		0.4

#define SPECIFIC_HEAT_TOXIN		200
#define SPECIFIC_HEAT_AIR		20
#define SPECIFIC_HEAT_CDO		30
#define SPECIFIC_HEAT_N2O		40
#define SPECIFIC_HEAT_AGENT_B	300

#define HEAT_CAPACITY_CALCULATION(oxygen, carbon_dioxide, nitrogen, toxins, sleeping_agent, agent_b) \
	(carbon_dioxide * SPECIFIC_HEAT_CDO + (oxygen + nitrogen) * SPECIFIC_HEAT_AIR + toxins * SPECIFIC_HEAT_TOXIN + sleeping_agent * SPECIFIC_HEAT_N2O + agent_b * SPECIFIC_HEAT_AGENT_B)

#define MINIMUM_HEAT_CAPACITY	0.0003
#define QUANTIZE(variable)		(round(variable, 0.0001))

/datum/gas_mixture
	var/oxygen = 0
	var/carbon_dioxide = 0
	var/nitrogen = 0
	var/toxins = 0
	var/sleeping_agent = 0
	var/agent_b = 0

	var/volume = CELL_VOLUME

	var/temperature = 0 //in Kelvin

	var/last_share

	var/tmp/oxygen_archived
	var/tmp/carbon_dioxide_archived
	var/tmp/nitrogen_archived
	var/tmp/toxins_archived
	var/tmp/sleeping_agent_archived
	var/tmp/agent_b_archived

	var/tmp/temperature_archived

	var/tmp/fuel_burnt = 0

/datum/gas_mixture/proc/share(datum/gas_mixture/sharer)
    // abstract

/datum/gas_mixture/proc/heat_capacity()
	return HEAT_CAPACITY_CALCULATION(oxygen, carbon_dioxide, nitrogen, toxins, sleeping_agent, agent_b)

/datum/gas_mixture/proc/total_moles()
	var/moles = oxygen + carbon_dioxide + nitrogen + toxins + sleeping_agent + agent_b
	return moles

/datum/gas_mixture/proc/temperature_share(datum/gas_mixture/sharer, conduction_coefficient)
    // abstract

/datum/gas_mixture/proc/archive()
    // abstract

/datum/gas_mixture/archive()
	oxygen_archived = oxygen
	carbon_dioxide_archived = carbon_dioxide
	nitrogen_archived =  nitrogen
	toxins_archived = toxins
	sleeping_agent_archived = sleeping_agent
	agent_b_archived = agent_b

	temperature_archived = temperature

	return 1

/datum/gas_mixture/proc/heat_capacity_archived()
	return HEAT_CAPACITY_CALCULATION(oxygen_archived, carbon_dioxide_archived, nitrogen_archived, toxins_archived, sleeping_agent_archived, agent_b_archived)

/datum/gas_mixture/temperature_share(datum/gas_mixture/sharer, conduction_coefficient)

	var/delta_temperature = (temperature_archived - sharer.temperature_archived)
	if(abs(delta_temperature) > MINIMUM_TEMPERATURE_DELTA_TO_CONSIDER)
		var/self_heat_capacity = heat_capacity_archived()
		var/sharer_heat_capacity = sharer.heat_capacity_archived()

		if((sharer_heat_capacity > MINIMUM_HEAT_CAPACITY) && (self_heat_capacity > MINIMUM_HEAT_CAPACITY))
			var/heat = conduction_coefficient*delta_temperature * \
				(self_heat_capacity * sharer_heat_capacity / (self_heat_capacity + sharer_heat_capacity))

			temperature -= heat / self_heat_capacity
			sharer.temperature += heat / sharer_heat_capacity

/datum/gas_mixture/share(datum/gas_mixture/sharer, atmos_adjacent_turfs = 4)
	if(!sharer)
		return 0
	//If there is no difference why do the calculations?
	if(oxygen_archived == sharer.oxygen_archived && carbon_dioxide_archived == sharer.carbon_dioxide_archived && nitrogen_archived == sharer.nitrogen_archived &&\
	toxins_archived == sharer.toxins_archived && sleeping_agent_archived == sharer.sleeping_agent_archived && agent_b_archived == sharer.agent_b_archived && temperature_archived == sharer.temperature_archived)
		return 0
	var/delta_oxygen = QUANTIZE(oxygen_archived - sharer.oxygen_archived) / (atmos_adjacent_turfs + 1)
	var/delta_carbon_dioxide = QUANTIZE(carbon_dioxide_archived - sharer.carbon_dioxide_archived) / (atmos_adjacent_turfs + 1)
	var/delta_nitrogen = QUANTIZE(nitrogen_archived - sharer.nitrogen_archived) / (atmos_adjacent_turfs + 1)
	var/delta_toxins = QUANTIZE(toxins_archived - sharer.toxins_archived) / (atmos_adjacent_turfs + 1)
	var/delta_sleeping_agent = QUANTIZE(sleeping_agent_archived - sharer.sleeping_agent_archived) / (atmos_adjacent_turfs + 1)
	var/delta_agent_b = QUANTIZE(agent_b_archived - sharer.agent_b_archived) / (atmos_adjacent_turfs + 1)

	var/delta_temperature = (temperature_archived - sharer.temperature_archived)

	var/old_self_heat_capacity = 0
	var/old_sharer_heat_capacity = 0

	var/heat_capacity_self_to_sharer = 0
	var/heat_capacity_sharer_to_self = 0

	if(abs(delta_temperature) > MINIMUM_TEMPERATURE_DELTA_TO_CONSIDER)

		var/delta_air = delta_oxygen + delta_nitrogen
		if(delta_air)
			var/air_heat_capacity = SPECIFIC_HEAT_AIR * delta_air
			if(delta_air > 0)
				heat_capacity_self_to_sharer += air_heat_capacity
			else
				heat_capacity_sharer_to_self -= air_heat_capacity

		if(delta_carbon_dioxide)
			var/carbon_dioxide_heat_capacity = SPECIFIC_HEAT_CDO * delta_carbon_dioxide
			if(delta_carbon_dioxide > 0)
				heat_capacity_self_to_sharer += carbon_dioxide_heat_capacity
			else
				heat_capacity_sharer_to_self -= carbon_dioxide_heat_capacity

		if(delta_toxins)
			var/toxins_heat_capacity = SPECIFIC_HEAT_TOXIN * delta_toxins
			if(delta_toxins > 0)
				heat_capacity_self_to_sharer += toxins_heat_capacity
			else
				heat_capacity_sharer_to_self -= toxins_heat_capacity

		if(delta_sleeping_agent)
			var/sleeping_agent_heat_capacity = SPECIFIC_HEAT_N2O * delta_sleeping_agent
			if(delta_sleeping_agent > 0)
				heat_capacity_self_to_sharer += sleeping_agent_heat_capacity
			else
				heat_capacity_sharer_to_self -= sleeping_agent_heat_capacity

		if(delta_agent_b)
			var/agent_b_heat_capacity = SPECIFIC_HEAT_AGENT_B * delta_agent_b
			if(delta_agent_b > 0)
				heat_capacity_self_to_sharer += agent_b_heat_capacity
			else
				heat_capacity_sharer_to_self -= agent_b_heat_capacity

		old_self_heat_capacity = heat_capacity()
		old_sharer_heat_capacity = sharer.heat_capacity()

	oxygen -= delta_oxygen
	sharer.oxygen += delta_oxygen

	carbon_dioxide -= delta_carbon_dioxide
	sharer.carbon_dioxide += delta_carbon_dioxide

	nitrogen -= delta_nitrogen
	sharer.nitrogen += delta_nitrogen

	toxins -= delta_toxins
	sharer.toxins += delta_toxins

	sleeping_agent -= delta_sleeping_agent
	sharer.sleeping_agent += delta_sleeping_agent

	agent_b -= delta_agent_b
	sharer.agent_b += delta_agent_b

	var/moved_moles = (delta_oxygen + delta_carbon_dioxide + delta_nitrogen + delta_toxins + delta_sleeping_agent + delta_agent_b)
	last_share = abs(delta_oxygen) + abs(delta_carbon_dioxide) + abs(delta_nitrogen) + abs(delta_toxins) + abs(delta_sleeping_agent) + abs(delta_agent_b)

	if(abs(delta_temperature) > MINIMUM_TEMPERATURE_DELTA_TO_CONSIDER)
		var/new_self_heat_capacity = old_self_heat_capacity + heat_capacity_sharer_to_self - heat_capacity_self_to_sharer
		var/new_sharer_heat_capacity = old_sharer_heat_capacity + heat_capacity_self_to_sharer - heat_capacity_sharer_to_self

		if(new_self_heat_capacity > MINIMUM_HEAT_CAPACITY)
			temperature = (old_self_heat_capacity * temperature - heat_capacity_self_to_sharer * temperature_archived + heat_capacity_sharer_to_self * sharer.temperature_archived) / new_self_heat_capacity

		if(new_sharer_heat_capacity > MINIMUM_HEAT_CAPACITY)
			sharer.temperature = (old_sharer_heat_capacity * sharer.temperature - heat_capacity_sharer_to_self * sharer.temperature_archived + heat_capacity_self_to_sharer * temperature_archived) / new_sharer_heat_capacity

			if(abs(old_sharer_heat_capacity) > MINIMUM_HEAT_CAPACITY)
				if(abs(new_sharer_heat_capacity / old_sharer_heat_capacity - 1) < 0.10) // <10% change in sharer heat capacity
					temperature_share(sharer, OPEN_HEAT_TRANSFER_COEFFICIENT)

	if((delta_temperature > MINIMUM_TEMPERATURE_TO_MOVE) || abs(moved_moles) > MINIMUM_MOLES_DELTA_TO_MOVE)
		var/delta_pressure = temperature_archived * (total_moles() + moved_moles) - sharer.temperature_archived * (sharer.total_moles() - moved_moles)
		return delta_pressure * R_IDEAL_GAS_EQUATION / volume
