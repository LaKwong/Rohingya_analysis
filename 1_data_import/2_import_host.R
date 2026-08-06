# This reads in the host dataset 
# Author: Chris LeBoa
# Version: 2022-12-05

# Libraries
library(tidyverse)

# Parameters

#===============================================================================
############################################################3
########### HOST #####################
############################################################3

#Code



# baseline 
survey_data_host_baseline <- 
	read_csv(here::here("2_data_raw/RohingyaFuelMaster_20200220_Corrected_20200308_HOST.csv"), col_types = cols(.default = col_character()), name_repair = "universal") %>%
	filter(consent == "OK")

# midline 
# v95 was 7 Oct 2020 and onwards
data_today_v95 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v95_endline_Rohingya_host.csv"), col_types = cols(.default = col_character()), name_repair = "universal") %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) %>%
	rename(
		reduce_meals_food = reduce_meals...388,
		not_eat_food = not_eat...389,
		reduce_meals_fuel = reduce_meals...413,
		not_eat_fuel = not_eat...414
	)
data_today_hh_v95 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v95_endline_Rohingya_host-hh_members.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_symptoms_v95 <-
	read_csv( here::here("2_data_raw/rohingya_fuel_v95_endline_Rohingya_host-symptoms.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_location_v95 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v95_endline_Rohingya_host-location.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 


###########################################
#endline
#This is the data that was collected in 2022 - the third round of surveys for the project. 
# V119 

data_today_host_v119 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v119_host.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_host_symptoms_v119 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v119_host-symptoms.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_host_hhmembers_v119 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v119_host.csv"), col_types = cols(.default = col_character()), name_repair = "universal")

########### HOST #####################
data_host_wide_character <-
	survey_data_host_baseline %>%
	full_join(data_today_v94 %>% filter(study_arm %in% c(4, 5)), by = c("hh_id"), suffix = c("", ".midline")) %>%
	full_join(data_today_v95 %>% filter(study_arm %in% c(4, 5)), by = c("hh_id"), suffix = c("", ".midline")) %>%
	full_join(data_today_host_v119, by = c("hh_id"), suffix = c("", ".endline")) %>%
	# select(-X1) %>%
	select(hh_id, everything()) 


data_host_long_character <-
	bind_rows(
		survey_data_host_baseline,
		data_today_v94 %>%
			filter(study_arm %in% c(4, 5))
	) %>%
	bind_rows(
		data_today_v95 %>%
			filter(study_arm %in% c(4, 5))
	) %>%
	bind_rows(data_today_host_v119)
# select(-X1) %>%
select(hh_id, everything())

data_hh_host_base <-
	bind_rows(
		data_today_hh_v94 %>%
			filter(PARENT_KEY %in% c(data_today_v94 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY))),
		data_today_hh_v95 %>%
			filter(PARENT_KEY %in% c(data_today_v95 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY))), 
		data_today_host_v119
	)

data_symptoms_host_base <-
	bind_rows(
		data_today_symptoms_v94 %>%
			filter(PARENT_KEY %in% c(data_today_v94 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY))),
		data_today_symptoms_v95 %>%
			filter(PARENT_KEY %in% c(data_today_v95 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY))), 
		data_today_host_symptoms_v119
	) 

data_location_host_base <-
	bind_rows(
		data_today_location_v94 %>%
			filter(PARENT_KEY %in% c(data_today_v94 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY))),
		data_today_location_v95 %>%
			filter(PARENT_KEY %in% c(data_today_v95 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY)))
	) 

data_hh_member_host_character <-
	data_hh_host_base %>% # 2169 rows
	left_join(
		data_host_wide_character %>% select(KEY, hh_id_host), by = c("PARENT_KEY" = "KEY") # 1078 rows
	) %>%
	left_join(
		data_symptoms_host_base, by = c("PARENT_KEY", "name" = "name_roster") # 1078 rows
	) %>%
	left_join(
		data_location_host_base, by = c("PARENT_KEY", "name" = "location_name_roster") # 1078 rows
	) %>%
	select(PARENT_KEY, hh_id_host, name, everything())



########### HOST #####################
data_host_wide <-
	data_host_wide_character %>%
	type_convert(
		col_types = NULL,
		na = c("", "NA"),
		trim_ws = TRUE,
		locale = default_locale()
	) %>%
	# mutate(
	# 	SubmissionDate = parse_date_time(SubmissionDate, orders = "mdy IMS", tz = "Asia/Dhaka"),
	# 	starttime = parse_date_time(starttime, orders = "mdy IMS", tz = "Asia/Dhaka"),
	# 	endtime = parse_date_time(endtime, orders = "mdy IMS", tz = "Asia/Dhaka"),
	# 	start_date = mdy(start_date) # Not working for most values, not sure why
	# ) %>%
	mutate(fcn_id = as.character(fcn_id)) %>%
	mutate_at(
		vars(
			block_id, # fcn_id, camp_id
			corn_source, bread_source, beef_source, veggies_source, lentils_source, eggs_source, poultry_source, 
			oil_source, fish_source, rice_source, potatoes_source, fruit_source, goat_sheep_source, sugar_source, dairy_source, 
			contact_number, first_enrolled_lpg, first_receive_lpg, date_safety_training, lpg_changes_lifestyle,
			# lpg_cylinder_repair, lpg_stove_repair,
			# fuel_cant_afford_action, food_cant_afford_action
			paste0(c("corn_source", "bread_source", "beef_source", "veggies_source", "lentils_source", "eggs_source", "poultry_source", 
							 "oil_source", "fish_source", "rice_source", "potatoes_source", "fruit_source", "goat_sheep_source", "sugar_source", "dairy_source", 
							 "first_enrolled_lpg", "first_receive_lpg", "lpg_changes_lifestyle"), ".endline")
		), 
		list(as.factor)
	) %>%
	mutate_at(
		vars(
			income_cash_ngo, income_abroad, income_farming, income_own_business, 
			income_wage_labor, income_skill_labor, income_handicrafts_tailoring, 
			income_humanitarian_asst, income_selling_wood,     
			life_failure, fearful, restless_sleep, less_talkative, lonely, 
			unfriendly_people, crying_spells, sick, feeling_disliked, 
			cant_get_going, suicidal_thoughts_30,
			paste0(
				c("income_cash_ngo", "income_abroad", "income_farming", "income_own_business", 
					"income_wage_labor", "income_skill_labor", "income_handicrafts_tailoring", 
					"income_humanitarian_asst", "income_selling_wood",     
					"life_failure", "fearful", "restless_sleep", "less_talkative", "lonely", 
					"unfriendly_people", "crying_spells", "sick", "feeling_disliked", 
					"cant_get_going", "suicidal_thoughts_30"), 
				".endline"
			)
		), 
		funs(as.numeric)
	)

data_host_long <-
	data_host_long_character %>%
	type_convert(
		col_types = NULL,
		na = c("", "NA"),
		trim_ws = TRUE,
		locale = default_locale()
	) %>%
	mutate(fcn_id = as.character(fcn_id)) %>%
	mutate_at(
		vars(
			block_id, # fcn_id, camp_id
			corn_source, bread_source, beef_source, veggies_source, lentils_source, eggs_source, poultry_source, 
			oil_source, fish_source, rice_source, potatoes_source, fruit_source, goat_sheep_source, sugar_source, dairy_source, 
			contact_number, first_enrolled_lpg, first_receive_lpg, date_safety_training, lpg_changes_lifestyle #,
			# lpg_cylinder_repair, lpg_stove_repair,
			# fuel_cant_afford_action, food_cant_afford_action
		), 
		list(as.factor)
	) %>%
	mutate_at(
		vars(
			income_cash_ngo, income_abroad, income_farming, income_own_business, 
			income_wage_labor, income_skill_labor, income_handicrafts_tailoring, 
			income_humanitarian_asst, income_selling_wood,     
			life_failure, fearful, restless_sleep, less_talkative, lonely, 
			unfriendly_people, crying_spells, sick, feeling_disliked, 
			cant_get_going, suicidal_thoughts_30
		), 
		funs(as.numeric)
	)

write_rds(data_host_wide, here::here("2_data_raw/RohingyaFuel_survey_data_wide_host.rds"))
write_rds(data_host_long, here::here("2_data_raw/RohingyaFuel_survey_data_long_host.rds"))
write_rds(data_hh_member_host, here::here("2_data_raw/RohingyaFuel_data_hh_member_host.rds"))
