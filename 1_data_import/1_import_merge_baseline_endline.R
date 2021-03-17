################################################################################
# @Project: Rohingya LPG Evaluation
# @Author: Laura H Kwong
# @Description: merge different versions of baseline and endline hh survey data
# @Version: 3.6.1
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))

# ============================================================================
# Parameters
# file_in <- here::here("1_data_raw/coded_clips_data.rds")
# file_out <- here::here("4_data/data.rds")


file_out_survey_multiple_entry_errors <- here::here("4_data/survey_multiple_entry_errors.xlsx")

# ============================================================================

############## Load Rohingya data ###################

# baseline
# Baseline master files that INCLUDE corrections noted in data review

survey_data_baseline <- 
	read_csv(here::here("2_data_raw/RohingyaFuelMaster_Corrected_20200419_refugee.csv"), col_types = cols(.default = col_character())) %>%
	filter(consent == "OK") %>%
	# This is data cleaning but it is fundamental at this step
	mutate(
		# some hh are missing the study_arm
		study_arm = ifelse(camp_id %in% c("8W", "9", "10"), 1, study_arm), # pre-intervention 
		study_arm = ifelse(camp_id %in% c("3", "4", "5", "8E", "18"), 2, study_arm) # intervention 
	)


# endline
# List input files and read them
# 20 Sep 2020
data_today_v86 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v86_endline_Rohingya.csv"), col_types = cols(.default = col_character())) %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	)
data_today_hh_v86 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v86_endline_Rohingya-hh_members.csv"), col_types = cols(.default = col_character())) 
data_today_symptoms_v86 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v86_endline_Rohingya-symptoms.csv"), col_types = cols(.default = col_character())) 
data_today_location_v86 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v86_endline_Rohingya-location.csv"), col_types = cols(.default = col_character())) 

# 21 Sep 2020
data_today_v89 <- read_csv(here::here("2_data_raw/rohingya_fuel_v89_endline_Rohingya.csv"), col_types = cols(.default = col_character())) %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	)
data_today_hh_v89 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v89_endline_Rohingya-hh_members.csv"), col_types = cols(.default = col_character())) 
data_today_symptoms_v89 <-
	read_csv(here::here("2_data_raw/rohingya_fuel_v89_endline_Rohingya-symptoms.csv"), col_types = cols(.default = col_character())) 
data_today_location_v89 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v89_endline_Rohingya-location.csv"), col_types = cols(.default = col_character())) 

# 22 Sep 2020
data_today_v90 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v90_endline_Rohingya.csv"), col_types = cols(.default = col_character())) %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	)
data_today_hh_v90 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v90_endline_Rohingya-hh_members.csv"), col_types = cols(.default = col_character())) 
data_today_symptoms_v90 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v90_endline_Rohingya-symptoms.csv"), col_types = cols(.default = col_character())) 
data_today_location_v90 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v90_endline_Rohingya-location.csv"), col_types = cols(.default = col_character())) 

# data_today_v91 <- read_csv(here::here("2_data_raw/rohingya_fuel_v91_endline_Rohingya.csv")) # 24? Sep 2020 - 14 surveys that are not yet on the surver
# data_today_hh_v91 <- read_csv(here::here("2_data_raw/rohingya_fuel_v91_endline_Rohingya-hh_members.csv")) 
# data_today_symptoms_v91 <- read_csv(here::here("2_data_raw/rohingya_fuel_v91_endline_Rohingya-symptoms.csv")) 
# data_today_location_v91 <- read_csv(here::here("2_data_raw/rohingya_fuel_v91_endline_Rohingya-location.csv")) 

# 26-20 Sep 2020
data_today_v92 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v92_endline_Rohingya.csv"), col_types = cols(.default = col_character())) %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) 
data_today_hh_v92 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v92_endline_Rohingya-hh_members.csv"), col_types = cols(.default = col_character())) 
data_today_symptoms_v92 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v92_endline_Rohingya-symptoms.csv"), col_types = cols(.default = col_character())) 
data_today_location_v92 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v92_endline_Rohingya-location.csv"), col_types = cols(.default = col_character())) 

# 1, 4, 5 Oct 2020
data_today_v93 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v93_endline_Rohingya.csv"), col_types = cols(.default = col_character())) %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) 
data_today_hh_v93 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v93_endline_Rohingya-hh_members.csv"), col_types = cols(.default = col_character())) 
data_today_symptoms_v93 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v93_endline_Rohingya-symptoms.csv"), col_types = cols(.default = col_character())) 
data_today_location_v93 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v93_endline_Rohingya-location.csv"), col_types = cols(.default = col_character())) 

# 7 Oct 2020 and onwards
data_today_v94 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v94_endline_Rohingya.csv"), col_types = cols(.default = col_character())) %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) 
data_today_hh_v94 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v94_endline_Rohingya-hh_members.csv"), col_types = cols(.default = col_character())) 
data_today_symptoms_v94 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v94_endline_Rohingya-symptoms.csv"), col_types = cols(.default = col_character())) 
data_today_location_v94 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v94_endline_Rohingya-location.csv"), col_types = cols(.default = col_character())) 

########### HOST #####################

# baseline 
survey_data_host_baseline <- 
	read_csv(here::here("2_data_raw/RohingyaFuelMaster_20200220_Corrected_20200308_HOST.csv"), col_types = cols(.default = col_character())) %>%
	filter(consent == "OK")

# endline
# v95 was 7 Oct 2020 and onwards
data_today_v95 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v94_endline_Rohingya.csv"), col_types = cols(.default = col_character())) %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) 
data_today_v95 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v95_endline_Rohingya_host.csv"), col_types = cols(.default = col_character())) 
data_today_hh_v95 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v95_endline_Rohingya_host-hh_members.csv"), col_types = cols(.default = col_character())) 
data_today_symptoms_v95 <-
	read_csv( here::here("2_data_raw/rohingya_fuel_v95_endline_Rohingya_host-symptoms.csv"), col_types = cols(.default = col_character())) 
data_today_location_v95 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v95_endline_Rohingya_host-location.csv"), col_types = cols(.default = col_character())) 



################## Merge data #############################333

########### Rohingya #####################
data_endline_base <-
	bind_rows(
		data_today_v86,
		data_today_v89
	) %>%
	bind_rows(
		data_today_v90
	) %>%
	bind_rows(
		data_today_v92
	) %>%
	bind_rows(
		data_today_v93
	) %>%
	bind_rows(
		data_today_v94 %>%
			filter(study_arm %in% c(1, 2, 3, 6))
	) %>%
	bind_rows(
		data_today_v95 %>%
			filter(study_arm %in% c(1, 2, 3, 6))
	) %>%
	# This is data cleaning but it is necesary at this step
	mutate(	# some hh are missing the study_arm
		study_arm = ifelse(camp_id %in% c("8W", "9", "10"), 3, study_arm), # post-intervention
		study_arm = ifelse(camp_id %in% c("3", "4", "5", "8E", "18"), 6, study_arm) # intervention follow-up
	)

data_wide_character <-
	survey_data_baseline %>%
	full_join(data_endline_base, by = c("fcn_id", "camp_id", "block_id", "subblock_id"), suffix = c("", ".endline")) %>%
	select(-X1) %>%
	select(fcn_id, everything())

data_long_character <-
	bind_rows(
		survey_data_baseline,
		data_endline_base
	) %>%
	select(-X1) %>%
	select(fcn_id, everything())

data_hh_base <-
	bind_rows(
		data_today_hh_v86,
		data_today_hh_v89
	) %>% 
	bind_rows(
		data_today_hh_v90
	) %>%
	bind_rows(
		data_today_hh_v92
	) %>%
	bind_rows(
		data_today_hh_v93
	) %>%
	bind_rows(
		data_today_hh_v94 %>%
			filter(PARENT_KEY %in% c(data_today_v94 %>% filter(study_arm %in% c(1, 2, 3, 6)) %>% pull(KEY)))
	) %>%
	bind_rows(
		data_today_hh_v95 %>%
			filter(PARENT_KEY %in% c(data_today_v95 %>% filter(study_arm %in% c(1, 2, 3, 6)) %>% pull(KEY)))
	) 

data_symptoms_base <-
	bind_rows(
		data_today_symptoms_v86,
		data_today_symptoms_v89
	) %>% 
	bind_rows(
		data_today_symptoms_v90
	) %>%
	bind_rows(
		data_today_symptoms_v92
	) %>%
	bind_rows(
		data_today_symptoms_v93
	) %>%
	bind_rows(
		data_today_symptoms_v94 %>%
			filter(PARENT_KEY %in% c(data_today_v94 %>% filter(study_arm %in% c(1, 2, 3, 6)) %>% pull(KEY)))
	) %>%
	bind_rows(
		data_today_symptoms_v95 %>%
			filter(PARENT_KEY %in% c(data_today_v95 %>% filter(study_arm %in% c(1, 2, 3, 6)) %>% pull(KEY)))
	) 

data_location_base <-
	bind_rows(
		data_today_location_v86,
		data_today_location_v89
	) %>% 
	bind_rows(
		data_today_location_v90
	) %>%
	bind_rows(
		data_today_location_v92
	) %>%
	bind_rows(
		data_today_location_v93
	) %>%
	bind_rows(
		data_today_location_v94 %>%
			filter(PARENT_KEY %in% c(data_today_v94 %>% filter(study_arm %in% c(1, 2, 3, 6)) %>% pull(KEY)))
	) %>%
	bind_rows(
		data_today_location_v95 %>%
			filter(PARENT_KEY %in% c(data_today_v95 %>% filter(study_arm %in% c(1, 2, 3, 6)) %>% pull(KEY)))
	) 

data_hh_member_character <-
	data_hh_base %>% # 4659 rows
	left_join(
		data_wide_character %>% select(KEY, fcn_id), by = c("PARENT_KEY" = "KEY")
	) %>%
	left_join(
		data_symptoms_base, by = c("PARENT_KEY", "name" = "name_roster") # 4659 rows
	) %>%
	left_join(
		data_location_base, by = c("PARENT_KEY", "name" = "location_name_roster") # 4659 rows
	) %>%
	select(PARENT_KEY, fcn_id, name, everything())
# Total 4677 rows -> there must have been some name misspellings --> 
# I don't know how this can be since the name had to be selected from the roster.  I'll figure this out later




########### HOST #####################
data_host_wide_character <-
	survey_data_host_baseline %>%
	full_join(data_today_v94 %>% filter(study_arm %in% c(4, 5)), by = c("hh_id"), suffix = c("", ".endline")) %>%
	full_join(data_today_v95 %>% filter(study_arm %in% c(4, 5)), by = c("hh_id"), suffix = c("", ".endline")) %>%
	select(-X1) %>%
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
	select(-X1) %>%
	select(hh_id, everything())

data_hh_host_base <-
	bind_rows(
		data_today_hh_v94 %>%
			filter(PARENT_KEY %in% c(data_today_v94 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY))),
		data_today_hh_v95 %>%
			filter(PARENT_KEY %in% c(data_today_v95 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY)))
	) 

data_symptoms_host_base <-
	bind_rows(
		data_today_symptoms_v94 %>%
			filter(PARENT_KEY %in% c(data_today_v94 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY))),
		data_today_symptoms_v95 %>%
			filter(PARENT_KEY %in% c(data_today_v95 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY)))
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



#################  Change from cols of type character back to cols of guessed type ##############################

########### Rohingya #####################

data_wide <-
	data_wide_character %>%
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

data_long <-
	data_long_character %>%
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


############### INDIVIDUAL DATA #####################
data_hh_member <-
	data_hh_member_character %>%
	type_convert(
		col_types = NULL,
		na = c("", "NA"),
		trim_ws = TRUE,
		locale = default_locale()
	)

data_hh_member_host <-
	data_hh_member_host_character %>%
	type_convert(
		col_types = NULL,
		na = c("", "NA"),
		trim_ws = TRUE,
		locale = default_locale()
	)



################# Save data ########################

write_rds(data_wide, here::here("2_data_raw/RohingyaFuel_survey_data_wide.rds"))
write_rds(data_long, here::here("2_data_raw/RohingyaFuel_survey_data_long.rds"))

write_rds(data_host_wide, here::here("2_data_raw/RohingyaFuel_survey_data_wide_host.rds"))
write_rds(data_host_long, here::here("2_data_raw/RohingyaFuel_survey_data_long_host.rds"))

write_rds(data_hh_member, here::here("2_data_raw/RohingyaFuel_data_hh_member.rds"))
write_rds(data_hh_member_host, here::here("2_data_raw/RohingyaFuel_data_hh_member_host.rds"))

