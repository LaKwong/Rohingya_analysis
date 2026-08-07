################################################################################
# @Project: Rohingya LPG Evaluation
# @Author: Laura H Kwong
# @Description: merge different versions of baseline and endline hh survey data
# @Version: 3.6.1
# @Date: 220707
################################################################################

rm(list = ls())

source(here::here("0_config.R"))



# ============================================================================
# files in
# So many that I load at the time of import

# files out
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



# variable reduce_meals shows up 3 times
# V86: not enough food -> reduce_food is followed by reduce_meals; not enough fuel -> reduce_fuel is followed by reduce_meals
#
# variable not_eat shows up 3 times
# V86: not enough food -> reduce_food is followed 2 col later by not_eat; not enough fuel -> reduce_fuel is followed 2 cols later by not_eat
#
# easiest way to fix the cols with duplicate names is to call them by position then rename them. 
# This is risky because if the underlying organization of the data changes, the method will fall apart, but the data is fixed so I'll go with this method


# name_repair = "universal" # changes the / to a . and - to . so that they are syntactically able to be read by non-standard eval df$name and 
# `__version__` -> `.__version__`

##################################################
# midline
# List input files and read them
# 20 Sep 2020
data_today_v86 <- 
	read_csv(
		here::here("2_data_raw/rohingya_fuel_v86_endline_Rohingya.csv"), col_types = cols(.default = col_character()), 
		name_repair = "universal"  
		# name_repair = make.names(c("reduce_meals", "reduce_meals"), unique = TRUE, allow_ = TRUE)
	) %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) %>%
	# rename vars at NS = col 383 and NT = col 384; OQ  = col 407 and OR = col 408 (https://www.vishalon.net/blog/excel-column-letter-to-number-quick-reference)
	rename(
		reduce_meals_food = reduce_meals...383, #.[[383]]
		not_eat_food = not_eat...384, # .[[384]]
		reduce_meals_fuel = reduce_meals...407, # .[[407]]
		not_eat_fuel = not_eat...408 # .[[408]]
	)

data_today_hh_v86 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v86_endline_Rohingya-hh_members.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_symptoms_v86 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v86_endline_Rohingya-symptoms.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_location_v86 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v86_endline_Rohingya-location.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 

# 21 Sep 2020
data_today_v89 <- read_csv(here::here("2_data_raw/rohingya_fuel_v89_endline_Rohingya.csv"), col_types = cols(.default = col_character()), name_repair = "universal") %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) %>%
	rename(
		reduce_meals_food = reduce_meals...384,
		not_eat_food = not_eat...385,
		reduce_meals_fuel = reduce_meals...408,
		not_eat_fuel = not_eat...409
	)
data_today_hh_v89 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v89_endline_Rohingya-hh_members.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_symptoms_v89 <-
	read_csv(here::here("2_data_raw/rohingya_fuel_v89_endline_Rohingya-symptoms.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_location_v89 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v89_endline_Rohingya-location.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 

# 22 Sep 2020
data_today_v90 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v90_endline_Rohingya.csv"), col_types = cols(.default = col_character()), name_repair = "universal") %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) %>%
	rename(
		reduce_meals_food = reduce_meals...387,
		not_eat_food = not_eat...388,
		reduce_meals_fuel = reduce_meals...411,
		not_eat_fuel = not_eat...412
	)
data_today_hh_v90 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v90_endline_Rohingya-hh_members.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_symptoms_v90 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v90_endline_Rohingya-symptoms.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_location_v90 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v90_endline_Rohingya-location.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 

# data_today_v91 <- read_csv(here::here("2_data_raw/rohingya_fuel_v91_endline_Rohingya.csv")) # 24? Sep 2020 - 14 surveys that are not yet on the surver
# data_today_hh_v91 <- read_csv(here::here("2_data_raw/rohingya_fuel_v91_endline_Rohingya-hh_members.csv")) 
# data_today_symptoms_v91 <- read_csv(here::here("2_data_raw/rohingya_fuel_v91_endline_Rohingya-symptoms.csv")) 
# data_today_location_v91 <- read_csv(here::here("2_data_raw/rohingya_fuel_v91_endline_Rohingya-location.csv")) 

# 26-20 Sep 2020
data_today_v92 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v92_endline_Rohingya.csv"), col_types = cols(.default = col_character()), name_repair = "universal") %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	)  %>%
	rename(
		reduce_meals_food = reduce_meals...388,
		not_eat_food = not_eat...389,
		reduce_meals_fuel = reduce_meals...413,
		not_eat_fuel = not_eat...414
	)
data_today_hh_v92 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v92_endline_Rohingya-hh_members.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_symptoms_v92 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v92_endline_Rohingya-symptoms.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_location_v92 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v92_endline_Rohingya-location.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 

# 1, 4, 5 Oct 2020
data_today_v93 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v93_endline_Rohingya.csv"), col_types = cols(.default = col_character()), name_repair = "universal") %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	)  %>%
	rename(
		reduce_meals_food = reduce_meals...388,
		not_eat_food = not_eat...389,
		reduce_meals_fuel = reduce_meals...413,
		not_eat_fuel = not_eat...414
	)
data_today_hh_v93 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v93_endline_Rohingya-hh_members.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_symptoms_v93 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v93_endline_Rohingya-symptoms.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_location_v93 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v93_endline_Rohingya-location.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 

# 7 Oct 2020 and onwards
# There are 8 host community households that were administered v94 of the survey, even though v94 was designed for Rohingya household. These host community hh have study_arm == 5. It will be hard to keep this data because many of the variable names won't match
data_today_v94 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v94_endline_Rohingya.csv"), col_types = cols(.default = col_character()), name_repair = "universal") %>%
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
data_today_hh_v94 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v94_endline_Rohingya-hh_members.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_symptoms_v94 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v94_endline_Rohingya-symptoms.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_location_v94 <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v94_endline_Rohingya-location.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 

###########################################
#endline
#This is the data that was collected in 2022 - the third round of surveys for the project. 
# V111, 112, 113, 114, 115, 116
# there is no location data rohingya_fuel_vxxx-location.csv for v111-v116

# v111
data_today_v111 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v111.csv"), col_types = cols(.default = col_character()), name_repair = "universal") %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) %>%
	rename(
		reduce_meals_fuel = reduce_meals_001,
		not_eat_fuel = not_eat_001
	) 
data_today_hh_v111 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v111-hh_members.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_symptoms_v111 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v111-symptoms.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 

# v112

## v112 has no data

# v113
data_today_v113 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v113.csv"), col_types = cols(.default = col_character()), name_repair = "universal") %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) %>%
	rename(
		reduce_meals_fuel = reduce_meals_001,
		not_eat_fuel = not_eat_001
	) 
data_today_hh_v113 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v113-hh_members.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_symptoms_v113 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v113-symptoms.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 

# v114
data_today_v114 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v114.csv"), col_types = cols(.default = col_character()), name_repair = "universal") %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) %>%
	rename(
		reduce_meals_fuel = reduce_meals_001,
		not_eat_fuel = not_eat_001
	) 
data_today_hh_v114 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v114-hh_members.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_symptoms_v114 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v114-symptoms.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 

# v115
data_today_v115 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v115.csv"), col_types = cols(.default = col_character()), name_repair = "universal") %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) %>%
	rename(
		reduce_meals_fuel = reduce_meals_001,
		not_eat_fuel = not_eat_001
	) 
data_today_hh_v115 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v115-hh_members.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_symptoms_v115 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v115-symptoms.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 

# v116
data_today_v116 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v116.csv"), col_types = cols(.default = col_character()), name_repair = "universal") %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) %>%
	rename(
		reduce_meals_fuel = reduce_meals_001,
		not_eat_fuel = not_eat_001
	) 
data_today_hh_v116 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v116-hh_members.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_symptoms_v116 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v116-symptoms.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 















############################################################3
########### HOST #####################
############################################################3


# baseline 
survey_data_host_baseline <- 
	read_csv(here::here("2_data_raw/RohingyaFuelMaster_20200220_Corrected_20200308_HOST.csv"), col_types = cols(.default = col_character()), name_repair = "universal") %>%
	filter(consent == "OK") %>%
	mutate(hh_id_short = grep("[0-9]{4}$", serial_id))

# midline 
# v95 was 7 Oct 2020 and onwards
# There were two Rohingy hh that were interviewed using v95 - v95 was designed for host communities so these Rohingya hh results should be discarded (fcn_id = "123698" and fcn_id = "125036")
data_today_v95_base <- 
	read_csv(here::here("2_data_raw/rohingya_fuel_v95_endline_Rohingya_host.csv"), col_types = cols(.default = col_character()), name_repair = "universal")

data_today_v95 <- 
	data_today_v95_base %>%
	select(
		-starts_with("generated_note"),
		-starts_with("reserved_name")
	) %>%
	rename(
		hh_id_short = fcn_id, # there is fcn_id, for which most host hh have a four-digit id that starts with a "3" or "4" and some zeros; then there is a hh_id_host, for which most host hh have a 0, three-digit number that starts with a "5" or a four-digit number that stars with a "1"
		reduce_meals_food = `reduce_meals...386`,
		not_eat_food = `not_eat...387`,
		reduce_meals_fuel = `reduce_meals...411`,
		not_eat_fuel = `not_eat...412`
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
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v119_host.csv"), col_types = cols(.default = col_character()), name_repair = "universal") %>%
	mutate(hh_id_short = house_id)
data_today_host_symptoms_v119 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v119_host-symptoms.csv"), col_types = cols(.default = col_character()), name_repair = "universal") 
data_today_host_hhmembers_v119 <- 
	read_csv(here::here("2_data_raw/survey_endline_with_review/rohingya_fuel_v119_host.csv"), col_types = cols(.default = col_character()), name_repair = "universal")


##########
##### These are great ideas from Chris to quickly read in the data, but I don't think it worked because of the duplicate col names that needed to be treated differently for different versions of the survey
#########
# #may need to reset the path to this folder if connecting from a different computer (set it to whatever )
# 
# all_files <- 
# 	list.files(path = "/Volumes/GoogleDrive/.shortcut-targets-by-id/1g4FGyxS0paoGycT8REXc9ZEXkWLVDMJr/Rohingya_analysis/2_data_raw/survey_endline_with_review", pattern = ".csv", full.names = TRUE)
# #finds all files from raw folder with .csv pattern
# 
# symptoms_files <- all_files[str_detect(all_files, "symptoms")]
# hh_members_files <-  all_files[str_detect(all_files, "hh_members")]
# hh_files <-  all_files[str_detect(all_files, "[0-9].csv")]


# ## I am not really sure what I should do with the individual data between midline and endline for these 
# endline_symptoms_repeat <- 
# 	symptoms_files %>% 
# 	map_dfr(read_csv, col_types = cols(.default = "c"))
# 
# endline_hh_members_repeat <- 
# 	hh_members_files %>% 
# 	map_dfr(read_csv, col_types = cols(.default = "c"))
# 
# data_endline_base <- 
# 	hh_files %>% 
# 	map_dfr(read_csv, col_types = cols(.default = "c"))










###################################################################3
################## Merge data #############################333
########################################################################


########### Rohingya #####################

survey_data_baseline <-
	survey_data_baseline %>%
	mutate(timepoint = "baseline")

data_midline_base <-
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
	# bind_rows(
	# 	data_today_v95 %>%
	# 		filter(study_arm %in% c(1, 2, 3, 6))
	# ) %>%
	# This is data cleaning but it is necesary at this step
	mutate(	# some hh are missing the study_arm
		study_arm = ifelse(camp_id %in% c("8W", "9", "10"), 3, study_arm), # intervention
		study_arm = ifelse(camp_id %in% c("3", "4", "5", "8E", "18"), 6, study_arm) # comparison
	) %>%
	mutate(timepoint = "midline")

data_endline_base <-
	bind_rows(
		data_today_v111,
		data_today_v113
	) %>%
	bind_rows(
		data_today_v114
	) %>%
	bind_rows(
		data_today_v115
	) %>%
	bind_rows(
		data_today_v116
	) %>%
	# This is data cleaning but it is necesary at this step
	# endline
	# 7 = post-intervention rd3 (intervention)
	# 8 = intervention follow-up rd3 (comparison)
	mutate(	# some hh are missing the study_arm
		study_arm = ifelse(camp_id %in% c("8W", "9", "10"), 7, study_arm), # intervention
		study_arm = ifelse(camp_id %in% c("3", "4", "5", "8E", "18"), 8, study_arm) # comparison
	) %>%
	mutate(timepoint = "endline")

# data_wide_character <-
# 	survey_data_baseline %>%
# 	full_join(data_midline_base, by = c("fcn_id", "camp_id", "block_id", "subblock_id"), suffix = c("", ".midline")) %>%
# 	full_join(data_endline_base, by = c("fcn_id", "camp_id", "block_id", "subblock_id"), suffix = c("", ".endline")) %>% 
# 	# select(-X1) %>%
# 	select(fcn_id, everything())

data_long_character <-
	bind_rows(
		survey_data_baseline,
		data_midline_base, 
		data_endline_base
	) %>%
	# select(-X1) %>%
	select(timepoint, fcn_id, everything())


######### Combine the individual data: hh, symptoms, location ##########33
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
	# bind_rows(
	# 	data_today_hh_v95 %>%
	# 		filter(PARENT_KEY %in% c(data_today_v95 %>% filter(study_arm %in% c(1, 2, 3, 6)) %>% pull(KEY)))
	# )  %>%
	bind_rows(
		data_today_hh_v111
	)  %>%
	bind_rows(
		data_today_hh_v113
	) %>%
	bind_rows(
		data_today_hh_v114
	)  %>%
	bind_rows(
		data_today_hh_v115
	) %>%
	bind_rows(
		data_today_hh_v116
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
	# bind_rows(
	# 	data_today_symptoms_v95 %>%
	# 		filter(PARENT_KEY %in% c(data_today_v95 %>% filter(study_arm %in% c(1, 2, 3, 6)) %>% pull(KEY)))
	# )  %>%
	bind_rows(
		data_today_symptoms_v111
	)  %>%
	bind_rows(
		data_today_symptoms_v113
	) %>%
	bind_rows(
		data_today_symptoms_v114
	)  %>%
	bind_rows(
		data_today_symptoms_v115
	) %>%
	bind_rows(
		data_today_symptoms_v116
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
	) # %>%
# bind_rows(
# 	data_today_location_v95 %>%
# 		filter(PARENT_KEY %in% c(data_today_v95 %>% filter(study_arm %in% c(1, 2, 3, 6)) %>% pull(KEY)))
# )  

# data_hh_member_character <-
# 	data_hh_base %>% # 4659 rows
# 	left_join(
# 		data_wide_character %>% select(KEY, fcn_id), by = c("PARENT_KEY" = "KEY")
# 	) %>%
# 	left_join(
# 		data_symptoms_base, by = c("PARENT_KEY", "name" = "name_roster") # 4659 rows
# 	) %>%
# 	left_join(
# 		data_location_base, by = c("PARENT_KEY", "name" = "location_name_roster") # 4659 rows
# 	) %>%
# 	select(PARENT_KEY, fcn_id, name, everything())
# # Total 4677 rows -> there must have been some name misspellings --> 
# # I don't know how this can be since the name had to be selected from the roster.  I'll figure this out later




########### HOST #####################

survey_data_host_baseline <-
	survey_data_host_baseline %>%
	mutate(timepoint = "baseline") %>%
	select(timepoint, hh_id, hh_id_short, everything())


survey_data_host_midline <-
	# not including v94 eliminates the 8 host hh that were accidentally administered the v94 version of the survey (repsondent names [there was no other identifier] [case-specific]: Laila, Monoara begum, anjuma begum, Farfiza khatun, Juhura, Halema khatun, Mahamoda begum, Jahida Khatun)
	data_today_v95 %>% 
	filter(study_arm %in% c(4, 5)) %>% # filtering v95 bu study_arm in 4 and 5 eliminates the two Rohingya hh that were accidentally administered the v95 version of the survey (these Rohingya hh had study_arm = 3)
	mutate(timepoint = "midline")

survey_data_host_endline <-
	data_today_host_v119 %>%
	mutate(timepoint = "endline")


# data_host_wide_character <-
# 	survey_data_host_baseline %>%
# 	full_join(survey_data_host_midline,  by = c("hh_id"), suffix = c("", ".midline")) %>% # this doesn't work because midline has no hh_id; instead it has hh_id_short, which is only part of the hh_id
# 	full_join(survey_data_host_endline, by = c("hh_id"), suffix = c("", ".endline")) %>%
# 	# select(-X1) %>%
# 	select(timepoint, hh_id, hh_id_short, everything()) 


data_host_long_character <-
	bind_rows(
		survey_data_host_baseline %>% mutate(hh_id_short = as.character(hh_id_short)),
		survey_data_host_midline %>% mutate(hh_id_short = as.character(hh_id_short)),
		survey_data_host_endline %>% mutate(hh_id_short = as.character(hh_id_short))
	) %>%
	# select(-X1) %>%
	select(timepoint, hh_id, hh_id_short, everything())


write.csv(data_host_long_character, here::here("2_data_raw/RohingyaFuel_survey_data_host_long_character_fix_hh_ids.csv"))


data_hh_host_base <-
	bind_rows(
		# data_today_hh_v94 %>%
		# 	filter(PARENT_KEY %in% c(data_today_v94 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY))),
		data_today_hh_v95 %>%
			filter(PARENT_KEY %in% c(data_today_v95 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY))), 
		data_today_host_v119
	)

data_symptoms_host_base <-
	bind_rows(
		# data_today_symptoms_v94 %>%
		# 	filter(PARENT_KEY %in% c(data_today_v94 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY))),
		data_today_symptoms_v95 %>%
			filter(PARENT_KEY %in% c(data_today_v95 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY))), 
		data_today_host_symptoms_v119
	) 

data_location_host_base <-
	bind_rows(
		# data_today_location_v94 %>%
		# 	filter(PARENT_KEY %in% c(data_today_v94 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY))),
		data_today_location_v95 %>%
			filter(PARENT_KEY %in% c(data_today_v95 %>% filter(study_arm %in% c(4, 5)) %>% pull(KEY)))
	) 

# data_hh_member_host_character <-
# 	data_hh_host_base %>% # 2169 rows
# 	left_join(
# 		data_host_wide_character %>% select(KEY, hh_id_host), by = c("PARENT_KEY" = "KEY") # 1078 rows
# 	) %>%
# 	left_join(
# 		data_symptoms_host_base, by = c("PARENT_KEY", "name" = "name_roster") # 1078 rows
# 	) %>%
# 	left_join(
# 		data_location_host_base, by = c("PARENT_KEY", "name" = "location_name_roster") # 1078 rows
# 	) %>%
# 	select(PARENT_KEY, hh_id_host, name, everything())










#########################################################################################################################
#################  Change from cols of type character back to cols of guessed type ##############################
#########################################################################################################################

########### Rohingya #####################

# data_wide <-
# 	data_wide_character %>%
# 	type_convert(
# 		col_types = NULL,
# 		na = c("", "NA"),
# 		trim_ws = TRUE,
# 		locale = default_locale()
# 	) %>%
# 	# mutate(
# 	# 	SubmissionDate = parse_date_time(SubmissionDate, orders = "mdy IMS", tz = "Asia/Dhaka"),
# 	# 	starttime = parse_date_time(starttime, orders = "mdy IMS", tz = "Asia/Dhaka"),
# 	# 	endtime = parse_date_time(endtime, orders = "mdy IMS", tz = "Asia/Dhaka"),
# 	# 	start_date = mdy(start_date) # Not working for most values, not sure why
# 	# ) %>%
# 	mutate(fcn_id = as.character(fcn_id)) %>%
# 	mutate_at(
# 		vars(
# 			timepoint, block_id, # fcn_id, camp_id
# 			corn_source, bread_source, beef_source, veggies_source, lentils_source, eggs_source, poultry_source, 
# 			oil_source, fish_source, rice_source, potatoes_source, fruit_source, goat_sheep_source, sugar_source, dairy_source, 
# 			contact_number, first_enrolled_lpg, first_receive_lpg, date_safety_training, lpg_changes_lifestyle,
# 			# lpg_cylinder_repair, lpg_stove_repair,
# 			# fuel_cant_afford_action, food_cant_afford_action
# 			paste0(c("corn_source", "bread_source", "beef_source", "veggies_source", "lentils_source", "eggs_source", "poultry_source", 
# 							 "oil_source", "fish_source", "rice_source", "potatoes_source", "fruit_source", "goat_sheep_source", "sugar_source", "dairy_source", 
# 							 "first_enrolled_lpg", "first_receive_lpg", "lpg_changes_lifestyle"), ".midline"),
# 			paste0(c("corn_source", "bread_source", "beef_source", "veggies_source", "lentils_source", "eggs_source", "poultry_source", 
# 							 "oil_source", "fish_source", "rice_source", "potatoes_source", "fruit_source", "goat_sheep_source", "sugar_source", "dairy_source", 
# 							 "lpg_changes_lifestyle"), ".endline") # "first_enrolled_lpg", "first_receive_lpg",
# 		), 
# 		list(as.factor)
# 	) %>%
# 	mutate_at(
# 		vars(
# 			income_cash_ngo, income_abroad, income_farming, income_own_business, 
# 			income_wage_labor, income_skill_labor, income_handicrafts_tailoring, 
# 			income_humanitarian_asst, income_selling_wood,     
# 			life_failure, fearful, restless_sleep, less_talkative, lonely, 
# 			unfriendly_people, crying_spells, sick, feeling_disliked, 
# 			cant_get_going, suicidal_thoughts_30,
# 			paste0(
# 				c("income_cash_ngo", "income_abroad", "income_farming", "income_own_business", 
# 					"income_wage_labor", "income_skill_labor", "income_handicrafts_tailoring", 
# 					"income_humanitarian_asst", "income_selling_wood",     
# 					"life_failure", "fearful", "restless_sleep", "less_talkative", "lonely", 
# 					"unfriendly_people", "crying_spells", "sick", "feeling_disliked", 
# 					"cant_get_going", "suicidal_thoughts_30"), 
# 				".midline"
# 			),
# 			paste0(
# 				c("income_cash_ngo", "income_abroad", "income_farming", "income_own_business", 
# 					"income_wage_labor", "income_skill_labor", "income_handicrafts_tailoring", 
# 					"income_humanitarian_asst", "income_selling_wood",     
# 					"life_failure", "fearful", "restless_sleep", "less_talkative", "lonely", 
# 					"unfriendly_people", "crying_spells", "sick", "feeling_disliked", 
# 					"cant_get_going", "suicidal_thoughts_30"), 
# 				".endline"
# 			)
# 		), 
# 		funs(as.numeric)
# 	)

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
# data_host_wide <-
# 	data_host_wide_character %>%
# 	type_convert(
# 		col_types = NULL,
# 		na = c("", "NA"),
# 		trim_ws = TRUE,
# 		locale = default_locale()
# 	) %>%
# 	# mutate(
# 	# 	SubmissionDate = parse_date_time(SubmissionDate, orders = "mdy IMS", tz = "Asia/Dhaka"),
# 	# 	starttime = parse_date_time(starttime, orders = "mdy IMS", tz = "Asia/Dhaka"),
# 	# 	endtime = parse_date_time(endtime, orders = "mdy IMS", tz = "Asia/Dhaka"),
# 	# 	start_date = mdy(start_date) # Not working for most values, not sure why
# 	# ) %>%
# 	mutate(hh_id_short = as.character(hh_id_short)) %>%
# 	mutate_at(
# 		vars(
# 			block_id, # fcn_id, camp_id
# 			corn_source, bread_source, beef_source, veggies_source, lentils_source, eggs_source, poultry_source, 
# 			oil_source, fish_source, rice_source, potatoes_source, fruit_source, goat_sheep_source, sugar_source, dairy_source, 
# 			contact_number, first_enrolled_lpg, first_receive_lpg, date_safety_training, lpg_changes_lifestyle,
# 			# lpg_cylinder_repair, lpg_stove_repair,
# 			# fuel_cant_afford_action, food_cant_afford_action
# 			paste0(c("corn_source", "bread_source", "beef_source", "veggies_source", "lentils_source", "eggs_source", "poultry_source", 
# 							 "oil_source", "fish_source", "rice_source", "potatoes_source", "fruit_source", "goat_sheep_source", "sugar_source", "dairy_source", 
# 							 "lpg_changes_lifestyle"), ".endline") # "first_enrolled_lpg", "first_receive_lpg",
# 		), 
# 		list(as.factor)
# 	) %>%
# 	mutate_at(
# 		vars(
# 			income_cash_ngo, income_abroad, income_farming, income_own_business, 
# 			income_wage_labor, income_skill_labor, income_handicrafts_tailoring, 
# 			income_humanitarian_asst, income_selling_wood,     
# 			life_failure, fearful, restless_sleep, less_talkative, lonely, 
# 			unfriendly_people, crying_spells, sick, feeling_disliked, 
# 			cant_get_going, suicidal_thoughts_30,
# 			paste0(
# 				c("income_cash_ngo", "income_abroad", "income_farming", "income_own_business", 
# 					"income_wage_labor", "income_skill_labor", "income_handicrafts_tailoring", 
# 					"income_humanitarian_asst", "income_selling_wood",     
# 					"life_failure", "fearful", "restless_sleep", "less_talkative", "lonely", 
# 					"unfriendly_people", "crying_spells", "sick", "feeling_disliked", 
# 					"cant_get_going", "suicidal_thoughts_30"), 
# 				".endline"
# 			)
# 		), 
# 		funs(as.numeric)
# 	)

data_host_long <-
	data_host_long_character %>%
	type_convert(
		col_types = NULL,
		na = c("", "NA"),
		trim_ws = TRUE,
		locale = default_locale()
	) %>%
	mutate(hh_id_short = as.character(hh_id_short)) %>%
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

#data_long %>% count(study_arm)


################# Save data ########################

# write_rds(data_wide, here::here("2_data_raw/RohingyaFuel_survey_data_wide.rds"))
write_rds(data_long, here::here("2_data_raw/RohingyaFuel_survey_data_long.rds"))

# write_rds(data_host_wide, here::here("2_data_raw/RohingyaFuel_survey_data_wide_host.rds"))
write_rds(data_host_long, here::here("2_data_raw/RohingyaFuel_survey_data_long_host.rds"))

# write_rds(data_hh_member, here::here("2_data_raw/RohingyaFuel_data_hh_member.rds"))
# write_rds(data_hh_member_host, here::here("2_data_raw/RohingyaFuel_data_hh_member_host.rds"))


xtabs(timepoint, data_long)
