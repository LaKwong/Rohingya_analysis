################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Datasets to share
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
source(here::here("1_config.R"))


# Parameters

# data_hh_member_filename <- here::here("4_data/RohingyaFuel_data_hh_member.rds")
# 
# data_hh_member_host_filename <- here::here("4_data/RohingyaFuel_data_hh_member_host.rds")

file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_paired.rds")
#===============================================================================


# Load input files
# data_hh_member_base <- read_rds(data_hh_member_filename)
# 
# data_hh_member_host_base <- read_rds(data_hh_member_host_filename)

survey_data <- read_rds(file_survey_data_base)






# Datasets to share

survey_data_fcn_id_codes <-
	survey_data %>%
	select(fcn_id) %>%
	arrange(fcn_id) %>%
	unique() %>%
	mutate(fcn_id_coded = seq(1, nrow(.))) %>%
	select(fcn_id, fcn_id_coded)

survey_data_fcn_id_two_obs <-
	survey_data %>%
	count(fcn_id) %>%
	filter(n == 2)

survey_data_fcn_id_codes_two_obs <-
	survey_data_fcn_id_codes %>%
	filter(fcn_id %in% survey_data_fcn_id_two_obs$fcn_id)

write_rds(survey_data_fcn_id_codes_two_obs, here::here("4_data/survey_data_fcn_id_codes_two_obs.rds"))
write_csv(survey_data_fcn_id_codes_two_obs, here::here("4_data/survey_data_fcn_id_codes_two_obs.csv") )

hh_id_for_field <-
	survey_data %>%
	select(camp_id, block_id, subblock_id, name_mahji, fcn_id, hh_id, hh_id_original, hh_id_host, name_respondent, name_hh_head, name_mahji, target_child_name, target_child_dob_1)

write_rds(hh_id_for_field, here::here("4_data/hh_id_for_field.rds"))
write_csv(hh_id_for_field, here::here("4_data/hh_id_for_field.csv") )


survey_data_coded_two_obs <-
	survey_data %>%
	left_join(survey_data_fcn_id_codes_two_obs, by = c("fcn_id")) %>%
	select(-c(camp_id, block_id, subblock_id, fcn_id, hh_id, hh_id_original, hh_id_host, name_respondent, name_hh_head, name_mahji, target_child_name))

write_rds(survey_data_coded_two_obs, here::here("4_data/survey_data_coded_two_obs.rds"))
write_csv(survey_data_coded_two_obs, here::here("4_data/survey_data_coded_two_obs.csv") )




#################################################################################


survey_data_coded_two_obs_fuel_violence_mental_health <-
	survey_data_coded_two_obs %>%
	select(
		fcn_id_coded, timepoint, study_arm_overall, study_arm, 
		all_of(mental_health_vars),
		starts_with("time"), all_of(harassment_vars), 
		starts_with("food_"), starts_with("spent_"),
		borrow_food, reduce_food, 
		reduce_meals_lack_food, not_eat_lack_food, restrict_food,
		borrow_fuel, reduce_fuel, 
		reduce_meals_lack_fuel, not_eat_lack_fuel
	)

write_rds(survey_data_coded_two_obs_fuel_violence_mental_health, here::here("data/survey_data_coded_two_obs_fuel_violence_mental_health.rds"))
write_csv(survey_data_coded_two_obs_fuel_violence_mental_health, here::here("data/survey_data_coded_two_obs_fuel_violence_mental_health.csv"))


################################################################################

## Data shared with Fouzia and Naima

survey_data_coded_two_obs_fuel_violence <-
	survey_data_coded_two_obs %>%
	select(
		fcn_id_coded, timepoint, study_arm_overall, study_arm, 
		starts_with("time"), all_of(harassment_base_names), 
		starts_with("food_"), starts_with("spent_"),
		borrow_food, reduce_food, 
		reduce_meals_lack_food, not_eat_lack_food, restrict_food,
		borrow_fuel, reduce_fuel, 
		reduce_meals_lack_fuel, not_eat_lack_fuel
	)

write_rds(survey_data_coded_two_obs_fuel_violence, here::here("data/survey_data_coded_two_obs_fuel_violence.rds"))

