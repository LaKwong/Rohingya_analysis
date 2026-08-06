################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Mental health analysis
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))


# Parameters

hh_data_listing <- here::here("4_data/RohingyaFuelMaster_hh_data.xlsx")


# Load input files

data <- read_xlsx(hh_data_listing)

data_intervention_baseline <- 
	data %>%
	filter(study_arm == "pre-intervention") %>%
	mutate(study_arm = "intervention_baseline") %>%
	# The baseline includes the hh_id with "_{Rand_nu}" at the end
	# For some reason there is no Rand_nu in the midline survey
	mutate(hh_id_Rand_nu = hh_id) %>%
	mutate(hh_id = str_to_upper(str_remove(hh_id_Rand_nu, "_.*$"))) %>%
	mutate(hh_id_baseline = hh_id) %>%
	mutate(survey_baseline = "yes")

data_intervention_midline <- 
	data %>%
	filter(study_arm == "post-intervention") %>%
	mutate(study_arm = "intervention_midline") %>%
	mutate(hh_id = str_to_upper(hh_id)) %>%
	mutate(hh_id_midline = hh_id) %>%
	mutate(survey_midline = "yes")

data_comparison_baseline <- 
	data %>%
	filter(study_arm == "intervention") %>%
	mutate(study_arm = "comparison_baseline") %>%
	# The baseline includes the hh_id with "_{Rand_nu}" at the end
	# For some reason there is no Rand_nu in the midline survey
	mutate(hh_id_Rand_nu = hh_id) %>%
	mutate(hh_id = str_to_upper(str_remove(hh_id_Rand_nu, "_.*$"))) %>%
	mutate(hh_id_baseline = hh_id) %>%
	mutate(survey_baseline = "yes")

data_comparison_midline <- 
	data %>%
	filter(study_arm == "intervention follow-up") %>%
	mutate(study_arm = "comparison_midline") %>%
	mutate(hh_id = str_to_upper(hh_id)) %>%
	mutate(hh_id_midline = hh_id) %>%
	mutate(survey_midline = "yes")

data_baseline <-
	bind_rows(data_intervention_baseline, data_comparison_baseline)

View(data_baseline)

data_intervention <-
	full_join(data_intervention_baseline %>% select(-study_arm), data_intervention_midline %>% select(hh_id, hh_id_midline, survey_midline), by = "hh_id") %>%
	select(hh_id, hh_id_baseline, hh_id_midline, survey_baseline, survey_midline, everything())

View(data_intervention)

data_comparison <-
	full_join(data_comparison_baseline, data_comparison_midline %>% select(hh_id, survey_midline), by = "hh_id")


file_survey_data_base_wide <- here::here("4_data/RohingyaFuel_survey_data_wide_paired.rds")
survey_data_wide <- read_rds(file_survey_data_base_wide)


file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_paired.rds")
survey_data <- read_rds(file_survey_data_base)

hh_listing_data_baseline_midline <-
	survey_data_wide %>%
	select(
		fcn_id, study_arm, # study_arm.baseline, study_arm.endline, 
		camp_id.baseline, subblock_id.baseline, 
		name_mahji.baseline, name_respondent.baseline, name_hh_head.baseline,
		contact_number.baseline #, contact_number.endline
	)

View(hh_listing_data_baseline_midline)



write.csv(hh_listing_data_baseline_midline, "4_data/data_hh_list_baseline_midline.csv")
