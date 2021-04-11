################################################################################
# @Project: Rohingya LPG Evaluation
# @Author: Laura H Kwong
# @Description: Pair Rohingya hh survey data
# @Version: 3.6.1
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))

# ============================================================================
# Files in
file_in_1 <- here::here("4_data/RohingyaFuel_survey_data_clean.rds")
file_in_2 <- here::here("4_data/RohingyaFuel_data_hh_member.rds")

file_in_3 <- here::here("4_data/RohingyaFuel_survey_data_clean_host.rds")
file_in_4 <- here::here("4_data/RohingyaFuel_data_hh_member_host.rds")


# Files out
file_out_1 <- here::here("4_data/RohingyaFuel_survey_data_paired.rds")
file_out_2 <- here::here("4_data/RohingyaFuel_data_hh_member_paired.rds") 

file_out_3 <- here::here("4_data/RohingyaFuel_survey_data_long_paired_host.rds")
file_out_4 <- here::here("4_data/RohingyaFuel_data_hh_member_paired_host.rds")



file_out_survey_data_intervention_baseline <- here::here("4_data/RohingyaFuel_survey_data_intervention_baseline.rds")
file_out_survey_data_intervention_endline <- here::here("4_data/RohingyaFuel_survey_data_intervention_endline.rds")
file_out_survey_data_comparison_baseline <- here::here("4_data/RohingyaFuel_comparison_baseline.rds")
file_out_survey_data_comparison_endline <- here::here("4_data/RohingyaFuel_comparison_endline.rds")
file_out_survey_data_intervention <- here::here("4_data/RohingyaFuel_intervention.rds")
file_out_survey_data_comparison <- here::here("4_data/RohingyaFuel_comparison.rds")
file_out_survey_data_wide_paired <- here::here("4_data/RohingyaFuel_survey_data_wide_paired.rds")

file_out_hh_data <- here::here("4_data/RohingyaFuelMaster_hh_data.xlsx")

file_out_hh_data_host <- here::here("4_data/RohingyaFuel_hh_data_host.xlsx")

# ============================================================================

# This is a difference-in-difference study so only analyze hh that were surveyed at baseline and endline

######  REFUGEE paired before-after datasets ########
survey_data_clean <- read_rds(file_in_1)
data_hh_member <- read_rds(file_in_2)

# These hh have errors (fcn_id should = 2 for baseline and endline but is >2)
fcn_id_more_than_two <-
	survey_data_clean %>%
	count(fcn_id) %>%
	filter(n > 2) %>%
	pull(fcn_id)

# These hh have baseline or endline surveys, but not both. This is disappointing!! 
fcn_id_only_one <-
	survey_data_clean %>%
	count(fcn_id) %>%
	filter(n < 2) %>% # 67 hh
	pull(fcn_id)

# include paired hh only
fcn_id_paired <-
	survey_data_clean %>%
	count(fcn_id) %>%
	filter(n == 2) %>%
	pull(fcn_id)

survey_data_long_paired <-
	survey_data_clean %>%
	filter(fcn_id %in% fcn_id_paired) 

data_hh_member_paired <-
	data_hh_member %>%
	filter(fcn_id %in% c(fcn_id_paired))


# Check number of hh in each arm after pairing

# survey_data_clean %>%
# 	count(study_arm)

survey_data_long_paired %>%
	count(study_arm) 

# survey_data_long_paired_only %>%
# 	select(fcn_id, study_arm) %>%
# 	arrange(fcn_id) %>%
# 	mutate(
# 		pre_int = ifelse(study_arm == "pre-intervention", 1, NA),
# 		post_int = ifelse(study_arm == "post-intervention", 1, NA),
# 		int = ifelse(study_arm == "intervention", 1, NA),
# 		int_fu = ifelse(study_arm == "intervention follow-up", 1, NA)
# 	) %>% View()


######  HOST paired before-after datasets ########


# # # The host dataset is a mess - I don't even know what to  use for the unique identifier
# # house_id_no_pair_host <-
# # 	survey_data_long_host %>%
# # 	count(nat_id) %>%
# # 	filter(n != 2) %>% # There are 157 hh that have baseline or endline surveys, but not both. This is disappointing!! 
# # 	pull(fcn_id)
# 
# survey_data_clean <- read_rds(file_in_1)
# data_hh_member <- read_rds(file_in_2)
# 
# # These hh have errors (fcn_id should = 2 for baseline and endline but is >2)
# fcn_id_more_than_two <-
# 	survey_data_clean %>%
# 	count(fcn_id) %>%
# 	filter(n > 2) %>%
# 	pull(fcn_id)
# 
# # These hh have baseline or endline surveys, but not both. This is disappointing!! 
# fcn_id_only_one <-
# 	survey_data_clean %>%
# 	count(fcn_id) %>%
# 	filter(n < 2) %>% # 67 hh
# 	pull(fcn_id)
# 
# # include paired hh only
# fcn_id_paired <-
# 	survey_data_clean %>%
# 	count(fcn_id) %>%
# 	filter(n == 2) %>% # 67 hh
# 	pull(fcn_id)
# 
# 
# survey_data_long_host_paired_only %>%
# 	select(fcn_id, study_arm) %>%
# 	arrange(fcn_id) %>%
# 	mutate(
# 		pre_int = ifelse(study_arm == "pre-intervention", 1, NA),
# 		post_int = ifelse(study_arm == "post-intervention", 1, NA),
# 		int = ifelse(study_arm == "intervention", 1, NA),
# 		int_fu = ifelse(study_arm == "intervention follow-up", 1, NA)
# 	) %>% View()
# 
# 
# 
# survey_data_long_paired_only <-
# 	survey_data_clean %>%
# 	filter(fcn_id %in% fcn_id_paired) 
# 
# data_hh_member_paired_only <-
# 	data_hh_member %>%
# 	filter(fcn_id %in% c(pre_post_fcn_id, intervention_follow_up_fcn_id))
# 
# 
# # Check number of hh in each arm after pairing
# 
# survey_data_long_paired_only %>%
# 	count(study_arm)






# ============================================================================
# Make a wide dataset
# ============================================================================

survey_data_intervention_baseline <-
	survey_data_long_paired %>%
	filter(study_arm == "pre-intervention")

survey_data_intervention_endline <-
	survey_data_long_paired %>%
	filter(study_arm == "post-intervention")

survey_data_comparison_baseline <-
	survey_data_long_paired %>%
	filter(study_arm == "intervention")

survey_data_comparison_endline <-
	survey_data_long_paired %>%
	filter(study_arm == "intervention follow-up")

survey_data_intervention <-
	survey_data_intervention_baseline %>%
	left_join(survey_data_intervention_endline, by = c("fcn_id"), suffix = c(".baseline", ".endline"))

survey_data_comparison <-
	survey_data_comparison_baseline %>%
	left_join(survey_data_comparison_endline, by = c("fcn_id"), suffix = c(".baseline", ".endline"))

survey_data_wide <-
	bind_rows(survey_data_intervention, survey_data_comparison) %>%
	mutate(
		study_arm =
			case_when(
				study_arm.baseline == "pre-intervention" ~ "intervention",
				study_arm.baseline == "intervention" ~ "comparison"
			),
		study_arm = 
			ordered(
				study_arm,
				levels = c("intervention", "comparison")
			)
	) 

# ============================================================================
# Get unique hh identifier 
# ============================================================================

hh_data <-
	survey_data_long_paired %>%
	select(study_arm, camp_id, block_id, subblock_id, fcn_id, Rand_nu, hh_id, name_mahji,  contact_number, name_hh_head, name_respondent, target_child_name) %>%
	distinct() %>%
	arrange(study_arm, camp_id, block_id, subblock_id, Rand_nu, fcn_id)

# hh_data_host <-
# 	survey_data_host_long %>%
# 	select(study_arm, contact_number, name_hh_head, name_respondent, target_child_name) %>%
# 	distinct() %>%
# 	arrange(hh_id, study_arm)



# ============================================================================
# Save datasets 
# ============================================================================

write_rds(survey_data_long_paired, file_out_1)
write_rds(data_hh_member_paired, file_out_2)



write_rds(survey_data_intervention_baseline, file_out_survey_data_intervention_baseline)
write_rds(survey_data_intervention_endline, file_out_survey_data_intervention_endline)
write_rds(survey_data_comparison_baseline, file_out_survey_data_comparison_baseline)
write_rds(survey_data_comparison_endline, file_out_survey_data_comparison_endline)
write_rds(survey_data_intervention, file_out_survey_data_intervention)
write_rds(survey_data_comparison, file_out_survey_data_comparison)
write_rds(survey_data_wide_paired, file_out_wide_paired)

write_rds(hh_data, file_out_hh_data)

# write_rds(survey_data_long_paired_host, file_out_3)
# write_rds(data_hh_member_paired_host, file_out_4)



