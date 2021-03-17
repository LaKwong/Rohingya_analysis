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
# Parameters
file_in_1 <- here::here("4_data/RohingyaFuel_survey_data_clean.rds") 
file_in_2 <- here::here("4_data/RohingyaFuel_data_hh_member.rds")

file_in_3 <- here::here("4_data/RohingyaFuel_survey_data_clean_host.rds") 
file_in_4 <- here::here("4_data/RohingyaFuel_data_hh_member_host.rds") 





file_out_1 <- here::here("4_data/RohingyaFuel_survey_data_paired.rds")
file_out_2 <- here::here("4_data/RohingyaFuel_data_hh_member_paired.rds") 

file_out_3 <- here::here("4_data/RohingyaFuel_survey_data_long_paired_host.rds")
file_out_4 <- here::here("4_data/RohingyaFuel_data_hh_member_paired_host.rds")

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

survey_data_long_paired_only <-
	survey_data_clean %>%
	filter(fcn_id %in% fcn_id_paired) 

data_hh_member_paired_only <-
	data_hh_member %>%
	filter(fcn_id %in% c(fcn_id_paired))


# Check number of hh in each arm after pairing

# survey_data_clean %>%
# 	count(study_arm)

survey_data_long_paired_only %>%
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








###### Save datasets ###########

write_rds(survey_data_long_paired_only, file_out_1)
write_rds(data_hh_member_paired_only, file_out_2)

# write_rds(survey_data_long_paired_only_host, file_out_3)
# write_rds(data_hh_member_paired_only_host, file_out_4)



