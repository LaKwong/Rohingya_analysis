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
file_in_1 <- here::here("4_data/RohingyaFuelMaster_survey_data_tidy_host.rds")=
file_in_2 <- here::here("4_data/RohingyaFuel_data_hh_member_host.rds")

# Files out
file_out_1 <- here::here("4_data/RohingyaFuel_survey_data_long_paired_host.rds")
file_out_2 <- here::here("4_data/RohingyaFuel_data_hh_member_paired_host.rds")



file_out_survey_data_intervention_baseline <- here::here("4_data/RohingyaFuel_survey_data_intervention_baseline_host.rds")
file_out_survey_data_intervention_endline <- here::here("4_data/RohingyaFuel_survey_data_intervention_endline_host.rds")
file_out_survey_data_comparison_baseline <- here::here("4_data/RohingyaFuel_comparison_baseline_host.rds")
file_out_survey_data_comparison_endline <- here::here("4_data/RohingyaFuel_comparison_endline_host.rds")
file_out_survey_data_intervention <- here::here("4_data/RohingyaFuel_intervention_host.rds")
file_out_survey_data_comparison <- here::here("4_data/RohingyaFuel_comparison_host.rds")
file_out_survey_data_wide_paired <- here::here("4_data/RohingyaFuel_survey_data_wide_paired_host.rds")

file_out_hh_data <- here::here("4_data/RohingyaFuel_hh_data_host.xlsx")

# ============================================================================


# Note that the code below does not specify host in the data frame name, but that is what is being analyzed 

# This is a difference-in-difference study so only analyze hh that were surveyed at baseline and endline

######  HOST paired before-after datasets ########
survey_data_clean <- read_rds(file_in_1)

#write_csv(survey_data_clean, "4_data/survey_data_host_unpaired.csv")

data_hh_member <- read_rds(file_in_2)


unique_id_more_than_three <-
	survey_data_clean %>%
	count(hh_id_short) %>%
	filter(n > 3) %>%
	pull(hh_id_short)
#none meet this definition

# These hh have baseline or endline surveys, but not both. This is disappointing!! 
unique_id_only_one <-
	survey_data_clean %>%
	count(hh_id_short) %>%
	filter(n < 2) %>% # 12 hh --> I think there should only be 9 that are missing, but as of 12 April 2021, this will have to do!
	pull(hh_id_short)
#  "0"    "1029" "1132" "182"  "3164" "3176" "3233" "3250" "3419" "3436" "3450" "3453" "3463" "3497" "3509" "3513" "3518" "3610" "3621"


# include paired hh only
unique_id_paired <-
	survey_data_clean %>%
	count(hh_id_short) %>%
	filter(n == 3) %>%
	pull(hh_id_short)


survey_data_long_paired <-
	survey_data_clean %>%
	filter(hh_id_short %in% unique_id_paired) 

####################


donor <- 
	survey_data_long_paired %>%
	filter(timepoint == "midline") %>% 
	select(unique_id, first_receive_lpg, first_enrolled_lpg)


data_begin_mid <- 
	survey_data_long_paired %>% filter(timepoint == "baseline" | timepoint == "midline")

data <- survey_data_long_paired %>% filter(timepoint == "endline")

data$unique_id
donor$unique_id

r = merge(data, donor, by="unique_id", suffixes=c(".data", ".donor"))
na.idx = which(is.na(data$first_receive_lpg))
data[na.idx,"first_receive_lpg"] = r[na.idx,"first_receive_lpg.donor"]


na.idx2 = which(is.na(data$first_enrolled_lpg))
data[na.idx2,"first_enrolled_lpg"] = r[na.idx2,"first_enrolled_lpg.donor"]

data %>% count(timepoint, first_receive_lpg)

survey_data_long_paired <- 
	data %>% 
	bind_rows(data_begin_mid)

survey_data_long_paired

#######################################

survey_data_long_paired %>%
	mutate(datetime = lubridate::parse_date_time(SubmissionDate, "%m/%d/%y %H:%M"), 
				 date = as_date(datetime)) %>% 
	count(year(date), timepoint)

survey_data_long_paired %>% group_by(hh_id_short) %>% count(timepoint) %>% arrange(desc(n))

# survey_data_long_paired_only %>%
# 	select(unique_id, study_arm) %>%
# 	arrange(unique_id) %>%
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
# # 	pull(unique_id)
# 
# survey_data_clean <- read_rds(file_in_1)
# data_hh_member <- read_rds(file_in_2)
# 
# # These hh have errors (unique_id should = 2 for baseline and endline but is >2)
# unique_id_more_than_two <-
# 	survey_data_clean %>%
# 	count(unique_id) %>%
# 	filter(n > 2) %>%
# 	pull(unique_id)
# 
# # These hh have baseline or endline surveys, but not both. This is disappointing!! 
# unique_id_only_one <-
# 	survey_data_clean %>%
# 	count(unique_id) %>%
# 	filter(n < 2) %>% # 67 hh
# 	pull(unique_id)
# 
# # include paired hh only
# unique_id_paired <-
# 	survey_data_clean %>%
# 	count(unique_id) %>%
# 	filter(n == 2) %>% # 67 hh
# 	pull(unique_id)
# 
# 
# survey_data_long_host_paired_only %>%
# 	select(unique_id, study_arm) %>%
# 	arrange(unique_id) %>%
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
# 	filter(unique_id %in% unique_id_paired) 
# 
# data_hh_member_paired_only <-
# 	data_hh_member %>%
# 	filter(unique_id %in% c(pre_post_unique_id, intervention_follow_up_unique_id))
# 
# 
# # Check number of hh in each arm after pairing
# 
# survey_data_long_paired_only %>%
# 	count(study_arm)






# ============================================================================
# Make a wide dataset
# ============================================================================

survey_data_long_paired %>% 
	

# ============================================================================
# Save datasets 
# ============================================================================

write_rds(survey_data_long_paired, file_out_1)


#write_rds(hh_data, file_out_hh_data)



