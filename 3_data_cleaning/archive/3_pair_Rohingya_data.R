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
file_in_1 <- here::here("4_data/RohingyaFuelMaster_survey_data_tidy.rds")


# Files out
file_out_1 <- here::here("4_data/RohingyaFuel_survey_one_survey.rds")
file_out_2 <- here::here("4_data/RohingyaFuel_survey_two_surveys.rds")
file_out_3 <- here::here("4_data/RohingyaFuel_survey_three_surveys.rds")
file_out_4 <- here::here("4_data/RohingyaFuel_survey_four_surveys.rds")

file_out_1_csv <- here::here("4_data/RohingyaFuel_survey_one_survey.csv")
file_out_2_csv <- here::here("4_data/RohingyaFuel_survey_two_surveys.csv")
file_out_3_csv <- here::here("4_data/RohingyaFuel_survey_three_surveys.csv")
file_out_4_csv <- here::here("4_data/RohingyaFuel_survey_four_surveys.csv")

file_out_5 <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")
file_out_5_deidentified <- here::here("4_data/RohingyaFuel_survey_data_triple_deidentified.rds")

file_out_hh_data <- here::here("4_data/RohingyaFuelMaster_hh_data.xlsx")

# file_out_survey_data_intervention_baseline <- here::here("4_data/RohingyaFuel_survey_data_intervention_baseline.rds")
# file_out_survey_data_intervention_endline <- here::here("4_data/RohingyaFuel_survey_data_intervention_endline.rds")
# file_out_survey_data_comparison_baseline <- here::here("4_data/RohingyaFuel_comparison_baseline.rds")
# file_out_survey_data_comparison_endline <- here::here("4_data/RohingyaFuel_comparison_endline.rds")
# file_out_survey_data_intervention <- here::here("4_data/RohingyaFuel_intervention.rds")
# file_out_survey_data_comparison <- here::here("4_data/RohingyaFuel_comparison.rds")
# file_out_survey_data_wide_paired <- here::here("4_data/RohingyaFuel_survey_data_wide_paired.rds")


# ============================================================================

# This is a difference-in-difference study so only analyze hh that were surveyed at baseline and endline

######  REFUGEE paired before-after datasets ########
survey_data_clean <- read_rds(file_in_1)



survey_data_clean %>%
	count(fcn_id) %>% 
	count(n)
# 220721 prior to 10 am
# 		# surveys   count
# 			<int> <int>
# 1     1    83
# 2     2   353
# 3     3   778
# 4     4     5

# 220721 at 11 am
# 		# surveys   count
# 			<int> <int>
# 1     1    70
# 2     2   340 # there are still 340 surveys that are missing endline....
# 3     3   791
# 4     4     5

# These fcn_id only have one survey
fcn_id_one_survey <- 
	survey_data_clean %>%
	filter(
		fcn_id %in% c(
			survey_data_clean %>%
				count(fcn_id) %>%
				filter(n == 1) %>% pull(fcn_id)
		)
	) %>% 
	select(camp_id, subblock_id, name_mahji, start_date, study_arm, timepoint, fcn_id, name_respondent, name_hh_head) %>%
	arrange(fcn_id, timepoint)

# # if we look at the camp and subblock id, can we find an fcn_id that is close? 
# survey_data_clean %>%
# 	filter(
# 		camp_id %in% c("8W"),
# 		subblock_id %in% c("I19")
# 	) %>%
# 	select(camp_id, subblock_id, name_mahji, start_date, study_arm, timepoint, fcn_id, name_respondent, name_hh_head) %>%
# 	arrange(fcn_id, timepoint) %>%
# 	View()



		
# These fcn_id only have two surveys
fcn_id_two_survey <-
	survey_data_clean %>%
	filter(
		fcn_id %in% c(
			survey_data_clean %>%
				count(fcn_id) %>%
				filter(n == 2) %>% pull(fcn_id)
		)
	) %>% 
	select(camp_id, subblock_id, name_mahji, start_date, study_arm, timepoint, fcn_id, name_respondent, name_hh_head) %>%
	arrange(fcn_id, timepoint)


# These fcn_id only have have three survey
fcn_id_three_survey <-
	survey_data_clean %>%
	filter(
		fcn_id %in% c(
			survey_data_clean %>%
				count(fcn_id) %>%
				filter(n == 3) %>% pull(fcn_id)
		)
	) %>% 
	select(camp_id, subblock_id, name_mahji, start_date, study_arm, timepoint, fcn_id, name_respondent, name_hh_head) %>%
	arrange(fcn_id, timepoint)


# These fcn_id only have one survey
fcn_id_four_survey <-
	survey_data_clean %>%
	filter(
		fcn_id %in% c(
			survey_data_clean %>%
				count(fcn_id) %>%
				filter(n == 4) %>% pull(fcn_id)
		)
	) %>% 
	select(camp_id, subblock_id, name_mahji, start_date, study_arm, timepoint, fcn_id, name_respondent, name_hh_head) %>%
	arrange(fcn_id, timepoint)




# maybe the fcn_id that only have midline or endline are related to those files n == 2 and are missing baseline or midline

fcn_id_one_survey_fncs <- fcn_id_one_survey %>% select(fcn_id)
fcn_id_two_survey_fncs <- fcn_id_two_survey %>% select(fcn_id)
 

inner_join(fcn_id_two_survey_fncs, fcn_id_one_survey_fncs)
right_join(fcn_id_two_survey_fncs, fcn_id_one_survey_fncs)




write_rds(fcn_id_one_survey, file_out_1)
write_csv(fcn_id_one_survey, file_out_1_csv)

write_rds(fcn_id_two_survey, file_out_2)
write_csv(fcn_id_two_survey, file_out_2_csv)
# Why does fcn_id == 165931 has baseline and endline but missing midline?

write_rds(fcn_id_three_survey, file_out_3)
write_csv(fcn_id_three_survey, file_out_3_csv)


write_rds(fcn_id_four_survey, file_out_4)
write_csv(fcn_id_four_survey, file_out_4_csv)
# Why do these fcn_id all have two endline surveys? Which should be kept? "179146" "181051" "184125" "286097" "302711"

# 8wDI21x should be 101595; 
# 10F192633F33_4329 should be  192633 (survey) or 192619 (geocene), 
# 4E179029Pp 15_398 should be 179086 (survey) or 179029 (geocene); 
# 8wD291519 was not found in survey? but fcn_id should be 291519; 

# survey_data_clean %>%
# 	# filter(
# 	# 	hh_id %in% c(
# 	# 		"8WDI18224646", "8WDI18101573", "8WDI21x", "8WG107339G10_3638", "10G192754G38_3622", "10F192633F33_4329", 
# 	# 		"4E179029Pp 15_398", "10F201621F40_4201", "10F201745F40_4208", "9G123577G29_6359", 
# 	# 		"8WDH21291216", "8WD291519", "10D192773D12_2837")
# 	# ) %>%
# 	filter(
# 		fcn_id %in% c(
# 			"101595", "192633", "192619", "179086", "179029", "291519"
# 		)
# 	) %>%
# 	# View()
# 	# count(fcn_id, hh_id) %>%
# 	# filter(n < 2) %>% # 67 hh
# 	select(fcn_id, hh_id)



###################################################################3
####################################################################

# include paired hh only
fcn_id_triple <-
	survey_data_clean %>%
	count(fcn_id) %>%
	filter(n == 3) %>%
	pull(fcn_id)

#this is meant to gather the number of IOM households at baseline 
#IOM HH are camps 18 and 8, 9 ,10
survey_data_clean %>%
	filter(timepoint == "baseline") %>% group_by(study_arm) %>% 
  count(camp_id)
# 781 hh

survey_data_long_triple <-
	survey_data_clean %>%
	filter(fcn_id %in% fcn_id_triple) 

# ============================================================================
# Get unique hh identifier 
# ============================================================================

hh_data <-
	survey_data_long_triple %>%
	select(study_arm, camp_id, block_id, subblock_id, fcn_id, Rand_nu, hh_id, name_mahji,  contact_number, name_hh_head, name_respondent, target_child_name) %>%
	distinct() %>%
	arrange(study_arm, camp_id, block_id, subblock_id, Rand_nu, fcn_id)


 
# # ============================================================================
# # Make a wide dataset
# # ============================================================================
# 
# survey_data_intervention_baseline <-
# 	survey_data_long_paired %>%
# 	filter(study_arm == "pre-intervention")
# 
# survey_data_intervention_endline <-
# 	survey_data_long_paired %>%
# 	filter(study_arm == "post-intervention")
# 
# survey_data_comparison_baseline <-
# 	survey_data_long_paired %>%
# 	filter(study_arm == "intervention")
# 
# survey_data_comparison_endline <-
# 	survey_data_long_paired %>%
# 	filter(study_arm == "intervention follow-up")
# 
# survey_data_intervention <-
# 	survey_data_intervention_baseline %>%
# 	left_join(survey_data_intervention_endline, by = c("fcn_id"), suffix = c(".baseline", ".endline"))
# 
# survey_data_comparison <-
# 	survey_data_comparison_baseline %>%
# 	left_join(survey_data_comparison_endline, by = c("fcn_id"), suffix = c(".baseline", ".endline"))
# 
# survey_data_wide_paired <-
# 	bind_rows(survey_data_intervention, survey_data_comparison) %>%
# 	mutate(
# 		study_arm =
# 			case_when(
# 				study_arm.baseline == "pre-intervention" ~ "intervention",
# 				study_arm.baseline == "intervention" ~ "comparison"
# 			),
# 		study_arm = 
# 			ordered(
# 				study_arm,
# 				levels = c("intervention", "comparison")
# 			)
# 	) 


survey_data_long_triple %>%
	tabyl(timepoint, study_arm_overall, camp_id)

# survey_data_long_triple %>%
# 	filter(camp_id == "8E", timepoint == "midline")

survey_data_long_triple %>%
	filter(camp_id == "8W", timepoint == "midline", study_arm_overall == "intervention") %>%
	select(study_arm_overall, study_arm, timepoint, camp_id, block_id, subblock_id, fcn_id, name_mahji, name_respondent, name_hh_head, target_child_name)
	
# study_arm_overall study_arm              timepoint camp_id block_id subblock_id fcn_id name_mahji name_respondent name_hh_head target_child_name
# <ord>             <chr>                  <ord>     <chr>   <chr>    <chr>       <chr>  <chr>      <chr>           <chr>        <chr>            
# 1 comparison        intervention follow-up midline   8W      c        B30         114556 NA         Toslima         NA           Shahida          
# 2 comparison        intervention follow-up midline   8W      a        B13         124614 NA         Arafat Begum    NA           Nur Kamal 

# --> Need to change their study arm to 8E

survey_data_long_triple %>%
	tabyl(timepoint, study_arm_overall)

# ============================================================================
# Save datasets 
# ============================================================================

write_rds(survey_data_long_triple, file_out_5)

survey_data_long_triple_deidentified <-
	survey_data_long_triple %>%
	select(-c(name_mahji,  contact_number, name_hh_head, name_respondent, target_child_name))


write_rds(survey_data_long_triple_deidentified, file_out_5_deidentified)


survey_data_long_triple %>% select(ends_with("insult_hh"))


survey_data_long_triple %>% 
	filter(camp_id == "NOJIMULLAH")

survey_data_long_triple %>% 
	filter(start_date == "2022-07-03")


# data <- read_rds("/Volumes/GoogleDrive/.shortcut-targets-by-id/1g4FGyxS0paoGycT8REXc9ZEXkWLVDMJr/Rohingya_analysis/4_data/RohingyaFuel_survey_data_wide_paired.rds")
# 
# data %>% count(survey)

# write_rds(survey_data_intervention_baseline, file_out_survey_data_intervention_baseline)
# write_rds(survey_data_intervention_endline, file_out_survey_data_intervention_endline)
# write_rds(survey_data_comparison_baseline, file_out_survey_data_comparison_baseline)
# write_rds(survey_data_comparison_endline, file_out_survey_data_comparison_endline)
# write_rds(survey_data_intervention, file_out_survey_data_intervention)
# write_rds(survey_data_comparison, file_out_survey_data_comparison)
# write_rds(survey_data_wide_paired, file_out_survey_data_wide_paired)

#Ensuring the numbers are the same for the three datasets from ll three rounds in the triplicate data
survey_data_long_triple %>%
	count(fcn_id, study_arm_overall) %>% 
	arrange((n))

survey_data_long_triple %>% select(timepoint, study_arm_overall, first_receive_lpg) 

