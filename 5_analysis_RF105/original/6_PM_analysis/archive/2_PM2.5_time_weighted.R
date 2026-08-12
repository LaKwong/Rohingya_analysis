################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong / Chris LeBoa 
# @Description: Combine with amount of time spent inside to get weighted average
# @Date: 221009
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
# source(here::here("3_data_cleaning/1.5_define_vector_columns.R")) #Pulls in all variable group names

## Chris
# source(here::here("1_config.R"))

library(tidyquant)
library(grid)
library(geomtextpath)
# library(R.utils)
# library(googledrive)




# https://cran.r-project.org/web/packages/MakefileR/vignettes/demo.html
# 
# Message from Jamil 
# "All the Outdoor monitors at the same place and same school at Intervention but Pre-Intervention At 2 Mosque(camp 8W and camp 10). All the Baseline outdoor monitor and midline outdoor monitor setup as the same location.Note that no different location in school and Mosque. Always had collected one location PM 2.5 data."



#Input Files


file_pm_data_hour_av_by_hh <- here::here("4_data/pm_data_hour_av_by_hh.rds")
file_in_10 <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")

# Load input files

pm_data_hour_av_by_hh <- read_rds(file_pm_data_hour_av_by_hh)
survey_data_time_inside <- read_rds(file_in_10) %>% 		
	select(
		hh_id, # timepoint, study_arm, # I don't think we asked time_inside for each timepoint
		hours_inside, #hours_outside, 
		target_child_hours_outside) 	%>% # target_child_hours_outside, 
		mutate(
			target_child_hours_inside = 24 - target_child_hours_outside, 
			respondent__pct_hours_insid =  hours_inside / 24 * 100, 
			target_child_pct_hours_inside = target_child_hours_inside  / 24 * 100)








#################################################################################

# survey_data_time_inside <-
# 	survey_data %>%
# 	select(
# 		hh_id, # timepoint, study_arm, # I don't think we asked time_inside for each timepoint
# 		hours_inside, #hours_outside, 
# 		target_child_hours_inside # target_child_hours_outside, 
# 	)

pm_data_hour_av_by_hh_time_inside <-
	left_join(pm_data_hour_av_by_hh, survey_data_time_inside, by = c("hh_id"))
						

pm_data_mean_timepoint_study_arm <-  
							pm_data_hour_av_by_hh %>% 
							group_by(study_arm, timepoint) %>% 
							summarise(
								mean(hh_mean, na.rm = TRUE), 
								sd(hh_sd, na.rm = TRUE), 
								
							)
						
						pm_data_average %>% 
							knitr::kable()
						
						
						pm_data_average <-  
							pm_data_hour %>% 
							group_by(timepoint, study_arm, hh_id) %>% 
							summarise(
								max(dateTime_hour) - min(dateTime_hour),
								hh_mean = mean(PM_Estimate), 
								sd(PM_Estimate)
							)
						
						# pm_data_average %>%
						# ungroup() %>% 
						# mutate(study_arm = as.character(study_arm)) %>% 
						# select(hh_id, timepoint, hh_mean) %>% 
						# pivot_wider(., 
						# names_from = c(timepoint),
						# values_from = hh_mean
						# ) %>% write_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1g4FGyxS0paoGycT8REXc9ZEXkWLVDMJr/Rohingya_analysis/4_data/sensor_data_wide.csv")
						
						
						
						################# Get data for HAPIT ########################3333
						#
						
						
						
						
						
						
						