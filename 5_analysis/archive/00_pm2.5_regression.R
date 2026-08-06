################################################################################
# @Project: Rohingya analysis
# @Author: Chris LeBoa
# @Description: LPG analysis 
# @Date: 220722
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))


# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")
hh_pm_data_location <-here::here("4_data/PM_hh_averages_20220717.rds")
#This comes from running PM2.5 weighted averages filein the pm analysis folder

dcl::create_data_project("/Users/ChrisLeBoa/GitHub/Biostatistics/Geospatial_analysis")

# Read in data 
survey_data <- read_rds(file_survey_data_base)
pm_data <- read_rds(hh_pm_data_location)
#===============================================================================

#Join particulate matter data readers and the surveys from the main study
pm_fcn <- 
	pm_data %>% 
	mutate(fcn_id = str_extract(hh_id, "[^[A-Za-z]]*$")) 

pm_wide <- 
	pm_fcn %>% 
	select(fcn_id, timepoint, hh_mean) %>% 
	filter(!is.na(fcn_id)) %>% 
	pivot_wider(., names_from = c(timepoint), values_from = hh_mean, values_fn = length) %>% 
	write_csv(here::here("4_data/PM_hh_averages_wide"))


combined_data <- 
pm_fcn %>% 
	mutate(study_arm = as.character(study_arm)) %>% 
	left_join(survey_data , by = c("fcn_id", "timepoint", "study_arm" = "study_arm_overall")) %>% 
	filter(!is.na(SubmissionDate))



lm_pm2.5 <- lm_robust(data = combined_data, formula = hh_mean ~ timepoint_num + study_arm_overall_num + did, clusters = fcn_id)
combined_data$timepoint_num
lm_pm2.5
