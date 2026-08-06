################################################################################
# @Project: EarthEnable
# @Title: EathEnable_Videos_combine_raw_data
# @Author: Laura H Kwong
# @Description: Import the raw data
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))



source(here::here("1_data_import/1_import_merge_baseline_endline.R"))
source(here::here("3_data_cleaning/1_clean_Rohingya_data.R"))
source(here::here("3_data_cleaning/1_clean_host_data.R"))
source(here::here("3_data_cleaning/2_tidy_Rohingya_host_data.R"))
source(here::here("3_data_cleaning/3_pair_Rohingya_data.R"))
# source(here::here("3_data_cleaning/3_pair_host_data.R"))

source(here::here("5_analysis/1_Rohingya_hh_survey_analysis/1_data_analysis_Rohingya.R"))
source(here::here("5_analysis/2_Rohingya_individual_survey_analysis/1_data_analysis_individual_Rohingya.R"))
# source(here::here("5_analysis/3_host_hh_survey_analysis/1_data_analysis_host.R"))
# source(here::here("5_analysis/4_host_individual_survey_analysis/4_data_analysis_individual_Rohingya.R"))

source(here::here("5_analysis/5_geocene_analysis/1_stove_use_analysis.R"))

source(here::here("5_analysis_RF105/6_PM_analysis/1_PM2.5_analysis_Rohingya.R"))
