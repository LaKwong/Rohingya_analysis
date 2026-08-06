################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Fuel sources analysis
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
## Chris
# source(here::here("1_config.R"))

source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))

# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")

#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)


################################################################################
################################################################################


# Based on IDIs and FGDs, "other is likely plastic

survey_data %>%
	tabyl(burn_plastic_frequency, study_arm_overall, timepoint)


# $baseline
# burn_plastic_frequency comparison intervention
# 0        323          269
# 1          7           48
# 2          1           40
# 3          0           39
# 4          1           41
# 7          0            0
# NA          0           49
# 
# $midline
# burn_plastic_frequency comparison intervention
# 0        319          454
# 1          7           13
# 2          6           13
# 3          0            2
# 4          0            4
# 7          0            0
# 
# $endline
# burn_plastic_frequency comparison intervention
# 0        334          482
# 1          0            0
# 2          0            0
# 3          0            1
# 4          0            0
# 7          0            1

survey_data %>% 
	filter(timepoint == "endline") %>% 
	select(contains("plastic"))
## Something is wrong here bc a lot of hh say that they are missing this information


# Almost 150 of 393 hh report burning households every day! We need to better understand how many meals and if they are relying on plastic for all of the fuel they need or if it is used in combination with other fuel sources

# 1. fuel_ever_plastic 
# 0. How often per week do you burn plastic? --> Need a recall period?
# 	2. When you burned plastic, was the only fuel you used plastic or did you combine plastic with other fuels?
# 	3. Why did you burn plastic
# Select_multiple
# a. We can't access other fuels
# b. If we burn plastic, we don't have to spend money buying other fuels 
# c. Burning plastic is a good way to get rid of trash
# d. Other
# Specify other
# 
# Add questions about burning plastic to the focus group discussions - doesn't quite fit with any of the FGDs, but add to distribution and training. 
