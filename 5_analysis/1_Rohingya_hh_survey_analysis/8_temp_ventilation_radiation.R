################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Fuel sources analysis
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))

## Chris
# source(here::here("1_config.R"))

#dcl::create_data_project("/Volumes/GoogleDrive/.shortcut-targets-by-id/1aMSePP0QEpnGatESIEaZaCJYd9I3tzVA/Mortality Reduction Blast Injury/Review/Code/blast_review")
# Parameters

file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_paired.rds")
#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)


################################################################################
# Ventilation openings in house
################################################################################



# How many hh have no windows?
survey_data %>%
	count(window_number == 0)
# Almost 80% of  hh have no windows!

# Of hh that have at least one window, what is the average number of windows?
survey_data %>%
	filter(window_number > 0) %>%
	summarise_at(vars(window_number), list(mean = mean), na.rm = TRUE)

# Of hh that have at least one window, how many have cross-ventilation (with either another window or a door)?
survey_data %>%
	filter(window_number > 0) %>%
	select(window_door_wall) %>%
	table()