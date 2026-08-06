################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Food security analysis
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
## Chris
# source(here::here("1_config.R"))
source(here::here("5_analysis/Dif_in_dif_fxn.R"))
source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))

# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")
#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)

###-============================================================================
survey_data %>% 
	summarise(mean = mean(sleep_hours, na.rm = TRUE))

survey_data %>% 
	count(sleep_bad_dreams)


survey_data %>% 
	count(sleep_quality)

