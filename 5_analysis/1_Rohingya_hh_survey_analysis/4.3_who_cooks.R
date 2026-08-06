################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Fuel sources analysis
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
# source(here::here("5_analysis/Dif_in_dif_fxn.R"))
## Chris
# source(here::here("1_config.R"))

source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))

# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")


#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)


###############################################################################
### Who cooks 
##############################################################################

# Percentage of hh that mention women, girls, men, and boys cook

## Vector of variables to summarize
# Use Hmisc::Cs to quote each variable
table12Vars <- 
  c("cook_who_w", "cook_who_g", "cook_who_m", "cook_who_b")

## Vector of categorical variables that need transformation
table12FactorVars <- table12Vars

# Create a TableOne object
tab12 <- 
  CreateTableOne(
    data = survey_data, 
    vars = table12Vars, 
    factorVars = table12FactorVars, 
    strata = "study_arm"
  )

tab12
# cook_who_b = 0 (%) why for other groups reportedd as ==1 ? because there was 0 boys so it doesn't show up as a factor?
