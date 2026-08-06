# Comparison of baseline characteristics

# Author: Chris LeBoa 
# Version: 2024-03-29

# Libraries
library(tidyverse)

# Parameters
source(here::here("0_config.R"))
## Chris


# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")

#===============================================================================

#Code
survey_data <- read_rds(file_survey_data_base)
baseline_data <- survey_data %>% filter(timepoint %in% c("baseline"))

table16Vars <- 
  Hmisc::Cs(
    borrow_food, reduce_food, 
    reduce_meals_lack_food, not_eat_lack_food, restrict_food,
    borrow_fuel, reduce_fuel, 
    reduce_meals_lack_fuel, not_eat_lack_fuel
  )

## Vector of categorical variables that need transformation
table16FactorVars <- table16Vars


tab16 <- 
  CreateTableOne(
    data = baseline_data, 
    vars = table16Vars, 
    factorVars = table16FactorVars, 
    strata = "study_arm_overall"
  )
  
