# RF106 analysis table 

# Author: Christopher LeBoa 
# Version: 2024-01-19

# Libraries
library(tidyverse)

# Parameters

#===============================================================================

rm(list = ls())

#install.packages("binom")
source(here::here("0_config.R"))
source(here::here("5_analysis/Dif_in_dif_fxn.R"))
## Chris
# source(here::here("1_config.R"))

source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))

# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")
#===============================================================================

# Fear of stoves and of children being burned by stoves 
survey_data <- read_rds(file_survey_data_base)

survey_data %>% group_by(timepoint, study_arm_overall) %>% count(lpg_child_burn)

survey_data %>% group_by(timepoint, study_arm_overall) %>% count(lpg_afraid) %>% view()

## Looking at reports of fire and why 
survey_data %>% group_by(timepoint, study_arm_overall) %>% count(fire_number, fire_why) 


## Stove repairs  
survey_data %>% 
  mutate(stove_repair = case_when(
    str_detect(lpg_stove_repair, "1") ~ "ignition", 
    str_detect(lpg_stove_repair, "2") ~ "burner", 
    str_detect(lpg_stove_repair, "3") ~ "structure", 
    str_detect(lpg_stove_repair, "4") ~ "rust", 
    str_detect(lpg_stove_repair, "4") ~ "body", 
    .default = "NA")) %>%
  group_by(timepoint) %>% 
  count(stove_repair) %>% view() 

## Flavor preferences LPG vs not LPG 
survey_data %>% 
  mutate(
    flavor_pref = case_when(
  str_detect(food_flavor, "1") ~ "wood",
  str_detect(food_flavor, "2") ~ "LPG",
  str_detect(food_flavor, "7")~ "no difference",
  .default = NA)) %>% 
   group_by(timepoint) %>% 
  count(flavor_pref) %>% 
  view() 


## 
