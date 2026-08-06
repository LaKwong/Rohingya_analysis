# MUAC analysis 

# Author: Chris LeBoa 
# Version: 2023-11-06

# Libraries
library(tidyverse)

# Parameters
# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")
survey_data <- read_rds(file_survey_data_base)

#MUAC cutoffs for Asian Children using Fiorentino et al 2022
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4739613/#:~:text=Early%20identification%20of%20children%20%3C5,(WHZ)%20%3C%2D2.
# Found to be more accurate among cambodian children (only cohort of Asian children to be tested)
# Alternative is WHO definition of 125 mm for malnourished and 115 for severe malnourished 
#Boys under 2 = 139 mm
#Boys 2-5 = 144 mm

#Girls under 2 = 136 
#Girls 2-5 = 142

#Girls 
#===============================================================================

#Code

survey_data %>% 
  select(target_child_sex, target_child_months, all_of(MUAC_vars)) %>% 
  filter(target_child_arm_measurements_yn == 1, target_child_mid_arm_circ_av < 145) 

survey_data %>% count(target_child_arm_measurements_yn )
  
