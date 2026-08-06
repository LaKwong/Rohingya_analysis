# File List Host hh for contacting for third round of interviews

# Author: Chris LeBoa
# Version: 2022-07-15

# Libraries
library(tidyverse)

host_data <- read_rds("/Users/ChrisLeBoa/GitHub/RohingyaLPG/4_data/RohingyaFuel_survey_data_clean_host.rds")

host_data %>%
	filter(study_arm == 5) %>% 
	select(fcn_id, upazila_id, union_id, ward_id, village_id, hh_id, name_respondent, name_hh_head, target_child_name, target_child_dob_1, contact_number_endline) %>% 
	
	write_csv("/Users/ChrisLeBoa/GitHub/RohingyaLPG/4_data/host_3rd_rd_220716.csv")
# Parameters
#view(host_data)
#===============================================================================

Code