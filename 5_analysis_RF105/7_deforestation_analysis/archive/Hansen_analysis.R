# Deforestation using Hansen Data

# Author: Christopher LeBoa 
# Version: 2023-06-08

# Libraries
library(tidyverse)

# Parameters
file_in <- here::here("/Users/ChrisLeBoa/Library/CloudStorage/GoogleDrive-cleboa@berkeley.edu/.shortcut-targets-by-id/1g4FGyxS0paoGycT8REXc9ZEXkWLVDMJr/Rohingya_analysis/2_data_raw/deforestation/hansen_deforestation_21.csv")

defor <- read_csv(file_in)
#===============================================================================

defor %>% 
  filter(year <= 2012) %>% 
  summarise(mean(forest_loss_m))

defor %>% 
  filter(2000 <= year, year <= 2004) %>% 
  summarise(mean(forest_loss_m))

defor %>% 
  filter(2017 <= year, year <= 2018) %>% 
  summarise(mean(forest_loss_m))

defor %>% 
  filter(2019 <= year) %>% 
  summarise(mean(forest_loss_m))
