# Data Missing from endline 

# Author: Chris 
# Version: 2022-09-01

# Libraries
library(tidyverse)
source(here::here("0_config.R"))

# Parameters
data_long %>% 
	filter(str_detect(study_arm, "comparison rd3|post-intervention rd3"))

data_long %>% 
	filter(str_detect(study_arm, "intervention follow-up|post-intervention"))
#===============================================================================

Code