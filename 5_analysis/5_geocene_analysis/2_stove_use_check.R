#stove use check 

library(tidyverse)
library(lubridate)

data <- read_csv("/Users/ChrisLeBoa/Downloads/8ad9a631-5248-4a85-abb8-42391aa2fc2a/events.csv")

data %>% 
  mutate(cook_time = stop_time - start_time) %>% 
  group_by(processor_name, day(start_time)) %>% 
  summarize(
    n(),
    sum(cook_time)
    )
