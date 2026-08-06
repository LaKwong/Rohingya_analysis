# RF105 supplemental table 1


# Author: Christopher LeBoa 
# Version: 2024-08-21

# Libraries
library(tidyverse)

# Parameters
source(here::here("5_analysis/5_geocene_analysis/2_RF105_Fig2_code.R"))

#===============================================================================


## Number of days use stove based on stove use monitor 
df_days_receive %>% 
  mutate(
    days_after_group = case_when(
      days_after_first_receiving < 0  ~ "not yer received", 
      days_after_first_receiving > 0 & days_after_first_receiving <30 ~ "0-30",
      days_after_first_receiving > 31 & days_after_first_receiving <60 ~ "31-60",
      days_after_first_receiving > 61 & days_after_first_receiving <90 ~ "61-90",
      days_after_first_receiving > 91 & days_after_first_receiving <120 ~ "91-120",
      days_after_first_receiving > 121 & days_after_first_receiving <150 ~ "121-150",
      days_after_first_receiving > 151 & days_after_first_receiving <180 ~ "151-180",
      days_after_first_receiving > 181 & days_after_first_receiving <210 ~ "181-210",
      days_after_first_receiving > 211  ~ "211+",
    ), 
    cooking_events_with_biomass = if_else(is.na(cooking_events_with_biomass), 0, cooking_events_with_biomass), 
    cooking_events_with_lpg = if_else(is.na(cooking_events_with_lpg), 0, cooking_events_with_lpg)) %>% 
  group_by(days_after_group) %>% 
  summarise(n = n(), 
            pct_lpg = cooking_events_with_lpg / (cooking_events_with_lpg + cooking_events_with_biomass)*100) %>% 
  slice(1) %>% 
  select(days_after_group, n)

df_days_receive %>% 
  mutate(
    days_after_group = case_when(
    days_after_first_receiving < 0  ~ "not yer received", 
    days_after_first_receiving > 0 & days_after_first_receiving <30 ~ "0-30",
    days_after_first_receiving > 31 & days_after_first_receiving <60 ~ "31-60",
    days_after_first_receiving > 61 & days_after_first_receiving <90 ~ "61-90",
    days_after_first_receiving > 91 & days_after_first_receiving <120 ~ "91-120",
    days_after_first_receiving > 121 & days_after_first_receiving <150 ~ "121-150",
    days_after_first_receiving > 151 & days_after_first_receiving <180 ~ "151-180",
    days_after_first_receiving > 181 & days_after_first_receiving <210 ~ "181-210",
    days_after_first_receiving > 211  ~ "211+",
    ), 
    cooking_events_with_biomass = if_else(is.na(cooking_events_with_biomass), 0, cooking_events_with_biomass), 
    cooking_events_with_lpg = if_else(is.na(cooking_events_with_lpg), 0, cooking_events_with_lpg)) %>% 
  group_by(days_after_group) %>% 
  summarise(n = n(), 
            pct_lpg = cooking_events_with_lpg / (cooking_events_with_lpg + cooking_events_with_biomass)*100) %>% 
  mutate(
    pct_lpg_grp = case_when(
      pct_lpg == 0 ~ "0",
      pct_lpg > 0 & pct_lpg <20 ~ "1-19",
      pct_lpg >= 20 & pct_lpg <40 ~ "20-39",
      pct_lpg >= 40 & pct_lpg <60 ~ "40-59",
      pct_lpg >= 60 & pct_lpg <80 ~ "60-79",
      pct_lpg >= 80 & pct_lpg <100 ~ "80-99",
      pct_lpg == 100 ~ "100"
    )) %>% 
  count(pct_lpg_grp) %>% 
  view()

