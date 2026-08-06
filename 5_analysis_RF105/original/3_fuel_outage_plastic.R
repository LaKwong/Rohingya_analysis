################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong/ Chris LeBoa 
# @Description: Household running out of LPG and sue of plastics to cook
# @Date: 241019
################################################################################

source(here::here("0_config.R"))
source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))
source(here::here("5_analysis/Dif_in_dif_fxn.R")) 

# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")


# Load input files
survey_data <- read_rds(file_survey_data_base)

survey_data_baseline <-
  survey_data %>%
  filter(timepoint == "baseline")


################################################################################
# Fuel sources
################################################################################
fuel_used_ever_bl <- 
  survey_data %>% 
  filter(timepoint == "baseline") %>%
  select(
    fcn_id,
    study_arm_overall,
    timepoint,
    fuel_ever_gather_scraps,
    fuel_ever_collect_wood,
    fuel_ever_receive_wood,
    fuel_ever_buy_wood,
    fuel_ever_receive_lpg,
    fuel_ever_buy_lpg,
    fuel_ever_receive_crh,
    fuel_ever_buy_crh,
    fuel_ever_other
  ) %>%
  pivot_longer( 
    cols = c(
      fuel_ever_gather_scraps,
      fuel_ever_collect_wood,
      fuel_ever_receive_wood,
      fuel_ever_buy_wood,
      fuel_ever_receive_lpg,
      fuel_ever_buy_lpg,
      fuel_ever_receive_crh,
      fuel_ever_buy_crh,
      fuel_ever_other
    ),
    names_to = "fuel"
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  mutate(fuel = str_remove(fuel, "^.{10}")) %>%
  mutate(
    study_arm_overall = 
      ordered(
        study_arm_overall,
        levels = c("comparison", "intervention"),
        labels = c("Comparison group", "Intervention group")
      )
  ) %>%
  mutate(
    fuel = 
      ordered(
        fuel,
        levels = c("gather_scraps", "collect_wood", "buy_wood", "receive_wood", "buy_crh", "receive_crh", "buy_lpg", "receive_lpg", "other"),
        labels = c("Scraps, gathered", "Wood, collected", "Wood, purchased", "Wood, received", "Compressed rice husks, purchased", "Compressed rice husks, received", "LPG, purchased", "LPG, received", "Plastic, collected") # We know from the qualitative interviews that Other == Plastic
      )
  )

fuel_used_ever_bl %>% 
  group_by(fuel, study_arm_overall) %>% 
  summarize(sum(value, na.rm = TRUE))

fuel_used_ever <- 
  survey_data %>% 
  filter(timepoint == "endline") %>%
  select(
    fcn_id,
    study_arm_overall,
    timepoint,
    fuel_ever_gather_scraps,
    fuel_ever_collect_wood,
    fuel_ever_receive_wood,
    fuel_ever_buy_wood,
    fuel_ever_receive_lpg,
    fuel_ever_buy_lpg,
    fuel_ever_receive_crh,
    fuel_ever_buy_crh,
    fuel_ever_other
  ) %>%
  pivot_longer( 
    cols = c(
      fuel_ever_gather_scraps,
      fuel_ever_collect_wood,
      fuel_ever_receive_wood,
      fuel_ever_buy_wood,
      fuel_ever_receive_lpg,
      fuel_ever_buy_lpg,
      fuel_ever_receive_crh,
      fuel_ever_buy_crh,
      fuel_ever_other
    ),
    names_to = "fuel"
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  mutate(fuel = str_remove(fuel, "^.{10}")) %>%
  mutate(
    study_arm_overall = 
      ordered(
        study_arm_overall,
        levels = c("comparison", "intervention"),
        labels = c("Comparison group", "Intervention group")
      )
  ) %>%
  mutate(
    fuel = 
      ordered(
        fuel,
        levels = c("gather_scraps", "collect_wood", "buy_wood", "receive_wood", "buy_crh", "receive_crh", "buy_lpg", "receive_lpg", "other"),
        labels = c("Scraps, gathered", "Wood, collected", "Wood, purchased", "Wood, received", "Compressed rice husks, purchased", "Compressed rice husks, received", "LPG, purchased", "LPG, received", "Plastic, collected") # We know from the qualitative interviews that Other == Plastic
      )
  ) 


fig_fuel_used_ever <-
  fuel_used_ever %>%
  group_by(fuel, study_arm_overall, timepoint) %>% 
  #	summarise(prop = sum(value) / n()) %>%
  summarise(prop = mean(value, na.rm = TRUE)) %>% 
  ggplot() + 
  geom_col(aes(x = fuel, y = prop, fill = fuel, group = fuel)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Types of Cooking Fuel Used Ever, By Study Arm and Timepoint",
    fill = "Fuel",
    x = "Timepoint", 
    y = "Percent of households (%)"
  ) + 
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
  facet_wrap(. ~ study_arm_overall)

fig_fuel_used_ever

ggsave(
  here::here("6_figures", "fig_fuel_used_ever.png"),
  plot = last_plot(),
  scale = 1,
  height = 6,
  width = 10,
  units = "in",
  device = "png"
)
### Fuel used in the past 30 days

fuel_used_30_days <- 
  survey_data %>% 
  select(
    fcn_id,
    study_arm_overall,
    timepoint,
    fuel_30_gather_scraps,
    fuel_30_collect_wood,
    fuel_30_receive_wood,
    fuel_30_buy_wood,
    fuel_30_receive_lpg,
    fuel_30_buy_lpg,
    fuel_30_receive_crh,
    fuel_30_buy_crh,
    fuel_30_other
  ) %>%
  pivot_longer( 
    cols = c(
      fuel_30_gather_scraps,
      fuel_30_collect_wood,
      fuel_30_receive_wood,
      fuel_30_buy_wood,
      fuel_30_receive_lpg,
      fuel_30_buy_lpg,
      fuel_30_receive_crh,
      fuel_30_buy_crh,
      fuel_30_other
    ),
    names_to = "fuel"
  ) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  mutate(fuel = str_remove(fuel, "^.{8}")) %>%
  mutate(
    study_arm_overall = 
      ordered(
        study_arm_overall,
        levels = c("comparison", "intervention"),
        labels = c("Comparison group", "Intervention group")
      )
  ) %>%
  mutate(
    fuel = 
      ordered(
        fuel,
        levels = c("gather_scraps", "collect_wood", "buy_wood", "receive_wood", "buy_crh", "receive_crh", "buy_lpg", "receive_lpg", "other"),
        labels = c("Scraps, gathered", "Wood, collected", "Wood, purchased", "Wood, received", "Compressed rice husks, purchased", "Compressed rice husks, received", "LPG, purchased", "LPG, received", "Plastic, collected") # We know from the qualitative interviews that Other == Plastic
      )
  )


fig_fuel_used_30_days <-
  fuel_used_30_days %>%
  group_by(fuel, study_arm_overall, timepoint) %>%
  summarise(prop = mean(value)) %>%
  ggplot() + 
  geom_point(aes(x = timepoint, y = prop, color = fuel, group = fuel)) + 
  geom_line(aes(x = timepoint, y = prop, color = fuel, group = fuel)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Types of Cooking Fuel Used in the Past 30 Days, By Study Arm and Timepoint",
    color = "Fuel",
    x = "Timepoint", 
    y = "Percent of households (%)"
  ) + 
  theme_bw() + 
  facet_wrap(. ~ study_arm_overall)

## Fuel 30 graph simplified 
fuel_used_30_days_simple_wide <-
  survey_data %>%
  group_by(study_arm_overall, timepoint) %>%
  mutate(
    wood = case_when(
      fuel_30_gather_scraps == 1 ~ 1, 
      fuel_30_buy_wood == 1 ~ 1, 
      fuel_30_collect_wood == 1 ~ 1, 
      fuel_30_receive_wood == 1 ~ 1, 
      .default = 0
    ), 
    lpg = case_when(
      fuel_30_buy_lpg == 1 ~ 1, 
      fuel_30_receive_lpg == 1 ~ 1, 
      .default = 0
    ),
    other = case_when(
      fuel_30_gather_scraps == 1 ~ 1, 
      fuel_30_buy_wood == 1 ~ 1, 
      fuel_30_collect_wood == 1 ~ 1, 
      fuel_30_receive_wood == 1 ~ 1, 
      fuel_30_buy_crh == 1 ~ 1,
      fuel_30_receive_crh == 1 ~ 1, 
      fuel_30_other == 1 ~ 1, 
      .default = 0
    ),
  ) 


fuel_used_30_days_simple <-  ## THe percent of hh that used wood and lpg in the last 30 days 
  fuel_used_30_days_simple_wide %>% 
  mutate(wood_and_lpg = if_else(wood == 1 & (fuel_30_receive_lpg == 1 | fuel_30_buy_lpg == 1), 1, 0)) %>% 
  pivot_longer(cols = c(wood, fuel_30_receive_lpg, fuel_30_buy_lpg, wood_and_lpg), names_to = "fuel_simple", values_to = "value") %>% 
  group_by(fuel_simple, timepoint, study_arm_overall) %>% 
  summarise(
    prop = mean(value), 
    sd = sd(value),
    n = n(),
    se = sd / sqrt(n)) %>% 
  #mutate(lower = prop -ci, upper = prop + ci) %>% 
  ungroup() 

fuel_used_30_days_simple <-  ## THe percent of hh that used wood and lpg in the last 30 days 
  fuel_used_30_days_simple_wide %>% 
  mutate(wood_and_lpg = if_else(wood == 1 & (fuel_30_receive_lpg == 1 | fuel_30_buy_lpg == 1), 1, 0)) %>% 
  pivot_longer(cols = c(wood, fuel_30_receive_lpg, fuel_30_buy_lpg, wood_and_lpg), names_to = "fuel_simple", values_to = "value") %>% 
  group_by(fuel_simple) %>% 
  summarise(
    prop = mean(value), 
    sd = sd(value),
    n = n(),
    se = sd / sqrt(n)) %>% 
  #mutate(lower = prop -ci, upper = prop + ci) %>% 
  ungroup() 




########################################################################
#Calculating the number of days before refill the hh ran out of LPG
#######################################################################

freq_cook <- 
  survey_data %>% 
  filter(!fuel_use_non_lpg_freq_cook > 200 | is.na(fuel_use_non_lpg_freq_cook)) %>% 
  group_by(study_arm_overall, timepoint) %>% 
  mutate(
    run_out = if_else(fuel_use_non_lpg_freq_cook > 0, 1, 0)
  ) 

freq_cook %>% 
  ungroup() %>% 
  group_by(timepoint) %>% 
  summarize(
    mean = mean(fuel_use_non_lpg_freq_cook,  na.rm = TRUE),    
    sd = sd(fuel_use_non_lpg_freq_cook, na.rm = TRUE),
    n = n(), 
    pct_out = sum(run_out, na.rm = TRUE)/n
  ) %>%
  mutate(
    se = sd/ sqrt(n),
    lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
    upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se, 
    lower.ci.prop = pct_out - 1.96 * sqrt((pct_out/(1-pct_out)/n)), 
    upper.ci.prop = pct_out + 1.96 * sqrt((pct_out/(1-pct_out)/n)),
  )  %>% 
  view()



survey_data %>% 
  filter(fuel_use_non_lpg_freq_cook < 200) %>% 
  ggplot() + 
  geom_histogram(aes(fuel_use_non_lpg_freq_cook)) + 
  labs(x = "How many days before your last LPG refill did your household run out of LPG for cooking food?")

########################################################################
## Percent of households that report fuel shortages use Plastics  
#######################################################################

freq_cook %>%
  filter(fuel_cant_afford_2wk == 1) %>% 
  group_by(timepoint) %>%
  mutate(
    burn_plastic_yn = if_else(burn_plastic_frequency > 0, 1, 0), #Burn plastic at all 
    burn_plastic_gt2 = if_else(burn_plastic_frequency > 1, 1, 0), #Burns plastic at least 2x per week 
  ) %>%
  summarize(
    prop_plastic_any = mean(burn_plastic_yn, na.rm = TRUE),
    prop_plastic_gt2x = mean(burn_plastic_gt2, na.rm = TRUE)
  ) 
  
  
