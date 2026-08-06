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

file_out_1 <- here::here("7_tables/collect_fuel_walk_wait_hr_arms_combined.csv")
file_out_2 <- here::here("7_tables/collect_fuel_walk_wait_hr.csv")

#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)


##############################################################################
# Time to collect fuel
##############################################################################

# At baseline: How long (in nearest hour) does it take you to walk to and from the area where you collect wood?
# Even comparison hh who don't actively collect may have interpreted this as "when you do/did get firewood, how long did it take you to walk" 
# because even though these hh had low usage in the past 30 days of firewood collected from the forest
# 380 comparison and 370 intervention hh answered this question

# in the later surveys we asked: "Currently, how long (in nearest hour) does it take you to walk to and from the area where you collect wood?"
# At most 20 hh responded at midline and endline 
collect_fuel_walk_wait_hr_base <-
  survey_data %>%
  select(timepoint, study_arm_overall, fcn_id, collect_wood_walk_hr, receive_lpg_walk, receive_lpg_wait, buy_lpg_walk, buy_lpg_wait, receive_crh_walk, receive_crh_wait) %>%
  # THere are some buy_lpg_walk and buy_lpg_wait times that are zero and all of the hh have a response. Assume there was sometimes a skip logic problem and set them to zero 
  mutate_at(vars(collect_wood_walk_hr, receive_lpg_walk, receive_lpg_wait, buy_lpg_walk, buy_lpg_wait, receive_crh_walk, receive_crh_wait), ~replace(., . < 0.01, NA)) %>%
  rowwise() %>%
  mutate(
    receive_lpg_walk_wait = sum(receive_lpg_walk, receive_lpg_wait, na.rm = TRUE), # reads NAs as zero
    buy_lpg_walk_wait = sum(buy_lpg_walk, buy_lpg_wait, na.rm = TRUE),
    receive_crh_walk_wait = sum(receive_crh_walk, receive_crh_wait, na.rm = TRUE)
  ) %>%
  mutate_at(vars(receive_lpg_walk_wait, buy_lpg_walk_wait, receive_crh_walk_wait), ~replace(., . < 0.01, NA)) %>%
  gather(-study_arm_overall, -fcn_id, -timepoint, key = "fuel_collection_activity", value = "time_hr") 

collect_fuel_walk_wait_hr_arms_combined <-
  collect_fuel_walk_wait_hr_base %>%
  # filter(fuel_collection_activity %in% c("collect_wood_walk_hr", "receive_lpg_walk", "receive_lpg_wait", "buy_lpg_walk", "buy_lpg_wait")) %>%
  group_by(timepoint, fuel_collection_activity) %>% 
  mutate(counter = ifelse(is.na(time_hr), 0, 1)) %>%
  summarise(
    n = sum(counter),
    time_hr_mean = mean(time_hr, na.rm = TRUE),
    time_hr_sd = sd(time_hr, na.rm = TRUE),
    time_hr_min = min(time_hr, na.rm = TRUE),
    time_hr_max = max(time_hr, na.rm = TRUE)
  )

collect_fuel_walk_wait_hr <-
  collect_fuel_walk_wait_hr_base %>%
  # filter(fuel_collection_activity %in% c("collect_wood_walk_hr", "receive_lpg_walk", "receive_lpg_wait", "buy_lpg_walk", "buy_lpg_wait")) %>%
  group_by(timepoint, study_arm_overall, fuel_collection_activity) %>% 
  mutate(counter = ifelse(is.na(time_hr), 0, 1)) %>%
  summarise(
    n = sum(counter),
    time_hr_mean = mean(time_hr, na.rm = TRUE),
    time_hr_sd = sd(time_hr, na.rm = TRUE),
    time_hr_min = min(time_hr, na.rm = TRUE),
    time_hr_max = max(time_hr, na.rm = TRUE)
  )

write.csv(collect_fuel_walk_wait_hr_arms_combined, file_out_1)
write.csv(collect_fuel_walk_wait_hr, file_out_2)

###############################################################################
### Make figure

fig_collect_fuel_walk_wait_hr <- 
  collect_fuel_walk_wait_hr_base %>%
  filter(fuel_collection_activity %in% c("collect_wood_walk_hr", "receive_lpg_walk_wait", "buy_lpg_walk_wait", "receive_crh_walk_wait")) %>%
  mutate(
    fuel_collection_activity = 
      ordered(
        fuel_collection_activity,
        levels = c("collect_wood_walk_hr", "receive_lpg_walk_wait", "buy_lpg_walk_wait", "receive_crh_walk_wait"),
        labels = c("Walk to and from forest", "Walk to, wait, and return from \nreceiving LPG", "Walk to, wait, and return from \nbuying LPG", "Walk to, wait, and return from \nreceiving  \ncompressed rice husks")
      )
  ) %>%
  ggplot(aes(x = fuel_collection_activity, y = time_hr, fill = fuel_collection_activity)) +
  geom_boxplot(varwidth = TRUE) +
  viridis::scale_fill_viridis(
    discrete = TRUE,
    end = 7/8
  ) +
  # scale_y_continuous(
  # 	labels = scales::percent_format(accuracy = 1)
  # ) + # Percentage labels rounded to the nearest integer
  theme_bw() +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  labs(
    title = "Fuel collection walk and wait times",
    x = "",
    # x = "Fuel collection activity",
    y = "Hours",
    fill = "Activity"
  ) + facet_wrap(vars(timepoint))

fig_collect_fuel_walk_wait_hr

ggsave(
  here::here("6_figures", "fig_collect_fuel_walk_wait_hr.png"),
  plot = last_plot(),
  scale = 1,
  height = 6,
  width = 10,
  units = "in",
  device = "png"
)
