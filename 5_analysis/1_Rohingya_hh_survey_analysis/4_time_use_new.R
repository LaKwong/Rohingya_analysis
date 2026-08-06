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

file_out_1 <- here::here("7_tables/time_respondent_more_less.csv")
file_out_2 <- here::here("7_tables/time_child_more_less.csv")

#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)


################################################################################
################################################################################


##########################################################################
## Livestyle changes since LPG distribution
##########################################################################

# lpg_changes_lifestyle is select_multiple... table percent of hh that answered each?

survey_data %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise_at(
    vars(starts_with("lpg_changes_lifestyle")),
    list(mean), 
    na.rm = TRUE
  ) %>%
  gather(-timepoint, -study_arm_overall, key = "lifestyle_change", value = "pc_hh")




##########################################################################
# Time use
##########################################################################

# "Think about a day before you obtained LPG at the distribution center and compare it to now, when you do get LPG from the distribution center. Compared to before, do you think that YOU currently spend MORE or LESS time on each of the following activities?"
# 
# Same question for children's time
# 
# [more_less]	1	more 
# [more_less]	2	same
# [more_less]	3	less
# 
# For the group of questions "time_free_lpg", at baseline the relevance was "selected(${fuel_30_receive_lpg},'1') and (selected(${collect_wood_g},'1') or selected(${collect_wood_b},'1'))" - this means we ONLY asked the time use questions to family's who's CHILDREN collected wood. That's fine for the group "time_child_free_lpg" but not for the adult respondent question! 
# 	
# 	Post-intervention might have better recall because they more recently transition; so assess separately and combine if it seems reasonable to do so.
# At endline, the Skip logic was selected(${fuel_ever_receive_lpg},'1') and (selected(${collect_wood_g_ever},'1') or selected(${collect_wood_b_ever},'1')) and (selected(${timepoint, study_arm_overall},'3') or selected(${study_arm},'5')) 
# 
# activity left column, more-less on top row





##########################################################################
# Change in respondent's time
##########################################################################

time_respondent_more_less <-
  survey_data %>%
  # At baseline, comparison hh had already not used fuel for a year, 
  # so use only the results from the intervention group at midline, who will have better recall
  filter(study_arm_overall == "intervention" & timepoint == "midline") %>%
  #filter(study_arm_overall %in%  c("intervention")) %>%
  select(fcn_id, timepoint, study_arm_overall, starts_with("time_")) %>% 
  select(!contains("_child_")) %>%
  # filter(!is.na(time_child_cooking)) %>% # this question was only answered if girls or boys were reported to collect firewood
  # count() # There were only 38 respondents of 597 hh that answered the intervention survey
  gather(-fcn_id, -timepoint, -study_arm_overall, key = "activity", value = "more_less") %>%
  filter(!is.na(more_less)) %>% # There are still some hh with na, because they reported not recently harvesting wood? 
  group_by(activity) %>%
  count(more_less) %>%
  mutate(
    pc = n / sum (n) # sum(n) = 99 because this many hh answered these questions
  ) %>% # make the less ones negative
  ungroup() %>%
  # necessary to apply prop.test to each row
  rowwise() %>%
  mutate(
    lower = ifelse(n > 0, prop.test(n, n/pc)$conf.int[1], 88),
    upper = ifelse(n > 0, prop.test(n, n/pc)$conf.int[2], NA)
  ) %>%
  mutate(
    # Change sign to negative if the value was less
    pc = ifelse(more_less == 3, pc * (-1), pc),
    lower = ifelse(more_less == 3, lower * (-1), lower),
    upper = ifelse(more_less == 3, upper * (-1), upper),
    more_less = factor(more_less, levels = c(1, 2, 3), labels = c("more", "same", "less"))
  )

write.csv(time_respondent_more_less, file_out_1)

##### Make fig #########

fig_time_respondent_more_less <-
  time_respondent_more_less %>%
  filter(more_less != "same") %>%
  # no people reported that they spend time gathering non-wood items so remove this
  filter(activity != "time_gathering_nonwood_items") %>%
  mutate(
    activity = 
      ordered(
        activity, 
        levels = 
          c(
            "time_harvesting_wood",
            "time_gathering_nonwood_items",
            "time_cooking",
            "time_selling_food",
            "time_washing_dishes",
            "time_washing_clothes",
            "time_collecting_water",
            "time_unskilled_labor",
            "time_employment_ngo",
            "time_accompanying_children",
            "time_caring_for_children",
            "time_caring_for_others",
            "time_eating",
            "time_learning",
            "time_nothing",
            "time_socializing",
            "time_sleeping"
          ),
        labels = 
          c(
            "Harvesting wood",
            "Gathering non-wood items",
            "Cooking", 
            "Selling food",
            "Washing dishes",
            "Washing clothes",
            "Collecting water", 
            "Working as unskilled labor",
            "Wokring for an NGO or wage laborer",
            "Accompanying children",
            "Caring for children",  
            "Caring for others",
            "Eating",
            "Learning", 
            "Doing nothing",
            "Socializing", 
            "Sleeping"
          )
      )
  ) %>%
  ggplot(aes(x = activity, y = pc, fill = more_less)) +
  geom_col() +
  geom_linerange(aes(ymin = lower, ymax = upper)) + # , position = position_dodge(0.9)
  viridis::scale_fill_viridis(
    discrete = TRUE,
    begin = 9/10,
    end = 3/10, 
    name = "Amount of time",
    breaks = c("more", "less"),
    labels = c("More", "Less")
  ) + 
  # scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # Percentage labels rounded to the nearest integer
  scale_y_continuous(labels = function(x) scales::percent(abs(x))) + # Percentage labels rounded to the nearest integer	
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Females reported change in time use after her households started receiving LPG",
    x = "Activity",
    y = "Percent of intervention households reporting change \n(n = 475)"
  ) # + 
# facet_wrap(~ more_less)

fig_time_respondent_more_less

ggsave(
  here::here("6_figures", "fig_time_respondent_more_less.png"),
  plot = last_plot(),
  scale = 1,
  height = 6,
  width = 10,
  units = "in",
  device = "png"
)

# # Skip logic: selected(${fuel_ever_receive_lpg},'1') and (selected(${collect_wood_g_ever},'1') or selected(${collect_wood_b_ever},'1')) and (selected(${study_arm},'3') or selected(${study_arm},'5')) 
# survey_data %>% 
# 	filter(study_arm %in%  c("post-intervention", "intervention follow-up")) %>%
# 	select(study_arm, fcn_id, fuel_ever_receive_lpg, collect_wood_g_ever, collect_wood_b_ever, starts_with("time_child"))




##########################################################################
# Change in children's time
##########################################################################

time_child_more_less <-
  survey_data %>%
  # At baseline, comparison hh had already not used fuel for a year, 
  # so use only the results from the intervention group at midline, who will have better recall
  filter(study_arm_overall == "intervention" & timepoint == "midline") %>%
  #filter(study_arm_overall %in%  c("intervention")) %>%
  select(fcn_id, timepoint, study_arm_overall, starts_with("time_child")) %>% 
  # filter(!is.na(time_child_cooking)) %>% # this question was only answered if girls or boys were reported to collect firewood
  # count() # There were only 38 respondents of 597 hh that answered the intervention survey
  gather(-fcn_id, -timepoint, -study_arm_overall, key = "activity", value = "more_less") %>%
  filter(!is.na(more_less)) %>% # There are still some hh with na, because they reported not recently harvesting wood? 
  group_by(activity) %>%
  count(more_less) %>%
  mutate(
    pc = n / sum (n) # sum(n) = 99 because this many hh answered these questions
  ) %>% # make the less ones negative
  ungroup() %>%
  # necessary to apply prop.test to each row
  rowwise() %>%
  mutate(
    lower = ifelse(n > 0, prop.test(n, n/pc)$conf.int[1], 88),
    upper = ifelse(n > 0, prop.test(n, n/pc)$conf.int[2], NA)
  ) %>%
  mutate(
    # Change sign to negative if the value was less
    pc = ifelse(more_less == 3, pc * (-1), pc),
    lower = ifelse(more_less == 3, lower * (-1), lower),
    upper = ifelse(more_less == 3, upper * (-1), upper),
    more_less = factor(more_less, levels = c(1, 2, 3), labels = c("more", "same", "less"))
  )

write.csv(time_child_more_less, file_out_2)

#### Make fig ####

fig_time_child_more_less <-
  time_child_more_less %>%
  filter(more_less != "same") %>%
  # We didn't ask about children working, but should have as we see in the IDIs this was the case
  mutate(
    activity = 
      ordered(
        activity, 
        levels = 
          c(
            "time_child_harvesting_wood",
            "time_child_gathering_nonwood_items", 
            "time_child_cooking", 
            "time_child_cleaning",
            "time_child_collecting_water",
            "time_child_school", 
            "time_child_nothing",
            "time_child_socializing",
            "time_child_sleeping"
            
          ),
        labels = 
          c(
            "Harvesting wood",
            "Gathering non-wood items",
            "Cooking", 
            "Cleaning",  
            "Collecting water",
            "Going to school", 
            "Doing nothing",
            "Socializing", 
            "Sleeping"
          )
      )
  ) %>%
  ggplot(aes(x = activity, y = pc, fill = more_less)) +
  geom_col() +
  geom_linerange(aes(ymin = lower, ymax = upper)) + # , position = position_dodge(0.9)
  viridis::scale_fill_viridis(
    discrete = TRUE,
    begin = 9/10,
    end = 3/10, 
    name = "Amount of time",
    breaks = c("more", "less"),
    labels = c("More", "Less")
  ) + 
  # scale_y_continuous(labels = function(x) scales::percent_format(abs(x), accuracy = 1)) + # Percentage labels rounded to the nearest integer
  scale_y_continuous(labels = function(x) scales::percent(abs(x))) + # Percentage labels rounded to the nearest integer
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Caregiver-reported change in time use among \nchildren who used to collect firewood \nafter their households started receiving LPG",
    x = "Activity",
    y = "Percent of households reporting change \n(n = 99)"
  )

fig_time_child_more_less

ggsave(
  here::here("6_figures", "fig_time_child_more_less.png"),
  plot = last_plot(),
  scale = 1,
  height = 6,
  width = 10,
  units = "in",
  device = "png"
)

# # Skip logic: selected(${fuel_ever_receive_lpg},'1') and (selected(${collect_wood_g_ever},'1') or selected(${collect_wood_b_ever},'1')) and (selected(${study_arm_overa},'3') or selected(${study_arm_overa},'5')) 
# survey_data %>% 
# 	filter(study_arm %in%  c("post-intervention", "intervention follow-up")) %>%
# 	select(study_arm, fcn_id, fuel_ever_receive_lpg, collect_wood_g_ever, collect_wood_b_ever, starts_with("time_child"))
