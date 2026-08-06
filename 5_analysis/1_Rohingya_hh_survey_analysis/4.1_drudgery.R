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

#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)


###########################################################################
###### Drugery
##########################################################################

## This section has not been finisehd because there are some fnc_ids that have been entered twice, which is preventing an effective "spread" 


# 1	Cooking
# 2	Washing dishes
# 3	Washing clothes
# 4	Collecting water (including time traveling to the water pump and time acquiring water)
# 5	Harvesting wood from the forest
# 6	Caring for children (washing, dressing, bathing)
# 7	Caring for the sick, disabled, or elderly (washing, dressing, bathing)
# 88	NA/Nothing is difficult


survey_data %>%
  # filter(study_arm %in% c("post-intervention", "intervention follow-up")) %>%
  select(enumerator, start_date, fcn_id, timepoint, study_arm_overall, contains("drudgery")) %>%
  arrange(timepoint, study_arm_overall, enumerator, start_date)



# In baseline, ,any hh have drudgery_most_diff == drudgery_second_most_diff == drudgery_easiest. Training enumerators that they should not be the same thing was an oversight. 
# There are also many NAs, because the question wasn't required- but why did they not just answer? I don't see any skip logic on it. 
# could drop baseline results by using filter(study_arm %in% c("post-intervention", "intervention follow-up")) %>%
# instead, filter out the bad baseline results and keep what I can
drudgery_data_analyzable <-
  survey_data %>%
  select(enumerator, start_date, fcn_id, timepoint, study_arm_overall, contains("drudgery")) %>%
  rowwise() %>%
  mutate(
    drudgery_most_second_most_same = ifelse(drudgery_most_diff == drudgery_second_most_diff, 1, 0),
    drudgery_most_easiest_same = ifelse(drudgery_most_diff == drudgery_easiest, 1, 0),
    drudgery_second_most_easiest_same = ifelse(drudgery_second_most_diff == drudgery_easiest, 1, 0),
    drudgery_categories_same = ifelse((drudgery_most_second_most_same == 1) | (drudgery_most_easiest_same == 1) | (drudgery_second_most_easiest_same == 1), 1, 0 )
  ) %>%
  filter(
    drudgery_categories_same == 0
  ) %>%
  select(fcn_id, timepoint, study_arm_overall, drudgery_most_diff, drudgery_second_most_diff, drudgery_easiest) %>%
  gather(-fcn_id, -timepoint, -study_arm_overall, key = "drudgery_level", value = "activity") %>%
  mutate(
    activity_label = 
      factor(
        activity,
        level = c(1, 2, 3, 4, 5, 6, 7, 88),
        label = c(
          "Cooking",
          "Washing dishes",
          "Washing clothes",
          "Collecting water",
          "Harvesting wood",
          "Caring for children",
          "Caring for others",
          "Nothing is difficult"
        )
      )
  )

drudgery_pre_intervention <-
  drudgery_data_analyzable %>%
  select(-activity) %>%
  filter(study_arm_overall == "intervention", timepoint == "baseline") %>%
  spread(-fcn_id, key = "drudgery_level")
# pivot_wider(names_from = "drudgery_level", values_from = "activity_label") # -fcn_id, -study_arm, 

drudgery_post_intervention <-
  drudgery_data_analyzable %>%
  select(-activity) %>%
  filter(study_arm_overall == "intervention", timepoint == "midline")

drudgery_pre_intervention %>%
  left_join(drudgery_pre_intervention %>% select(-study_arm), by = c("fcn_id"), suffix = c(".baseline", ".midline") )


# drudgery_data %>%
# 	ggplot(aes(x = activity_label,  y = )) +
# 	geom_col()



