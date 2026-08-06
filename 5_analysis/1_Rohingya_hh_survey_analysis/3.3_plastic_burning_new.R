################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Fuel sources analysis
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
source(here::here("5_analysis/Dif_in_dif_fxn.R"))
source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))
source(here::here("5_analysis/Dif_in_dif_fxn.R")) 

# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")

#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)


################################################################################
################################################################################


# Based on IDIs and FGDs, "other is likely plastic"

survey_data %>%
	tabyl(burn_plastic_frequency, study_arm_overall, timepoint)

freq_plastic <- 
  survey_data %>% 
  ## Unsure why this is used as filtering criteria
  # fuel_use_non_lpg_freq_cook: "How long before the next LPG refill does your household run out of LPG for cooking food. (Answer in meals or days – select the answer that best matches the response) "
  filter(!fuel_use_non_lpg_freq_cook > 200 | is.na(fuel_use_non_lpg_freq_cook)) %>%
  group_by(study_arm_overall, timepoint) %>% 
  mutate(
    burn_plastic_yn = if_else(burn_plastic_frequency > 0, 1, 0) # How often per week do you burn plastic?
  ) %>% 
  summarize(
    mean = mean(fuel_use_non_lpg_freq_cook,  na.rm = TRUE),    
    sd = sd(fuel_use_non_lpg_freq_cook, na.rm = TRUE),
    n = n(), 
    pct_out = sum(burn_plastic_yn, na.rm = TRUE)/n
  ) %>%
  mutate(
    se = sd/ sqrt(n),
    lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
    upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se, 
    lower.ci.prop = pct_out - 1.96 * sqrt((pct_out/(1-pct_out)/n)), 
    upper.ci.prop = pct_out + 1.96 * sqrt((pct_out/(1-pct_out)/n)),
  )


plastics <- 
	survey_data %>% 
	mutate(
		plastic_burn_yn = if_else(burn_plastic_frequency > 1, 1, 0)
	) %>% 
	filter(timepoint %in% c("baseline", "endline"))
	
dind_fxn(data = plastics, "plastic_burn_yn")

# $baseline
# burn_plastic_frequency comparison intervention
# 0        323          269
# 1          7           48
# 2          1           40
# 3          0           39
# 4          1           41
# 7          0            0
# NA          0           49
# 
# $midline
# burn_plastic_frequency comparison intervention
# 0        319          454
# 1          7           13
# 2          6           13
# 3          0            2
# 4          0            4
# 7          0            0
# 
# $endline
# burn_plastic_frequency comparison intervention
# 0        334          482
# 1          0            0
# 2          0            0
# 3          0            1
# 4          0            0
# 7          0            1

survey_data %>% 
	filter(timepoint == "endline") %>% 
	select(contains("plastic"))

survey_data %>% 
	group_by(fuel_30_receive_lpg) %>% 
	count(fire_consequence)

survey_data %>% 
	group_by(timepoint, study_arm_overall) %>% 
	count(stove_boil_drink)



# Almost 150 of 393 hh report burning households every day! We need to better understand how many meals and if they are relying on plastic for all of the fuel they need or if it is used in combination with other fuel sources

# 1. fuel_ever_plastic 
# 0. How often per week do you burn plastic? --> Need a recall period?
# 	2. When you burned plastic, was the only fuel you used plastic or did you combine plastic with other fuels?
# 	3. Why did you burn plastic
# Select_multiple
# a. We can't access other fuels
# b. If we burn plastic, we don't have to spend money buying other fuels 
# c. Burning plastic is a good way to get rid of trash
# d. Other
# Specify other
# 
# Add questions about burning plastic to the focus group discussions - doesn't quite fit with any of the FGDs, but add to distribution and training. 
