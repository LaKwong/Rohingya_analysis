################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Coping strategies
# @Date: 210309
################################################################################
rm(list = ls())
#install.packages("here")
source(here::here("0_config.R"))
## Chris
# source(here::here("1_config.R"))

source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))
source(here::here("5_analysis/Dif_in_dif_fxn.R"))

# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")
#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)
model_data <- survey_data %>% filter(timepoint %in% c("baseline", "endline"))




# Trying to find the time between different surveys 
# survey_data %>% count(camp_id, timepoint) %>% view()
# 
# survey_data %>% select(timepoint, SubmissionDate, fcn_id) %>% pivot_wider(names_from = timepoint, values_from = SubmissionDate) %>% 
# 	select(baseline, midline, endline)

################################################################################
################################################################################

#Overall hh LPG lasting 

### Food Cant afford Analysis
survey_data %>% group_by(study_arm_overall, timepoint) %>% count(food_cant_afford_2wk) %>% pivot_wider(names_from = study_arm_overall, values_from = n)

dind_fxn_binom("food_cant_afford_2wk", model_data)

#Intervention = 49%
#Comparusibn = 29%

#### Fuel cant afford Analysis 
survey_data %>% group_by(study_arm_overall, timepoint) %>% count(fuel_cant_afford_2wk) %>% pivot_wider(names_from = study_arm_overall, values_from = n)

dind_fxn("fuel_cant_afford_2wk", model_data)


survey_data %>% count(fuel_cant_afford_action) %>% mutate(
	prop = n / 225 *100)


# fuel_borrow_amount
# borrow_fuel
# reduce_fuel
# reduce_meals
# not_eat

# Coping necessity and strategies


survey_data %>%
	tabyl(study_arm, food_insufficient_nutrition) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

survey_data %>%
	tabyl(study_arm, food_didnt_want) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

survey_data %>%
	tabyl(study_arm, food_cant_afford_2wk) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

# What did you do?
# food_cant_afford_action
survey_data %>%
	tabyl(study_arm, food_cant_afford_action) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

# Most difficult 
# Rank by the percentage of hh that report the most difficult

# food_cant_afford_difficult
survey_data %>%
	tabyl(study_arm, food_cant_afford_difficult) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

# Of those that report having to do this action, the weekly frequency

# Least difficult
# Rank by the percentage of hh that report the least difficult

# food_cant_afford_easiest
survey_data %>%
	tabyl(study_arm, food_cant_afford_easiest) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

# Of those that report having to do this action, the weekly frequency

# fuel
survey_data %>%
	tabyl(study_arm, fuel_cant_afford_2wk) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

# What did you do?
    #This is what we used in RF105 

# Fuel_cant_afford_action
survey_data %>%
	tabyl(study_arm, fuel_cant_afford_action) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

## Vector of variables to summarize
# Use Hmisc::Cs to quote each variable
table16Vars <- 
	Hmisc::Cs(
		borrow_food, reduce_food, 
		reduce_meals_lack_food, not_eat_lack_food, restrict_food,
		borrow_fuel, reduce_fuel, 
		reduce_meals_lack_fuel, not_eat_lack_fuel
	)

## Vector of categorical variables that need transformation
table16FactorVars <- table16Vars

# Create a TableOne object
tab16 <- 
	CreateTableOne(
		data = survey_data, 
		vars = table16Vars, 
		factorVars = table16FactorVars, 
		strata = "food_cant_afford_2wk"
	)

tab16

## For those that could not afford foof in the prior two weeks 
survey_data %>% 
	filter(food_cant_afford_2wk == 1) %>% 
	count(food_cant_afford_action) %>% 
	mutate(percent = n / 706) %>% arrange(desc(percent))

dind_fxn("food_cant_afford_2wk",model_data)



	
######## Fuel inaffordability #####################

# Percentage of households that could not afford fuel in prior 2 weeks 
	survey_data %>% 
		group_by(study_arm_overall, timepoint) %>% 
		summarize(
			hh = n(),
			p = (sum(fuel_cant_afford_2wk, na.rm = TRUE)) / n(),
			margin = qnorm(0.975)*sqrt(p*(1-p)/hh), 
			low = p - margin, 
			high = p + margin)

dind_fxn("fuel_cant_afford_2wk",model_data)

# Fuel cant afford 
survey_data %>% 
		filter(fuel_cant_afford_2wk == 1, !is.na(fuel_cant_afford_action)) %>% 
	#count(timepoint, study_arm_overall, fuel_cant_afford_2wk)
count(fuel_cant_afford_action) %>% 
	mutate(
		hh = 223,
		p = n / hh, 
		margin = qnorm(0.975)*sqrt(p*(1-p)/hh),
		low = p - margin, 
		high = p + margin
	) %>%
	arrange(desc(p))



