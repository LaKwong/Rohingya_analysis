################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Coping strategies
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
## Chris
# source(here::here("1_config.R"))

source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))

# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")
#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)


################################################################################
################################################################################










# NEED TO FIX THIS 


















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
# food_cant_afford_action
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
		strata = "study_arm"
	)

tab16



