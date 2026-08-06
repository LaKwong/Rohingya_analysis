################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Forest use
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


# Forest use

## vegetation_dead when collected for fuel?
# 1	Yes, the scraps/wood I collect are all dead
# 2	I collect some scraps/wood that are dead and I gather scraps/leaves/twigs while they are still alive
# 3	I cut down live plants for the twigs/wood I need

survey_data %>%
	select(gather_scraps_dead) %>%
	table()

survey_data %>%
	select(gather_wood_dead) %>%
	table()


## Wood for reasons other than cooking
"Have you or your family ever collected firewood for purposes other than your own household's cooking needs (cooking food to sell, etc)?"


survey_data %>%
	select(gather_wood) %>%
	table()

# 16 report gathering wood for reasons other than their own household's cooking - how does this compare to the number that cook to sell (1?), the number that cook to stay warm (?) + the number that cook for non-household member (guessing these people didn't include themselves)

################################################################################
################################################################################

## Forest for reasons other than wood

"Have you or your family ever gone to the forest to take anything other than wood (food, household materials, etc)?"

survey_data %>%
	select(forest_collect_not_wood) %>%
	table()


## Vector of variables to summarize
table10Vars <- 
	c(
		"reason_forest_food", "reason_forest_med", "reason_forest_shelter",
		"reason_forest_privacy", "reason_forest_defecation", "reason_forest_leisure",
		"reason_forest_other", "reason_forest_other_specified"
	)

## Vector of categorical variables that need transformation
table10FactorVars <- table10Vars

# Create a TableOne object
tab10 <- 
	CreateTableOne(
		data = survey_data, 
		vars = table10Vars, 
		factorVars = table10FactorVars, 
		strata = "study_arm"
	)

tab10

survey_data %>%
	ggplot(aes(x = cost_forest_not_wood)) +
	geom_histogram() + 
	geom_vline(aes(xintercept = mean(cost_forest_not_wood, na.rm = TRUE), col = 'red'), size = 2, show.legend = TRUE) + 
	geom_vline(aes(xintercept = median(cost_forest_not_wood, na.rm = TRUE), col = 'blue'), size = 2, show.legend = TRUE) +
	scale_colour_manual("Legend", values = c(red = "red", blue = "blue"), labels = c("mean", "median"))

survey_data %>%
	summarise_at(vars(cost_forest_not_wood), list(mean), na.rm = TRUE)

survey_data %>%
	filter(forest_collect_not_wood == 1) %>%
	select(cost_forest_not_wood, starts_with("reason_forest")) %>%
	arrange(desc(cost_forest_not_wood))



# 
# 14% of 36 people replied that they go to the forest for "other" reasons but no other reasons are specified - why?...becuase on Q487 cost_forest_not_wood the relevance is wrong...selected({reason_forest},'66') should have been selected({reason_forest_other},'1')...oops
# 
# --> ask about this in the fgd?
# 	
# 	Three hh report paying <100 BDT to go into the forest for non-wood. This is ever more than was paid to collect firewood. Interestingly, the one hh that paid 300 BDT was the only hh that went to the forest for privacy (and they recorded privacy as the only reason for going)

