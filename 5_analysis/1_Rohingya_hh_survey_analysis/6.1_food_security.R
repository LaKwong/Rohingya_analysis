################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Food security analysis
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))


library(mclogit)

## Chris
# source(here::here("1_config.R"))
#source(here::here("5_analysis/Dif_in_dif_fxn.R"))
source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))

# Parameters
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")
#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)

################################################################################
################################################################################

food_dataset <-
	survey_data %>%
	select(
		study_arm_overall,
		timepoint,
		fcn_id, 
		camp_id,
		block_id,
		subblock_id,
		hh_size,
		contains("food"), 
		contains("rice"),
		contains("bread"),
		contains("corn"),
		contains("potatoes"),
		contains("lentils"),
		contains("dairy"),
		contains("veggies"),
		contains("fruit"),
		contains("eggs"),
		contains("fish"),
		contains("poultry"),
		contains("goat_sheep"),
		contains("beef"),
		contains("oil"),
		contains("sugar")
		
	) %>%
	select(
		-c(
			contains("corner"),
			contains("boil")
		)
	)

saveRDS(food_dataset, here::here("4_data/food_dataset.rds"))

#####################################################################################
#######################################################################################

# Food consumption patterns

# source: WFP_Comprehensive food security and vulnerability analysis
# p. 100
# The main indicators emanating from the analysis of these data are: 
# 	(a) number of days out of seven that items and food groups are consumed; 
# (b) household Food Consumption Score; and
# (c) percentage contribution of the sources to the household food basket over the
# previous seven days.
# 
# 
# If also ask about consumption in the past 24 hours: Household Dietary Diversity Score (HDDS)
# 
# p. 101
# The Food Consumption Score (FCS). Information is collected from a country-specific list of food
# items and food groups. The household is asked about the number of times (in days) a given
# food item was consumed over a recall period of the past seven days. Items are grouped into
# eight standard food groups (each group has a maximum value of seven days/week). The
# consumption frequency of each food group is multiplied by an assigned weight based on the
# nutrient content of a portion. Those values are then summed to obtain the FCS. The FCS has
# a theoretical range from 0 to 112; WFP has defined thresholds (WFP 2007) to convert the
# continuous FCS into categories creating three food consumption groups (FCGs): poor,
# borderline, and acceptable.
# 
# The Household Dietary Diversity Score (HDDS). A standard list of 16 food groups, the same for
# any country/context, is used to gather information on food consumed in the past 24 hours.
# Information for each group is of a bivariate type (yes/no). To calculate the HDDS, the 16 food
# groups are aggregated into 12 main groups. All food groups have the same importance (relative
# weights equal to 1), with each group consumed providing 1 point. The HDDS is the simple sum
# of the number of consumed food groups (it goes theoretically from 0 to 12). For analytical
# purposes, the HDDS is often ranked into thirds or quartiles.
# Both the FCS and HDDS are used as proxy indicators of household access to food. Data collected
# for both indicators can also be used to consider dietary patterns and the consumption of specific
# foods. The FCS and HDDS are used for monitoring economic access to food and surveillance at
# decentralized levels; moreover, the FCS is used for classifying households who are food insecure,
# while the HDDS is used for monitoring dietary quality.
# 
# source: WFP_2008_Food consumption score analysis guideline
# Food consumption score 
# weights: 
# 	staples = 2
# pulses = 3
# veggies = 1
# fruit = 1
# meat and fish = 4
# milk = 4
# sugar = 0.5
# oil = 0.5
# condiments = 0
# 
# cutoffs
# FCS 0 to 21 = poor
# FCS 21.5 to 35 = borderline
# FCS >35 = acceptable
# 
# In the past 7 days, how many days did you eat each of the following foods?

## Vector of variables to summarize
# Use Hmisc::Cs to quote each variable

FCS_HDDS_data <-
	survey_data %>%
	select(
		study_arm_overall, timepoint, fcn_id, camp_id, block_id, subblock_id, SubmissionDate,
		all_of(food_types_consumed_vars), contains("food_consump_adult_day")
	) %>%
	
	
	rowwise() %>%
	mutate(
		staple = 
			ifelse(
				sum(rice_adults_week, bread_adults_week, corn_adults_week, potatoes_adults_week, na.rm = TRUE) >= 7, 
				7, 
				sum(rice_adults_week, bread_adults_week, corn_adults_week, potatoes_adults_week, na.rm = TRUE)
			),
		pulses = 
			ifelse(
				lentils_adults_week >= 7, 
				7, 
				lentils_adults_week
			),
		veggies = 
			ifelse(
				veggies_adults_week >= 7, 
				7, 
				veggies_adults_week
			),
		fruit = 
			ifelse(
				fruit_adults_week >= 7, 
				7, 
				fruit_adults_week
			),
		meat_fish = 
			ifelse(
				sum(eggs_adults_week, fish_adults_week, poultry_adults_week, goat_sheep_adults_week, beef_adults_week, na.rm = TRUE) >= 7, 
				7, 
				sum(eggs_adults_week, fish_adults_week, poultry_adults_week, goat_sheep_adults_week, beef_adults_week, na.rm = TRUE)
			),
		dairy = 
			ifelse(
				dairy_adults_week >= 7, 
				7, 
				dairy_adults_week
			),
		sugar = 
			ifelse(
				sugar_adults_week >= 7, 
				7, 
				sugar_adults_week
			),
		oil = 
			ifelse(
				oil_adults_week >= 7, 
				7, 
				oil_adults_week
			),
		# did not ask about condiments
		
		# For the household dietary diversity score (HDDS), need  yes/no for each food group consumed in the past 24 hours
		
		# [1] "food_consump_adult_day"    "food_consump_adult_day/1"  "food_consump_adult_day/2"  "food_consump_adult_day/3"  "food_consump_adult_day/4" 
		# [6] "food_consump_adult_day/5"  "food_consump_adult_day/6"  "food_consump_adult_day/7"  "food_consump_adult_day/8"  "food_consump_adult_day/9" 
		# [11] "food_consump_adult_day/10" "food_consump_adult_day/11" "food_consump_adult_day/12" "food_consump_adult_day/13" "food_consump_adult_day/14"
		# [16] "food_consump_adult_day/15"
		# [food_groups]	1	rice
		# [food_groups]	2	bread
		# [food_groups]	3	corn
		# [food_groups]	4	potatoes
		# [food_groups]	5	lentils
		# [food_groups]	6	eggs
		# [food_groups]	7	dairy
		# [food_groups]	8	vegetables
		# [food_groups]	9	fruit
		# [food_groups]	10	fish
		# [food_groups]	11	poultry
		# [food_groups]	12	goat
		# [food_groups]	13	beef
		# [food_groups]	14	soil
		# [food_groups]	15	sugar
		
		# survey_data %>% select(contains("food_consump_adult_day")) %>% View()
		
		# These columns aren't found for some reason - FIX THIS #################3
		
		cereals_yn = ifelse(sum(food_consump_adult_day.1, food_consump_adult_day.2, food_consump_adult_day.3, na.rm = TRUE) > 0, 1, 0),
		tubers_yn = ifelse(food_consump_adult_day.4 > 0, 1, 0),
		veggies_yn = ifelse(food_consump_adult_day.8 > 0, 1, 0),
		fruit_yn = ifelse(food_consump_adult_day.9 > 0, 1, 0),
		meat_yn = ifelse(sum(food_consump_adult_day.11, food_consump_adult_day.12, food_consump_adult_day.13, na.rm = TRUE) > 0, 1, 0),
		eggs_yn = ifelse(food_consump_adult_day.6 > 0, 1, 0),
		fish_yn = ifelse(food_consump_adult_day.10 > 0, 1, 0),
		pulses_yn = ifelse(food_consump_adult_day.5 > 0, 1, 0),
		milk_yn = ifelse(food_consump_adult_day.7 > 0, 1, 0),
		oil_yn = ifelse(food_consump_adult_day.14 > 0, 1, 0),
		sugar_yn = ifelse(food_consump_adult_day.15 > 0, 1, 0),
		# did not ask about miscellaneous
		
		hdds_no_misc = sum(cereals_yn, tubers_yn, veggies_yn, fruit_yn, meat_yn, eggs_yn, fish_yn, pulses_yn, milk_yn, oil_yn, sugar_yn),
		hdds_assume_misc_1 = hdds_no_misc + 1,
		
		staple_weight = staple * 2,
		pulses_weight = pulses * 3,
		veggies_weight = veggies * 1,
		fruit_weight = fruit * 1, 
		meat_fish_weight = meat_fish * 4,
		milk_weight = dairy * 4,
		sugar_weight = sugar * 0.5, 
		oil_weight = oil * 0.5,
		# did not ask about condiments
		
		
		fcs = sum(staple_weight, pulses_weight, veggies_weight, fruit_weight, meat_fish_weight, milk_weight, sugar_weight, oil_weight),
		
		fcs_category = 
			case_when(
				fcs <= 21 ~ "poor",
				fcs >= 21.5 & fcs <= 35 ~ "borderline",
				fcs > 35 ~ "acceptable"
			),
		
		fcs_category = ordered(fcs_category, levels = c("poor", "borderline", "acceptable"))
	) %>%
	ungroup()


write_rds(FCS_HDDS_data, here::here("4_data/FCS_HDDS_data.rds"))

# FCS numeric
FCS_HDDS_data %>% 
  group_by(timepoint, study_arm_overall) %>% 
  summarise(
    n(),
    mean(fcs)
  )

# FCS by category
FCS_HDDS_data %>%
	tabyl(fcs_category, study_arm_overall, timepoint) %>%
	adorn_percentages("col") %>%
	adorn_pct_formatting(digits = 1) %>%
	adorn_ns()

# There are no hh in the poor category, so ignore this an analyze if there is a sig diff between hh in the borderline to acceptable category from baseline to endline 

# $baseline
# fcs_category  comparison intervention
# poor  0.0%   (0)   0.2%   (1)
# borderline  4.2%  (14)   7.4%  (36)
# acceptable 95.8% (318)  92.4% (449)
# 
# $midline
# fcs_category  comparison intervention
# poor  0.0%   (0)   0.0%   (0)
# borderline  1.8%   (6)   5.6%  (27)
# acceptable 98.2% (326)  94.4% (459)
# 
# $endline
# fcs_category  comparison intervention
# poor  0.0%   (0)   0.0%   (0)
# borderline  0.3%   (1)   0.4%   (2)
# acceptable 99.7% (333)  99.6% (482)

# Find difference in difference of FCS category between baseline and endline

fcs_model <- 
  FCS_HDDS_data %>% 
	filter(timepoint %in% c("baseline", "endline")) %>% 
	mutate(
		timepoint_num = if_else(timepoint == "endline", 1, 0),
		fcs_binary = if_else(fcs_category == "acceptable", 1, 0),
		study_arm_overall_num = if_else(as.character(study_arm_overall) == "intervention", 1, 0)) %>%
  select(study_arm_overall, timepoint, fcn_id, fcs, fcs_category, fcs_binary)


# dind_fxn("fcs_binary",  fcs_model) # This is Chris's function

fcs_binary_mod <- glmer(fcs_binary ~ study_arm_overall + timepoint + study_arm_overall * timepoint + (1 | fcn_id), family = binomial(link = "logit"), data = fcs_model)
summary(fcs_binary_mod)
# Fixed effects:
#                                Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                       4.1773     0.2624  15.920  < 2e-16 ***
#   study_arm_overall.L              -0.2577     0.3711  -0.695    0.487    
# timepoint.L                       1.7910     0.3711   4.826 1.39e-06 ***
#   study_arm_overall.L:timepoint.L   0.4876     0.5247   0.929    0.353 

# There is NO significant difference in binary FCS category between baseline and endline




# Difference in FCS category between groups from midline to endline
# Simple linear regression because the the data is continuous
fcs_mod <- mclogit(fcs_category ~ study_arm_overall + timepoint + study_arm_overall * timepoint + (1 | fcn_id), data = FCS_HDDS_data %>% filter(timepoint %in% c("baseline", "endline")))
summary(fcs_mod)
#                                 Estimate Std. Error t value
# study_arm_overall.L:timepoint.L  1.9801     0.5031   3.936
# p_value is 2 * (1 - pnorm(abs(3.936))) # 8.285097e-05
# NOT significant difference between intervention and comparison group between midline and endline




# Difference in FCS (numeric) between groups from midline to endline
# Simple linear regression because the the data is continuous
fcs_num_mod <- lmer(fcs ~ study_arm_overall + timepoint + study_arm_overall * timepoint + (1 | fcn_id), data = FCS_HDDS_data %>% filter(timepoint %in% c("baseline", "endline")))
# boundary (singular) fit: see help('isSingular')
summary(fcs_num_mod)
#                                 Estimate Std. Error t value
# study_arm_overall.L:timepoint.L  1.9801     0.5031   3.936
# p_value is 2 * (1 - pnorm(abs(3.936))) # 8.285097e-05
# NOT significant difference between intervention and comparison group between midline and endline







# HDDS score

# we did not ask for food consumption in the past 24 hours at baseline. Bummer
FCS_HDDS_data %>%
	group_by(timepoint, study_arm_overall) %>%
	summarise(
		hdds_assume_misc_1_mean = mean(hdds_assume_misc_1, na.rm = TRUE), hdds_assume_misc_1_sd = sd(hdds_assume_misc_1, na.rm = TRUE)
	)
# timepoint study_arm_overall hdds_assume_misc_1_mean hdds_assume_misc_1_sd
# <ord>     <ord>                               <dbl>                 <dbl>
#   1 baseline  comparison                         NaN                    NA   
# 2 baseline  intervention                       NaN                    NA   
# 3 midline   comparison                           5.99                  1.16
# 4 midline   intervention                         5.71                  1.30
# 5 endline   comparison                           6.90                  1.62
# 6 endline   intervention                         6.81                  1.46

# Is there a sig diff between the groups at midline?
hdds_midline_mod <- lm(hdds_assume_misc_1 ~ study_arm_overall, data = FCS_HDDS_data %>% filter(timepoint == "midline"))
summary(hdds_midline_mod)
# There IS a SIGNIFICANT difference in HDDS between groups at midline

# Is there a sig diff between the groups at endline?
hdds_endline_mod <- lm(hdds_assume_misc_1 ~ study_arm_overall, data = FCS_HDDS_data %>% filter(timepoint == "endline"))
summary(hdds_endline_mod)
# There is NOT a significant difference in HDDS between groups at endline


# Do the following but I don't expect a difference: Difference between groups from midline to endline
# Simple linear regression because the the data is continuous
hdds_mod <- lmer(hdds_assume_misc_1 ~ study_arm_overall + timepoint + study_arm_overall * timepoint + (1 | fcn_id), data = FCS_HDDS_data)
# boundary (singular) fit: see help('isSingular')
summary(hdds_mod)
#                                 Estimate Std. Error t value
# study_arm_overall.L:timepoint.L  0.09281    0.06482   1.432
# p_value is 2 * (1 - pnorm(abs(1.432))) # 0.1521438
# NOT significant difference between intervention and comparison group between midline and endline







fig_FCS_HDDS_data <-
	FCS_HDDS_data %>%
	select(timepoint, study_arm_overall, fcn_id, hdds_assume_misc_1, fcs) %>%
	#gather(-timepoint, -study_arm_overall, -fcn_id, key = "category", value = "score") %>%
	ggplot(aes(x = fcs, fill = study_arm_overall)) +
  
  # annotate("text", x = 18, y = 160, label = "poor") +
  # annotate("text", x = 28, y = 160, label = "borderline") +
  # annotate("text", x = 43, y = 160, label = "acceptable") +
  
	# geom_histogram() +  # gives count value
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = 35, linetype = "dashed") +
	geom_vline(xintercept = 21.5, linetype = "dashed") +
	theme_bw() + 
	scale_fill_manual(
		name = "Study_arm",
		breaks = c("intervention", "comparison"),
		# labels = c("Pre-intervention", "Intervention", "Outdoor"),
		labels = c("Intervention", "Comparison"),
		values = c("#138b87",  "#430154"),
		# values = c("#7570b3",  "#d95f02", "#1b9e77")
	) +
  labs(x = "Food consumption score", y = "percentage of households") + 
	facet_grid(timepoint ~ ., scale = "fixed") 
	
fig_FCS_HDDS_data



ggsave(
	here::here("6_figures", "fig_FCS_HDDS_data.tiff"),
	plot = last_plot(),
	scale = 1,
	height = 6,
	width = 10,
	units = "in",
	device = "tiff"
)
# 
# fig_FCS_HDDS_data <-
# 	FCS_HDDS_data_change_long %>%
# 	ggplot(aes(x = "", y = change)) + # , fill = org x = category,
# 	geom_violin(aes(fill = study_arm_overall), position = "dodge") +
# 	# Mean_sdl adds the mean and sd to the plot
# 	stat_summary(fun.data = mean_sdl, geom = "pointrange", aes(group = study_arm_overall), position = position_dodge(0.9), color="red") +
# 	# try to add an arrow outside the left axis to show that higher values are more at risk for depression)
# 	# 	geom_segment(aes(x = -0.1, xend = -0.1, y= 30, yend= 35),
# 	#                          arrow = arrow(length = unit(0.2,"cm"))) +
# 	#         geom_text(data = arrowLab, aes(x = x, y = y,label = lab), size = 3) +
# 	ggpubr::stat_compare_means(
# 		label = "p.signif", #"p.signif" # symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
# 		method = "t.test", # distributions are pretty normal
# 		
# 		
# 		# --> Need to actually test normality
# 		
# 		paired = FALSE, # baseline and endline are paired but the change is not, and on-going and intervention are not
# 		# label.y = 18,
# 		hide.ns = TRUE#, The baseline and endline samples are paired but the change is not
# 	) +
# 	# ggpubr::stat_compare_means(label.y = 50) +
# 	viridis::scale_fill_viridis(
# 		discrete = TRUE,
# 		end = 2/3
# 	) +
# 	theme_classic() +
# 	theme(
# 		# axis.text.x = element_blank(), # angle = 90
# 		# axis.ticks.x = element_blank(),
# 		legend.position = "bottom",
# 		legend.title = element_blank()
# 	) +
# 	guides(fill = guide_legend(nrow = 1, label.position = "bottom")) + # , hjust = -40
# 	labs(
# 		title = "Change in Food Security Scores",
# 		x = "",  #"Study timepoint",
# 		y = "Score"
# 	) + 
# 	facet_wrap(~ category, scale = "free_y")
# # coord_cartesian(xlim = c(0, 1), clip = "off")
# 
# fig_FCS_HDDS_data
# 
# ggsave(
# 	here::here("6_figures", "fig_FCS_HDDS_data.tiff"),
# 	plot = last_plot(),
# 	scale = 1,
# 	height = 6,
# 	width = 10,
# 	units = "in",
# 	device = "tiff"
# )




#Hmisc::Cs() will quote everything within the paranthesis 




# Potatoes and lentils higher in pre-intervention
# Veggies and oil higher and fish substantially higher (2.81 vs 3.42) in intervention
# Number of food groups is significantly higher in intervention hh (5.65 vs 5.78)
# 
# beef is high among some pre-intervention hh because in the previous week WFP held a WFP Day and distributed beef to everyone in the camp. Did they also distribute potatoes on this day?

################################################################################
# Food source
#################################################################################
	
# What % of people reply that they receive this food from aid? from the market?...what do we want to see? That people are getting more from the market? ...this could be because they are saving money since they don't have to purchase fuel

# Check what is in the WPF guides.

# [food_source]	0	Don't eat / don't have
# [food_source]	1	Ration card / food aid
# [food_source]	2	Purchased # Not sure the difference from "Purchased with wages"
# [food_source]	3	Purchased with wages # Not sure the difference from "Purchased"
# [food_source]	4	Gift from relative or friend
# [food_source]	5	Borrow from relative or friend
# [food_source]	6	Own production (own garden or animals)
# [food_source]	7	Exchange labour for food
# [food_source]	8	Exchange items for food
# [food_source]	9	Gather, hunt, fish
# [food_source] 66 Other

# Why is almost all rice purchased with wages? Not food aid? Some also report that they gather it?


## Vector of variables to summarize
# Use Hmisc::Cs to quote each variable
table15Vars <- food_source_all

# table15Vars <- 
# 	Hmisc::Cs(
# 		rice_source,
# 		bread_source,
# 		corn_source,
# 		potatoes_source,
# 		lentils_source, 
# 		dairy_source,
# 		veggies_source,
# 		fruit_source,
# 		fish_source,
# 		poultry_source,
# 		goat_sheep_source,
# 		beef_source,
# 		oil_source,
# 		sugar_source
# 	)

## Vector of categorical variables that need transformation
# table15FactorVars <- table15Vars

# Create a TableOne object
tab15 <- 
	CreateTableOne(
		data = survey_data %>% filter(timepoint == "baseline"), 
		vars = table15Vars, 
		# factorVars = table15FactorVars, 
		strata = "study_arm_overall"
	)

tab15

# veggies_source6, fish_source1, fish_source2 are sig higher in intervention, veggies_source1 (0.01 vs 0.07), fish_source1 sub higher (0.05 vs 0.12), beef_source2 (0.15 vs 0.21), oil_source1 (0.84 vs 0.96), -> intervention are receiving more fish through aid distribution, but purchasing beef (beef_source1 0.15 in pre-intervention compared to 0.01 in intervention due to WFP distribution day), 




###############################################################################
# Coping necessity and strategies
###############################################################################

# NEED TO FIX THIS 

survey_data %>%
	tabyl(timepoint, study_arm_overall, food_insufficient_nutrition) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 1) %>%
	adorn_ns()

survey_data %>%
	tabyl(timepoint, study_arm_overall, food_didnt_want) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

survey_data %>%
	tabyl(timepoint, study_arm_overall, food_cant_afford_2wk) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

# What did you do?
# food_cant_afford_action
survey_data %>%
	tabyl(timepoint, study_arm_overall, food_cant_afford_action) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

# Most difficult 
# Rank by the percentage of hh that report the most difficult

# food_cant_afford_difficult
survey_data %>%
	tabyl(timepoint, study_arm_overall, food_cant_afford_difficult) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

# Of those that report having to do this action, the weekly frequency

# Least difficult
# Rank by the percentage of hh that report the least difficult

# food_cant_afford_easiest
survey_data %>%
	tabyl(timepoint, study_arm_overall, food_cant_afford_easiest) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

# Of those that report having to do this action, the weekly frequency

# fuel
survey_data %>%
	tabyl(timepoint, study_arm_overall, fuel_cant_afford_2wk) %>%
	adorn_percentages("row") %>%
	adorn_pct_formatting(digits = 2) %>%
	adorn_ns()

# What did you do?
# food_cant_afford_action
survey_data %>%
	tabyl(timepoint, study_arm_overall, fuel_cant_afford_action) %>%
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
		strata = "study_arm_overall"
	)

tab16

survey_data %>% select(all_of(MUAC_vars)) %>% count(target_child_arm_measurements_yn)

