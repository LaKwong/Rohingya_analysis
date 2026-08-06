################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Fuel sources analysis
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
	filter(study_arm_overall %in%  c("comparison", "intervention")) %>%
	select(fcn_id, timepoint, study_arm_overall, starts_with("time_")) %>% 
	select(!contains("_child_")) %>%
	# filter(!is.na(time_child_cooking)) %>% # this question was only answered if girls or boys were reported to collect firewood
	# count() # There were only 38 respondents of 597 hh that answered the intervention survey
	gather(-fcn_id, -timepoint, -study_arm_overall, key = "activity", value = "more_less") %>%
	filter(!is.na(more_less)) %>%
	group_by(activity) %>%
	count(more_less) %>%
	ungroup() %>%
	group_by(activity) %>%
	mutate(pc = n / sum (n)) %>%
	mutate(pc = ifelse(more_less == 3, pc * (-1), pc)) %>% # make the less ones negative
	select(-n) %>%
	ungroup() %>%
	filter(activity != "time_gathering_nonwood_items") %>%
	mutate(
		more_less = factor(more_less, levels = c(1, 2, 3), labels = c("more", "same", "less")), 
		activity = 
			ordered(
				activity, 
				levels = 
					c(
						"time_harvesting_wood",
						# "time_gathering_nonwood_items",
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
						# "Gathering non-wood items",
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
	) # %>%
# spread(more_less, pc)

time_respondent_all_combos <-
	expand.grid(
		activity = 
			c(						
				"Harvesting wood",
				# "Gathering non-wood items",
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
			), 
		more_less = c("more", "same", "less")
	) %>%
	mutate( # seems unnecessary but need because there is a mismatch in types for some reason
		activity = 
			ordered(
				activity, 
				levels = 
					c(
						"Harvesting wood",
						# "Gathering non-wood items",
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
	)

time_respondent_more_less_all_combos <-
	time_respondent_all_combos %>%
	left_join(time_respondent_more_less, by = c("activity", "more_less") )

fig_time_respondent_more_less <-
	time_respondent_more_less_all_combos %>%
	filter(more_less != "same") %>%
	ggplot(aes(x = activity, y = pc, fill = more_less)) +
	# geom_col(position = "dodge") + 
	stat_summary(fun = "mean", geom = "bar") +
	stat_summary(fun.data = "mean_cl_boot", geom = "linerange") + 
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
		title = "Reported change in time spent by female respondent after her households started receiving LPG",
		x = "Activity",
		y = "Percent of households reporting change \n(n = 544)"
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
	filter(study_arm_overall %in%  c("comparison", "intervention")) %>%
	select(fcn_id, timepoint, study_arm_overall, starts_with("time_child")) %>% # For now, don't evaluation time that is not time_child
	# filter(!is.na(time_child_cooking)) %>% # this question was only answered if girls or boys were reported to collect firewood
	# count() # There were only 38 respondents of 597 hh that answered the intervention survey
	gather(-fcn_id, -timepoint, -study_arm_overall, key = "activity", value = "more_less") %>%
	filter(!is.na(more_less)) %>%
	group_by(activity) %>%
	count(more_less) %>%
	ungroup() %>%
	group_by(activity) %>%
	mutate(pc = n / sum (n)) %>%
	mutate(pc = ifelse(more_less == 3, pc * (-1), pc)) %>%
	select(-n) %>%
	ungroup() %>%
	mutate(
		more_less = factor(more_less, levels = c(1, 2, 3), labels = c("more", "same", "less")), 
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
	) # %>%
# spread(more_less, pc)

time_child_all_combos <-
	expand.grid(
		activity = 
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
			), 
		more_less = c("more", "same", "less")
	) %>%
	mutate( # seems unnecessary but need because there is a mismatch in types for some reason
		activity = 
			ordered(
				activity, 
				levels = 
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
	)

time_child_more_less_all_combos <-
	time_child_all_combos %>%
	left_join(time_child_more_less, by = c("activity", "more_less") )

fig_time_child_more_less <-
	time_child_more_less_all_combos %>%
	filter(more_less != "same") %>%
	ggplot(aes(x = activity, y = pc, fill = more_less)) +
	# geom_col(position = "dodge") + 
	stat_summary(fun = "mean", geom = "bar") +
	stat_summary(fun.data = "mean_cl_boot", geom = "linerange") +
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
		title = "Reported change in time spent by children who used to collect firewood \nafter their households started receiving LPG",
		x = "Activity",
		y = "Percent of households reporting change \n(n = 113)"
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
	spread(-fcn_id, -study_arm, key = "drudgery_level")
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




##############################################################################
# Time to collect fuel
##############################################################################

# Combine all survey data
# - For pre- and post-intervention results, collect both so that we can compare recall reliability and then average before combining with intervention group. 

fig_collect_fuel_walk_wait_hr <-
	survey_data %>%
	filter(timepoint == "endline") %>% # forgot to gathre some data at bseline
	select(study_arm_overall, fcn_id, collect_wood_walk_hr, receive_lpg_walk, receive_lpg_wait, buy_lpg_walk, buy_lpg_wait, receive_crh_walk, receive_crh_wait) %>%
	rowwise() %>%
	mutate(
		receive_lpg_walk_wait = sum(receive_lpg_walk, receive_lpg_wait, na.rm = TRUE),
		buy_lpg_walk_wait = sum(buy_lpg_walk, buy_lpg_wait, na.rm = TRUE),
		receive_crh_walk_wait = sum(receive_crh_walk, receive_crh_wait, na.rm = TRUE)
	) %>%
	gather(-study_arm_overall, -fcn_id, key = "fuel_collection_activity", value = "time_hr") %>%
	filter(fuel_collection_activity %in% c("collect_wood_walk_hr", "receive_lpg_walk", "receive_lpg_wait", "buy_lpg_walk", "buy_lpg_wait")) %>%
	mutate(
		fuel_collection_activity = 
			ordered(
				fuel_collection_activity,
				levels = c("collect_wood_walk_hr", "receive_lpg_walk", "receive_lpg_wait", "buy_lpg_walk", "buy_lpg_wait"),
				labels = c("Collect wood", "Walk to \nreceive LPG", "Wait to \nreceive LPG", "Walk to \nbuy  LPG", "Wait to \nbuy LPG")
			)
	)%>%
	ggplot(aes(x = fuel_collection_activity, y = time_hr, fill = fuel_collection_activity)) +
	stat_summary(
		fun = "mean",
		geom = "bar"
	) +
	stat_summary(
		fun.data = "mean_cl_boot",
		geom = "linerange"
	) +
	viridis::scale_fill_viridis(
		discrete = TRUE,
		end = 7/8
	) +
	# scale_y_continuous(
	# 	labels = scales::percent_format(accuracy = 1)
	# ) + # Percentage labels rounded to the nearest integer
	theme_bw() +
	theme(
		# axis.text.x = element_text(angle = 45, hjust = 1),
		legend.position = "none"
	) +
	labs(
		title = "Fuel collection walk and wait times",
		x = "Fuel collection activity",
		y = "Hours"
	) 

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


###############################################################################
### Who cooks 
##############################################################################

# Percentage of hh that mention women, girls, men, and boys cook

## Vector of variables to summarize
# Use Hmisc::Cs to quote each variable
table12Vars <- 
	c("cook_who_w", "cook_who_g", "cook_who_m", "cook_who_b")

## Vector of categorical variables that need transformation
table12FactorVars <- table12Vars

# Create a TableOne object
tab12 <- 
	CreateTableOne(
		data = survey_data, 
		vars = table12Vars, 
		factorVars = table12FactorVars, 
		strata = "study_arm"
	)

tab12
# cook_who_b = 0 (%) why for other groups reportedd as ==1 ? because there was 0 boys so it doesn't show up as a factor?
