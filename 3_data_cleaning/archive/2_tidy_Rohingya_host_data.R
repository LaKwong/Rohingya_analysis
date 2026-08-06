################################################################################
# @Project: Rohingya LPG Evaluation
# @Author: Laura H Kwong
# @Description: tidy the merged baseline data and endline data
# @Version: 3.6.1
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))

# ============================================================================
# files in
file_in_1 <- here::here("4_data/RohingyaFuel_survey_data_clean.rds") 
file_in_2 <- here::here("4_data/RohingyaFuel_survey_data_clean_host.rds") 


# files out
file_out_1 <- here::here("4_data/RohingyaFuelMaster_survey_data_tidy.rds")
file_out_2 <- here::here("4_data/RohingyaFuelMaster_survey_data_tidy_host.rds")
# ============================================================================



# load files
survey_data_base <- read_rds(file_in_1)
survey_data_host_base <- read_rds(file_in_2)

survey_data_host_base %>% 
	count(study_arm_overall, timepoint)

# To do
# 1.  house_material
# 
# 
# Errors:
# 	# There was MORE beef in the pre-intervention communities? Look for outliers/mistakes
# 	# fuel_ever_receive_lpg = 1 is not true for all the intervention hh and it should be!!
# 	# fuel_30_receive_lpg = 1 (%) is not all true for all the intervention hh!!
# 	# What is the "other fuel"  - need to ask in FGD
# 	
# 	# We can use this question and the price of firewood in August / Sep (when the survey was conducted), to estimate the number of bundles of firewood purchased, then get the average weight of 100 bundles to calculate demand
# 	"In the past 30 days, how much money (taka) did you spend on firewood?"
# 
# 
# Considerations:
# 	
# 	# buy_wood_cost There are 4 hh above 3000 - check their hh size and if they a for others or cooking to sell or boiling frequently
# 	
# 	"Who in your family gathers scraps/leaves/twigs?" - is this question interpreted as who has EVER gathered? Or only, who currently gathers? If so, do we think this is a problem?
# 	
# 	Add question: fuel_30_other is 10% of homes - what is this other?
# 	
# 	Almost 150 of 393 hh report burning plastic every day!  We need to better understand how many meals and if they are relying on plastic for all of the fuel they need or if it is used in combination with other fuel sources --> ask about how often they burned plastic
# 
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
# 
# Many hh have drudgery_most_diff == drudgery_second_most_diff == drudgery_easiest! Need to train that this shouldn't happen. 
# The drudgery questions were accidentally not required, but still why did many skip them?  
# 	Will probably have to throw this question out.
# 
# The dates are in three different formats. 
# 
# 
# 
# Need to factor everything -- look at DataChallenge notes
# 
# Instead of refactoring or labeling everything, why not just have the label equal the choices tab "name"? Or does the name have to be a number. 
# 
# I averaged as many MUAC measurements as were taken - is that right?
# 	
# 	Need to know what breathing rate and MUAC should be for each age group to know if ther are fewer out of the ordinary. 
# 
# How does icddrb do PCR for wealth? Should have some way to see if the hh are different in terms of SES. 
# 
# 
# 
# This script is to analyze the Rohingya LPG Assessment household survey data
# 
# 
# 
# - percent of hh involved in ngo or wage labor
# 
# pre-intervention sep expenditure charts for people 
# - percentage of people buying wood 
# - percentage of those collecting X fuel and being harassed
# - 
# 	- send Mickael my requests for cost info
# - add Peter, Rajib, Ritu, Geophrey to IRB



# ============================================================================
# Define vectors to group columns
# ============================================================================


asset_vars <- 
	c(
		"mattress", "blanket", "mosquito_net", "solar_lamp", "portable_lamp",
		"umbrella", "table", "chair_bench", "almirah_wardrobe_show_case", 
		"electric_fan", "refrigerator",
		"shovel", "sickle", "weaving_tool", "fish_net",
		"mobile_phone", "smartphone", "radio", 
		"television", "DVD_VCD_player", "computer_laptop", 
		"bicycle", "motorcycle", "rickshaw_van", "CNG_tempo_electric_bike",
		"chicken_duck_pigeon", "goat_sheep", "cow_buffalo"
	)

spent_monthly_vars <- 
	c(
		"spent_food",
		"buy_wood_cost", # "In the past 30 days, how much money (taka) did you spend on firewood?"
		"spent_hh_items",
		"spent_hygiene",
		"spent_tobacco_pan",
		"spent_transport",
		"spent_phone",
		"spent_clothing_month", 
		"spent_shelter_month",
		"spent_celebrations_month",
		"spent_debt_month",
		"spent_medical_month",
		"spent_education_month",
		"spent_other_month"
	)
# 		"spent_total_month",

nonrespiratory_vars <- 
	c(
		"target_child_eye_red", "target_child_eye_itch",  
		"respondent_eye_red", "respondent_eye_itch", "respondent_eye_sore",
		"respondent_headache", "respondent_backache"
	)

nonrespiratory_vars_yn <-  
	map_chr(nonrespiratory_vars, ~ paste0(., "_yn"))

respiratory_vars <- 
	c(
		"target_child_cough", "target_child_resp_rate", 
		"target_child_clinic_resp", "target_child_wheezing", 
		"target_child_disturbed_sleep", "target_child_distrubed_speech", 
		"respondent_cough", "respondent_resp_rate", 
		"respondent_weight_loss", "respondent_wheezing", 
		"respondent_disturbed_sleep", "respondent_disturbed_speech"
	)

respiratory_vars_yn <-  
	map_chr(respiratory_vars, ~ paste0(., "_yn"))

generalhealth_vars <-  
	c(
		"target_child_fever", "target_child_weight_loss", "target_child_lethargy"
	)

generalhealth_vars_yn <-  
	map_chr(generalhealth_vars, ~ paste0(., "_yn"))

MUAC_vars <- 
	c(
		"target_child_arm_measurements_yn", "target_child_mid_arm_circ_av"
	)

breathing_vars <- 
	c(
		"target_child_breathing_yn", "target_child_resp_rate_measured"
	)

mental_health_good_vars <- c("happy", "enjoyed_life", "self_worth", "hopeful")

mental_health_bad_vars <- 
	c(
		"bothered", "sick", 
		"no_appetite", "diff_concentrating", "restless_sleep",
		"exert_effort", "less_talkative", 
		"feeling_disliked", "unfriendly_people",
		"fearful", "lonely", "crying_spells",
		"cant_get_going", "feeling_down", "life_failure", "depressed" 
	)

anxiety_vars <- 
	c("feeling_anxious", "feeling_worry", "feeling_not_relaxing", "feeling_restless", "feeling_annoyed", "feeling_afraid")

mental_health_vars <-
	c(mental_health_good_vars, mental_health_bad_vars, anxiety_vars, "suicidal_thoughts_30")

mental_health_vars_yn <-  
	map_chr(mental_health_vars, ~ paste0(., "_yn"))



# food soure
# This question was select_multiple. For each category there is a column with a y/n response for that food/source category
food_source <- 
	c(
		"rice_source", "bread_source", "corn_source", 
		"potatoes_source", "lentils_source", "eggs_source", 
		"dairy_source", "veggies_source", "fruit_source", 
		"fish_source", "poultry_source", "goat_sheep_source",
		"beef_source", "oil_source", "sugar_source" 
	)

food_source_sources = 
	c(
		"Don't eat",
		"Food aid",
		"Purchased",
		"Purchased", #"Purchased with wages",
		"Gift",
		"Borrowed",
		"Own production",
		"Exchanged labor",
		"Exchanged items",
		"Gathered",
		"Other"
	)

food_source_all <- c()
for (food in food_source){
	for (i in c(1:9, 66)){
		food_source_all <- c(food_source_all, paste(food, i, sep = ""))
	}
}


# food_cant_afford
food_cant_afford <-
	c(
		"food_cant_afford_action",
		"food_cant_afford_difficult",
		"food_cant_afford_easiest"
	)

# fuel_cant_afford
fuel_cant_afford <-
	c(
		"fuel_cant_afford_action"
	)

food_cant_afford_reasons <- 
	c(
		"Borrowed food",
		"Reduce food per meal",
		"Reduce meals per day",
		"Skip meals",
		"Restrict adult food",
		"Sold goods",
		"Purchased on credit",
		"Borrowed money",
		"Reduced expenditures",
		"Spent savings",
		"Worked for money for food",
		"Sold or consumed livestock",
		"Exchanged food",
		"Other",
		"Did not need to manage"
	)

fuel_cant_afford_reason <- 
	c(
		"Borrowed fuel", # Response was "borrow food", which is also possible, but I'm going to interpret as "borrow fuel"
		"Reduce food per meal",
		"Reduce meals per day",
		"Skip meals",
		"Restrict adult food",
		"Sold goods",
		"Purchased on credit",
		"Borrowed money",
		"Reduced expenditures",
		"Spent savings",
		"Worked for money for fuel",
		"Sold livestock to purchase fuel",
		"Sold food to purchase fuel",
		"Eat food that doesn't need to be cooked",
		"Eat food that wasn't fully cooked",
		"Other",
		"Did not need to manage"
	)

# This question was select_multiple. For each category there is a column with a y/n response for that response category
food_cant_afford_all <- c()
for (type in food_cant_afford){
	for (i in c(1:13, 66, 88)){ # 
		food_cant_afford_all <- c(food_cant_afford_all, paste(type, i, sep = ""))
	}
}

# This question was select_multiple. For each category there is a column with a y/n response for that response category
fuel_cant_afford_all <- c()
for (type in fuel_cant_afford){
	for (i in c(1:15, 66, 88)){ #
		fuel_cant_afford_all <- c(fuel_cant_afford_all, paste(type, i, sep = ""))
	}
}

# mutate_at(
# 		vars(food_cant_afford_action),
# 		funs(factor),
# 			levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 66, 88),
# 		labels = c(
# 			"Borrowed food",
# 			"Reduce food per meal",
# 			"Reduce meals per day",
# 			"Skip meals",
# 			"Restrict adult food",
# 			"Sold goods",
# 			"Purchased on credit",
# 			"Borrowed money",
# 			"Reduced expenditures",
# 			"Spent savings",
# 			"Worked for money for food",
# 			"Sold or consumed livestock",
# 			"Exchanged food",
# 			"Other",
# 			"Did not need to manage"
# 		)
# 	)
# 	
# 	mutate_at(
# 		vars(fuel_cant_afford_action),
# 		funs(factor),
# 		levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 66, 88),
# 		labels = c(
# 			"Borrowed fuel", # Response was "borrow food", which is also possible, but I'm going to interpret as "borrow fuel"
# 			"Reduce food per meal",
# 			"Reduce meals per day",
# 			"Skip meals",
# 			"Restrict adult food",
# 			"Sold goods",
# 			"Purchased on credit",
# 			"Borrowed money",
# 			"Reduced expenditures",
# 			"Spent savings",
# 			"Worked for money for fuel",
# 			"Sold livestock to purchase fuel",
# 			"Sold food to purchase fuel",
# 			"Eat food that doesn't need to be cooked",
# 			"Eat food that wasn't fully cooked",
# 			"Other",
# 			"Did not need to manage"
# 		)
# 	) 


fuel_use_non_lpg_type <-
	c(
		"fuel_use_non_lpg_type_buy_lpg",
		"fuel_use_non_lpg_type_buy_wood",
		"fuel_use_non_lpg_type_collect_wood",
		"fuel_use_non_lpg_type_other"
	)

credit <-
	c(
		"credit_access",
		"credit_relatives",
		"credit_charities", 
		"credit_village_head", 
		"credit_lender", 
		"credit_bank",
		"credit_cooperative"
	)

# Ugh! We could have done a much better job of naming the variables so they would be easy to separate


# for fuel_collection_who, the denominator is hh that EVER collected type X fuel
fuel_collection_who <- c()
for (fuel in c("gather_scraps", "collect_wood", "receive_wood", "buy_wood", "receive_lpg", "buy_lpg", "receive_crh", "buy_crh")){
	for (person in c("w", "g", "m", "b")){
		fuel_collection_who <- c(fuel_collection_who, paste(fuel, person, sep = "_"))
	}
}

fuel_collection_who_ever <- c()
for (fuel in c("gather_scraps", "collect_wood", "receive_wood", "buy_wood", "receive_lpg", "buy_lpg", "receive_crh", "buy_crh")){
	for (person in c("w", "g", "m", "b")){
		fuel_collection_who_ever <- c(fuel_collection_who_ever, paste(fuel, person, "ever", sep = "_"))
	}
}

# for any_harassment_hh, the denominator is ALL households
any_harassment_hh <- # did anyone in the hh experience this type of harassment FOR ANY REASON? Y/N
	c(
		"insult_hh", "belittle_hh", "scare_hh", "push_hh", 
		"hit_hh", "kick_hh", "choke_hh", "weapon_hh", 
		"sex_lang_hh", "sex_contact_hh", "sex_rumor_hh", 
		"clothing_pull_hh", "sex_corner_hh"
	)

# for fuel_harassment_hh, the denominator is households that said they EvER collected type X fuel 
fuel_harassment_hh <- # did anyone in the hh experience this type of harassment related to fuel collection? Y/N
	survey_data_base %>% 
	select(
		ends_with("insult_hh"), ends_with("belittle_hh"), ends_with("scare_hh"), ends_with("push_hh"), 
		ends_with("hit_hh"), ends_with("kick_hh"), ends_with("choke_hh"), ends_with("weapon_hh"), 
		ends_with("sex_lang_hh"), ends_with("sex_contact_hh"), ends_with("sex_rumor_hh"), 
		ends_with("clothing_pull_hh"), ends_with("sex_corner_hh")
	) %>%
	select(
		-all_of(any_harassment_hh)
	) %>%
	names()

# What was the frequency of harassment by the specific people? (Categorical number of times per week)
# Determine prev with harassment_vars_yn
# Need to make the gender-specific denominator the fuel_collection_who - fuel-and gender-specifc 
harassment_vars <- 
	survey_data_base %>%
	select( 
		starts_with("gather_scraps"), 
		starts_with("collect_wood"), 
		starts_with("receive_wood"), 
		starts_with("buy_wood"), 
		starts_with("receive_lpg"), 
		starts_with("buy_lpg"), 
		starts_with("receive_crh"), 
		starts_with("buy_crh")
	) %>%
	select(
		-c(
			all_of(fuel_collection_who), # remove the vars that only indicate of that type of person collects the type of fuel
			all_of(fuel_collection_who_ever), # remove the vars that only indicate of that type of person collects the type of fuel
			# any_harassment_hh, # These weren't selected by the section criteria
			all_of(fuel_harassment_hh), # remove the vars that are the hh level of harassment 
			gather_scraps_dead, gather_scraps_dead.1, gather_scraps_dead1,
			collect_wood_walk_hr, collect_wood_forest_start, collect_wood_forest_stop,
			receive_wood_wait,
			buy_wood_cost, buy_wood_reason, buy_wood_when_start,
			receive_lpg_walk, receive_lpg_wait,
			buy_lpg_cost, buy_lpg_walk, buy_lpg_wait,
			receive_crh_walk, receive_crh_wait
			
			#	# vars that don't exist yet	
			# time_child_gathering_nonwood_items = time_child_gathering_non.wood_items, 
			# respondent_resp_rate  = resp_rate_reported_respondant,       
			# respondent_weight_loss = weight_loss_reported_respondant,
			# reduce_meals_lack_food = reduce_meals,
			# not_eat_lack_food = not_eat,
			# reduce_meals_lack_fuel = reduce_meals1,
			# not_eat_lack_fuel = not_eat1,
			# fuel_ever_gather_scraps = fuel_ever_scraps,
			# fuel_30_gather_scraps = fuel_30_scraps,
			# gather_wood_dead = gather_scraps_dead1,
			# collect_wood_times_week = times_wood_day,
			# reason_forest_defecation = reason_forest_defacation,
			# cook_to_sell_percent = cook_to_sell,
			# cook_sell_days_week = cook_sell_yesterday
		)
	) %>%
	names()

# Prevalence of harassment of certain genders; denominator is hh with that gender that EVER collected type X fuel
harassment_vars_yn <-  
	map_chr(harassment_vars, ~ paste0(., "_yn"))

# harassment_vars_by_group_yn <-  
# 	map_chr(harassment_vars_by_group, ~ paste0(., "_yn"))




# ============================================================================
# Coalsce values that were accidentally separated into two variables
# ============================================================================


# There are many vars that have <var>1 and <var>.1 becuase of a change in the survey xlm that happened after hh 9I100788I3_5992, the last pre-intervention hh, which was in camp 9 and surveyed 10/2. The first intervention hh, 4E184565Pp21_514, was in camp 4 and surveyed 10/10. This change happened when we 

# Function to coalese columns
survey_data_coalesce_fcn <- function(df){
	df %>%
		select(
			-starts_with("generated_note"),
			-starts_with("reserved_name")
		) %>%
		mutate_at(
			vars(camp_id, block_id, subblock_id, fcn_id),
			funs(str_to_lower(str_replace_all(., fixed(" "), "")))
		) %>%
		# select(camp_id, block_id, subblock_id, fcn_id, Rand_nu, hh_id, hh_id_original, name_respondent, consent) %>%
		# arrange(fcn_id) %>%
		# View()
		mutate(
			org =
				case_when(
					camp_id %in% c("3", "4", "5") ~ "UNHCR",
					camp_id %in% c("8w", "8e", "9", "10", "18") ~ "IOM"
				)
		) %>%
		
		mutate_at(
			vars(
				starts_with("rice_source"),
				starts_with("bread_source"),
				starts_with("corn_source"),
				starts_with("potatoes_source"),
				starts_with("lentils_source"),
				starts_with("eggs_source"),
				starts_with("dairy_source"),
				starts_with("veggies_source"),
				starts_with("fruit_source"),
				starts_with("fish_source"),
				starts_with("poultry_source"),
				starts_with("goat_sheep_source"),
				starts_with("oil_source"),
				starts_with("sugar_source"),
				starts_with("food_cant_afford_action"),
				starts_with("fuel_cant_afford_action"),
				starts_with("reduce_meals"),
				starts_with("not_eat"),
				starts_with("gather_scraps_dead")
			),
			list(as.numeric)
		) %>%
		mutate(
			reduce_meals1 = coalesce(reduce_meals1, reduce_meals.1, reduce_meals1),
			
			not_eat1 = coalesce(as.numeric(not_eat1), as.numeric(not_eat.1), as.numeric(reduce_meals1)),
			
			## "Are the scraps/leaves/twigs you collect all dead?" has the variable gather_scraps_dead 
			## "Is all the wood you collect all dead?" accidentally has the variable gather_scraps_dead1
			gather_wood_dead = coalesce(gather_scraps_dead1, gather_scraps_dead.1), # this is actually gather_wood_dead
			
			time_child_gathering_nonwood_items = coalesce(time_child_gathering_nonwood_ite, time_child_gathering_non.wood_items),
			# # "time_child_gathering_nonwood_ite" was misspelled - missng the "ms" at the end
			# # coalesce isn't working becuase everything in time_child_gathering_nonwood_ite is NA
			# instead just rename the variable that does have the data (see survey_data_2)
			
			lpg_changes_lifestyle1 = coalesce(lpg_changes_lifestyle1, lpg_changes_lifestyle.1),
			lpg_changes_lifestyle2 = coalesce(lpg_changes_lifestyle2, lpg_changes_lifestyle.2),
			lpg_changes_lifestyle3 = coalesce(lpg_changes_lifestyle3, lpg_changes_lifestyle.3),
			lpg_changes_lifestyle4 = coalesce(lpg_changes_lifestyle4, lpg_changes_lifestyle.4),
			lpg_changes_lifestyle5 = coalesce(lpg_changes_lifestyle5, lpg_changes_lifestyle.5),
			lpg_changes_lifestyle6 = coalesce(lpg_changes_lifestyle6, lpg_changes_lifestyle.6),
			
			food_cant_afford_action1 = coalesce(food_cant_afford_action1, food_cant_afford_action.1, `food_cant_afford_action.1`),
			food_cant_afford_action2 = coalesce(food_cant_afford_action2, food_cant_afford_action.2, `food_cant_afford_action.2`),
			food_cant_afford_action3 = coalesce(food_cant_afford_action3, food_cant_afford_action.3, `food_cant_afford_action.3`),
			food_cant_afford_action4 = coalesce(food_cant_afford_action4, food_cant_afford_action.4, `food_cant_afford_action.4`),
			food_cant_afford_action5 = coalesce(food_cant_afford_action5, food_cant_afford_action.5, `food_cant_afford_action.5`),
			food_cant_afford_action6 = coalesce(food_cant_afford_action6, food_cant_afford_action.6, `food_cant_afford_action.6`),
			food_cant_afford_action7 = coalesce(food_cant_afford_action7, food_cant_afford_action.7, `food_cant_afford_action.7`),
			food_cant_afford_action8 = coalesce(food_cant_afford_action8, food_cant_afford_action.8, `food_cant_afford_action.8`),
			food_cant_afford_action9 = coalesce(food_cant_afford_action9, food_cant_afford_action.9, `food_cant_afford_action.9`),
			food_cant_afford_action10 = coalesce(food_cant_afford_action10, food_cant_afford_action.10, `food_cant_afford_action.10`),
			food_cant_afford_action11 = coalesce(food_cant_afford_action11, food_cant_afford_action.11, `food_cant_afford_action.11`),
			food_cant_afford_action12 = coalesce(food_cant_afford_action12, food_cant_afford_action.12, `food_cant_afford_action.12`),
			food_cant_afford_action13 = coalesce(food_cant_afford_action13, food_cant_afford_action.13, `food_cant_afford_action.13`),
			food_cant_afford_action66 = coalesce(food_cant_afford_action66, food_cant_afford_action.66, `food_cant_afford_action.66`),
			food_cant_afford_action88 = coalesce(food_cant_afford_action88, food_cant_afford_action.88, `food_cant_afford_action.88`),
			
			fuel_cant_afford_action1 = coalesce(fuel_cant_afford_action1, fuel_cant_afford_action.1, `fuel_cant_afford_action.1`),
			fuel_cant_afford_action2 = coalesce(fuel_cant_afford_action2, fuel_cant_afford_action.2, `fuel_cant_afford_action.2`),
			fuel_cant_afford_action3 = coalesce(fuel_cant_afford_action3, fuel_cant_afford_action.3, `fuel_cant_afford_action.3`),
			fuel_cant_afford_action4 = coalesce(fuel_cant_afford_action4, fuel_cant_afford_action.4, `fuel_cant_afford_action.4`),
			fuel_cant_afford_action5 = coalesce(fuel_cant_afford_action5, fuel_cant_afford_action.5, `fuel_cant_afford_action.5`),
			fuel_cant_afford_action6 = coalesce(fuel_cant_afford_action6, fuel_cant_afford_action.6, `fuel_cant_afford_action.6`),
			fuel_cant_afford_action7 = coalesce(fuel_cant_afford_action7, fuel_cant_afford_action.7, `fuel_cant_afford_action.7`),
			fuel_cant_afford_action8 = coalesce(fuel_cant_afford_action8, fuel_cant_afford_action.8, `fuel_cant_afford_action.8`),
			fuel_cant_afford_action9 = coalesce(fuel_cant_afford_action9, fuel_cant_afford_action.9, `fuel_cant_afford_action.9`),
			fuel_cant_afford_action10 = coalesce(fuel_cant_afford_action10, fuel_cant_afford_action.10, `fuel_cant_afford_action.10`),
			fuel_cant_afford_action11 = coalesce(fuel_cant_afford_action11, fuel_cant_afford_action.11, `fuel_cant_afford_action.11`),
			fuel_cant_afford_action12 = coalesce(fuel_cant_afford_action12, fuel_cant_afford_action.12, `fuel_cant_afford_action.12`),
			fuel_cant_afford_action13 = coalesce(fuel_cant_afford_action13, fuel_cant_afford_action.13, `fuel_cant_afford_action.13`),
			fuel_cant_afford_action66 = coalesce(fuel_cant_afford_action66, fuel_cant_afford_action.66, `fuel_cant_afford_action.66`),
			fuel_cant_afford_action88 = coalesce(fuel_cant_afford_action88, fuel_cant_afford_action.88, `fuel_cant_afford_action.88`),
			
			# baseline had rice_source.0 but endline has rice_source.0
			
			rice_source0 = coalesce(rice_source0, rice_source.0, `rice_source.0`),
			rice_source1 = coalesce(rice_source1, rice_source.1, `rice_source.1`),
			rice_source2 = coalesce(rice_source2, rice_source.2, `rice_source.2`),
			rice_source3 = coalesce(rice_source3, rice_source.3, `rice_source.3`),
			rice_source4 = coalesce(rice_source4, rice_source.4, `rice_source.4`),
			rice_source5 = coalesce(rice_source5, rice_source.5, `rice_source.5`),
			rice_source6 = coalesce(rice_source6, rice_source.6, `rice_source.6`),
			rice_source7 = coalesce(rice_source7, rice_source.7, `rice_source.7`),
			rice_source8 = coalesce(rice_source8, rice_source.8, `rice_source.8`),
			rice_source9 = coalesce(rice_source9, rice_source.9, `rice_source.9`),
			rice_source66 = coalesce(rice_source66, rice_source.66, `rice_source.66`),
			
			bread_source0 = coalesce(bread_source0, bread_source.0, `bread_source.0`),
			bread_source1 = coalesce(bread_source1, bread_source.1, `bread_source.1`),
			bread_source2 = coalesce(bread_source2, bread_source.2, `bread_source.2`),
			bread_source3 = coalesce(bread_source3, bread_source.3, `bread_source.3`),
			bread_source4 = coalesce(bread_source4, bread_source.4, `bread_source.4`),
			bread_source5 = coalesce(bread_source5, bread_source.5, `bread_source.5`),
			bread_source6 = coalesce(bread_source6, bread_source.6, `bread_source.6`),
			bread_source7 = coalesce(bread_source7, bread_source.7, `bread_source.7`),
			bread_source8 = coalesce(bread_source8, bread_source.8, `bread_source.8`),
			bread_source9 = coalesce(bread_source9, bread_source.9, `bread_source.9`),
			bread_source66 = coalesce(bread_source66, bread_source.66, `bread_source.66`),
			
			corn_source0 = coalesce(corn_source0, corn_source.0, `corn_source.0`),
			corn_source1 = coalesce(corn_source1, corn_source.1, `corn_source.1`),
			corn_source2 = coalesce(corn_source2, corn_source.2, `corn_source.2`),
			corn_source3 = coalesce(corn_source3, corn_source.3, `corn_source.3`),
			corn_source4 = coalesce(corn_source4, corn_source.4, `corn_source.4`),
			corn_source5 = coalesce(corn_source5, corn_source.5, `corn_source.5`),
			corn_source6 = coalesce(corn_source6, corn_source.6, `corn_source.6`),
			corn_source7 = coalesce(corn_source7, corn_source.7, `corn_source.7`),
			corn_source8 = coalesce(corn_source8, corn_source.8, `corn_source.8`),
			corn_source9 = coalesce(corn_source9, corn_source.9, `corn_source.9`),
			corn_source66 = coalesce(corn_source66, corn_source.66, `corn_source.66`),
			
			potatoes_source0 = coalesce(potatoes_source0, potatoes_source.0, `potatoes_source.0`),
			potatoes_source1 = coalesce(potatoes_source1, potatoes_source.1, `potatoes_source.1`),
			potatoes_source2 = coalesce(potatoes_source2, potatoes_source.2, `potatoes_source.2`),
			potatoes_source3 = coalesce(potatoes_source3, potatoes_source.3, `potatoes_source.3`),
			potatoes_source4 = coalesce(potatoes_source4, potatoes_source.4, `potatoes_source.4`),
			potatoes_source5 = coalesce(potatoes_source5, potatoes_source.5, `potatoes_source.5`),
			potatoes_source6 = coalesce(potatoes_source6, potatoes_source.6, `potatoes_source.6`),
			potatoes_source7 = coalesce(potatoes_source7, potatoes_source.7, `potatoes_source.7`),
			potatoes_source8 = coalesce(potatoes_source8, potatoes_source.8, `potatoes_source.8`),
			potatoes_source9 = coalesce(potatoes_source9, potatoes_source.9, `potatoes_source.9`),
			potatoes_source66 = coalesce(potatoes_source66, potatoes_source.66, `potatoes_source.66`),
			
			lentils_source0 = coalesce(lentils_source0, lentils_source.0, `lentils_source.0`),
			lentils_source1 = coalesce(lentils_source1, lentils_source.1, `lentils_source.1`),
			lentils_source2 = coalesce(lentils_source2, lentils_source.2, `lentils_source.2`),
			lentils_source3 = coalesce(lentils_source3, lentils_source.3, `lentils_source.3`),
			lentils_source4 = coalesce(lentils_source4, lentils_source.4, `lentils_source.4`),
			lentils_source5 = coalesce(lentils_source5, lentils_source.5, `lentils_source.5`),
			lentils_source6 = coalesce(lentils_source6, lentils_source.6, `lentils_source.6`),
			lentils_source7 = coalesce(lentils_source7, lentils_source.7, `lentils_source.7`),
			lentils_source8 = coalesce(lentils_source8, lentils_source.8, `lentils_source.8`),
			lentils_source9 = coalesce(lentils_source9, lentils_source.9, `lentils_source.9`),
			lentils_source66 = coalesce(lentils_source66, lentils_source.66, `lentils_source.66`),
			
			eggs_source0 = coalesce(eggs_source0, eggs_source.0, `eggs_source.0`),
			eggs_source1 = coalesce(eggs_source1, eggs_source.1, `eggs_source.1`),
			eggs_source2 = coalesce(eggs_source2, eggs_source.2, `eggs_source.2`),
			eggs_source3 = coalesce(eggs_source3, eggs_source.3, `eggs_source.3`),
			eggs_source4 = coalesce(eggs_source4, eggs_source.4, `eggs_source.4`),
			eggs_source5 = coalesce(eggs_source5, eggs_source.5, `eggs_source.5`),
			eggs_source6 = coalesce(eggs_source6, eggs_source.6, `eggs_source.6`),
			eggs_source7 = coalesce(eggs_source7, eggs_source.7, `eggs_source.7`),
			eggs_source8 = coalesce(eggs_source8, eggs_source.8, `eggs_source.8`),
			eggs_source9 = coalesce(eggs_source9, eggs_source.9, `eggs_source.9`),
			eggs_source66 = coalesce(eggs_source66, eggs_source.66, `eggs_source.66`),
			
			dairy_source0 = coalesce(dairy_source0, dairy_source.0, `dairy_source.0`),
			dairy_source1 = coalesce(dairy_source1, dairy_source.1, `dairy_source.1`),
			dairy_source2 = coalesce(dairy_source2, dairy_source.2, `dairy_source.2`),
			dairy_source3 = coalesce(dairy_source3, dairy_source.3, `dairy_source.3`),
			dairy_source4 = coalesce(dairy_source4, dairy_source.4, `dairy_source.4`),
			dairy_source5 = coalesce(dairy_source5, dairy_source.5, `dairy_source.5`),
			dairy_source6 = coalesce(dairy_source6, dairy_source.6, `dairy_source.6`),
			dairy_source7 = coalesce(dairy_source7, dairy_source.7, `dairy_source.7`),
			dairy_source8 = coalesce(dairy_source8, dairy_source.8, `dairy_source.8`),
			dairy_source9 = coalesce(dairy_source9, dairy_source.9, `dairy_source.9`),
			dairy_source66 = coalesce(dairy_source66, dairy_source.66, `dairy_source.66`),
			
			veggies_source0 = coalesce(veggies_source0, veggies_source.0, `veggies_source.0`),
			veggies_source1 = coalesce(veggies_source1, veggies_source.1, `veggies_source.1`),
			veggies_source2 = coalesce(veggies_source2, veggies_source.2, `veggies_source.2`),
			veggies_source3 = coalesce(veggies_source3, veggies_source.3, `veggies_source.3`),
			veggies_source4 = coalesce(veggies_source4, veggies_source.4, `veggies_source.4`),
			veggies_source5 = coalesce(veggies_source5, veggies_source.5, `veggies_source.5`),
			veggies_source6 = coalesce(veggies_source6, veggies_source.6, `veggies_source.6`),
			veggies_source7 = coalesce(veggies_source7, veggies_source.7, `veggies_source.7`),
			veggies_source8 = coalesce(veggies_source8, veggies_source.8, `veggies_source.8`),
			veggies_source9 = coalesce(veggies_source9, veggies_source.9, `veggies_source.9`),
			veggies_source66 = coalesce(veggies_source66, veggies_source.66, `veggies_source.66`),
			
			fruit_source0 = coalesce(fruit_source0, fruit_source.0, `fruit_source.0`),
			fruit_source1 = coalesce(fruit_source1, fruit_source.1, `fruit_source.1`),
			fruit_source2 = coalesce(fruit_source2, fruit_source.2, `fruit_source.2`),
			fruit_source3 = coalesce(fruit_source3, fruit_source.3, `fruit_source.3`),
			fruit_source4 = coalesce(fruit_source4, fruit_source.4, `fruit_source.4`),
			fruit_source5 = coalesce(fruit_source5, fruit_source.5, `fruit_source.5`),
			fruit_source6 = coalesce(fruit_source6, fruit_source.6, `fruit_source.6`),
			fruit_source7 = coalesce(fruit_source7, fruit_source.7, `fruit_source.7`),
			fruit_source8 = coalesce(fruit_source8, fruit_source.8, `fruit_source.8`),
			fruit_source9 = coalesce(fruit_source9, fruit_source.9, `fruit_source.9`),
			fruit_source66 = coalesce(fruit_source66, fruit_source.66, `fruit_source.66`),
			
			fish_source0 = coalesce(fish_source0, fish_source.0, `fish_source.0`),
			fish_source1 = coalesce(fish_source1, fish_source.1, `fish_source.1`),
			fish_source2 = coalesce(fish_source2, fish_source.2, `fish_source.2`),
			fish_source3 = coalesce(fish_source3, fish_source.3, `fish_source.3`),
			fish_source4 = coalesce(fish_source4, fish_source.4, `fish_source.4`),
			fish_source5 = coalesce(fish_source5, fish_source.5, `fish_source.5`),
			fish_source6 = coalesce(fish_source6, fish_source.6, `fish_source.6`),
			fish_source7 = coalesce(fish_source7, fish_source.7, `fish_source.7`),
			fish_source8 = coalesce(fish_source8, fish_source.8, `fish_source.8`),
			fish_source9 = coalesce(fish_source9, fish_source.9, `fish_source.9`),
			fish_source66 = coalesce(fish_source66, fish_source.66, `fish_source.66`),
			
			poultry_source0 = coalesce(poultry_source0, poultry_source.0, `poultry_source.0`),
			poultry_source1 = coalesce(poultry_source1, poultry_source.1, `poultry_source.1`),
			poultry_source2 = coalesce(poultry_source2, poultry_source.2, `poultry_source.2`),
			poultry_source3 = coalesce(poultry_source3, poultry_source.3, `poultry_source.3`),
			poultry_source4 = coalesce(poultry_source4, poultry_source.4, `poultry_source.4`),
			poultry_source5 = coalesce(poultry_source5, poultry_source.5, `poultry_source.5`),
			poultry_source6 = coalesce(poultry_source6, poultry_source.6, `poultry_source.6`),
			poultry_source7 = coalesce(poultry_source7, poultry_source.7, `poultry_source.7`),
			poultry_source8 = coalesce(poultry_source8, poultry_source.8, `poultry_source.8`),
			poultry_source9 = coalesce(poultry_source9, poultry_source.9, `poultry_source.9`),
			poultry_source66 = coalesce(poultry_source66, poultry_source.66, `poultry_source.66`),
			
			goat_sheep_source0 = coalesce(goat_sheep_source0, goat_sheep_source.0, `goat_sheep_source.0`),
			goat_sheep_source1 = coalesce(goat_sheep_source1, goat_sheep_source.1, `goat_sheep_source.1`),
			goat_sheep_source2 = coalesce(goat_sheep_source2, goat_sheep_source.2, `goat_sheep_source.2`),
			goat_sheep_source3 = coalesce(goat_sheep_source3, goat_sheep_source.3, `goat_sheep_source.3`),
			goat_sheep_source4 = coalesce(goat_sheep_source4, goat_sheep_source.4, `goat_sheep_source.4`),
			goat_sheep_source5 = coalesce(goat_sheep_source5, goat_sheep_source.5, `goat_sheep_source.5`),
			goat_sheep_source6 = coalesce(goat_sheep_source6, goat_sheep_source.6, `goat_sheep_source.6`),
			goat_sheep_source7 = coalesce(goat_sheep_source7, goat_sheep_source.7, `goat_sheep_source.7`),
			goat_sheep_source8 = coalesce(goat_sheep_source8, goat_sheep_source.8, `goat_sheep_source.8`),
			goat_sheep_source9 = coalesce(goat_sheep_source9, goat_sheep_source.9, `goat_sheep_source.9`),
			goat_sheep_source66 = coalesce(goat_sheep_source66, goat_sheep_source.66, `goat_sheep_source.66`),
			
			beef_source0 = coalesce(beef_source0, beef_source.0, `beef_source.0`),
			beef_source1 = coalesce(beef_source1, beef_source.1, `beef_source.1`),
			beef_source2 = coalesce(beef_source2, beef_source.2, `beef_source.2`),
			beef_source3 = coalesce(beef_source3, beef_source.3, `beef_source.3`),
			beef_source4 = coalesce(beef_source4, beef_source.4, `beef_source.4`),
			beef_source5 = coalesce(beef_source5, beef_source.5, `beef_source.5`),
			beef_source6 = coalesce(beef_source6, beef_source.6, `beef_source.6`),
			beef_source7 = coalesce(beef_source7, beef_source.7, `beef_source.7`),
			beef_source8 = coalesce(beef_source8, beef_source.8, `beef_source.8`),
			beef_source9 = coalesce(beef_source9, beef_source.9, `beef_source.9`),
			beef_source66 = coalesce(beef_source66, beef_source.66, `beef_source.66`),
			
			oil_source0 = coalesce(oil_source0, oil_source.0, `oil_source.0`),
			oil_source1 = coalesce(oil_source1, oil_source.1, `oil_source.1`),
			oil_source2 = coalesce(oil_source2, oil_source.2, `oil_source.2`),
			oil_source3 = coalesce(oil_source3, oil_source.3, `oil_source.3`),
			oil_source4 = coalesce(oil_source4, oil_source.4, `oil_source.4`),
			oil_source5 = coalesce(oil_source5, oil_source.5, `oil_source.5`),
			oil_source6 = coalesce(oil_source6, oil_source.6, `oil_source.6`),
			oil_source7 = coalesce(oil_source7, oil_source.7, `oil_source.7`),
			oil_source8 = coalesce(oil_source8, oil_source.8, `oil_source.8`),
			oil_source9 = coalesce(oil_source9, oil_source.9, `oil_source.9`),
			oil_source66 = coalesce(oil_source66, oil_source.66, `oil_source.66`),
			
			sugar_source0 = coalesce(sugar_source0, sugar_source.0, `sugar_source.0`),
			sugar_source1 = coalesce(sugar_source1, sugar_source.1, `sugar_source.1`),
			sugar_source2 = coalesce(sugar_source2, sugar_source.2, `sugar_source.2`),
			sugar_source3 = coalesce(sugar_source3, sugar_source.3, `sugar_source.3`),
			sugar_source4 = coalesce(sugar_source4, sugar_source.4, `sugar_source.4`),
			sugar_source5 = coalesce(sugar_source5, sugar_source.5, `sugar_source.5`),
			sugar_source6 = coalesce(sugar_source6, sugar_source.6, `sugar_source.6`),
			sugar_source7 = coalesce(sugar_source7, sugar_source.7, `sugar_source.7`),
			sugar_source8 = coalesce(sugar_source8, sugar_source.8, `sugar_source.8`),
			sugar_source9 = coalesce(sugar_source9, sugar_source.9, `sugar_source.9`),
			sugar_source66 = coalesce(sugar_source66, sugar_source.66, `sugar_source.66`)
		) %>%
		select(
			-c(
				reduce_meals.1,,
				not_eat.1,,
				gather_scraps_dead1, gather_scraps_dead.1, # this is actually gather_wood_dead
				
				time_child_gathering_nonwood_ite, time_child_gathering_non.wood_items,
				contains("_lifestyle.1"),
				contains("_lifestyle.2"),
				contains("_lifestyle.3"),
				contains("_lifestyle.4"),
				contains("_lifestyle.5"),
				contains("_lifestyle.6"),
				contains("_lifestyle/1"),
				contains("_lifestyle/2"),
				contains("_lifestyle/3"),
				contains("_lifestyle/4"),
				contains("_lifestyle/5"),
				contains("_lifestyle/6"),
				
				contains("_action.1"),
				contains("_action.2"),
				contains("_action.3"),
				contains("_action.4"),
				contains("_action.5"),
				contains("_action.6"),
				contains("_action.7"),
				contains("_action.8"),
				contains("_action.9"),
				contains("_action.10"),
				contains("_action.11"),
				contains("_action.12"),
				contains("_action.13"),
				contains("_action.66"),
				contains("_action.88"),
				contains("_action/1"),
				contains("_action/2"),
				contains("_action/3"),
				contains("_action/4"),
				contains("_action/5"),
				contains("_action/6"),
				contains("_action/7"),
				contains("_action/8"),
				contains("_action/9"),
				contains("_action/10"),
				contains("_action/11"),
				contains("_action/12"),
				contains("_action/13"),
				contains("_action/66"),
				contains("_action/88"),
				
				
				contains("_source.0"),
				contains("_source.1"),
				contains("_source.2"),
				contains("_source.3"),
				contains("_source.4"),
				contains("_source.5"),
				contains("_source.6"),
				contains("_source.7"),
				contains("_source.8"),
				contains("_source.9"),
				contains("_source.66"),
				contains("_source/0"),
				contains("_source/1"),
				contains("_source/2"),
				contains("_source/3"),
				contains("_source/4"),
				contains("_source/5"),
				contains("_source/6"),
				contains("_source/7"),
				contains("_source/8"),
				contains("_source/9"),
				contains("_source/66")
			)
		) %>%
		
		# clean up values
		mutate(
			camp_id = 
				str_to_upper(str_trim(camp_id)), # could use stringr in case there are other mis-matches
			subblock_id = 
				str_to_upper(str_trim(subblock_id)), # could use stringr in case there are other mis-matches
			name_mahji = 
				str_to_title(str_trim(name_mahji)),
			name_respondent = 
				str_to_title(str_trim(name_respondent)),
			name_hh_head = 
				str_to_title(str_trim(name_hh_head)),
			target_child_name = 
				str_to_title(str_trim(target_child_name)),
		) %>%
		
		# rename vars
		# rename(newname, oldname) renames (not copy and rename new column)
		rename( 
			
			
			# time_child_gathering_nonwood_items = time_child_gathering_non.wood_items, # corrected above
			respondent_resp_rate  = resp_rate_reported_respondant,
			respondent_weight_loss = weight_loss_reported_respondant,
			
			# # coping strategies food and fuel sections both have "reduce_meals" and "not_eat" vars
			reduce_meals_lack_food = reduce_meals,
			not_eat_lack_food = not_eat,
			reduce_meals_lack_fuel = reduce_meals1,
			not_eat_lack_fuel = not_eat1,
			
			fuel_ever_gather_scraps = fuel_ever_scraps,
			fuel_30_gather_scraps = fuel_30_scraps,
			
			
			## When you depend on wood for cooking fuel, then how many times in one week do you go to the forest to collect wood?
			collect_wood_times_week = times_wood_day,
			reason_forest_defecation = reason_forest_defacation,
			cook_to_sell_percent = cook_to_sell,
			cook_sell_days_week = cook_sell_yesterday
		) # %>%
	# select(-time_child_gathering_nonwood_ite)
}

# Refugee
survey_data_coalesced_cols <- 
	survey_data_coalesce_fcn(survey_data_base)


# Host
survey_data_host_base_missing_col_names <-
	setdiff(names(survey_data_base), names(survey_data_host_base))

survey_data_host_base_missing_cols <- data.frame(matrix(ncol = length(survey_data_host_base_missing_col_names), nrow = nrow(survey_data_host_base) ))
colnames(survey_data_host_base_missing_cols) <- survey_data_host_base_missing_col_names
				 
survey_data_host_base_extra_cols <-
	bind_cols(survey_data_host_base, survey_data_host_base_missing_cols)

survey_data_host_coalesced_cols <-
	survey_data_coalesce_fcn(survey_data_host_base_extra_cols)


# ============================================================================
# Set NAs and ordered values
# ============================================================================

# Function to set NAs and ordered values
survey_data_set_nas_order_fcn <-
	function(df){
		df %>%
			mutate(
				timepoint_num = ifelse(timepoint == "baseline", 0, 1), # endline == 1
				study_arm_overall_num = ifelse(study_arm_overall == "comparison", 0, 1), # new intervention == 1
				did = timepoint_num * study_arm_overall_num # difference in difference indicator
			) %>%
			mutate_at(
				vars(
					all_of(generalhealth_vars), 
					all_of(respiratory_vars),
					UNHCR_card, 
					food_cant_afford_2wk, 
					fuel_cant_afford_2wk,
					window_door_wall, 
					credit_borrow_money_food,
					all_of(fuel_use_non_lpg_type),
					stove_reason_sell_food,  
					all_of(fuel_collection_who),
					all_of(any_harassment_hh),
					harassment_continue, # This just asks the respondent if we can start asking the harassment questions
					all_of(harassment_vars) #, harassment_vars_by_group
				), 
				na_if, 77
			) %>% # 77 = refused -  can't analyze if set to NA like this
			mutate_at(
				vars(
					all_of(generalhealth_vars), 
					all_of(respiratory_vars),
					UNHCR_card, 
					food_cant_afford_2wk, 
					fuel_cant_afford_2wk,
					window_door_wall, 
					credit_borrow_money_food,
					all_of(fuel_use_non_lpg_type),
					stove_reason_sell_food, 
					all_of(fuel_collection_who),
					all_of(any_harassment_hh),
					harassment_continue,
					all_of(harassment_vars) #, harassment_vars_by_group
				), 
				na_if, 99
			) %>% # 99 = don't know - can't analyze if set to NA like this
			
			mutate_at(
				vars(
					borrow_food, reduce_food, 
					reduce_meals_lack_food, not_eat_lack_food, restrict_food,
					borrow_fuel, reduce_fuel, 
					reduce_meals_lack_fuel, not_eat_lack_fuel,
					burn_plastic_frequency
				),
				funs(
					ifelse(food_cant_afford_2wk == 0, 0, .)
				)
			) %>%
			
			
			# For
			# mental_health_vars
			# mental_health_bad_vars
			# mental_health_good_vars
			# 
			# choices: [weekly_choices]     CES-D score for NEGATIVE (bad) feelings   CES-D score for POSITIVE (good) feelings
			# 0	Never                                         0                                    3
			# 1	One or two days                               1                                    2
			# 2	Three to four days                            2                                    1
		# 3	Five to six days                              3                                    0
		# 4	Every day                                     3                                    0
		# 
		# *In the original CES-D (Radloff 1977), the options are
		# Rarely or None of the time (Less than 1 day)         = 0 points for negative feelings
		# Some or a little of the time (1-2 days)              = 1 points for negative feelings
		# Occasionally or a moderate amount of time (3-4 days) = 2 points for negative feelings
		# Most or all of the time (5-7 days)                   = 3 points for negative feelings
		
		mutate_at(
			vars(all_of(mental_health_bad_vars)),
			funs(
				case_when(
					# . == "Never" ~ 0,
					# . == "1-2 days/wk" ~ 1,
					# . == "3-4 days/wk" ~ 2,
					# . == "5-6 days/wk" ~ 3,
					# . == "Every day" ~ 3,
					. == 0 ~ 0,
					. == 1 ~ 1,
					. == 2 ~ 2,
					. == 3 ~ 3,
					. == 4 ~ 4,
					TRUE ~ NA_real_
				)
			)
		) %>%
			mutate_at(
				vars(all_of(mental_health_good_vars)),
				funs(
					case_when(
						# . == "Never" ~ 3,
						# . == "1-2 days/wk" ~ 2,
						# . == "3-4 days/wk" ~ 1,
						# . == "5-6 days/wk" ~ 0,
						# . == "Every day" ~ 0,
						. == 0 ~ 4,
						. == 1 ~ 3,
						. == 2 ~ 2,
						. == 3 ~ 1,
						. == 4 ~ 0,
						TRUE ~ NA_real_
					)
				)
			) %>%
			# rowwise() %>% 
			# mutate(
			# 	CES_D_score = sum(c_across(c(mental_health_bad_vars, mental_health_good_vars))) # use all_of() when using select and use c_across() when using other verbs
			# ) %>%
			# ungroup() %>%
			# mutate(
			# 	CES_D_o16 = ifelse(CES_D_score > 16, 1, 0)
			# ) %>%
			
			
			mutate_at(
				vars(suicidal_thoughts_30),
				funs(
					suicidal_thoughts_30_yn = 
						case_when(
							. == 0 ~ 0,
							TRUE ~ 1
						)
				)
			) %>%
			
			# Sep into "Never" and "Once or more"
			mutate_at(
				vars(all_of(nonrespiratory_vars)),
				funs(
					yn = 
						case_when(
							. == 0 ~ 0,
							is.na(.) ~ NA_real_, # shouldn't be needed
							TRUE ~ 1
						)
				)
			) %>%
			
			# weekly_choices
			mutate_at(
				vars(all_of(mental_health_vars)),
				funs(
					yn = 
						case_when(
							. == 0 ~ 0,
							is.na(.) ~ NA_real_, # shouldn't be needed
							TRUE ~ 1
						)
				)
			) %>%
			
			# yes_no_dk	
			# 0	No
			# 1	Yes
			# 77	Refused
			# 99	Don't know
			
			mutate_at(
				vars(all_of(respiratory_vars), all_of(generalhealth_vars)),
				funs(
					yn = 
						case_when(
							. == 0 ~ 0, # "No"
							. %in% c(77, 99) ~ NA_real_, # "Refused", "Don't know"
							is.na(.) ~ NA_real_,
							TRUE ~ 1 # Yes
						)
				)
			) %>%
			
			# # This is wrong - the hh with that have NA could be NA because they do collect that fuel and no one in the hh was harassed or because they don't collect that fueld
			# # If they do collect that fuel and no one in the hh was harassed, then the value should be 0, not NA
			# mutate_at(
			# 	vars(harassment_vars), # , harassment_vars_by_group
			# 	funs(
			# 		yn = case_when(
			# 			. == 0 ~ 0,
			# 			is.na(.) ~ NA_real_, # Inclues don't know and refused becuase I already set 77 and 99 to NA
			# 			TRUE ~ 1 # includes "Once", "Twice", "Three times", "Four or more times"
			# 		)
		# 	)
		# ) %>%
		
		# mutate_at(
		# 	vars(harassment_vars_yn, harassment_vars_by_group_yn),
		# 	funs(ordered),
		# 	levels = c(0, 1),
		# 	labels = c("Never", "Once or more")
		# ) %>%
		
		mutate_at(
			vars(
				stove_reason_cook_together, 
				stove_boil_drink, stove_boil_bathe, 
				stove_reason_stay_warm,
				soak_rice, soak_lentils, cover_pot, 
				fuel_use_non_lpg_ever, lpg_prior_use, 
				stove_used, stove_training,
				lpg_safety_visit, lpg_gas_leak, 
				gather_wood_dead, forest_collect_not_wood,
				
				all_of(fuel_harassment_hh)
			),
			funs(ordered),
			levels = c(0, 1)#,
			# labels = c("No", "Yes")
		) %>%
			
			mutate(
				fuel_ever_gather_scraps = 
					if_else(
						fuel_ever_gather_scraps == 0 | is.na(fuel_ever_gather_scraps), 
						fuel_30_gather_scraps, 
						fuel_ever_gather_scraps
					),
				fuel_ever_collect_wood = 
					if_else(
						fuel_ever_collect_wood == 0 | is.na(fuel_ever_collect_wood), 
						fuel_30_collect_wood, 
						fuel_ever_collect_wood
					),
				
				buy_wood_cost = 
					ifelse(
						is.na(buy_wood_cost),
						as.double(0),
						buy_wood_cost
					),
				# buy_wood_cost is an integer while fuel_30_buy_wood is a logical (though actually represented by an integer)
				# ifelsse will accept differences in type, if_else will not
				fuel_30_buy_wood = 
					ifelse(
						buy_wood_cost > 1.0, 
						as.integer(1),
						as.integer(0)
					),
				fuel_ever_buy_wood = 
					ifelse( #if_else requires the true and false replacements to be the same type, which is causing trouble
						fuel_ever_buy_wood == 0 | is.na(fuel_ever_buy_wood), 
						fuel_30_buy_wood, 
						fuel_ever_buy_wood
					),
				fuel_ever_receive_wood = 
					ifelse(
						fuel_ever_receive_wood == 0 | is.na(fuel_ever_receive_wood), 
						fuel_30_receive_wood, 
						fuel_ever_receive_wood
					),
				fuel_ever_receive_lpg = 
					ifelse(
						fuel_ever_receive_lpg == 0 | is.na(fuel_ever_receive_lpg), 
						fuel_30_receive_lpg, 
						fuel_ever_receive_lpg
					),
				fuel_ever_buy_lpg = 
					ifelse(
						fuel_ever_buy_lpg == 0 | is.na(fuel_ever_buy_lpg), 
						fuel_30_buy_lpg, 
						fuel_ever_buy_lpg
					),
				fuel_ever_receive_crh = 
					ifelse(
						fuel_ever_receive_crh == 0 | is.na(fuel_ever_receive_crh), 
						fuel_30_receive_crh, 
						fuel_ever_receive_crh
					),
				fuel_ever_buy_crh = 
					ifelse(
						fuel_ever_buy_crh == 0 | is.na(fuel_ever_buy_crh), 
						fuel_30_buy_crh, 
						fuel_ever_buy_crh
					) #,
				
				# first_receive_lpg = ifelse(!is.na(first_receive_lpg), dmy(as.character(first_receive_lpg)), NA_Date_),
				# first_enrolled_lpg = ifelse(!is.na(first_enrolled_lpg), dmy(as.character(first_enrolled_lpg)), NA_Date_),
				# date_safety_training = ifelse(!is.na(date_safety_training), dmy(as.character(date_safety_training)), NA_Date_)
			) %>%
			
			mutate_at(
				vars(
					income_cash_ngo, income_abroad, income_farming, income_own_business,
					income_wage_labor, income_skill_labor, income_handicrafts_tailoring,
					income_humanitarian_asst, income_selling_wood
				),
				funs(as.numeric) # used to be funs()
			) %>%
			rowwise() %>%
			
			mutate(
				
				use_non_lpg_30_days = sum(fuel_30_gather_scraps, fuel_30_collect_wood, fuel_30_buy_wood, fuel_30_buy_lpg, fuel_30_buy_crh, na.rm = TRUE),
				lpg_sufficiency = ifelse(use_non_lpg_30_days > 0, "LPG insufficient", "LPG sufficent"),
				
				income =
					sum(
						income_cash_ngo, income_abroad, income_farming, income_own_business,
						income_wage_labor, income_skill_labor, income_handicrafts_tailoring,
						income_humanitarian_asst, income_selling_wood,
						na.rm = TRUE
					),
				income_lockdown =
					sum(
						income_cash_ngo_lockdown, income_abroad_lockdown, income_farming_lockdown, income_own_business_lockdown,
						income_wage_labor_lockdown, income_skill_labor_lockdown, income_handicrafts_tailoring_lockdown,
						income_humanitarian_asst_lockdown, income_selling_wood_lockdown,
						na.rm = TRUE
					),
				income = replace_na(income, replace = 0),
				
				buy_lpg_cost = replace_na(buy_lpg_cost, replace = 0),
				
				spent_total_month = # If this value is missing, estimate it using the reported monthly costs. this is missing other costs, but that's okay
					ifelse(
						is.na(spent_total_month),
						sum(
							c(
								buy_wood_cost,
								buy_lpg_cost,
								spent_food,
								spent_hh_items,
								spent_hygiene,
								spent_tobacco_pan,
								spent_transport,
								spent_phone,
								na.rm = TRUE
							)
						),
						spent_total_month
					),
				
				# Convert money spent in past 6 months to monthly estimate
				spent_clothing_month = clothing / 6,
				spent_shelter_month = shelter / 6,
				spent_celebrations_month = celebrations / 6,
				spent_debt_month = debt / 6,
				spent_medical_month = medical / 6,
				spent_education_month = education / 6,
				spent_other_month = other_expenditures / 6,
				spent_total_month_with_6mo_monthly =
					sum(
						spent_total_month, # This is the self-reported total for the month, NOT the sum of the other expenditures that were asked about directly (food, hh_items, hygiene, tobacco/pan, transport, phone) - may have included wood and lpg in this estimate or other things. other things may have been captured in "What other expenditures did you have in the past 6 months?) but may not have been
						spent_clothing_month,
						spent_shelter_month,
						spent_celebrations_month,
						spent_debt_month,
						spent_medical_month,
						spent_education_month,
						spent_other_month,
						na.rm = TRUE
					),
				
				spent_other = ifelse(((spent_total_month_with_6mo_monthly - spent_total_month) + spent_other_month) > 0, ((spent_total_month_with_6mo_monthly - spent_total_month) + spent_other_month), 0),
				
				target_child_resp_rate_measured_av =
					mean(
						target_child_resp_rate_measured,
						target_child_resp_rate_measured_2,
						na.rm = TRUE,
						trim = 0 # Don't usually need trim = 0 because that's the default, but not working without it.
					),
				target_child_mid_arm_circ_measured_av =
					mean(
						target_child_mid_arm_circ,
						target_child_mid_arm_circ_2,
						target_child_mid_arm_circ_3,
						na.rm = TRUE,
						trim = 0
					)
			) %>%
			ungroup() %>%
			
			mutate(
				target_child_resp_rate_measured_strange =
					case_when(
						target_child_resp_rate < 10 ~ "low",
						target_child_resp_rate > 20 ~ "high",
						TRUE ~ "normal"
					),
				target_child_mid_arm_circ_measured_strange =
					case_when(
						target_child_mid_arm_circ_measured_av < 80 ~ "low",
						target_child_mid_arm_circ_measured_av > 1500 ~ "high",
						TRUE ~ "normal"
					)
			) %>%
			mutate(
				mattress_yn = if_else(mattress == 0, 0, 1),
				blanket_yn = if_else(blanket == 0, 0, 1),
				mosquito_net_yn = if_else(mosquito_net == 0, 0, 1),
				solar_lamp_yn = if_else(solar_lamp == 0, 0, 1),
				portable_lamp_yn = if_else(portable_lamp == 0, 0, 1),
				umbrella_yn = if_else(umbrella == 0, 0, 1),
				table_yn = if_else(table == 0, 0, 1),
				chair_bench_yn = if_else(chair_bench == 0, 0, 1),
				almirah_wardrobe_show_case_yn = if_else(almirah_wardrobe_show_case == 0, 0, 1),
				electric_fan_yn = if_else(electric_fan == 0, 0, 1),
				refrigerator_yn = if_else(refrigerator == 0, 0, 1),
				shovel_yn = if_else(shovel == 0, 0, 1),
				sickle_yn = if_else(sickle == 0, 0, 1),
				weaving_tool_yn = if_else(weaving_tool == 0, 0, 1),
				fish_net_yn = if_else(fish_net == 0, 0, 1),
				mobile_phone_yn = if_else(mobile_phone == 0, 0, 1),
				smartphone_yn = if_else(smartphone == 0, 0, 1),
				radio_yn = if_else(radio == 0, 0, 1),
				bicycle_yn = if_else(bicycle == 0, 0, 1),
				chicken_duck_pigeon_yn = if_else(chicken_duck_pigeon == 0, 0, 1),
				goat_sheep_yn = if_else(goat_sheep == 0, 0, 1),
				cow_buffalo_yn = if_else(cow_buffalo == 0, 0, 1)
			) %>%
			
			
			mutate(
				food_diversity_starch = 
					ifelse(
						rice_adults_week > 0 | bread_adults_week > 0 | potatoes_adults_week > 0 | corn_adults_week > 0 | lentils_adults_week > 0, 
						1, 
						0
					),
				
				food_diversity_legumes = 
					ifelse(
						lentils_adults_week > 0, 
						1, 
						0
					),
				
				food_diversity_dairy = 
					ifelse(
						dairy_adults_week > 0, 
						1, 
						0
					),
				
				food_diversity_veggies = 
					ifelse(
						veggies_adults_week > 0, 
						1, 
						0
					),
				
				food_diversity_fruit = 
					ifelse(
						fruit_adults_week > 0, 
						1, 
						0
					),
				
				food_diversity_meat = 
					ifelse(
						eggs_adults_week > 0 | fish_adults_week > 0 | poultry_adults_week > 0 | goat_sheep_adults_week > 0 | beef_adults_week > 0, 
						1, 
						0
					),
				
				food_diversity_fat = 
					ifelse(
						oil_adults_week > 0, 
						1, 
						0
					),
				
				food_diversity_sugar = 
					ifelse(
						sugar_adults_week > 0, 
						1, 
						0
					)
			) %>%
			rowwise() %>%
			mutate(
				food_diversity_num_groups = 
					sum(
						food_diversity_starch,
						food_diversity_legumes,
						food_diversity_dairy,
						food_diversity_veggies,
						food_diversity_fruit,
						food_diversity_meat,
						food_diversity_fat,
						food_diversity_sugar
					),
				food_diversity_eight_groups =
					ifelse(food_diversity_num_groups == 7, 1, 0)
			) %>%
			
			ungroup() %>%
			
			##### Relabel #####
		
		# # weekly_choices
		# mutate_at(
		# 	vars(
		# 		all_of(mental_health_vars), 
		# 		
		# 		borrow_food, reduce_food, 
		# 		reduce_meals_lack_food, not_eat_lack_food, restrict_food,
		# 		borrow_fuel, reduce_fuel, 
		# 		reduce_meals_lack_fuel, not_eat_lack_fuel,
		# 		burn_plastic_frequency
		# 	),
		# 	funs(ordered),
		# 	levels = c(0, 1, 2, 3, 4),
		# 	labels = c(
		# 		"Never", "1-2 days/wk", 
		# 		"3-4 days/wk", "5-6 days/wk", 
		# 		"Every day"
		# 	)
		# ) %>%
		# 
		# #illness_freq
		# mutate_at(
		# 	vars(all_of(nonrespiratory_vars)),
		# 	funs(ordered),
		# 	levels = c(0, 1, 2, 3),
		# 	labels =
		# 		c(
		# 			"Never",
		# 			"A few days (1-2 days)",
		# 			"Most days (5-12 days)",
		# 			"Almost every day (13-14 days)"
		# 		)
		# ) %>%
		# 
		# # mutate_at(
		# # 	vars(all_of(nonrespiratory_vars_yn), all_of(mental_health_vars_yn), all_of(respiratory_vars_yn), all_of(generalhealth_vars_yn)),
		# # 	funs(ordered),
		# # 	levels = c(0, 1),
		# # 	labels = c("No", "Yes")
		# # ) %>%
		# 
		# mutate_at(
		# 	vars(starts_with("time_")),
		# 	funs(ordered),
		# 	levels = c(1, 2, 3),
		# 	labels = c("more", "same", "less")
		# ) %>%
		# 
		# mutate_at(
		# 	vars(
		# 		harassment_vars, #harassment_vars_by_group, 
		# 		credit_borrow_money_food
		# 	), 
		# 	funs(factor), 
		# 	levels  = c(0, 1, 2, 3, 4),
		# 	labels = c(
		# 		"Never", "Once", 
		# 		"Twice", "Three times", 
		# 		"Four or more times"
		# 	)
		# ) %>%
		
		
		
		
		
		
		
		
		
		
		
		
		# mutate_at(
		# 	vars(
		# 		UNHCR_card, 
		# 		food_cant_afford_2wk, fuel_cant_afford_2wk,
		# 		window_door_wall, stove_reason_sell_food,  
		# 		harassment_continue
		# 	),
		# 	funs(ordered),
		# 	levels = c(0, 1) # ,
		# 	# labels = c("No", "Yes")
		# ) %>%
		
		# mutate_at(
		# 	vars(food_source),
		# 	funs(factor),
		# 	levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 66),
		# 	labels = c(
		# 		"Don't eat", 
		# 		"Food aid",
		# 		"Purchased",
		# 		"Purchased", #"Purchased with wages",
		# 		"Gift",
		# 		"Borrowed",
		# 		"Own production",
		# 		"Exchanged labor",
		# 		"Exchanged items", 
		# 		"Gathered",
		# 		"Other"
		# 	)
		# ) %>%
		# 
		# mutate_at(
		# 	vars(food_cant_afford_action),
		# 	funs(factor),
		# 	levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 66, 88),
		# 	labels = c(
		# 		"Borrowed food",
		# 		"Reduce food per meal",
		# 		"Reduce meals per day",
		# 		"Skip meals",
		# 		"Restrict adult food",
		# 		"Sold goods",
		# 		"Purchased on credit",
		# 		"Borrowed money",
		# 		"Reduced expenditures",
		# 		"Spent savings",
		# 		"Worked for money for food",
		# 		"Sold or consumed livestock",
		# 		"Exchanged food",
		# 		"Other",
		# 		"Did not need to manage"
		# 	)
		# ) %>%
		# 
		# mutate_at(
		# 	vars(fuel_cant_afford_action),
		# 	funs(factor),
		# 	levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 66, 88),
		# 	labels = c(
		# 		"Borrowed fuel", # Response was "borrow food", which is also possible, but I'm going to interpret as "borrow fuel"
		# 		"Reduce food per meal",
		# 		"Reduce meals per day",
		# 		"Skip meals",
		# 		"Restrict adult food",
		# 		"Sold goods",
		# 		"Purchased on credit",
		# 		"Borrowed money",
		# 		"Reduced expenditures",
		# 		"Spent savings",
		# 		"Worked for money for fuel",
		# 		"Sold livestock to purchase fuel",
		# 		"Sold food to purchase fuel",
		# 		"Eat food that doesn't need to be cooked",
		# 		"Eat food that wasn't fully cooked",
		# 		"Other",
		# 		"Did not need to manage"
		# 	)
		# ) %>%
		# 
		# mutate_at(
		# 	vars(credit, fuel_use_non_lpg_type),
		# 	funs(ordered),
		# 	levels = c(0, 1), # , 99
		# 	labels = c("No", "Yes") # , "DK"
		# ) %>%
		
		# Rename vars that have a slash in them
		rename_with(.fn = ~ str_replace_all(., "/", "_"), .cols = contains("/")) %>%
			
			# Don't know why I need this again as it is also above
			mutate(
				hh_id = as.character(hh_id)
			) %>%
			select(fcn_id, everything())
	}

# survey_data_1 %>%
# 	select(first_receive_lpg, first_enrolled_lpg) %>% 
# 	# arrange(desc()) %>%
# 	View()
# 
# 
# survey_data_1 %>%
# 	mutate(
# 												first_receive_lpg = ifelse(!is.na(first_receive_lpg), lubridate::dmy(first_receive_lpg), NA_Date_),
# 				first_enrolled_lpg = ifelse(!is.na(first_enrolled_lpg), dmy(first_enrolled_lpg), NA_Date_),
# 				date_safety_training = ifelse(!is.na(date_safety_training), dmy(date_safety_training), NA_Date_)
# 	) %>%
# 	select(first_receive_lpg, first_enrolled_lpg) %>% 
# 	View()


###################### Adding in endline data to tidying of data done above ##############
## Edited 7/17/22

#R is reading in the select multiple split entries as the same as an _ which makes the select multiple individual option read in the same way as a separate question about pregnancy location. Will fix this 
# survey_data_coalesced_cols[,2343]
# survey_data_coalesced_cols[,2638]


#Replace all / with . in the dataset. 
## I feel like i should put this above but it will reduce a lot of the cleaning data that had been done earlier so im leaving it
# survey_data_coalesced_cols <- 
# 	survey_data_coalesced_cols %>% 
# 	rename_with(., ~gsub("/", ".", .x, fixed = TRUE))
# 
# survey_data_host_coalesced_cols <- 
# 	survey_data_host_coalesced_cols %>% 
# 	rename_with(., ~gsub("/", ".", .x, fixed = TRUE))

survey_data_host_clean <-
	survey_data_set_nas_order_fcn(survey_data_host_coalesced_cols)

survey_data_host_clean %>% count(study_arm_overall, timepoint)

# 1127 failed to parse in two columns, not sure which two!



write_rds(survey_data_host_clean, file_out_2)



