################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Specify basic parameters for the 
# @Date: 210309
################################################################################
rm(list = ls())

#here::set_here("/Volumes/GoogleDrive/.shortcut-targets-by-id/1g4FGyxS0paoGycT8REXc9ZEXkWLVDMJr/Rohingya_analysis/")
#setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1g4FGyxS0paoGycT8REXc9ZEXkWLVDMJr/Rohingya_analysis/")
#This is not working and may be the same issues as described on this stack overflow but I haven't been able to figure out how to fix it 
getwd()
#working directory is set to the right place. 

#here::here("/Volumes/GoogleDrive/.shortcut-targets-by-id/1g4FGyxS0paoGycT8REXc9ZEXkWLVDMJr/Rohingya_analysis")

#here::i_am("/Volumes/GoogleDrive/.shortcut-targets-by-id/1g4FGyxS0paoGycT8REXc9ZEXkWLVDMJr/Rohingya_analysis/")

here::here()

# Parameters

file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_paired.rds")
#===============================================================================


# Load input files

survey_data <- read_rds(file_survey_data_base)

init_here <- function() {
  `%!in%` = Negate(`%in%`)
  files <- dir( all.files = T )
  while ( ".here" %!in% files & getwd()!="/" ) {
    setwd("..")
    files <- dir( all.files = T )
  }
  library(here)
}
init_here()

# Constants

BDT_USD_exchange_rate <- 84.88 # as of 2 Nov 2020
#############################################################################
BDT_USD_exchange_rate_baseline <- 84.88 # as of 2 Nov 2020
BDT_USD_exchange_rate_midline <- 84.74 #as of 21 August 2021
BDT_USD_exchange_rate_endline <- 93.45 #as of 4 July 2022



# lists of variables

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

income_monthly_vars <-
	c(
		"income",
		"income_cash_ngo", "income_wage_labor",
		"income_skill_labor", "income_own_business", "income_abroad",
		"income_handicrafts_tailoring", "income_farming", 
		"income_humanitarian_asst", "income_selling_wood",  
		"income_home_garden_amt"
	)


income_monthly_lockdown_vars <-  
	map_chr(income_monthly_vars, ~ paste0(., "_lockdown"))

spent_monthly_vars <- 
	c(
		"spent_total_month_with_6mo_monthly", 
		"spent_total_month",
		"spent_food",
		"buy_wood_cost", # "In the past 30 days, how much money (taka) did you spend on firewood?"
		"buy_lpg_cost",
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
		"spent_other" # includes #spent_other_month and the difference between recalled calculated spent_total_month_with_6mo_monthly and recalled spent_total_month 
	)

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


food_types_consumed_vars <- 
	c(		
		"rice_adults_week", "bread_adults_week", "corn_adults_week", "potatoes_adults_week", 
		"lentils_adults_week", "veggies_adults_week", "fruit_adults_week", "eggs_adults_week", 
		"fish_adults_week", "poultry_adults_week", "goat_sheep_adults_week", "beef_adults_week", 
		"dairy_adults_week", "sugar_adults_week", "oil_adults_week"
	)


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
		"food_cant_afford_action"
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
	survey_data %>% 
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
	survey_data %>%
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
			gather_scraps_dead, # gather_scraps_dead.1, #gather_scraps_dead1,
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
