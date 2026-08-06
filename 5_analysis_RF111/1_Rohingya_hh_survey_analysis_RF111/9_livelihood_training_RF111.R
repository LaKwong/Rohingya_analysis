################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Fuel sources analysis
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
source(here::here("1_config.R"))


# Parameters

data_hh_member_filename <- here::here("4_data/RohingyaFuel_data_hh_member.rds")

data_hh_member_host_filename <- here::here("4_data/RohingyaFuel_data_hh_member_host.rds")

file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_paired.rds")
#===============================================================================


# Load input files
data_hh_member_base <- read_rds(data_hh_member_filename)

data_hh_member_host_base <- read_rds(data_hh_member_host_filename)

survey_data <- read_rds(file_survey_data_base)


################################################################################
################################################################################


# Fuel sources
# Percentage of hh that have used the following fuel sources in the past 30 days

survey_data %>%
	count(fuel_ever_collect_wood == 1)
# Why are 393 NA? because we added the question late?

## Vector of variables to summarize
# Use Hmisc::Cs to quote each variable
table7Vars <- 
	Hmisc::Cs(
		fuel_ever_scraps,
		fuel_ever_collect_wood,
		fuel_ever_receive_wood,
		fuel_ever_buy_wood,
		fuel_ever_receive_lpg,
		fuel_ever_buy_lpg,
		fuel_ever_receive_crh,
		fuel_ever_buy_crh,
		fuel_ever_other
	)

## Vector of categorical variables that need transformation
table7FactorVars <- table7Vars

# Create a TableOne object
tab7 <- 
	CreateTableOne(
		data = survey_data, 
		vars = table7Vars, 
		factorVars = table7FactorVars, 
		strata = "study_arm"
	)

tab7

## Vector of variables to summarize
# Use Hmisc::Cs to quote each variable
table8Vars <- 
	Hmisc::Cs(
		fuel_30_gather_scraps,
		fuel_30_collect_wood,
		fuel_30_receive_wood,
		fuel_30_buy_wood,
		fuel_30_receive_lpg,
		fuel_30_buy_lpg,
		fuel_30_receive_crh,
		fuel_30_buy_crh,
		fuel_30_other
	)

## Vector of categorical variables that need transformation
table8FactorVars <- table8Vars

# Create a TableOne object
tab8 <- 
	CreateTableOne(
		data = survey_data, 
		vars = table8Vars, 
		factorVars = table8FactorVars, 
		strata = "study_arm"
	)

tab8


# Based on IDIs and FGDs, "other is likely plastic

survey_data %>%
	tabyl(burn_plastic_frequency, study_arm)

# Almost 150 of 393 hh report burning househols every day! We need to better understand how many meals and if they are relying on plastic for all of the fuel they need or if it is used in combination with other fuel sources


# Who gathers fuel

# Not mutally exclusive

## Vector of variables to summarize
# Use Hmisc::Cs to quote each variable
table9Vars <- 
	Hmisc::Cs(
		gather_scraps_w, gather_scraps_m, gather_scraps_g, gather_scraps_b,
		collect_wood_w, collect_wood_m, collect_wood_g, collect_wood_b,
		buy_wood_w, buy_wood_m, buy_wood_g, buy_wood_b,
		receive_wood_w, receive_wood_m, receive_wood_g, receive_wood_b,
		receive_lpg_w, receive_lpg_m, receive_lpg_g, receive_lpg_b,
		buy_lpg_w, buy_lpg_m, buy_lpg_g, buy_lpg_b,
		receive_crh_w, receive_crh_m, receive_crh_g, receive_crh_b,
		buy_crh_w, buy_crh_m, buy_crh_g, buy_crh_b
	)

## Vector of categorical variables that need transformation
table9FactorVars <- table9Vars

# Create a TableOne object
tab9 <- 
	CreateTableOne(
		data = survey_data, 
		vars = table9Vars, 
		factorVars = table9FactorVars #, 
		# strata = "study_arm"
	)

tab9

# # Add N for each fuel type
# survey_data %>%
# 	summarise_at(
# 		vars(
# 			gather_scraps_w, gather_scraps_m, gather_scraps_g, gather_scraps_b,
# 			collect_wood_w, collect_wood_m, collect_wood_g, collect_wood_b,
# 			buy_wood_w, buy_wood_m, buy_wood_g, buy_wood_b,
# 			receive_wood_w, receive_wood_m, receive_wood_g, receive_wood_b,
# 			receive_lpg_w, receive_lpg_m, receive_lpg_g, receive_lpg_b,
# 			buy_lpg_w, buy_lpg_m, buy_lpg_g, buy_lpg_b,
# 			receive_crh_w, receive_crh_m, receive_crh_g, receive_crh_b,
# 			buy_crh_w, buy_crh_m, buy_crh_g, buy_crh_b
# 		),
# 		list(mean),
# 		na.rm = TRUE
# 	) %>%
# 	gather(key = "gather_who", value = "pc_hh")
# 
# # Use kable to make this into four columns with women, men, girls, and boys at the top. 


# Men are primarily responsible for gathering all types of fuel; we should see that more women are getting LPG as some camps (like 8w) require that women procure the LPG. 
