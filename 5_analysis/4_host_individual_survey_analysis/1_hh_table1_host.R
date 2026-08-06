# Host hh table 

# Author: Chris LeBoa 
# Version: 2022-11-09

# Libraries
library(tidyverse)
library(tableone)

# Parameters
source(here::here("0_config.R"))
file_in <- here::here("4_data/RohingyaFuel_survey_data_long_paired_host.rds")

survey_data_host <-read_rds(file_in) 
#===============================================================================


survey_data_tab1_host <-
	survey_data_host %>%
	filter(timepoint == "baseline") %>%
	# Convert to USD
	mutate(
		income_USD = income / BDT_USD_exchange_rate_baseline,
		spent_total_month_USD = spent_total_month/ BDT_USD_exchange_rate_baseline,
		debt_USD = debt / BDT_USD_exchange_rate_baseline
	) %>% 
	select(
		c(
			"study_arm_overall",
			"target_child_sex",
			"target_child_months",
			"hh_size", "hh_size_2mo_u5", "hh_size_u2mo",
			"hh_ppl_smoke",
			"electricity",
			"electric_fan_yn",
			"smartphone_yn", "mobile_phone_yn", "radio_yn",   
			"portable_lamp_yn", "solar_lamp_yn", 
			"mattress_yn", "blanket_yn", "mosquito_net_yn",
			"umbrella_yn",
			"chair_bench_yn", "table_yn", "almirah_wardrobe_show_case_yn",
			"shovel_yn", "sickle_yn", "weaving_tool_yn",
			"chicken_duck_pigeon_yn",
			"income_USD",
			"spent_total_month_USD",
			"debt_USD"
		)
	) 


names(survey_data_tab1_host) <- 
	c(
		"Study arm",
		"Female", "Age (mo)", 
		"Number of household members", 
		"Number of hh members 2-59 months",
		"Number of household members 0 < 2 months",
		"Number of household members who smoke",
		"House has solar electricity",
		"Has 1+ electric fan", 
		"Has 1+ smartphone", 
		"Has 1+ mobile phone",
		"Has 1+ radio",
		"Has 1+ portable lamp",
		"Has 1+ solar lamp",
		"Has 1+ mattress", 
		"Has 1+ blanket",
		"Has 1+ mosquito net",
		"Has 1+ umbrella", 
		"Has 1+ chair/bench", 
		"Has 1+ table", 
		"Has 1+ wardrobe", 
		"Has 1+ shovel", 
		"Has 1+ sickle", 
		"Has 1+ weaving tool", 
		"Has 1+ poultry",
		"Monthly income [USD]",
		"Monthly expenditure [USD]",
		"Total debt [USD]"
	)

table1Vars <- 
	c(
		"Study arm",
		"Female", "Age (mo)", 
		"Number of household members", 
		"Number of hh members 2-59 months",
		"Number of household members 0 < 2 months",
		"Number of household members who smoke",
		"House has solar electricity",
		"Has 1+ electric fan", 
		"Has 1+ smartphone", 
		"Has 1+ mobile phone",
		"Has 1+ radio",
		"Has 1+ portable lamp",
		"Has 1+ solar lamp",
		"Has 1+ mattress", 
		"Has 1+ blanket",
		"Has 1+ mosquito net",
		"Has 1+ umbrella", 
		"Has 1+ chair/bench", 
		"Has 1+ table", 
		"Has 1+ wardrobe", 
		"Has 1+ shovel", 
		"Has 1+ sickle", 
		"Has 1+ weaving tool", 
		"Has 1+ poultry",
		# "Has home garden",
		"Monthly income [USD]",
		"Monthly expenditure [USD]",
		"Total debt [USD]"
	)

table1FactorVars <- 
	c(
		"Female", 
		"House has solar electricity",
		"Has 1+ electric fan", 
		"Has 1+ smartphone", 
		"Has 1+ mobile phone",
		"Has 1+ radio",
		"Has 1+ portable lamp",
		"Has 1+ solar lamp",
		"Has 1+ mattress", 
		"Has 1+ blanket",
		"Has 1+ mosquito net",
		"Has 1+ umbrella", 
		"Has 1+ chair/bench", 
		"Has 1+ table", 
		"Has 1+ wardrobe", 
		"Has 1+ shovel", 
		"Has 1+ sickle", 
		"Has 1+ weaving tool", 
		"Has 1+ poultry"
	)

## Create a TableOne object
tab1_baseline_host <- 
	CreateTableOne(
		data = survey_data_tab1_host, 
		vars = table1Vars, factorVars = table1FactorVars
	)

tab1_baseline_host
# Not that no family had a radio so the table has radio_yn = 0

# Col names are still a bit off but not time/patience to fix them now. Will fix them manually on latex
kable(
	print(tab1_baseline_host, printToggle = FALSE, noSpaces = TRUE), 
	booktabs = TRUE,
	digits = 1,
	align = c("l", "c", "C", "c", "c"), 
	format = "latex"
) %>%
	kable_styling(latex_options = c("scale_down")) # "striped",

tab_csv <- print(tab1_baseline_host)

write.csv(tab_csv, "/Volumes/GoogleDrive/.shortcut-targets-by-id/1g4FGyxS0paoGycT8REXc9ZEXkWLVDMJr/Rohingya_analysis/Table_1_221206_host.csv")

