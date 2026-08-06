################################################################################
# @Project: Rohingya analysis
# @Author: Chris LeBoa 
# @Description: 
# This document describes the enrollment characteristics and baseline household characteristics that are used for the analysis of RF105
# @Date: 220109
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))
source(here::here("5_analysis/Dif_in_dif_fxn.R"))

# File locations
file_survey_data_all <- here::here("4_data/RohingyaFuelMaster_survey_data_tidy.rds")
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")

#Read in values
survey_data <- read_rds(file_survey_data_base)
survey_data_all <- read_rds(file_survey_data_all)

survey_data %>% group_by(timepoint) %>% 
	summarize(median(start_date))

survey_data %>% group_by(timepoint) %>% 
  summarize(median(income, ra.rm = TRUE))

survey_data %>% group_by(timepoint) %>% 
  summarize(mean(buy_wood_cost_bundle, ra.rm = TRUE))

4500/BDT_USD_exchange_rate_endline
100/BDT_USD_exchange_rate_endline

(1.07*3*4) / 48.15

survey_data$buy_wood_cost_bundle
#The overall enrollment numbers at each time point 
survey_data_all %>%
	count(timepoint, study_arm_overall)

# 1 baseline  comparison          598
# 2 baseline  intervention        594
# 3 midline   comparison          580
# 4 midline   intervention        565
# 5 endline   comparison          457
# 6 endline   intervention        497


#The number of households that were able to match of triplicate
survey_data %>%
	filter(timepoint == "baseline") %>%
	count(study_arm_overall)

survey_data %>% 
  group_by(timepoint) %>% 
  summarise(median(lpg_willingness_to_pay, na.rm = TRUE), sum(lpg_willingness_to_pay == 0, na.rm = TRUE), n()) 
  
# study_arm_overall     n
# <ord>             <int>
# 1 comparison          437
# 2 intervention        494


## Note: If you want to center-align values in Word, use noSpaces option.
# print(tableOne, # nonnormal = c(nonnormal vars, each in quotes)
# exact = c("status","stage"), quote = TRUE, noSpaces = TRUE) # quote = TRUE so excel doesn't mess up the cells; noSpaces == TRUE to center-align


###############################################################################
# # Descriptive statistics
###############################################################################


survey_data_tab1 <-
	survey_data %>%
	filter(timepoint == "baseline") %>% 
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
			"income",
			"spent_total_month",
			"debt"
		)
	) %>%
	# Convert to USD
	mutate(
		income_USD = income / BDT_USD_exchange_rate_baseline,
		spent_total_month_USD = spent_total_month/ BDT_USD_exchange_rate_baseline,
		debt_USD = debt / BDT_USD_exchange_rate_baseline
	)


names(survey_data_tab1) <- 
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
	
	# Should change this to USD
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

#Create Table 1 analysis tables
#Comparing intervention and control groups at baseline 

## Create a TableOne object
tab1_baseline <- 
	CreateTableOne(
		data = survey_data_tab1, 
		vars = table1Vars, factorVars = table1FactorVars, strata = "Study arm"
	)

tab1_baseline
# Not that no family had a radio so the table has radio_yn = 0



# Col names are still a bit off but not time/patience to fix them now. Will fix them manually on latex
kable(
	print(tab1_baseline, printToggle = FALSE, noSpaces = TRUE), 
	booktabs = TRUE,
	digits = 1,
	align = c("l", "c", "C", "c", "c"), 
	format = "latex"
) %>%
	kable_styling(latex_options = c("scale_down")) # "striped",

tab_csv <- print(tab1_baseline)

write.csv(tab_csv, "Table_1_230109_bl.csv")

# Make a table where the unit of comparison is the timepoint and not the study 
#arm for this - we reduce to only IOM hh

survey_data_tab1_int <-
	survey_data %>%
#	filter(study_arm_overall == "intervention") %>% 
	select(
		c(
			"timepoint",
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
			"income",
			"spent_total_month",
			"debt"
		)
	) %>%
	# Convert to USD
	mutate(
		income_USD = income / BDT_USD_exchange_rate_baseline,
		spent_total_month_USD = spent_total_month/ BDT_USD_exchange_rate_baseline,
		debt_USD = debt / BDT_USD_exchange_rate_baseline
	)

names(survey_data_tab1_int) <- 
	c(
		"Timepoint",
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
		
		# Should change this to USD
		"Monthly income [USD]",
		"Monthly expenditure [USD]",
		"Total debt [USD]"
	)

tab1_int <- 
	CreateTableOne(
		data = survey_data_tab1_int, 
		vars = table1Vars, factorVars = table1FactorVars, strata = "Timepoint"
	)

tab1_int

tab_csv <- print(tab1_int)

write.csv(tab_csv, "Table_1_230109_time.csv")


### Confidence intervals of differences we discuss in paper 

# HH size 
#input sample size, sample mean, and sample standard deviation

n <- 437
xbar <- 5.33
s <- survey_data %>% filter(timepoint == "baseline" & study_arm_overall == "comparison") %>% summarize(sd(hh_size)) %>% pull
#calculate margin of error
margin <- qt(0.975,df=n-1)*s/sqrt(n)
#calculate lower and upper bounds of confidence interval
low <- xbar - margin
low

high <- xbar + margin
high


n <- 494
xbar <- 5.58
s <- survey_data %>% filter(timepoint == "baseline" & study_arm_overall == "intervention") %>% summarize(sd(hh_size)) %>% pull
#calculate margin of error
margin <- qt(0.975,df=n-1)*s/sqrt(n)
#calculate lower and upper bounds of confidence interval
low <- xbar - margin
low

high <- xbar + margin
high


### HH solar energy 
#input sample size and sample proportion
n <- 494
p <- .41

#calculate margin of error
margin <- qnorm(0.975)*sqrt(p*(1-p)/n)

#calculate lower and upper bounds of confidence interval
low <- p - margin
low

high <- p + margin
high

### HH electric fan 
#input sample size and sample proportion
n <- 437
p <- .22

#calculate margin of error
margin <- qnorm(0.975)*sqrt(p*(1-p)/n)

#calculate lower and upper bounds of confidence interval
low <- p - margin
low

high <- p + margin
high

#input sample size and sample proportion
n <- 494
p <- .08

#calculate margin of error
margin <- qnorm(0.975)*sqrt(p*(1-p)/n)

#calculate lower and upper bounds of confidence interval
low <- p - margin
low

high <- p + margin
high


#Confidence int. proportion 
conf_int_prop_solar <- 
survey_data %>% 
	group_by(study_arm_overall, timepoint) %>% 
	summarise(
		n = n(), 
		p = sum(solar_lamp_yn) / n, 
		margin = qnorm(0.975)*sqrt(p*(1-p)/n), 
		low = p - margin, 
		high = p + margin
	) %>% 
	filter(timepoint == "baseline")

###########################################
# The following confidence intervals are used to look at baseline fuel use among populations


#Confidence int. proportion LPG Receive 
conf_int_prop_lpg_receive <- 
	survey_data %>% 
	group_by(study_arm_overall, timepoint) %>% 
	summarise(
		n = n(), 
		p = sum(fuel_30_receive_lpg) / n, 
		margin = qnorm(0.975)*sqrt(p*(1-p)/n), 
		low = p - margin, 
		high = p + margin
	) %>% 
	filter(timepoint == "baseline")

conf_int_prop_lpg_receive

#Confidence int. proportion LPG Receive 
conf_int_prop_lpg_receive <- 
	survey_data %>% 
	group_by(study_arm_overall, timepoint) %>% 
	summarise(
		n = n(), 
		p = sum(fuel_30_buy_lpg) / n, 
		margin = qnorm(0.975)*sqrt(p*(1-p)/n), 
		low = p - margin, 
		high = p + margin
	) %>% 
	filter(timepoint == "baseline")

conf_int_prop_lpg_receive

conf_int_prop_gather_scrap <- 
	survey_data %>% 
	group_by(study_arm_overall, timepoint) %>% 
	summarise(
		n = n(), 
		p = sum(fuel_30_gather_scraps) / n, 
		margin = qnorm(0.975)*sqrt(p*(1-p)/n), 
		low = p - margin, 
		high = p + margin
	) %>% 
	filter(timepoint == "baseline")


conf_int_prop_buy_wood <- 
	survey_data %>% 
	group_by(study_arm_overall, timepoint) %>% 
	summarise(
		n = n(), 
		p = sum(fuel_30_buy_wood) / n, 
		margin = qnorm(0.975)*sqrt(p*(1-p)/n), 
		low = p - margin, 
		high = p + margin
	) %>% 
	filter(timepoint == "baseline")

conf_int_prop_gather_wood <- 
	survey_data %>% 
	group_by(study_arm_overall, timepoint) %>% 
	summarise(
		n = n(), 
		p = sum(fuel_30_collect_wood) / n, 
		margin = qnorm(0.975)*sqrt(p*(1-p)/n), 
		low = p - margin, 
		high = p + margin
	) %>% 
	filter(timepoint == "baseline")

conf_int_prop_gather_wood

conf_int_prop_gather_wood <- 
	survey_data %>% 
	group_by(study_arm_overall, timepoint) %>% 
	summarise(
		n = n(), 
		p = sum(fuel_30_collect_wood) / n, 
		margin = qnorm(0.975)*sqrt(p*(1-p)/n), 
		low = p - margin, 
		high = p + margin
	) %>% 
	filter(timepoint == "baseline")

conf_int_prop_gather_wood

conf_int_prop_purchase_crh <- 
	survey_data %>% 
	group_by(study_arm_overall, timepoint) %>% 
	summarise(
		n = n(), 
		p = sum(fuel_30_buy_crh) / n, 
		margin = qnorm(0.975)*sqrt(p*(1-p)/n), 
		low = p - margin, 
		high = p + margin
	) %>% 
	filter(timepoint == "baseline")

conf_int_prop_purchase_crh

conf_int_prop_plastic <- 
	survey_data %>% 
	group_by(study_arm_overall, timepoint) %>% 
	summarise(
		n = n(), 
		p = sum(fuel_30_other) / n, 
		margin = qnorm(0.975)*sqrt(p*(1-p)/n), 
		low = p - margin, 
		high = p + margin
	) %>% 
	filter(timepoint == "baseline")

conf_int_prop_plastic


##########################
#Midline fuel use numbers 
##########################

conf_int_prop_lpg <- 
	survey_data %>% 
	group_by(study_arm_overall, timepoint) %>% 
	summarise(
		n = n(), 
		p = sum(fuel_30_receive_lpg) / n, 
		margin = qnorm(0.975)*sqrt(p*(1-p)/n), 
		low = p - margin, 
		high = p + margin
	)

conf_int_prop_lpg

## Gather scraps 
conf_int_prop_gather_scrap <- 
	survey_data %>% 
	group_by(study_arm_overall, timepoint) %>% 
	summarise(
		n = n(), 
		p = sum(fuel_30_gather_scraps) / n, 
		margin = qnorm(0.975)*sqrt(p*(1-p)/n), 
		low = p - margin, 
		high = p + margin
	) 

conf_int_prop_gather_scrap 

## Collect wood 
conf_int_prop_buy_wood <- 
	survey_data %>% 
	group_by(study_arm_overall, timepoint) %>% 
	summarise(
		n = n(), 
		p = sum(fuel_30_buy_wood) / n, 
		margin = qnorm(0.975)*sqrt(p*(1-p)/n), 
		low = p - margin, 
		high = p + margin
	) 

conf_int_prop_buy_wood

## Collect wood 
conf_int_prop_collect_wood <- 
	survey_data %>% 
	group_by(study_arm_overall, timepoint) %>% 
	summarise(
		n = n(), 
		p = sum(fuel_30_collect_wood) / n, 
		margin = qnorm(0.975)*sqrt(p*(1-p)/n), 
		low = p - margin, 
		high = p + margin
	) 

conf_int_prop_collect_wood

### Collection of wood difference in difference
dind_fxn("fuel_30_collect_wood", model_data)


######### The next steps of analysis in the paper come from the script 3.2 fuel sources


