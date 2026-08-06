################################################################################
# @Project: Rohingya analysis
# @Author: Laura H Kwong
# @Description: Firewood collection, cost of wood in the market, reasons for using wood
# @Date: 210309
################################################################################
rm(list = ls())
source(here::here("0_config.R"))
## Chris
# source(here::here("1_config.R"))

source(here::here("3_data_cleaning/1.5_define_vector_columns.R"))

# Parameters
file_survey_data_all <- here::here("4_data/RohingyaFuelMaster_survey_data_tidy.rds")
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")


file_out_1 <- here::here("4_data/study_arm_camp_id_errors_220724.csv")
file_out_2 <- here::here("4_data/camp_block_id_errors_220724.csv")
camp_block_id_path <- here::here("4_data/camp_block_id.csv")
camp_block_sublock_id_path <- here::here("4_data/camp_block_sublock_id.csv")
camp_block_subblock_id_100_largest_path <- here::here("4_data/camp_block_subblock_id_100_largest.csv")

## Chris
# data_hh_member_filename <- here::here("4_data/RohingyaFuel_data_hh_member.rds")
# 
# data_hh_member_host_filename <- here::here("4_data/RohingyaFuel_data_hh_member_host.rds")
# 
# file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_paired.rds")
#===============================================================================



# =====================================================================================
#### Get the differences for each var ####
# =====================================================================================


# Load input files
# data_hh_member_base <- read_rds(data_hh_member_filename)
# 
# data_hh_member_host_base <- read_rds(data_hh_member_host_filename)

survey_data_all <- read_rds(file_survey_data_all)
survey_data <- read_rds(file_survey_data_base)


survey_data_baseline <-
	survey_data %>%
	filter(timepoint == "baseline")

survey_data %>%
	filter(timepoint == "baseline") %>%
	count(study_arm_overall)
# For each baseline, midline, endline there are 305 comparison hh and 486 intervention hh
# 791 hh with triplicate measures

## If you want to center-align values in Word, use noSpaces option.
# print(tableOne, # nonnormal = c(nonnormal vars, each in quotes)
# 			exact = c("status","stage"), quote = TRUE, noSpaces = TRUE) # quote = TRUE so excel doesn't mess up the cells; noSpaces == TRUE to center-align



survey_data_hh <-
	survey_data_all %>%
	mutate(
		study_arm_overall = 
			ordered(
				study_arm_overall,
				labels = c("comparison group", "intervention group")
			)
	) %>%
	tabyl(timepoint, study_arm_overall)

survey_data_hh %>%
	kable(
caption = "Number of households surveyed at each timepoint",
	booktabs = TRUE,
	digits = 1,
	align = c("l", "c", "C"),
	format = "latex"
) %>%
	kable_styling(latex_options = c("scale_down")) # "striped",



################################################################################
# Where are the hh located?
################################################################################
survey_data_all %>%
	tabyl(timepoint, camp_id, study_arm_overall)

# study_arm_overall == intervention: camp_id %in% ("8w", "9", "10)
# study_arm_overall == comparison: camp_id %in% ("8E", "18", "3", "4", "5") # 3,4,5 are UNCHR; others are IOM


# There are a number of hh that appear to have the wrong study_arm assigned. 
# E.g. there are hh labeled as intervention that are in camps 3, 4, 5, 8E, which are all comparison camps

study_arm_error_all <-
	survey_data_all %>%
	mutate(
		anomolous = 
			case_when(
				timepoint == "endline" & study_arm_overall == "intervention" & camp_id == "3" ~ 1,
				timepoint == "endline" & study_arm_overall == "intervention" & camp_id == "4" ~ 1,
				timepoint == "endline" & study_arm_overall == "intervention" & camp_id == "5" ~ 1,
				timepoint == "endline" & study_arm_overall == "intervention" & camp_id == "8E" ~ 1,
				timepoint == "endline" & study_arm_overall == "comparison" & camp_id == "8W" ~ 1,
				timepoint == "endline" & study_arm_overall == "comparison" & camp_id == "9" ~ 1,
				timepoint == "endline" & study_arm_overall == "comparison" & camp_id == "10" ~ 1,
				TRUE ~ 0
			)
	) %>%
	select(study_arm_overall, timepoint, camp_id, block_id, subblock_id, fcn_id, name_mahji, name_respondent, name_hh_head, target_child_name, anomolous) %>%
	filter(anomolous == 1) %>%
	select(-anomolous)

# write.csv(study_arm_error_all, file_out_1)

################################################################################
# Where are the hh located?
################################################################################

# Check for errors by examining all the data
camp_block_id_all <-
	survey_data_all %>%
	# tabyl(camp_id)
	# tabyl(camp_id, lpg_depots) # lpg_depots = "What LPG distribution center do you go to?
	tabyl(camp_id, block_id)
# unite(block_sub_block_id, c("block_id", "subblock_id")) %>%
# tabyl(camp_id, block_sub_block_id)

camp_block_id_all_errors <-
	survey_data_all %>%
	mutate(
		anomolous = 
			case_when(
				camp_id == "MOJIMULLAH" ~ 1,
				camp_id == "L18" ~ 1,
				camp_id == "G" ~ 1,
				camp_id == "10" & block_id == "e" ~ 1,
				camp_id == "4" & block_id == "b" ~ 1,
				camp_id == "8W" & block_id == "c" ~ 1,
				camp_id == "8W" & block_id == "g" ~ 1,
				camp_id == "9" & block_id == "b" ~ 1,
				camp_id == "9" & block_id == "c" ~ 1,
				camp_id == "9" & block_id == "i" ~ 1,
				camp_id == "9" & block_id == "c" ~ 1,
				TRUE ~ 0
			)
	) %>%
	select(study_arm_overall, timepoint, camp_id, block_id, subblock_id, fcn_id, name_mahji, name_respondent, name_hh_head, target_child_name, anomolous) %>%
	filter(anomolous == 1) %>%
	select(-anomolous)

# write.csv(camp_block_id_all_errors, file_out_2)





### Now only the paired data

camp_org <-
	survey_data %>%
	tabyl(timepoint, camp_id, org)

# org_lpg_insufficient <-
# 	survey_data %>%
# 	filter(study_arm %in% c("post-intervention", "intervention follow-up")) %>%
# 	tabyl(org, lpg_sufficiency) %>%
# 	adorn_percentages("row") %>%
# 	adorn_pct_formatting(digits = 1) %>%
# 	adorn_ns()


camp_block_id <-
	survey_data %>%
	# tabyl(camp_id)
	# tabyl(camp_id, lpg_depots) # lpg_depots = "What LPG distribution center do you go to?
	tabyl(camp_id, block_id)
# unite(block_sub_block_id, c("block_id", "subblock_id")) %>%
# tabyl(camp_id, block_sub_block_id)

write.csv(camp_block_id, camp_block_id_path)


camp_block_subblock_id <-
	survey_data %>%
	group_by(camp_id, block_id, subblock_id) %>% 
	count() %>%
	ungroup() %>%
	arrange(camp_id, block_id, subblock_id) 
# tabyl(camp_id)
# tabyl(camp_id, lpg_depots) # lpg_depots = "What LPG distribution center do you go to?
# tabyl(camp_id, block_id)

write.csv(camp_block_subblock_id, camp_block_sublock_id_path)

# For the Rohingya COVID-19 proposal, we need the 100 sub-blocks with the most hh interviewed
camp_block_subblock_id_100_largest <-
	camp_block_subblock_id %>%
	arrange(desc(n)) %>%
	slice(1:100) %>%
	arrange(camp_id, block_id, subblock_id)

write.csv(camp_block_subblock_id_100_largest, camp_block_subblock_id_100_largest_path)

# survey_data %>% filter(camp_id == "9", block_id == "g") %>% select(fcn_id, camp_id, block_id, subblock_id, SubmissionDate, enumerator_name)



###############################################################################
# Access to phones
#################################################################################
# We didn't collect phone numbers (phone are not allowed so we didn't collect phone numbers)

survey_data %>% select(hh_id, mobile_phone_yn) %>% summarise(phone.pc = mean(mobile_phone_yn)) 
# 49.2% of our sample has mobile phones; we should've collected their phone numbers! but we didn't, so couldn't do an endline survey by phone. 





###############################################################################
# # Descriptive statistics
###############################################################################

# [47] "hh_size"                             "hh_size_u2mo"                       
# [49] "hh_size_2mo_u5"                      "hh_size_5_18"                       
# [51] "hh_size_o18"                         "hh_size_o40"

# Use the very convenient R package tableone! 
# 	https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html




## Vector of variables to summarize
# table1Vars <- 
# 	c(
# 		"target_child_sex", 
# 		"target_child_months", 
# 		"hh_size", "hh_size_2mo_u5", "hh_size_u2mo", 
# 		"hh_ppl_smoke",
# 		"electricity",
# 		"mattress_yn", "blanket_yn", "mosquito_net_yn", 
# 		"solar_lamp_yn", "portable_lamp_yn",
# 		"umbrella_yn", 
# 		"chair_bench_yn", "table_yn", "almirah_wardrobe_show_case_yn",
# 		"electric_fan_yn", 
# 		"shovel_yn", "sickle_yn", "weaving_tool_yn", 
# 		"mobile_phone_yn", "smartphone_yn", "radio_yn", 
# 		"chicken_duck_pigeon_yn", 
# 		"income", 
# 		"spent_total_month",
# 		"debt"
# 	)

## Vector of categorical variables that need transformation
# table1FactorVars <- 
# 	c(
# 		"target_child_sex", "electricity", 	
# 		"blanket_yn", "mattress_yn", "mosquito_net_yn", 
# 		"solar_lamp_yn", "portable_lamp_yn",
# 		"umbrella_yn", 
# 		"chair_bench_yn", "table_yn", "almirah_wardrobe_show_case_yn", 
# 		"electric_fan_yn", 
# 		"shovel_yn", "sickle_yn", "weaving_tool_yn",
# 		"mobile_phone_yn", "smartphone_yn", "radio_yn",	
# 		"chicken_duck_pigeon_yn",
# 		"income_home_garden" # income_home_garden is have a garden? yn
# 	)
# 
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
		income = income / BDT_USD_exchange_rate,
		spent_total_month = spent_total_month/ BDT_USD_exchange_rate,
		debt = debt / BDT_USD_exchange_rate
	)



names(survey_data_tab1) <- 
	c(
	"Study arm",
	"Female", "Age (mo)", 
	"Number of household members", 
	"Number of hh members 3-59 months",
	"Number of household members 0-2 months",
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
		"Number of hh members 3-59 months",
		"Number of household members 0-2 months",
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


# When to use kable() vs kbl()?
## Example
# kbl(
# 	caption = "Fuel Procurement by Sex",
# 	booktabs = TRUE,
# 	digits = 1, # only rounded the dbl (didn't add digits to int), which is good 
# 	format = "latex"
# ) %>%
# 	add_header_above(c("", "n", "%", "n", "%", "n", "%", "n", "%")) %>%
# 	add_header_above(c("Fuel", "Men" = 2, "Women" = 2, "Boys" = 2, "Girls" = 2)) %>%
# 	# remove the header manually
# 	kable_styling(latex_options = c("scale_down")) # "striped",




# PCA to determine weath quintiles

# How do I consider that these assets have differnt value?
	
	# Determine the percentage of hh that do not have the basics, no productive tools, no animals

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

# Asked about but did not include in asset assessment because so few people had them: c("fish_net","almirah_wardrobe_show_case_yn", "refrigerator", "television", "DVD_VCD_player", "computer_laptop", "bicycle", "motorcycle", "rickshaw_van", "CNG_tempo_electric_bike")