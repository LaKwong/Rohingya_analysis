################################################################################
# @Project: Rohingya analysis
# @Author: Christopher LeBoa and Layla Kwong 
# @Description: This creates a tableone object for household demographic variables in the 
# @Date: 241019

################################################################################
rm(list = ls())
source(here::here("0_config.R")) # a config file that needs to be run to load neccesary packages 
source(here::here("3_data_cleaning/1.5_define_vector_columns.R")) # a file that defines which columns are 
## Chris
# source(here::here("1_config.R"))


# Parameters
file_survey_data_all <- here::here("4_data/RohingyaFuelMaster_survey_data_tidy.rds")
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")
file_survey_data_baseline <- here::here("4_data/RohingyaFuelMaster_survey_data_intervention_baseline.rds")


file_out_1 <- here::here("4_data/study_arm_camp_id_errors_220724.csv")
file_out_2 <- here::here("4_data/camp_block_id_errors_220724.csv")
camp_block_id_path <- here::here("4_data/camp_block_id.csv")
camp_block_sublock_id_path <- here::here("4_data/camp_block_sublock_id.csv")
camp_block_subblock_id_100_largest_path <- here::here("4_data/camp_block_subblock_id_100_largest.csv")


# Load input files

survey_data_all <- 
  read_rds(file_survey_data_all) %>%
  mutate(electricity = ifelse(electricity == 2, 1, 0)) ## Redefine the electricity variable 
survey_data <- 
  read_rds(file_survey_data_base)  %>%
  mutate(electricity = ifelse(electricity == 2, 1, 0))


survey_data_baseline <-  # select only baseline hh for table one 
  survey_data %>%
  filter(timepoint == "baseline")


survey_data %>%
  filter(timepoint == "baseline") %>%
  count(study_arm_overall)
# For each baseline, midline, endline there are 305 comparison hh and 486 intervention hh
# 791 hh with triplicate measures

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
  kable_styling(latex_options = c("scale_down")) 



################################################################################
# Where are the hh located?
################################################################################
survey_data_all %>%
  tabyl(timepoint, camp_id, study_arm_overall)

survey_data %>%
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
# Data Cleaning 

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



### Now only the paired data

camp_org <-
  survey_data %>%
  tabyl(timepoint, camp_id, org)

survey_data_all %>%
  tabyl(timepoint, camp_id, org)



camp_block_id <-
  survey_data %>%
  # tabyl(camp_id)
  # tabyl(camp_id, lpg_depots) # lpg_depots = "What LPG distribution center do you go to?
  tabyl(camp_id, block_id)
# unite(block_sub_block_id, c("block_id", "subblock_id")) %>%
# tabyl(camp_id, block_sub_block_id)

# write.csv(camp_block_id, camp_block_id_path)


camp_block_subblock_id <-
  survey_data %>%
  group_by(camp_id, block_id, subblock_id) %>% 
  count() %>%
  ungroup() %>%
  arrange(camp_id, block_id, subblock_id) 

camp_block_subblock_id_100_largest <-
  camp_block_subblock_id %>%
  arrange(desc(n)) %>%
  slice(1:100) %>%
  arrange(camp_id, block_id, subblock_id)



###############################################################################
# # Create dataset of households missing endline data to check for differential missingness 
###############################################################################
missing_from_endline_data  <- survey_data_all %>% filter(!hh_id %in% survey_data$hh_id)


###############################################################################
# # Create Table 1 datasets 
###############################################################################


survey_data_tab1 <-
  survey_data %>%
  filter(timepoint == "baseline") %>%
  select(
    c(
      "study_arm_overall",
      "hh_size", "hh_size_2mo_u5", "hh_size_u2mo",
      "hh_ppl_smoke",
      "electricity",
      "electric_fan_yn",
      "smartphone_yn", "mobile_phone_yn", # "radio_yn",   
      "portable_lamp_yn", "solar_lamp_yn", 
      
      "mattress_yn", "blanket_yn", "mosquito_net_yn",
      "umbrella_yn",
      "chair_bench_yn", "table_yn", # "almirah_wardrobe_show_case_yn",
      "shovel_yn", "sickle_yn", "weaving_tool_yn",
      "chicken_duck_pigeon_yn",
      "income",
      "spent_total_month",
      "debt",
      "target_child_sex",
      "target_child_months"
    )
  ) %>%
  # Convert to USD
  mutate(
    dropped_yn = "Participated in 3 surveys",
    income_USD = income / BDT_USD_exchange_rate_baseline,
    spent_total_month_USD = spent_total_month/ BDT_USD_exchange_rate_baseline,
    debt_USD = debt / BDT_USD_exchange_rate_baseline
  ) %>%
  select(-c(income, spent_total_month, debt)) %>%
  select(
    dropped_yn, study_arm_overall, target_child_sex, target_child_months,
    hh_size, hh_size_2mo_u5, hh_size_u2mo, hh_ppl_smoke,
    income_USD, spent_total_month_USD, debt_USD,
    everything()
  )



survey_data_tab1_IOM <- 
  survey_data %>%
  filter(camp_id %in% c("8E", "8W", "9", "10", "18"))	%>% 
  filter(timepoint == "baseline") %>%
  select(
    c(
      "study_arm_overall",
      "hh_size", "hh_size_2mo_u5", "hh_size_u2mo",
      "hh_ppl_smoke",
      "electricity",
      "electric_fan_yn",
      "smartphone_yn", "mobile_phone_yn", # "radio_yn",   
      "portable_lamp_yn", "solar_lamp_yn", 
      
      "mattress_yn", "blanket_yn", "mosquito_net_yn",
      "umbrella_yn",
      "chair_bench_yn", "table_yn", # "almirah_wardrobe_show_case_yn",
      "shovel_yn", "sickle_yn", "weaving_tool_yn",
      "chicken_duck_pigeon_yn",
      "income",
      "spent_total_month",
      "debt",
      "target_child_sex",
      "target_child_months"
    )
  ) %>%
  # Convert to USD
  mutate(
    dropped_yn = "Participated in 3 surveys",
    income_USD = income / BDT_USD_exchange_rate_baseline,
    spent_total_month_USD = spent_total_month/ BDT_USD_exchange_rate_baseline,
    debt_USD = debt / BDT_USD_exchange_rate_baseline
  ) %>%
  select(-c(income, spent_total_month, debt)) %>%
  select(
    dropped_yn, study_arm_overall, target_child_sex, target_child_months,
    hh_size, hh_size_2mo_u5, hh_size_u2mo, hh_ppl_smoke,
    income_USD, spent_total_month_USD, debt_USD,
    everything()
  )

## The following hh were missing from endline data collection -- we want to create separate table object to view their assets 
# and compare wealth to see if they are fundementally different than those included in study

missing_data_tab1 <-
  missing_from_endline_data %>%
  filter(timepoint == "baseline") %>%
  select(
    c(
      "study_arm_overall",
      "hh_size", "hh_size_2mo_u5", "hh_size_u2mo",
      "hh_ppl_smoke",
      "electricity",
      "electric_fan_yn",
      "smartphone_yn", "mobile_phone_yn", # "radio_yn",   
      "portable_lamp_yn", "solar_lamp_yn", 
      
      "mattress_yn", "blanket_yn", "mosquito_net_yn",
      "umbrella_yn",
      "chair_bench_yn", "table_yn", # "almirah_wardrobe_show_case_yn",
      "shovel_yn", "sickle_yn", "weaving_tool_yn",
      "chicken_duck_pigeon_yn",
      "income",
      "spent_total_month",
      "debt",
      "target_child_sex",
      "target_child_months"
    )
  ) %>%
  # Convert to USD
  mutate(
    dropped_yn = "Lost to attrition",
    income_USD = income/ BDT_USD_exchange_rate_baseline,
    spent_total_month_USD = spent_total_month/ BDT_USD_exchange_rate_baseline,
    debt_USD = debt / BDT_USD_exchange_rate_baseline
  ) %>%
  select(-c(income, spent_total_month, debt)) %>%
  select(
    dropped_yn, study_arm_overall, 
    hh_size, hh_size_2mo_u5, hh_size_u2mo, hh_ppl_smoke,
    income_USD, spent_total_month_USD, debt_USD,
    everything()
  )


###############################################################################
# # Define variables that will be used in the tables 
###############################################################################


### Define the vars for the table and specify which are factors
table1Vars <-
  c(
    "dropped_yn",
    "Study arm",
    "Target child is female", 
    "Age of target child (mo)",
    "Number of household members", 
    "Number of hh members 2-59 months",
    "Number of household members 0 to < 2 months",
    "Number of household members who smoke",
    "Monthly income [USD]",
    "Monthly expenditure [USD]",
    "Total debt [USD]",
    "House has solar electricity",
    "Has 1+ electric fan", 
    "Has 1+ smartphone", 
    "Has 1+ mobile phone",
    # "Has 1+ radio", # Note that no family had a radio so the table has radio_yn = 0
    "Has 1+ portable lamp",
    "Has 1+ solar lamp",
    "Has 1+ mattress", 
    "Has 1+ blanket",
    "Has 1+ mosquito net",
    "Has 1+ umbrella", 
    "Has 1+ chair/bench", 
    "Has 1+ table", 
    # "Has 1+ wardrobe", 
    "Has 1+ shovel", 
    "Has 1+ sickle", 
    "Has 1+ weaving tool", 
    "Has 1+ poultry"
    # "Has home garden",

  )


table1FactorVars <- 
  c(
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
    # "Has 1+ wardrobe", # remove because only 3 total people have a wardrobe
    "Has 1+ shovel", 
    "Has 1+ sickle", 
    "Has 1+ weaving tool", 
    "Has 1+ poultry",
    "Target child is female"
  )


# Make full dataset
survey_all_tab1 <- 
  survey_data_tab1 %>% 
  bind_rows(missing_data_tab1) %>%
  mutate(dropped_yn = ordered(dropped_yn, levels = c("Participated in 3 surveys", "Lost to attrition")))

# Name datasets
names(survey_all_tab1) <- table1Vars

names(survey_data_tab1) <- table1Vars

names(survey_data_tab1_IOM) <- table1Vars

names(missing_data_tab1) <-  table1Vars

survey_all_tab1$`Monthly income [USD]`


###############################################################################
# # Create Table 1 Objects
###############################################################################


# Get average stats for all hh
tab1_baseline_no_study_arm <- 
  CreateTableOne(
    data = survey_data_tab1, 
    vars = table1Vars, factorVars = table1FactorVars
  )

tab1_baseline_no_study_arm


## Table 1 that was used for paper 

# Compare arms among hh used for analysis (participated in all three timepoints)
tab1_baseline <- 
  CreateTableOne(
    data = survey_data_tab1, 
    vars = table1Vars, factorVars = table1FactorVars, strata = "Study arm"
  )

tab1_baseline

# Compare only the hh that were in the IOM camps
tab1_baseline_IOM <- 
  CreateTableOne(
    data = survey_data_tab1_IOM, 
    vars = table1Vars, factorVars = table1FactorVars, strata = "Study arm"
  )

tab1_baseline_IOM

###############################################################################
# # Create Supplement table 2 
###############################################################################

## Supplemental table 2 code.... Comparing hh that were included in study compared to those dropped from study.
# Compare hh that dropped compared to those that did not
#Used to calculate the data that were missing by endline 
tab1_missing <-  
  CreateTableOne(data = missing_data_tab1, 
                 vars = table1Vars, factorVars = table1FactorVars, strata = "Study arm"
  )

tab1_comparison_dropped <-  
  CreateTableOne(data = survey_all_tab1 %>% filter(`Study arm` == "comparison"), 
                 vars = table1Vars, factorVars = table1FactorVars, strata = "dropped_yn"
  )

tab1_comparison_dropped

tab1_intervention_dropped <-  
  CreateTableOne(data = survey_all_tab1 %>% filter(`Study arm` == "intervention"), 
                 vars = table1Vars, factorVars = table1FactorVars, strata = "dropped_yn"
  )

tab1_intervention_dropped




###############################################################################
# # Save Table 1 datasets as CSVs
###############################################################################

tab1_baseline_no_study_arm_csv <- print(tab1_baseline_no_study_arm)
write.csv(tab1_baseline_no_study_arm_csv, "7_tables/tab1_baseline_no_study_arm.csv")

tab1_baseline_csv <- print(tab1_baseline)
write.csv(tab1_baseline_csv, "7_tables/tab1_baseline.csv")

tab1_comparison_dropped_csv <- print(tab1_comparison_dropped)
write.csv(tab1_comparison_dropped_csv, "7_tables/tab1_comparison_dropped.csv")

tab1_intervention_dropped_csv <- print(tab1_intervention_dropped)
write.csv(tab1_intervention_dropped_csv, "7_tables/tab1_intervention_dropped.csv")







