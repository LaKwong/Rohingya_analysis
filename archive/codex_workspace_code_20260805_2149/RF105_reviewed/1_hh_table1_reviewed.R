################################################################################
# RF105 reviewed Table 1
#
# Purpose:
#   Recreate the RF105 baseline household characteristic tables using the
#   cleaned final refugee household survey file.
#
# Input:
#   4_data/clean_final/survey_refugee_household.rds
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/tab1_baseline_by_arm_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/tab1_baseline_by_arm_and_followup_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/tab1_attrition_comparison_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/tab1_attrition_intervention_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/tab1_*.csv
#
# Main checks/fixes relative to the older script:
#   1. Reads from 4_data/clean_final, not older intermediate survey files.
#   2. Preserves old variable names by creating documented aliases where the
#      clean_final names changed (for example fuel_30_scraps).
#   3. Does not silently ignore duplicate fcn_id/timepoint records; writes a
#      QA file before deduplicating one record per fcn_id/timepoint.
#   4. Writes a count reconciliation table against the current manuscript
#      Appendix Table 1 sample sizes.
################################################################################

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)

  if (length(file_arg) == 1) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg),
                                 winslash = "/", mustWork = FALSE)))
  }

  source_files <- vapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile)) frame$ofile else NA_character_
  }, character(1))
  source_files <- source_files[!is.na(source_files)]

  if (length(source_files) > 0) {
    return(dirname(normalizePath(source_files[[length(source_files)]],
                                 winslash = "/", mustWork = FALSE)))
  }

  getwd()
}

script_dir <- get_script_dir()
config_file <- file.path(script_dir, "0_RF105_reviewed_config.R")
if (!file.exists(config_file)) {
  config_file <- file.path("5_analysis_RF105", "0_RF105_reviewed_config.R")
}
source(config_file)

survey_data_raw <- readRDS(file_survey_refugee_household) %>%
  add_rf105_aliases()

analysis_population <- make_analysis_population(survey_data_raw, id_var = "fcn_id")

write_reviewed_csv(
  analysis_population$duplicate_records,
  "tab1_duplicate_fcn_id_timepoint_records.csv",
  subfolder = "qa"
)

write_reviewed_csv(
  analysis_population$sample_counts,
  "tab1_sample_counts_after_deduplication.csv",
  subfolder = "qa"
)

################################################################################
# Table 1 variable definitions
################################################################################

table1_vars_original <- c(
  "camp_id",
  "block_id",
  "subblock_id",
  "n_hh_members",
  "n_kids",
  "target_child_months",
  "target_child_sex",
  "electricity",
  "electric_fan_yn",
  "debt_yn",
  "income_usd",
  "spent_total_month_usd",
  "n_meals_yesterday",
  "smartphone_yn",
  "mobile_phone_yn",
  "mattress_yn",
  "blanket_yn",
  "mosquito_net_yn",
  "umbrella_yn",
  "chair_bench_yn",
  "table_yn",
  "shovel_yn",
  "sickle_yn",
  "weaving_tool_yn",
  "chicken_duck_pigeon_yn"
)

table1_labels <- c(
  camp_id = "Camp",
  block_id = "Block",
  subblock_id = "Sub-block",
  n_hh_members = "Number of household members",
  n_kids = "Number of children",
  target_child_months = "Target child age, months",
  target_child_sex = "Target child sex",
  electricity = "Household has electricity",
  electric_fan_yn = "Owns electric fan",
  debt_yn = "Household has debt",
  income_usd = "Monthly income, USD",
  spent_total_month_usd = "Monthly expenditures, USD",
  n_meals_yesterday = "Meals eaten yesterday",
  smartphone_yn = "Owns smartphone",
  mobile_phone_yn = "Owns mobile phone",
  mattress_yn = "Owns mattress",
  blanket_yn = "Owns blanket",
  mosquito_net_yn = "Owns mosquito net",
  umbrella_yn = "Owns umbrella",
  chair_bench_yn = "Owns chair or bench",
  table_yn = "Owns table",
  shovel_yn = "Owns shovel",
  sickle_yn = "Owns sickle",
  weaving_tool_yn = "Owns weaving tool",
  chicken_duck_pigeon_yn = "Owns poultry"
)

binary_table1_vars <- c(
  "electricity",
  "electric_fan_yn",
  "debt_yn",
  "smartphone_yn",
  "mobile_phone_yn",
  "mattress_yn",
  "blanket_yn",
  "mosquito_net_yn",
  "umbrella_yn",
  "chair_bench_yn",
  "table_yn",
  "shovel_yn",
  "sickle_yn",
  "weaving_tool_yn",
  "chicken_duck_pigeon_yn"
)

prepare_table1_data <- function(df) {
  df %>%
    mutate(
      exchange_rate = exchange_bdt_per_usd[as.character(timepoint)],
      income_usd = suppressWarnings(as.numeric(income)) / exchange_rate,
      spent_total_month_usd =
        suppressWarnings(as.numeric(spent_total_month)) / exchange_rate
    )
}

baseline_complete <- analysis_population$complete_3_survey %>%
  filter(timepoint == "baseline") %>%
  prepare_table1_data()

baseline_followup <- analysis_population$baseline_followup_status %>%
  prepare_table1_data()

missing_table1_vars <- flag_missing_vars(
  baseline_followup,
  table1_vars_original,
  "RF105 Table 1 variables after clean_final aliases"
)

write_reviewed_csv(
  missing_table1_vars,
  "tab1_variable_availability.csv",
  subfolder = "qa"
)

table1_vars_available <- missing_table1_vars %>%
  filter(available) %>%
  pull(variable)

################################################################################
# Sample size reconciliation with manuscript Appendix Table 1
################################################################################

current_count_reconciliation <- baseline_followup %>%
  count(study_arm_overall, attrition_status, name = "current_clean_final_n") %>%
  mutate(
    study_arm_overall = as.character(study_arm_overall),
    attrition_status = as.character(attrition_status)
  )

manuscript_count_reconciliation <- tribble(
  ~study_arm_overall, ~attrition_status, ~manuscript_appendix_table1_n,
  "comparison", "participated_in_all_3_surveys", 437,
  "comparison", "lost_before_endline", 161,
  "intervention", "participated_in_all_3_surveys", 494,
  "intervention", "lost_before_endline", 100
) %>%
  left_join(
    current_count_reconciliation,
    by = c("study_arm_overall", "attrition_status")
  ) %>%
  mutate(
    difference_current_minus_manuscript =
      current_clean_final_n - manuscript_appendix_table1_n,
    note = "Review before changing manuscript counts; differences may reflect clean_final ID deduplication or manuscript sample restriction."
  )

write_reviewed_csv(
  manuscript_count_reconciliation,
  "tab1_manuscript_count_reconciliation.csv",
  subfolder = "qa"
)

################################################################################
# Baseline characteristics among participants with all three surveys
################################################################################

tab1_baseline_by_arm <- make_characteristics_table(
  df = baseline_complete,
  vars = table1_vars_available,
  labels = table1_labels,
  group_var = "study_arm_overall",
  binary_vars = binary_table1_vars
)

write_reviewed_csv(
  tab1_baseline_by_arm,
  "tab1_baseline_by_arm_reviewed.csv"
)

################################################################################
# Baseline characteristics by arm and follow-up completion status
################################################################################

baseline_followup_for_table <- baseline_followup %>%
  mutate(
    arm_followup_group = paste(study_arm_overall, attrition_status, sep = "_")
  )

tab1_baseline_by_arm_followup <- make_characteristics_table(
  df = baseline_followup_for_table,
  vars = table1_vars_available,
  labels = table1_labels,
  group_var = "arm_followup_group",
  binary_vars = binary_table1_vars
)

write_reviewed_csv(
  tab1_baseline_by_arm_followup,
  "tab1_baseline_by_arm_and_followup_reviewed.csv"
)

tab1_attrition_comparison <- baseline_followup %>%
  filter(study_arm_overall == "comparison") %>%
  make_characteristics_table(
    vars = table1_vars_available,
    labels = table1_labels,
    group_var = "attrition_status",
    binary_vars = binary_table1_vars
  )

write_reviewed_csv(
  tab1_attrition_comparison,
  "tab1_attrition_comparison_reviewed.csv"
)

tab1_attrition_intervention <- baseline_followup %>%
  filter(study_arm_overall == "intervention") %>%
  make_characteristics_table(
    vars = table1_vars_available,
    labels = table1_labels,
    group_var = "attrition_status",
    binary_vars = binary_table1_vars
  )

write_reviewed_csv(
  tab1_attrition_intervention,
  "tab1_attrition_intervention_reviewed.csv"
)

