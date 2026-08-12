# RF105 child physical-health rDiD sample-size diagnostic
#
# Purpose:
#   Determine why target-child physical-health rDiD panels have inconsistent
#   sample sizes across outcomes. This script mirrors the active reviewed rDiD
#   household-panel eligibility logic, then decomposes child outcome missingness
#   by contrast, study arm, timepoint, and outcome.
#
# Inputs:
#   4_data/clean_final/survey_refugee_household.rds
#   5_analysis_RF105/reviewed/0_RF105_config_20260805_2213.R
#
# Outputs:
#   Shareable aggregate QA tables:
#     7_tables/RF105_reviewed_YYYYMMDD/qa/table_qa_child_health_expected_panel_counts.csv
#     7_tables/RF105_reviewed_YYYYMMDD/qa/table_qa_child_health_outcome_missingness.csv
#     7_tables/RF105_reviewed_YYYYMMDD/qa/table_qa_child_health_raw_value_codes.csv
#     7_tables/RF105_reviewed_YYYYMMDD/qa/table_qa_child_health_inconsistency_summary.csv
#     7_tables/RF105_reviewed_YYYYMMDD/qa/table_qa_child_vs_respondent_health_counts.csv
#   Restricted household-level QA tables:
#     8_restricted/RF105_reviewed_YYYYMMDD/qa/restricted_qa_child_health_missing_households.csv
#     8_restricted/RF105_reviewed_YYYYMMDD/qa/restricted_qa_child_vs_respondent_health_discrepancies.csv

options(stringsAsFactors = FALSE)

required_packages <- c("tidyverse")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Install required package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages(library(tidyverse))

get_script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
  }
  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(normalizePath(sys.frames()[[1]]$ofile, winslash = "/", mustWork = TRUE))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

script_path <- get_script_path()
script_dir <- if (dir.exists(script_path)) script_path else dirname(script_path)
config_file <- file.path(script_dir, "0_RF105_config_20260805_2213.R")
if (!file.exists(config_file)) {
  stop("Missing RF105 config file: ", config_file, call. = FALSE)
}
source(config_file)

as_number_diag <- function(x) {
  suppressWarnings(as.numeric(x))
}

first_nonmissing_diag <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  x[[1]]
}

make_yn_diag <- function(x) {
  x_chr <- str_squish(str_to_lower(as.character(x)))
  x_num <- as_number_diag(x_chr)

  case_when(
    is.na(x) ~ NA_integer_,
    !is.na(x_num) & x_num %in% c(77, 88, 99) ~ NA_integer_,
    !is.na(x_num) ~ as.integer(x_num > 0),
    x_chr %in% c("yes", "y", "true", "present") ~ 1L,
    x_chr %in% c("no", "n", "false", "absent") ~ 0L,
    TRUE ~ NA_integer_
  )
}

child_outcomes <- tribble(
  ~raw_variable, ~outcome, ~outcome_label,
  "target_child_cough", "target_child_cough_yn", "Child cough",
  "target_child_resp_rate", "target_child_resp_rate_yn", "Child increased respiratory rate today",
  "target_child_wheezing", "target_child_wheezing_yn", "Child current wheeze",
  "target_child_eye_red", "target_child_eye_red_yn", "Child red eyes",
  "target_child_eye_itch", "target_child_eye_itch_yn", "Child itchy eyes",
  "target_child_lethargy", "target_child_lethargy_yn", "Child lethargy",
  "target_child_weight_loss", "target_child_weight_loss_yn", "Child unexplained weight loss in 3 months",
  "target_child_fever", "target_child_fever_yn", "Child fever",
  "target_child_clinic_resp", "target_child_clinic_resp_yn", "Child clinic visit for respiratory complaint"
)

respondent_outcomes <- tribble(
  ~raw_variable, ~outcome, ~outcome_label,
  "respondent_cough", "respondent_cough_yn", "Caregiver cough",
  "respondent_resp_rate_reported_combined", "respondent_resp_rate_yn", "Caregiver increased respiratory rate today",
  "respondent_wheezing", "respondent_wheezing_yn", "Caregiver current wheeze",
  "respondent_eye_red", "respondent_eye_red_yn", "Caregiver red eyes",
  "respondent_eye_itch", "respondent_eye_itch_yn", "Caregiver itchy eyes",
  "respondent_eye_sore", "respondent_eye_sore_yn", "Caregiver sore eyes",
  "respondent_weight_loss_reported_combined", "respondent_weight_loss_yn", "Caregiver unexplained weight loss in 3 months",
  "respondent_headache", "respondent_headache_yn", "Caregiver headache",
  "respondent_backache", "respondent_backache_yn", "Caregiver backache"
)

expected_contrast_counts <- tribble(
  ~contrast, ~followup_timepoint, ~study_arm_overall, ~expected_n_households,
  "primary_baseline_midline", "midline", "intervention", 558L,
  "primary_baseline_midline", "midline", "comparison", 575L,
  "secondary_baseline_endline", "endline", "intervention", 496L,
  "secondary_baseline_endline", "endline", "comparison", 446L
) %>%
  mutate(
    followup_timepoint = as_ordered_timepoint(followup_timepoint),
    study_arm_overall = factor(study_arm_overall, levels = arm_levels)
  )

survey_raw <- readRDS(file_survey_refugee_household) %>%
  add_rf105_aliases() %>%
  clean_timepoint_arm()

analysis_population <- make_analysis_population(survey_raw, id_var = "fcn_id")
survey_model_data <- analysis_population$all_deduplicated

coalesce_character_columns <- function(df, vars) {
  vars <- vars[vars %in% names(df)]
  if (length(vars) == 0) return(rep(NA_character_, nrow(df)))
  values <- lapply(vars, function(var) as.character(df[[var]]))
  out <- values[[1]]
  if (length(values) > 1) {
    for (i in seq_along(values)[-1]) {
      out <- dplyr::coalesce(out, values[[i]])
    }
  }
  out
}

# Baseline/midline use the historical misspelling "respondant" while endline
# uses "respondent". Combine both spellings for respondent comparator QA.
survey_model_data <- survey_model_data %>%
  mutate(
    respondent_resp_rate_reported_combined = coalesce_character_columns(
      ., c("resp_rate_reported_respondent", "resp_rate_reported_respondant")
    ),
    respondent_weight_loss_reported_combined = coalesce_character_columns(
      ., c("weight_loss_reported_respondent", "weight_loss_reported_respondant")
    )
  )

all_physical_outcomes <- bind_rows(
  child_outcomes %>% mutate(respondent_group = "target_child"),
  respondent_outcomes %>% mutate(respondent_group = "respondent")
)

for (i in seq_len(nrow(all_physical_outcomes))) {
  raw_var <- all_physical_outcomes$raw_variable[[i]]
  outcome_var <- all_physical_outcomes$outcome[[i]]
  if (raw_var %in% names(survey_model_data)) {
    survey_model_data[[outcome_var]] <- make_yn_diag(survey_model_data[[raw_var]])
  } else {
    survey_model_data[[outcome_var]] <- NA_integer_
  }
}

baseline_covars <- survey_model_data %>%
  filter(timepoint == "baseline") %>%
  transmute(
    fcn_id = as.character(fcn_id),
    study_arm_overall,
    A = if_else(study_arm_overall == "intervention", 1, 0, missing = NA_real_),
    hh_size = as_number_diag(hh_size),
    hh_per_structure = as_number_diag(hh_per_structure)
  ) %>%
  group_by(fcn_id) %>%
  summarise(
    study_arm_overall = first_nonmissing_diag(study_arm_overall),
    A = as_number_diag(first_nonmissing_diag(A)),
    hh_size = as_number_diag(first_nonmissing_diag(hh_size)),
    hh_per_structure = as_number_diag(first_nonmissing_diag(hh_per_structure)),
    .groups = "drop"
  ) %>%
  mutate(
    study_arm_overall = factor(as.character(study_arm_overall), levels = arm_levels)
  )

make_contrast_households <- function(followup_timepoint, contrast) {
  followup_ids <- survey_model_data %>%
    filter(timepoint == followup_timepoint) %>%
    distinct(fcn_id = as.character(fcn_id))

  baseline_covars %>%
    inner_join(followup_ids, by = "fcn_id") %>%
    filter(!is.na(A), !is.na(study_arm_overall)) %>%
    mutate(
      contrast = contrast,
      followup_timepoint = as.character(followup_timepoint)
    )
}

contrast_households <- bind_rows(
  make_contrast_households("midline", "primary_baseline_midline"),
  make_contrast_households("endline", "secondary_baseline_endline")
)

expected_panel_counts <- contrast_households %>%
  count(contrast, followup_timepoint, study_arm_overall, name = "observed_paired_households") %>%
  full_join(expected_contrast_counts, by = c("contrast", "followup_timepoint", "study_arm_overall")) %>%
  mutate(
    observed_paired_households = replace_na(observed_paired_households, 0L),
    expected_n_households = replace_na(expected_n_households, NA_integer_),
    difference_observed_minus_expected = observed_paired_households - expected_n_households,
    matches_expected = observed_paired_households == expected_n_households
  ) %>%
  arrange(contrast, study_arm_overall)

make_outcome_missingness <- function(outcome_row, followup_timepoint, contrast) {
  baseline_y <- survey_model_data %>%
    filter(timepoint == "baseline") %>%
    transmute(
      fcn_id = as.character(fcn_id),
      baseline_value = as_number_diag(.data[[outcome_row$outcome]]),
      baseline_raw_value = if (outcome_row$raw_variable %in% names(.)) as.character(.data[[outcome_row$raw_variable]]) else NA_character_
    )

  followup_y <- survey_model_data %>%
    filter(timepoint == followup_timepoint) %>%
    transmute(
      fcn_id = as.character(fcn_id),
      followup_value = as_number_diag(.data[[outcome_row$outcome]]),
      followup_raw_value = if (outcome_row$raw_variable %in% names(.)) as.character(.data[[outcome_row$raw_variable]]) else NA_character_
    )

  contrast_households %>%
    filter(.data$contrast == contrast) %>%
    left_join(baseline_y, by = "fcn_id") %>%
    left_join(followup_y, by = "fcn_id") %>%
    mutate(
      outcome = outcome_row$outcome,
      raw_variable = outcome_row$raw_variable,
      outcome_label = outcome_row$outcome_label,
      baseline_missing = is.na(baseline_value),
      followup_missing = is.na(followup_value),
      complete_panel = !baseline_missing & !followup_missing,
      missing_pattern = case_when(
        !baseline_missing & !followup_missing ~ "complete_baseline_and_followup",
        baseline_missing & !followup_missing ~ "missing_baseline_only",
        !baseline_missing & followup_missing ~ "missing_followup_only",
        baseline_missing & followup_missing ~ "missing_both_timepoints",
        TRUE ~ "unclassified"
      )
    )
}

outcome_household_diagnostics <- bind_rows(
  map_dfr(seq_len(nrow(child_outcomes)), function(i) {
    make_outcome_missingness(child_outcomes[i, ], "midline", "primary_baseline_midline")
  }),
  map_dfr(seq_len(nrow(child_outcomes)), function(i) {
    make_outcome_missingness(child_outcomes[i, ], "endline", "secondary_baseline_endline")
  })
) %>%
  mutate(respondent_group = "target_child")

respondent_household_diagnostics <- bind_rows(
  map_dfr(seq_len(nrow(respondent_outcomes)), function(i) {
    make_outcome_missingness(respondent_outcomes[i, ], "midline", "primary_baseline_midline")
  }),
  map_dfr(seq_len(nrow(respondent_outcomes)), function(i) {
    make_outcome_missingness(respondent_outcomes[i, ], "endline", "secondary_baseline_endline")
  })
) %>%
  mutate(respondent_group = "respondent")

outcome_missingness_summary <- outcome_household_diagnostics %>%
  group_by(contrast, followup_timepoint, study_arm_overall, outcome, raw_variable, outcome_label) %>%
  summarise(
    expected_or_paired_households = n_distinct(fcn_id),
    n_complete_panel = sum(complete_panel, na.rm = TRUE),
    n_missing_baseline_only = sum(missing_pattern == "missing_baseline_only", na.rm = TRUE),
    n_missing_followup_only = sum(missing_pattern == "missing_followup_only", na.rm = TRUE),
    n_missing_both_timepoints = sum(missing_pattern == "missing_both_timepoints", na.rm = TRUE),
    n_missing_baseline_any = sum(baseline_missing, na.rm = TRUE),
    n_missing_followup_any = sum(followup_missing, na.rm = TRUE),
    n_dropped_from_expected = expected_or_paired_households - n_complete_panel,
    pct_complete_panel = 100 * n_complete_panel / expected_or_paired_households,
    .groups = "drop"
  ) %>%
  arrange(contrast, study_arm_overall, outcome)

inconsistency_summary <- outcome_missingness_summary %>%
  group_by(contrast, study_arm_overall) %>%
  summarise(
    n_outcomes = n_distinct(outcome),
    min_complete_panel = min(n_complete_panel, na.rm = TRUE),
    max_complete_panel = max(n_complete_panel, na.rm = TRUE),
    range_complete_panel = max_complete_panel - min_complete_panel,
    max_missing_baseline_any = max(n_missing_baseline_any, na.rm = TRUE),
    max_missing_followup_any = max(n_missing_followup_any, na.rm = TRUE),
    outcomes_with_min_complete_panel = paste(outcome_label[n_complete_panel == min_complete_panel], collapse = "; "),
    outcomes_with_max_complete_panel = paste(outcome_label[n_complete_panel == max_complete_panel], collapse = "; "),
    likely_explanation = case_when(
      range_complete_panel == 0 ~ "All target-child physical-health outcomes have the same complete-case panel size.",
      max_missing_baseline_any > 0 & max_missing_followup_any > 0 ~ "Outcome-specific missing/refused/don't-know values occur at both baseline and follow-up.",
      max_missing_baseline_any > 0 ~ "Outcome-specific missing/refused/don't-know values occur at baseline.",
      max_missing_followup_any > 0 ~ "Outcome-specific missing/refused/don't-know values occur at follow-up.",
      TRUE ~ "Complete-case panel sizes differ for a reason not captured by baseline/follow-up outcome missingness."
    ),
    .groups = "drop"
  ) %>%
  left_join(expected_panel_counts, by = c("contrast", "study_arm_overall")) %>%
  arrange(contrast, study_arm_overall)

raw_value_codes <- bind_rows(lapply(child_outcomes$raw_variable, function(raw_var) {
  if (!raw_var %in% names(survey_model_data)) {
    return(tibble(
      raw_variable = raw_var,
      timepoint = factor(character(), levels = timepoint_levels, ordered = TRUE),
      study_arm_overall = factor(character(), levels = arm_levels),
      raw_value = character(),
      n = integer()
    ))
  }
  survey_model_data %>%
    count(timepoint, study_arm_overall, raw_value = as.character(.data[[raw_var]]), name = "n") %>%
    mutate(raw_variable = raw_var, .before = timepoint)
})) %>%
  left_join(child_outcomes, by = "raw_variable") %>%
  arrange(raw_variable, timepoint, study_arm_overall, desc(n), raw_value)

respondent_missingness_summary <- respondent_household_diagnostics %>%
  group_by(contrast, followup_timepoint, study_arm_overall, outcome, raw_variable, outcome_label) %>%
  summarise(
    expected_or_paired_households = n_distinct(fcn_id),
    n_complete_panel = sum(complete_panel, na.rm = TRUE),
    n_missing_baseline_any = sum(baseline_missing, na.rm = TRUE),
    n_missing_followup_any = sum(followup_missing, na.rm = TRUE),
    n_dropped_from_expected = expected_or_paired_households - n_complete_panel,
    .groups = "drop"
  )

child_vs_respondent_health_counts <- bind_rows(
  outcome_missingness_summary %>%
    mutate(respondent_group = "target_child") %>%
    select(respondent_group, contrast, followup_timepoint, study_arm_overall,
           outcome, raw_variable, outcome_label, expected_or_paired_households,
           n_complete_panel, n_missing_baseline_any, n_missing_followup_any,
           n_dropped_from_expected),
  respondent_missingness_summary %>%
    mutate(respondent_group = "respondent") %>%
    select(respondent_group, contrast, followup_timepoint, study_arm_overall,
           outcome, raw_variable, outcome_label, expected_or_paired_households,
           n_complete_panel, n_missing_baseline_any, n_missing_followup_any,
           n_dropped_from_expected)
) %>%
  arrange(contrast, study_arm_overall, respondent_group, outcome)

household_any_child_missing <- outcome_household_diagnostics %>%
  group_by(contrast, followup_timepoint, study_arm_overall, fcn_id) %>%
  summarise(
    n_child_outcomes_missing = sum(!complete_panel, na.rm = TRUE),
    child_outcomes_missing = paste(outcome_label[!complete_panel], collapse = "; "),
    .groups = "drop"
  )

household_any_respondent_missing <- respondent_household_diagnostics %>%
  group_by(contrast, followup_timepoint, study_arm_overall, fcn_id) %>%
  summarise(
    n_respondent_outcomes_missing = sum(!complete_panel, na.rm = TRUE),
    respondent_outcomes_missing = paste(outcome_label[!complete_panel], collapse = "; "),
    .groups = "drop"
  )

restricted_child_vs_respondent_discrepancies <- household_any_child_missing %>%
  left_join(
    household_any_respondent_missing,
    by = c("contrast", "followup_timepoint", "study_arm_overall", "fcn_id")
  ) %>%
  filter(n_child_outcomes_missing > 0 | n_respondent_outcomes_missing > 0) %>%
  mutate(
    discrepancy_type = case_when(
      n_child_outcomes_missing > 0 & replace_na(n_respondent_outcomes_missing, 0L) == 0L ~
        "child_missing_respondent_complete",
      n_child_outcomes_missing == 0 & n_respondent_outcomes_missing > 0 ~
        "respondent_missing_child_complete",
      n_child_outcomes_missing > 0 & n_respondent_outcomes_missing > 0 ~
        "both_child_and_respondent_missing",
      TRUE ~ "none"
    )
  ) %>%
  arrange(contrast, study_arm_overall, discrepancy_type, fcn_id)

restricted_missing_households <- outcome_household_diagnostics %>%
  filter(!complete_panel) %>%
  select(
    contrast, followup_timepoint, study_arm_overall, fcn_id,
    outcome, raw_variable, outcome_label,
    missing_pattern, baseline_missing, followup_missing,
    baseline_value, followup_value, baseline_raw_value, followup_raw_value,
    hh_size, hh_per_structure
  ) %>%
  arrange(contrast, study_arm_overall, outcome, missing_pattern, fcn_id)

write_reviewed_csv(
  expected_panel_counts,
  "table_qa_child_health_expected_panel_counts.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  outcome_missingness_summary,
  "table_qa_child_health_outcome_missingness.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  raw_value_codes,
  "table_qa_child_health_raw_value_codes.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  inconsistency_summary,
  "table_qa_child_health_inconsistency_summary.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  child_vs_respondent_health_counts,
  "table_qa_child_vs_respondent_health_counts.csv",
  subfolder = "qa"
)
write_restricted_qa_csv(
  restricted_child_vs_respondent_discrepancies,
  "restricted_qa_child_vs_respondent_health_discrepancies.csv",
  reason = paste(
    "Contains fcn_id-level comparison of target-child and respondent",
    "physical-health complete-case status by contrast."
  )
)
write_restricted_qa_csv(
  restricted_missing_households,
  "restricted_qa_child_health_missing_households.csv",
  reason = paste(
    "Contains fcn_id-level records showing which households are dropped from",
    "target-child physical-health rDiD panels because baseline and/or follow-up",
    "outcome values are missing."
  )
)

message("Child physical-health sample-size diagnostic complete.")
message("Expected paired-household counts:")
print(expected_panel_counts)
message("Outcome missingness summary:")
print(outcome_missingness_summary, n = Inf)