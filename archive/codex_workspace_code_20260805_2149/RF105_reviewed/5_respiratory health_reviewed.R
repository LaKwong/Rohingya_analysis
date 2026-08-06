################################################################################
# RF105 reviewed respiratory, physical health, mental health, and expenditure
# sensitivity analyses
#
# Purpose:
#   Recreate the RF105B health outcome checks using the final cleaned household
#   survey file, without writing derived health data back into 4_data.
#
# Input:
#   4_data/clean_final/survey_refugee_household.rds
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/health_*.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/health_*.csv
#   6_figures/RF105_reviewed_YYYYMMDD/health_child_symptoms_reviewed.png
#
# Sensitivity analyses:
#   1. Baseline-to-midline unadjusted DiD.
#   2. Baseline-to-endline unadjusted DiD.
#   3. Complete three-survey household population versus all deduplicated
#      household-timepoint records.
#
# Note:
#   The manuscript primary RF105B models used double machine learning. This
#   reviewed file does not replace those models; it provides transparent,
#   easy-to-run sensitivity analyses that use the same cleaned inputs.
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
  analysis_population$sample_counts,
  "health_sample_counts_after_deduplication.csv",
  subfolder = "qa"
)

################################################################################
# Outcome derivations
################################################################################

derive_health_outcomes <- function(df) {
  health_roots <- c(
    "target_child_clinic_resp",
    "target_child_wheezing",
    "target_child_distrubed_speech",
    "target_child_disturbed_sleep",
    "target_child_eye_red",
    "target_child_eye_itch",
    "target_child_cough",
    "target_child_fever",
    "target_child_resp_rate",
    "target_child_weight_loss",
    "target_child_lethargy",
    "respondent_cough",
    "respondent_wheezing",
    "respondent_disturbed_sleep",
    "respondent_disturbed_speech",
    "respondent_eye_red",
    "respondent_eye_itch",
    "respondent_eye_sore",
    "respondent_headache",
    "respondent_backache",
    "depressed",
    "sick",
    "mental_health_difficulty"
  )

  for (root in health_roots) {
    yn_name <- paste0(root, "_yn")
    if (root %in% names(df) && yn_name %notin% names(df)) {
      df[[yn_name]] <- make_yn(df[[root]])
    }
  }

  # Preserve the misspelled historical variable name used in the old RF105B
  # script, so downstream comparisons still line up with prior code.
  if ("target_child_distrubed_speech_yn" %in% names(df) &&
      "target_child_disturbed_speech_yn" %notin% names(df)) {
    df$target_child_disturbed_speech_yn <- df$target_child_distrubed_speech_yn
  }

  if (all(c("target_child_cough_yn", "target_child_fever_yn") %in% names(df))) {
    df <- df %>%
      mutate(
        target_child_alri = case_when(
          target_child_cough_yn == 1 & target_child_fever_yn == 1 ~ 1L,
          is.na(target_child_cough_yn) | is.na(target_child_fever_yn) ~ NA_integer_,
          TRUE ~ 0L
        )
      )
  }

  if ("healthcare_visits_6mo" %in% names(df)) {
    df <- df %>%
      mutate(
        healthcare_visit_6mo_yn =
          as.integer(suppressWarnings(as.numeric(healthcare_visits_6mo)) > 0)
      )
  }

  df %>%
    mutate(
      exchange_rate = exchange_bdt_per_usd[as.character(timepoint)],
      medical_usd = if ("medical" %in% names(.)) {
        suppressWarnings(as.numeric(medical)) / exchange_rate
      } else {
        NA_real_
      },
      total_expenditures_30_usd = if ("total_expenditures_30" %in% names(.)) {
        suppressWarnings(as.numeric(total_expenditures_30)) / exchange_rate
      } else {
        NA_real_
      },
      spent_total_month_usd = if ("spent_total_month" %in% names(.)) {
        suppressWarnings(as.numeric(spent_total_month)) / exchange_rate
      } else {
        NA_real_
      }
    )
}

health_complete <- analysis_population$complete_3_survey %>%
  derive_health_outcomes()

health_all_dedup <- analysis_population$all_deduplicated %>%
  derive_health_outcomes()

################################################################################
# Outcome list aligned with the existing RF105B health script
################################################################################

resp_vars_impacted <- c(
  "target_child_clinic_resp_yn",
  "target_child_wheezing_yn",
  "target_child_distrubed_speech_yn",
  "respondent_disturbed_speech_yn",
  "target_child_eye_red_yn",
  "target_child_eye_itch_yn",
  "respondent_eye_red_yn",
  "respondent_eye_itch_yn",
  "respondent_eye_sore_yn",
  "target_child_alri",
  "target_child_cough_yn",
  "target_child_fever_yn",
  "target_child_resp_rate_yn"
)

additional_rf105b_outcomes <- c(
  "healthcare_visit_6mo_yn",
  "depressed_yn",
  "sick_yn",
  "mental_health_difficulty_yn",
  "medical_usd",
  "total_expenditures_30_usd",
  "spent_total_month_usd"
)

health_outcomes <- c(resp_vars_impacted, additional_rf105b_outcomes)

health_labels <- c(
  target_child_clinic_resp_yn = "Child clinic visit for respiratory complaint",
  target_child_wheezing_yn = "Child wheeze",
  target_child_distrubed_speech_yn = "Child disturbed speech",
  respondent_disturbed_speech_yn = "Respondent disturbed speech",
  target_child_eye_red_yn = "Child red eyes",
  target_child_eye_itch_yn = "Child itchy eyes",
  respondent_eye_red_yn = "Respondent red eyes",
  respondent_eye_itch_yn = "Respondent itchy eyes",
  respondent_eye_sore_yn = "Respondent sore eyes",
  target_child_alri = "Child cough and fever",
  target_child_cough_yn = "Child persistent cough",
  target_child_fever_yn = "Child fever",
  target_child_resp_rate_yn = "Child increased respiratory rate",
  healthcare_visit_6mo_yn = "Any healthcare visit in past 6 months",
  depressed_yn = "Respondent felt depressed",
  sick_yn = "Respondent felt sick",
  mental_health_difficulty_yn = "Mental health difficulty",
  medical_usd = "Medical expenditures, USD",
  total_expenditures_30_usd = "Total expenditures in past 30 days, USD",
  spent_total_month_usd = "Monthly expenditures, USD"
)

binary_health_outcomes <- setdiff(health_outcomes, c(
  "medical_usd",
  "total_expenditures_30_usd",
  "spent_total_month_usd"
))

health_variable_availability <- flag_missing_vars(
  health_complete,
  health_outcomes,
  "RF105B reviewed health and expenditure outcomes"
)

write_reviewed_csv(
  health_variable_availability,
  "health_variable_availability.csv",
  subfolder = "qa"
)

health_outcomes_available <- health_variable_availability %>%
  filter(available) %>%
  pull(variable)

################################################################################
# Prevalence/mean summaries by timepoint and arm
################################################################################

summarise_health_outcomes <- function(df, population_label) {
  df %>%
    select(fcn_id, timepoint, study_arm_overall, all_of(health_outcomes_available)) %>%
    pivot_longer(
      cols = all_of(health_outcomes_available),
      names_to = "outcome",
      values_to = "value"
    ) %>%
    mutate(
      value = suppressWarnings(as.numeric(value)),
      outcome_label = recode(outcome, !!!health_labels, .default = outcome),
      outcome_type = if_else(
        outcome %in% binary_health_outcomes,
        "binary_percent",
        "continuous_mean"
      ),
      population = population_label
    ) %>%
    group_by(population, outcome, outcome_label, outcome_type,
             timepoint, study_arm_overall) %>%
    summarise(
      n_nonmissing = sum(!is.na(value)),
      mean_or_prevalence = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      mean_or_prevalence = if_else(
        outcome_type == "binary_percent",
        100 * mean_or_prevalence,
        mean_or_prevalence
      ),
      sd = if_else(is.nan(sd), NA_real_, sd)
    )
}

health_summary <- bind_rows(
  summarise_health_outcomes(health_complete, "complete_3_survey"),
  summarise_health_outcomes(health_all_dedup, "all_deduplicated_records")
)

write_reviewed_csv(
  health_summary,
  "health_outcome_summary_by_arm_timepoint_reviewed.csv"
)

################################################################################
# Unadjusted difference-in-differences sensitivity
################################################################################

run_health_did <- function(df, population_label) {
  map_dfr(health_outcomes_available, function(outcome) {
    bind_rows(
      did_lm_sensitivity(df, outcome, end_timepoint = "midline"),
      did_lm_sensitivity(df, outcome, end_timepoint = "endline")
    )
  }) %>%
    mutate(
      population = population_label,
      outcome_label = recode(outcome, !!!health_labels, .default = outcome),
      outcome_type = if_else(
        outcome %in% binary_health_outcomes,
        "binary_probability_points",
        "continuous_units"
      )
    ) %>%
    relocate(population, outcome, outcome_label, outcome_type)
}

health_did_sensitivity <- bind_rows(
  run_health_did(health_complete, "complete_3_survey"),
  run_health_did(health_all_dedup, "all_deduplicated_records")
) %>%
  mutate(
    estimate = if_else(outcome_type == "binary_probability_points",
                       100 * estimate, estimate),
    conf.low = if_else(outcome_type == "binary_probability_points",
                       100 * conf.low, conf.low),
    conf.high = if_else(outcome_type == "binary_probability_points",
                        100 * conf.high, conf.high),
    note = if_else(
      outcome_type == "binary_probability_points",
      paste0(note, "; binary estimates shown as percentage-point differences"),
      note
    )
  )

write_reviewed_csv(
  health_did_sensitivity,
  "health_unadjusted_did_sensitivity_reviewed.csv"
)

################################################################################
# Child symptom figure for manuscript QA
################################################################################

child_figure_outcomes <- intersect(
  c(
    "target_child_cough_yn",
    "target_child_resp_rate_yn",
    "target_child_wheezing_yn",
    "target_child_eye_red_yn",
    "target_child_eye_itch_yn",
    "target_child_fever_yn",
    "target_child_clinic_resp_yn"
  ),
  health_outcomes_available
)

if (length(child_figure_outcomes) > 0) {
  child_plot_data <- health_summary %>%
    filter(
      population == "complete_3_survey",
      outcome %in% child_figure_outcomes,
      outcome_type == "binary_percent"
    ) %>%
    mutate(
      outcome_label = factor(
        outcome_label,
        levels = health_labels[child_figure_outcomes]
      )
    )

  fig_child_health <- ggplot(
    child_plot_data,
    aes(x = timepoint, y = mean_or_prevalence, fill = study_arm_overall)
  ) +
    geom_col(position = position_dodge(width = 0.75), width = 0.68) +
    facet_wrap(~ outcome_label, ncol = 4) +
    scale_y_continuous(labels = label_number(suffix = "%")) +
    scale_fill_manual(
      values = c(comparison = "#4E79A7", intervention = "#F28E2B"),
      na.translate = FALSE
    ) +
    labs(
      x = NULL,
      y = "Households reporting symptom",
      fill = "Study arm"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )

  save_reviewed_plot(
    fig_child_health,
    "health_child_symptoms_reviewed.png",
    width = 10,
    height = 5.5
  )
}

