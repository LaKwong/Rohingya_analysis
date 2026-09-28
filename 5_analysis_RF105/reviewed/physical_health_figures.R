################################################################################
# RF105 physical-health descriptive figures
#
# Purpose:
#   Generate the child/caregiver physical-health summary and both publication
#   figure filenames from one shared definition. Caregiver respiratory rate is
#   harmonized across its baseline/midline and endline spellings. Wheeze
#   follow-up outcomes count structural skips among participants reporting no
#   current wheeze as no, while retaining true missing values among participants
#   reporting wheeze or with missing wheeze status.
#
# Input:
#   4_data/clean_final/survey_refugee_household.rds
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_physical_health_symptoms.csv
#   7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_health_panel_plot_data.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/
#     table_descriptive_physical_health_denominator_audit.csv
#     table_descriptive_caregiver_resp_rate_harmonization.csv
#   6_figures/RF105_reviewed_YYYYMMDD/fig_descriptive_physical_health_symptoms.png
#   6_figures/RF105_reviewed_YYYYMMDD/fig_descriptive_health_symptom_panel.png
#
# Expected standalone use from the project root:
#   source("5_analysis_RF105/reviewed/physical_health_figures.R")
################################################################################

get_physical_health_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) == 1L) {
    return(dirname(normalizePath(
      sub("^--file=", "", file_arg),
      winslash = "/",
      mustWork = FALSE
    )))
  }

  source_files <- vapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile)) frame$ofile else NA_character_
  }, character(1))
  source_files <- source_files[!is.na(source_files)]
  if (length(source_files) > 0) {
    return(dirname(normalizePath(
      source_files[[length(source_files)]],
      winslash = "/",
      mustWork = FALSE
    )))
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

physical_health_script_dir <- get_physical_health_script_dir()
if (!exists("file_survey_refugee_household", inherits = TRUE) ||
    !exists("save_reviewed_plot", inherits = TRUE)) {
  source(file.path(
    physical_health_script_dir,
    "0_RF105_config_20260805_2213.R"
  ))
}

if (!exists("survey", inherits = TRUE)) {
  survey_raw_physical_health <- readr::read_rds(file_survey_refugee_household) %>%
    add_rf105_aliases()
  survey <- make_analysis_population(survey_raw_physical_health)$all_deduplicated
}

physical_health_yn_col <- function(df, var) {
  if (var %in% names(df)) {
    make_yn(df[[var]])
  } else {
    rep(NA_integer_, nrow(df))
  }
}

physical_health_skip_as_no <- function(value_yn, wheeze_yn) {
  value_yn <- as.integer(value_yn)
  wheeze_yn <- as.integer(wheeze_yn)
  case_when(
    !is.na(value_yn) ~ value_yn,
    wheeze_yn == 0 ~ 0L,
    wheeze_yn == 1 ~ NA_integer_,
    TRUE ~ NA_integer_
  )
}

add_health_descriptive_vars <- function(df) {
  health_vars <- c(
    "target_child_eye_red", "target_child_eye_itch", "target_child_cough",
    "target_child_resp_rate", "target_child_fever", "target_child_weight_loss",
    "target_child_lethargy", "target_child_clinic_resp", "target_child_wheezing",
    "target_child_disturbed_sleep", "target_child_distrubed_speech",
    "respondent_cough", "respondent_wheezing", "respondent_disturbed_sleep",
    "respondent_disturbed_speech", "respondent_eye_red", "respondent_eye_itch",
    "respondent_eye_sore", "respondent_headache", "respondent_backache",
    "resp_rate_reported_respondant", "resp_rate_reported_respondent",
    "weight_loss_reported_respondent"
  )

  for (var in intersect(health_vars, names(df))) {
    yn_name <- paste0(var, "_yn")
    if (!yn_name %in% names(df)) {
      df[[yn_name]] <- make_yn(df[[var]])
    }
  }

  if ("target_child_distrubed_speech_yn" %in% names(df) &&
      "target_child_disturbed_speech_yn" %notin% names(df)) {
    df$target_child_disturbed_speech_yn <- df$target_child_distrubed_speech_yn
  }

  child_wheeze <- physical_health_yn_col(df, "target_child_wheezing_yn")
  caregiver_wheeze <- physical_health_yn_col(df, "respondent_wheezing_yn")
  resp_rate_respondant <- physical_health_yn_col(
    df, "resp_rate_reported_respondant_yn"
  )
  resp_rate_respondent <- physical_health_yn_col(
    df, "resp_rate_reported_respondent_yn"
  )

  df <- df %>%
    mutate(
      respondent_resp_rate_yn = case_when(
        as.character(timepoint) %in% c("baseline", "midline") ~
          coalesce(resp_rate_respondant, resp_rate_respondent),
        as.character(timepoint) == "endline" ~
          coalesce(resp_rate_respondent, resp_rate_respondant),
        TRUE ~ coalesce(resp_rate_respondent, resp_rate_respondant)
      ),
      target_child_disturbed_sleep_all_households_yn =
        physical_health_skip_as_no(
          physical_health_yn_col(., "target_child_disturbed_sleep_yn"),
          child_wheeze
        ),
      target_child_disturbed_speech_all_households_yn =
        physical_health_skip_as_no(
          physical_health_yn_col(., "target_child_disturbed_speech_yn"),
          child_wheeze
        ),
      respondent_disturbed_sleep_all_households_yn =
        physical_health_skip_as_no(
          physical_health_yn_col(., "respondent_disturbed_sleep_yn"),
          caregiver_wheeze
        ),
      respondent_disturbed_speech_all_households_yn =
        physical_health_skip_as_no(
          physical_health_yn_col(., "respondent_disturbed_speech_yn"),
          caregiver_wheeze
        )
    )

  if ("target_child_wheezing_yn" %in% names(df)) {
    df <- df %>%
      mutate(
        target_child_asthma = case_when(
          target_child_wheezing_yn == 1 ~ 1L,
          target_child_wheezing_yn == 0 ~ 0L,
          TRUE ~ NA_integer_
        )
      )
  }

  derive_child_severe_asthma_vars(df)
}

physical_health_labels <- tibble(
  source_variable = c(
    "target_child_cough_yn",
    "target_child_resp_rate_yn",
    "target_child_wheezing_yn",
    "target_child_disturbed_sleep_all_households_yn",
    "target_child_disturbed_speech_all_households_yn",
    "target_child_eye_red_yn",
    "target_child_eye_itch_yn",
    "target_child_lethargy_yn",
    "target_child_weight_loss_yn",
    "target_child_fever_yn",
    "target_child_clinic_resp_yn",
    "lpg_child_burn",
    "respondent_cough_yn",
    "respondent_resp_rate_yn",
    "respondent_wheezing_yn",
    "respondent_disturbed_sleep_all_households_yn",
    "respondent_disturbed_speech_all_households_yn",
    "respondent_eye_red_yn",
    "respondent_eye_itch_yn",
    "respondent_eye_sore_yn",
    "weight_loss_reported_respondent_yn",
    "respondent_headache_yn",
    "respondent_backache_yn"
  ),
  outcome_name = source_variable,
  outcome_label = c(
    "Persistent cough",
    "Increased respiratory rate today",
    "Current wheeze",
    "Sleep disturbed sleep by wheeze",
    "Speech disturbed by wheeze",
    "Red eyes",
    "Itchy eyes",
    "Lethargy",
    "Unexplained weight loss in 3 mo.",
    "Fever",
    "Clinic visit for respiratory complaint",
    "Burned by LPG",
    "Persistent cough",
    "Increased respiratory rate today",
    "Current wheeze",
    "Sleep disturbed by wheeze",
    "Speech disturbed by wheeze",
    "Red eyes",
    "Itchy eyes",
    "Sore eyes",
    "Unexplained weight loss in 3 mo.",
    "Headache",
    "Backache"
  ),
  respondent_group = c(rep("Child", 12), rep("Caregiver", 11)),
  display_order = seq_len(23)
)

physical_health_figure_exclusions <- c(
  "lpg_child_burn",
  "target_child_weight_loss_yn",
  "weight_loss_reported_respondent_yn"
)
physical_health_all_household_vars <- c(
  "target_child_disturbed_sleep_all_households_yn",
  "target_child_disturbed_speech_all_households_yn",
  "respondent_disturbed_sleep_all_households_yn",
  "respondent_disturbed_speech_all_households_yn"
)

survey_health <- add_health_descriptive_vars(survey)
physical_health_labels <- physical_health_labels %>%
  filter(source_variable %in% names(survey_health))

physical_health_add_all_arms <- function(df) {
  arm_rows <- df %>%
    mutate(study_arm_overall = as.character(study_arm_overall))
  all_rows <- arm_rows %>%
    filter(study_arm_overall %in% arm_levels) %>%
    mutate(study_arm_overall = "all_arms")
  bind_rows(arm_rows, all_rows)
}

physical_health_summary <- purrr::map_dfr(
  seq_len(nrow(physical_health_labels)),
  function(i) {
    var <- physical_health_labels$source_variable[[i]]
    tibble(
      fcn_id = survey_health$fcn_id,
      timepoint = survey_health$timepoint,
      study_arm_overall = survey_health$study_arm_overall,
      outcome_group = "physical_health_outcomes",
      outcome_name = physical_health_labels$outcome_name[[i]],
      outcome_label = physical_health_labels$outcome_label[[i]],
      source_variable = var,
      respondent_group = physical_health_labels$respondent_group[[i]],
      display_order = physical_health_labels$display_order[[i]],
      unit = "percent",
      population = "all_deduplicated_household_timepoint_records",
      value = as.integer(survey_health[[var]])
    )
  }
) %>%
  physical_health_add_all_arms() %>%
  group_by(
    timepoint, study_arm_overall, outcome_group, outcome_name, outcome_label,
    source_variable, respondent_group, display_order, unit, population
  ) %>%
  summarise(
    n_total = n(),
    n_nonmissing = sum(!is.na(value)),
    n_yes = sum(value == 1L, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    proportion_nonmissing = if_else(
      n_nonmissing > 0,
      n_yes / n_nonmissing,
      NA_real_
    ),
    percent_nonmissing = 100 * proportion_nonmissing,
    se_nonmissing = if_else(
      n_nonmissing > 0,
      sqrt(proportion_nonmissing * (1 - proportion_nonmissing) / n_nonmissing),
      NA_real_
    ),
    ci_lower_nonmissing = pmax(
      0,
      100 * (proportion_nonmissing - qnorm(0.975) * se_nonmissing)
    ),
    ci_upper_nonmissing = pmin(
      100,
      100 * (proportion_nonmissing + qnorm(0.975) * se_nonmissing)
    ),
    proportion_total = if_else(n_total > 0, n_yes / n_total, NA_real_),
    percent_total = 100 * proportion_total,
    se_total = if_else(
      n_total > 0,
      sqrt(proportion_total * (1 - proportion_total) / n_total),
      NA_real_
    ),
    ci_lower_total = pmax(0, 100 * (proportion_total - qnorm(0.975) * se_total)),
    ci_upper_total = pmin(100, 100 * (proportion_total + qnorm(0.975) * se_total)),
    denominator_definition = if_else(
      source_variable %in% physical_health_all_household_vars,
      "all household-timepoint records",
      "nonmissing outcome responses"
    ),
    n_denominator = if_else(
      source_variable %in% physical_health_all_household_vars,
      n_total,
      n_nonmissing
    ),
    proportion = if_else(
      source_variable %in% physical_health_all_household_vars,
      proportion_total,
      proportion_nonmissing
    ),
    percent = 100 * proportion,
    se = if_else(
      source_variable %in% physical_health_all_household_vars,
      se_total,
      se_nonmissing
    ),
    ci_lower = if_else(
      source_variable %in% physical_health_all_household_vars,
      ci_lower_total,
      ci_lower_nonmissing
    ),
    ci_upper = if_else(
      source_variable %in% physical_health_all_household_vars,
      ci_upper_total,
      ci_upper_nonmissing
    )
  ) %>%
  arrange(
    display_order,
    factor(as.character(timepoint), levels = timepoint_levels),
    factor(as.character(study_arm_overall), levels = c(arm_levels, "all_arms"))
  )

write_reviewed_csv(
  physical_health_summary,
  "table_descriptive_physical_health_symptoms.csv"
)

physical_health_denominator_audit <- physical_health_summary %>%
  filter(
    source_variable %in% physical_health_all_household_vars,
    study_arm_overall %in% arm_levels
  ) %>%
  mutate(
    denominator_note = paste(
      "Percentages use all household-timepoint records as the denominator.",
      "Participants reporting no current wheeze are coded 0 when the",
      "follow-up response is structurally missing; true missing responses",
      "are retained in n_nonmissing but remain part of the requested",
      "all-household denominator."
    )
  ) %>%
  select(
    respondent_group, outcome_label, source_variable, timepoint,
    study_arm_overall, denominator_definition, n_denominator,
    n_total, n_nonmissing, n_yes, percent, ci_lower, ci_upper,
    denominator_note
  )
write_reviewed_csv(
  physical_health_denominator_audit,
  "table_descriptive_physical_health_denominator_audit.csv",
  subfolder = "qa"
)

physical_health_resp_rate_harmonization <- survey_health %>%
  transmute(
    timepoint,
    study_arm_overall,
    respondant_value = physical_health_yn_col(
      survey_health, "resp_rate_reported_respondant_yn"
    ),
    respondent_value = physical_health_yn_col(
      survey_health, "resp_rate_reported_respondent_yn"
    ),
    harmonized_value = respondent_resp_rate_yn
  ) %>%
  filter(study_arm_overall %in% arm_levels) %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_households = n(),
    n_nonmissing_respondant_spelling = sum(!is.na(respondant_value)),
    n_nonmissing_respondent_spelling = sum(!is.na(respondent_value)),
    n_nonmissing_harmonized = sum(!is.na(harmonized_value)),
    n_yes_harmonized = sum(harmonized_value == 1L, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    factor(as.character(timepoint), levels = timepoint_levels),
    factor(as.character(study_arm_overall), levels = arm_levels)
  )
write_reviewed_csv(
  physical_health_resp_rate_harmonization,
  "table_descriptive_caregiver_resp_rate_harmonization.csv",
  subfolder = "qa"
)

physical_health_figure_labels <- physical_health_labels %>%
  filter(source_variable %notin% physical_health_figure_exclusions)

stopifnot(
  nrow(filter(physical_health_figure_labels, respondent_group == "Child")) == 10L,
  nrow(filter(physical_health_figure_labels, respondent_group == "Caregiver")) == 10L,
  "respondent_resp_rate_yn" %in% physical_health_figure_labels$source_variable,
  "target_child_weight_loss_yn" %notin% physical_health_figure_labels$source_variable,
  all(
    filter(
      physical_health_summary,
      source_variable %in% physical_health_all_household_vars,
      study_arm_overall %in% arm_levels
    )$n_denominator ==
      filter(
        physical_health_summary,
        source_variable %in% physical_health_all_household_vars,
        study_arm_overall %in% arm_levels
      )$n_total
  ),
  all(c("baseline", "midline", "endline") %in%
        as.character(filter(
          physical_health_resp_rate_harmonization,
          n_nonmissing_harmonized > 0
        )$timepoint))
)

physical_health_plot_data <- physical_health_summary %>%
  filter(
    n_nonmissing > 0,
    !is.na(percent),
    study_arm_overall %in% arm_levels,
    source_variable %notin% physical_health_figure_exclusions
  ) %>%
  mutate(
    timepoint = factor(as.character(timepoint), levels = timepoint_levels),
    study_arm_overall = factor(
      as.character(study_arm_overall),
      levels = arm_levels
    ),
    respondent_group = factor(respondent_group, levels = c("Child", "Caregiver")),
    facet_label = factor(
      str_wrap(paste(respondent_group, outcome_label, sep = ": "), width = 24),
      levels = str_wrap(
        paste(
          physical_health_figure_labels$respondent_group,
          physical_health_figure_labels$outcome_label,
          sep = ": "
        ),
        width = 24
      )
    )
  )

write_reviewed_csv(
  physical_health_plot_data,
  "table_descriptive_health_panel_plot_data.csv"
)

fig_physical_health <- ggplot(
  physical_health_plot_data,
  aes(
    x = timepoint,
    y = percent,
    color = study_arm_overall,
    group = study_arm_overall
  )
) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 1.8, na.rm = TRUE) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.08,
    linewidth = 0.4,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ facet_label,
    ncol = sum(physical_health_figure_labels$respondent_group == "Child")
  ) +
  scale_color_manual(
    values = rf105_arm_colors[arm_levels],
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = function(x) paste0(round(x), "%"),
    limits = c(0, 100)
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    strip.text = element_text(size = 8)
  ) +
  labs(
    x = "Timepoint",
    y = "Percent reporting outcome",
    color = "Study arm"
  )

for (physical_health_figure_filename in c(
  "fig_descriptive_physical_health_symptoms.png",
  "fig_descriptive_health_symptom_panel.png"
)) {
  save_reviewed_plot(
    fig_physical_health,
    physical_health_figure_filename,
    width = 16,
    height = 8
  )
}

message("Physical-health descriptive figures complete.")
