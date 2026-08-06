################################################################################
# RF105 reviewed PM2.5 analysis and time-of-day figure
#
# Purpose:
#   Recreate the indoor PM2.5 summaries and time-of-day figure using the final
#   cleaned PATS PM2.5 files.
#
# Inputs:
#   4_data/clean_final/pm25_pats_refugee_indoor.rds
#   4_data/clean_final/pm25_pats_refugee_ambient.rds
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/pm25_*.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/pm25_*.csv
#   6_figures/RF105_reviewed_YYYYMMDD/pm25_time_of_day_reviewed.png
#
# Sensitivity analyses:
#   1. Main RF105B-style LOD handling: values below 10 ug/m3 set to 10.
#   2. No LOD substitution.
#   3. Main LOD handling plus exclusion of the highest 2.5% of values.
#   4. Summary stratified by season when a season variable is available.
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

pm_indoor_raw <- readRDS(file_pm25_indoor) %>%
  clean_timepoint_arm()

pm_ambient_raw <- readRDS(file_pm25_ambient) %>%
  clean_timepoint_arm()

detect_first_existing <- function(df, candidates, label) {
  found <- candidates[candidates %in% names(df)]
  if (length(found) == 0) {
    stop("Could not find ", label, ". Checked: ", paste(candidates, collapse = ", "))
  }
  found[[1]]
}

pm_value_var <- detect_first_existing(
  pm_indoor_raw,
  c("PM_Estimate", "pm25_ug_m3", "pm25", "pm2_5", "pm2.5"),
  "PM2.5 concentration column"
)

datetime_var <- detect_first_existing(
  pm_indoor_raw,
  c("dateTime", "datetime", "date_time", "sample_datetime", "DateTime"),
  "date-time column"
)

household_id_var <- detect_first_existing(
  pm_indoor_raw,
  c("fcn_id", "hh_id", "household_id", "pats_household_id"),
  "household ID column"
)

season_var <- intersect(
  c("season_monitored", "season", "monitoring_season"),
  names(pm_indoor_raw)
)
season_var <- if (length(season_var) == 0) NA_character_ else season_var[[1]]

pm_required_vars <- c(
  pm_value_var, datetime_var, household_id_var,
  "timepoint", "study_arm_overall", season_var
)

write_reviewed_csv(
  flag_missing_vars(
    pm_indoor_raw,
    pm_required_vars[!is.na(pm_required_vars)],
    "RF105 PM2.5 analysis variables"
  ),
  "pm25_variable_availability.csv",
  subfolder = "qa"
)

################################################################################
# Prepare PM2.5 data
################################################################################

pm_indoor <- pm_indoor_raw %>%
  mutate(
    pm25_raw = suppressWarnings(as.numeric(.data[[pm_value_var]])),
    monitor_datetime = as.POSIXct(.data[[datetime_var]],
                                  tz = "Asia/Dhaka"),
    household_id = as.character(.data[[household_id_var]])
  ) %>%
  filter(
    !is.na(pm25_raw),
    !is.na(monitor_datetime),
    !is.na(timepoint),
    !is.na(study_arm_overall),
    !is.na(household_id)
  )

if ("include_in_indoor_final" %in% names(pm_indoor)) {
  pm_indoor <- pm_indoor %>%
    filter(is.na(include_in_indoor_final) | include_in_indoor_final == TRUE)
}

pm_ambient <- pm_ambient_raw %>%
  mutate(
    pm25_raw = suppressWarnings(as.numeric(.data[[pm_value_var]])),
    monitor_datetime = as.POSIXct(.data[[datetime_var]],
                                  tz = "Asia/Dhaka")
  ) %>%
  filter(!is.na(pm25_raw), !is.na(monitor_datetime))

pm_qc_counts <- bind_rows(
  pm_indoor_raw %>%
    summarise(
      file = "pm25_pats_refugee_indoor.rds",
      n_rows_raw = n(),
      n_pm_nonmissing = sum(!is.na(.data[[pm_value_var]])),
      n_datetime_nonmissing = sum(!is.na(.data[[datetime_var]])),
      n_households = n_distinct(.data[[household_id_var]], na.rm = TRUE)
    ),
  pm_indoor %>%
    summarise(
      file = "pm25_pats_refugee_indoor.rds_after_review_filters",
      n_rows_raw = n(),
      n_pm_nonmissing = sum(!is.na(pm25_raw)),
      n_datetime_nonmissing = sum(!is.na(monitor_datetime)),
      n_households = n_distinct(household_id, na.rm = TRUE)
    )
)

write_reviewed_csv(pm_qc_counts, "pm25_qc_counts.csv", subfolder = "qa")

make_pm_scenario <- function(df, scenario) {
  out <- df

  if (scenario %in% c("main_lod10", "trim_top_2_5_percent_lod10")) {
    out <- out %>%
      mutate(pm25_reviewed = pmax(pm25_raw, 10))
  } else if (scenario == "no_lod_substitution") {
    out <- out %>%
      mutate(pm25_reviewed = pm25_raw)
  } else {
    stop("Unknown PM2.5 scenario: ", scenario)
  }

  if (scenario == "trim_top_2_5_percent_lod10") {
    trim_value <- quantile(out$pm25_reviewed, probs = 0.975, na.rm = TRUE)
    out <- out %>%
      filter(pm25_reviewed <= trim_value)
  }

  out %>%
    mutate(scenario = scenario)
}

pm_scenarios <- bind_rows(
  make_pm_scenario(pm_indoor, "main_lod10"),
  make_pm_scenario(pm_indoor, "no_lod_substitution"),
  make_pm_scenario(pm_indoor, "trim_top_2_5_percent_lod10")
)

################################################################################
# Summary table by timepoint and study arm
################################################################################

summarise_pm <- function(df, grouping_vars) {
  df %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      n_observations = n(),
      n_households = n_distinct(household_id),
      mean_pm25 = mean(pm25_reviewed, na.rm = TRUE),
      median_pm25 = median(pm25_reviewed, na.rm = TRUE),
      p25_pm25 = quantile(pm25_reviewed, probs = 0.25, na.rm = TRUE),
      p75_pm25 = quantile(pm25_reviewed, probs = 0.75, na.rm = TRUE),
      p95_pm25 = quantile(pm25_reviewed, probs = 0.95, na.rm = TRUE),
      max_pm25 = max(pm25_reviewed, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(across(all_of(grouping_vars)))
}

pm_summary <- pm_scenarios %>%
  summarise_pm(c("scenario", "timepoint", "study_arm_overall"))

write_reviewed_csv(pm_summary, "pm25_summary_by_arm_timepoint_reviewed.csv")

if (!is.na(season_var)) {
  pm_season_summary <- pm_scenarios %>%
    mutate(season_reviewed = as.character(.data[[season_var]])) %>%
    summarise_pm(c("scenario", "season_reviewed", "timepoint", "study_arm_overall"))

  write_reviewed_csv(pm_season_summary, "pm25_summary_by_season_reviewed.csv")
}

################################################################################
# Household-level unadjusted DiD sensitivity
################################################################################

pm_household_mean <- pm_scenarios %>%
  filter(scenario == "main_lod10") %>%
  group_by(household_id, timepoint, study_arm_overall) %>%
  summarise(
    mean_pm25 = mean(pm25_reviewed, na.rm = TRUE),
    median_pm25 = median(pm25_reviewed, na.rm = TRUE),
    n_observations = n(),
    .groups = "drop"
  ) %>%
  mutate(fcn_id = household_id)

pm_did_sensitivity <- bind_rows(
  did_lm_sensitivity(pm_household_mean, "mean_pm25", end_timepoint = "midline"),
  did_lm_sensitivity(pm_household_mean, "mean_pm25", end_timepoint = "endline")
)

write_reviewed_csv(
  pm_did_sensitivity,
  "pm25_household_mean_unadjusted_did_sensitivity.csv"
)

################################################################################
# Time-of-day figure
################################################################################

pm_time_of_day <- pm_scenarios %>%
  filter(scenario == "main_lod10") %>%
  mutate(hour = hour(monitor_datetime)) %>%
  group_by(timepoint, study_arm_overall, hour) %>%
  summarise(
    mean_pm25 = mean(pm25_reviewed, na.rm = TRUE),
    p25_pm25 = quantile(pm25_reviewed, probs = 0.25, na.rm = TRUE),
    p75_pm25 = quantile(pm25_reviewed, probs = 0.75, na.rm = TRUE),
    n_observations = n(),
    n_households = n_distinct(household_id),
    .groups = "drop"
  ) %>%
  filter(n_households >= 3)

write_reviewed_csv(pm_time_of_day, "pm25_time_of_day_plot_data_reviewed.csv")

fig_pm_time_of_day <- ggplot(
  pm_time_of_day,
  aes(x = hour, y = mean_pm25, color = study_arm_overall, fill = study_arm_overall)
) +
  geom_ribbon(
    aes(ymin = p25_pm25, ymax = p75_pm25),
    alpha = 0.16,
    color = NA
  ) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ timepoint, nrow = 1) +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  scale_y_log10(labels = label_number()) +
  scale_color_manual(
    values = c(comparison = "#4E79A7", intervention = "#F28E2B"),
    na.translate = FALSE
  ) +
  scale_fill_manual(
    values = c(comparison = "#4E79A7", intervention = "#F28E2B"),
    na.translate = FALSE
  ) +
  labs(
    x = "Hour of day",
    y = expression(PM[2.5]~(mu*g/m^3)),
    color = "Study arm",
    fill = "Study arm"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

save_reviewed_plot(
  fig_pm_time_of_day,
  "pm25_time_of_day_reviewed.png",
  width = 9,
  height = 4
)

################################################################################
# Ambient PM2.5 summary, for comparison and manuscript QA
################################################################################

if (nrow(pm_ambient) > 0) {
  pm_ambient_summary <- pm_ambient %>%
    mutate(pm25_reviewed = pmax(pm25_raw, 10)) %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise(
      n_observations = n(),
      mean_pm25 = mean(pm25_reviewed, na.rm = TRUE),
      median_pm25 = median(pm25_reviewed, na.rm = TRUE),
      p25_pm25 = quantile(pm25_reviewed, probs = 0.25, na.rm = TRUE),
      p75_pm25 = quantile(pm25_reviewed, probs = 0.75, na.rm = TRUE),
      p95_pm25 = quantile(pm25_reviewed, probs = 0.95, na.rm = TRUE),
      max_pm25 = max(pm25_reviewed, na.rm = TRUE),
      .groups = "drop"
    )

  write_reviewed_csv(pm_ambient_summary, "pm25_ambient_summary_reviewed.csv")
}

