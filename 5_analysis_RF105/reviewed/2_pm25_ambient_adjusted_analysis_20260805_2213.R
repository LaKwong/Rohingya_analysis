# Ambient-adjusted PM2.5 analysis for Rohingya refugee household monitoring
#
# Purpose:
#   Summarize indoor PATS+ PM2.5 to household monitoring windows, match each
#   household window to concurrent ambient PM2.5, and compare intervention and
#   comparison households with temporal/ambient adjustment and sensitivity
#   analyses.
#
# Inputs:
#   4_data/clean_final/pm25_pats_refugee_indoor.rds
#   4_data/clean_final/pm25_pats_refugee_ambient.rds
#   4_data/clean_final/survey_refugee_household.rds
#   4_data/clean_final/survey_refugee_location.rds
#   4_data/clean_final/survey_refugee_hh_members.rds
#
# Outputs:
#   7_tables/pm25_ambient_adjusted_<YYYYMMDD>/
#   8_restricted/pm25_ambient_adjusted_<YYYYMMDD>/
#   6_figures/pm25_ambient_adjusted_<YYYYMMDD>/

options(stringsAsFactors = FALSE)

required_packages <- c("dplyr", "readr", "lubridate", "ggplot2", "lme4", "splines", "xgboost")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing package(s) for this script: ",
    paste(missing_packages, collapse = ", "),
    ". Run renv::restore() from the project root, then rerun.",
    call. = FALSE
  )
}

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(lme4)

timepoint_levels <- c("baseline", "midline", "endline")
arm_levels <- c("comparison", "intervention")

as_ordered_timepoint <- function(x, extra_levels = character()) {
  x_clean <- trimws(tolower(as.character(x)))
  factor(
    x_clean,
    levels = c(timepoint_levels, extra_levels),
    ordered = TRUE
  )
}
primary_model_label <- "primary_log_indoor_ambient_calendar_lmer"
default_material_infiltration_factor <- 0.75
sensitivity_infiltration_factors <- c(0.25, 0.50, 1.00)
valid_monitoring_coverage_threshold <- 0.75
monitoring_period_hours <- 24
monitoring_period_count <- 2
monitoring_total_hours <- monitoring_period_hours * monitoring_period_count
expected_monitoring_interval_seconds <- 60
long_monitoring_interval_threshold_seconds <- expected_monitoring_interval_seconds
hapin_hour_time_zone <- "Asia/Dhaka"
hapin_pm25_reference_lines <- c(15, 25, 35, 37.5, 50, 75)
hapin_pm25_exceedance_thresholds <- c(0, 15, 25, 35, 37.5, 50, 75, 150, 400, 1000)

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

find_project_root <- function(start_dir) {
  current <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)
  repeat {
    if (
      file.exists(file.path(current, "Rohingya_analysis.Rproj")) ||
        file.exists(file.path(current, ".here"))
    ) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find Rohingya_analysis project root from: ", start_dir, call. = FALSE)
    }
    current <- parent
  }
}

script_path <- get_script_path()
script_dir <- if (dir.exists(script_path)) script_path else dirname(script_path)
project_root_env <- Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = "")
project_root <- if (nzchar(project_root_env)) {
  normalizePath(project_root_env, winslash = "/", mustWork = TRUE)
} else {
  find_project_root(script_dir)
}

clean_final_dir <- file.path(project_root, "4_data", "clean_final")
input_indoor_path <- file.path(clean_final_dir, "pm25_pats_refugee_indoor.rds")
input_indoor_anomaly_retained_path <- NA_character_
input_ambient_path <- file.path(clean_final_dir, "pm25_pats_refugee_ambient.rds")
input_survey_household_path <- file.path(clean_final_dir, "survey_refugee_household.rds")
input_survey_location_path <- file.path(clean_final_dir, "survey_refugee_location.rds")
input_survey_hh_members_path <- file.path(clean_final_dir, "survey_refugee_hh_members.rds")

analysis_date <- format(Sys.Date(), "%Y%m%d")
table_dir <- file.path(project_root, "7_tables", paste0("pm25_ambient_adjusted_", analysis_date))
restricted_table_dir <- file.path(project_root, "8_restricted", paste0("pm25_ambient_adjusted_", analysis_date))
figure_dir <- file.path(project_root, "6_figures", paste0("pm25_ambient_adjusted_", analysis_date))
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(restricted_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
quarantine_restricted_pm_csvs <- function() {
  # Old runs wrote ID-level PM datasets under 7_tables. Move them to the
  # restricted tree before writing fresh public outputs for the current run.
  restricted_public_files <- c(
    "table_pm25_window_dataset_internal.csv",
    "table_pm25_window_dataset_anomaly_retained_internal.csv",
    "table_rDiD_pm25_panel_internal.csv",
    "table_rDiD_pm25_panel_anomaly_retained_internal.csv",
    "table_descriptive_pm25_hours_internal.csv"
  )
  quarantine_dir <- file.path(restricted_table_dir, "quarantined_from_7_tables")
  dir.create(quarantine_dir, recursive = TRUE, showWarnings = FALSE)
  for (file_name in restricted_public_files) {
    from <- file.path(table_dir, file_name)
    if (file.exists(from)) {
      to <- file.path(quarantine_dir, file_name)
      if (file.exists(to)) {
        to <- file.path(
          quarantine_dir,
          paste0(tools::file_path_sans_ext(file_name), "_", format(Sys.time(), "%H%M%S"), ".csv")
        )
      }
      file.rename(from, to)
    }
  }
}
quarantine_restricted_pm_csvs()

check_required_columns <- function(data, required_cols, data_name) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      data_name,
      " is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}

geo_mean <- function(x) {
  x <- x[is.finite(x) & !is.na(x) & x > 0]
  if (length(x) == 0) return(NA_real_)
  exp(mean(log(x)))
}

first_nonmissing <- function(x) {
  if (is.character(x)) {
    x <- x[!is.na(x) & nzchar(x)]
  } else {
    x <- x[!is.na(x)]
  }
  if (length(x) == 0) return(NA)
  x[[1]]
}

as_number <- function(x) {
  suppressWarnings(as.numeric(x))
}

weighted_mean_pm <- function(x, w) {
  x <- as_number(x)
  w <- as_number(w)
  keep <- !is.na(x) & is.finite(x) & !is.na(w) & is.finite(w) & w > 0
  if (!any(keep)) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

clean_hours_0_24 <- function(x) {
  x <- as_number(x)
  x[!is.finite(x) | x < 0 | x > 24] <- NA_real_
  x
}

clean_age_years <- function(x) {
  x <- as_number(x)
  x[!is.finite(x) | x < 0 | x > 120] <- NA_real_
  x
}

mean_or_na <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

safe_quantile <- function(x, prob) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE, type = 7))
}

mean_datetime <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  as.POSIXct(mean(as.numeric(x)), origin = "1970-01-01", tz = "UTC")
}

as_clean_datetime <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  parsed <- suppressWarnings(lubridate::ymd_hms(x, quiet = TRUE, tz = "UTC"))
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(lubridate::ymd_hm(x, quiet = TRUE, tz = "UTC"))
  }
  parsed
}

median_or_na <- function(x) {
  x <- as_number(x)
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  median(x)
}

min_or_na <- function(x) {
  x <- as_number(x)
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

max_or_na <- function(x) {
  x <- as_number(x)
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

mode_number <- function(x) {
  x <- as_number(x)
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  x <- round(x, 3)
  tab <- table(x)
  as.numeric(names(tab)[which.max(tab)])
}

pct_at_mode <- function(x) {
  x <- as_number(x)
  x <- x[!is.na(x) & is.finite(x)]
  mode_x <- mode_number(x)
  if (length(x) == 0 || is.na(mode_x) || !is.finite(mode_x)) return(NA_real_)
  mean(round(x, 3) == mode_x) * 100
}

summarize_monitoring_period_coverage <- function(timestamp_rows, start_datetime, usual_interval_seconds, period_number) {
  period_start <- start_datetime + lubridate::hours(monitoring_period_hours * (period_number - 1L))
  period_end <- start_datetime + lubridate::hours(monitoring_period_hours * period_number)

  if (
    nrow(timestamp_rows) == 0 ||
      is.na(usual_interval_seconds) ||
      !is.finite(usual_interval_seconds) ||
      usual_interval_seconds <= 0
  ) {
    covered_seconds <- 0
    n_valid_timestamps <- 0L
  } else {
    represented_seconds <- ifelse(
      !is.na(timestamp_rows$positive_interval_seconds) &
        is.finite(timestamp_rows$positive_interval_seconds) &
        timestamp_rows$positive_interval_seconds > 0,
      pmin(timestamp_rows$positive_interval_seconds, usual_interval_seconds),
      usual_interval_seconds
    )
    observation_start <- as.numeric(timestamp_rows$dateTime)
    observation_end <- observation_start + represented_seconds
    period_start_num <- as.numeric(period_start)
    period_end_num <- as.numeric(period_end)
    overlap_seconds <- pmax(
      0,
      pmin(observation_end, period_end_num) - pmax(observation_start, period_start_num)
    )
    covered_seconds <- sum(overlap_seconds, na.rm = TRUE)
    n_valid_timestamps <- sum(timestamp_rows$dateTime >= period_start & timestamp_rows$dateTime < period_end, na.rm = TRUE)
  }

  coverage_prop <- min(1, covered_seconds / (monitoring_period_hours * 3600))
  data.frame(
    period_number = period_number,
    period_start_datetime = period_start,
    period_end_datetime = period_end,
    n_valid_timestamps = n_valid_timestamps,
    valid_monitoring_hours = covered_seconds / 3600,
    coverage_prop = coverage_prop,
    coverage_percent = coverage_prop * 100,
    has_valid_75pct_coverage = coverage_prop >= valid_monitoring_coverage_threshold,
    stringsAsFactors = FALSE
  )
}

make_scaffold <- function() {
  expand.grid(
    timepoint = timepoint_levels,
    study_arm_overall = arm_levels,
    stringsAsFactors = FALSE
  )
}

write_scaffolded_csv <- function(data, path) {
  scaffold <- make_scaffold()
  out <- merge(scaffold, data, by = c("timepoint", "study_arm_overall"), all.x = TRUE)
  out <- out[order(match(out$timepoint, timepoint_levels), match(out$study_arm_overall, arm_levels)), ]
  readr::write_csv(out, path, na = "")
  invisible(path)
}

message("Reading cleaned PM2.5 inputs")
if (!file.exists(input_indoor_path)) stop("Missing input file: ", input_indoor_path, call. = FALSE)
if (!file.exists(input_ambient_path)) stop("Missing input file: ", input_ambient_path, call. = FALSE)
if (!file.exists(input_survey_household_path)) stop("Missing input file: ", input_survey_household_path, call. = FALSE)
if (!file.exists(input_survey_location_path)) stop("Missing input file: ", input_survey_location_path, call. = FALSE)
if (!file.exists(input_survey_hh_members_path)) stop("Missing input file: ", input_survey_hh_members_path, call. = FALSE)

indoor <- readRDS(input_indoor_path)
indoor_anomaly_retained <- NULL
ambient <- readRDS(input_ambient_path)
survey_household <- readRDS(input_survey_household_path)
survey_location <- readRDS(input_survey_location_path)
survey_hh_members <- readRDS(input_survey_hh_members_path)

check_required_columns(
  indoor,
  c(
    "timepoint", "study_arm_overall", "hh_id", "fcn_id", "hh_id_note",
    "raw_source_file", "PM_monitor", "dateTime", "pm25_ug_m3"
  ),
  "pm25_pats_refugee_indoor.rds"
)
if (!is.null(indoor_anomaly_retained)) {
  check_required_columns(
    indoor_anomaly_retained,
    c(
      "timepoint", "study_arm_overall", "hh_id", "fcn_id", "hh_id_note",
      "raw_source_file", "PM_monitor", "dateTime", "pm25_ug_m3"
    ),
    "pm25_pats_refugee_indoor_anomaly_retained_sensitivity.rds"
  )
}
check_required_columns(
  ambient,
  c(
    "timepoint", "study_arm_overall", "ambient_site_id", "note_clean",
    "raw_source_file", "dateTime", "pm25_ug_m3"
  ),
  "pm25_pats_refugee_ambient.rds"
)
check_required_columns(
  survey_household,
  c("fcn_id", "hh_id", "KEY", "timepoint", "study_arm_overall", "hh_size", "hh_per_structure", "hours_outside", "target_child_hours_outside", "respondent_sl"),
  "survey_refugee_household.rds"
)
check_required_columns(
  survey_location,
  c("PARENT_KEY", "location_number", "timepoint", "hours_inside", "hours_outside"),
  "survey_refugee_location.rds"
)
check_required_columns(
  survey_hh_members,
  c("PARENT_KEY", "mem_serial", "timepoint", "age_yrs", "hours_outside"),
  "survey_refugee_hh_members.rds"
)

prepare_indoor_pm <- function(data) {
  data %>%
    mutate(
      dateTime = as_clean_datetime(dateTime),
      timepoint = as_ordered_timepoint(timepoint),
      study_arm_overall = as.character(study_arm_overall),
      hh_id = as.character(hh_id),
      fcn_id = as.character(fcn_id),
      hh_id_note = as.character(hh_id_note),
      raw_source_file = as.character(raw_source_file),
      PM_monitor = as.character(PM_monitor),
      pm25_ug_m3 = as.numeric(pm25_ug_m3)
    )
}

indoor <- prepare_indoor_pm(indoor)
if (!is.null(indoor_anomaly_retained)) {
  indoor_anomaly_retained <- prepare_indoor_pm(indoor_anomaly_retained)
}

ambient <- ambient %>%
  mutate(
    dateTime = as_clean_datetime(dateTime),
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    ambient_site_id = as.character(ambient_site_id),
    note_clean = as.character(note_clean),
    raw_source_file = as.character(raw_source_file),
    PM_monitor = as.character(PM_monitor),
    pm25_ug_m3 = as.numeric(pm25_ug_m3)
  )

message("Auditing indoor PM2.5 data collection intervals and 24-hour monitoring coverage")
monitoring_window_keys <- c(
  "timepoint", "study_arm_overall", "hh_id", "fcn_id", "hh_id_note",
  "raw_source_file", "PM_monitor"
)

indoor_monitoring_valid <- indoor %>%
  filter(
    timepoint %in% timepoint_levels,
    study_arm_overall %in% arm_levels,
    !is.na(hh_id),
    !is.na(dateTime),
    !is.na(pm25_ug_m3),
    is.finite(pm25_ug_m3),
    pm25_ug_m3 > 0
  )

if (nrow(indoor_monitoring_valid) == 0) {
  stop("No valid indoor PM2.5 observations available for monitoring coverage audit.", call. = FALSE)
}

monitoring_window_index <- indoor_monitoring_valid %>%
  group_by(across(all_of(monitoring_window_keys))) %>%
  summarise(
    start_datetime = min(dateTime, na.rm = TRUE),
    end_datetime = max(dateTime, na.rm = TRUE),
    n_valid_observations = n(),
    n_unique_timestamps = n_distinct(dateTime),
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall, start_datetime, raw_source_file, PM_monitor) %>%
  mutate(
    monitoring_coverage_window_id = sprintf("coverage_window_%04d", row_number()),
    elapsed_hours_first_to_last = as.numeric(difftime(end_datetime, start_datetime, units = "hours"))
  )

monitoring_window_lookup <- monitoring_window_index %>%
  select(
    all_of(monitoring_window_keys),
    monitoring_coverage_window_id,
    deployment_start_datetime = start_datetime
  )

indoor_monitoring_timestamps <- indoor_monitoring_valid %>%
  inner_join(monitoring_window_lookup, by = monitoring_window_keys) %>%
  distinct(monitoring_coverage_window_id, dateTime, .keep_all = TRUE) %>%
  arrange(monitoring_coverage_window_id, dateTime) %>%
  group_by(monitoring_coverage_window_id) %>%
  mutate(
    next_datetime = lead(dateTime),
    next_pm25_ug_m3 = lead(pm25_ug_m3),
    interval_seconds = as.numeric(difftime(next_datetime, dateTime, units = "secs")),
    positive_interval_seconds = ifelse(
      !is.na(interval_seconds) & is.finite(interval_seconds) & interval_seconds > 0,
      round(interval_seconds, 3),
      NA_real_
    ),
    monitor_hour = lubridate::floor_date(dateTime, unit = "hour")
  ) %>%
  ungroup()

global_modal_interval_seconds <- mode_number(indoor_monitoring_timestamps$positive_interval_seconds)

monitoring_window_interval_diagnostics <- indoor_monitoring_timestamps %>%
  group_by(monitoring_coverage_window_id) %>%
  summarise(
    n_positive_intervals = sum(!is.na(positive_interval_seconds)),
    modal_interval_seconds = mode_number(positive_interval_seconds),
    median_interval_seconds = median_or_na(positive_interval_seconds),
    min_interval_seconds = min_or_na(positive_interval_seconds),
    max_interval_seconds = max_or_na(positive_interval_seconds),
    n_distinct_positive_intervals = n_distinct(positive_interval_seconds[!is.na(positive_interval_seconds)]),
    pct_positive_intervals_at_window_mode = pct_at_mode(positive_interval_seconds),
    .groups = "drop"
  ) %>%
  mutate(
    all_positive_intervals_same = ifelse(
      n_positive_intervals > 0,
      n_distinct_positive_intervals == 1L,
      NA
    ),
    usual_interval_seconds = ifelse(
      !is.na(modal_interval_seconds) & is.finite(modal_interval_seconds),
      modal_interval_seconds,
      global_modal_interval_seconds
    )
  )

monitoring_window_index <- monitoring_window_index %>%
  left_join(monitoring_window_interval_diagnostics, by = "monitoring_coverage_window_id")

long_interval_within_window_details <- indoor_monitoring_timestamps %>%
  filter(
    !is.na(positive_interval_seconds),
    positive_interval_seconds > long_monitoring_interval_threshold_seconds
  ) %>%
  mutate(
    gap_minutes = positive_interval_seconds / 60,
    gap_hours = positive_interval_seconds / 3600,
    expected_records_missed = pmax(
      0,
      floor(positive_interval_seconds / expected_monitoring_interval_seconds) - 1
    ),
    elapsed_hours_since_window_start = as.numeric(difftime(dateTime, deployment_start_datetime, units = "hours")),
    preceding_24h_period = pmax(
      1L,
      floor(elapsed_hours_since_window_start / monitoring_period_hours) + 1L
    ),
    gap_category = case_when(
      positive_interval_seconds <= 5 * 60 ~ "gt_60sec_to_5min",
      positive_interval_seconds <= 30 * 60 ~ "gt_5min_to_30min",
      positive_interval_seconds <= 2 * 3600 ~ "gt_30min_to_2h",
      TRUE ~ "gt_2h"
    ),
    crosses_study_timepoint = FALSE,
    crosses_raw_source_file = FALSE,
    crosses_pm_monitor = FALSE,
    gap_scope = "within_monitoring_window",
    gap_interpretation = "This interval is within one monitoring_coverage_window_id; it is not a gap between study timepoints, source files, or PM monitors in the coverage audit."
  ) %>%
  select(
    monitoring_coverage_window_id,
    all_of(monitoring_window_keys),
    dateTime,
    next_datetime,
    pm25_ug_m3,
    next_pm25_ug_m3,
    positive_interval_seconds,
    gap_minutes,
    gap_hours,
    expected_records_missed,
    preceding_24h_period,
    gap_category,
    gap_scope,
    gap_interpretation,
    crosses_study_timepoint,
    crosses_raw_source_file,
    crosses_pm_monitor
  ) %>%
  arrange(desc(positive_interval_seconds), timepoint, study_arm_overall, hh_id, dateTime)

within_window_interval_denominators <- indoor_monitoring_timestamps %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_household_windows = n_distinct(monitoring_coverage_window_id),
    n_positive_intervals = sum(!is.na(positive_interval_seconds)),
    .groups = "drop"
  )

within_window_long_interval_counts <- long_interval_within_window_details %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_long_intervals_gt_60sec = n(),
    n_household_windows_with_long_interval = n_distinct(monitoring_coverage_window_id),
    median_long_interval_seconds = median_or_na(positive_interval_seconds),
    p95_long_interval_seconds = safe_quantile(positive_interval_seconds, 0.95),
    max_long_interval_seconds = max_or_na(positive_interval_seconds),
    total_expected_records_missed = sum(expected_records_missed, na.rm = TRUE),
    n_long_intervals_gt_60sec_to_5min = sum(gap_category == "gt_60sec_to_5min", na.rm = TRUE),
    n_long_intervals_gt_5min_to_30min = sum(gap_category == "gt_5min_to_30min", na.rm = TRUE),
    n_long_intervals_gt_30min_to_2h = sum(gap_category == "gt_30min_to_2h", na.rm = TRUE),
    n_long_intervals_gt_2h = sum(gap_category == "gt_2h", na.rm = TRUE),
    .groups = "drop"
  )

long_interval_within_window_summary_by_arm <- within_window_interval_denominators %>%
  left_join(within_window_long_interval_counts, by = c("timepoint", "study_arm_overall")) %>%
  mutate(
    across(
      c(
        n_long_intervals_gt_60sec,
        n_household_windows_with_long_interval,
        total_expected_records_missed,
        n_long_intervals_gt_60sec_to_5min,
        n_long_intervals_gt_5min_to_30min,
        n_long_intervals_gt_30min_to_2h,
        n_long_intervals_gt_2h
      ),
      ~ dplyr::coalesce(.x, 0)
    ),
    pct_positive_intervals_gt_60sec = ifelse(
      n_positive_intervals > 0,
      n_long_intervals_gt_60sec / n_positive_intervals * 100,
      NA_real_
    ),
    interval_scope = "within_monitoring_window",
    interval_interpretation = "These long intervals occur inside the same timepoint/source-file/PM-monitor monitoring window."
  ) %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  )

long_interval_within_window_summary_overall <- data.frame(
  timepoint = "all",
  study_arm_overall = "all",
  n_household_windows = n_distinct(indoor_monitoring_timestamps$monitoring_coverage_window_id),
  n_positive_intervals = sum(!is.na(indoor_monitoring_timestamps$positive_interval_seconds)),
  n_long_intervals_gt_60sec = nrow(long_interval_within_window_details),
  n_household_windows_with_long_interval = n_distinct(long_interval_within_window_details$monitoring_coverage_window_id),
  median_long_interval_seconds = median_or_na(long_interval_within_window_details$positive_interval_seconds),
  p95_long_interval_seconds = safe_quantile(long_interval_within_window_details$positive_interval_seconds, 0.95),
  max_long_interval_seconds = max_or_na(long_interval_within_window_details$positive_interval_seconds),
  total_expected_records_missed = sum(long_interval_within_window_details$expected_records_missed, na.rm = TRUE),
  n_long_intervals_gt_60sec_to_5min = sum(long_interval_within_window_details$gap_category == "gt_60sec_to_5min", na.rm = TRUE),
  n_long_intervals_gt_5min_to_30min = sum(long_interval_within_window_details$gap_category == "gt_5min_to_30min", na.rm = TRUE),
  n_long_intervals_gt_30min_to_2h = sum(long_interval_within_window_details$gap_category == "gt_30min_to_2h", na.rm = TRUE),
  n_long_intervals_gt_2h = sum(long_interval_within_window_details$gap_category == "gt_2h", na.rm = TRUE),
  stringsAsFactors = FALSE
) %>%
  mutate(
    pct_positive_intervals_gt_60sec = ifelse(
      n_positive_intervals > 0,
      n_long_intervals_gt_60sec / n_positive_intervals * 100,
      NA_real_
    ),
    interval_scope = "within_monitoring_window",
    interval_interpretation = "These long intervals occur inside the same timepoint/source-file/PM-monitor monitoring window."
  )

long_interval_within_window_summary <- bind_rows(
  long_interval_within_window_summary_overall,
  long_interval_within_window_summary_by_arm
)

household_naive_intervals <- indoor_monitoring_valid %>%
  mutate(
    household_monitoring_identity = dplyr::coalesce(
      ifelse(!is.na(fcn_id) & nzchar(fcn_id), fcn_id, NA_character_),
      ifelse(!is.na(hh_id) & nzchar(hh_id), hh_id, NA_character_)
    ),
    timepoint_chr = as.character(timepoint),
    study_arm_overall_chr = as.character(study_arm_overall),
    raw_source_file_chr = as.character(raw_source_file),
    PM_monitor_chr = as.character(PM_monitor)
  ) %>%
  filter(!is.na(household_monitoring_identity)) %>%
  distinct(
    household_monitoring_identity,
    dateTime,
    timepoint_chr,
    study_arm_overall_chr,
    hh_id,
    fcn_id,
    raw_source_file_chr,
    PM_monitor_chr,
    .keep_all = TRUE
  ) %>%
  arrange(household_monitoring_identity, dateTime) %>%
  group_by(household_monitoring_identity) %>%
  mutate(
    next_datetime_household = lead(dateTime),
    next_timepoint = lead(timepoint_chr),
    next_study_arm_overall = lead(study_arm_overall_chr),
    next_hh_id = lead(hh_id),
    next_fcn_id = lead(fcn_id),
    next_raw_source_file = lead(raw_source_file_chr),
    next_PM_monitor = lead(PM_monitor_chr),
    naive_household_interval_seconds = as.numeric(difftime(next_datetime_household, dateTime, units = "secs"))
  ) %>%
  ungroup()

naive_household_long_interval_details <- household_naive_intervals %>%
  filter(
    !is.na(naive_household_interval_seconds),
    is.finite(naive_household_interval_seconds),
    naive_household_interval_seconds > long_monitoring_interval_threshold_seconds
  ) %>%
  mutate(
    gap_minutes = naive_household_interval_seconds / 60,
    gap_hours = naive_household_interval_seconds / 3600,
    gap_days = naive_household_interval_seconds / 86400,
    crosses_study_timepoint = !is.na(next_timepoint) & timepoint_chr != next_timepoint,
    crosses_study_arm = !is.na(next_study_arm_overall) & study_arm_overall_chr != next_study_arm_overall,
    crosses_hh_id = !is.na(next_hh_id) & hh_id != next_hh_id,
    crosses_fcn_id = !is.na(next_fcn_id) & fcn_id != next_fcn_id,
    crosses_raw_source_file = !is.na(next_raw_source_file) & raw_source_file_chr != next_raw_source_file,
    crosses_pm_monitor = !is.na(next_PM_monitor) & PM_monitor_chr != next_PM_monitor,
    gap_source_class = case_when(
      crosses_study_timepoint ~ "between_study_timepoints",
      crosses_raw_source_file | crosses_pm_monitor ~ "between_source_files_or_monitors_same_timepoint",
      TRUE ~ "within_same_household_sequence"
    ),
    answers_timepoint_gap_hypothesis = ifelse(
      crosses_study_timepoint,
      "yes_this_naive_household_gap_crosses_study_timepoints",
      "no_this_naive_household_gap_does_not_cross_study_timepoints"
    )
  ) %>%
  select(
    household_monitoring_identity,
    hh_id,
    fcn_id,
    timepoint = timepoint_chr,
    study_arm_overall = study_arm_overall_chr,
    raw_source_file = raw_source_file_chr,
    PM_monitor = PM_monitor_chr,
    dateTime,
    next_datetime_household,
    next_timepoint,
    next_study_arm_overall,
    next_hh_id,
    next_fcn_id,
    next_raw_source_file,
    next_PM_monitor,
    naive_household_interval_seconds,
    gap_minutes,
    gap_hours,
    gap_days,
    crosses_study_timepoint,
    crosses_study_arm,
    crosses_hh_id,
    crosses_fcn_id,
    crosses_raw_source_file,
    crosses_pm_monitor,
    gap_source_class,
    answers_timepoint_gap_hypothesis
  ) %>%
  arrange(desc(naive_household_interval_seconds), household_monitoring_identity, dateTime)

naive_household_interval_denominators <- household_naive_intervals %>%
  group_by(timepoint_chr, study_arm_overall_chr) %>%
  summarise(
    n_naive_household_positive_intervals = sum(
      !is.na(naive_household_interval_seconds) &
        is.finite(naive_household_interval_seconds) &
        naive_household_interval_seconds > 0
    ),
    .groups = "drop"
  )

naive_household_long_interval_counts <- naive_household_long_interval_details %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_naive_household_long_intervals_gt_60sec = n(),
    n_cross_study_timepoint = sum(crosses_study_timepoint, na.rm = TRUE),
    n_cross_raw_source_file = sum(crosses_raw_source_file, na.rm = TRUE),
    n_cross_pm_monitor = sum(crosses_pm_monitor, na.rm = TRUE),
    n_within_same_timepoint_source_file_monitor = sum(
      !crosses_study_timepoint & !crosses_raw_source_file & !crosses_pm_monitor,
      na.rm = TRUE
    ),
    median_naive_long_interval_seconds = median_or_na(naive_household_interval_seconds),
    max_naive_long_interval_seconds = max_or_na(naive_household_interval_seconds),
    .groups = "drop"
  )

naive_household_long_interval_summary_by_arm <- naive_household_interval_denominators %>%
  left_join(
    naive_household_long_interval_counts,
    by = c("timepoint_chr" = "timepoint", "study_arm_overall_chr" = "study_arm_overall")
  ) %>%
  mutate(
    across(
      c(
        n_naive_household_long_intervals_gt_60sec,
        n_cross_study_timepoint,
        n_cross_raw_source_file,
        n_cross_pm_monitor,
        n_within_same_timepoint_source_file_monitor
      ),
      ~ dplyr::coalesce(.x, 0)
    ),
    pct_naive_long_intervals_cross_study_timepoint = ifelse(
      n_naive_household_long_intervals_gt_60sec > 0,
      n_cross_study_timepoint / n_naive_household_long_intervals_gt_60sec * 100,
      NA_real_
    ),
    timepoint = timepoint_chr,
    study_arm_overall = study_arm_overall_chr,
    interval_scope = "naive_household_sequence",
    interval_interpretation = "This diagnostic intentionally sorts all valid rows by household identity only; it tests whether cross-timepoint deployment gaps would appear if deployment boundaries were ignored."
  ) %>%
  select(-timepoint_chr, -study_arm_overall_chr)

naive_household_long_interval_summary_overall <- data.frame(
  timepoint = "all",
  study_arm_overall = "all",
  n_naive_household_positive_intervals = sum(
    !is.na(household_naive_intervals$naive_household_interval_seconds) &
      is.finite(household_naive_intervals$naive_household_interval_seconds) &
      household_naive_intervals$naive_household_interval_seconds > 0
  ),
  n_naive_household_long_intervals_gt_60sec = nrow(naive_household_long_interval_details),
  n_cross_study_timepoint = sum(naive_household_long_interval_details$crosses_study_timepoint, na.rm = TRUE),
  n_cross_raw_source_file = sum(naive_household_long_interval_details$crosses_raw_source_file, na.rm = TRUE),
  n_cross_pm_monitor = sum(naive_household_long_interval_details$crosses_pm_monitor, na.rm = TRUE),
  n_within_same_timepoint_source_file_monitor = sum(
    !naive_household_long_interval_details$crosses_study_timepoint &
      !naive_household_long_interval_details$crosses_raw_source_file &
      !naive_household_long_interval_details$crosses_pm_monitor,
    na.rm = TRUE
  ),
  median_naive_long_interval_seconds = median_or_na(naive_household_long_interval_details$naive_household_interval_seconds),
  max_naive_long_interval_seconds = max_or_na(naive_household_long_interval_details$naive_household_interval_seconds),
  stringsAsFactors = FALSE
) %>%
  mutate(
    pct_naive_long_intervals_cross_study_timepoint = ifelse(
      n_naive_household_long_intervals_gt_60sec > 0,
      n_cross_study_timepoint / n_naive_household_long_intervals_gt_60sec * 100,
      NA_real_
    ),
    interval_scope = "naive_household_sequence",
    interval_interpretation = "This diagnostic intentionally sorts all valid rows by household identity only; it tests whether cross-timepoint deployment gaps would appear if deployment boundaries were ignored."
  )

naive_household_long_interval_summary <- bind_rows(
  naive_household_long_interval_summary_overall,
  naive_household_long_interval_summary_by_arm
)

long_interval_source_assessment <- data.frame(
  question = c(
    "Are intervals longer than 60 seconds present in the actual coverage audit?",
    "Can the actual coverage audit create long intervals by joining different study timepoints for the same household?",
    "Would a naive household-only interval calculation create cross-timepoint gaps?"
  ),
  answer = c(
    ifelse(nrow(long_interval_within_window_details) > 0, "yes", "no"),
    "no; monitoring_coverage_window_id includes timepoint, raw_source_file, and PM_monitor before intervals are calculated",
    ifelse(
      sum(naive_household_long_interval_details$crosses_study_timepoint, na.rm = TRUE) > 0,
      "yes; see naive household-only diagnostic tables",
      "no cross-timepoint gaps found even under household-only sorting"
    )
  ),
  value = c(
    as.character(nrow(long_interval_within_window_details)),
    "0 by construction",
    as.character(sum(naive_household_long_interval_details$crosses_study_timepoint, na.rm = TRUE))
  ),
  stringsAsFactors = FALSE
)

readr::write_csv(
  long_interval_within_window_summary,
  file.path(table_dir, "table_pm25_long_interval_within_window_summary.csv"),
  na = ""
)
readr::write_csv(
  naive_household_long_interval_summary,
  file.path(table_dir, "table_pm25_naive_household_long_interval_source_summary.csv"),
  na = ""
)
readr::write_csv(
  long_interval_source_assessment,
  file.path(table_dir, "table_pm25_long_interval_source_assessment.csv"),
  na = ""
)
readr::write_csv(
  long_interval_within_window_details,
  file.path(restricted_table_dir, "table_pm25_long_interval_within_window_details_internal.csv"),
  na = ""
)
readr::write_csv(
  naive_household_long_interval_details,
  file.path(restricted_table_dir, "table_pm25_naive_household_long_interval_details_internal.csv"),
  na = ""
)

monitoring_interval_by_household_hour <- indoor_monitoring_timestamps %>%
  group_by(monitoring_coverage_window_id, monitor_hour) %>%
  summarise(
    first_timestamp = min(dateTime, na.rm = TRUE),
    last_timestamp = max(dateTime, na.rm = TRUE),
    n_valid_timestamps = n(),
    n_positive_intervals = sum(!is.na(positive_interval_seconds)),
    modal_interval_seconds = mode_number(positive_interval_seconds),
    median_interval_seconds = median_or_na(positive_interval_seconds),
    min_interval_seconds = min_or_na(positive_interval_seconds),
    max_interval_seconds = max_or_na(positive_interval_seconds),
    n_distinct_positive_intervals = n_distinct(positive_interval_seconds[!is.na(positive_interval_seconds)]),
    pct_positive_intervals_at_hour_mode = pct_at_mode(positive_interval_seconds),
    .groups = "drop"
  ) %>%
  mutate(
    all_positive_intervals_same_within_hour = ifelse(
      n_positive_intervals > 0,
      n_distinct_positive_intervals == 1L,
      NA
    )
  ) %>%
  left_join(
    monitoring_window_index %>% select(monitoring_coverage_window_id, all_of(monitoring_window_keys)),
    by = "monitoring_coverage_window_id"
  ) %>%
  select(monitoring_coverage_window_id, all_of(monitoring_window_keys), monitor_hour, everything()) %>%
  arrange(timepoint, study_arm_overall, hh_id, monitor_hour)

interval_rows_for_summary <- indoor_monitoring_timestamps

interval_summary_by_arm <- interval_rows_for_summary %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_household_windows = n_distinct(monitoring_coverage_window_id),
    n_household_hour_groups = n_distinct(paste(monitoring_coverage_window_id, monitor_hour)),
    n_positive_intervals = sum(!is.na(positive_interval_seconds)),
    modal_interval_seconds = mode_number(positive_interval_seconds),
    median_interval_seconds = median_or_na(positive_interval_seconds),
    min_interval_seconds = min_or_na(positive_interval_seconds),
    max_interval_seconds = max_or_na(positive_interval_seconds),
    n_distinct_positive_intervals = n_distinct(positive_interval_seconds[!is.na(positive_interval_seconds)]),
    pct_positive_intervals_at_modal = pct_at_mode(positive_interval_seconds),
    all_positive_intervals_same = {
      x <- positive_interval_seconds[!is.na(positive_interval_seconds)]
      if (length(x) == 0) NA else length(unique(x)) == 1L
    },
    .groups = "drop"
  )

hour_interval_summary_by_arm <- monitoring_interval_by_household_hour %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_household_hour_groups_with_positive_intervals = sum(n_positive_intervals > 0, na.rm = TRUE),
    n_household_hour_groups_constant_interval = sum(all_positive_intervals_same_within_hour %in% TRUE, na.rm = TRUE),
    pct_household_hour_groups_constant_interval = ifelse(
      n_household_hour_groups_with_positive_intervals > 0,
      n_household_hour_groups_constant_interval / n_household_hour_groups_with_positive_intervals * 100,
      NA_real_
    ),
    .groups = "drop"
  )

interval_summary_by_arm <- interval_summary_by_arm %>%
  left_join(hour_interval_summary_by_arm, by = c("timepoint", "study_arm_overall"))

interval_summary_overall <- data.frame(
  timepoint = "all",
  study_arm_overall = "all",
  n_household_windows = n_distinct(interval_rows_for_summary$monitoring_coverage_window_id),
  n_household_hour_groups = n_distinct(paste(interval_rows_for_summary$monitoring_coverage_window_id, interval_rows_for_summary$monitor_hour)),
  n_positive_intervals = sum(!is.na(interval_rows_for_summary$positive_interval_seconds)),
  modal_interval_seconds = global_modal_interval_seconds,
  median_interval_seconds = median_or_na(interval_rows_for_summary$positive_interval_seconds),
  min_interval_seconds = min_or_na(interval_rows_for_summary$positive_interval_seconds),
  max_interval_seconds = max_or_na(interval_rows_for_summary$positive_interval_seconds),
  n_distinct_positive_intervals = n_distinct(interval_rows_for_summary$positive_interval_seconds[!is.na(interval_rows_for_summary$positive_interval_seconds)]),
  pct_positive_intervals_at_modal = pct_at_mode(interval_rows_for_summary$positive_interval_seconds),
  all_positive_intervals_same = {
    x <- interval_rows_for_summary$positive_interval_seconds[!is.na(interval_rows_for_summary$positive_interval_seconds)]
    if (length(x) == 0) NA else length(unique(x)) == 1L
  },
  n_household_hour_groups_with_positive_intervals = sum(monitoring_interval_by_household_hour$n_positive_intervals > 0, na.rm = TRUE),
  n_household_hour_groups_constant_interval = sum(monitoring_interval_by_household_hour$all_positive_intervals_same_within_hour %in% TRUE, na.rm = TRUE),
  pct_household_hour_groups_constant_interval = ifelse(
    sum(monitoring_interval_by_household_hour$n_positive_intervals > 0, na.rm = TRUE) > 0,
    sum(monitoring_interval_by_household_hour$all_positive_intervals_same_within_hour %in% TRUE, na.rm = TRUE) /
      sum(monitoring_interval_by_household_hour$n_positive_intervals > 0, na.rm = TRUE) * 100,
    NA_real_
  ),
  stringsAsFactors = FALSE
)

monitoring_interval_summary <- bind_rows(
  interval_summary_overall,
  interval_summary_by_arm %>% mutate(timepoint = as.character(timepoint), study_arm_overall = as.character(study_arm_overall))
) %>%
  mutate(
    interval_question = "Are positive inter-record data collection intervals the same for all hours in all households?",
    interval_answer = ifelse(
      all_positive_intervals_same,
      "yes_all_positive_intervals_identical",
      "no_positive_intervals_vary"
    )
  )

readr::write_csv(
  monitoring_interval_summary,
  file.path(table_dir, "table_pm25_monitoring_interval_summary.csv"),
  na = ""
)
readr::write_csv(
  monitoring_interval_by_household_hour,
  file.path(restricted_table_dir, "table_pm25_monitoring_interval_by_household_hour_internal.csv"),
  na = ""
)

monitoring_period_coverage <- bind_rows(lapply(seq_len(nrow(monitoring_window_index)), function(i) {
  window_id <- monitoring_window_index$monitoring_coverage_window_id[[i]]
  timestamp_rows <- indoor_monitoring_timestamps %>%
    filter(monitoring_coverage_window_id == window_id)
  out <- bind_rows(lapply(seq_len(monitoring_period_count), function(period_number) {
    summarize_monitoring_period_coverage(
      timestamp_rows = timestamp_rows,
      start_datetime = monitoring_window_index$start_datetime[[i]],
      usual_interval_seconds = monitoring_window_index$usual_interval_seconds[[i]],
      period_number = period_number
    )
  }))
  out$monitoring_coverage_window_id <- window_id
  out[, c("monitoring_coverage_window_id", setdiff(names(out), "monitoring_coverage_window_id"))]
}))

monitoring_period_1 <- monitoring_period_coverage %>%
  filter(period_number == 1L) %>%
  transmute(
    monitoring_coverage_window_id,
    period_1_start_datetime = period_start_datetime,
    period_1_end_datetime = period_end_datetime,
    n_valid_timestamps_24h_1 = n_valid_timestamps,
    valid_monitoring_hours_24h_1 = valid_monitoring_hours,
    coverage_prop_24h_1 = coverage_prop,
    coverage_percent_24h_1 = coverage_percent,
    has_valid_75pct_24h_1 = has_valid_75pct_coverage
  )

monitoring_period_2 <- monitoring_period_coverage %>%
  filter(period_number == 2L) %>%
  transmute(
    monitoring_coverage_window_id,
    period_2_start_datetime = period_start_datetime,
    period_2_end_datetime = period_end_datetime,
    n_valid_timestamps_24h_2 = n_valid_timestamps,
    valid_monitoring_hours_24h_2 = valid_monitoring_hours,
    coverage_prop_24h_2 = coverage_prop,
    coverage_percent_24h_2 = coverage_percent,
    has_valid_75pct_24h_2 = has_valid_75pct_coverage
  )

household_monitoring_coverage <- monitoring_window_index %>%
  left_join(monitoring_period_1, by = "monitoring_coverage_window_id") %>%
  left_join(monitoring_period_2, by = "monitoring_coverage_window_id") %>%
  mutate(
    valid_monitoring_hours_full_48h = pmin(
      monitoring_total_hours,
      dplyr::coalesce(valid_monitoring_hours_24h_1, 0) + dplyr::coalesce(valid_monitoring_hours_24h_2, 0)
    ),
    coverage_prop_full_48h = valid_monitoring_hours_full_48h / monitoring_total_hours,
    coverage_percent_full_48h = coverage_prop_full_48h * 100,
    average_coverage_percent_24h_periods = (
      dplyr::coalesce(coverage_percent_24h_1, 0) + dplyr::coalesce(coverage_percent_24h_2, 0)
    ) / monitoring_period_count,
    has_valid_75pct_24h_1 = dplyr::coalesce(has_valid_75pct_24h_1, FALSE),
    has_valid_75pct_24h_2 = dplyr::coalesce(has_valid_75pct_24h_2, FALSE),
    n_valid_24h_periods_75pct = as.integer(has_valid_75pct_24h_1) + as.integer(has_valid_75pct_24h_2),
    valid_monitoring_definition = paste0(
      "Valid monitoring uses positive finite indoor PM2.5 rows. Coverage is interval-overlap time using each deployment's modal positive interval, ",
      "with gaps capped at that usual interval. A 24-hour period is valid at >= ",
      valid_monitoring_coverage_threshold * 100,
      "% coverage."
    )
  ) %>%
  arrange(timepoint, study_arm_overall, start_datetime, hh_id)

monitoring_coverage_counts <- household_monitoring_coverage %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_households = n_distinct(hh_id),
    n_household_windows = n(),
    n_monitor_files = n_distinct(raw_source_file),
    n_households_valid_first_24h_75pct = n_distinct(hh_id[has_valid_75pct_24h_1]),
    n_households_valid_second_24h_75pct = n_distinct(hh_id[has_valid_75pct_24h_2]),
    n_household_windows_valid_first_24h_75pct = sum(has_valid_75pct_24h_1, na.rm = TRUE),
    n_household_windows_valid_second_24h_75pct = sum(has_valid_75pct_24h_2, na.rm = TRUE),
    n_households_with_0_valid_24h_periods_75pct = n_distinct(hh_id[n_valid_24h_periods_75pct == 0L]),
    n_households_with_1_valid_24h_period_75pct = n_distinct(hh_id[n_valid_24h_periods_75pct == 1L]),
    n_households_with_2_valid_24h_periods_75pct = n_distinct(hh_id[n_valid_24h_periods_75pct == 2L]),
    n_household_windows_with_0_valid_24h_periods_75pct = sum(n_valid_24h_periods_75pct == 0L, na.rm = TRUE),
    n_household_windows_with_1_valid_24h_period_75pct = sum(n_valid_24h_periods_75pct == 1L, na.rm = TRUE),
    n_household_windows_with_2_valid_24h_periods_75pct = sum(n_valid_24h_periods_75pct == 2L, na.rm = TRUE),
    median_coverage_percent_24h_1 = median_or_na(coverage_percent_24h_1),
    median_coverage_percent_24h_2 = median_or_na(coverage_percent_24h_2),
    mean_coverage_percent_24h_1 = mean_or_na(coverage_percent_24h_1),
    mean_coverage_percent_24h_2 = mean_or_na(coverage_percent_24h_2),
    median_average_coverage_percent_24h_periods = median_or_na(average_coverage_percent_24h_periods),
    median_coverage_percent_full_48h = median_or_na(coverage_percent_full_48h),
    valid_period_coverage_threshold_percent = valid_monitoring_coverage_threshold * 100,
    count_unit = "one row per household monitoring file/window; household counts use distinct hh_id within arm/timepoint",
    .groups = "drop"
  ) %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  )

readr::write_csv(
  household_monitoring_coverage,
  file.path(restricted_table_dir, "table_pm25_household_monitoring_coverage_internal.csv"),
  na = ""
)
write_scaffolded_csv(
  monitoring_coverage_counts,
  file.path(table_dir, "table_pm25_monitoring_coverage_counts.csv")
)

message("Summarizing indoor PM2.5 to household monitoring windows")
indoor_windows <- indoor %>%
  filter(
    timepoint %in% timepoint_levels,
    study_arm_overall %in% arm_levels,
    !is.na(hh_id),
    !is.na(dateTime),
    !is.na(pm25_ug_m3),
    is.finite(pm25_ug_m3),
    pm25_ug_m3 > 0
  ) %>%
  group_by(timepoint, study_arm_overall, hh_id, fcn_id, hh_id_note, raw_source_file, PM_monitor) %>%
  summarise(
    start_datetime = min(dateTime, na.rm = TRUE),
    end_datetime = max(dateTime, na.rm = TRUE),
    midpoint_datetime = mean_datetime(dateTime),
    n_obs_indoor = n(),
    n_hours_indoor = n_distinct(lubridate::floor_date(dateTime, unit = "hour")),
    duration_hours = as.numeric(difftime(max(dateTime, na.rm = TRUE), min(dateTime, na.rm = TRUE), units = "hours")),
    indoor_mean_pm = mean(pm25_ug_m3, na.rm = TRUE),
    indoor_gmean_pm = geo_mean(pm25_ug_m3),
    indoor_median_pm = median(pm25_ug_m3, na.rm = TRUE),
    indoor_p05_pm = safe_quantile(pm25_ug_m3, 0.05),
    indoor_p25_pm = safe_quantile(pm25_ug_m3, 0.25),
    indoor_p75_pm = safe_quantile(pm25_ug_m3, 0.75),
    indoor_p95_pm = safe_quantile(pm25_ug_m3, 0.95),
    indoor_p99_pm = safe_quantile(pm25_ug_m3, 0.99),
    indoor_max_pm = max(pm25_ug_m3, na.rm = TRUE),
    pct_obs_gt_35 = mean(pm25_ug_m3 > 35, na.rm = TRUE) * 100,
    pct_obs_gt_75 = mean(pm25_ug_m3 > 75, na.rm = TRUE) * 100,
    pct_obs_gt_150 = mean(pm25_ug_m3 > 150, na.rm = TRUE) * 100,
    pct_obs_gt_400 = mean(pm25_ug_m3 > 400, na.rm = TRUE) * 100,
    pct_obs_gt_1000 = mean(pm25_ug_m3 >= 1000, na.rm = TRUE) * 100,
    pct_obs_gt_5000 = mean(pm25_ug_m3 >= 5000, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    household_window_id = sprintf("pm_window_%04d", row_number()),
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = arm_levels)
  ) %>%
  arrange(timepoint, study_arm_overall, start_datetime, household_window_id)

ambient_clean <- ambient %>%
  filter(
    !is.na(dateTime),
    !is.na(pm25_ug_m3),
    is.finite(pm25_ug_m3),
    pm25_ug_m3 > 0
  ) %>%
  mutate(
    ambient_hour = lubridate::floor_date(dateTime, unit = "hour")
  )

message("Creating HAPIN-style PM2.5 exposure graphics and tables")

hapin_metric_metadata <- data.frame(
  metric_name = c("raw_indoor_pm25", "ambient_adjusted_indoor_excess_pm25"),
  metric_label = c(
    "Raw indoor PM2.5",
    sprintf(
      "Ambient-adjusted indoor-excess PM2.5: indoor minus %.2f x concurrent ambient",
      default_material_infiltration_factor
    )
  ),
  metric_scale = c("log_positive", "linear_zero"),
  stringsAsFactors = FALSE
)

hapin_identifier_cols <- c(
  "hh_id", "fcn_id", "hh_id_note", "raw_source_file", "PM_monitor",
  "monitoring_coverage_window_id", "start_datetime", "end_datetime",
  "period_start_datetime", "period_end_datetime"
)

hapin_deidentify <- function(data) {
  data %>% select(-any_of(hapin_identifier_cols))
}

hapin_weighted_mean <- function(value, weight) {
  value <- as_number(value)
  weight <- as_number(weight)
  ok <- !is.na(value) & is.finite(value) & !is.na(weight) & is.finite(weight) & weight > 0
  if (!any(ok)) return(NA_real_)
  sum(value[ok] * weight[ok]) / sum(weight[ok])
}

hapin_safe_metric_ratio <- function(numerator, denominator) {
  ifelse(
    !is.na(numerator) & is.finite(numerator) & numerator > 0 &
      !is.na(denominator) & is.finite(denominator) & denominator > 0,
    numerator / denominator,
    NA_real_
  )
}

hapin_window_id_lookup <- monitoring_window_index %>%
  transmute(
    monitoring_coverage_window_id,
    hapin_window_id = sprintf("hapin_window_%04d", row_number())
  )

hapin_period_index <- monitoring_period_coverage %>%
  left_join(
    monitoring_window_index %>%
      select(
        monitoring_coverage_window_id,
        all_of(monitoring_window_keys),
        start_datetime,
        end_datetime,
        usual_interval_seconds
      ),
    by = "monitoring_coverage_window_id"
  ) %>%
  left_join(hapin_window_id_lookup, by = "monitoring_coverage_window_id") %>%
  mutate(
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = arm_levels),
    period_label = factor(
      paste0("Day ", period_number),
      levels = paste0("Day ", seq_len(monitoring_period_count))
    )
  )

hapin_period_pm_raw <- indoor_monitoring_timestamps %>%
  select(monitoring_coverage_window_id, dateTime, pm25_ug_m3) %>%
  inner_join(
    hapin_period_index %>%
      select(
        monitoring_coverage_window_id,
        period_number,
        period_start_datetime,
        period_end_datetime
      ),
    by = "monitoring_coverage_window_id",
    relationship = "many-to-many"
  ) %>%
  filter(dateTime >= period_start_datetime, dateTime < period_end_datetime) %>%
  group_by(monitoring_coverage_window_id, period_number) %>%
  summarise(
    n_pm_rows_24h = n(),
    indoor_mean_pm25_24h = mean(pm25_ug_m3, na.rm = TRUE),
    indoor_gmean_pm25_24h = geo_mean(pm25_ug_m3),
    indoor_median_pm25_24h = median(pm25_ug_m3, na.rm = TRUE),
    indoor_p10_pm25_24h = safe_quantile(pm25_ug_m3, 0.10),
    indoor_p25_pm25_24h = safe_quantile(pm25_ug_m3, 0.25),
    indoor_p75_pm25_24h = safe_quantile(pm25_ug_m3, 0.75),
    indoor_p90_pm25_24h = safe_quantile(pm25_ug_m3, 0.90),
    indoor_p95_pm25_24h = safe_quantile(pm25_ug_m3, 0.95),
    indoor_max_pm25_24h = max(pm25_ug_m3, na.rm = TRUE),
    pct_minute_rows_gt_15 = mean(pm25_ug_m3 > 15, na.rm = TRUE) * 100,
    pct_minute_rows_gt_35 = mean(pm25_ug_m3 > 35, na.rm = TRUE) * 100,
    pct_minute_rows_gt_75 = mean(pm25_ug_m3 > 75, na.rm = TRUE) * 100,
    pct_minute_rows_gt_150 = mean(pm25_ug_m3 > 150, na.rm = TRUE) * 100,
    pct_minute_rows_gt_400 = mean(pm25_ug_m3 > 400, na.rm = TRUE) * 100,
    pct_minute_rows_gt_1000 = mean(pm25_ug_m3 > 1000, na.rm = TRUE) * 100,
    .groups = "drop"
  )

hapin_summarize_period_ambient <- function(i) {
  period_rows <- ambient_clean %>%
    filter(
      dateTime >= hapin_period_index$period_start_datetime[[i]],
      dateTime < hapin_period_index$period_end_datetime[[i]]
    )

  data.frame(
    monitoring_coverage_window_id = hapin_period_index$monitoring_coverage_window_id[[i]],
    period_number = hapin_period_index$period_number[[i]],
    n_ambient_rows_24h = nrow(period_rows),
    n_ambient_hours_24h = if (nrow(period_rows) == 0) 0L else n_distinct(period_rows$ambient_hour),
    ambient_mean_pm25_24h = if (nrow(period_rows) == 0) NA_real_ else mean(period_rows$pm25_ug_m3, na.rm = TRUE),
    ambient_gmean_pm25_24h = if (nrow(period_rows) == 0) NA_real_ else geo_mean(period_rows$pm25_ug_m3),
    ambient_median_pm25_24h = if (nrow(period_rows) == 0) NA_real_ else median(period_rows$pm25_ug_m3, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

hapin_period_ambient <- bind_rows(lapply(seq_len(nrow(hapin_period_index)), hapin_summarize_period_ambient))

hapin_24h_period_wide <- hapin_period_index %>%
  left_join(hapin_period_pm_raw, by = c("monitoring_coverage_window_id", "period_number")) %>%
  left_join(hapin_period_ambient, by = c("monitoring_coverage_window_id", "period_number")) %>%
  mutate(
    n_pm_rows_24h = dplyr::coalesce(n_pm_rows_24h, 0L),
    n_ambient_rows_24h = dplyr::coalesce(n_ambient_rows_24h, 0L),
    n_ambient_hours_24h = dplyr::coalesce(n_ambient_hours_24h, 0L),
    ambient_adjusted_mean_pm25_24h = indoor_mean_pm25_24h -
      default_material_infiltration_factor * ambient_mean_pm25_24h,
    has_valid_raw_24h = has_valid_75pct_coverage &
      !is.na(indoor_mean_pm25_24h) & is.finite(indoor_mean_pm25_24h),
    has_valid_ambient_adjusted_24h = has_valid_75pct_coverage &
      n_ambient_rows_24h > 0 &
      !is.na(ambient_adjusted_mean_pm25_24h) &
      is.finite(ambient_adjusted_mean_pm25_24h)
  )

hapin_24h_period_summary_internal <- bind_rows(
  hapin_24h_period_wide %>%
    transmute(
      timepoint,
      study_arm_overall,
      hh_id,
      fcn_id,
      hh_id_note,
      raw_source_file,
      PM_monitor,
      monitoring_coverage_window_id,
      hapin_window_id,
      start_datetime,
      end_datetime,
      period_number,
      period_label,
      period_start_datetime,
      period_end_datetime,
      valid_monitoring_hours_24h = valid_monitoring_hours,
      coverage_prop_24h = coverage_prop,
      coverage_percent_24h = coverage_percent,
      has_valid_75pct_coverage,
      metric_name = "raw_indoor_pm25",
      metric_label = hapin_metric_metadata$metric_label[hapin_metric_metadata$metric_name == "raw_indoor_pm25"],
      metric_scale = "log_positive",
      metric_valid_24h = has_valid_raw_24h,
      metric_value_24h = indoor_mean_pm25_24h,
      n_pm_rows_24h,
      n_ambient_rows_24h,
      n_ambient_hours_24h,
      indoor_mean_pm25_24h,
      indoor_gmean_pm25_24h,
      indoor_median_pm25_24h,
      indoor_p10_pm25_24h,
      indoor_p25_pm25_24h,
      indoor_p75_pm25_24h,
      indoor_p90_pm25_24h,
      indoor_p95_pm25_24h,
      indoor_max_pm25_24h,
      ambient_mean_pm25_24h,
      ambient_gmean_pm25_24h,
      ambient_median_pm25_24h,
      ambient_adjusted_mean_pm25_24h,
      infiltration_factor_for_adjustment = default_material_infiltration_factor,
      pct_minute_rows_gt_15,
      pct_minute_rows_gt_35,
      pct_minute_rows_gt_75,
      pct_minute_rows_gt_150,
      pct_minute_rows_gt_400,
      pct_minute_rows_gt_1000
    ),
  hapin_24h_period_wide %>%
    transmute(
      timepoint,
      study_arm_overall,
      hh_id,
      fcn_id,
      hh_id_note,
      raw_source_file,
      PM_monitor,
      monitoring_coverage_window_id,
      hapin_window_id,
      start_datetime,
      end_datetime,
      period_number,
      period_label,
      period_start_datetime,
      period_end_datetime,
      valid_monitoring_hours_24h = valid_monitoring_hours,
      coverage_prop_24h = coverage_prop,
      coverage_percent_24h = coverage_percent,
      has_valid_75pct_coverage,
      metric_name = "ambient_adjusted_indoor_excess_pm25",
      metric_label = hapin_metric_metadata$metric_label[hapin_metric_metadata$metric_name == "ambient_adjusted_indoor_excess_pm25"],
      metric_scale = "linear_zero",
      metric_valid_24h = has_valid_ambient_adjusted_24h,
      metric_value_24h = ambient_adjusted_mean_pm25_24h,
      n_pm_rows_24h,
      n_ambient_rows_24h,
      n_ambient_hours_24h,
      indoor_mean_pm25_24h,
      indoor_gmean_pm25_24h,
      indoor_median_pm25_24h,
      indoor_p10_pm25_24h,
      indoor_p25_pm25_24h,
      indoor_p75_pm25_24h,
      indoor_p90_pm25_24h,
      indoor_p95_pm25_24h,
      indoor_max_pm25_24h,
      ambient_mean_pm25_24h,
      ambient_gmean_pm25_24h,
      ambient_median_pm25_24h,
      ambient_adjusted_mean_pm25_24h,
      infiltration_factor_for_adjustment = default_material_infiltration_factor,
      pct_minute_rows_gt_15 = NA_real_,
      pct_minute_rows_gt_35 = NA_real_,
      pct_minute_rows_gt_75 = NA_real_,
      pct_minute_rows_gt_150 = NA_real_,
      pct_minute_rows_gt_400 = NA_real_,
      pct_minute_rows_gt_1000 = NA_real_
    )
) %>%
  arrange(metric_name, timepoint, study_arm_overall, hapin_window_id, period_number)

readr::write_csv(
  hapin_24h_period_summary_internal,
  file.path(restricted_table_dir, "table_pm25_hapin_24h_period_summary_internal.csv"),
  na = ""
)
readr::write_csv(
  hapin_deidentify(hapin_24h_period_summary_internal),
  file.path(table_dir, "table_pm25_hapin_24h_period_summary_deidentified.csv"),
  na = ""
)

hapin_48h_household_summary_internal <- hapin_24h_period_summary_internal %>%
  filter(metric_valid_24h) %>%
  group_by(
    metric_name,
    metric_label,
    metric_scale,
    timepoint,
    study_arm_overall,
    hh_id,
    fcn_id,
    hh_id_note,
    raw_source_file,
    PM_monitor,
    monitoring_coverage_window_id,
    hapin_window_id,
    start_datetime,
    end_datetime
  ) %>%
  summarise(
    n_valid_24h_periods = n_distinct(period_number),
    valid_24h_periods = paste(sort(unique(period_number)), collapse = ";"),
    valid_monitoring_hours_48h = sum(valid_monitoring_hours_24h, na.rm = TRUE),
    coverage_prop_valid_periods_48h = min(1, valid_monitoring_hours_48h / monitoring_total_hours),
    coverage_percent_valid_periods_48h = coverage_prop_valid_periods_48h * 100,
    metric_value_48h_time_weighted = hapin_weighted_mean(metric_value_24h, valid_monitoring_hours_24h),
    mean_valid_24h_metric_value = mean(metric_value_24h, na.rm = TRUE),
    geometric_mean_valid_24h_metric_value = if (dplyr::first(metric_name) == "raw_indoor_pm25") geo_mean(metric_value_24h) else NA_real_,
    median_valid_24h_metric_value = median(metric_value_24h, na.rm = TRUE),
    p25_valid_24h_metric_value = safe_quantile(metric_value_24h, 0.25),
    p75_valid_24h_metric_value = safe_quantile(metric_value_24h, 0.75),
    p90_valid_24h_metric_value = safe_quantile(metric_value_24h, 0.90),
    p95_valid_24h_metric_value = safe_quantile(metric_value_24h, 0.95),
    min_valid_24h_metric_value = min(metric_value_24h, na.rm = TRUE),
    max_valid_24h_metric_value = max(metric_value_24h, na.rm = TRUE),
    pct_valid_24h_periods_ge_0 = mean(metric_value_24h >= 0, na.rm = TRUE) * 100,
    pct_valid_24h_periods_ge_15 = mean(metric_value_24h >= 15, na.rm = TRUE) * 100,
    pct_valid_24h_periods_ge_35 = mean(metric_value_24h >= 35, na.rm = TRUE) * 100,
    pct_valid_24h_periods_ge_75 = mean(metric_value_24h >= 75, na.rm = TRUE) * 100,
    pct_valid_24h_periods_ge_150 = mean(metric_value_24h >= 150, na.rm = TRUE) * 100,
    pct_valid_24h_periods_ge_400 = mean(metric_value_24h >= 400, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(metric_name, timepoint, study_arm_overall, hapin_window_id)

readr::write_csv(
  hapin_48h_household_summary_internal,
  file.path(restricted_table_dir, "table_pm25_hapin_48h_household_summary_internal.csv"),
  na = ""
)
readr::write_csv(
  hapin_deidentify(hapin_48h_household_summary_internal),
  file.path(table_dir, "table_pm25_hapin_48h_household_summary_deidentified.csv"),
  na = ""
)

hapin_distribution_summary_24h <- hapin_24h_period_summary_internal %>%
  filter(metric_valid_24h) %>%
  group_by(metric_name, metric_label, metric_scale, timepoint, study_arm_overall) %>%
  summarise(
    summary_level = "valid_24h_period",
    n_observations = n(),
    n_households = n_distinct(hh_id),
    n_household_windows = n_distinct(hapin_window_id),
    mean_pm25 = mean(metric_value_24h, na.rm = TRUE),
    geometric_mean_pm25 = if (dplyr::first(metric_name) == "raw_indoor_pm25") geo_mean(metric_value_24h) else NA_real_,
    median_pm25 = median(metric_value_24h, na.rm = TRUE),
    p10_pm25 = safe_quantile(metric_value_24h, 0.10),
    p25_pm25 = safe_quantile(metric_value_24h, 0.25),
    p75_pm25 = safe_quantile(metric_value_24h, 0.75),
    p90_pm25 = safe_quantile(metric_value_24h, 0.90),
    p95_pm25 = safe_quantile(metric_value_24h, 0.95),
    min_pm25 = min(metric_value_24h, na.rm = TRUE),
    max_pm25 = max(metric_value_24h, na.rm = TRUE),
    .groups = "drop"
  )

hapin_distribution_summary_48h <- hapin_48h_household_summary_internal %>%
  filter(!is.na(metric_value_48h_time_weighted), is.finite(metric_value_48h_time_weighted)) %>%
  group_by(metric_name, metric_label, metric_scale, timepoint, study_arm_overall) %>%
  summarise(
    summary_level = "household_48h_time_weighted_mean",
    n_observations = n(),
    n_households = n_distinct(hh_id),
    n_household_windows = n_distinct(hapin_window_id),
    mean_pm25 = mean(metric_value_48h_time_weighted, na.rm = TRUE),
    geometric_mean_pm25 = if (dplyr::first(metric_name) == "raw_indoor_pm25") geo_mean(metric_value_48h_time_weighted) else NA_real_,
    median_pm25 = median(metric_value_48h_time_weighted, na.rm = TRUE),
    p10_pm25 = safe_quantile(metric_value_48h_time_weighted, 0.10),
    p25_pm25 = safe_quantile(metric_value_48h_time_weighted, 0.25),
    p75_pm25 = safe_quantile(metric_value_48h_time_weighted, 0.75),
    p90_pm25 = safe_quantile(metric_value_48h_time_weighted, 0.90),
    p95_pm25 = safe_quantile(metric_value_48h_time_weighted, 0.95),
    min_pm25 = min(metric_value_48h_time_weighted, na.rm = TRUE),
    max_pm25 = max(metric_value_48h_time_weighted, na.rm = TRUE),
    .groups = "drop"
  )

hapin_distribution_summary <- bind_rows(
  hapin_distribution_summary_24h,
  hapin_distribution_summary_48h
) %>%
  arrange(summary_level, metric_name, timepoint, study_arm_overall)

readr::write_csv(
  hapin_distribution_summary,
  file.path(table_dir, "table_pm25_hapin_distribution_summary.csv"),
  na = ""
)

hapin_day1 <- hapin_24h_period_summary_internal %>%
  filter(period_number == 1L, metric_valid_24h) %>%
  transmute(
    metric_name,
    metric_label,
    metric_scale,
    timepoint,
    study_arm_overall,
    hh_id,
    fcn_id,
    hh_id_note,
    raw_source_file,
    PM_monitor,
    monitoring_coverage_window_id,
    hapin_window_id,
    metric_value_24h_day1 = metric_value_24h,
    coverage_percent_24h_day1 = coverage_percent_24h,
    valid_monitoring_hours_24h_day1 = valid_monitoring_hours_24h
  )

hapin_day2 <- hapin_24h_period_summary_internal %>%
  filter(period_number == 2L, metric_valid_24h) %>%
  transmute(
    metric_name,
    metric_label,
    metric_scale,
    timepoint,
    study_arm_overall,
    hh_id,
    fcn_id,
    hh_id_note,
    raw_source_file,
    PM_monitor,
    monitoring_coverage_window_id,
    hapin_window_id,
    metric_value_24h_day2 = metric_value_24h,
    coverage_percent_24h_day2 = coverage_percent_24h,
    valid_monitoring_hours_24h_day2 = valid_monitoring_hours_24h
  )

hapin_day1_day2_agreement_internal <- full_join(
  hapin_day1,
  hapin_day2,
  by = c(
    "metric_name", "metric_label", "metric_scale", "timepoint", "study_arm_overall",
    "hh_id", "fcn_id", "hh_id_note", "raw_source_file", "PM_monitor",
    "monitoring_coverage_window_id", "hapin_window_id"
  )
) %>%
  mutate(
    has_both_valid_24h_periods = !is.na(metric_value_24h_day1) & !is.na(metric_value_24h_day2),
    day2_minus_day1 = metric_value_24h_day2 - metric_value_24h_day1,
    day2_divided_by_day1 = hapin_safe_metric_ratio(metric_value_24h_day2, metric_value_24h_day1),
    absolute_day_difference = abs(day2_minus_day1)
  ) %>%
  arrange(metric_name, timepoint, study_arm_overall, hapin_window_id)

readr::write_csv(
  hapin_deidentify(hapin_day1_day2_agreement_internal),
  file.path(table_dir, "table_pm25_hapin_day1_day2_agreement.csv"),
  na = ""
)

hapin_valid_period_index <- hapin_period_index %>%
  filter(has_valid_75pct_coverage) %>%
  select(
    monitoring_coverage_window_id,
    hapin_window_id,
    all_of(monitoring_window_keys),
    period_number,
    period_label,
    period_start_datetime,
    period_end_datetime
  )

hapin_ambient_hourly <- ambient_clean %>%
  group_by(ambient_hour) %>%
  summarise(
    n_ambient_rows_hour = n(),
    ambient_mean_pm25_hour = mean(pm25_ug_m3, na.rm = TRUE),
    .groups = "drop"
  )

hapin_household_period_hour_wide <- indoor_monitoring_timestamps %>%
  select(monitoring_coverage_window_id, dateTime, pm25_ug_m3) %>%
  inner_join(
    hapin_valid_period_index,
    by = "monitoring_coverage_window_id",
    relationship = "many-to-many"
  ) %>%
  filter(dateTime >= period_start_datetime, dateTime < period_end_datetime) %>%
  mutate(
    monitor_hour = lubridate::floor_date(dateTime, unit = "hour"),
    hour_of_day = lubridate::hour(lubridate::with_tz(monitor_hour, tzone = hapin_hour_time_zone))
  ) %>%
  group_by(
    timepoint,
    study_arm_overall,
    hh_id,
    fcn_id,
    raw_source_file,
    PM_monitor,
    monitoring_coverage_window_id,
    hapin_window_id,
    period_number,
    period_label,
    monitor_hour,
    hour_of_day
  ) %>%
  summarise(
    n_pm_rows_hour = n(),
    indoor_mean_pm25_hour = mean(pm25_ug_m3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(hapin_ambient_hourly, by = c("monitor_hour" = "ambient_hour")) %>%
  mutate(
    n_ambient_rows_hour = dplyr::coalesce(n_ambient_rows_hour, 0L),
    ambient_adjusted_mean_pm25_hour = indoor_mean_pm25_hour -
      default_material_infiltration_factor * ambient_mean_pm25_hour
  )

hapin_household_period_hour_long <- bind_rows(
  hapin_household_period_hour_wide %>%
    transmute(
      metric_name = "raw_indoor_pm25",
      metric_label = hapin_metric_metadata$metric_label[hapin_metric_metadata$metric_name == "raw_indoor_pm25"],
      metric_scale = "log_positive",
      timepoint,
      study_arm_overall,
      hh_id,
      fcn_id,
      hapin_window_id,
      period_number,
      period_label,
      monitor_hour,
      hour_of_day,
      metric_value_hour = indoor_mean_pm25_hour,
      n_pm_rows_hour,
      n_ambient_rows_hour
    ),
  hapin_household_period_hour_wide %>%
    transmute(
      metric_name = "ambient_adjusted_indoor_excess_pm25",
      metric_label = hapin_metric_metadata$metric_label[hapin_metric_metadata$metric_name == "ambient_adjusted_indoor_excess_pm25"],
      metric_scale = "linear_zero",
      timepoint,
      study_arm_overall,
      hh_id,
      fcn_id,
      hapin_window_id,
      period_number,
      period_label,
      monitor_hour,
      hour_of_day,
      metric_value_hour = ambient_adjusted_mean_pm25_hour,
      n_pm_rows_hour,
      n_ambient_rows_hour
    )
) %>%
  filter(!is.na(metric_value_hour), is.finite(metric_value_hour))

hapin_hour_of_day_summary <- hapin_household_period_hour_long %>%
  group_by(metric_name, metric_label, metric_scale, timepoint, study_arm_overall, hour_of_day) %>%
  summarise(
    n_household_period_hours = n(),
    n_households = n_distinct(hh_id),
    n_household_windows = n_distinct(hapin_window_id),
    n_valid_24h_periods = n_distinct(paste(hapin_window_id, period_number)),
    mean_pm25 = mean(metric_value_hour, na.rm = TRUE),
    median_pm25 = median(metric_value_hour, na.rm = TRUE),
    p10_pm25 = safe_quantile(metric_value_hour, 0.10),
    p25_pm25 = safe_quantile(metric_value_hour, 0.25),
    p75_pm25 = safe_quantile(metric_value_hour, 0.75),
    p90_pm25 = safe_quantile(metric_value_hour, 0.90),
    .groups = "drop"
  ) %>%
  filter(n_households >= 3) %>%
  arrange(metric_name, timepoint, study_arm_overall, hour_of_day)

readr::write_csv(
  hapin_hour_of_day_summary,
  file.path(table_dir, "table_pm25_hapin_hour_of_day_summary.csv"),
  na = ""
)

hapin_tail_exceedance_summary <- bind_rows(lapply(hapin_pm25_exceedance_thresholds, function(threshold_i) {
  hapin_48h_household_summary_internal %>%
    filter(!is.na(metric_value_48h_time_weighted), is.finite(metric_value_48h_time_weighted)) %>%
    group_by(metric_name, metric_label, metric_scale, timepoint, study_arm_overall) %>%
    summarise(
      threshold_pm25 = threshold_i,
      n_household_windows = n(),
      n_household_windows_at_or_above_threshold = sum(metric_value_48h_time_weighted >= threshold_i, na.rm = TRUE),
      percent_household_windows_at_or_above_threshold =
        100 * n_household_windows_at_or_above_threshold / n_household_windows,
      summary_unit = "household_48h_time_weighted_mean",
      .groups = "drop"
    )
})) %>%
  arrange(metric_name, timepoint, study_arm_overall, threshold_pm25)

readr::write_csv(
  hapin_tail_exceedance_summary,
  file.path(table_dir, "table_pm25_hapin_tail_exceedance_summary.csv"),
  na = ""
)

hapin_generated_figures <- character()

hapin_metric_file_token <- function(metric_name) {
  gsub("[^a-z0-9]+", "_", tolower(metric_name))
}

hapin_save_plot <- function(plot, filename, width = 10, height = 6) {
  ggplot2::ggsave(
    file.path(figure_dir, filename),
    plot,
    width = width,
    height = height,
    dpi = 300
  )
  hapin_generated_figures <<- c(hapin_generated_figures, filename)
  invisible(filename)
}

hapin_arm_colors <- c(comparison = "#4E79A7", intervention = "#D55E00")

hapin_add_y_scale <- function(plot, metric_name) {
  if (identical(metric_name, "raw_indoor_pm25")) {
    plot +
      geom_hline(
        yintercept = hapin_pm25_reference_lines,
        color = "grey55",
        linetype = "dashed",
        linewidth = 0.25
      ) +
      scale_y_log10()
  } else {
    plot + geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35)
  }
}

for (metric_i in hapin_metric_metadata$metric_name) {
  metric_label_i <- hapin_metric_metadata$metric_label[hapin_metric_metadata$metric_name == metric_i]
  metric_scale_i <- hapin_metric_metadata$metric_scale[hapin_metric_metadata$metric_name == metric_i]
  metric_token_i <- hapin_metric_file_token(metric_i)
  log_positive_i <- identical(metric_scale_i, "log_positive")

  plot_48h_data <- hapin_48h_household_summary_internal %>%
    filter(
      metric_name == metric_i,
      !is.na(metric_value_48h_time_weighted),
      is.finite(metric_value_48h_time_weighted)
    )
  if (log_positive_i) {
    plot_48h_data <- plot_48h_data %>% filter(metric_value_48h_time_weighted > 0)
  }

  if (nrow(plot_48h_data) > 0) {
    if (identical(metric_i, "raw_indoor_pm25")) {
      plot_48h_indoor_data <- plot_48h_data %>%
        transmute(
          timepoint,
          study_arm_overall,
          exposure_group = case_when(
            study_arm_overall == "comparison" ~ "comparison_indoor",
            study_arm_overall == "intervention" ~ "intervention_indoor",
            TRUE ~ NA_character_
          ),
          metric_value_48h_time_weighted,
          source_unit = "household_indoor_48h_time_weighted_mean"
        )

      plot_48h_outdoor_data <- hapin_24h_period_wide %>%
        filter(
          has_valid_75pct_coverage,
          n_ambient_rows_24h > 0,
          !is.na(ambient_mean_pm25_24h),
          is.finite(ambient_mean_pm25_24h),
          ambient_mean_pm25_24h > 0
        ) %>%
        group_by(timepoint, hapin_window_id) %>%
        summarise(
          metric_value_48h_time_weighted = hapin_weighted_mean(
            ambient_mean_pm25_24h,
            valid_monitoring_hours
          ),
          source_unit = "concurrent_outdoor_48h_time_weighted_mean",
          .groups = "drop"
        ) %>%
        filter(
          !is.na(metric_value_48h_time_weighted),
          is.finite(metric_value_48h_time_weighted),
          metric_value_48h_time_weighted > 0
        ) %>%
        mutate(
          study_arm_overall = "outdoor_pm25",
          exposure_group = "outdoor_pm25"
        ) %>%
        select(
          timepoint,
          study_arm_overall,
          exposure_group,
          metric_value_48h_time_weighted,
          source_unit
        )

      plot_48h_display_data <- bind_rows(plot_48h_indoor_data, plot_48h_outdoor_data) %>%
        filter(!is.na(exposure_group)) %>%
        mutate(
          exposure_group = factor(
            exposure_group,
            levels = c("comparison_indoor", "intervention_indoor", "outdoor_pm25"),
            labels = c("Comparison indoor", "Intervention indoor", "Outdoor PM2.5")
          )
        )

      fig_48h <- ggplot(
        plot_48h_display_data,
        aes(
          x = exposure_group,
          y = metric_value_48h_time_weighted,
          color = exposure_group
        )
      ) +
        geom_hline(
          yintercept = c(25, 50, 75),
          color = "grey45",
          linetype = "dotted",
          linewidth = 0.35
        ) +
        geom_boxplot(width = 0.46, outlier.shape = NA, alpha = 0.12) +
        geom_jitter(width = 0.08, height = 0, alpha = 0.45, size = 1.6) +
        stat_summary(
          fun = mean,
          geom = "point",
          shape = 23,
          fill = "white",
          color = "black",
          size = 2.8
        ) +
        facet_wrap(~ timepoint, nrow = 1) +
        scale_y_log10(
          breaks = c(10, 25, 50, 75, 100, 250, 500, 1000),
          labels = function(x) format(x, trim = TRUE, scientific = FALSE)
        ) +
        scale_color_manual(
          values = c(
            "Comparison indoor" = "#4E79A7",
            "Intervention indoor" = "#D55E00",
            "Outdoor PM2.5" = "#3A3A3A"
          ),
          drop = FALSE
        ) +
        labs(
          x = NULL,
          y = "48-hour time-weighted PM2.5 (ug/m3, log scale)",
          color = NULL,
          title = paste0(metric_label_i, " and concurrent outdoor PM2.5"),
          subtitle = paste(
            "Points are household monitoring windows; boxplots show median and IQR;",
            "diamonds show arithmetic means; dotted lines mark 25, 50, and 75 ug/m3"
          )
        ) +
        theme_bw(base_size = 11) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 20, hjust = 1),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey92", color = "grey75")
        )
    } else {
      fig_48h <- ggplot(
        plot_48h_data,
        aes(
          x = study_arm_overall,
          y = metric_value_48h_time_weighted,
          color = study_arm_overall
        )
      ) +
        geom_boxplot(width = 0.46, outlier.shape = NA, alpha = 0.12) +
        geom_jitter(width = 0.08, height = 0, alpha = 0.45, size = 1.6) +
        stat_summary(
          fun = mean,
          geom = "point",
          shape = 23,
          fill = "white",
          color = "black",
          size = 2.8
        ) +
        facet_wrap(~ timepoint, nrow = 1) +
        scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
        labs(
          x = NULL,
          y = "48-hour time-weighted PM2.5 (ug/m3)",
          color = "Study arm",
          title = paste0(metric_label_i, " by study arm and timepoint"),
          subtitle = "Points are household monitoring windows; boxplots show median and IQR; diamonds show arithmetic means"
        ) +
        theme_bw(base_size = 11) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey92", color = "grey75")
        )
      fig_48h <- hapin_add_y_scale(fig_48h, metric_i)
    }

    hapin_save_plot(
      fig_48h,
      paste0("fig_pm25_hapin_48h_household_distribution_", metric_token_i, ".png"),
      width = 10,
      height = 5.6
    )
  }

  plot_period_data <- hapin_24h_period_summary_internal %>%
    filter(metric_name == metric_i, metric_valid_24h)
  if (log_positive_i) {
    plot_period_data <- plot_period_data %>% filter(metric_value_24h > 0)
  }

  if (nrow(plot_period_data) > 0) {
    fig_period <- ggplot(
      plot_period_data,
      aes(
        x = period_label,
        y = metric_value_24h,
        color = study_arm_overall
      )
    ) +
      geom_boxplot(
        aes(group = interaction(period_label, study_arm_overall)),
        position = position_dodge(width = 0.72),
        width = 0.52,
        outlier.shape = NA,
        alpha = 0.12
      ) +
      geom_jitter(
        position = position_jitterdodge(jitter.width = 0.10, dodge.width = 0.72),
        alpha = 0.35,
        size = 1.35
      ) +
      facet_wrap(~ timepoint, nrow = 1) +
      scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
      labs(
        x = NULL,
        y = "Valid 24-hour PM2.5 (ug/m3)",
        color = "Study arm",
        title = paste0(metric_label_i, " in each valid 24-hour monitoring period"),
        subtitle = "Each point is one valid household 24-hour period; boxplots show median and IQR"
      ) +
      theme_bw(base_size = 11) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", color = "grey75")
      )
    fig_period <- hapin_add_y_scale(fig_period, metric_i)
    hapin_save_plot(
      fig_period,
      paste0("fig_pm25_hapin_24h_period_distribution_", metric_token_i, ".png"),
      width = 10,
      height = 5.6
    )
  }

  plot_agreement_data <- hapin_day1_day2_agreement_internal %>%
    filter(
      metric_name == metric_i,
      has_both_valid_24h_periods,
      !is.na(metric_value_24h_day1),
      !is.na(metric_value_24h_day2),
      is.finite(metric_value_24h_day1),
      is.finite(metric_value_24h_day2)
    )
  if (log_positive_i) {
    plot_agreement_data <- plot_agreement_data %>%
      filter(metric_value_24h_day1 > 0, metric_value_24h_day2 > 0)
  }

  if (nrow(plot_agreement_data) > 0) {
    fig_agreement <- ggplot(
      plot_agreement_data,
      aes(
        x = metric_value_24h_day1,
        y = metric_value_24h_day2,
        color = study_arm_overall
      )
    ) +
      geom_abline(slope = 1, intercept = 0, color = "grey35", linetype = "dashed", linewidth = 0.45) +
      geom_point(alpha = 0.58, size = 1.8) +
      facet_wrap(~ timepoint, nrow = 1) +
      scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
      labs(
        x = "Day 1 valid 24-hour PM2.5 (ug/m3)",
        y = "Day 2 valid 24-hour PM2.5 (ug/m3)",
        color = "Study arm",
        title = paste0(metric_label_i, ": Day 1 versus Day 2 agreement"),
        subtitle = "Dashed line is equality between the two valid 24-hour periods"
      ) +
      theme_bw(base_size = 11) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", color = "grey75")
      )
    if (log_positive_i) {
      fig_agreement <- fig_agreement + scale_x_log10() + scale_y_log10()
    } else {
      fig_agreement <- fig_agreement +
        geom_hline(yintercept = 0, color = "grey60", linewidth = 0.30) +
        geom_vline(xintercept = 0, color = "grey60", linewidth = 0.30)
    }
    hapin_save_plot(
      fig_agreement,
      paste0("fig_pm25_hapin_day1_day2_agreement_", metric_token_i, ".png"),
      width = 10,
      height = 5.6
    )
  }

  plot_hour_data <- hapin_hour_of_day_summary %>% filter(metric_name == metric_i)
  if (log_positive_i) {
    plot_hour_data <- plot_hour_data %>% filter(p10_pm25 > 0, p90_pm25 > 0, median_pm25 > 0)
  }

  if (nrow(plot_hour_data) > 0) {
    fig_hour <- ggplot(
      plot_hour_data,
      aes(
        x = hour_of_day,
        y = median_pm25,
        color = study_arm_overall,
        fill = study_arm_overall
      )
    ) +
      geom_ribbon(aes(ymin = p10_pm25, ymax = p90_pm25), alpha = 0.10, color = NA) +
      geom_ribbon(aes(ymin = p25_pm25, ymax = p75_pm25), alpha = 0.22, color = NA) +
      geom_line(linewidth = 0.85) +
      facet_wrap(~ timepoint, nrow = 1) +
      scale_x_continuous(breaks = seq(0, 23, by = 3)) +
      scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
      scale_fill_manual(values = hapin_arm_colors, drop = FALSE) +
      labs(
        x = paste0("Hour of day (", hapin_hour_time_zone, ")"),
        y = "Household-period-hour PM2.5 (ug/m3)",
        color = "Study arm",
        fill = "Study arm",
        title = paste0(metric_label_i, " by hour of day"),
        subtitle = "Line is median; darker band is IQR; lighter band is 10th to 90th percentile"
      ) +
      theme_bw(base_size = 11) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", color = "grey75")
      )
    fig_hour <- hapin_add_y_scale(fig_hour, metric_i)
    hapin_save_plot(
      fig_hour,
      paste0("fig_pm25_hapin_hour_of_day_", metric_token_i, ".png"),
      width = 10,
      height = 5.6
    )
  }

  plot_tail_data <- hapin_48h_household_summary_internal %>%
    filter(
      metric_name == metric_i,
      !is.na(metric_value_48h_time_weighted),
      is.finite(metric_value_48h_time_weighted)
    )
  if (log_positive_i) {
    plot_tail_data <- plot_tail_data %>% filter(metric_value_48h_time_weighted > 0)
  }

  if (nrow(plot_tail_data) > 0) {
    fig_tail <- ggplot(
      plot_tail_data,
      aes(x = metric_value_48h_time_weighted, color = study_arm_overall)
    ) +
      stat_ecdf(aes(y = after_stat((1 - y) * 100)), linewidth = 0.85) +
      facet_wrap(~ timepoint, nrow = 1) +
      scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
      scale_y_continuous(limits = c(0, 100)) +
      labs(
        x = "48-hour time-weighted PM2.5 (ug/m3)",
        y = "Household windows at or above concentration (%)",
        color = "Study arm",
        title = paste0(metric_label_i, " upper-tail distribution"),
        subtitle = "Complementary empirical distribution of household 48-hour summaries"
      ) +
      theme_bw(base_size = 11) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", color = "grey75")
      )
    if (log_positive_i) {
      fig_tail <- fig_tail +
        geom_vline(
          xintercept = hapin_pm25_reference_lines,
          color = "grey55",
          linetype = "dashed",
          linewidth = 0.25
        ) +
        scale_x_log10()
    } else {
      fig_tail <- fig_tail + geom_vline(xintercept = 0, color = "grey35", linewidth = 0.35)
    }
    hapin_save_plot(
      fig_tail,
      paste0("fig_pm25_hapin_tail_exceedance_", metric_token_i, ".png"),
      width = 10,
      height = 5.6
    )
  }
}

hapin_adjusted_metric_name <- "ambient_adjusted_indoor_excess_pm25"
hapin_adjusted_metric_label <- hapin_metric_metadata$metric_label[
  hapin_metric_metadata$metric_name == hapin_adjusted_metric_name
]
hapin_adjusted_metric_token <- hapin_metric_file_token(hapin_adjusted_metric_name)
hapin_adjusted_log_note <- paste(
  "Positive adjusted values only are shown because log scales cannot display",
  "zero or negative ambient-adjusted indoor-excess PM2.5 values."
)

plot_48h_adjusted_log <- hapin_48h_household_summary_internal %>%
  filter(
    metric_name == hapin_adjusted_metric_name,
    !is.na(metric_value_48h_time_weighted),
    is.finite(metric_value_48h_time_weighted),
    metric_value_48h_time_weighted > 0
  )

if (nrow(plot_48h_adjusted_log) > 0) {
  fig_48h_adjusted_log <- ggplot(
    plot_48h_adjusted_log,
    aes(
      x = study_arm_overall,
      y = metric_value_48h_time_weighted,
      color = study_arm_overall
    )
  ) +
    geom_boxplot(width = 0.46, outlier.shape = NA, alpha = 0.12) +
    geom_jitter(width = 0.08, height = 0, alpha = 0.45, size = 1.6) +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 23,
      fill = "white",
      color = "black",
      size = 2.8
    ) +
    facet_wrap(~ timepoint, nrow = 1) +
    scale_y_log10() +
    scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
    labs(
      x = NULL,
      y = "Positive 48-hour time-weighted PM2.5 (ug/m3, log scale)",
      color = "Study arm",
      title = paste0(hapin_adjusted_metric_label, " by study arm and timepoint"),
      subtitle = paste(
        "Positive adjusted household windows only; boxplots show median and IQR;",
        "diamonds show arithmetic means"
      )
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = "grey75")
    )

  hapin_save_plot(
    fig_48h_adjusted_log,
    paste0(
      "fig_pm25_hapin_48h_household_distribution_",
      hapin_adjusted_metric_token,
      "_positive_log_y.png"
    ),
    width = 10,
    height = 5.6
  )
}

plot_period_adjusted_log <- hapin_24h_period_summary_internal %>%
  filter(
    metric_name == hapin_adjusted_metric_name,
    metric_valid_24h,
    !is.na(metric_value_24h),
    is.finite(metric_value_24h),
    metric_value_24h > 0
  )

if (nrow(plot_period_adjusted_log) > 0) {
  fig_period_adjusted_log <- ggplot(
    plot_period_adjusted_log,
    aes(
      x = period_label,
      y = metric_value_24h,
      color = study_arm_overall
    )
  ) +
    geom_boxplot(
      aes(group = interaction(period_label, study_arm_overall)),
      position = position_dodge(width = 0.72),
      width = 0.52,
      outlier.shape = NA,
      alpha = 0.12
    ) +
    geom_jitter(
      position = position_jitterdodge(jitter.width = 0.10, dodge.width = 0.72),
      alpha = 0.35,
      size = 1.35
    ) +
    facet_wrap(~ timepoint, nrow = 1) +
    scale_y_log10() +
    scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
    labs(
      x = NULL,
      y = "Positive valid 24-hour PM2.5 (ug/m3, log scale)",
      color = "Study arm",
      title = paste0(hapin_adjusted_metric_label, " in each valid 24-hour monitoring period"),
      subtitle = "Positive adjusted 24-hour periods only; boxplots show median and IQR"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = "grey75")
    )

  hapin_save_plot(
    fig_period_adjusted_log,
    paste0(
      "fig_pm25_hapin_24h_period_distribution_",
      hapin_adjusted_metric_token,
      "_positive_log_y.png"
    ),
    width = 10,
    height = 5.6
  )
}

plot_agreement_adjusted_log <- hapin_day1_day2_agreement_internal %>%
  filter(
    metric_name == hapin_adjusted_metric_name,
    has_both_valid_24h_periods,
    !is.na(metric_value_24h_day1),
    !is.na(metric_value_24h_day2),
    is.finite(metric_value_24h_day1),
    is.finite(metric_value_24h_day2),
    metric_value_24h_day1 > 0,
    metric_value_24h_day2 > 0
  )

if (nrow(plot_agreement_adjusted_log) > 0) {
  fig_agreement_adjusted_log <- ggplot(
    plot_agreement_adjusted_log,
    aes(
      x = metric_value_24h_day1,
      y = metric_value_24h_day2,
      color = study_arm_overall
    )
  ) +
    geom_abline(slope = 1, intercept = 0, color = "grey35", linetype = "dashed", linewidth = 0.45) +
    geom_point(alpha = 0.58, size = 1.8) +
    facet_wrap(~ timepoint, nrow = 1) +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
    labs(
      x = "Positive Day 1 valid 24-hour PM2.5 (ug/m3, log scale)",
      y = "Positive Day 2 valid 24-hour PM2.5 (ug/m3, log scale)",
      color = "Study arm",
      title = paste0(hapin_adjusted_metric_label, ": Day 1 versus Day 2 agreement"),
      subtitle = "Positive adjusted paired 24-hour periods only; dashed line is equality"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = "grey75")
    )

  hapin_save_plot(
    fig_agreement_adjusted_log,
    paste0(
      "fig_pm25_hapin_day1_day2_agreement_",
      hapin_adjusted_metric_token,
      "_positive_log_xy.png"
    ),
    width = 10,
    height = 5.6
  )
}

plot_hour_adjusted_log <- hapin_hour_of_day_summary %>%
  filter(
    metric_name == hapin_adjusted_metric_name,
    p10_pm25 > 0,
    p25_pm25 > 0,
    median_pm25 > 0,
    p75_pm25 > 0,
    p90_pm25 > 0
  )

if (nrow(plot_hour_adjusted_log) > 0) {
  fig_hour_adjusted_log <- ggplot(
    plot_hour_adjusted_log,
    aes(
      x = hour_of_day,
      y = median_pm25,
      color = study_arm_overall,
      fill = study_arm_overall
    )
  ) +
    geom_ribbon(aes(ymin = p10_pm25, ymax = p90_pm25), alpha = 0.10, color = NA) +
    geom_ribbon(aes(ymin = p25_pm25, ymax = p75_pm25), alpha = 0.22, color = NA) +
    geom_line(linewidth = 0.85) +
    facet_wrap(~ timepoint, nrow = 1) +
    scale_x_continuous(breaks = seq(0, 23, by = 3)) +
    scale_y_log10() +
    scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
    scale_fill_manual(values = hapin_arm_colors, drop = FALSE) +
    labs(
      x = paste0("Hour of day (", hapin_hour_time_zone, ")"),
      y = "Positive household-period-hour PM2.5 (ug/m3, log scale)",
      color = "Study arm",
      fill = "Study arm",
      title = paste0(hapin_adjusted_metric_label, " by hour of day"),
      subtitle = paste(
        "Positive adjusted percentile bands only; line is median;",
        "darker band is IQR; lighter band is 10th to 90th percentile"
      )
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = "grey75")
    )

  hapin_save_plot(
    fig_hour_adjusted_log,
    paste0(
      "fig_pm25_hapin_hour_of_day_",
      hapin_adjusted_metric_token,
      "_positive_log_y.png"
    ),
    width = 10,
    height = 5.6
  )
}

plot_tail_adjusted_log <- hapin_48h_household_summary_internal %>%
  filter(
    metric_name == hapin_adjusted_metric_name,
    !is.na(metric_value_48h_time_weighted),
    is.finite(metric_value_48h_time_weighted),
    metric_value_48h_time_weighted > 0
  )

if (nrow(plot_tail_adjusted_log) > 0) {
  fig_tail_adjusted_log <- ggplot(
    plot_tail_adjusted_log,
    aes(x = metric_value_48h_time_weighted, color = study_arm_overall)
  ) +
    stat_ecdf(aes(y = after_stat((1 - y) * 100)), linewidth = 0.85) +
    facet_wrap(~ timepoint, nrow = 1) +
    scale_x_log10() +
    scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(
      x = "Positive 48-hour time-weighted PM2.5 (ug/m3, log scale)",
      y = "Household windows at or above concentration (%)",
      color = "Study arm",
      title = paste0(hapin_adjusted_metric_label, " upper-tail distribution"),
      subtitle = "Positive adjusted household windows only; PM2.5 axis is log scaled"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = "grey75")
    )

  hapin_save_plot(
    fig_tail_adjusted_log,
    paste0(
      "fig_pm25_hapin_tail_exceedance_",
      hapin_adjusted_metric_token,
      "_positive_log_x.png"
    ),
    width = 10,
    height = 5.6
  )
}

hapin_graphic_source_audit <- bind_rows(
  data.frame(
    audit_item = c(
      "source_script",
      "period_definition",
      "valid_24h_coverage_threshold_percent",
      "monitoring_interval_assumption_seconds",
      "raw_metric_definition",
      "ambient_adjusted_metric_definition",
      "hour_of_day_time_zone",
      "public_row_level_deidentification",
      "uncertainty_display",
      "ambient_adjusted_log_scale_display"
    ),
    audit_value = c(
      normalizePath(script_path, winslash = "/", mustWork = FALSE),
      "Day 1 and Day 2 are consecutive 24-hour periods beginning at each monitoring deployment start_datetime.",
      as.character(valid_monitoring_coverage_threshold * 100),
      as.character(expected_monitoring_interval_seconds),
      "Raw indoor PM2.5 uses the arithmetic mean of positive finite indoor rows in each valid period/window.",
      sprintf(
        "Ambient-adjusted indoor-excess PM2.5 is raw indoor arithmetic mean minus %.2f x concurrent ambient arithmetic mean.",
        default_material_infiltration_factor
      ),
      hapin_hour_time_zone,
      "Public HAPIN PM2.5 row-level outputs remove household IDs, FCN IDs, household notes, raw filenames, monitor IDs, internal coverage-window IDs, and exact timestamps.",
      "Shaded hour-of-day bands are descriptive percentiles across household-period-hour summaries, not 95% confidence intervals.",
      hapin_adjusted_log_note
    ),
    output_file = NA_character_,
    stringsAsFactors = FALSE
  ),
  data.frame(
    audit_item = "generated_figure",
    audit_value = hapin_generated_figures,
    output_file = file.path(figure_dir, hapin_generated_figures),
    stringsAsFactors = FALSE
  )
)

readr::write_csv(
  hapin_graphic_source_audit,
  file.path(table_dir, "table_pm25_hapin_graphic_source_audit.csv"),
  na = ""
)


make_analysis_data_from_indoor <- function(indoor_input, dataset_label) {
  indoor_windows_local <- indoor_input %>%
    filter(
      timepoint %in% timepoint_levels,
      study_arm_overall %in% arm_levels,
      !is.na(hh_id),
      !is.na(dateTime),
      !is.na(pm25_ug_m3),
      is.finite(pm25_ug_m3),
      pm25_ug_m3 > 0
    ) %>%
    group_by(timepoint, study_arm_overall, hh_id, fcn_id, hh_id_note, raw_source_file, PM_monitor) %>%
    summarise(
      start_datetime = min(dateTime, na.rm = TRUE),
      end_datetime = max(dateTime, na.rm = TRUE),
      midpoint_datetime = mean_datetime(dateTime),
      n_obs_indoor = n(),
      n_hours_indoor = n_distinct(lubridate::floor_date(dateTime, unit = "hour")),
      duration_hours = as.numeric(difftime(max(dateTime, na.rm = TRUE), min(dateTime, na.rm = TRUE), units = "hours")),
      indoor_mean_pm = mean(pm25_ug_m3, na.rm = TRUE),
      indoor_gmean_pm = geo_mean(pm25_ug_m3),
      indoor_median_pm = median(pm25_ug_m3, na.rm = TRUE),
      indoor_p05_pm = safe_quantile(pm25_ug_m3, 0.05),
      indoor_p25_pm = safe_quantile(pm25_ug_m3, 0.25),
      indoor_p75_pm = safe_quantile(pm25_ug_m3, 0.75),
      indoor_p95_pm = safe_quantile(pm25_ug_m3, 0.95),
      indoor_p99_pm = safe_quantile(pm25_ug_m3, 0.99),
      indoor_max_pm = max(pm25_ug_m3, na.rm = TRUE),
      pct_obs_gt_35 = mean(pm25_ug_m3 > 35, na.rm = TRUE) * 100,
      pct_obs_gt_75 = mean(pm25_ug_m3 > 75, na.rm = TRUE) * 100,
      pct_obs_gt_150 = mean(pm25_ug_m3 > 150, na.rm = TRUE) * 100,
      pct_obs_gt_400 = mean(pm25_ug_m3 > 400, na.rm = TRUE) * 100,
      pct_obs_gt_1000 = mean(pm25_ug_m3 >= 1000, na.rm = TRUE) * 100,
      pct_obs_gt_5000 = mean(pm25_ug_m3 >= 5000, na.rm = TRUE) * 100,
      .groups = "drop"
    ) %>%
    mutate(
      household_window_id = sprintf(paste0(dataset_label, "_pm_window_%04d"), row_number()),
      timepoint = as_ordered_timepoint(timepoint),
      study_arm_overall = factor(study_arm_overall, levels = arm_levels)
    ) %>%
    arrange(timepoint, study_arm_overall, start_datetime, household_window_id)

  match_one_window_local <- function(i) {
    start_i <- indoor_windows_local$start_datetime[[i]]
    end_i <- indoor_windows_local$end_datetime[[i]]
    arm_i <- as.character(indoor_windows_local$study_arm_overall[[i]])
    duration_i <- indoor_windows_local$duration_hours[[i]]

    concurrent <- ambient_clean %>% filter(dateTime >= start_i, dateTime <= end_i)
    concurrent_same_arm <- concurrent %>% filter(study_arm_overall == arm_i)

    summarize_ambient <- function(data, prefix) {
      if (nrow(data) == 0) {
        out <- data.frame(
          n_rows = 0L,
          n_files = 0L,
          n_hours = 0L,
          coverage_prop = NA_real_,
          mean_pm = NA_real_,
          gmean_pm = NA_real_,
          median_pm = NA_real_,
          p05_pm = NA_real_,
          p95_pm = NA_real_,
          first_datetime = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"),
          last_datetime = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"),
          sites = NA_character_,
          notes = NA_character_,
          stringsAsFactors = FALSE
        )
      } else {
        n_hours <- n_distinct(data$ambient_hour)
        expected_hours <- max(1, ceiling(duration_i))
        out <- data.frame(
          n_rows = nrow(data),
          n_files = n_distinct(data$raw_source_file),
          n_hours = n_hours,
          coverage_prop = min(1, n_hours / expected_hours),
          mean_pm = mean(data$pm25_ug_m3, na.rm = TRUE),
          gmean_pm = geo_mean(data$pm25_ug_m3),
          median_pm = median(data$pm25_ug_m3, na.rm = TRUE),
          p05_pm = safe_quantile(data$pm25_ug_m3, 0.05),
          p95_pm = safe_quantile(data$pm25_ug_m3, 0.95),
          first_datetime = min(data$dateTime, na.rm = TRUE),
          last_datetime = max(data$dateTime, na.rm = TRUE),
          sites = paste(sort(unique(na.omit(data$ambient_site_id))), collapse = "; "),
          notes = paste(sort(unique(na.omit(data$note_clean))), collapse = "; "),
          stringsAsFactors = FALSE
        )
        if (!nzchar(out$sites)) out$sites <- NA_character_
        if (!nzchar(out$notes)) out$notes <- NA_character_
      }
      names(out) <- paste0(prefix, names(out))
      out
    }

    cbind(
      summarize_ambient(concurrent, "ambient_all_"),
      summarize_ambient(concurrent_same_arm, "ambient_same_arm_")
    )
  }

  ambient_matches_local <- bind_rows(lapply(seq_len(nrow(indoor_windows_local)), match_one_window_local))
  out <- bind_cols(indoor_windows_local, ambient_matches_local) %>%
    mutate(
      midpoint_date = as.Date(midpoint_datetime),
      midpoint_day_num = as.numeric(midpoint_date),
      log_indoor_gmean_pm = log(indoor_gmean_pm),
      log_indoor_mean_pm = log(indoor_mean_pm),
      log_ambient_gmean_pm = log(ambient_all_gmean_pm),
      log_ambient_mean_pm = log(ambient_all_mean_pm),
      log_io_ratio = log(indoor_gmean_pm / ambient_all_gmean_pm),
      indoor_minus_ambient_material_default = indoor_mean_pm - default_material_infiltration_factor * ambient_all_mean_pm,
      indoor_minus_ambient_f025 = indoor_mean_pm - 0.25 * ambient_all_mean_pm,
      indoor_minus_ambient_f050 = indoor_mean_pm - 0.50 * ambient_all_mean_pm,
      indoor_minus_ambient_f075 = indoor_mean_pm - 0.75 * ambient_all_mean_pm,
      indoor_minus_ambient_f100 = indoor_mean_pm - 1.00 * ambient_all_mean_pm,
      has_concurrent_ambient = ambient_all_n_rows > 0,
      timepoint = as_ordered_timepoint(timepoint),
      study_arm_overall = factor(study_arm_overall, levels = arm_levels),
      analysis_population = dataset_label
    )

  round_start_dates_local <- out %>%
    group_by(timepoint) %>%
    summarise(round_start_day_num = min(midpoint_day_num, na.rm = TRUE), .groups = "drop")

  out %>%
    left_join(round_start_dates_local, by = "timepoint") %>%
    mutate(days_since_round_start = midpoint_day_num - round_start_day_num)
}
message("Matching household windows to concurrent ambient PM2.5")
match_one_window <- function(i) {
  start_i <- indoor_windows$start_datetime[[i]]
  end_i <- indoor_windows$end_datetime[[i]]
  arm_i <- as.character(indoor_windows$study_arm_overall[[i]])
  duration_i <- indoor_windows$duration_hours[[i]]

  concurrent <- ambient_clean %>%
    filter(dateTime >= start_i, dateTime <= end_i)
  concurrent_same_arm <- concurrent %>%
    filter(study_arm_overall == arm_i)

  summarize_ambient <- function(data, prefix) {
    if (nrow(data) == 0) {
      out <- data.frame(
        n_rows = 0L,
        n_files = 0L,
        n_hours = 0L,
        coverage_prop = NA_real_,
        mean_pm = NA_real_,
        gmean_pm = NA_real_,
        median_pm = NA_real_,
        p05_pm = NA_real_,
        p95_pm = NA_real_,
        first_datetime = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"),
        last_datetime = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"),
        sites = NA_character_,
        notes = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      n_hours <- n_distinct(data$ambient_hour)
      expected_hours <- max(1, ceiling(duration_i))
      out <- data.frame(
        n_rows = nrow(data),
        n_files = n_distinct(data$raw_source_file),
        n_hours = n_hours,
        coverage_prop = min(1, n_hours / expected_hours),
        mean_pm = mean(data$pm25_ug_m3, na.rm = TRUE),
        gmean_pm = geo_mean(data$pm25_ug_m3),
        median_pm = median(data$pm25_ug_m3, na.rm = TRUE),
        p05_pm = safe_quantile(data$pm25_ug_m3, 0.05),
        p95_pm = safe_quantile(data$pm25_ug_m3, 0.95),
        first_datetime = min(data$dateTime, na.rm = TRUE),
        last_datetime = max(data$dateTime, na.rm = TRUE),
        sites = paste(sort(unique(na.omit(data$ambient_site_id))), collapse = "; "),
        notes = paste(sort(unique(na.omit(data$note_clean))), collapse = "; "),
        stringsAsFactors = FALSE
      )
      if (!nzchar(out$sites)) out$sites <- NA_character_
      if (!nzchar(out$notes)) out$notes <- NA_character_
    }
    names(out) <- paste0(prefix, names(out))
    out
  }

  cbind(
    summarize_ambient(concurrent, "ambient_all_"),
    summarize_ambient(concurrent_same_arm, "ambient_same_arm_")
  )
}

ambient_matches <- bind_rows(lapply(seq_len(nrow(indoor_windows)), match_one_window))

analysis_data <- bind_cols(indoor_windows, ambient_matches) %>%
  mutate(
    midpoint_date = as.Date(midpoint_datetime),
    midpoint_day_num = as.numeric(midpoint_date),
    log_indoor_gmean_pm = log(indoor_gmean_pm),
    log_indoor_mean_pm = log(indoor_mean_pm),
    log_ambient_gmean_pm = log(ambient_all_gmean_pm),
    log_ambient_mean_pm = log(ambient_all_mean_pm),
    log_io_ratio = log(indoor_gmean_pm / ambient_all_gmean_pm),
    indoor_minus_ambient_material_default = indoor_mean_pm - default_material_infiltration_factor * ambient_all_mean_pm,
    indoor_minus_ambient_f025 = indoor_mean_pm - 0.25 * ambient_all_mean_pm,
    indoor_minus_ambient_f050 = indoor_mean_pm - 0.50 * ambient_all_mean_pm,
    indoor_minus_ambient_f075 = indoor_mean_pm - 0.75 * ambient_all_mean_pm,
    indoor_minus_ambient_f100 = indoor_mean_pm - 1.00 * ambient_all_mean_pm,
    has_concurrent_ambient = ambient_all_n_rows > 0,
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = arm_levels)
  )

round_start_dates <- analysis_data %>%
  group_by(timepoint) %>%
  summarise(round_start_day_num = min(midpoint_day_num, na.rm = TRUE), .groups = "drop")

analysis_data <- analysis_data %>%
  left_join(round_start_dates, by = "timepoint") %>%
  mutate(days_since_round_start = midpoint_day_num - round_start_day_num)

message("Writing analysis-ready datasets and diagnostics")
analysis_internal_path <- file.path(restricted_table_dir, "table_pm25_window_dataset_internal.csv")
analysis_deidentified_path <- file.path(table_dir, "table_pm25_window_dataset_deidentified.csv")

readr::write_csv(analysis_data, analysis_internal_path, na = "")
analysis_deidentified <- analysis_data %>%
  select(
    -hh_id, -fcn_id, -hh_id_note, -raw_source_file, -PM_monitor,
    -ambient_all_sites, -ambient_same_arm_sites
  )
readr::write_csv(analysis_deidentified, analysis_deidentified_path, na = "")

household_window_counts <- analysis_data %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_households = n_distinct(hh_id),
    n_windows = n(),
    n_monitor_files = n_distinct(raw_source_file),
    n_pm_rows = sum(n_obs_indoor, na.rm = TRUE),
    median_duration_hours = median(duration_hours, na.rm = TRUE),
    n_windows_with_concurrent_ambient = sum(has_concurrent_ambient, na.rm = TRUE),
    n_windows_without_concurrent_ambient = sum(!has_concurrent_ambient, na.rm = TRUE),
    count_unit = "one row per household monitoring file/window",
    .groups = "drop"
  ) %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  )
write_scaffolded_csv(
  household_window_counts,
  file.path(table_dir, "table_pm25_window_counts.csv")
)

ambient_matching_diagnostics <- analysis_data %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_windows = n(),
    n_matched_concurrent_ambient = sum(has_concurrent_ambient, na.rm = TRUE),
    n_unmatched_concurrent_ambient = sum(!has_concurrent_ambient, na.rm = TRUE),
    prop_matched_concurrent_ambient = mean(has_concurrent_ambient, na.rm = TRUE),
    median_ambient_coverage_prop = median(ambient_all_coverage_prop, na.rm = TRUE),
    min_ambient_coverage_prop = min(ambient_all_coverage_prop, na.rm = TRUE),
    max_ambient_coverage_prop = max(ambient_all_coverage_prop, na.rm = TRUE),
    median_concurrent_ambient_gmean_pm = median(ambient_all_gmean_pm, na.rm = TRUE),
    median_concurrent_ambient_mean_pm = median(ambient_all_mean_pm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  )
write_scaffolded_csv(
  ambient_matching_diagnostics,
  file.path(table_dir, "table_pm25_matching_diagnostics.csv")
)

sampling_period_diagnostics <- analysis_data %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_households = n_distinct(hh_id),
    n_windows = n(),
    first_start_datetime = min(start_datetime, na.rm = TRUE),
    last_end_datetime = max(end_datetime, na.rm = TRUE),
    sampling_span_days = as.numeric(difftime(max(end_datetime, na.rm = TRUE), min(start_datetime, na.rm = TRUE), units = "days")),
    median_midpoint_date = median(midpoint_date, na.rm = TRUE),
    median_indoor_gmean_pm = median(indoor_gmean_pm, na.rm = TRUE),
    median_ambient_gmean_pm = median(ambient_all_gmean_pm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  )
write_scaffolded_csv(
  sampling_period_diagnostics,
  file.path(table_dir, "table_pm25_sampling_diagnostics.csv")
)

ambient_period_diagnostics <- ambient_clean %>%
  mutate(
    ambient_year = lubridate::year(dateTime),
    ambient_date = as.Date(dateTime)
  ) %>%
  group_by(ambient_year, timepoint, study_arm_overall, ambient_site_id, note_clean) %>%
  summarise(
    n_rows = n(),
    n_files = n_distinct(raw_source_file),
    first_ambient_datetime = min(dateTime, na.rm = TRUE),
    last_ambient_datetime = max(dateTime, na.rm = TRUE),
    ambient_mean_pm = mean(pm25_ug_m3, na.rm = TRUE),
    ambient_gmean_pm = geo_mean(pm25_ug_m3),
    .groups = "drop"
  ) %>%
  arrange(ambient_year, timepoint, study_arm_overall, ambient_site_id, note_clean)
readr::write_csv(
  ambient_period_diagnostics,
  file.path(table_dir, "table_pm25_ambient_periods.csv"),
  na = ""
)

common_support_by_arm <- analysis_data %>%
  filter(has_concurrent_ambient) %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_windows = n(),
    midpoint_day_min = min(midpoint_day_num, na.rm = TRUE),
    midpoint_day_max = max(midpoint_day_num, na.rm = TRUE),
    ambient_gmean_min = min(ambient_all_gmean_pm, na.rm = TRUE),
    ambient_gmean_max = max(ambient_all_gmean_pm, na.rm = TRUE),
    .groups = "drop"
  )

common_support_diagnostics <- bind_rows(lapply(timepoint_levels, function(tp) {
  x <- common_support_by_arm %>% filter(as.character(timepoint) == tp)
  if (n_distinct(as.character(x$study_arm_overall)) < 2) {
    return(data.frame(
      timepoint = tp,
      has_both_arms = FALSE,
      overlap_midpoint_day_min = NA_real_,
      overlap_midpoint_day_max = NA_real_,
      has_date_overlap = FALSE,
      overlap_ambient_gmean_min = NA_real_,
      overlap_ambient_gmean_max = NA_real_,
      has_ambient_overlap = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  date_min <- max(x$midpoint_day_min, na.rm = TRUE)
  date_max <- min(x$midpoint_day_max, na.rm = TRUE)
  ambient_min <- max(x$ambient_gmean_min, na.rm = TRUE)
  ambient_max <- min(x$ambient_gmean_max, na.rm = TRUE)
  data.frame(
    timepoint = tp,
    has_both_arms = TRUE,
    overlap_midpoint_day_min = date_min,
    overlap_midpoint_day_max = date_max,
    has_date_overlap = is.finite(date_min) && is.finite(date_max) && date_min <= date_max,
    overlap_ambient_gmean_min = ambient_min,
    overlap_ambient_gmean_max = ambient_max,
    has_ambient_overlap = is.finite(ambient_min) && is.finite(ambient_max) && ambient_min <= ambient_max,
    stringsAsFactors = FALSE
  )
}))

analysis_data <- analysis_data %>%
  left_join(common_support_diagnostics, by = "timepoint") %>%
  mutate(
    in_common_support = has_concurrent_ambient &
      has_date_overlap &
      has_ambient_overlap &
      midpoint_day_num >= overlap_midpoint_day_min &
      midpoint_day_num <= overlap_midpoint_day_max &
      ambient_all_gmean_pm >= overlap_ambient_gmean_min &
      ambient_all_gmean_pm <= overlap_ambient_gmean_max
  )

common_support_counts <- analysis_data %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_windows = n(),
    n_windows_concurrent_ambient = sum(has_concurrent_ambient, na.rm = TRUE),
    n_windows_common_support = sum(in_common_support, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  )

common_support_out <- common_support_counts %>%
  left_join(common_support_diagnostics, by = "timepoint")
write_scaffolded_csv(
  common_support_out,
  file.path(table_dir, "table_pm25_common_support_diagnostics.csv")
)

# Rewrite analysis datasets after common-support indicators are added.
readr::write_csv(analysis_data, analysis_internal_path, na = "")
analysis_deidentified <- analysis_data %>%
  select(
    -hh_id, -fcn_id, -hh_id_note, -raw_source_file, -PM_monitor,
    -ambient_all_sites, -ambient_same_arm_sites
  )
readr::write_csv(analysis_deidentified, analysis_deidentified_path, na = "")

add_common_support_indicators <- function(data) {
  common_support_by_arm_local <- data %>%
    filter(has_concurrent_ambient) %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise(
      n_windows = n(),
      midpoint_day_min = min(midpoint_day_num, na.rm = TRUE),
      midpoint_day_max = max(midpoint_day_num, na.rm = TRUE),
      ambient_gmean_min = min(ambient_all_gmean_pm, na.rm = TRUE),
      ambient_gmean_max = max(ambient_all_gmean_pm, na.rm = TRUE),
      .groups = "drop"
    )

  common_support_diagnostics_local <- bind_rows(lapply(timepoint_levels, function(tp) {
    x <- common_support_by_arm_local %>% filter(as.character(timepoint) == tp)
    if (n_distinct(as.character(x$study_arm_overall)) < 2) {
      return(data.frame(
        timepoint = tp,
        has_both_arms = FALSE,
        overlap_midpoint_day_min = NA_real_,
        overlap_midpoint_day_max = NA_real_,
        has_date_overlap = FALSE,
        overlap_ambient_gmean_min = NA_real_,
        overlap_ambient_gmean_max = NA_real_,
        has_ambient_overlap = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    date_min <- max(x$midpoint_day_min, na.rm = TRUE)
    date_max <- min(x$midpoint_day_max, na.rm = TRUE)
    ambient_min <- max(x$ambient_gmean_min, na.rm = TRUE)
    ambient_max <- min(x$ambient_gmean_max, na.rm = TRUE)
    data.frame(
      timepoint = tp,
      has_both_arms = TRUE,
      overlap_midpoint_day_min = date_min,
      overlap_midpoint_day_max = date_max,
      has_date_overlap = is.finite(date_min) && is.finite(date_max) && date_min <= date_max,
      overlap_ambient_gmean_min = ambient_min,
      overlap_ambient_gmean_max = ambient_max,
      has_ambient_overlap = is.finite(ambient_min) && is.finite(ambient_max) && ambient_min <= ambient_max,
      stringsAsFactors = FALSE
    )
  }))

  data %>%
    left_join(common_support_diagnostics_local, by = "timepoint") %>%
    mutate(
      in_common_support = has_concurrent_ambient &
        has_date_overlap &
        has_ambient_overlap &
        midpoint_day_num >= overlap_midpoint_day_min &
        midpoint_day_num <= overlap_midpoint_day_max &
        ambient_all_gmean_pm >= overlap_ambient_gmean_min &
        ambient_all_gmean_pm <= overlap_ambient_gmean_max
    )
}
date_adjustment_term <- function(data) {
  n_unique_days <- length(unique(data$midpoint_day_num[is.finite(data$midpoint_day_num)]))
  if (n_unique_days >= 4) {
    return("splines::ns(midpoint_day_num, df = 3)")
  }
  if (n_unique_days >= 2) {
    return("midpoint_day_num")
  }
  character()
}

make_not_fit_rows <- function(model_label, model_formula, model_class, fit_status, note, outcome, outcome_scale, data) {
  bind_rows(lapply(timepoint_levels, function(tp) {
    data.frame(
      model_label = model_label,
      model_formula = model_formula,
      model_class = model_class,
      fit_status = fit_status,
      note = note,
      outcome = outcome,
      outcome_scale = outcome_scale,
      timepoint = tp,
      comparison = "intervention_vs_comparison",
      n_obs_model = nrow(data),
      n_households_model = if ("hh_id" %in% names(data)) n_distinct(data$hh_id) else NA_integer_,
      n_obs_timepoint = sum(as.character(data$timepoint) == tp, na.rm = TRUE),
      estimate = NA_real_,
      std_error = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      ratio = NA_real_,
      ratio_conf_low = NA_real_,
      ratio_conf_high = NA_real_,
      percent_difference = NA_real_,
      percent_difference_conf_low = NA_real_,
      percent_difference_conf_high = NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}

extract_arm_contrasts <- function(model, model_data, model_label, model_formula, model_class, outcome, outcome_scale) {
  beta <- if (inherits(model, "merMod")) lme4::fixef(model) else stats::coef(model)
  vc <- as.matrix(stats::vcov(model))
  coef_names <- names(beta)
  arm_coef <- "study_arm_overallintervention"
  has_any_interaction <- any(grepl("study_arm_overallintervention:timepoint|timepoint.*:study_arm_overallintervention", coef_names))
  model_timepoints <- unique(as.character(model_data$timepoint))

  bind_rows(lapply(timepoint_levels, function(tp) {
    tp_data <- model_data %>% filter(as.character(timepoint) == tp)
    n_obs_tp <- nrow(tp_data)
    arms_tp <- unique(as.character(tp_data$study_arm_overall))

    base_row <- data.frame(
      model_label = model_label,
      model_formula = model_formula,
      model_class = model_class,
      fit_status = "fit",
      note = "",
      outcome = outcome,
      outcome_scale = outcome_scale,
      timepoint = tp,
      comparison = "intervention_vs_comparison",
      n_obs_model = nrow(model_data),
      n_households_model = n_distinct(model_data$hh_id),
      n_obs_timepoint = n_obs_tp,
      estimate = NA_real_,
      std_error = NA_real_,
      conf_low = NA_real_,
      conf_high = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      ratio = NA_real_,
      ratio_conf_low = NA_real_,
      ratio_conf_high = NA_real_,
      percent_difference = NA_real_,
      percent_difference_conf_low = NA_real_,
      percent_difference_conf_high = NA_real_,
      stringsAsFactors = FALSE
    )

    if (n_obs_tp == 0 || length(intersect(arms_tp, arm_levels)) < 2) {
      base_row$fit_status <- "not_estimable"
      base_row$note <- "This timepoint does not include both arms in the model dataset."
      return(base_row)
    }

    if (!arm_coef %in% coef_names) {
      base_row$fit_status <- "not_estimable"
      base_row$note <- "The main intervention coefficient was not estimable."
      return(base_row)
    }

    contrast <- setNames(rep(0, length(beta)), coef_names)
    contrast[[arm_coef]] <- 1

    if (tp != timepoint_levels[[1]]) {
      interaction_candidates <- c(
        paste0("study_arm_overallintervention:timepoint", tp),
        paste0("timepoint", tp, ":study_arm_overallintervention")
      )
      interaction_coef <- interaction_candidates[interaction_candidates %in% coef_names]

      if (length(interaction_coef) > 0) {
        contrast[[interaction_coef[[1]]]] <- 1
      } else if (length(model_timepoints) > 1 && has_any_interaction) {
        base_row$fit_status <- "not_estimable"
        base_row$note <- "The required arm-by-timepoint interaction coefficient was not estimable."
        return(base_row)
      }
    }

    estimate <- as.numeric(sum(contrast * beta))
    std_error <- as.numeric(sqrt(t(contrast) %*% vc %*% contrast))
    if (!is.finite(std_error) || std_error <= 0) {
      base_row$fit_status <- "not_estimable"
      base_row$note <- "The contrast standard error was not finite."
      return(base_row)
    }
    statistic <- estimate / std_error
    conf_low <- estimate - 1.96 * std_error
    conf_high <- estimate + 1.96 * std_error

    base_row$estimate <- estimate
    base_row$std_error <- std_error
    base_row$conf_low <- conf_low
    base_row$conf_high <- conf_high
    base_row$statistic <- statistic
    base_row$p_value <- 2 * stats::pnorm(abs(statistic), lower.tail = FALSE)

    if (identical(outcome_scale, "log_ratio")) {
      base_row$ratio <- exp(estimate)
      base_row$ratio_conf_low <- exp(conf_low)
      base_row$ratio_conf_high <- exp(conf_high)
      base_row$percent_difference <- 100 * (exp(estimate) - 1)
      base_row$percent_difference_conf_low <- 100 * (exp(conf_low) - 1)
      base_row$percent_difference_conf_high <- 100 * (exp(conf_high) - 1)
    }

    base_row
  }))
}

fit_arm_contrast_model <- function(
  data,
  outcome,
  model_label,
  adjustment_terms = character(),
  date_adjust = FALSE,
  use_random_intercept = TRUE,
  outcome_scale = "log_ratio"
) {
  model_data <- data %>%
    filter(
      timepoint %in% timepoint_levels,
      study_arm_overall %in% arm_levels,
      !is.na(hh_id),
      !is.na(.data[[outcome]]),
      is.finite(.data[[outcome]])
    ) %>%
    mutate(
      timepoint = as_ordered_timepoint(timepoint),
      study_arm_overall = factor(as.character(study_arm_overall), levels = arm_levels),
      hh_id = factor(hh_id)
    )

  if (length(adjustment_terms) > 0) {
    for (term_var in c("log_ambient_gmean_pm", "midpoint_day_num")) {
      if (grepl(term_var, paste(adjustment_terms, collapse = " + "), fixed = TRUE)) {
        model_data <- model_data %>% filter(!is.na(.data[[term_var]]), is.finite(.data[[term_var]]))
      }
    }
  }

  if (date_adjust) {
    date_term <- date_adjustment_term(model_data)
    adjustment_terms <- c(adjustment_terms, date_term)
  }

  fixed_terms <- c("study_arm_overall * timepoint", adjustment_terms)
  fixed_formula_text <- paste(outcome, "~", paste(fixed_terms, collapse = " + "))
  use_lmer <- use_random_intercept && n_distinct(model_data$hh_id) > 1 && nrow(model_data) > n_distinct(model_data$hh_id)
  model_formula <- if (use_lmer) {
    paste(fixed_formula_text, "+ (1 | hh_id)")
  } else {
    fixed_formula_text
  }

  if (
    nrow(model_data) < 6 ||
      n_distinct(model_data$study_arm_overall) < 2 ||
      n_distinct(model_data$timepoint) < 1
  ) {
    return(make_not_fit_rows(
      model_label,
      model_formula,
      if (use_lmer) "lmer" else "lm",
      "not_fit",
      "Insufficient observations, arms, or timepoints for model fitting.",
      outcome,
      outcome_scale,
      model_data
    ))
  }

  fit <- tryCatch(
    {
      if (use_lmer) {
        lme4::lmer(
          stats::as.formula(model_formula),
          data = model_data,
          REML = FALSE,
          control = lme4::lmerControl(
            optimizer = "bobyqa",
            optCtrl = list(maxfun = 100000),
            check.conv.singular = "ignore"
          )
        )
      } else {
        stats::lm(stats::as.formula(model_formula), data = model_data)
      }
    },
    error = function(e) e
  )

  model_class <- if (use_lmer) "lmer" else "lm"

  if (inherits(fit, "error") && use_lmer) {
    fallback_formula <- fixed_formula_text
    fit <- tryCatch(
      stats::lm(stats::as.formula(fallback_formula), data = model_data),
      error = function(e) e
    )
    model_formula <- fallback_formula
    model_class <- "lm_fallback_after_lmer_error"
  }

  if (inherits(fit, "error")) {
    return(make_not_fit_rows(
      model_label,
      model_formula,
      model_class,
      "not_fit",
      paste("Model error:", conditionMessage(fit)),
      outcome,
      outcome_scale,
      model_data
    ))
  }

  out <- extract_arm_contrasts(
    model = fit,
    model_data = model_data,
    model_label = model_label,
    model_formula = model_formula,
    model_class = model_class,
    outcome = outcome,
    outcome_scale = outcome_scale
  )

  if (inherits(fit, "merMod") && lme4::isSingular(fit, tol = 1e-4)) {
    out$note <- ifelse(
      nzchar(out$note),
      paste(out$note, "Random-effect fit is singular.", sep = " "),
      "Random-effect fit is singular."
    )
  }

  out
}

################################################################################
# rDiD estimator helpers for paired baseline-follow-up excess PM2.5 outcomes
################################################################################

xgb_xfit <- function(X_tr, y_tr, X_te, objective,
                     depths = as.numeric(strsplit(Sys.getenv("PM25_XGB_DEPTHS", unset = "2"), ",")[[1]]),
                     etas = as.numeric(strsplit(Sys.getenv("PM25_XGB_ETAS", unset = "0.05"), ",")[[1]]),
                     max_nrounds = as.integer(Sys.getenv("PM25_XGB_MAX_NROUNDS", unset = "100")),
                     early_stopping_rounds = as.integer(Sys.getenv("PM25_XGB_EARLY_STOP", unset = "10")),
                     seed = 1) {
  keep <- !is.na(y_tr)
  X_tr <- X_tr[keep, , drop = FALSE]
  y_tr <- y_tr[keep]

  fallback <- mean(y_tr, na.rm = TRUE)
  if (!is.finite(fallback)) fallback <- 0

  if (nrow(X_tr) < 5) return(rep(fallback, nrow(X_te)))
  if (objective == "binary:logistic" && length(unique(y_tr)) < 2) return(rep(fallback, nrow(X_te)))

  d <- xgboost::xgb.DMatrix(X_tr, label = y_tr, missing = NA)
  metric <- if (objective == "reg:squarederror") "rmse" else "logloss"
  log_col <- paste0("test_", metric, "_mean")
  cv_nfold <- min(3, nrow(X_tr))
  best <- list(score = Inf, pars = NULL, nrounds = NULL)

  for (mxd in depths) {
    for (eta in etas) {
      pars <- list(
        objective = objective,
        max_depth = mxd,
        eta = eta,
        nthread = as.integer(Sys.getenv("PM25_XGB_NTHREAD", unset = "2")),
        verbosity = 0
      )

      set.seed(seed)
      cv <- tryCatch(
        xgboost::xgb.cv(
          params = pars,
          data = d,
          nrounds = max_nrounds,
          nfold = cv_nfold,
          early_stopping_rounds = early_stopping_rounds,
          verbose = 0,
          metrics = metric
        ),
        error = function(e) NULL
      )

      if (is.null(cv) || !(log_col %in% names(cv$evaluation_log))) next
      i <- which.min(cv$evaluation_log[[log_col]])
      score <- cv$evaluation_log[[log_col]][i]
      if (!is.na(score) && score < best$score) {
        best <- list(score = score, pars = pars, nrounds = i)
      }
    }
  }

  if (is.null(best$pars) || is.null(best$nrounds)) return(rep(fallback, nrow(X_te)))

  mod <- xgboost::xgb.train(
    params = best$pars,
    data = d,
    nrounds = best$nrounds,
    verbose = 0
  )
  predict(mod, xgboost::xgb.DMatrix(X_te, missing = NA))
}

rdid_dml_xgb <- function(dat, x_vars, K = 5, seed = 1) {
  dat <- dat %>% filter(!is.na(Z), !is.na(Y), !is.na(A))
  n <- nrow(dat)
  arm_counts <- table(dat$A)

  if (n < 10 || length(arm_counts) < 2 || min(arm_counts) < 2) {
    return(list(estimate = NA_real_, se = NA_real_, conf.low = NA_real_,
                conf.high = NA_real_, p.value = NA_real_, n = n,
                note = "insufficient data for rDiD/XGBoost"))
  }

  set.seed(seed)
  X <- data.matrix(dat[, x_vars, drop = FALSE])
  A <- as_number(dat$A)
  D <- as_number(dat$Y - dat$Z)
  K_eff <- min(K, n, as.integer(min(arm_counts)))
  folds <- sample(rep(seq_len(K_eff), length.out = n))

  m1_hat <- numeric(n)
  p_hat <- numeric(n)

  for (k in seq_len(K_eff)) {
    te <- which(folds == k)
    tr <- which(folds != k)
    i_tr <- tr[A[tr] == 1]

    m1_hat[te] <- xgb_xfit(
      X[i_tr, , drop = FALSE],
      D[i_tr],
      X[te, , drop = FALSE],
      "reg:squarederror",
      seed = seed + k
    )

    p_hat[te] <- xgb_xfit(
      X[tr, , drop = FALSE],
      A[tr],
      X[te, , drop = FALSE],
      "binary:logistic",
      seed = seed + k
    )
  }

  p_hat <- pmin(pmax(p_hat, 0.01), 0.99)
  pi0 <- mean(1 - A)
  psi_vec <- (A - p_hat) / p_hat * (D - m1_hat) / pi0
  psi <- mean(psi_vec)
  phi <- psi_vec - ((1 - A) / pi0) * psi
  se <- stats::sd(phi) / sqrt(n)
  conf <- psi + c(-1, 1) * stats::qnorm(0.975) * se
  p_value <- ifelse(is.na(se) || se == 0, NA_real_, 2 * stats::pnorm(-abs(psi / se)))

  list(
    estimate = psi,
    se = se,
    conf.low = conf[[1]],
    conf.high = conf[[2]],
    p.value = p_value,
    n = n,
    note = "rDiD DML-DR estimator with cross-fit XGBoost nuisance models"
  )
}

impute_xvars_for_glm <- function(dat, x_vars) {
  dat %>%
    mutate(across(all_of(x_vars), ~ {
      x <- as_number(.x)
      fill <- mean(x, na.rm = TRUE)
      if (is.na(fill) || is.nan(fill)) fill <- 0
      ifelse(is.na(x), fill, x)
    }))
}

rdid_glm_sensitivity <- function(dat, x_vars) {
  dat <- dat %>%
    filter(!is.na(Z), !is.na(Y), !is.na(A)) %>%
    impute_xvars_for_glm(x_vars)

  n <- nrow(dat)
  arm_counts <- table(dat$A)
  if (n < 10 || length(arm_counts) < 2 || min(arm_counts) < 2) {
    return(list(estimate = NA_real_, se = NA_real_, conf.low = NA_real_,
                conf.high = NA_real_, p.value = NA_real_, n = n,
                note = "insufficient data for rDiD/GLM"))
  }

  X <- data.matrix(dat[, x_vars, drop = FALSE])
  A <- as_number(dat$A)
  D <- as_number(dat$Y - dat$Z)

  m1_fit <- tryCatch(
    stats::glm(D ~ ., data = data.frame(D = D, X)[A == 1, , drop = FALSE], family = stats::gaussian()),
    error = function(e) NULL
  )
  p_fit <- tryCatch(
    stats::glm(A ~ ., data = data.frame(A = A, X), family = stats::binomial()),
    error = function(e) NULL
  )

  if (is.null(m1_fit) || is.null(p_fit)) {
    return(list(estimate = NA_real_, se = NA_real_, conf.low = NA_real_,
                conf.high = NA_real_, p.value = NA_real_, n = n,
                note = "rDiD/GLM nuisance model failed"))
  }

  m1 <- stats::predict(m1_fit, newdata = data.frame(X), type = "response")
  p_hat <- pmin(pmax(stats::predict(p_fit, newdata = data.frame(X), type = "response"), 0.01), 0.99)

  pi0 <- mean(1 - A)
  psi_vec <- (A - p_hat) / p_hat * (D - m1) / pi0
  psi <- mean(psi_vec)
  phi <- psi_vec - ((1 - A) / pi0) * psi
  se <- stats::sd(phi) / sqrt(n)
  conf <- psi + c(-1, 1) * stats::qnorm(0.975) * se
  p_value <- ifelse(is.na(se) || se == 0, NA_real_, 2 * stats::pnorm(-abs(psi / se)))

  list(
    estimate = psi,
    se = se,
    conf.low = conf[[1]],
    conf.high = conf[[2]],
    p.value = p_value,
    n = n,
    note = "rDiD estimator with GLM nuisance models"
  )
}

format_rdid_result_row <- function(res, outcome_info, contrast, followup_timepoint,
                                   estimator, panel) {
  did_estimate <- res$estimate
  did_conf_low <- res$conf.low
  did_conf_high <- res$conf.high
  reduction_estimate <- -did_estimate
  reduction_conf_low <- -did_conf_high
  reduction_conf_high <- -did_conf_low

  data.frame(
    contrast = contrast,
    followup_timepoint = followup_timepoint,
    estimator = estimator,
    outcome = outcome_info$outcome,
    outcome_label = outcome_info$outcome_label,
    infiltration_factor = outcome_info$f_value,
    infiltration_factor_role = outcome_info$infiltration_factor_role,
    outcome_type = "continuous",
    unit = "ug/m3",
    did_estimate_intervention_minus_comparison_change = did_estimate,
    did_se = res$se,
    did_conf_low = did_conf_low,
    did_conf_high = did_conf_high,
    reduction_estimate = reduction_estimate,
    reduction_conf_low = reduction_conf_low,
    reduction_conf_high = reduction_conf_high,
    p_value = res$p.value,
    statistically_significant = ifelse(
      is.na(reduction_conf_low) | is.na(reduction_conf_high),
      NA,
      reduction_conf_low > 0 | reduction_conf_high < 0
    ),
    sample_size = res$n,
    n_households = res$n,
    n_intervention = sum(panel$A == 1, na.rm = TRUE),
    n_comparison = sum(panel$A == 0, na.rm = TRUE),
    reduction_direction_note = "Positive reduction_estimate means intervention households had a larger decrease, or smaller increase, in ambient-excess indoor PM2.5 than comparison households.",
    estimator_note = res$note,
    stringsAsFactors = FALSE
  )
}
message("Fitting primary and sensitivity models")
primary_data <- analysis_data %>%
  filter(has_concurrent_ambient, !is.na(log_ambient_gmean_pm), is.finite(log_ambient_gmean_pm))

model_results <- bind_rows(
  fit_arm_contrast_model(
    data = analysis_data,
    outcome = "log_indoor_gmean_pm",
    model_label = "unadjusted_log_indoor_lmer",
    adjustment_terms = character(),
    date_adjust = FALSE,
    outcome_scale = "log_ratio"
  ),
  fit_arm_contrast_model(
    data = primary_data,
    outcome = "log_indoor_gmean_pm",
    model_label = "ambient_adjusted_no_calendar_lmer",
    adjustment_terms = "log_ambient_gmean_pm",
    date_adjust = FALSE,
    outcome_scale = "log_ratio"
  ),
  fit_arm_contrast_model(
    data = primary_data,
    outcome = "log_indoor_gmean_pm",
    model_label = primary_model_label,
    adjustment_terms = "log_ambient_gmean_pm",
    date_adjust = TRUE,
    outcome_scale = "log_ratio"
  )
)

common_support_data <- primary_data %>% filter(in_common_support)
model_results <- bind_rows(
  model_results,
  fit_arm_contrast_model(
    data = common_support_data,
    outcome = "log_indoor_gmean_pm",
    model_label = "common_support_log_indoor_ambient_calendar_lmer",
    adjustment_terms = "log_ambient_gmean_pm",
    date_adjust = TRUE,
    outcome_scale = "log_ratio"
  )
)

excess_specs <- data.frame(
  f_value = c(0.25, 0.50, default_material_infiltration_factor, 1.00),
  outcome = c(
    "indoor_minus_ambient_f025",
    "indoor_minus_ambient_f050",
    "indoor_minus_ambient_f075",
    "indoor_minus_ambient_f100"
  ),
  infiltration_factor_role = c(
    "sensitivity_lower_bound",
    "sensitivity_midpoint",
    "default_tarp_wall_material_assumption",
    "sensitivity_upper_bound"
  ),
  stringsAsFactors = FALSE
)

excess_results <- bind_rows(lapply(seq_len(nrow(excess_specs)), function(i) {
  model_label_i <- if (identical(excess_specs$infiltration_factor_role[[i]], "default_tarp_wall_material_assumption")) {
    sprintf("absolute_excess_F_material_default_%0.2f_calendar_lmer", excess_specs$f_value[[i]])
  } else {
    sprintf("absolute_excess_sensitivity_F_%0.2f_calendar_lmer", excess_specs$f_value[[i]])
  }

  fit_arm_contrast_model(
    data = primary_data,
    outcome = excess_specs$outcome[[i]],
    model_label = model_label_i,
    adjustment_terms = character(),
    date_adjust = TRUE,
    outcome_scale = "absolute_pm"
  ) %>%
    mutate(
      infiltration_factor = excess_specs$f_value[[i]],
      infiltration_factor_role = excess_specs$infiltration_factor_role[[i]]
    )
}))

io_ratio_results <- fit_arm_contrast_model(
  data = primary_data,
  outcome = "log_io_ratio",
  model_label = "log_indoor_outdoor_ratio_calendar_lmer",
  adjustment_terms = character(),
  date_adjust = TRUE,
  outcome_scale = "log_ratio"
)

model_results <- bind_rows(model_results, excess_results, io_ratio_results)

readr::write_csv(
  model_results,
  file.path(table_dir, "table_pm25_adjusted_sensitivity_results.csv"),
  na = ""
)

primary_model_results <- model_results %>%
  filter(model_label == primary_model_label)
readr::write_csv(
  primary_model_results,
  file.path(table_dir, "table_pm25_primary_adjusted_results.csv"),
  na = ""
)

sensitivity_results <- model_results %>%
  filter(model_label != primary_model_label)
readr::write_csv(
  sensitivity_results,
  file.path(table_dir, "table_pm25_sensitivity_results.csv"),
  na = ""
)
readr::write_csv(
  model_results %>% filter(infiltration_factor_role == "default_tarp_wall_material_assumption"),
  file.path(table_dir, "table_pm25_default_excess_results.csv"),
  na = ""
)
readr::write_csv(
  model_results %>% filter(infiltration_factor_role %in% c("sensitivity_lower_bound", "sensitivity_midpoint", "sensitivity_upper_bound")),
  file.path(table_dir, "table_pm25_excess_sensitivity_results.csv"),
  na = ""
)

analysis_data_anomaly_retained <- NULL
anomaly_retained_model_results <- data.frame(stringsAsFactors = FALSE)
anomaly_retained_window_counts <- data.frame(stringsAsFactors = FALSE)
if (!is.null(indoor_anomaly_retained)) {
  message("Running retained high-PM compatibility/check PM2.5 models")
  analysis_data_anomaly_retained <- make_analysis_data_from_indoor(
    indoor_anomaly_retained,
    "anomaly_retained_sensitivity"
  ) %>%
    add_common_support_indicators()

  readr::write_csv(
    analysis_data_anomaly_retained,
    file.path(restricted_table_dir, "table_pm25_window_dataset_anomaly_retained_internal.csv"),
    na = ""
  )
  readr::write_csv(
    analysis_data_anomaly_retained %>%
      select(-any_of(c(
        "hh_id", "fcn_id", "hh_id_note", "raw_source_file", "PM_monitor",
        "ambient_all_sites", "ambient_same_arm_sites"
      ))),
    file.path(table_dir, "table_pm25_window_dataset_anomaly_retained_deidentified.csv"),
    na = ""
  )

  anomaly_retained_window_counts <- analysis_data_anomaly_retained %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise(
      n_households = n_distinct(hh_id),
      n_windows = n(),
      n_monitor_files = n_distinct(raw_source_file),
      n_pm_rows = sum(n_obs_indoor, na.rm = TRUE),
      n_windows_with_concurrent_ambient = sum(has_concurrent_ambient, na.rm = TRUE),
      n_windows_without_concurrent_ambient = sum(!has_concurrent_ambient, na.rm = TRUE),
      n_windows_common_support = sum(in_common_support, na.rm = TRUE),
      count_unit = "one row per household monitoring file/window",
      .groups = "drop"
    ) %>%
    mutate(
      timepoint = as_ordered_timepoint(timepoint),
      study_arm_overall = as.character(study_arm_overall),
      analysis_population = "anomaly_retained_sensitivity"
    )
  write_scaffolded_csv(
    anomaly_retained_window_counts,
    file.path(table_dir, "table_pm25_window_counts_anomaly_retained_sensitivity.csv")
  )

  primary_data_anomaly_retained <- analysis_data_anomaly_retained %>%
    filter(has_concurrent_ambient, !is.na(log_ambient_gmean_pm), is.finite(log_ambient_gmean_pm))
  common_support_data_anomaly_retained <- primary_data_anomaly_retained %>% filter(in_common_support)

  anomaly_retained_excess_results <- bind_rows(lapply(seq_len(nrow(excess_specs)), function(i) {
    model_label_i <- if (identical(excess_specs$infiltration_factor_role[[i]], "default_tarp_wall_material_assumption")) {
      sprintf("anomaly_retained_absolute_excess_F_material_default_%0.2f_calendar_lmer", excess_specs$f_value[[i]])
    } else {
      sprintf("anomaly_retained_absolute_excess_sensitivity_F_%0.2f_calendar_lmer", excess_specs$f_value[[i]])
    }

    fit_arm_contrast_model(
      data = primary_data_anomaly_retained,
      outcome = excess_specs$outcome[[i]],
      model_label = model_label_i,
      adjustment_terms = character(),
      date_adjust = TRUE,
      outcome_scale = "absolute_pm"
    ) %>%
      mutate(
        infiltration_factor = excess_specs$f_value[[i]],
        infiltration_factor_role = excess_specs$infiltration_factor_role[[i]]
      )
  }))

  anomaly_retained_model_results <- bind_rows(
    fit_arm_contrast_model(
      data = analysis_data_anomaly_retained,
      outcome = "log_indoor_gmean_pm",
      model_label = "anomaly_retained_unadjusted_log_indoor_lmer",
      adjustment_terms = character(),
      date_adjust = FALSE,
      outcome_scale = "log_ratio"
    ),
    fit_arm_contrast_model(
      data = primary_data_anomaly_retained,
      outcome = "log_indoor_gmean_pm",
      model_label = "anomaly_retained_ambient_adjusted_no_calendar_lmer",
      adjustment_terms = "log_ambient_gmean_pm",
      date_adjust = FALSE,
      outcome_scale = "log_ratio"
    ),
    fit_arm_contrast_model(
      data = primary_data_anomaly_retained,
      outcome = "log_indoor_gmean_pm",
      model_label = paste0("anomaly_retained_", primary_model_label),
      adjustment_terms = "log_ambient_gmean_pm",
      date_adjust = TRUE,
      outcome_scale = "log_ratio"
    ),
    fit_arm_contrast_model(
      data = common_support_data_anomaly_retained,
      outcome = "log_indoor_gmean_pm",
      model_label = "anomaly_retained_common_support_log_indoor_ambient_calendar_lmer",
      adjustment_terms = "log_ambient_gmean_pm",
      date_adjust = TRUE,
      outcome_scale = "log_ratio"
    ),
    anomaly_retained_excess_results,
    fit_arm_contrast_model(
      data = primary_data_anomaly_retained,
      outcome = "log_io_ratio",
      model_label = "anomaly_retained_log_indoor_outdoor_ratio_calendar_lmer",
      adjustment_terms = character(),
      date_adjust = TRUE,
      outcome_scale = "log_ratio"
    )
  ) %>%
    mutate(
      analysis_population = "anomaly_retained_sensitivity",
      sensitivity_note = "Compatibility check using the retained-high-PM dataset. The two reviewed endline high-PM traces are now retained in the primary cleaned indoor PM2.5 dataset; PM values remain capped at 30000 ug/m3."
    )

  readr::write_csv(
    anomaly_retained_model_results,
    file.path(table_dir, "table_pm25_anomaly_retained_sensitivity_results.csv"),
    na = ""
  )
  readr::write_csv(
    bind_rows(
      primary_model_results %>%
        mutate(
          analysis_population = "primary_reviewed_high_pm_retained",
          sensitivity_note = "Primary cleaned indoor PM2.5 dataset retains the two manually reviewed endline high-PM traces; PM values remain capped at 30000 ug/m3."
        ),
      anomaly_retained_model_results %>%
        filter(model_label == paste0("anomaly_retained_", primary_model_label))
    ),
    file.path(table_dir, "table_pm25_primary_vs_anomaly_retained_sensitivity_results.csv"),
    na = ""
  )
}

message("Running rDiD analyses for ambient-excess indoor PM2.5 outcomes")
rdid_xvars <- c("hh_size", "hh_per_structure")

rdid_outcome_specs <- excess_specs %>%
  mutate(
    outcome_label = if_else(
      infiltration_factor_role == "default_tarp_wall_material_assumption",
      sprintf(
        "Default tarp-wall material assumption: time-weighted household mean indoor PM2.5 minus %.2f * concurrent ambient PM2.5",
        f_value
      ),
      sprintf(
        "Sensitivity analysis: time-weighted household mean indoor PM2.5 minus %.2f * concurrent ambient PM2.5",
        f_value
      )
    )
  )

rdid_excess_household <- primary_data %>%
  filter(!is.na(fcn_id), nzchar(fcn_id)) %>%
  mutate(
    fcn_id = as.character(fcn_id),
    study_arm_overall = as.character(study_arm_overall),
    timepoint = as.character(timepoint)
  ) %>%
  group_by(fcn_id, timepoint) %>%
  summarise(
    study_arm_overall = as.character(first_nonmissing(study_arm_overall)),
    collection_date_min = as.Date(min(start_datetime, na.rm = TRUE)),
    collection_date_max = as.Date(max(end_datetime, na.rm = TRUE)),
    n_windows = n(),
    n_monitor_files = n_distinct(raw_source_file),
    n_pm_observations = sum(n_obs_indoor, na.rm = TRUE),
    mean_ambient_coverage_prop = mean(ambient_all_coverage_prop, na.rm = TRUE),
    indoor_minus_ambient_material_default = weighted_mean_pm(indoor_minus_ambient_material_default, n_obs_indoor),
    indoor_minus_ambient_f025 = weighted_mean_pm(indoor_minus_ambient_f025, n_obs_indoor),
    indoor_minus_ambient_f050 = weighted_mean_pm(indoor_minus_ambient_f050, n_obs_indoor),
    indoor_minus_ambient_f075 = weighted_mean_pm(indoor_minus_ambient_f075, n_obs_indoor),
    indoor_minus_ambient_f100 = weighted_mean_pm(indoor_minus_ambient_f100, n_obs_indoor),
    .groups = "drop"
  ) %>%
  filter(timepoint %in% timepoint_levels, study_arm_overall %in% arm_levels) %>%
  mutate(
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = arm_levels)
  ) %>%
  arrange(timepoint, study_arm_overall, fcn_id)

readr::write_csv(
  rdid_excess_household,
  file.path(restricted_table_dir, "table_rDiD_pm25_panel_internal.csv"),
  na = ""
)
readr::write_csv(
  rdid_excess_household %>% select(-fcn_id, -collection_date_min, -collection_date_max),
  file.path(table_dir, "table_rDiD_pm25_panel_deidentified.csv"),
  na = ""
)

baseline_pm_arm <- rdid_excess_household %>%
  filter(timepoint == "baseline") %>%
  transmute(
    fcn_id,
    A_pm = case_when(
      study_arm_overall == "intervention" ~ 1,
      study_arm_overall == "comparison" ~ 0,
      TRUE ~ NA_real_
    ),
    baseline_pm_arm = study_arm_overall
  ) %>%
  group_by(fcn_id) %>%
  summarise(
    A_pm = as_number(first_nonmissing(A_pm)),
    baseline_pm_arm = as.character(first_nonmissing(baseline_pm_arm)),
    .groups = "drop"
  )

baseline_survey_covars <- survey_household %>%
  mutate(
    fcn_id = as.character(fcn_id),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  ) %>%
  filter(timepoint == "baseline", !is.na(fcn_id), nzchar(fcn_id)) %>%
  transmute(
    fcn_id,
    A_survey = case_when(
      study_arm_overall == "intervention" ~ 1,
      study_arm_overall == "comparison" ~ 0,
      TRUE ~ NA_real_
    ),
    baseline_survey_arm = study_arm_overall,
    hh_size = as_number(hh_size),
    hh_per_structure = as_number(hh_per_structure)
  ) %>%
  group_by(fcn_id) %>%
  summarise(
    A_survey = as_number(first_nonmissing(A_survey)),
    baseline_survey_arm = as.character(first_nonmissing(baseline_survey_arm)),
    hh_size = as_number(first_nonmissing(hh_size)),
    hh_per_structure = as_number(first_nonmissing(hh_per_structure)),
    .groups = "drop"
  )

rdid_baseline_covars <- baseline_survey_covars %>%
  full_join(baseline_pm_arm, by = "fcn_id") %>%
  mutate(
    A = dplyr::coalesce(A_survey, A_pm),
    treatment_source = case_when(
      !is.na(A_survey) ~ "baseline_survey_arm",
      is.na(A_survey) & !is.na(A_pm) ~ "baseline_pm_arm",
      TRUE ~ NA_character_
    ),
    treatment_arm = case_when(
      A == 1 ~ "intervention",
      A == 0 ~ "comparison",
      TRUE ~ NA_character_
    )
  )

rdid_covariate_missingness <- data.frame(
  measure = c("A", rdid_xvars),
  n_missing = c(
    sum(is.na(rdid_baseline_covars$A)),
    vapply(rdid_xvars, function(v) sum(is.na(rdid_baseline_covars[[v]])), integer(1))
  ),
  n_households = nrow(rdid_baseline_covars),
  stringsAsFactors = FALSE
)
readr::write_csv(
  rdid_covariate_missingness,
  file.path(table_dir, "table_rDiD_pm25_covariate_missingness.csv"),
  na = ""
)

make_rdid_excess_panel <- function(outcome_name, followup_timepoint) {
  baseline_y <- rdid_excess_household %>%
    filter(timepoint == "baseline") %>%
    transmute(
      fcn_id,
      Z = as_number(.data[[outcome_name]]),
      baseline_collection_date_min = collection_date_min,
      baseline_collection_date_max = collection_date_max,
      baseline_n_windows = n_windows,
      baseline_n_pm_observations = n_pm_observations
    )

  followup_y <- rdid_excess_household %>%
    filter(timepoint == followup_timepoint) %>%
    transmute(
      fcn_id,
      Y = as_number(.data[[outcome_name]]),
      followup_collection_date_min = collection_date_min,
      followup_collection_date_max = collection_date_max,
      followup_n_windows = n_windows,
      followup_n_pm_observations = n_pm_observations
    )

  rdid_baseline_covars %>%
    select(fcn_id, A, all_of(rdid_xvars), baseline_survey_arm, baseline_pm_arm, treatment_source) %>%
    inner_join(baseline_y, by = "fcn_id") %>%
    inner_join(followup_y, by = "fcn_id") %>%
    filter(!is.na(A), !is.na(Z), !is.na(Y))
}

make_rdid_panel_count <- function(outcome_info, followup_timepoint, contrast) {
  panel <- make_rdid_excess_panel(outcome_info$outcome, followup_timepoint)
  data.frame(
    contrast = contrast,
    followup_timepoint = followup_timepoint,
    outcome = outcome_info$outcome,
    outcome_label = outcome_info$outcome_label,
    infiltration_factor = outcome_info$f_value,
    infiltration_factor_role = outcome_info$infiltration_factor_role,
    n_households = nrow(panel),
    n_intervention = sum(panel$A == 1, na.rm = TRUE),
    n_comparison = sum(panel$A == 0, na.rm = TRUE),
    baseline_min_collection_date = if (nrow(panel) == 0) as.Date(NA) else min(panel$baseline_collection_date_min, na.rm = TRUE),
    baseline_max_collection_date = if (nrow(panel) == 0) as.Date(NA) else max(panel$baseline_collection_date_max, na.rm = TRUE),
    followup_min_collection_date = if (nrow(panel) == 0) as.Date(NA) else min(panel$followup_collection_date_min, na.rm = TRUE),
    followup_max_collection_date = if (nrow(panel) == 0) as.Date(NA) else max(panel$followup_collection_date_max, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

run_rdid_excess_contrast <- function(followup_timepoint, contrast, seed_offset) {
  bind_rows(lapply(seq_len(nrow(rdid_outcome_specs)), function(i) {
    outcome_info <- rdid_outcome_specs[i, , drop = FALSE]
    message("Running rDiD ", contrast, ": ", outcome_info$outcome)
    panel <- make_rdid_excess_panel(outcome_info$outcome, followup_timepoint)
    res_xgb <- rdid_dml_xgb(panel, rdid_xvars, seed = 202607 + seed_offset + i)
    res_glm <- rdid_glm_sensitivity(panel, rdid_xvars)

    bind_rows(
      format_rdid_result_row(
        res_xgb,
        outcome_info,
        contrast,
        followup_timepoint,
        "rDID_XGBoost",
        panel
      ),
      format_rdid_result_row(
        res_glm,
        outcome_info,
        contrast,
        followup_timepoint,
        "rDID_GLM_sensitivity",
        panel
      )
    )
  }))
}

rdid_panel_counts <- bind_rows(
  bind_rows(lapply(seq_len(nrow(rdid_outcome_specs)), function(i) {
    make_rdid_panel_count(rdid_outcome_specs[i, , drop = FALSE], "midline", "primary_baseline_midline")
  })),
  bind_rows(lapply(seq_len(nrow(rdid_outcome_specs)), function(i) {
    make_rdid_panel_count(rdid_outcome_specs[i, , drop = FALSE], "endline", "secondary_baseline_endline")
  }))
)
readr::write_csv(
  rdid_panel_counts,
  file.path(table_dir, "table_rDiD_pm25_panel_counts.csv"),
  na = ""
)

rdid_results <- bind_rows(
  run_rdid_excess_contrast("midline", "primary_baseline_midline", seed_offset = 10),
  run_rdid_excess_contrast("endline", "secondary_baseline_endline", seed_offset = 20)
) %>%
  arrange(contrast, outcome, estimator)

readr::write_csv(
  rdid_results,
  file.path(table_dir, "table_rDiD_pm25_reduction_results.csv"),
  na = ""
)
readr::write_csv(
  rdid_results %>% filter(estimator == "rDID_XGBoost"),
  file.path(table_dir, "table_rDiD_pm25_xgboost_results.csv"),
  na = ""
)
readr::write_csv(
  rdid_results %>% filter(estimator == "rDID_GLM_sensitivity"),
  file.path(table_dir, "table_rDiD_pm25_glm_sensitivity_results.csv"),
  na = ""
)
readr::write_csv(
  rdid_results %>% filter(infiltration_factor_role == "default_tarp_wall_material_assumption"),
  file.path(table_dir, "table_rDiD_pm25_default_reduction_results.csv"),
  na = ""
)
readr::write_csv(
  rdid_results %>% filter(infiltration_factor_role %in% c("sensitivity_lower_bound", "sensitivity_midpoint", "sensitivity_upper_bound")),
  file.path(table_dir, "table_rDiD_pm25_sensitivity_reduction_results.csv"),
  na = ""
)

rdid_anomaly_retained_results <- data.frame(stringsAsFactors = FALSE)
if (!is.null(analysis_data_anomaly_retained)) {
  message("Running retained high-PM compatibility/check rDiD PM2.5 analyses")
  rdid_excess_household_anomaly_retained <- primary_data_anomaly_retained %>%
    filter(!is.na(fcn_id), nzchar(fcn_id)) %>%
    mutate(
      fcn_id = as.character(fcn_id),
      study_arm_overall = as.character(study_arm_overall),
      timepoint = as.character(timepoint)
    ) %>%
    group_by(fcn_id, timepoint) %>%
    summarise(
      study_arm_overall = as.character(first_nonmissing(study_arm_overall)),
      collection_date_min = as.Date(min(start_datetime, na.rm = TRUE)),
      collection_date_max = as.Date(max(end_datetime, na.rm = TRUE)),
      n_windows = n(),
      n_monitor_files = n_distinct(raw_source_file),
      n_pm_observations = sum(n_obs_indoor, na.rm = TRUE),
      mean_ambient_coverage_prop = mean(ambient_all_coverage_prop, na.rm = TRUE),
      indoor_minus_ambient_material_default = weighted_mean_pm(indoor_minus_ambient_material_default, n_obs_indoor),
      indoor_minus_ambient_f025 = weighted_mean_pm(indoor_minus_ambient_f025, n_obs_indoor),
      indoor_minus_ambient_f050 = weighted_mean_pm(indoor_minus_ambient_f050, n_obs_indoor),
      indoor_minus_ambient_f075 = weighted_mean_pm(indoor_minus_ambient_f075, n_obs_indoor),
      indoor_minus_ambient_f100 = weighted_mean_pm(indoor_minus_ambient_f100, n_obs_indoor),
      .groups = "drop"
    ) %>%
    filter(timepoint %in% timepoint_levels, study_arm_overall %in% arm_levels) %>%
    mutate(
      timepoint = as_ordered_timepoint(timepoint),
      study_arm_overall = factor(study_arm_overall, levels = arm_levels)
    ) %>%
    arrange(timepoint, study_arm_overall, fcn_id)

  readr::write_csv(
    rdid_excess_household_anomaly_retained,
    file.path(restricted_table_dir, "table_rDiD_pm25_panel_anomaly_retained_internal.csv"),
    na = ""
  )
  readr::write_csv(
    rdid_excess_household_anomaly_retained %>% select(-fcn_id, -collection_date_min, -collection_date_max),
    file.path(table_dir, "table_rDiD_pm25_panel_anomaly_retained_deidentified.csv"),
    na = ""
  )

  make_rdid_excess_panel_anomaly_retained <- function(outcome_name, followup_timepoint) {
    baseline_y <- rdid_excess_household_anomaly_retained %>%
      filter(timepoint == "baseline") %>%
      transmute(
        fcn_id,
        Z = as_number(.data[[outcome_name]]),
        baseline_collection_date_min = collection_date_min,
        baseline_collection_date_max = collection_date_max
      )

    followup_y <- rdid_excess_household_anomaly_retained %>%
      filter(timepoint == followup_timepoint) %>%
      transmute(
        fcn_id,
        Y = as_number(.data[[outcome_name]]),
        followup_collection_date_min = collection_date_min,
        followup_collection_date_max = collection_date_max
      )

    rdid_baseline_covars %>%
      select(fcn_id, A, all_of(rdid_xvars), baseline_survey_arm, baseline_pm_arm, treatment_source) %>%
      inner_join(baseline_y, by = "fcn_id") %>%
      inner_join(followup_y, by = "fcn_id") %>%
      filter(!is.na(A), !is.na(Z), !is.na(Y))
  }

  make_rdid_panel_count_anomaly_retained <- function(outcome_info, followup_timepoint, contrast) {
    panel <- make_rdid_excess_panel_anomaly_retained(outcome_info$outcome, followup_timepoint)
    data.frame(
      contrast = contrast,
      followup_timepoint = followup_timepoint,
      outcome = outcome_info$outcome,
      outcome_label = outcome_info$outcome_label,
      infiltration_factor = outcome_info$f_value,
      infiltration_factor_role = outcome_info$infiltration_factor_role,
      n_households = nrow(panel),
      n_intervention = sum(panel$A == 1, na.rm = TRUE),
      n_comparison = sum(panel$A == 0, na.rm = TRUE),
      baseline_min_collection_date = if (nrow(panel) == 0) as.Date(NA) else min(panel$baseline_collection_date_min, na.rm = TRUE),
      baseline_max_collection_date = if (nrow(panel) == 0) as.Date(NA) else max(panel$baseline_collection_date_max, na.rm = TRUE),
      followup_min_collection_date = if (nrow(panel) == 0) as.Date(NA) else min(panel$followup_collection_date_min, na.rm = TRUE),
      followup_max_collection_date = if (nrow(panel) == 0) as.Date(NA) else max(panel$followup_collection_date_max, na.rm = TRUE),
      analysis_population = "anomaly_retained_sensitivity",
      stringsAsFactors = FALSE
    )
  }

  run_rdid_excess_contrast_anomaly_retained <- function(followup_timepoint, contrast, seed_offset) {
    bind_rows(lapply(seq_len(nrow(rdid_outcome_specs)), function(i) {
      outcome_info <- rdid_outcome_specs[i, , drop = FALSE]
      message("Running retained high-PM check rDiD ", contrast, ": ", outcome_info$outcome)
      panel <- make_rdid_excess_panel_anomaly_retained(outcome_info$outcome, followup_timepoint)
      res_xgb <- rdid_dml_xgb(panel, rdid_xvars, seed = 202607 + seed_offset + i)
      res_glm <- rdid_glm_sensitivity(panel, rdid_xvars)

      bind_rows(
        format_rdid_result_row(
          res_xgb,
          outcome_info,
          contrast,
          followup_timepoint,
          "rDID_XGBoost",
          panel
        ),
        format_rdid_result_row(
          res_glm,
          outcome_info,
          contrast,
          followup_timepoint,
          "rDID_GLM_sensitivity",
          panel
        )
      )
    }))
  }

  rdid_anomaly_retained_panel_counts <- bind_rows(
    bind_rows(lapply(seq_len(nrow(rdid_outcome_specs)), function(i) {
      make_rdid_panel_count_anomaly_retained(rdid_outcome_specs[i, , drop = FALSE], "midline", "primary_baseline_midline")
    })),
    bind_rows(lapply(seq_len(nrow(rdid_outcome_specs)), function(i) {
      make_rdid_panel_count_anomaly_retained(rdid_outcome_specs[i, , drop = FALSE], "endline", "secondary_baseline_endline")
    }))
  )
  readr::write_csv(
    rdid_anomaly_retained_panel_counts,
    file.path(table_dir, "table_rDiD_pm25_panel_counts_anomaly_retained_sensitivity.csv"),
    na = ""
  )

  rdid_anomaly_retained_results <- bind_rows(
    run_rdid_excess_contrast_anomaly_retained("midline", "primary_baseline_midline", seed_offset = 110),
    run_rdid_excess_contrast_anomaly_retained("endline", "secondary_baseline_endline", seed_offset = 120)
  ) %>%
    mutate(
      analysis_population = "anomaly_retained_sensitivity",
      sensitivity_note = "Compatibility check using the retained-high-PM dataset. The two reviewed endline high-PM traces are now retained in the primary cleaned indoor PM2.5 dataset; PM values remain capped at 30000 ug/m3."
    ) %>%
    arrange(contrast, outcome, estimator)

  readr::write_csv(
    rdid_anomaly_retained_results,
    file.path(table_dir, "table_rDiD_pm25_anomaly_retained_reduction_results.csv"),
    na = ""
  )
  readr::write_csv(
    bind_rows(
      rdid_results %>%
        mutate(
          analysis_population = "primary_reviewed_high_pm_retained",
          sensitivity_note = "Primary cleaned indoor PM2.5 dataset retains the two manually reviewed endline high-PM traces; PM values remain capped at 30000 ug/m3."
        ),
      rdid_anomaly_retained_results
    ),
    file.path(table_dir, "table_rDiD_pm25_primary_vs_anomaly_retained_reduction_results.csv"),
    na = ""
  )
}

rdid_package_versions <- data.frame(
  package = c("R", "xgboost", "dplyr", "lme4"),
  version = c(
    as.character(getRversion()),
    as.character(utils::packageVersion("xgboost")),
    as.character(utils::packageVersion("dplyr")),
    as.character(utils::packageVersion("lme4"))
  ),
  stringsAsFactors = FALSE
)
readr::write_csv(
  rdid_package_versions,
  file.path(table_dir, "table_rDiD_pm25_package_versions.csv"),
  na = ""
)

rdid_xgboost_tuning_settings <- data.frame(
  parameter = c("max_nrounds", "early_stopping_rounds", "nthread", "max_depth_grid", "eta_grid"),
  value = c(
    Sys.getenv("PM25_XGB_MAX_NROUNDS", unset = "100"),
    Sys.getenv("PM25_XGB_EARLY_STOP", unset = "10"),
    Sys.getenv("PM25_XGB_NTHREAD", unset = "2"),
    Sys.getenv("PM25_XGB_DEPTHS", unset = "2"),
    Sys.getenv("PM25_XGB_ETAS", unset = "0.05")
  ),
  stringsAsFactors = FALSE
)
readr::write_csv(
  rdid_xgboost_tuning_settings,
  file.path(table_dir, "table_rDiD_pm25_xgboost_tuning.csv"),
  na = ""
)

message("Evaluating caregiver and child time-weighted PM2.5 exposure")
household_key <- survey_household %>%
  transmute(
    KEY = as.character(KEY),
    fcn_id = as.character(fcn_id),
    hh_id = as.character(hh_id),
    timepoint_household = as.character(timepoint),
    study_arm_overall_household = as.character(study_arm_overall),
    respondent_sl = as.character(respondent_sl)
  )

member_key <- survey_hh_members %>%
  transmute(
    PARENT_KEY = as.character(PARENT_KEY),
    mem_serial = as.character(mem_serial),
    member_age_yrs = clean_age_years(age_yrs),
    member_hours_outside = clean_hours_0_24(hours_outside)
  )

baseline_caregiver_hours <- survey_household %>%
  mutate(
    fcn_id = as.character(fcn_id),
    hh_id = as.character(hh_id),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    hours_outside_clean = clean_hours_0_24(hours_outside)
  ) %>%
  filter(timepoint == "baseline", study_arm_overall %in% arm_levels, !is.na(fcn_id), !is.na(hours_outside_clean)) %>%
  transmute(
    fcn_id,
    hh_id,
    timepoint,
    study_arm_overall,
    individual = "caregiver",
    hours_outside = hours_outside_clean,
    time_outside_source = "baseline_household_hours_outside"
  )

baseline_child_hours <- survey_household %>%
  mutate(
    fcn_id = as.character(fcn_id),
    hh_id = as.character(hh_id),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    hours_outside_clean = clean_hours_0_24(target_child_hours_outside)
  ) %>%
  filter(timepoint == "baseline", study_arm_overall %in% arm_levels, !is.na(fcn_id), !is.na(hours_outside_clean)) %>%
  transmute(
    fcn_id,
    hh_id,
    timepoint,
    study_arm_overall,
    individual = "child_under5",
    hours_outside = hours_outside_clean,
    time_outside_source = "baseline_household_target_child_hours_outside"
  )

midline_location_hours <- survey_location %>%
  mutate(
    PARENT_KEY = as.character(PARENT_KEY),
    location_number = as.character(location_number),
    timepoint = as.character(timepoint),
    hours_inside_clean = clean_hours_0_24(hours_inside),
    hours_outside_clean = clean_hours_0_24(hours_outside),
    hours_outside_clean = coalesce(hours_outside_clean, if_else(!is.na(hours_inside_clean), 24 - hours_inside_clean, NA_real_)),
    hours_inside_clean = coalesce(hours_inside_clean, if_else(!is.na(hours_outside_clean), 24 - hours_outside_clean, NA_real_))
  ) %>%
  left_join(
    household_key %>%
      rename(fcn_id_household = fcn_id, hh_id_household = hh_id),
    by = c("PARENT_KEY" = "KEY")
  ) %>%
  left_join(
    member_key %>% select(PARENT_KEY, mem_serial, member_age_yrs),
    by = c("PARENT_KEY", "location_number" = "mem_serial")
  ) %>%
  mutate(
    fcn_id = coalesce(as.character(fcn_id), as.character(fcn_id_household)),
    hh_id = coalesce(as.character(hh_id), as.character(hh_id_household)),
    timepoint = coalesce(as.character(timepoint), as.character(timepoint_household)),
    study_arm_overall = coalesce(study_arm_overall_household, as.character(study_arm_overall)),
    is_caregiver = !is.na(respondent_sl) & location_number == respondent_sl,
    is_child_under5 = !is.na(member_age_yrs) & member_age_yrs < 5
  )

midline_caregiver_hours <- midline_location_hours %>%
  filter(timepoint == "midline", study_arm_overall %in% arm_levels, is_caregiver, !is.na(hours_outside_clean)) %>%
  group_by(fcn_id, hh_id, timepoint, study_arm_overall) %>%
  summarise(hours_outside = mean_or_na(hours_outside_clean), .groups = "drop") %>%
  mutate(
    individual = "caregiver",
    time_outside_source = "midline_location_repeat_respondent_serial"
  )

midline_child_hours <- midline_location_hours %>%
  filter(timepoint == "midline", study_arm_overall %in% arm_levels, is_child_under5, !is.na(hours_outside_clean)) %>%
  group_by(fcn_id, hh_id, timepoint, study_arm_overall) %>%
  summarise(hours_outside = mean_or_na(hours_outside_clean), n_children_under5_with_hours = n(), .groups = "drop") %>%
  mutate(
    individual = "child_under5",
    time_outside_source = "midline_location_repeat_children_under5"
  ) %>%
  select(-n_children_under5_with_hours)

endline_member_hours <- survey_hh_members %>%
  mutate(
    PARENT_KEY = as.character(PARENT_KEY),
    mem_serial = as.character(mem_serial),
    timepoint = as.character(timepoint),
    member_age_yrs = clean_age_years(age_yrs),
    hours_outside_clean = clean_hours_0_24(hours_outside)
  ) %>%
  left_join(
    household_key %>%
      rename(fcn_id_household = fcn_id, hh_id_household = hh_id),
    by = c("PARENT_KEY" = "KEY")
  ) %>%
  mutate(
    fcn_id = coalesce(as.character(fcn_id), as.character(fcn_id_household)),
    hh_id = coalesce(as.character(hh_id), as.character(hh_id_household)),
    timepoint = coalesce(as.character(timepoint), as.character(timepoint_household)),
    study_arm_overall = coalesce(study_arm_overall_household, as.character(study_arm_overall)),
    is_caregiver = !is.na(respondent_sl) & mem_serial == respondent_sl,
    is_child_under5 = !is.na(member_age_yrs) & member_age_yrs < 5
  )

endline_caregiver_hours <- endline_member_hours %>%
  filter(timepoint == "endline", study_arm_overall %in% arm_levels, is_caregiver, !is.na(hours_outside_clean)) %>%
  group_by(fcn_id, hh_id, timepoint, study_arm_overall) %>%
  summarise(hours_outside = mean_or_na(hours_outside_clean), .groups = "drop") %>%
  mutate(
    individual = "caregiver",
    time_outside_source = "endline_hh_members_respondent_serial"
  )

endline_child_hours <- endline_member_hours %>%
  filter(timepoint == "endline", study_arm_overall %in% arm_levels, is_child_under5, !is.na(hours_outside_clean)) %>%
  group_by(fcn_id, hh_id, timepoint, study_arm_overall) %>%
  summarise(hours_outside = mean_or_na(hours_outside_clean), n_children_under5_with_hours = n(), .groups = "drop") %>%
  mutate(
    individual = "child_under5",
    time_outside_source = "endline_hh_members_children_under5"
  ) %>%
  select(-n_children_under5_with_hours)

individual_hours_household <- bind_rows(
  baseline_caregiver_hours,
  baseline_child_hours,
  midline_caregiver_hours,
  midline_child_hours,
  endline_caregiver_hours,
  endline_child_hours
) %>%
  filter(!is.na(hours_outside), is.finite(hours_outside), hours_outside >= 0, hours_outside <= 24) %>%
  mutate(
    hours_inside = 24 - hours_outside,
    pct_time_inside = hours_inside / 24 * 100,
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = arm_levels)
  )

readr::write_csv(
  individual_hours_household,
  file.path(restricted_table_dir, "table_descriptive_pm25_hours_internal.csv"),
  na = ""
)
readr::write_csv(
  individual_hours_household %>% select(-fcn_id, -hh_id),
  file.path(table_dir, "table_descriptive_pm25_hours_deidentified.csv"),
  na = ""
)

hours_outside_summary <- individual_hours_household %>%
  group_by(timepoint, study_arm_overall, individual) %>%
  summarise(
    n_households = n_distinct(fcn_id),
    mean_hr_outside_hh = mean(hours_outside, na.rm = TRUE),
    sd_hr_outside_hh = sd(hours_outside, na.rm = TRUE),
    mean_hr_inside_hh = mean(hours_inside, na.rm = TRUE),
    sd_hr_inside_hh = sd(hours_inside, na.rm = TRUE),
    pct_time_inside = mean(pct_time_inside, na.rm = TRUE),
    time_outside_sources = paste(sort(unique(time_outside_source)), collapse = "; "),
    .groups = "drop"
  ) %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  )
readr::write_csv(
  hours_outside_summary,
  file.path(table_dir, "table_descriptive_pm25_hours_summary.csv"),
  na = ""
)

pm_time_weight_inputs <- analysis_data %>%
  filter(has_concurrent_ambient) %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_pm_households = n_distinct(hh_id),
    n_pm_windows = n(),
    mean_pm2_5_indoor_hh = mean(indoor_mean_pm, na.rm = TRUE),
    sd_pm2_5_indoor_hh = sd(indoor_mean_pm, na.rm = TRUE),
    se_pm2_5_indoor_hh = sd_pm2_5_indoor_hh / sqrt(n_pm_windows),
    lower_pm2_5_indoor_hh = mean_pm2_5_indoor_hh - 1.96 * se_pm2_5_indoor_hh,
    upper_pm2_5_indoor_hh = mean_pm2_5_indoor_hh + 1.96 * se_pm2_5_indoor_hh,
    mean_pm2_5_outdoor_concurrent = mean(ambient_all_mean_pm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  )

round_ambient_pm <- ambient_clean %>%
  filter(timepoint %in% timepoint_levels) %>%
  group_by(timepoint) %>%
  summarise(
    mean_pm2_5_outdoor_round = mean(pm25_ug_m3, na.rm = TRUE),
    n_ambient_rows_round = n(),
    .groups = "drop"
  ) %>%
  mutate(timepoint = as.character(timepoint))

pm_time_weight_inputs <- pm_time_weight_inputs %>%
  left_join(round_ambient_pm, by = "timepoint") %>%
  mutate(
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = arm_levels),
    mean_pm2_5_outdoor_for_weighting = coalesce(mean_pm2_5_outdoor_concurrent, mean_pm2_5_outdoor_round),
    outdoor_pm_source = if_else(!is.na(mean_pm2_5_outdoor_concurrent), "concurrent_household_window_ambient", "round_ambient_mean")
  ) %>%
  arrange(timepoint, study_arm_overall)

readr::write_csv(
  pm_time_weight_inputs,
  file.path(table_dir, "table_descriptive_pm25_input_summary.csv"),
  na = ""
)

time_weighted_exposure <- hours_outside_summary %>%
  left_join(pm_time_weight_inputs, by = c("timepoint", "study_arm_overall")) %>%
  mutate(
    weighted_mean_pm2_5 = mean_pm2_5_indoor_hh * (mean_hr_inside_hh / 24) +
      mean_pm2_5_outdoor_for_weighting * (mean_hr_outside_hh / 24),
    weighted_mean_pm2_5_ambient_excess_default = weighted_mean_pm2_5 -
      default_material_infiltration_factor * mean_pm2_5_outdoor_for_weighting,
    infiltration_factor_for_excess = default_material_infiltration_factor,
    exposure_note = "Group-level time-weighted exposure using mean hours inside/outside and arm-timepoint mean household indoor PM2.5 with concurrent ambient PM2.5. Child rows use target-child hours at baseline and all children under 5 in member/location repeats at midline/endline."
  ) %>%
  arrange(individual, match(timepoint, timepoint_levels), study_arm_overall)

readr::write_csv(
  time_weighted_exposure,
  file.path(table_dir, "table_descriptive_pm25_caregiver_child.csv"),
  na = ""
)

caregiver_weighted <- time_weighted_exposure %>%
  filter(individual == "caregiver") %>%
  select(
    timepoint,
    study_arm_overall,
    caregiver_weighted_mean_pm2_5 = weighted_mean_pm2_5,
    caregiver_pct_time_inside = pct_time_inside
  )
child_weighted <- time_weighted_exposure %>%
  filter(individual == "child_under5") %>%
  select(
    timepoint,
    study_arm_overall,
    child_weighted_mean_pm2_5 = weighted_mean_pm2_5,
    child_pct_time_inside = pct_time_inside
  )
time_weighted_exposure_ratios <- child_weighted %>%
  left_join(caregiver_weighted, by = c("timepoint", "study_arm_overall")) %>%
  mutate(
    child_caregiver_weighted_exposure_ratio = child_weighted_mean_pm2_5 / caregiver_weighted_mean_pm2_5,
    child_caregiver_pct_inside_ratio = child_pct_time_inside / caregiver_pct_time_inside
  )
readr::write_csv(
  time_weighted_exposure_ratios,
  file.path(table_dir, "table_descriptive_pm25_child_caregiver_ratios.csv"),
  na = ""
)

try({
  p_time_weighted <- time_weighted_exposure %>%
    ggplot(aes(x = timepoint, y = weighted_mean_pm2_5, color = study_arm_overall, group = study_arm_overall)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 2) +
    facet_wrap(~individual) +
    labs(
      x = NULL,
      y = "Time-weighted PM2.5 (ug/m3)",
      color = "Study arm",
      title = "Caregiver and Child Time-Weighted PM2.5 Exposure"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(
    file.path(figure_dir, "fig_descriptive_pm25_time_weighted_exposure.png"),
    p_time_weighted,
    width = 8,
    height = 5,
    dpi = 300
  )
}, silent = TRUE)
message("Writing diagnostic figures")
try({
  p_timeline <- analysis_data %>%
    ggplot(aes(x = midpoint_datetime, y = study_arm_overall, color = study_arm_overall)) +
    geom_point(position = position_jitter(height = 0.12, width = 0), alpha = 0.75, size = 1.8) +
    facet_wrap(~timepoint, scales = "free_x") +
    labs(
      x = "Household monitoring window midpoint",
      y = NULL,
      color = "Study arm",
      title = "PM2.5 Household Monitoring Timing by Arm"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(
    file.path(figure_dir, "fig_descriptive_pm25_sampling_timeline.png"),
    p_timeline,
    width = 10,
    height = 5,
    dpi = 300
  )
}, silent = TRUE)

try({
  p_indoor_ambient <- analysis_data %>%
    filter(has_concurrent_ambient) %>%
    ggplot(aes(x = midpoint_datetime)) +
    geom_point(aes(y = indoor_gmean_pm, color = study_arm_overall), alpha = 0.75, size = 1.8) +
    geom_point(aes(y = ambient_all_gmean_pm), color = "black", alpha = 0.45, size = 1.4, shape = 17) +
    facet_wrap(~timepoint, scales = "free_x") +
    scale_y_log10() +
    labs(
      x = "Household monitoring window midpoint",
      y = "Geometric mean PM2.5 (ug/m3, log scale)",
      color = "Indoor study arm",
      title = "Indoor PM2.5 and Concurrent Ambient PM2.5"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(
    file.path(figure_dir, "fig_descriptive_pm25_ambient_by_date.png"),
    p_indoor_ambient,
    width = 10,
    height = 5,
    dpi = 300
  )
}, silent = TRUE)

try({
  p_primary <- primary_model_results %>%
    filter(fit_status == "fit") %>%
    ggplot(aes(x = timepoint, y = percent_difference)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    geom_pointrange(aes(ymin = percent_difference_conf_low, ymax = percent_difference_conf_high), size = 0.5) +
    labs(
      x = NULL,
      y = "Percent difference, intervention vs comparison",
      title = "Ambient- and Calendar-Adjusted Indoor PM2.5 Contrast"
    ) +
    theme_minimal(base_size = 11)
  ggsave(
    file.path(figure_dir, "fig_model_pm25_adjusted_contrasts.png"),
    p_primary,
    width = 7,
    height = 4.5,
    dpi = 300
  )
}, silent = TRUE)

run_manifest <- data.frame(
  field = c(
    "analysis_date",
    "project_root",
    "input_indoor_path",
    "input_indoor_anomaly_retained_path",
    "input_ambient_path",
    "input_survey_household_path",
    "input_survey_location_path",
    "input_survey_hh_members_path",
    "table_dir",
    "figure_dir",
    "n_indoor_minute_rows",
    "n_ambient_minute_rows",
    "n_household_windows",
    "n_monitoring_coverage_windows",
    "n_monitoring_coverage_windows_valid_first_24h_75pct",
    "n_monitoring_coverage_windows_valid_second_24h_75pct",
    "monitoring_interval_modal_seconds",
    "monitoring_intervals_all_positive_same",
    "expected_monitoring_interval_seconds",
    "n_within_window_long_intervals_gt_60sec",
    "n_naive_household_long_intervals_gt_60sec",
    "n_naive_household_long_intervals_cross_study_timepoint",
    "n_household_windows_with_concurrent_ambient",
    "n_household_windows_common_support",
    "n_anomaly_retained_sensitivity_windows",
    "n_anomaly_retained_sensitivity_model_rows",
    "n_anomaly_retained_sensitivity_rdid_rows",
    "n_time_weighted_exposure_rows",
    "n_time_weighted_exposure_household_hours_rows",
    "primary_model_label"
  ),
  value = c(
    analysis_date,
    project_root,
    input_indoor_path,
    if (file.exists(input_indoor_anomaly_retained_path)) input_indoor_anomaly_retained_path else "",
    input_ambient_path,
    input_survey_household_path,
    input_survey_location_path,
    input_survey_hh_members_path,
    table_dir,
    figure_dir,
    as.character(nrow(indoor)),
    as.character(nrow(ambient)),
    as.character(nrow(analysis_data)),
    as.character(nrow(household_monitoring_coverage)),
    as.character(sum(household_monitoring_coverage$has_valid_75pct_24h_1, na.rm = TRUE)),
    as.character(sum(household_monitoring_coverage$has_valid_75pct_24h_2, na.rm = TRUE)),
    as.character(global_modal_interval_seconds),
    as.character(monitoring_interval_summary$all_positive_intervals_same[monitoring_interval_summary$timepoint == "all" & monitoring_interval_summary$study_arm_overall == "all"][[1]]),
    as.character(expected_monitoring_interval_seconds),
    as.character(nrow(long_interval_within_window_details)),
    as.character(nrow(naive_household_long_interval_details)),
    as.character(sum(naive_household_long_interval_details$crosses_study_timepoint, na.rm = TRUE)),
    as.character(sum(analysis_data$has_concurrent_ambient, na.rm = TRUE)),
    as.character(sum(analysis_data$in_common_support, na.rm = TRUE)),
    as.character(if (!is.null(analysis_data_anomaly_retained)) nrow(analysis_data_anomaly_retained) else 0L),
    as.character(nrow(anomaly_retained_model_results)),
    as.character(nrow(rdid_anomaly_retained_results)),
    as.character(nrow(time_weighted_exposure)),
    as.character(nrow(individual_hours_household)),
    primary_model_label
  ),
  stringsAsFactors = FALSE
)
readr::write_csv(run_manifest, file.path(table_dir, "table_pm25_run_manifest.csv"), na = "")

message("Done.")
message("Tables: ", normalizePath(table_dir, winslash = "/", mustWork = TRUE))
message("Figures: ", normalizePath(figure_dir, winslash = "/", mustWork = TRUE))








