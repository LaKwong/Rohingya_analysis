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
#   6_figures/pm25_ambient_adjusted_<YYYYMMDD>/

options(stringsAsFactors = FALSE)

required_packages <- c("dplyr", "readr", "lubridate", "ggplot2", "lme4", "splines", "xgboost")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Install required package(s) before running this script: ",
    paste(missing_packages, collapse = ", "),
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
primary_model_label <- "primary_log_indoor_ambient_calendar_lmer"
default_material_infiltration_factor <- 0.75
sensitivity_infiltration_factors <- c(0.25, 0.50, 1.00)

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
input_ambient_path <- file.path(clean_final_dir, "pm25_pats_refugee_ambient.rds")
input_survey_household_path <- file.path(clean_final_dir, "survey_refugee_household.rds")
input_survey_location_path <- file.path(clean_final_dir, "survey_refugee_location.rds")
input_survey_hh_members_path <- file.path(clean_final_dir, "survey_refugee_hh_members.rds")

analysis_date <- format(Sys.Date(), "%Y%m%d")
table_dir <- file.path(project_root, "7_tables", paste0("pm25_ambient_adjusted_", analysis_date))
figure_dir <- file.path(project_root, "6_figures", paste0("pm25_ambient_adjusted_", analysis_date))
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

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

indoor <- indoor %>%
  mutate(
    dateTime = as_clean_datetime(dateTime),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    hh_id = as.character(hh_id),
    fcn_id = as.character(fcn_id),
    hh_id_note = as.character(hh_id_note),
    raw_source_file = as.character(raw_source_file),
    PM_monitor = as.character(PM_monitor),
    pm25_ug_m3 = as.numeric(pm25_ug_m3)
  )

ambient <- ambient %>%
  mutate(
    dateTime = as_clean_datetime(dateTime),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    ambient_site_id = as.character(ambient_site_id),
    note_clean = as.character(note_clean),
    raw_source_file = as.character(raw_source_file),
    PM_monitor = as.character(PM_monitor),
    pm25_ug_m3 = as.numeric(pm25_ug_m3)
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
    timepoint = factor(timepoint, levels = timepoint_levels),
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
    timepoint = factor(timepoint, levels = timepoint_levels),
    study_arm_overall = factor(study_arm_overall, levels = arm_levels)
  )

round_start_dates <- analysis_data %>%
  group_by(timepoint) %>%
  summarise(round_start_day_num = min(midpoint_day_num, na.rm = TRUE), .groups = "drop")

analysis_data <- analysis_data %>%
  left_join(round_start_dates, by = "timepoint") %>%
  mutate(days_since_round_start = midpoint_day_num - round_start_day_num)

message("Writing analysis-ready datasets and diagnostics")
analysis_internal_path <- file.path(table_dir, "table_pm25_window_dataset_internal.csv")
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
      timepoint = factor(as.character(timepoint), levels = timepoint_levels),
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
  filter(timepoint %in% timepoint_levels, study_arm_overall %in% arm_levels)

readr::write_csv(
  rdid_excess_household,
  file.path(table_dir, "table_rDiD_pm25_panel_internal.csv"),
  na = ""
)
readr::write_csv(
  rdid_excess_household %>% select(-fcn_id),
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
  left_join(household_key, by = c("PARENT_KEY" = "KEY")) %>%
  left_join(
    member_key %>% select(PARENT_KEY, mem_serial, member_age_yrs),
    by = c("PARENT_KEY", "location_number" = "mem_serial")
  ) %>%
  mutate(
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
  left_join(household_key, by = c("PARENT_KEY" = "KEY")) %>%
  mutate(
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
    timepoint = factor(timepoint, levels = timepoint_levels),
    study_arm_overall = factor(study_arm_overall, levels = arm_levels)
  )

readr::write_csv(
  individual_hours_household,
  file.path(table_dir, "table_descriptive_pm25_hours_internal.csv"),
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
    mean_pm2_5_outdoor_for_weighting = coalesce(mean_pm2_5_outdoor_concurrent, mean_pm2_5_outdoor_round),
    outdoor_pm_source = if_else(!is.na(mean_pm2_5_outdoor_concurrent), "concurrent_household_window_ambient", "round_ambient_mean")
  )

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
    "input_ambient_path",
    "input_survey_household_path",
    "input_survey_location_path",
    "input_survey_hh_members_path",
    "table_dir",
    "figure_dir",
    "n_indoor_minute_rows",
    "n_ambient_minute_rows",
    "n_household_windows",
    "n_household_windows_with_concurrent_ambient",
    "n_household_windows_common_support",
    "n_time_weighted_exposure_rows",
    "n_time_weighted_exposure_household_hours_rows",
    "primary_model_label"
  ),
  value = c(
    analysis_date,
    project_root,
    input_indoor_path,
    input_ambient_path,
    input_survey_household_path,
    input_survey_location_path,
    input_survey_hh_members_path,
    table_dir,
    figure_dir,
    as.character(nrow(indoor)),
    as.character(nrow(ambient)),
    as.character(nrow(analysis_data)),
    as.character(sum(analysis_data$has_concurrent_ambient, na.rm = TRUE)),
    as.character(sum(analysis_data$in_common_support, na.rm = TRUE)),
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







