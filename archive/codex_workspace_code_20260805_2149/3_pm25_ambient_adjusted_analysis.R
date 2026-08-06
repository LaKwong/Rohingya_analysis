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
#
# Outputs:
#   7_tables/pm25_ambient_adjusted_<YYYYMMDD>/
#   6_figures/pm25_ambient_adjusted_<YYYYMMDD>/

options(stringsAsFactors = FALSE)

required_packages <- c("dplyr", "readr", "lubridate", "ggplot2", "lme4", "splines")
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

indoor <- readRDS(input_indoor_path)
ambient <- readRDS(input_ambient_path)

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
    pct_obs_gt_35 = mean(pm25_ug_m3 > 35, na.rm = TRUE) * 100,
    pct_obs_gt_75 = mean(pm25_ug_m3 > 75, na.rm = TRUE) * 100,
    pct_obs_gt_150 = mean(pm25_ug_m3 > 150, na.rm = TRUE) * 100,
    pct_obs_gt_400 = mean(pm25_ug_m3 > 400, na.rm = TRUE) * 100,
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
analysis_internal_path <- file.path(table_dir, "pm25_household_window_analysis_dataset_internal.csv")
analysis_deidentified_path <- file.path(table_dir, "pm25_household_window_analysis_dataset_deidentified.csv")

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
  file.path(table_dir, "pm25_household_window_counts_by_arm_timepoint.csv")
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
  file.path(table_dir, "pm25_ambient_matching_diagnostics_by_arm_timepoint.csv")
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
  file.path(table_dir, "pm25_sampling_period_diagnostics_by_arm_timepoint.csv")
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
  file.path(table_dir, "pm25_ambient_period_diagnostics.csv"),
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
  file.path(table_dir, "pm25_common_support_diagnostics_by_arm_timepoint.csv")
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
  f_value = c(0.25, 0.50, 0.75, 1.00),
  outcome = c(
    "indoor_minus_ambient_f025",
    "indoor_minus_ambient_f050",
    "indoor_minus_ambient_f075",
    "indoor_minus_ambient_f100"
  ),
  stringsAsFactors = FALSE
)

excess_results <- bind_rows(lapply(seq_len(nrow(excess_specs)), function(i) {
  fit_arm_contrast_model(
    data = primary_data,
    outcome = excess_specs$outcome[[i]],
    model_label = sprintf("absolute_excess_F_%0.2f_calendar_lmer", excess_specs$f_value[[i]]),
    adjustment_terms = character(),
    date_adjust = TRUE,
    outcome_scale = "absolute_pm"
  ) %>%
    mutate(infiltration_factor = excess_specs$f_value[[i]])
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
  file.path(table_dir, "pm25_ambient_adjusted_model_and_sensitivity_results.csv"),
  na = ""
)

primary_model_results <- model_results %>%
  filter(model_label == primary_model_label)
readr::write_csv(
  primary_model_results,
  file.path(table_dir, "pm25_primary_ambient_adjusted_model_results.csv"),
  na = ""
)

sensitivity_results <- model_results %>%
  filter(model_label != primary_model_label)
readr::write_csv(
  sensitivity_results,
  file.path(table_dir, "pm25_sensitivity_analysis_results.csv"),
  na = ""
)

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
    file.path(figure_dir, "pm25_sampling_timeline_by_arm_timepoint.png"),
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
    file.path(figure_dir, "pm25_indoor_and_concurrent_ambient_by_date.png"),
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
    file.path(figure_dir, "pm25_primary_adjusted_contrasts.png"),
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
    "table_dir",
    "figure_dir",
    "n_indoor_minute_rows",
    "n_ambient_minute_rows",
    "n_household_windows",
    "n_household_windows_with_concurrent_ambient",
    "n_household_windows_common_support",
    "primary_model_label"
  ),
  value = c(
    analysis_date,
    project_root,
    input_indoor_path,
    input_ambient_path,
    table_dir,
    figure_dir,
    as.character(nrow(indoor)),
    as.character(nrow(ambient)),
    as.character(nrow(analysis_data)),
    as.character(sum(analysis_data$has_concurrent_ambient, na.rm = TRUE)),
    as.character(sum(analysis_data$in_common_support, na.rm = TRUE)),
    primary_model_label
  ),
  stringsAsFactors = FALSE
)
readr::write_csv(run_manifest, file.path(table_dir, "pm25_ambient_adjusted_run_manifest.csv"), na = "")

message("Done.")
message("Tables: ", normalizePath(table_dir, winslash = "/", mustWork = TRUE))
message("Figures: ", normalizePath(figure_dir, winslash = "/", mustWork = TRUE))
