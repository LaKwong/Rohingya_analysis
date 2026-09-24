################################################################################
# Deterministic checks for the unified RF105 PM2.5 pipeline
################################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

find_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "Rohingya_analysis.Rproj"))) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) stop("Could not locate Rohingya_analysis.Rproj.", call. = FALSE)
    path <- parent
  }
}

root <- find_project_root()
date_stamp <- Sys.getenv("RF105_TEST_DATE", unset = format(Sys.Date(), "%Y%m%d"))
reviewed_dir <- file.path(root, "5_analysis_RF105", "reviewed")
table_dir <- file.path(root, "7_tables", paste0("RF105_reviewed_", date_stamp))
figure_dir <- file.path(root, "6_figures", paste0("RF105_reviewed_", date_stamp))
restricted_dir <- file.path(
  root, "8_restricted", paste0("RF105_reviewed_", date_stamp), "identified_tables"
)

scripts <- c(
  "3_descriptive_outcomes_20260805_2213.R",
  "4_rdid_xgboost_20260805_2213.R",
  "5_drDiD_comparison_20260805_2213.R",
  "00_run_RF105_20260805_2213.R"
)
for (script in scripts) invisible(parse(file = file.path(reviewed_dir, script)))

canonical_file <- file.path(restricted_dir, "table_descriptive_pm25_household_timepoint_internal.csv")
period_file <- file.path(restricted_dir, "table_descriptive_pm25_24h_period_internal.csv")
exposure_internal_file <- file.path(
  restricted_dir,
  "table_descriptive_pm25_time_weighted_exposure_internal.csv"
)
exposure_summary_file <- file.path(
  table_dir,
  "table_descriptive_pm25_time_weighted_exposure.csv"
)
indoor_selected_file <- file.path(
  table_dir,
  "table_descriptive_pm25_indoor_selected_arm_timepoint.csv"
)
time_use_audit_file <- file.path(
  table_dir,
  "table_descriptive_pm25_time_use_source_audit.csv"
)
hour_summary_file <- file.path(table_dir, "table_descriptive_pm25_time_of_day.csv")
hour_figure_file <- file.path(figure_dir, "fig_descriptive_pm25_hourly_patterns.png")
mental_health_file <- file.path(table_dir, "table_descriptive_mental_health_outcomes.csv")
lpg_runout_file <- file.path(table_dir, "table_descriptive_lpg_runout_before_refill.csv")
public_files <- c(
  file.path(table_dir, "table_descriptive_pm25_household_timepoint_deidentified.csv"),
  file.path(table_dir, "table_descriptive_pm25_24h_period_deidentified.csv"),
  file.path(table_dir, "table_descriptive_pm25_arm_timepoint.csv"),
  exposure_summary_file,
  time_use_audit_file
)
stopifnot(
  file.exists(canonical_file),
  file.exists(period_file),
  file.exists(exposure_internal_file),
  file.exists(indoor_selected_file),
  file.exists(hour_summary_file),
  file.exists(hour_figure_file),
  file.exists(mental_health_file),
  file.exists(lpg_runout_file),
  all(file.exists(public_files))
)

canonical <- read_csv(canonical_file, show_col_types = FALSE)
periods <- read_csv(period_file, show_col_types = FALSE)
fraction_cols <- c(
  "pm25_ambient_excess_f000", "pm25_ambient_excess_f025",
  "pm25_ambient_excess_f050", "pm25_ambient_excess_f075",
  "pm25_ambient_excess_f100"
)
required_cols <- c(
  "fcn_id", "timepoint", "study_arm_overall", "indoor_pm25_mean",
  "ambient_pm25_mean", "valid_monitoring_hours", fraction_cols
)
stopifnot(all(required_cols %in% names(canonical)))
stopifnot(nrow(canonical) > 0)
stopifnot(!anyDuplicated(canonical[c("fcn_id", "timepoint")]))
stopifnot(all(stats::complete.cases(canonical[fraction_cols])))
stopifnot(all(canonical$valid_monitoring_hours > 0))

tolerance <- 1e-8
fractions <- c(0, 0.25, 0.50, 0.75, 1.00)
for (i in seq_along(fraction_cols)) {
  expected <- canonical$indoor_pm25_mean - fractions[[i]] * canonical$ambient_pm25_mean
  stopifnot(max(abs(canonical[[fraction_cols[[i]]]] - expected), na.rm = TRUE) < tolerance)
}

matched_periods <- periods %>% filter(analytic_period_common_support)
stopifnot(nrow(matched_periods) > 0)
stopifnot(all(matched_periods$has_valid_75pct_coverage))
stopifnot(all(matched_periods$valid_monitoring_hours >= 18 - tolerance))

weighted_mean_safe <- function(x, w) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  weighted.mean(x[keep], w[keep])
}
recomputed <- matched_periods %>%
  group_by(fcn_id, timepoint) %>%
  summarise(
    valid_monitoring_hours_check = sum(valid_monitoring_hours),
    indoor_pm25_mean_check = weighted_mean_safe(indoor_mean_pm25_24h, valid_monitoring_hours),
    ambient_pm25_mean_check = weighted_mean_safe(ambient_mean_pm25_24h, valid_monitoring_hours),
    across(all_of(fraction_cols), ~ weighted_mean_safe(.x, valid_monitoring_hours), .names = "{.col}_check"),
    .groups = "drop"
  )
comparison <- canonical %>% inner_join(recomputed, by = c("fcn_id", "timepoint"))
stopifnot(nrow(comparison) == nrow(canonical))
stopifnot(max(abs(comparison$valid_monitoring_hours - comparison$valid_monitoring_hours_check)) < tolerance)
stopifnot(max(abs(comparison$indoor_pm25_mean - comparison$indoor_pm25_mean_check)) < tolerance)
stopifnot(max(abs(comparison$ambient_pm25_mean - comparison$ambient_pm25_mean_check)) < tolerance)
for (column in fraction_cols) {
  stopifnot(max(abs(comparison[[column]] - comparison[[paste0(column, "_check")]])) < tolerance)
}

exposure_internal <- read_csv(exposure_internal_file, show_col_types = FALSE)
exposure_required_cols <- c(
  "fcn_id", "timepoint", "study_arm_overall", "population",
  "reported_hours_inside", "reported_hours_outside",
  "hours_inside_est", "hours_outside_est",
  "indoor_pm25_ug_m3", "outdoor_pm25_ug_m3",
  "time_weighted_average_pm25_ug_m3"
)
stopifnot(all(exposure_required_cols %in% names(exposure_internal)))
stopifnot(!anyDuplicated(exposure_internal[c("fcn_id", "timepoint", "population")]))
stopifnot(
  max(
    abs(exposure_internal$hours_inside_est + exposure_internal$hours_outside_est - 24),
    na.rm = TRUE
  ) < tolerance
)
both_reported <- exposure_internal %>%
  filter(household_time_reporting_pattern == "both_reported")
stopifnot(nrow(both_reported) > 0)
stopifnot(
  max(
    abs(
      both_reported$hours_inside_est -
        (both_reported$reported_hours_inside + 24 - both_reported$reported_hours_outside) / 2
    ),
    na.rm = TRUE
  ) < tolerance
)
joint_exposure <- exposure_internal %>%
  filter(is.finite(time_weighted_average_pm25_ug_m3))
expected_exposure <- (
  joint_exposure$hours_inside_est * joint_exposure$indoor_pm25_ug_m3 +
    joint_exposure$hours_outside_est * joint_exposure$outdoor_pm25_ug_m3
) / 24
stopifnot(nrow(joint_exposure) > 0)
stopifnot(
  max(
    abs(joint_exposure$time_weighted_average_pm25_ug_m3 - expected_exposure),
    na.rm = TRUE
  ) < tolerance
)

exposure_summary <- read_csv(exposure_summary_file, show_col_types = FALSE)
stopifnot(nrow(exposure_summary) == 18)
stopifnot(
  setequal(exposure_summary$timepoint, c("baseline", "midline", "endline")),
  setequal(exposure_summary$study_arm_overall, c("comparison", "intervention", "all_arms")),
  setequal(exposure_summary$population, c("caregiver", "target_child")),
  all(exposure_summary$n_households_with_time_and_pm > 0)
)
mean_sd_pairs <- list(
  c("mean_hours_inside_est", "sd_hours_inside_est"),
  c("mean_hours_outside_est", "sd_hours_outside_est"),
  c("mean_indoor_pm25_ug_m3", "sd_indoor_pm25_ug_m3"),
  c("mean_outdoor_pm25_ug_m3", "sd_outdoor_pm25_ug_m3"),
  c("mean_time_weighted_average_pm25_ug_m3", "sd_time_weighted_average_pm25_ug_m3")
)
for (pair in mean_sd_pairs) {
  stopifnot(match(pair[[2]], names(exposure_summary)) == match(pair[[1]], names(exposure_summary)) + 1)
}

exposure_summary_input <- bind_rows(
  exposure_internal,
  exposure_internal %>%
    filter(study_arm_overall %in% c("comparison", "intervention")) %>%
    mutate(study_arm_overall = "all_arms")
)
expected_exposure_summary <- exposure_summary_input %>%
  group_by(timepoint, study_arm_overall, population) %>%
  group_modify(function(.x, .y) {
    time_rows <- .x %>%
      filter(is.finite(hours_inside_est), is.finite(hours_outside_est))
    pm_rows <- time_rows %>%
      filter(is.finite(indoor_pm25_ug_m3), is.finite(outdoor_pm25_ug_m3))
    group_hours_inside <- mean(time_rows$hours_inside_est)
    group_hours_outside <- mean(time_rows$hours_outside_est)
    group_weighted_exposure <-
      (group_hours_inside / 24) * pm_rows$indoor_pm25_ug_m3 +
      (group_hours_outside / 24) * pm_rows$outdoor_pm25_ug_m3
    tibble(
      expected_mean_hours_inside = group_hours_inside,
      expected_mean_hours_outside = group_hours_outside,
      expected_mean_exposure = mean(group_weighted_exposure),
      expected_sd_exposure = stats::sd(group_weighted_exposure)
    )
  }) %>%
  ungroup()
exposure_summary_check <- exposure_summary %>%
  inner_join(
    expected_exposure_summary,
    by = c("timepoint", "study_arm_overall", "population")
  )
stopifnot(nrow(exposure_summary_check) == nrow(exposure_summary))
stopifnot(
  max(abs(exposure_summary_check$mean_hours_inside_est -
    exposure_summary_check$expected_mean_hours_inside)) < tolerance,
  max(abs(exposure_summary_check$mean_hours_outside_est -
    exposure_summary_check$expected_mean_hours_outside)) < tolerance,
  max(abs(exposure_summary_check$mean_time_weighted_average_pm25_ug_m3 -
    exposure_summary_check$expected_mean_exposure)) < tolerance,
  max(abs(exposure_summary_check$sd_time_weighted_average_pm25_ug_m3 -
    exposure_summary_check$expected_sd_exposure)) < tolerance
)

indoor_selected <- read_csv(indoor_selected_file, show_col_types = FALSE)
stopifnot(nrow(indoor_selected) == 5)
stopifnot(
  sum(indoor_selected$study_arm_overall == "comparison") == 3,
  sum(indoor_selected$study_arm_overall == "intervention") == 2,
  !any(
    indoor_selected$study_arm_overall == "intervention" &
      indoor_selected$timepoint == "baseline"
  )
)
expected_indoor_selected <- canonical %>%
  filter(
    study_arm_overall == "comparison" |
      (study_arm_overall == "intervention" & timepoint %in% c("midline", "endline"))
  ) %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    expected_n = n(),
    expected_mean = mean(indoor_pm25_mean),
    expected_sd = stats::sd(indoor_pm25_mean),
    expected_ci_lower = expected_mean -
      stats::qt(0.975, expected_n - 1) * expected_sd / sqrt(expected_n),
    expected_ci_upper = expected_mean +
      stats::qt(0.975, expected_n - 1) * expected_sd / sqrt(expected_n),
    .groups = "drop"
  )
indoor_selected_check <- indoor_selected %>%
  inner_join(expected_indoor_selected, by = c("timepoint", "study_arm_overall"))
stopifnot(nrow(indoor_selected_check) == 5)
stopifnot(
  all(indoor_selected_check$n_households == indoor_selected_check$expected_n),
  max(abs(indoor_selected_check$mean_indoor_pm25_ug_m3 -
    indoor_selected_check$expected_mean)) < tolerance,
  max(abs(indoor_selected_check$sd_indoor_pm25_ug_m3 -
    indoor_selected_check$expected_sd)) < tolerance,
  max(abs(indoor_selected_check$mean_indoor_pm25_95ci_lower -
    indoor_selected_check$expected_ci_lower)) < tolerance,
  max(abs(indoor_selected_check$mean_indoor_pm25_95ci_upper -
    indoor_selected_check$expected_ci_upper)) < tolerance
)

lpg_runout <- read_csv(lpg_runout_file, show_col_types = FALSE)
stopifnot(
  all(c(
    "n_all_household_days_summary_nonmissing",
    "mean_days_before_refill_all_households",
    "sd_days_before_refill_all_households"
  ) %in% names(lpg_runout)),
  match("sd_days_before_refill_all_households", names(lpg_runout)) ==
    match("mean_days_before_refill_all_households", names(lpg_runout)) + 1,
  all(
    lpg_runout$n_all_household_days_summary_nonmissing ==
      lpg_runout$n_households - lpg_runout$n_days_before_refill_gt_60_excluded
  ),
  all(lpg_runout$mean_days_before_refill_all_households >= 0),
  all(lpg_runout$sd_days_before_refill_all_households >= 0)
)

time_use_audit <- read_csv(time_use_audit_file, show_col_types = FALSE)
stopifnot(
  "both_reported" %in% time_use_audit$household_time_reporting_pattern,
  "outside_only" %in% time_use_audit$household_time_reporting_pattern
)

hour_summary <- read_csv(hour_summary_file, show_col_types = FALSE)
stopifnot("concurrent_outdoor_pm25" %in% hour_summary$metric_name)
stopifnot(file.info(hour_figure_file)$size > 0)

mental_health <- read_csv(mental_health_file, show_col_types = FALSE)
cesd_at_risk <- mental_health %>% filter(source_variable == "CES_D_go16_score")
stopifnot(nrow(cesd_at_risk) == 9)
stopifnot(all(abs(cesd_at_risk$percent_at_risk - 100 * cesd_at_risk$mean) < tolerance))
stopifnot(all(cesd_at_risk$n_at_risk == round(cesd_at_risk$n_nonmissing * cesd_at_risk$mean)))

descriptive_text <- paste(
  readLines(file.path(reviewed_dir, "3_descriptive_outcomes_20260805_2213.R"), warn = FALSE),
  collapse = "\n"
)
stopifnot(grepl("df$CES_D_score >= 16", descriptive_text, fixed = TRUE))
stopifnot(!grepl("df$CES_D_score > 16", descriptive_text, fixed = TRUE))

restricted_name_pattern <- paste(
  c("(^|_)fcn_id($|_)", "(^|_)hh_id($|_)", "date(time)?", "raw_source", "file(name|s)?", "monitor(_id|_ids)", "note"),
  collapse = "|"
)
for (file in public_files) {
  public <- read_csv(file, show_col_types = FALSE, n_max = 5)
  stopifnot(!any(grepl(restricted_name_pattern, names(public), ignore.case = TRUE)))
}

legacy_dirs <- c(
  file.path(root, "7_tables", paste0("pm25_ambient_adjusted_", date_stamp)),
  file.path(root, "8_restricted", paste0("pm25_ambient_adjusted_", date_stamp)),
  file.path(root, "6_figures", paste0("pm25_ambient_adjusted_", date_stamp))
)
stopifnot(!any(dir.exists(legacy_dirs)))

runner_text <- paste(readLines(file.path(reviewed_dir, "00_run_RF105_20260805_2213.R"), warn = FALSE), collapse = "\n")
stopifnot(!grepl("2_pm25_ambient_adjusted_analysis_20260805_2213.R", runner_text, fixed = TRUE))

for (script in c("4_rdid_xgboost_20260805_2213.R", "5_drDiD_comparison_20260805_2213.R")) {
  text <- paste(readLines(file.path(reviewed_dir, script), warn = FALSE), collapse = "\n")
  stopifnot(grepl("table_descriptive_pm25_household_timepoint_internal.csv", text, fixed = TRUE))
  stopifnot(!grepl("find_ambient_adjusted_pm_file", text, fixed = TRUE))
  stopifnot(!grepl("indoor_minus_ambient", text, fixed = TRUE))
  stopifnot(!grepl("weighted_mean_pm_rdid", text, fixed = TRUE))
  stopifnot(grepl("pm25_ambient_excess_f000", text, fixed = TRUE))
}

cat("Unified PM2.5 pipeline checks passed for RF105_reviewed_", date_stamp, ".\n", sep = "")
