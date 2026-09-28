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
  "4.1_rdid_time_weighted_pm25_20260928.R",
  "5_drDiD_comparison_20260805_2213.R",
  "00_run_RF105_20260805_2213.R"
)
for (script in scripts) invisible(parse(file = file.path(reviewed_dir, script)))

canonical_file <- file.path(restricted_dir, "table_descriptive_pm25_household_timepoint_internal.csv")
period_file <- file.path(restricted_dir, "table_descriptive_pm25_24h_period_internal.csv")
infiltration_file <- file.path(
  restricted_dir,
  "table_descriptive_pm25_infiltration_sensitivity_internal.csv"
)
ambient_dedup_file <- file.path(
  table_dir, "qa", "table_qa_pm25_ambient_deduplication.csv"
)
indoor_dedup_file <- file.path(
  table_dir, "qa", "table_qa_pm25_indoor_timestamp_deduplication.csv"
)
indoor_weighting_qa_file <- file.path(
  table_dir, "qa", "table_qa_pm25_indoor_time_weighting.csv"
)
hour_period_reconciliation_file <- file.path(
  table_dir, "qa", "table_qa_pm25_hour_period_reconciliation.csv"
)
hour_period_reconciliation_internal_file <- file.path(
  restricted_dir, "table_descriptive_pm25_hour_period_reconciliation_internal.csv"
)
ambient_coverage_file <- file.path(
  table_dir, "qa", "table_qa_pm25_ambient_period_coverage.csv"
)
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
ambient_pm25_hour_figure_file <- file.path(
  figure_dir,
  "fig_pm25_hapin_hour_of_day_raw_indoor_pm25_with_concurrent_ambient_pm25.png"
)
ambient_pm25_hour_reference_figure_file <- file.path(
  figure_dir,
  paste0(
    "fig_pm25_hapin_hour_of_day_raw_indoor_pm25_",
    "with_concurrent_ambient_pm25_reference_limits.png"
  )
)
raw_indoor_48h_baseline_midline_figure_file <- file.path(
  figure_dir,
  paste0(
    "fig_pm25_hapin_48h_household_distribution_raw_indoor_pm25_",
    "baseline_midline_no_title.png"
  )
)
raw_indoor_hour_baseline_midline_reference_figure_file <- file.path(
  figure_dir,
  paste0(
    "fig_pm25_hapin_hour_of_day_raw_indoor_pm25_",
    "baseline_midline_reference_limits_no_title.png"
  )
)
pm25_collection_timeline_figure_file <- file.path(
  figure_dir,
  "fig_descriptive_pm25_sampling_timeline.png"
)
pm25_collection_timeline_ambient_figure_file <- file.path(
  figure_dir,
  "fig_descriptive_pm25_sampling_timeline_with_ambient_pm25.png"
)
pm25_collection_timeline_internal_file <- file.path(
  restricted_dir,
  "table_descriptive_pm25_collection_timeline_daily_internal.csv"
)
pm25_ambient_daily_mean_internal_file <- file.path(
  restricted_dir,
  "table_descriptive_pm25_ambient_daily_mean_internal.csv"
)
rdid_results_file <- file.path(table_dir, "table_rDiD_xgboost_all_results.csv")
drdid_results_file <- file.path(table_dir, "table_DRDID_all_results.csv")
time_weighted_rdid_file <- file.path(
  table_dir,
  "table_rDiD_pm25_time_weighted_exposure_baseline_midline.csv"
)
time_weighted_rdid_counts_file <- file.path(
  table_dir,
  "qa",
  "table_rDiD_pm25_time_weighted_exposure_panel_counts.csv"
)
pm25_sampling_overlap_file <- file.path(
  table_dir, "qa", "table_rDiD_pm25_sampling_date_overlap.csv"
)
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
  file.exists(infiltration_file),
  file.exists(ambient_dedup_file),
  file.exists(indoor_dedup_file),
  file.exists(indoor_weighting_qa_file),
  file.exists(hour_period_reconciliation_file),
  file.exists(hour_period_reconciliation_internal_file),
  file.exists(ambient_coverage_file),
  file.exists(exposure_internal_file),
  file.exists(indoor_selected_file),
  file.exists(hour_summary_file),
  file.exists(hour_figure_file),
  file.exists(ambient_pm25_hour_figure_file),
  file.exists(ambient_pm25_hour_reference_figure_file),
  file.exists(raw_indoor_48h_baseline_midline_figure_file),
  file.exists(raw_indoor_hour_baseline_midline_reference_figure_file),
  file.exists(pm25_collection_timeline_figure_file),
  file.exists(pm25_collection_timeline_ambient_figure_file),
  file.exists(pm25_collection_timeline_internal_file),
  file.exists(pm25_ambient_daily_mean_internal_file),
  file.exists(rdid_results_file),
  file.exists(time_weighted_rdid_file),
  file.exists(time_weighted_rdid_counts_file),
  file.exists(drdid_results_file),
  file.exists(pm25_sampling_overlap_file),
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
  "ambient_pm25_mean", "valid_monitoring_hours", "valid_ambient_hours",
  "mean_ambient_coverage_prop", "ambient_fraction_default",
  "pm25_infiltration_factor_estimated",
  "pm25_indoor_excess_estimated_infiltration", fraction_cols
)
stopifnot(all(required_cols %in% names(canonical)))
stopifnot(nrow(canonical) > 0)
stopifnot(!anyDuplicated(canonical[c("fcn_id", "timepoint")]))
stopifnot(all(stats::complete.cases(canonical[fraction_cols])))
stopifnot(all(canonical$valid_monitoring_hours > 0))
stopifnot(all(abs(canonical$ambient_fraction_default - 0.25) < 1e-10))

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
stopifnot(all(matched_periods$has_valid_ambient_75pct_coverage))
stopifnot(all(matched_periods$ambient_coverage_prop_24h >= 0.75 - tolerance))
stopifnot(all(matched_periods$valid_ambient_hours_24h >= 18 - tolerance))
stopifnot(all(!is.na(matched_periods$matched_ambient_site_id)))
stopifnot(
  all(matched_periods$indoor_mean_weighting == "represented_monitoring_seconds"),
  max(abs(
    matched_periods$represented_monitoring_hours_24h -
      matched_periods$valid_monitoring_hours
  )) < tolerance
)

indoor_dedup <- read_csv(indoor_dedup_file, show_col_types = FALSE)
stopifnot(
  nrow(indoor_dedup) == 1,
  indoor_dedup$n_duplicate_rows_removed > 0,
  indoor_dedup$n_timestamps_represented_by_multiple_rows > 0,
  indoor_dedup$n_timestamps_with_conflicting_pm25 > 0,
  indoor_dedup$max_within_timestamp_pm25_range > 0,
  indoor_dedup$duplicate_resolution ==
    "mean PM2.5 within monitoring-window timestamp"
)

indoor_weighting_qa <- read_csv(indoor_weighting_qa_file, show_col_types = FALSE)
stopifnot(
  nrow(indoor_weighting_qa) > 0,
  all(indoor_weighting_qa$indoor_mean_weighting ==
    "represented_monitoring_seconds"),
  sum(indoor_weighting_qa$n_periods_changed_by_weighting) > 0,
  max(indoor_weighting_qa$max_absolute_weighting_difference_ug_m3) > 0
)

hour_period_reconciliation <- read_csv(
  hour_period_reconciliation_file,
  show_col_types = FALSE
)
hour_period_reconciliation_internal <- read_csv(
  hour_period_reconciliation_internal_file,
  show_col_types = FALSE
)
stopifnot(
  nrow(hour_period_reconciliation) == 1,
  hour_period_reconciliation$n_valid_periods ==
    nrow(hour_period_reconciliation_internal),
  hour_period_reconciliation$n_monitoring_hour_discrepancies_gt_1e_8 == 0,
  hour_period_reconciliation$n_indoor_mean_discrepancies_gt_1e_8 == 0,
  hour_period_reconciliation$max_absolute_monitoring_hour_difference < tolerance,
  hour_period_reconciliation$max_absolute_indoor_mean_difference_ug_m3 < tolerance
)

ambient_dedup <- read_csv(ambient_dedup_file, show_col_types = FALSE)
stopifnot(
  nrow(ambient_dedup) == 1,
  ambient_dedup$n_duplicate_rows_removed > 0,
  ambient_dedup$n_unique_site_monitor_timestamps < ambient_dedup$n_source_rows,
  ambient_dedup$monitor_hour_coverage_threshold_percent == 75
)

ambient_coverage <- read_csv(ambient_coverage_file, show_col_types = FALSE)
stopifnot(
  nrow(ambient_coverage) > 0,
  all(ambient_coverage$ambient_period_coverage_threshold_percent == 75),
  all(
    ambient_coverage$n_common_support_24h_periods <=
      ambient_coverage$n_indoor_valid_24h_periods
  )
)

infiltration <- read_csv(infiltration_file, show_col_types = FALSE)
stopifnot(
  nrow(infiltration) > 0,
  all(infiltration$infiltration_factor_estimated >= 0),
  all(infiltration$infiltration_factor_estimated <= 1),
  all(infiltration$infiltration_intercept_estimated >= 0)
)
nonmissing_infiltration_excess <- canonical$pm25_indoor_excess_estimated_infiltration[
  is.finite(canonical$pm25_indoor_excess_estimated_infiltration)
]
stopifnot(
  length(nonmissing_infiltration_excess) > 0,
  all(nonmissing_infiltration_excess >= 0)
)

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
    tibble(
      expected_mean_hours_inside = group_hours_inside,
      expected_mean_hours_outside = group_hours_outside,
      expected_mean_exposure = mean(pm_rows$time_weighted_average_pm25_ug_m3),
      expected_sd_exposure = stats::sd(pm_rows$time_weighted_average_pm25_ug_m3)
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
stopifnot(
  "metric_plot_label" %in% names(hour_summary),
  !any(is.na(hour_summary$metric_plot_label)),
  setequal(
    hour_summary$metric_plot_label,
    c(
      "Raw indoor PM2.5",
      "Concurrent outdoor PM2.5",
      "Indoor excess after 0.25 x outdoor PM2.5"
    )
  )
)
stopifnot(file.info(hour_figure_file)$size > 0)

ambient_pm25_hour_summary <- hour_summary %>%
  filter(metric_name == "concurrent_outdoor_pm25")
ambient_pm25_hour_groups <- interaction(
  ambient_pm25_hour_summary$timepoint,
  ambient_pm25_hour_summary$study_arm_overall,
  drop = TRUE
)
stopifnot(
  nrow(ambient_pm25_hour_summary) == 144L,
  length(levels(ambient_pm25_hour_groups)) == 6L,
  all(table(ambient_pm25_hour_groups) == 24L),
  setequal(
    unique(ambient_pm25_hour_summary$study_arm_overall),
    c("comparison", "intervention")
  ),
  all(is.finite(ambient_pm25_hour_summary$median_pm25)),
  all(ambient_pm25_hour_summary$median_pm25 > 0),
  file.info(ambient_pm25_hour_figure_file)$size > 0,
  file.info(ambient_pm25_hour_reference_figure_file)$size > 0,
  file.info(raw_indoor_48h_baseline_midline_figure_file)$size > 0,
  file.info(raw_indoor_hour_baseline_midline_reference_figure_file)$size > 0
)

pm25_collection_timeline <- read_csv(
  pm25_collection_timeline_internal_file,
  show_col_types = FALSE
)
stopifnot(
  setequal(
    unique(pm25_collection_timeline$collection_group),
    c("Comparison households", "Intervention households", "Ambient monitors")
  ),
  setequal(
    unique(pm25_collection_timeline$timepoint),
    c("baseline", "midline", "endline")
  ),
  all(!is.na(pm25_collection_timeline$collection_date)),
  all(pm25_collection_timeline$n_active_monitoring_units >= 1),
  all(pm25_collection_timeline$n_observations >= 1),
  all(
    c("comparison", "intervention") %in%
      unique(stats::na.omit(pm25_collection_timeline$study_arm_overall))
  ),
  all(
    c("baseline", "midline", "endline") %in%
      pm25_collection_timeline$timepoint[
        pm25_collection_timeline$collection_group == "Ambient monitors"
      ]
  ),
  file.info(pm25_collection_timeline_figure_file)$size > 0
)

pm25_ambient_daily_mean <- read_csv(
  pm25_ambient_daily_mean_internal_file,
  show_col_types = FALSE
)
stopifnot(
  nrow(pm25_ambient_daily_mean) ==
    dplyr::n_distinct(pm25_ambient_daily_mean$collection_date),
  setequal(
    unique(pm25_ambient_daily_mean$timepoint),
    c("baseline", "midline", "endline")
  ),
  all(is.finite(pm25_ambient_daily_mean$ambient_mean_pm25)),
  all(pm25_ambient_daily_mean$ambient_mean_pm25 > 0),
  all(pm25_ambient_daily_mean$n_valid_ambient_site_hours >= 1),
  file.info(pm25_collection_timeline_ambient_figure_file)$size > 0
)

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
stopifnot(
  grepl("default_material_infiltration_factor <- 0.25", descriptive_text, fixed = TRUE),
  grepl('"pm25_ambient_excess_f025", "ambient_adjusted_f025", 0.25, TRUE', descriptive_text, fixed = TRUE),
  grepl("pm25_ambient_excess_f025", descriptive_text, fixed = TRUE)
)
rdid_text <- paste(
  readLines(file.path(reviewed_dir, "4_rdid_xgboost_20260805_2213.R"), warn = FALSE),
  collapse = "\n"
)
stopifnot(grepl("df$CES_D_score >= 16", rdid_text, fixed = TRUE))
stopifnot(!grepl("df$CES_D_score > 16", rdid_text, fixed = TRUE))
stopifnot(
  grepl('reviewed_pm_outcome_primary = "pm25_ambient_excess_default"', rdid_text, fixed = TRUE),
  grepl("pm25_ambient_excess_default = as_number(pm25_ambient_excess_f025)", rdid_text, fixed = TRUE),
  !grepl("ambient_pm25_baseline", rdid_text, fixed = TRUE),
  !grepl("ambient_pm25_followup", rdid_text, fixed = TRUE)
)
drdid_text <- paste(
  readLines(file.path(reviewed_dir, "5_drDiD_comparison_20260805_2213.R"), warn = FALSE),
  collapse = "\n"
)
time_weighted_rdid_text <- paste(
  readLines(file.path(reviewed_dir, "4.1_rdid_time_weighted_pm25_20260928.R"), warn = FALSE),
  collapse = "\n"
)
stopifnot(
  grepl("reviewed_pm_default_infiltration_factor <- 0.25", drdid_text, fixed = TRUE),
  grepl("pm25_ambient_excess_default = as_number(pm25_ambient_excess_f025)", drdid_text, fixed = TRUE),
  !grepl("ambient_pm25_baseline", drdid_text, fixed = TRUE),
  !grepl("ambient_pm25_followup", drdid_text, fixed = TRUE),
  grepl('colnames(X)[[1]] <- "(Intercept)"', drdid_text, fixed = TRUE),
  grepl("if (skip_pm25)", drdid_text, fixed = TRUE)
)
runner_text <- paste(readLines(file.path(reviewed_dir, "00_run_RF105_20260805_2213.R"), warn = FALSE), collapse = "\n")
stopifnot(
  grepl("4.1_rdid_time_weighted_pm25_20260928.R", runner_text, fixed = TRUE),
  grepl(
    "table_rDiD_pm25_time_weighted_exposure_baseline_midline.csv",
    time_weighted_rdid_text,
    fixed = TRUE
  )
)

rdid_pm <- read_csv(rdid_results_file, show_col_types = FALSE) %>%
  filter(domain == "PM2.5 exposure", estimator == "rDID_XGBoost")
expected_pm_outcomes <- c(
  "pm25_ambient_excess_default", "pm25_ambient_excess_f000",
  "pm25_ambient_excess_f050", "pm25_ambient_excess_f075",
  "pm25_ambient_excess_f100"
)
stopifnot(
  nrow(rdid_pm) == 10,
  setequal(unique(rdid_pm$outcome), expected_pm_outcomes),
  all(rdid_pm$outcome_regression_covariates == "hh_size;hh_per_structure"),
  all(rdid_pm$propensity_score_covariates == "hh_size;hh_per_structure")
)
rdid_pm_default <- rdid_pm %>% filter(outcome == "pm25_ambient_excess_default")
stopifnot(
  nrow(rdid_pm_default) == 2,
  all(grepl("0.25", rdid_pm_default$outcome_label, fixed = TRUE)),
  all(rdid_pm_default$outcome_source == "pm25_fixed_fraction_default")
)

pm25_sampling_overlap <- read_csv(
  pm25_sampling_overlap_file,
  show_col_types = FALSE
)
stopifnot(
  nrow(pm25_sampling_overlap) == 3,
  setequal(pm25_sampling_overlap$timepoint, c("baseline", "midline", "endline")),
  all(pm25_sampling_overlap$n_calendar_days_overlap == 0),
  all(!pm25_sampling_overlap$arm_monitoring_date_windows_overlap)
)

time_weighted_rdid <- read_csv(time_weighted_rdid_file, show_col_types = FALSE)
time_weighted_rdid_counts <- read_csv(time_weighted_rdid_counts_file, show_col_types = FALSE)
stopifnot(
  nrow(time_weighted_rdid) == 2,
  setequal(time_weighted_rdid$population, c("caregiver", "target_child")),
  all(time_weighted_rdid$contrast == "primary_baseline_midline"),
  all(time_weighted_rdid$estimator == "rDID_XGBoost"),
  all(time_weighted_rdid$outcome == "time_weighted_average_pm25_ug_m3"),
  all(time_weighted_rdid$unit == "ug/m3"),
  all(time_weighted_rdid$outcome %in% names(read_csv(exposure_internal_file, show_col_types = FALSE, n_max = 0))),
  nrow(time_weighted_rdid_counts) == 4,
  setequal(time_weighted_rdid_counts$population, c("caregiver", "target_child")),
  setequal(time_weighted_rdid_counts$study_arm, c("comparison", "intervention")),
  all(time_weighted_rdid_counts$passes_minimum)
)

drdid_pm_default <- read_csv(drdid_results_file, show_col_types = FALSE) %>%
  filter(domain == "PM2.5 exposure", outcome == "pm25_ambient_excess_default")
stopifnot(
  nrow(drdid_pm_default) == 4,
  all(drdid_pm_default$drdid_covariates ==
    "(Intercept);hh_size;hh_per_structure"),
  all(!grepl("ambient_pm25", drdid_pm_default$drdid_covariates)),
  all(grepl(
    "default equals indoor minus 0.25 times concurrent outdoor",
    drdid_pm_default$ambient_covariate_note,
    fixed = TRUE
  ))
)

drdid_benchmark <- read_csv(
  file.path(table_dir, "table_DRDID_rDID_benchmark.csv"),
  show_col_types = FALSE
)
benchmark_pm_default <- drdid_benchmark %>%
  filter(outcome == "pm25_ambient_excess_default")
stopifnot(
  nrow(benchmark_pm_default) == 2,
  all(benchmark_pm_default$drdid_covariates ==
    "(Intercept);hh_size;hh_per_structure"),
  all(benchmark_pm_default$rdid_outcome_regression_covariates ==
    "hh_size;hh_per_structure"),
  all(benchmark_pm_default$rdid_propensity_score_covariates ==
    benchmark_pm_default$rdid_outcome_regression_covariates),
  all(grepl(
    "default PM2.5 outcome is indoor minus 0.25 times concurrent outdoor",
    benchmark_pm_default$pm25_covariate_comparison_note,
    fixed = TRUE
  ))
)
stopifnot(
  grepl("scales::pseudo_log_trans", descriptive_text, fixed = TRUE),
  !grepl("Positive adjusted percentile summaries only", descriptive_text, fixed = TRUE)
)

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

stopifnot(!grepl("2_pm25_ambient_adjusted_analysis_20260805_2213.R", runner_text, fixed = TRUE))

for (script in c("4_rdid_xgboost_20260805_2213.R", "5_drDiD_comparison_20260805_2213.R")) {
  text <- paste(readLines(file.path(reviewed_dir, script), warn = FALSE), collapse = "\n")
  stopifnot(grepl("table_descriptive_pm25_household_timepoint_internal.csv", text, fixed = TRUE))
  stopifnot(!grepl("find_ambient_adjusted_pm_file", text, fixed = TRUE))
  stopifnot(!grepl("indoor_minus_ambient", text, fixed = TRUE))
  stopifnot(!grepl("weighted_mean_pm_rdid", text, fixed = TRUE))
  stopifnot(grepl("pm25_ambient_excess_f000", text, fixed = TRUE))
  stopifnot(grepl("pm25_ambient_excess_default", text, fixed = TRUE))
  stopifnot(grepl("pm25_ambient_excess_f075", text, fixed = TRUE))
  stopifnot(!grepl("pm25_raw_indoor", text, fixed = TRUE))
  stopifnot(!grepl("pm25_indoor_excess_estimated_infiltration", text, fixed = TRUE))
}

stopifnot(
  grepl("table_descriptive_pm25_household_timepoint_internal.csv", time_weighted_rdid_text, fixed = TRUE),
  grepl("table_descriptive_pm25_time_weighted_exposure_internal.csv", time_weighted_rdid_text, fixed = TRUE),
  grepl("time_weighted_average_pm25_ug_m3", time_weighted_rdid_text, fixed = TRUE),
  !grepl("find_ambient_adjusted_pm_file", time_weighted_rdid_text, fixed = TRUE),
  !grepl("weighted_mean_pm_rdid", time_weighted_rdid_text, fixed = TRUE),
  !grepl("pm25_raw_indoor", time_weighted_rdid_text, fixed = TRUE)
)

cat("Unified PM2.5 pipeline checks passed for RF105_reviewed_", date_stamp, ".\n", sep = "")
