# Shared summaries for primary and sensitivity Geocene event exports.
source(file.path(Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = "."),
  "1_data_import", "fixed", "geocene_pipeline_helpers.R"))

geocene_clean_data_root <- function() {
  path <- Sys.getenv("RF105_CLEAN_DATA_DIR", unset = "4_data/clean_final")
  if (!nzchar(path)) path <- "4_data/clean_final"
  if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\|~)", path)) path <- raw_import_path(path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

geocene_note <- paste("One household-day is one household and Bangladesh-local event start date with recorded stove use.",
  "Events from documented broken-probe missions are excluded; all other events are retained. Concurrent LPG and biomass count as one household-day and two recorded fuels.")

geocene_all_arms <- function(d) bind_rows(d, mutate(d, study_arm_overall = "all_arms"))
geocene_safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

geocene_use_summary <- function(d, groups = character()) {
  d %>% group_by(across(all_of(groups))) %>% summarise(
    n_daily_records = n(), n_household_days_monitored = n(), n_households = n_distinct(fcn_id),
    n_days_exclusive_denominator = n(), n_stoves_monitored = sum(n_stoves_with_recorded_use),
    n_exclusive_lpg_days = sum(exclusive_lpg_recalc), n_exclusive_biomass_days = sum(exclusive_biomass_recalc),
    n_mixed_use_days = sum(mixed_use_recalc),
    mean_lpg_events_per_day = mean(cooking_events_with_lpg_zero),
    mean_biomass_events_per_day = mean(cooking_events_with_biomass_zero),
    mean_lpg_minutes_per_day = mean(stove_on_min_sum_lpg_zero),
    mean_biomass_minutes_per_day = mean(stove_on_min_sum_biomass_zero),
    mean_total_stove_minutes_per_day = mean(stove_on_min_sum_total_zero), .groups = "drop") %>%
    mutate(pct_exclusive_lpg_days = if_else(n_daily_records > 0, 100 * n_exclusive_lpg_days / n_daily_records, NA_real_),
      pct_exclusive_biomass_days = if_else(n_daily_records > 0, 100 * n_exclusive_biomass_days / n_daily_records, NA_real_),
      pct_mixed_use_days = if_else(n_daily_records > 0, 100 * n_mixed_use_days / n_daily_records, NA_real_),
      denominator_note = geocene_note)
}

geocene_scope <- function(d, groups = character()) {
  d %>% group_by(across(all_of(groups))) %>% summarise(
    n_refugee_households_monitored = n_distinct(fcn_id), n_household_days_monitored = n(),
    n_household_days_with_days_after_first_receiving = sum(!is.na(days_after_first_receiving)),
    n_household_days_missing_days_after_first_receiving = sum(is.na(days_after_first_receiving)),
    n_stoves_monitored = sum(n_stoves_with_recorded_use),
    min_n_stoves_monitored_per_household_day_gt0 = if (n()) min(n_stoves_with_recorded_use) else NA_integer_,
    max_n_stoves_monitored_per_household_day_gt0 = if (n()) max(n_stoves_with_recorded_use) else NA_integer_,
    max_days_after_first_receiving_to_monitoring = geocene_safe_max(days_after_first_receiving), .groups = "drop") %>%
    mutate(monitoring_denominator_note = geocene_note)
}

geocene_tables <- function(events, daily) {
  stopifnot(!anyDuplicated(daily[c("fcn_id", "date")]), all(daily$observed_stove_use_day),
    nrow(daily) == n_distinct(events$fcn_id, events$date),
    sum(daily$cooking_events_with_lpg_zero + daily$cooking_events_with_biomass_zero) == nrow(events))
  summary <- geocene_use_summary(geocene_all_arms(daily), c("timepoint", "study_arm_overall"))
  sample <- geocene_all_arms(daily) %>% group_by(timepoint, study_arm_overall) %>% summarise(
    n_daily_records = n(), n_household_days_monitored = n(), n_households = n_distinct(fcn_id),
    first_monitoring_date = min(date), last_monitoring_date = max(date), .groups = "drop")
  scope <- bind_rows(
    geocene_scope(daily) %>% mutate(summary_scope = "overall"),
    geocene_scope(daily, "timepoint") %>% mutate(summary_scope = "timepoint_all_arms", study_arm_overall = "all_arms"),
    geocene_scope(daily, c("timepoint", "study_arm_overall")) %>% mutate(summary_scope = "timepoint_arm")) %>%
    select(summary_scope, timepoint, study_arm_overall, everything())
  stove_counts <- function(groups) daily %>% group_by(across(all_of(groups))) %>% summarise(
    n_households = n_distinct(fcn_id), n_household_days_monitored = n(),
    n_lpg_stoves_monitored = sum(lpg_recorded), n_biomass_stoves_monitored = sum(biomass_recorded),
    n_stoves_monitored = sum(n_stoves_with_recorded_use), .groups = "drop")
  counts <- stove_counts("days_after_first_receiving")
  counts_arm <- stove_counts(c("study_arm_overall", "days_after_first_receiving"))
  stopifnot(sum(counts$n_household_days_monitored) == nrow(daily),
    sum(counts_arm$n_household_days_monitored) == nrow(daily),
    sum(counts$n_stoves_monitored) == sum(daily$n_stoves_with_recorded_use),
    all(summary$n_exclusive_lpg_days + summary$n_exclusive_biomass_days + summary$n_mixed_use_days == summary$n_household_days_monitored))
  post <- function(minimum) {
    d <- filter(daily, !is.na(days_after_first_receiving), days_after_first_receiving >= minimum)
    categories <- c("exclusive_lpg_recalc", "exclusive_biomass_recalc", "mixed_use_recalc")
    tibble(stove_use_category = c("exclusive_lpg", "exclusive_biomass", "both_stoves"),
      n_household_days = vapply(categories, function(x) sum(d[[x]]), integer(1)),
      n_household_days_monitored = nrow(d), n_households = n_distinct(d$fcn_id),
      pct_household_days = if (nrow(d)) 100 * n_household_days / nrow(d) else NA_real_,
      minimum_days_after_first_receiving = minimum, denominator_note = geocene_note)
  }
  event_summary <- geocene_all_arms(events) %>% group_by(timepoint, study_arm_overall, fuel_type) %>%
    summarise(n_events = n(), n_households = n_distinct(fcn_id), n_household_days = n_distinct(fcn_id, date),
      total_minutes = sum(stove_on_min), mean_event_minutes = mean(stove_on_min), .groups = "drop")
  recodes <- events %>% filter(recode_baseline_intervention_lpg_date) %>% group_by(timepoint, study_arm_overall, fuel_type) %>%
    summarise(n_events = n(), n_households = n_distinct(fcn_id), n_household_days = n_distinct(fcn_id, date), .groups = "drop")
  list(
    table_descriptive_geocene_sample_counts = sample,
    table_descriptive_stove_sample_counts = sample,
    table_descriptive_geocene_daily_summary = summary,
    table_descriptive_stove_daily_summary = summary,
    table_descriptive_geocene_monitoring_scope_summary = scope,
    table_descriptive_geocene_stoves_monitored_by_days_after_receipt = counts,
    table_descriptive_geocene_stoves_monitored_by_days_after_receipt_by_arm = counts_arm,
    table_descriptive_geocene_post_lpg_exclusive_use_summary = post(0L),
    table_descriptive_geocene_post_lpg_30day_exclusive_use_summary = post(30L),
    table_descriptive_geocene_post_lpg_exclusive_use_denominator_by_arm_timepoint = geocene_use_summary(geocene_all_arms(filter(daily, !is.na(days_after_first_receiving), days_after_first_receiving >= 0)), c("timepoint", "study_arm_overall")),
    table_descriptive_geocene_post_lpg_30day_exclusive_use_denominator_by_arm_timepoint = geocene_use_summary(geocene_all_arms(filter(daily, !is.na(days_after_first_receiving), days_after_first_receiving >= 30)), c("timepoint", "study_arm_overall")),
    table_descriptive_stove_events_summary = event_summary,
    table_descriptive_geocene_event_duration_qa = events %>% group_by(fuel_type) %>% summarise(
      n_events = n(), n_events_longer_than_24_hours = sum(stove_on_min > 1440),
      maximum_event_minutes = max(stove_on_min), .groups = "drop") %>%
      mutate(note = "All events outside documented mission exclusions are retained; full exported duration is assigned to the local start date, including events spanning multiple days."),
    table_descriptive_stove_early_sensitivity = bind_rows(lapply(c(-Inf, 7, 14, 30, 60), function(minimum) {
      selected <- if (is.infinite(minimum)) daily else filter(daily, !is.na(days_after_first_receiving), days_after_first_receiving >= minimum)
      geocene_use_summary(geocene_all_arms(selected), c("timepoint", "study_arm_overall")) %>%
        mutate(sensitivity = if (is.infinite(minimum)) "main_all_days" else paste0("exclude_first_", minimum, "_days"), min_days_after_first_receiving = minimum)
    })),
    table_descriptive_geocene_baseline_intervention_lpg_recoded_midline_summary = recodes,
    table_descriptive_geocene_receipt_date_missingness = daily %>% group_by(timepoint, study_arm_overall) %>% summarise(
      n_household_days = n(), n_households = n_distinct(fcn_id), n_missing_receipt_household_days = sum(is.na(days_after_first_receiving)),
      n_conflicting_receipt_household_days = sum(receipt_date_conflict), n_before_receipt_household_days = sum(days_after_first_receiving < 0, na.rm = TRUE), .groups = "drop"),
    table_descriptive_geocene_days_after_receipt_reconciliation = tibble(n_household_days_monitored = nrow(daily),
      household_days_in_receipt_table = sum(counts$n_household_days_monitored), stove_days_in_receipt_table = sum(counts$n_stoves_monitored), passed = TRUE)
  )
}

geocene_run_analysis <- function(variant, figures = TRUE, output_root = NULL) {
  source(raw_import_path("5_analysis_RF105", "reviewed", "0_RF105_config_20260805_2213.R"), local = TRUE)
  dir_clean_final <- geocene_clean_data_root()
  if (!is.null(output_root)) {
    dir_tables_reviewed <- file.path(output_root, "tables")
    dir_figures_reviewed <- file.path(output_root, "figures")
    dir_restricted_reviewed <- file.path(output_root, "restricted")
    dir_tables_qa <- file.path(dir_tables_reviewed, "qa")
    dir_tables_release <- file.path(dir_tables_reviewed, "release")
    dir_restricted_qa <- file.path(dir_restricted_reviewed, "qa")
  }
  if (variant != geocene_variants[1]) {
    suffix <- paste0("sensitivity_", variant)
    dir_tables_reviewed <- file.path(dir_tables_reviewed, suffix)
    dir_figures_reviewed <- file.path(dir_figures_reviewed, suffix)
    dir_restricted_reviewed <- file.path(dir_restricted_reviewed, suffix)
    dir_tables_qa <- file.path(dir_tables_reviewed, "qa")
    dir_tables_release <- file.path(dir_tables_reviewed, "release")
    dir_restricted_qa <- file.path(dir_restricted_reviewed, "qa")
  }
  for (path in c(dir_tables_reviewed, dir_tables_qa, dir_tables_release, dir_figures_reviewed, dir_restricted_qa)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  stove_daily <- readRDS(file.path(dir_clean_final, "geocene", variant, "household_days.rds"))
  events <- readRDS(file.path(dir_clean_final, "geocene", variant, "events.rds"))
  tables <- geocene_tables(events, stove_daily)
  for (name in names(tables)) write_reviewed_csv(tables[[name]], paste0(name, ".csv"))
  exclusion_path <- file.path(dir_clean_final, "geocene", variant, "mission_exclusion_summary.rds")
  if (file.exists(exclusion_path)) write_reviewed_csv(readRDS(exclusion_path), "table_descriptive_geocene_mission_exclusion_summary.csv")
  write_reviewed_csv(stove_daily, "table_descriptive_stove_daily_dataset.csv")
  write_reviewed_csv(filter(events, stove_on_min > 1440), "table_descriptive_geocene_events_longer_than_24_hours.csv", subfolder = "qa")
  write_reviewed_csv(events %>% filter(recode_baseline_intervention_lpg_date) %>% group_by(mission_key, fcn_id, timepoint, fuel_type) %>%
    summarise(n_events = n(), first_date = min(date), last_date = max(date), .groups = "drop"), "table_descriptive_geocene_recoded_events_by_mission.csv", subfolder = "qa")
  if (figures) {
    as_number <- function(x) suppressWarnings(as.numeric(as.character(x)))
    collapse_collection_years <- function(x) paste(sort(unique(x[!is.na(x)])), collapse = ", ")
    add_prop_ci <- function(df, x_var, n_var, lower_name = "ci_lower", upper_name = "ci_upper") {
      if (!nrow(df)) { df[[lower_name]] <- numeric(); df[[upper_name]] <- numeric(); return(df) }
      ci <- t(vapply(seq_len(nrow(df)), function(i) 100 * binom.test(df[[x_var]][i], df[[n_var]][i])$conf.int, numeric(2)))
      df[[lower_name]] <- ci[, 1]; df[[upper_name]] <- ci[, 2]; df
    }
    set.seed(105)
    source(raw_import_path("5_analysis_RF105", "reviewed", "geocene_standard_figures.R"), local = TRUE)
    source(raw_import_path("5_analysis_RF105", "reviewed", "geocene_month_figures.R"), local = TRUE)
    source(raw_import_path("5_analysis_RF105", "reviewed", "geocene_composite_figures.R"), local = TRUE)
    stove_colors <- c(lpg = "#0072B2", biomass = "#D55E00")
    month_axis_label <- function(label, years) label
    save_plot_if_data <- function(data, plot, filename, width, height, ...) {
      if (nrow(data)) save_reviewed_plot(plot, filename, width = width, height = height, ...)
    }
    source(raw_import_path("5_analysis_RF105", "reviewed", "geocene_midline_figures.R"), local = TRUE)
  }
  write_reviewed_csv(tibble(note = c(geocene_note,
    "Receipt date is not an eligibility criterion for overall monitoring or stove-use summaries.",
    "Post-receipt summaries require a known receipt date and nonnegative elapsed days; 30-day summaries require at least 30 elapsed days.",
    "Mission logs are not available in these exports. No sensor-window denominator is used.",
    "Stove counts are recorded fuel types (one LPG and/or one biomass), not distinct physical devices.",
    "Energy conversion constants reproduce the previous figures and are not a new validated energy model.")), "table_descriptive_geocene_analysis_notes.csv")
  invisible(tables)
}
