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

source(raw_import_path("5_analysis_RF105", "reviewed", "geocene_household_weighting.R"))

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

# Cooking summaries are defined in the shared household-weighting module.

geocene_household_stove_minutes <- function(daily) {
  stopifnot(!anyDuplicated(daily[c("fcn_id", "date")]),
    !anyNA(daily$fcn_id), all(daily$observed_stove_use_day),
    all(is.finite(daily$stove_on_min_sum_total_zero)), all(daily$stove_on_min_sum_total_zero >= 0))
  daily %>% group_by(fcn_id) %>% summarise(
    n_household_days = n(),
    total_stove_use_minutes = sum(stove_on_min_sum_total_zero), .groups = "drop")
}

geocene_household_minutes_bins <- function(totals) {
  if (!nrow(totals)) return(tibble(bin_start_minutes = numeric(), bin_end_minutes = numeric(), n_households = integer()))
  totals %>% count(bin_start_minutes = 60 * floor(total_stove_use_minutes / 60), name = "n_households") %>%
    tidyr::complete(bin_start_minutes = seq(0, max(bin_start_minutes), by = 60), fill = list(n_households = 0L)) %>%
    mutate(bin_end_minutes = bin_start_minutes + 60) %>%
    select(bin_start_minutes, bin_end_minutes, n_households)
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
  counts_calendar <- stove_counts("date") %>% arrange(date)
  peak_calendar <- counts_calendar %>% filter(n_stoves_monitored == max(n_stoves_monitored))
  maximum_calendar <- tibble(
    max_n_stoves_monitored_on_calendar_date = if (nrow(counts_calendar)) max(counts_calendar$n_stoves_monitored) else NA_integer_,
    n_calendar_dates_at_maximum = nrow(peak_calendar),
    calendar_dates_at_maximum = paste(peak_calendar$date, collapse = "; "),
    note = "Across all households and both arms on each Bangladesh-local calendar date; counts one recorded LPG and/or biomass fuel per household, not distinct devices. Only recorded-use days are counted."
  )
  stopifnot(sum(counts_calendar$n_household_days_monitored) == nrow(daily),
    sum(counts_calendar$n_stoves_monitored) == sum(daily$n_stoves_with_recorded_use),
    all(counts_calendar$n_stoves_monitored == counts_calendar$n_lpg_stoves_monitored + counts_calendar$n_biomass_stoves_monitored))
  stopifnot(sum(counts$n_household_days_monitored) == nrow(daily),
    sum(counts_arm$n_household_days_monitored) == nrow(daily),
    sum(counts$n_stoves_monitored) == sum(daily$n_stoves_with_recorded_use),
    all(summary$n_exclusive_lpg_days + summary$n_exclusive_biomass_days + summary$n_mixed_use_days == summary$n_household_days_monitored))
  post <- function(minimum) {
    d <- filter(daily, !is.na(days_after_first_receiving), days_after_first_receiving >= minimum)
    result <- geocene_reconcilable(d)$prevalence
    counts <- tibble(stove_use_category = geocene_categories,
      n_household_days = c(sum(d$exclusive_lpg_recalc), sum(d$exclusive_biomass_recalc), sum(d$mixed_use_recalc)))
    result %>% left_join(counts, by = "stove_use_category", suffix = c("_monitored", "")) %>%
      mutate(minimum_days_after_first_receiving = minimum, denominator_note = geocene_weighting_note)
  }
  event_summary <- geocene_all_arms(events) %>% group_by(timepoint, study_arm_overall, fuel_type) %>%
    summarise(n_events = n(), n_households = n_distinct(fcn_id), n_household_days = n_distinct(fcn_id, date),
      total_minutes = sum(stove_on_min), mean_event_minutes = mean(stove_on_min),
      sd_event_minutes = if (n() > 1) sd(stove_on_min) else NA_real_, median_event_minutes = median(stove_on_min),
      sd_reason = if (n() > 1) NA_character_ else "fewer_than_two_events", .groups = "drop") %>%
    mutate(aggregation_note = "Event-duration QA: individual events, not household-average daily use. Counts and cumulative totals have no applicable SD.")
  recodes <- events %>% filter(recode_baseline_intervention_lpg_date) %>% group_by(timepoint, study_arm_overall, fuel_type) %>%
    summarise(n_events = n(), n_households = n_distinct(fcn_id), n_household_days = n_distinct(fcn_id, date), .groups = "drop")
  list(
    table_descriptive_geocene_sample_counts = sample,
    table_descriptive_stove_sample_counts = sample,
    table_descriptive_geocene_daily_summary = summary,
    table_descriptive_stove_daily_summary = summary,
    table_descriptive_geocene_monitoring_scope_summary = scope,
    table_descriptive_geocene_cooking_by_stove_use_category = geocene_cooking_by_use(daily),
    table_descriptive_geocene_mixed_use_cooking_by_fuel = geocene_mixed_use_by_fuel(daily),
    table_descriptive_geocene_stoves_monitored_by_calendar_date = counts_calendar,
    table_descriptive_geocene_max_stoves_monitored_on_calendar_date = maximum_calendar,
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

geocene_output_filename <- function(filename, variant) {
  stopifnot(length(variant) == 1L, variant %in% geocene_variants)
  extension <- tools::file_ext(filename)
  stopifnot(nzchar(extension))
  paste0(tools::file_path_sans_ext(filename), "_", variant, ".", extension)
}

geocene_event_definition <- function(variant) {
  stopifnot(length(variant) == 1L, variant %in% geocene_variants)
  separation <- if (variant == "100_80_5_20") 20L else 30L
  paste0("Cooking events: thermocouple temperature above 80 C for at least 5 minutes, ",
    "with at least one recording >100 C during that time, and at least ", separation,
    " minutes since the prior cooking event. Applied upstream to exported events; not re-detected during analysis.")
}

geocene_run_analysis <- function(variant, figures = TRUE, output_root = NULL) {
  source(raw_import_path("5_analysis_RF105", "reviewed", "0_RF105_config_20260805_2213.R"), local = TRUE)
  # Scope naming to this variant without changing survey/PM writers or privacy routing.
  csv_writer <- write_reviewed_csv
  plot_writer <- save_reviewed_plot
  write_reviewed_csv <- function(x, filename, subfolder = NULL) {
    csv_writer(x, geocene_output_filename(filename, variant), subfolder)
  }
  save_reviewed_plot <- function(plot, filename, ...) {
    plot_writer(plot, geocene_output_filename(filename, variant), ...)
  }
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
  stopifnot(all(events$analysis_variant == variant), all(stove_daily$analysis_variant == variant),
    !anyNA(events$analysis_variant), !anyNA(stove_daily$analysis_variant))
  tables <- geocene_tables(events, stove_daily)
  for (name in names(tables)) write_reviewed_csv(tables[[name]], paste0(name, ".csv"))
  linked <- geocene_window_tables(stove_daily)
  for (name in names(linked)) {
    if (name %in% c("household_values", "overall_household_values")) {
      geocene_write(linked[[name]], file.path(dir_restricted_qa,
        geocene_output_filename(paste0("geocene_weighted_", name, ".rds"), variant)))
    } else {
      write_reviewed_csv(linked[[name]], paste0("table_descriptive_geocene_household_weighted_", name, ".csv"))
    }
  }
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
    source(raw_import_path("5_analysis_RF105", "reviewed", "geocene_household_minutes_histogram.R"), local = TRUE)
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
  write_reviewed_csv(tibble(note = c(paste("Analysis variant:", variant), geocene_event_definition(variant),
    geocene_note, geocene_weighting_note,
    "Receipt date is not an eligibility criterion for overall monitoring or stove-use summaries.",
    "Post-receipt summaries require a known receipt date and nonnegative elapsed days; 30-day summaries require at least 30 elapsed days.",
    "Mission logs are not available in these exports. No sensor-window denominator is used.",
    "Stove counts are recorded fuel types (one LPG and/or one biomass), not distinct physical devices.",
    "Energy conversion constants reproduce the previous figures and are not a new validated energy model.")), "table_descriptive_geocene_analysis_notes.csv")
  invisible(tables)
}
