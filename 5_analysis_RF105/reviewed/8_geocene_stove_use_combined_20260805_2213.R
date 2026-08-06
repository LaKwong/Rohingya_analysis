################################################################################
# RF105 reviewed Geocene stove-use combined analysis
#
# Purpose:
#   Combine and check the intended analyses from:
#   This self-contained script builds the Geocene stove-use analysis tables and figures from clean_final inputs.
#
#   This reviewed script is intentionally written as one linear workflow instead
#   of sourcing the three older scripts. The copied draft scripts were interactive and
#   contained View() calls, incomplete objects, and output paths that bypassed the
#   reviewed RF105 output folders.
#
# Input:
#   4_data/clean_final/stove_use_geocene_refugee_daily.rds
#
# Outputs:
#   Tables:
#     7_tables/RF105_reviewed_YYYYMMDD/geocene_stove_use_*.csv
#     7_tables/RF105_reviewed_YYYYMMDD/qa/geocene_stove_use_*.csv
#
#   Figures:
#     6_figures/RF105_reviewed_YYYYMMDD/geocene_stove_use_*.png
#
# Notes:
#   The clean_final daily stove-use and monitor-day files are used as analysis
#   inputs. The copied Geocene draft scripts are retained under
#   reviewed/5_geocene_analysis for provenance, but this script does not source
#   them.
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
config_file <- file.path(script_dir, "0_RF105_config_20260805_2213.R")
if (!file.exists(config_file)) {
  config_file <- file.path("5_analysis_RF105", "reviewed",
                           "0_RF105_config_20260805_2213.R")
}
source(config_file)

if (!requireNamespace("gridExtra", quietly = TRUE)) {
  stop("Install the gridExtra package before running this combined Geocene script.")
}

suppressPackageStartupMessages({
  library(gridExtra)
})

################################################################################
# Source-script audit
################################################################################

geocene_source_dir <- file.path(script_dir, "5_geocene_analysis")

source_scripts <- tibble(
  source_script = c(
    "1_stove_use_analysis_cl.R",
    "2_RF105_Fig2_code.R",
    "3_RF105_SupplementTable_stove_use_day.R"
  ),
  source_file = file.path(geocene_source_dir, source_script),
  exists = file.exists(source_file)
)

write_reviewed_csv(
  source_scripts,
  "table_descriptive_geocene_source_audit.csv",
  subfolder = "qa"
)

geocene_pathway_notes <- tibble(
  note_id = 1:4,
  note = c(
    "The copied Geocene draft scripts are retained under reviewed/5_geocene_analysis for provenance.",
    "The active reviewed workflow does not source those copied draft scripts.",
    "Raw Geocene exports are imported by 1_data_import/fixed/import_geocene_refugee_raw.R and cleaned by 3_data_cleaning/fixed/clean_geocene_refugee_20260805_2141.R.",
    "This script uses clean_final stove-use daily and monitor-day files and writes all outputs through reviewed RF105 table/figure helpers."
  )
)

write_reviewed_csv(
  geocene_pathway_notes,
  "table_descriptive_geocene_pathway_notes.csv",
  subfolder = "qa"
)

raw_paths_used_by_reviewed_import <- tibble(
  raw_file_role = c("events", "mission_logs", "missions", "sensors", "tags"),
  raw_file = file.path(
    project_root, "2_data_raw", "Geocene_220705",
    c("events_22.csv", "mission_logs_22.csv", "missions_22.csv", "sensors_22.csv", "tags_22.csv")
  ),
  exists = file.exists(raw_file)
)

write_reviewed_csv(
  raw_paths_used_by_reviewed_import,
  "table_descriptive_geocene_raw_paths.csv",
  subfolder = "qa"
)


read_imported_raw_csv <- function(filename) {
  in_file <- file.path(dir_clean_final, "imported_raw", filename)
  if (!file.exists(in_file)) {
    return(tibble(missing_input_file = in_file))
  }
  suppressMessages(readr::read_csv(in_file, show_col_types = FALSE))
}

geocene_import_inclusion_audit_by_household <- read_imported_raw_csv(
  "geocene_refugee_import_inclusion_audit_by_household.csv"
)
geocene_import_inclusion_audit_by_arm <- read_imported_raw_csv(
  "geocene_refugee_import_inclusion_audit_by_arm.csv"
)
geocene_import_inclusion_audit_analysis_eligible_by_household <-
  read_imported_raw_csv(
    "geocene_refugee_import_inclusion_audit_analysis_eligible_by_household.csv"
  )
geocene_import_inclusion_audit_analysis_eligible_by_arm <-
  read_imported_raw_csv(
    "geocene_refugee_import_inclusion_audit_analysis_eligible_by_arm.csv"
  )

write_reviewed_csv(
  geocene_import_inclusion_audit_by_household,
  "table_descriptive_geocene_inclusion_audit_household.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  geocene_import_inclusion_audit_by_arm,
  "table_descriptive_geocene_inclusion_audit_arm.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  geocene_import_inclusion_audit_analysis_eligible_by_household,
  "table_descriptive_geocene_analysis_inclusion_audit_household.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  geocene_import_inclusion_audit_analysis_eligible_by_arm,
  "table_descriptive_geocene_analysis_inclusion_audit_arm.csv",
  subfolder = "qa"
)################################################################################
# Load and check the reviewed daily stove-use input
################################################################################

stove_required_vars <- c(
  "community",
  "data_type",
  "fcn_id",
  "hh_id",
  "timepoint",
  "study_arm_overall",
  "date",
  "first_receive_lpg_ymd",
  "lpg_enrolled_and_receiving",
  "cooking_events_with_lpg",
  "cooking_events_with_biomass",
  "stove_on_min_sum_lpg",
  "stove_on_min_sum_biomass",
  "stove_on_min_sum_total",
  "stove_on_min_pc_biomass",
  "stove_on_min_pc_lpg",
  "days_after_first_receiving",
  "months_after_first_receiving_numeric",
  "exclusive_biomass",
  "exclusive_lpg",
  "mixed_use"
)

stove_daily_raw <- readRDS(file_stove_daily)

stove_var_audit <- flag_missing_vars(
  stove_daily_raw,
  stove_required_vars,
  "clean_final Geocene daily stove-use file"
)

write_reviewed_csv(
  stove_var_audit,
  "table_descriptive_geocene_variable_availability.csv",
  subfolder = "qa"
)

missing_required <- stove_var_audit %>%
  filter(!available) %>%
  pull(variable)

if (length(missing_required) > 0) {
  stop(
    "Required variables are missing from clean_final stove-use data: ",
    paste(missing_required, collapse = ", ")
  )
}

stove_daily <- stove_daily_raw %>%
  clean_timepoint_arm() %>%
  mutate(
    timepoint_unclassified = is.na(timepoint),
    study_arm_unclassified = is.na(study_arm_overall),
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = replace_na(
      as.character(study_arm_overall),
      "missing_study_arm"
    ),
    fcn_id = str_squish(as.character(fcn_id)),
    hh_id = str_squish(as.character(hh_id)),
    date = as.Date(date),
    first_receive_lpg_ymd = as.Date(first_receive_lpg_ymd),
    days_after_first_receiving =
      suppressWarnings(as.numeric(days_after_first_receiving)),
    months_after_first_receiving_numeric =
      suppressWarnings(as.numeric(months_after_first_receiving_numeric)),
    cooking_events_with_lpg =
      suppressWarnings(as.numeric(cooking_events_with_lpg)),
    cooking_events_with_biomass =
      suppressWarnings(as.numeric(cooking_events_with_biomass)),
    stove_on_min_sum_lpg =
      suppressWarnings(as.numeric(stove_on_min_sum_lpg)),
    stove_on_min_sum_biomass =
      suppressWarnings(as.numeric(stove_on_min_sum_biomass)),
    stove_on_min_sum_total =
      suppressWarnings(as.numeric(stove_on_min_sum_total)),
    stove_on_min_pc_lpg =
      suppressWarnings(as.numeric(stove_on_min_pc_lpg)),
    stove_on_min_pc_biomass =
      suppressWarnings(as.numeric(stove_on_min_pc_biomass)),
    exclusive_biomass = as.logical(exclusive_biomass),
    exclusive_lpg = as.logical(exclusive_lpg),
    mixed_use = as.logical(mixed_use)
  ) %>%
  filter(!is.na(fcn_id), fcn_id != "", !is.na(date))

stove_daily_unclassified_timepoint <- stove_daily %>%
  filter(timepoint_unclassified) %>%
  select(any_of(c(
    "fcn_id", "hh_id", "date", "timepoint", "collection_date",
    "raw_collection_round", "raw_source_file", "study_arm_overall"
  )))

write_reviewed_csv(
  stove_daily_unclassified_timepoint,
  "table_descriptive_geocene_unclassified_timepoints.csv",
  subfolder = "qa"
)

if (nrow(stove_daily_unclassified_timepoint) > 0) {
  stop(
    "Geocene stove-use daily data include rows outside baseline/midline/endline date windows. ",
    "See table_descriptive_geocene_unclassified_timepoints.csv.",
    call. = FALSE
  )
}

# Preserve the source-data missingness used to infer whether a fuel had a recorded
# event on that household-day. Then create zero-filled variables for summary
# calculations where "no event for that fuel" should contribute 0 minutes/events.
stove_daily <- stove_daily %>%
  mutate(
    stove_on_min_sum_lpg_na = stove_on_min_sum_lpg,
    stove_on_min_sum_biomass_na = stove_on_min_sum_biomass,
    cooking_events_with_lpg_na = cooking_events_with_lpg,
    cooking_events_with_biomass_na = cooking_events_with_biomass,
    lpg_recorded = !is.na(stove_on_min_sum_lpg_na) |
      !is.na(cooking_events_with_lpg_na),
    biomass_recorded = !is.na(stove_on_min_sum_biomass_na) |
      !is.na(cooking_events_with_biomass_na),
    stove_on_min_sum_lpg_zero = replace_na(stove_on_min_sum_lpg, 0),
    stove_on_min_sum_biomass_zero = replace_na(stove_on_min_sum_biomass, 0),
    cooking_events_with_lpg_zero = replace_na(cooking_events_with_lpg, 0),
    cooking_events_with_biomass_zero =
      replace_na(cooking_events_with_biomass, 0),
    stove_on_min_sum_total_zero =
      stove_on_min_sum_lpg_zero + stove_on_min_sum_biomass_zero,
    stove_on_min_pc_lpg_zero = if_else(
      stove_on_min_sum_total_zero > 0,
      100 * stove_on_min_sum_lpg_zero / stove_on_min_sum_total_zero,
      NA_real_
    ),
    stove_on_min_pc_biomass_zero = if_else(
      stove_on_min_sum_total_zero > 0,
      100 * stove_on_min_sum_biomass_zero / stove_on_min_sum_total_zero,
      NA_real_
    ),
    exclusive_lpg_recalc =
      lpg_recorded & !biomass_recorded,
    exclusive_biomass_recalc =
      biomass_recorded & !lpg_recorded,
    mixed_use_recalc =
      biomass_recorded & lpg_recorded
  )

# Preserve event-only household-days for QA. The reviewed analysis denominator is
# based on mission-log days with monitor samples when that clean-final file is
# available. This avoids treating "had a stove-on event" as the denominator for
# "was monitored."
stove_daily_event_days <- stove_daily

stove_monitor_days_raw <- if (exists("file_stove_monitor_days") && file.exists(file_stove_monitor_days)) {
  readRDS(file_stove_monitor_days)
} else {
  tibble()
}

stove_monitor_days <- tibble()
stove_events_without_monitor_day <- tibble()

if (nrow(stove_monitor_days_raw) > 0) {
  stove_monitor_days <- stove_monitor_days_raw %>%
    clean_timepoint_arm() %>%
    mutate(
      timepoint_unclassified = is.na(timepoint),
      study_arm_unclassified = is.na(study_arm_overall),
      timepoint = as_ordered_timepoint(timepoint),
      study_arm_overall = replace_na(
        as.character(study_arm_overall),
        "missing_study_arm"
      ),
      fcn_id = str_squish(as.character(fcn_id)),
      hh_id = str_squish(as.character(hh_id)),
      fuel_type = str_squish(str_to_lower(as.character(fuel_type))),
      date = as.Date(date),
      first_receive_lpg_ymd = as.Date(first_receive_lpg_ymd),
      days_after_first_receiving = suppressWarnings(as.numeric(date - first_receive_lpg_ymd)),
      months_after_first_receiving = cut(
        days_after_first_receiving,
        breaks = seq(-30, 30 * 36, 30),
        labels = c(-1, seq(0, 35, 1))
      ),
      months_after_first_receiving_numeric =
        suppressWarnings(as.numeric(as.character(months_after_first_receiving)))
    ) %>%
    filter(
      !is.na(fcn_id), fcn_id != "",
      !is.na(date),
      fuel_type %in% c("lpg", "biomass"),
      lpg_enrolled_and_receiving == "receiving LPG through distribution program"
    )

  stove_monitor_days_unclassified_timepoint <- stove_monitor_days %>%
    filter(timepoint_unclassified) %>%
    select(any_of(c(
      "fcn_id", "hh_id", "date", "timepoint", "collection_date",
      "raw_collection_round", "raw_source_file", "study_arm_overall", "fuel_type"
    )))

  write_reviewed_csv(
    stove_monitor_days_unclassified_timepoint,
    "table_descriptive_geocene_monitor_unclassified_timepoints.csv",
    subfolder = "qa"
  )

  if (nrow(stove_monitor_days_unclassified_timepoint) > 0) {
    stop(
      "Geocene stove monitor-day data include rows outside baseline/midline/endline date windows. ",
      "See table_descriptive_geocene_monitor_unclassified_timepoints.csv.",
      call. = FALSE
    )
  }

  stove_monitor_day_wide <- stove_monitor_days %>%
    group_by(
      fcn_id, timepoint, study_arm_overall, date,
      first_receive_lpg_ymd, days_after_first_receiving,
      months_after_first_receiving_numeric, lpg_enrolled_and_receiving
    ) %>%
    summarise(
      hh_id = first(hh_id[!is.na(hh_id) & hh_id != ""], default = NA_character_),
      raw_collection_round = paste(sort(unique(raw_collection_round)), collapse = "; "),
      raw_source_file = paste(sort(unique(raw_source_file)), collapse = "; "),
      lpg_monitored = any(fuel_type == "lpg"),
      biomass_monitored = any(fuel_type == "biomass"),
      n_monitor_stove_days = n_distinct(fuel_type),
      .groups = "drop"
    )

  event_day_for_join <- stove_daily_event_days %>%
    group_by(fcn_id, date) %>%
    summarise(
      hh_id_event = paste(sort(unique(hh_id[!is.na(hh_id) & hh_id != ""])), collapse = "; "),
      cooking_events_with_lpg = sum(cooking_events_with_lpg, na.rm = TRUE),
      cooking_events_with_biomass = sum(cooking_events_with_biomass, na.rm = TRUE),
      stove_on_min_sum_lpg = sum(stove_on_min_sum_lpg, na.rm = TRUE),
      stove_on_min_sum_biomass = sum(stove_on_min_sum_biomass, na.rm = TRUE),
      .groups = "drop"
    )

  stove_events_without_monitor_day <- stove_daily_event_days %>%
    anti_join(stove_monitor_day_wide, by = c("fcn_id", "date"))

  stove_daily <- stove_monitor_day_wide %>%
    left_join(event_day_for_join, by = c("fcn_id", "date")) %>%
    mutate(
      cooking_events_with_lpg = if_else(
        lpg_monitored,
        replace_na(suppressWarnings(as.numeric(cooking_events_with_lpg)), 0),
        NA_real_
      ),
      cooking_events_with_biomass = if_else(
        biomass_monitored,
        replace_na(suppressWarnings(as.numeric(cooking_events_with_biomass)), 0),
        NA_real_
      ),
      stove_on_min_sum_lpg = if_else(
        lpg_monitored,
        replace_na(suppressWarnings(as.numeric(stove_on_min_sum_lpg)), 0),
        NA_real_
      ),
      stove_on_min_sum_biomass = if_else(
        biomass_monitored,
        replace_na(suppressWarnings(as.numeric(stove_on_min_sum_biomass)), 0),
        NA_real_
      ),
      lpg_recorded = lpg_monitored & cooking_events_with_lpg > 0,
      biomass_recorded = biomass_monitored & cooking_events_with_biomass > 0,
      stove_on_min_sum_lpg_na = if_else(lpg_monitored, stove_on_min_sum_lpg, NA_real_),
      stove_on_min_sum_biomass_na = if_else(biomass_monitored, stove_on_min_sum_biomass, NA_real_),
      cooking_events_with_lpg_na = if_else(lpg_monitored, cooking_events_with_lpg, NA_real_),
      cooking_events_with_biomass_na = if_else(biomass_monitored, cooking_events_with_biomass, NA_real_),
      stove_on_min_sum_lpg_zero = replace_na(stove_on_min_sum_lpg, 0),
      stove_on_min_sum_biomass_zero = replace_na(stove_on_min_sum_biomass, 0),
      cooking_events_with_lpg_zero = replace_na(cooking_events_with_lpg, 0),
      cooking_events_with_biomass_zero = replace_na(cooking_events_with_biomass, 0),
      stove_on_min_sum_total_zero =
        stove_on_min_sum_lpg_zero + stove_on_min_sum_biomass_zero,
      stove_on_min_pc_lpg_zero = if_else(
        stove_on_min_sum_total_zero > 0,
        100 * stove_on_min_sum_lpg_zero / stove_on_min_sum_total_zero,
        NA_real_
      ),
      stove_on_min_pc_biomass_zero = if_else(
        stove_on_min_sum_total_zero > 0,
        100 * stove_on_min_sum_biomass_zero / stove_on_min_sum_total_zero,
        NA_real_
      ),
      valid_exclusive_use_denominator = lpg_monitored & biomass_monitored,
      exclusive_lpg_recalc = valid_exclusive_use_denominator &
        lpg_recorded & !biomass_recorded,
      exclusive_biomass_recalc = valid_exclusive_use_denominator &
        biomass_recorded & !lpg_recorded,
      mixed_use_recalc = valid_exclusive_use_denominator &
        biomass_recorded & lpg_recorded,
      exclusive_lpg = exclusive_lpg_recalc,
      exclusive_biomass = exclusive_biomass_recalc,
      mixed_use = mixed_use_recalc,
      timepoint_unclassified = is.na(timepoint),
      study_arm_unclassified = is.na(study_arm_overall),
      community = "refugee",
      data_type = "geocene_stove_use_daily_monitor_denominator",
      monitor_denominator_source = "mission_logs_num_samples_gt0"
    )
} else {
  stove_daily <- stove_daily_event_days %>%
    mutate(
      lpg_monitored = lpg_recorded,
      biomass_monitored = biomass_recorded,
      valid_exclusive_use_denominator = lpg_recorded | biomass_recorded,
      n_monitor_stove_days = as.integer(lpg_monitored) + as.integer(biomass_monitored),
      monitor_denominator_source = "event_day_fallback_no_monitor_file"
    )
}

# This reviewed household-day dataset is the downstream source for analyses of
# the amount of time stoves were in use. It is derived here after the Geocene
# QA/recode steps so later scripts do not independently reprocess clean_final.
geocene_stove_use_daily_analysis_dataset <- stove_daily %>%
  select(any_of(c(
    "fcn_id", "hh_id", "timepoint", "study_arm_overall", "date",
    "first_receive_lpg_ymd", "days_after_first_receiving",
    "months_after_first_receiving_numeric", "lpg_enrolled_and_receiving",
    "lpg_monitored", "biomass_monitored", "valid_exclusive_use_denominator",
    "n_monitor_stove_days", "monitor_denominator_source",
    "lpg_recorded", "biomass_recorded",
    "cooking_events_with_lpg_zero", "cooking_events_with_biomass_zero",
    "stove_on_min_sum_lpg_zero", "stove_on_min_sum_biomass_zero",
    "stove_on_min_sum_total_zero", "stove_on_min_pc_lpg_zero",
    "stove_on_min_pc_biomass_zero", "exclusive_lpg_recalc",
    "exclusive_biomass_recalc", "mixed_use_recalc",
    "raw_collection_round", "raw_source_file"
  ))) %>%
  arrange(timepoint, study_arm_overall, fcn_id, date)

write_reviewed_csv(
  geocene_stove_use_daily_analysis_dataset,
  "table_descriptive_stove_daily_dataset.csv"
)

################################################################################
# Data QA checks
################################################################################

stove_duplicate_fcn_date <- stove_daily %>%
  add_count(fcn_id, date, name = "n_records_for_fcn_date") %>%
  filter(n_records_for_fcn_date > 1) %>%
  select(
    fcn_id, hh_id, date, timepoint, study_arm_overall,
    n_records_for_fcn_date, raw_collection_round, raw_source_file
  ) %>%
  arrange(fcn_id, date)

write_reviewed_csv(
  stove_duplicate_fcn_date,
  "table_descriptive_geocene_duplicate_dates.csv",
  subfolder = "qa"
)

stove_events_without_monitor_day_distinct <- stove_events_without_monitor_day %>%
  distinct(fcn_id, date, timepoint, study_arm_overall, .keep_all = TRUE)

stove_events_without_monitor_day_by_arm <- stove_events_without_monitor_day_distinct %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_event_household_days_without_monitor_day = n(),
    n_households_without_monitor_day = n_distinct(fcn_id),
    first_unmatched_date = min(date, na.rm = TRUE),
    last_unmatched_date = max(date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall)

stove_events_without_monitor_day_by_household <- stove_events_without_monitor_day_distinct %>%
  group_by(timepoint, study_arm_overall, fcn_id, hh_id) %>%
  summarise(
    n_event_household_days_without_monitor_day = n_distinct(date),
    first_unmatched_date = min(date, na.rm = TRUE),
    last_unmatched_date = max(date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall, fcn_id, hh_id)

write_reviewed_csv(
  stove_events_without_monitor_day_by_arm,
  "table_descriptive_geocene_monitor_denominator_unmatched_arm.csv",
  subfolder = "qa"
)

write_reviewed_csv(
  stove_events_without_monitor_day_by_household,
  "table_descriptive_geocene_monitor_denominator_unmatched_household.csv",
  subfolder = "qa"
)

qa_checks <- tibble(
  check = c(
    "daily records",
    "households",
    "missing fcn_id",
    "missing date",
    "duplicate fcn_id-date rows",
    "negative stove minutes",
    "daily total minutes greater than 24 hours",
    "percent LPG plus percent biomass not approximately 100",
    "exclusive-use flag mismatch after recalculation",
    "records not assigned to baseline/midline/endline survey timepoint",
    "rows before first receiving LPG",
    "rows missing first_receive_lpg_ymd",
    "reviewed raw Geocene_220705 import files missing",
    "event household-days without matching monitor-day denominator"
  ),
  n_records = c(
    nrow(stove_daily),
    n_distinct(stove_daily$fcn_id),
    sum(is.na(stove_daily$fcn_id) | stove_daily$fcn_id == ""),
    sum(is.na(stove_daily$date)),
    nrow(stove_duplicate_fcn_date),
    sum(stove_daily$stove_on_min_sum_lpg_zero < 0 |
          stove_daily$stove_on_min_sum_biomass_zero < 0,
        na.rm = TRUE),
    sum(stove_daily$stove_on_min_sum_total_zero > 24 * 60, na.rm = TRUE),
    sum(
      stove_daily$stove_on_min_sum_total_zero > 0 &
        abs(stove_daily$stove_on_min_pc_lpg_zero +
              stove_daily$stove_on_min_pc_biomass_zero - 100) > 0.01,
      na.rm = TRUE
    ),
    sum(
      stove_daily$exclusive_lpg != stove_daily$exclusive_lpg_recalc |
        stove_daily$exclusive_biomass != stove_daily$exclusive_biomass_recalc |
        stove_daily$mixed_use != stove_daily$mixed_use_recalc,
      na.rm = TRUE
    ),
    sum(stove_daily$timepoint_unclassified, na.rm = TRUE),
    sum(stove_daily$days_after_first_receiving < 0, na.rm = TRUE),
    sum(is.na(stove_daily$first_receive_lpg_ymd)),
    sum(!raw_paths_used_by_reviewed_import$exists),
    nrow(stove_events_without_monitor_day_distinct)
  )
)

write_reviewed_csv(
  qa_checks,
  "table_descriptive_geocene_quality_checks.csv",
  subfolder = "qa"
)

sample_counts <- stove_daily %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_daily_records = n(),
    n_households = n_distinct(fcn_id),
    first_monitoring_date = min(date, na.rm = TRUE),
    last_monitoring_date = max(date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall)

write_reviewed_csv(
  sample_counts,
  "table_descriptive_geocene_sample_counts.csv"
)

daily_summary <- stove_daily %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_daily_records = n(),
    n_households = n_distinct(fcn_id),
    n_days_exclusive_denominator =
      sum(valid_exclusive_use_denominator, na.rm = TRUE),
    n_exclusive_lpg_days = sum(exclusive_lpg_recalc, na.rm = TRUE),
    pct_exclusive_lpg_days = if_else(
      n_days_exclusive_denominator > 0,
      100 * n_exclusive_lpg_days / n_days_exclusive_denominator,
      NA_real_
    ),
    n_exclusive_biomass_days = sum(exclusive_biomass_recalc, na.rm = TRUE),
    pct_exclusive_biomass_days = if_else(
      n_days_exclusive_denominator > 0,
      100 * n_exclusive_biomass_days / n_days_exclusive_denominator,
      NA_real_
    ),
    n_mixed_use_days = sum(mixed_use_recalc, na.rm = TRUE),
    pct_mixed_use_days = if_else(
      n_days_exclusive_denominator > 0,
      100 * n_mixed_use_days / n_days_exclusive_denominator,
      NA_real_
    ),
    mean_lpg_events_per_day = mean(cooking_events_with_lpg_zero, na.rm = TRUE),
    mean_biomass_events_per_day =
      mean(cooking_events_with_biomass_zero, na.rm = TRUE),
    mean_lpg_minutes_per_day =
      mean(stove_on_min_sum_lpg_zero, na.rm = TRUE),
    mean_biomass_minutes_per_day =
      mean(stove_on_min_sum_biomass_zero, na.rm = TRUE),
    mean_total_stove_minutes_per_day =
      mean(stove_on_min_sum_total_zero, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall)

write_reviewed_csv(
  daily_summary,
  "table_descriptive_geocene_daily_summary.csv"
)

################################################################################
# RF105 Figure 2-style daily stove-use panels
################################################################################

min_household_days_for_day_plot <- 6

df_days_receive <- stove_daily %>%
  mutate(
    before_after = if_else(
      days_after_first_receiving < 0,
      "Days Before Intervention",
      "Days After Receiving"
    ),
    before_after = factor(
      before_after,
      levels = c("Days Before Intervention", "Days After Receiving")
    )
  ) %>%
  filter(
    before_after == "Days After Receiving",
    !is.na(days_after_first_receiving)
  )

fig2_day_summary <- df_days_receive %>%
  group_by(days_after_first_receiving, before_after) %>%
  summarise(
    n_household_days = n(),
    n_households = n_distinct(fcn_id),
    mean_lpg_minutes_per_day =
      mean(stove_on_min_sum_lpg_zero, na.rm = TRUE),
    mean_biomass_minutes_per_day =
      mean(stove_on_min_sum_biomass_zero, na.rm = TRUE),
    mean_pct_lpg_minutes =
      mean(stove_on_min_pc_lpg_zero, na.rm = TRUE),
    mean_pct_biomass_minutes =
      mean(stove_on_min_pc_biomass_zero, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_household_days >= min_household_days_for_day_plot)

fig2_minutes_data <- fig2_day_summary %>%
  select(
    days_after_first_receiving,
    before_after,
    n_household_days,
    mean_lpg_minutes_per_day,
    mean_biomass_minutes_per_day
  ) %>%
  pivot_longer(
    cols = c(mean_lpg_minutes_per_day, mean_biomass_minutes_per_day),
    names_to = "stove",
    values_to = "average_minutes_of_use"
  ) %>%
  mutate(
    stove = recode(
      stove,
      mean_lpg_minutes_per_day = "LPG",
      mean_biomass_minutes_per_day = "Biomass"
    )
  )

fig2_percent_data <- fig2_day_summary %>%
  select(
    days_after_first_receiving,
    before_after,
    n_household_days,
    mean_pct_lpg_minutes,
    mean_pct_biomass_minutes
  ) %>%
  pivot_longer(
    cols = c(mean_pct_lpg_minutes, mean_pct_biomass_minutes),
    names_to = "stove",
    values_to = "average_percent_use"
  ) %>%
  mutate(
    stove = recode(
      stove,
      mean_pct_lpg_minutes = "LPG",
      mean_pct_biomass_minutes = "Biomass"
    )
  )

fig2_monitor_data <- df_days_receive %>%
  transmute(
    days_after_first_receiving,
    before_after,
    LPG = as.integer(lpg_monitored),
    Biomass = as.integer(biomass_monitored)
  ) %>%
  pivot_longer(
    cols = c(LPG, Biomass),
    names_to = "stove",
    values_to = "stove_count"
  ) %>%
  group_by(days_after_first_receiving, before_after, stove) %>%
  summarise(
    number_stoves_monitored = sum(stove_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(days_after_first_receiving, before_after) %>%
  mutate(
    n_household_days = max(number_stoves_monitored, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(n_household_days >= min_household_days_for_day_plot)

write_reviewed_csv(
  bind_rows(
    fig2_minutes_data %>%
      transmute(
        panel = "average_minutes_of_use",
        days_after_first_receiving,
        before_after,
        stove,
        n_household_days,
        value = average_minutes_of_use
      ),
    fig2_percent_data %>%
      transmute(
        panel = "average_percent_use",
        days_after_first_receiving,
        before_after,
        stove,
        n_household_days,
        value = average_percent_use
      ),
    fig2_monitor_data %>%
      transmute(
        panel = "number_stoves_monitored",
        days_after_first_receiving,
        before_after,
        stove,
        n_household_days,
        value = number_stoves_monitored
      )
  ),
  "table_descriptive_stove_composite_plot_data.csv"
)

stove_colors <- c(LPG = "#0072B2", Biomass = "#D55E00")

fig2_minutes <- ggplot(
  fig2_minutes_data,
  aes(x = days_after_first_receiving,
      y = average_minutes_of_use,
      color = stove)
) +
  geom_point(alpha = 0.75, size = 1.3) +
  scale_color_manual(values = stove_colors) +
  labs(
    x = NULL,
    y = "Average minutes of use",
    color = "Stove"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

fig2_percent <- ggplot(
  fig2_percent_data,
  aes(x = days_after_first_receiving,
      y = average_percent_use,
      color = stove)
) +
  geom_point(alpha = 0.75, size = 1.3) +
  scale_color_manual(values = stove_colors) +
  scale_y_continuous(limits = c(0, 100), labels = label_number(suffix = "%")) +
  labs(
    x = NULL,
    y = "Average percent of daily cooking time",
    color = "Stove"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

fig2_monitored <- ggplot(
  fig2_monitor_data,
  aes(x = days_after_first_receiving,
      y = number_stoves_monitored,
      color = stove)
) +
  geom_point(alpha = 0.75, size = 1.3) +
  scale_color_manual(values = stove_colors) +
  labs(
    x = "Days after first receiving LPG through free distribution program",
    y = "Number of stoves monitored",
    color = "Stove"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

fig2_combined <- gridExtra::arrangeGrob(
  fig2_minutes,
  fig2_percent,
  fig2_monitored,
  ncol = 1,
  heights = c(1, 1, 1)
)

save_reviewed_plot(
  fig2_combined,
  "fig_descriptive_stove_use_composite_panel.png",
  width = 7,
  height = 8
)

save_reviewed_plot(
  fig2_minutes,
  "fig_descriptive_stove_minutes_by_day.png",
  width = 7,
  height = 4.5
)
save_reviewed_plot(
  fig2_percent,
  "fig_descriptive_stove_use_percent_by_day.png",
  width = 7,
  height = 4.5
)
save_reviewed_plot(
  fig2_monitored,
  "fig_descriptive_stoves_monitored_by_day.png",
  width = 7,
  height = 4.5
)

################################################################################
# Exclusive LPG use by month after receipt
################################################################################

exclusive_use_by_month_hh <- stove_daily %>%
  filter(
    lpg_enrolled_and_receiving ==
      "receiving LPG through distribution program",
    !is.na(months_after_first_receiving_numeric)
  ) %>%
  group_by(
    fcn_id,
    study_arm_overall,
    months_after_first_receiving_numeric
  ) %>%
  summarise(
    n_daily_records = n(),
    n_days_exclusive_denominator =
      sum(valid_exclusive_use_denominator, na.rm = TRUE),
    pct_days_exclusive_lpg = if_else(
      n_days_exclusive_denominator > 0,
      100 * sum(exclusive_lpg_recalc, na.rm = TRUE) / n_days_exclusive_denominator,
      NA_real_
    ),
    .groups = "drop"
  )

exclusive_use_by_month_summary <- exclusive_use_by_month_hh %>%
  group_by(months_after_first_receiving_numeric) %>%
  summarise(
    n_households = n_distinct(fcn_id),
    mean_pct_days_exclusive_lpg =
      mean(pct_days_exclusive_lpg, na.rm = TRUE),
    median_pct_days_exclusive_lpg =
      median(pct_days_exclusive_lpg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_households >= 3)

write_reviewed_csv(
  exclusive_use_by_month_hh,
  "table_descriptive_stove_exclusive_household_month.csv"
)
write_reviewed_csv(
  exclusive_use_by_month_summary,
  "table_descriptive_stove_exclusive_month_summary.csv"
)

fig_exclusive_lpg_month <- ggplot(
  exclusive_use_by_month_hh %>%
    semi_join(
      exclusive_use_by_month_summary,
      by = "months_after_first_receiving_numeric"
    ),
  aes(
    x = months_after_first_receiving_numeric,
    y = pct_days_exclusive_lpg
  )
) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.45, color = "#4E79A7") +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 17,
    size = 2,
    color = "black"
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom = "linerange",
    color = "black"
  ) +
  scale_y_continuous(limits = c(0, 105), labels = label_number(suffix = "%")) +
  labs(
    x = "Months after first receiving LPG through free distribution program",
    y = "Household-days with exclusive LPG use"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())

save_reviewed_plot(
  fig_exclusive_lpg_month,
  "fig_descriptive_exclusive_lpg_by_month.png",
  width = 7,
  height = 4.5
)

################################################################################
# Supplemental stove-use table by days after receipt
################################################################################

make_days_after_group <- function(days_after_first_receiving) {
  case_when(
    is.na(days_after_first_receiving) ~ NA_character_,
    days_after_first_receiving < 0 ~ "not_yet_received",
    days_after_first_receiving <= 30 ~ "0-30",
    days_after_first_receiving <= 60 ~ "31-60",
    days_after_first_receiving <= 90 ~ "61-90",
    days_after_first_receiving <= 120 ~ "91-120",
    days_after_first_receiving <= 150 ~ "121-150",
    days_after_first_receiving <= 180 ~ "151-180",
    days_after_first_receiving <= 210 ~ "181-210",
    days_after_first_receiving > 210 ~ "211+",
    TRUE ~ NA_character_
  )
}

days_after_levels <- c(
  "not_yet_received", "0-30", "31-60", "61-90", "91-120",
  "121-150", "151-180", "181-210", "211+"
)

supplement_day_data <- stove_daily %>%
  mutate(
    days_after_group = factor(
      make_days_after_group(days_after_first_receiving),
      levels = days_after_levels
    ),
    pct_lpg_events = if_else(
      cooking_events_with_lpg_zero + cooking_events_with_biomass_zero > 0,
      100 * cooking_events_with_lpg_zero /
        (cooking_events_with_lpg_zero + cooking_events_with_biomass_zero),
      NA_real_
    ),
    pct_lpg_minutes = stove_on_min_pc_lpg_zero,
    pct_lpg_minutes_group = case_when(
      is.na(pct_lpg_minutes) ~ NA_character_,
      pct_lpg_minutes == 0 ~ "0",
      pct_lpg_minutes > 0 & pct_lpg_minutes < 20 ~ "1-19",
      pct_lpg_minutes >= 20 & pct_lpg_minutes < 40 ~ "20-39",
      pct_lpg_minutes >= 40 & pct_lpg_minutes < 60 ~ "40-59",
      pct_lpg_minutes >= 60 & pct_lpg_minutes < 80 ~ "60-79",
      pct_lpg_minutes >= 80 & pct_lpg_minutes < 100 ~ "80-99",
      pct_lpg_minutes == 100 ~ "100",
      TRUE ~ NA_character_
    ),
    pct_lpg_minutes_group = factor(
      pct_lpg_minutes_group,
      levels = c("0", "1-19", "20-39", "40-59", "60-79", "80-99", "100")
    )
  ) %>%
  filter(!is.na(days_after_group))

supplement_day_summary <- supplement_day_data %>%
  group_by(days_after_group) %>%
  summarise(
    n_household_days = n(),
    n_households = n_distinct(fcn_id),
    mean_pct_lpg_events = mean(pct_lpg_events, na.rm = TRUE),
    median_pct_lpg_events = median(pct_lpg_events, na.rm = TRUE),
    mean_pct_lpg_minutes = mean(pct_lpg_minutes, na.rm = TRUE),
    median_pct_lpg_minutes = median(pct_lpg_minutes, na.rm = TRUE),
    n_days_exclusive_denominator =
      sum(valid_exclusive_use_denominator, na.rm = TRUE),
    pct_exclusive_lpg_days = if_else(
      n_days_exclusive_denominator > 0,
      100 * sum(exclusive_lpg_recalc, na.rm = TRUE) / n_days_exclusive_denominator,
      NA_real_
    ),
    pct_exclusive_biomass_days = if_else(
      n_days_exclusive_denominator > 0,
      100 * sum(exclusive_biomass_recalc, na.rm = TRUE) / n_days_exclusive_denominator,
      NA_real_
    ),
    pct_mixed_use_days = if_else(
      n_days_exclusive_denominator > 0,
      100 * sum(mixed_use_recalc, na.rm = TRUE) / n_days_exclusive_denominator,
      NA_real_
    ),
    .groups = "drop"
  )

supplement_pct_lpg_distribution <- supplement_day_data %>%
  filter(!is.na(pct_lpg_minutes_group)) %>%
  count(days_after_group, pct_lpg_minutes_group, name = "n_household_days") %>%
  group_by(days_after_group) %>%
  mutate(
    pct_household_days =
      100 * n_household_days / sum(n_household_days, na.rm = TRUE)
  ) %>%
  ungroup()

write_reviewed_csv(
  supplement_day_summary,
  "table_descriptive_stove_day_summary.csv"
)
write_reviewed_csv(
  supplement_pct_lpg_distribution,
  "table_descriptive_stove_lpg_distribution.csv"
)

################################################################################
# Energy-conversion summaries from the RF105 Fig2 script
################################################################################

# These constants are copied from 2_RF105_Fig2_code.R for comparability. The
# previous project notes mix power and energy units, so this output should be treated
# as a reviewed reproduction of that calculation, not a new independent energy
# model.
lpg_efficiency <- 0.67
biomass_efficiency <- 0.128
conv_mj <- 3.6
power_wood <- 6.824 / conv_mj
power_lpg <- 3.4 / conv_mj

energy_day <- df_days_receive %>%
  mutate(
    biomass_energy_mj =
      (stove_on_min_sum_biomass_zero / 60 * power_wood) *
      biomass_efficiency,
    lpg_energy_mj =
      (stove_on_min_sum_lpg_zero / 60 * power_lpg) *
      lpg_efficiency,
    total_energy_mj = biomass_energy_mj + lpg_energy_mj
  )

energy_summary <- energy_day %>%
  summarise(
    n_household_days = n(),
    n_households = n_distinct(fcn_id),
    mean_lpg_energy_mj = mean(lpg_energy_mj, na.rm = TRUE),
    median_lpg_energy_mj = median(lpg_energy_mj, na.rm = TRUE),
    mean_biomass_energy_mj = mean(biomass_energy_mj, na.rm = TRUE),
    median_biomass_energy_mj = median(biomass_energy_mj, na.rm = TRUE),
    mean_total_energy_mj = mean(total_energy_mj, na.rm = TRUE),
    median_total_energy_mj = median(total_energy_mj, na.rm = TRUE)
  )

write_reviewed_csv(
  energy_summary,
  "table_descriptive_stove_energy_summary.csv"
)

energy_plot_data <- energy_day %>%
  filter(days_after_first_receiving %in% fig2_day_summary$days_after_first_receiving) %>%
  group_by(days_after_first_receiving) %>%
  summarise(
    n_household_days = n(),
    LPG = mean(lpg_energy_mj, na.rm = TRUE),
    Biomass = mean(biomass_energy_mj, na.rm = TRUE),
    Total = mean(total_energy_mj, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(LPG, Biomass, Total),
    names_to = "energy_type",
    values_to = "mean_daily_energy_mj"
  )

write_reviewed_csv(
  energy_plot_data,
  "table_descriptive_stove_energy_plot_data.csv"
)

fig_energy_day <- ggplot(
  energy_plot_data,
  aes(
    x = days_after_first_receiving,
    y = mean_daily_energy_mj,
    color = energy_type
  )
) +
  geom_point(alpha = 0.75, size = 1.3) +
  scale_color_manual(
    values = c(LPG = "#0072B2", Biomass = "#D55E00", Total = "#4D4D4D")
  ) +
  labs(
    x = "Days after first receiving LPG through free distribution program",
    y = "Mean daily energy reaching cooking pot (MJ)",
    color = "Energy type"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

save_reviewed_plot(
  fig_energy_day,
  "fig_descriptive_stove_energy_by_day.png",
  width = 7,
  height = 4.5
)

message("RF105 reviewed combined Geocene stove-use analysis complete.")



