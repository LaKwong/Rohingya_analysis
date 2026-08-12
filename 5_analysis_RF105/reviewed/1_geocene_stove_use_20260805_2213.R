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
#   The clean_final daily stove-use, imported raw stove-event, and imported raw
#   mission-log monitor files are used as analysis inputs. This is the only
#   active reviewed Geocene analysis script.
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

add_all_arms_rows <- function(df) {
  df_arm <- df %>%
    mutate(study_arm_overall = as.character(study_arm_overall))

  df_all_arms <- df_arm %>%
    filter(!is.na(study_arm_overall), study_arm_overall %in% arm_levels) %>%
    mutate(study_arm_overall = "all_arms")

  bind_rows(df_arm, df_all_arms)
}

if (!requireNamespace("gridExtra", quietly = TRUE)) {
  stop("Install the gridExtra package before running this combined Geocene script.")
}

suppressPackageStartupMessages({
  library(gridExtra)
})

################################################################################
# Consolidated workflow and raw-source audit
################################################################################

geocene_pathway_notes <- tibble(
  note_id = 1:5,
  note = c(
    "1_geocene_stove_use_20260805_2213.R is the only active reviewed Geocene analysis script.",
    "Standalone files previously copied under reviewed/5_geocene_analysis have been removed from the active workflow to avoid duplicate or contradictory stove-use code.",
    "Raw Geocene exports are imported by 1_data_import/fixed/import_geocene_refugee_raw.R and cleaned by 3_data_cleaning/fixed/clean_geocene_refugee_20260805_2141.R.",
    "This script uses clean_final daily stove-use, imported raw stove-event, and imported raw mission-log files to reconcile stove-use events to monitored-day denominators.",
    "Event-days that cannot be matched to a mission-log denominator by fcn_id, date, fuel type, and source mission are a blocking error."
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
geocene_lpg_date_correction_audit <- read_imported_raw_csv(
  "geocene_refugee_lpg_date_correction_audit.csv"
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
)
write_reviewed_csv(
  geocene_lpg_date_correction_audit,
  "table_descriptive_geocene_lpg_date_correction_audit.csv",
  subfolder = "qa"
)

################################################################################
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
  "lpg_available_for_analysis",
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
    lpg_available_for_analysis = as.logical(lpg_available_for_analysis),
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

# The clean-final monitor-day file stores one row per mission-log record. The
# mission-log date can be a download/log date rather than each calendar day that
# was actually monitored. Build the reviewed denominator from the imported raw
# mission logs by expanding each source mission from its mission start date to
# its log/download date, with an event-date fallback for mission logs missing a
# parsable mission_date_raw. Event-days must then match the denominator by
# fcn_id, date, fuel type, and source mission information.
geocene_timepoint_windows <- tibble(
  timepoint = c("baseline", "midline", "endline"),
  start_date = as.Date(c("2019-09-01", "2020-09-01", "2022-01-15")),
  end_date = as.Date(c("2020-04-15", "2020-12-15", "2022-08-15"))
)

geocene_timepoint_from_date_reviewed <- function(x) {
  x_date <- as.Date(x)
  out <- rep(NA_character_, length(x_date))
  for (ii in seq_len(nrow(geocene_timepoint_windows))) {
    in_window <- !is.na(x_date) &
      x_date >= geocene_timepoint_windows$start_date[[ii]] &
      x_date <= geocene_timepoint_windows$end_date[[ii]]
    out[in_window] <- geocene_timepoint_windows$timepoint[[ii]]
  }
  as_ordered_timepoint(out)
}

derive_lpg_available_for_analysis <- function(study_arm_overall, timepoint, date, first_receive_lpg_ymd) {
  arm <- str_squish(str_to_lower(as.character(study_arm_overall)))
  tp <- as.character(timepoint)
  date <- as.Date(date)
  first_receive <- as.Date(first_receive_lpg_ymd)
  case_when(
    arm == "comparison" ~ TRUE,
    arm == "intervention" & tp == "baseline" ~ FALSE,
    arm == "intervention" &
      tp %in% c("midline", "endline") &
      !is.na(first_receive) & !is.na(date) & date >= first_receive ~ TRUE,
    TRUE ~ FALSE
  )
}

parse_mission_date_raw <- function(x) {
  suppressWarnings(lubridate::ymd(as.character(x)))
}

pmax_date <- function(...) {
  out <- do.call(pmax, c(list(...), na.rm = TRUE))
  out[is.infinite(out)] <- NA_real_
  as.Date(out, origin = "1970-01-01")
}

make_date_sequence <- function(start_date, end_date) {
  if (is.na(start_date) || is.na(end_date) || end_date < start_date) {
    return(as.Date(NA))
  }
  seq.Date(start_date, end_date, by = "day")
}

stove_event_source_file <- file.path(
  dir_clean_final,
  "imported_raw",
  "geocene_refugee_stove_events_derived_raw.rds"
)
stove_monitor_source_file <- file.path(
  dir_clean_final,
  "imported_raw",
  "geocene_refugee_monitor_days_derived_raw.rds"
)

geocene_reconciliation_source_audit <- tibble(
  source_role = c("clean_final_daily_event_days", "imported_raw_stove_events", "imported_raw_monitor_mission_logs"),
  source_file = c(file_stove_daily, stove_event_source_file, stove_monitor_source_file),
  exists = file.exists(source_file)
)

write_reviewed_csv(
  geocene_reconciliation_source_audit,
  "table_descriptive_geocene_reconciliation_source_audit.csv",
  subfolder = "qa"
)

missing_reconciliation_sources <- geocene_reconciliation_source_audit %>%
  filter(!exists) %>%
  pull(source_file)

if (length(missing_reconciliation_sources) > 0) {
  stop(
    "Required Geocene denominator reconciliation inputs are missing: ",
    paste(missing_reconciliation_sources, collapse = ", "),
    call. = FALSE
  )
}

geocene_baseline_arm_lookup <- readRDS(file_survey_refugee_household) %>%
  as_tibble() %>%
  clean_timepoint_arm() %>%
  filter(timepoint == "baseline") %>%
  transmute(
    fcn_id = str_squish(as.character(fcn_id)),
    baseline_hh_id = str_squish(as.character(hh_id)),
    baseline_study_arm_overall = as.character(study_arm_overall)
  ) %>%
  filter(!is.na(fcn_id), fcn_id != "") %>%
  distinct(fcn_id, .keep_all = TRUE)
stove_events_raw <- readRDS(stove_event_source_file) %>%
  as_tibble()

analysis_event_day_fuel_keys <- bind_rows(
  stove_daily_event_days %>%
    filter(replace_na(cooking_events_with_lpg, 0) > 0) %>%
    transmute(
      fcn_id = str_squish(as.character(fcn_id)),
      event_date = as.Date(date),
      fuel_type = "lpg"
    ),
  stove_daily_event_days %>%
    filter(replace_na(cooking_events_with_biomass, 0) > 0) %>%
    transmute(
      fcn_id = str_squish(as.character(fcn_id)),
      event_date = as.Date(date),
      fuel_type = "biomass"
    )
) %>%
  filter(!is.na(fcn_id), fcn_id != "", !is.na(event_date)) %>%
  distinct()

stove_events_clean_all <- stove_events_raw %>%
  mutate(
    mission_id = str_squish(as.character(mission_id)),
    fcn_id = str_squish(as.character(fcn_id)),
    hh_id = str_squish(as.character(hh_id)),
    fuel_type = str_squish(str_to_lower(as.character(fuel_type))),
    event_date = as.Date(date),
    start_time = as.POSIXct(start_time),
    stop_time = as.POSIXct(stop_time),
    stove_on_min = suppressWarnings(as.numeric(stove_on_min)),
    study_arm_overall = str_squish(str_to_lower(as.character(study_arm_overall))),
    lpg_enrolled_and_receiving = as.character(lpg_enrolled_and_receiving),
    first_receive_lpg_ymd = as.Date(first_receive_lpg_ymd)
  ) %>%
  left_join(geocene_baseline_arm_lookup, by = "fcn_id") %>%
  mutate(
    study_arm_overall = coalesce(baseline_study_arm_overall, study_arm_overall),
    hh_id = coalesce(baseline_hh_id, hh_id),
    lpg_available_for_analysis = derive_lpg_available_for_analysis(
      study_arm_overall,
      geocene_timepoint_from_date_reviewed(event_date),
      event_date,
      first_receive_lpg_ymd
    )
  ) %>%
  select(-baseline_study_arm_overall, -baseline_hh_id) %>%
  filter(
    !is.na(fcn_id), fcn_id != "",
    !is.na(event_date),
    fuel_type %in% c("lpg", "biomass")
  ) %>%
  semi_join(
    analysis_event_day_fuel_keys,
    by = c("fcn_id", "event_date", "fuel_type")
  )

geocene_lpg_unavailable_event_rows <- stove_events_clean_all %>%
  filter(fuel_type == "lpg", !lpg_available_for_analysis) %>%
  mutate(timepoint = geocene_timepoint_from_date_reviewed(event_date)) %>%
  arrange(timepoint, study_arm_overall, fcn_id, event_date)

stove_events_clean <- stove_events_clean_all %>%
  filter(fuel_type != "lpg" | lpg_available_for_analysis)

stove_event_bounds_by_source_mission <- stove_events_clean %>%
  group_by(mission_id, fcn_id, fuel_type) %>%
  summarise(
    first_event_date = min(event_date, na.rm = TRUE),
    last_event_date = max(event_date, na.rm = TRUE),
    n_event_rows = n(),
    .groups = "drop"
  )

stove_event_day_fuel <- stove_events_clean %>%
  group_by(fcn_id, event_date, fuel_type) %>%
  summarise(
    hh_id_event = first(hh_id[!is.na(hh_id) & hh_id != ""], default = NA_character_),
    study_arm_event = first(study_arm_overall[!is.na(study_arm_overall) & study_arm_overall != ""], default = NA_character_),
    first_receive_lpg_ymd_event = first(first_receive_lpg_ymd[!is.na(first_receive_lpg_ymd)], default = as.Date(NA)),
    lpg_available_for_analysis_event = any(lpg_available_for_analysis, na.rm = TRUE),
    n_source_missions_with_events = n_distinct(mission_id),
    source_mission_ids_with_events = paste(sort(unique(mission_id)), collapse = "; "),
    cooking_events = n(),
    stove_on_min_sum = sum(stove_on_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(date = event_date)

stove_monitor_days_clean <- readRDS(stove_monitor_source_file) %>%
  as_tibble() %>%
  mutate(
    mission_id = str_squish(as.character(mission_id)),
    fcn_id = str_squish(as.character(fcn_id)),
    hh_id = str_squish(as.character(hh_id)),
    fuel_type = str_squish(str_to_lower(as.character(fuel_type))),
    study_arm_overall = str_squish(str_to_lower(as.character(study_arm_overall))),
    mission_start_date_raw = parse_mission_date_raw(mission_date_raw),
    log_date = as.Date(date),
    phone_date = as.Date(phone_time),
    meter_date = as.Date(meter_time),
    first_receive_lpg_ymd = as.Date(first_receive_lpg_ymd),
    num_samples = suppressWarnings(as.numeric(num_samples)),
    lpg_enrolled_and_receiving = as.character(lpg_enrolled_and_receiving),
    raw_collection_round = as.character(raw_collection_round),
    raw_source_file = as.character(raw_source_file)
  ) %>%
  left_join(geocene_baseline_arm_lookup, by = "fcn_id") %>%
  mutate(
    study_arm_overall = coalesce(baseline_study_arm_overall, study_arm_overall),
    hh_id = coalesce(baseline_hh_id, hh_id)
  ) %>%
  select(-baseline_study_arm_overall, -baseline_hh_id) %>%
  filter(
    !is.na(fcn_id), fcn_id != "",
    fuel_type %in% c("lpg", "biomass"),
    !is.na(num_samples),
    num_samples > 0
  ) %>%
  left_join(
    stove_event_bounds_by_source_mission,
    by = c("mission_id", "fcn_id", "fuel_type")
  ) %>%
  mutate(
    mission_start_date = coalesce(mission_start_date_raw, first_event_date, log_date),
    mission_end_date = pmax_date(
      mission_start_date,
      log_date,
      phone_date,
      last_event_date
    ),
    mission_start_date_source = case_when(
      !is.na(mission_start_date_raw) ~ "mission_date_raw",
      is.na(mission_start_date_raw) & !is.na(first_event_date) ~ "first_event_date_fallback",
      is.na(mission_start_date_raw) & is.na(first_event_date) & !is.na(log_date) ~ "log_date_fallback",
      TRUE ~ "missing"
    ),
    mission_end_date_source = case_when(
      !is.na(phone_date) & phone_date == mission_end_date ~ "phone_time_log_date",
      !is.na(log_date) & log_date == mission_end_date ~ "monitor_log_date",
      !is.na(last_event_date) & last_event_date == mission_end_date ~ "last_event_date_fallback",
      !is.na(mission_start_date) & mission_start_date == mission_end_date ~ "mission_start_date",
      TRUE ~ "missing"
    )
  ) %>%
  filter(
    !is.na(mission_start_date),
    !is.na(mission_end_date),
    mission_end_date >= mission_start_date
  )

monitor_denominator_window_long <- stove_monitor_days_clean %>%
  rowwise() %>%
  mutate(date = list(make_date_sequence(mission_start_date, mission_end_date))) %>%
  ungroup() %>%
  unnest(date) %>%
  filter(!is.na(date)) %>%
  mutate(
    timepoint = geocene_timepoint_from_date_reviewed(date),
    study_arm_overall = factor(study_arm_overall, levels = arm_levels),
    denominator_variant = "mission_window_expanded",
    monitor_denominator_source = "mission_logs_window_expanded_num_samples_gt0"
  ) %>%
  filter(!is.na(timepoint)) %>%
  distinct(fcn_id, fuel_type, date, mission_id, .keep_all = TRUE)

monitor_denominator_log_date_long <- stove_monitor_days_clean %>%
  transmute(
    fcn_id, hh_id, fuel_type, date = log_date, mission_id,
    study_arm_overall = factor(study_arm_overall, levels = arm_levels),
    first_receive_lpg_ymd, lpg_enrolled_and_receiving,
    raw_collection_round, raw_source_file, num_samples,
    mission_start_date, mission_end_date,
    mission_start_date_source, mission_end_date_source,
    denominator_variant = "mission_log_date_only",
    monitor_denominator_source = "mission_logs_log_date_only"
  ) %>%
  filter(!is.na(date)) %>%
  mutate(timepoint = geocene_timepoint_from_date_reviewed(date)) %>%
  filter(!is.na(timepoint))

monitor_denominator_start_date_long <- stove_monitor_days_clean %>%
  transmute(
    fcn_id, hh_id, fuel_type, date = mission_start_date, mission_id,
    study_arm_overall = factor(study_arm_overall, levels = arm_levels),
    first_receive_lpg_ymd, lpg_enrolled_and_receiving,
    raw_collection_round, raw_source_file, num_samples,
    mission_start_date, mission_end_date,
    mission_start_date_source, mission_end_date_source,
    denominator_variant = "mission_start_date_only",
    monitor_denominator_source = "mission_logs_start_date_only"
  ) %>%
  filter(!is.na(date)) %>%
  mutate(timepoint = geocene_timepoint_from_date_reviewed(date)) %>%
  filter(!is.na(timepoint))

monitor_denominator_event_day_long <- stove_event_day_fuel %>%
  transmute(
    fcn_id,
    hh_id = hh_id_event,
    fuel_type,
    date,
    mission_id = source_mission_ids_with_events,
    study_arm_overall = factor(study_arm_event, levels = arm_levels),
    first_receive_lpg_ymd = first_receive_lpg_ymd_event,
    lpg_enrolled_and_receiving = "receiving LPG through distribution program",
    lpg_available_for_analysis = lpg_available_for_analysis_event,
    raw_collection_round = "geocene_event_day_sensitivity",
    raw_source_file = "events_22.csv; missions_22.csv; tags_22.csv",
    num_samples = NA_real_,
    mission_start_date = date,
    mission_end_date = date,
    mission_start_date_source = "event_day",
    mission_end_date_source = "event_day",
    denominator_variant = "event_day_only",
    monitor_denominator_source = "event_days_only_sensitivity"
  ) %>%
  mutate(timepoint = geocene_timepoint_from_date_reviewed(date)) %>%
  filter(!is.na(timepoint))

make_monitor_day_wide <- function(monitor_day_long) {
  monitor_day_long %>%
    group_by(fcn_id, timepoint, study_arm_overall, date) %>%
    summarise(
      hh_id = first(hh_id[!is.na(hh_id) & hh_id != ""], default = NA_character_),
      first_receive_lpg_ymd = first(first_receive_lpg_ymd[!is.na(first_receive_lpg_ymd)], default = as.Date(NA)),
      lpg_enrolled_and_receiving = first(lpg_enrolled_and_receiving[!is.na(lpg_enrolled_and_receiving)], default = NA_character_),
      raw_collection_round = paste(sort(unique(raw_collection_round)), collapse = "; "),
      raw_source_file = paste(sort(unique(raw_source_file)), collapse = "; "),
      source_mission_ids = paste(sort(unique(mission_id)), collapse = "; "),
      n_source_mission_logs = n_distinct(mission_id),
      n_source_fuel_log_rows = n(),
      lpg_monitored = any(fuel_type == "lpg"),
      biomass_monitored = any(fuel_type == "biomass"),
      n_monitor_stove_days = n_distinct(fuel_type),
      monitor_window_start = min(mission_start_date, na.rm = TRUE),
      monitor_window_end = max(mission_end_date, na.rm = TRUE),
      monitor_denominator_source = paste(sort(unique(monitor_denominator_source)), collapse = "; "),
      denominator_variant = paste(sort(unique(denominator_variant)), collapse = "; "),
      .groups = "drop"
    ) %>%
    mutate(
      lpg_available_for_analysis = derive_lpg_available_for_analysis(
        study_arm_overall,
        timepoint,
        date,
        first_receive_lpg_ymd
      ),
      days_after_first_receiving =
        suppressWarnings(as.numeric(as.Date(date) - first_receive_lpg_ymd)),
      months_after_first_receiving = cut(
        days_after_first_receiving,
        breaks = seq(-30, 30 * 36, 30),
        labels = c(-1, seq(0, 35, 1))
      ),
      months_after_first_receiving_numeric =
        suppressWarnings(as.numeric(as.character(months_after_first_receiving)))
    )
}

event_day_for_join <- stove_event_day_fuel %>%
  select(fcn_id, date, fuel_type, cooking_events, stove_on_min_sum) %>%
  pivot_wider(
    names_from = fuel_type,
    values_from = c(cooking_events, stove_on_min_sum),
    values_fill = 0
  ) %>%
  rename(
    cooking_events_with_biomass = any_of("cooking_events_biomass"),
    cooking_events_with_lpg = any_of("cooking_events_lpg"),
    stove_on_min_sum_biomass = any_of("stove_on_min_sum_biomass"),
    stove_on_min_sum_lpg = any_of("stove_on_min_sum_lpg")
  )

for (needed_col in c(
  "cooking_events_with_biomass", "cooking_events_with_lpg",
  "stove_on_min_sum_biomass", "stove_on_min_sum_lpg"
)) {
  if (!needed_col %in% names(event_day_for_join)) {
    event_day_for_join[[needed_col]] <- 0
  }
}

stove_monitor_days <- monitor_denominator_window_long
stove_monitor_day_wide <- make_monitor_day_wide(stove_monitor_days)

stove_events_without_monitor_day <- stove_event_day_fuel %>%
  anti_join(
    stove_monitor_days %>% distinct(fcn_id, date, fuel_type),
    by = c("fcn_id", "date", "fuel_type")
  ) %>%
  arrange(fcn_id, fuel_type, date)

if (nrow(stove_events_without_monitor_day) == 0) {
  stove_events_without_monitor_day_by_arm <- tibble(
    timepoint = factor(character(), levels = timepoint_levels, ordered = TRUE),
    study_arm_event = character(),
    fuel_type = character(),
    n_event_fuel_days_without_monitor_day = integer(),
    n_households_without_monitor_day = integer(),
    first_unmatched_date = as.Date(character()),
    last_unmatched_date = as.Date(character())
  )
  stove_events_without_monitor_day_by_household <- tibble(
    timepoint = factor(character(), levels = timepoint_levels, ordered = TRUE),
    study_arm_event = character(),
    fcn_id = character(),
    hh_id_event = character(),
    fuel_type = character(),
    n_event_fuel_days_without_monitor_day = integer(),
    first_unmatched_date = as.Date(character()),
    last_unmatched_date = as.Date(character()),
    source_mission_ids_with_events = character()
  )
} else {
  stove_events_without_monitor_day_by_arm <- stove_events_without_monitor_day %>%
    mutate(timepoint = geocene_timepoint_from_date_reviewed(date)) %>%
    group_by(timepoint, study_arm_event, fuel_type) %>%
    summarise(
      n_event_fuel_days_without_monitor_day = n(),
      n_households_without_monitor_day = n_distinct(fcn_id),
      first_unmatched_date = min(date, na.rm = TRUE),
      last_unmatched_date = max(date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(timepoint, study_arm_event, fuel_type)

  stove_events_without_monitor_day_by_household <- stove_events_without_monitor_day %>%
    mutate(timepoint = geocene_timepoint_from_date_reviewed(date)) %>%
    group_by(timepoint, study_arm_event, fcn_id, hh_id_event, fuel_type) %>%
    summarise(
      n_event_fuel_days_without_monitor_day = n_distinct(date),
      first_unmatched_date = min(date, na.rm = TRUE),
      last_unmatched_date = max(date, na.rm = TRUE),
      source_mission_ids_with_events = paste(sort(unique(source_mission_ids_with_events)), collapse = "; "),
      .groups = "drop"
    ) %>%
    arrange(timepoint, study_arm_event, fcn_id, fuel_type)
}

write_reviewed_csv(
  stove_events_without_monitor_day,
  "table_descriptive_geocene_monitor_denominator_unmatched_event_days.csv",
  subfolder = "qa"
)
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

if (nrow(stove_events_without_monitor_day) > 0) {
  stop(
    "Geocene stove event-days do not all match the mission-log monitor denominator. ",
    "Review table_descriptive_geocene_monitor_denominator_unmatched_event_days.csv before using stove-use estimates.",
    call. = FALSE
  )
}

make_denominator_variant_summary <- function(monitor_day_long, variant_label) {
  make_monitor_day_wide(monitor_day_long) %>%
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
      lpg_available_for_analysis = replace_na(lpg_available_for_analysis, FALSE),
      lpg_recorded_raw = lpg_monitored & replace_na(cooking_events_with_lpg > 0, FALSE),
      lpg_recorded = lpg_available_for_analysis & lpg_recorded_raw,
      biomass_recorded = biomass_monitored & cooking_events_with_biomass > 0,
      observed_stove_use_day = lpg_recorded | biomass_recorded,
      n_stoves_with_recorded_use =
        replace_na(as.integer(lpg_recorded), 0L) +
        replace_na(as.integer(biomass_recorded), 0L),
      valid_exclusive_use_denominator =
        lpg_available_for_analysis & observed_stove_use_day,
      exclusive_lpg_recalc = lpg_available_for_analysis & valid_exclusive_use_denominator & lpg_recorded & !biomass_recorded,
      exclusive_biomass_recalc = valid_exclusive_use_denominator & biomass_recorded & !lpg_recorded,
      mixed_use_recalc = valid_exclusive_use_denominator & biomass_recorded & lpg_recorded,
      denominator_variant = variant_label
    ) %>%
    group_by(denominator_variant, timepoint, study_arm_overall) %>%
    summarise(
      n_monitor_household_days = n(),
      n_households = n_distinct(fcn_id),
      n_lpg_monitored_days = sum(lpg_monitored, na.rm = TRUE),
      n_biomass_monitored_days = sum(biomass_monitored, na.rm = TRUE),
      n_exclusive_use_denominator_days = sum(valid_exclusive_use_denominator, na.rm = TRUE),
      n_exclusive_lpg_days = sum(exclusive_lpg_recalc, na.rm = TRUE),
      pct_exclusive_lpg_days = if_else(
        n_exclusive_use_denominator_days > 0,
        100 * n_exclusive_lpg_days / n_exclusive_use_denominator_days,
        NA_real_
      ),
      n_exclusive_biomass_days = sum(exclusive_biomass_recalc, na.rm = TRUE),
      pct_exclusive_biomass_days = if_else(
        n_exclusive_use_denominator_days > 0,
        100 * n_exclusive_biomass_days / n_exclusive_use_denominator_days,
        NA_real_
      ),
      n_mixed_use_days = sum(mixed_use_recalc, na.rm = TRUE),
      pct_mixed_use_days = if_else(
        n_exclusive_use_denominator_days > 0,
        100 * n_mixed_use_days / n_exclusive_use_denominator_days,
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    arrange(denominator_variant, timepoint, study_arm_overall)
}

geocene_denominator_sensitivity_summary <- bind_rows(
  make_denominator_variant_summary(
    monitor_denominator_log_date_long,
    "mission_log_date_only"
  ),
  make_denominator_variant_summary(
    monitor_denominator_start_date_long,
    "mission_start_date_only"
  ),
  make_denominator_variant_summary(
    monitor_denominator_event_day_long,
    "event_day_only"
  ),
  make_denominator_variant_summary(
    monitor_denominator_window_long,
    "mission_window_expanded_primary"
  )
)

write_reviewed_csv(
  geocene_denominator_sensitivity_summary,
  "table_descriptive_geocene_denominator_sensitivity.csv",
  subfolder = "qa"
)

geocene_monitor_window_source_summary <- stove_monitor_days_clean %>%
  count(
    study_arm_overall,
    fuel_type,
    mission_start_date_source,
    mission_end_date_source,
    name = "n_source_monitor_rows"
  ) %>%
  arrange(study_arm_overall, fuel_type, mission_start_date_source, mission_end_date_source)

write_reviewed_csv(
  geocene_monitor_window_source_summary,
  "table_descriptive_geocene_monitor_window_source_summary.csv",
  subfolder = "qa"
)

stove_daily <- stove_monitor_day_wide %>%
  left_join(event_day_for_join, by = c("fcn_id", "date")) %>%
  mutate(
    lpg_available_for_analysis = replace_na(lpg_available_for_analysis, FALSE),
    cooking_events_with_lpg_raw = suppressWarnings(as.numeric(cooking_events_with_lpg)),
    stove_on_min_sum_lpg_raw = suppressWarnings(as.numeric(stove_on_min_sum_lpg)),
    cooking_events_with_lpg = if_else(
      lpg_monitored,
      if_else(lpg_available_for_analysis, replace_na(cooking_events_with_lpg_raw, 0), 0),
      NA_real_
    ),
    cooking_events_with_biomass = if_else(
      biomass_monitored,
      replace_na(suppressWarnings(as.numeric(cooking_events_with_biomass)), 0),
      NA_real_
    ),
    stove_on_min_sum_lpg = if_else(
      lpg_monitored,
      if_else(lpg_available_for_analysis, replace_na(stove_on_min_sum_lpg_raw, 0), 0),
      NA_real_
    ),
    stove_on_min_sum_biomass = if_else(
      biomass_monitored,
      replace_na(suppressWarnings(as.numeric(stove_on_min_sum_biomass)), 0),
      NA_real_
    ),
    lpg_recorded_raw = lpg_monitored & replace_na(cooking_events_with_lpg_raw > 0, FALSE),
    lpg_recorded_unavailable_for_analysis = lpg_recorded_raw & !lpg_available_for_analysis,
    lpg_recorded = lpg_available_for_analysis & lpg_recorded_raw,
    biomass_recorded = biomass_monitored & cooking_events_with_biomass > 0,
    observed_stove_use_day = lpg_recorded | biomass_recorded,
    n_stoves_with_recorded_use =
      replace_na(as.integer(lpg_recorded), 0L) +
      replace_na(as.integer(biomass_recorded), 0L),
    n_sensor_window_stove_days =
      replace_na(as.integer(lpg_monitored), 0L) +
      replace_na(as.integer(biomass_monitored), 0L),
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
    valid_exclusive_use_denominator =
      lpg_available_for_analysis & observed_stove_use_day,
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
    monitor_denominator_source = "mission_logs_window_expanded_num_samples_gt0"
  )
# This reviewed household-day dataset is the downstream source for analyses of
# the amount of time stoves were in use. It is derived here after the Geocene
# QA/recode steps so later scripts do not independently reprocess clean_final.
geocene_stove_use_daily_analysis_dataset <- stove_daily %>%
  select(any_of(c(
    "fcn_id", "hh_id", "timepoint", "study_arm_overall", "date",
    "first_receive_lpg_ymd", "days_after_first_receiving",
    "months_after_first_receiving_numeric", "lpg_enrolled_and_receiving",
    "lpg_available_for_analysis", "lpg_monitored", "biomass_monitored",
    "observed_stove_use_day", "n_stoves_with_recorded_use",
    "valid_exclusive_use_denominator", "n_monitor_stove_days", "n_sensor_window_stove_days",
    "monitor_denominator_source", "lpg_recorded", "lpg_recorded_raw",
    "lpg_recorded_unavailable_for_analysis", "biomass_recorded",
    "cooking_events_with_lpg_raw", "stove_on_min_sum_lpg_raw",
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
# Monitoring scope summary
################################################################################

safe_min_positive <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x) & x > 0]
  if (length(x) == 0) NA_real_ else min(x)
}

safe_max_numeric <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) NA_real_ else max(x)
}

stove_daily_monitoring_scope_data <- stove_daily %>%
  mutate(
    n_sensor_window_stoves_household_day =
      replace_na(as.integer(n_sensor_window_stove_days), 0L),
    n_stoves_monitored_household_day =
      replace_na(as.integer(n_stoves_with_recorded_use), 0L),
    days_after_first_receiving =
      suppressWarnings(as.numeric(days_after_first_receiving))
  )

stove_daily_monitoring_scope_observed <- stove_daily_monitoring_scope_data %>%
  filter(observed_stove_use_day)

stove_daily_monitoring_scope_sensor_window <- stove_daily_monitoring_scope_data %>%
  filter(n_sensor_window_stoves_household_day > 0)

geocene_stoves_monitored_by_days_after_receipt <-
  stove_daily_monitoring_scope_observed %>%
  group_by(days_after_first_receiving) %>%
  summarise(
    n_stoves_monitored = sum(n_stoves_monitored_household_day, na.rm = TRUE),
    n_household_days_monitored = n(),
    n_households_monitored = n_distinct(fcn_id),
    .groups = "drop"
  ) %>%
  filter(n_stoves_monitored > 0) %>%
  arrange(is.na(days_after_first_receiving), days_after_first_receiving)

geocene_stoves_monitored_by_days_after_receipt_by_arm <-
  stove_daily_monitoring_scope_observed %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall, days_after_first_receiving) %>%
  summarise(
    n_stoves_monitored = sum(n_stoves_monitored_household_day, na.rm = TRUE),
    n_household_days_monitored = n(),
    n_households_monitored = n_distinct(fcn_id),
    .groups = "drop"
  ) %>%
  filter(n_stoves_monitored > 0) %>%
  arrange(timepoint, study_arm_overall, is.na(days_after_first_receiving), days_after_first_receiving)

geocene_sensor_window_stoves_by_days_after_receipt <-
  stove_daily_monitoring_scope_sensor_window %>%
  group_by(days_after_first_receiving) %>%
  summarise(
    n_sensor_window_stoves_monitored = sum(n_sensor_window_stoves_household_day, na.rm = TRUE),
    n_household_days_sensor_window = n(),
    n_household_days_without_recorded_stove_use = sum(!observed_stove_use_day, na.rm = TRUE),
    n_households_sensor_window = n_distinct(fcn_id),
    .groups = "drop"
  ) %>%
  arrange(is.na(days_after_first_receiving), days_after_first_receiving)

geocene_sensor_window_stoves_by_days_after_receipt_by_arm <-
  stove_daily_monitoring_scope_sensor_window %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall, days_after_first_receiving) %>%
  summarise(
    n_sensor_window_stoves_monitored = sum(n_sensor_window_stoves_household_day, na.rm = TRUE),
    n_household_days_sensor_window = n(),
    n_household_days_without_recorded_stove_use = sum(!observed_stove_use_day, na.rm = TRUE),
    n_households_sensor_window = n_distinct(fcn_id),
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall, is.na(days_after_first_receiving), days_after_first_receiving)

monitoring_scope_relative_overall <-
  geocene_stoves_monitored_by_days_after_receipt %>%
  filter(!is.na(days_after_first_receiving)) %>%
  summarise(
    min_n_stoves_monitored_per_days_after_receipt_gt0 =
      safe_min_positive(n_stoves_monitored),
    max_n_stoves_monitored_per_days_after_receipt_gt0 =
      safe_max_numeric(n_stoves_monitored),
    n_days_after_receipt_with_gt0_stoves_monitored = n(),
    .groups = "drop"
  )

monitoring_scope_relative_by_arm <-
  geocene_stoves_monitored_by_days_after_receipt_by_arm %>%
  filter(!is.na(days_after_first_receiving)) %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    min_n_stoves_monitored_per_days_after_receipt_gt0 =
      safe_min_positive(n_stoves_monitored),
    max_n_stoves_monitored_per_days_after_receipt_gt0 =
      safe_max_numeric(n_stoves_monitored),
    n_days_after_receipt_with_gt0_stoves_monitored = n(),
    .groups = "drop"
  )

monitoring_scope_overall <- stove_daily_monitoring_scope_observed %>%
  summarise(
    summary_scope = "overall",
    timepoint = NA_character_,
    study_arm_overall = NA_character_,
    n_refugee_households_monitored = n_distinct(fcn_id),
    n_household_days_monitored = n(),
    n_household_days_with_days_after_first_receiving =
      sum(!is.na(days_after_first_receiving)),
    n_household_days_missing_days_after_first_receiving =
      sum(is.na(days_after_first_receiving)),
    min_n_stoves_monitored_per_household_day_gt0 =
      safe_min_positive(n_stoves_monitored_household_day),
    max_n_stoves_monitored_per_household_day_gt0 =
      safe_max_numeric(n_stoves_monitored_household_day),
    max_days_after_first_receiving_to_monitoring =
      safe_max_numeric(days_after_first_receiving),
    max_days_after_first_receiving_to_lpg_available_monitoring =
      safe_max_numeric(days_after_first_receiving[lpg_available_for_analysis]),
    .groups = "drop"
  ) %>%
  bind_cols(monitoring_scope_relative_overall)

monitoring_scope_by_arm <- stove_daily_monitoring_scope_observed %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_refugee_households_monitored = n_distinct(fcn_id),
    n_household_days_monitored = n(),
    n_household_days_with_days_after_first_receiving =
      sum(!is.na(days_after_first_receiving)),
    n_household_days_missing_days_after_first_receiving =
      sum(is.na(days_after_first_receiving)),
    min_n_stoves_monitored_per_household_day_gt0 =
      safe_min_positive(n_stoves_monitored_household_day),
    max_n_stoves_monitored_per_household_day_gt0 =
      safe_max_numeric(n_stoves_monitored_household_day),
    max_days_after_first_receiving_to_monitoring =
      safe_max_numeric(days_after_first_receiving),
    max_days_after_first_receiving_to_lpg_available_monitoring =
      safe_max_numeric(days_after_first_receiving[lpg_available_for_analysis]),
    .groups = "drop"
  ) %>%
  mutate(
    summary_scope = if_else(study_arm_overall == "all_arms", "timepoint_all_arms", "timepoint_arm"),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  ) %>%
  left_join(
    monitoring_scope_relative_by_arm %>%
      mutate(
        timepoint = as.character(timepoint),
        study_arm_overall = as.character(study_arm_overall)
      ),
    by = c("timepoint", "study_arm_overall")
  ) %>%
  select(names(monitoring_scope_overall))

geocene_monitoring_scope_summary <- bind_rows(
  monitoring_scope_overall,
  monitoring_scope_by_arm
) %>%
  mutate(
    monitoring_denominator_note =
      "Household-days are counted as monitored only when LPG or biomass stove use was recorded.",
    stove_count_note =
      "Household-day stove counts sum LPG and biomass stoves with recorded use. Sensor-window days without recorded stove use are retained only in QA/sensitivity outputs."
  ) %>%
  arrange(summary_scope, timepoint, study_arm_overall)

sensor_window_monitoring_scope_overall <- stove_daily_monitoring_scope_sensor_window %>%
  summarise(
    summary_scope = "overall",
    timepoint = NA_character_,
    study_arm_overall = NA_character_,
    n_household_days_sensor_window = n(),
    n_household_days_with_recorded_stove_use = sum(observed_stove_use_day, na.rm = TRUE),
    n_household_days_without_recorded_stove_use = sum(!observed_stove_use_day, na.rm = TRUE),
    n_sensor_window_stove_days = sum(n_sensor_window_stoves_household_day, na.rm = TRUE),
    pct_sensor_window_household_days_without_recorded_stove_use = if_else(
      n_household_days_sensor_window > 0,
      100 * n_household_days_without_recorded_stove_use / n_household_days_sensor_window,
      NA_real_
    ),
    .groups = "drop"
  )

sensor_window_monitoring_scope_by_arm <- stove_daily_monitoring_scope_sensor_window %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_household_days_sensor_window = n(),
    n_household_days_with_recorded_stove_use = sum(observed_stove_use_day, na.rm = TRUE),
    n_household_days_without_recorded_stove_use = sum(!observed_stove_use_day, na.rm = TRUE),
    n_sensor_window_stove_days = sum(n_sensor_window_stoves_household_day, na.rm = TRUE),
    pct_sensor_window_household_days_without_recorded_stove_use = if_else(
      n_household_days_sensor_window > 0,
      100 * n_household_days_without_recorded_stove_use / n_household_days_sensor_window,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    summary_scope = if_else(study_arm_overall == "all_arms", "timepoint_all_arms", "timepoint_arm"),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  ) %>%
  select(names(sensor_window_monitoring_scope_overall))

geocene_sensor_window_monitoring_scope_summary <- bind_rows(
  sensor_window_monitoring_scope_overall,
  sensor_window_monitoring_scope_by_arm
) %>%
  mutate(
    denominator_note =
      "QA only: mission-log window-expanded household-days before excluding days with no recorded LPG or biomass stove use."
  ) %>%
  arrange(summary_scope, timepoint, study_arm_overall)

geocene_days_after_receipt_reconciliation_overall <- tibble(
  summary_scope = "overall",
  timepoint = NA_character_,
  study_arm_overall = NA_character_,
  n_household_days_monitoring_scope = nrow(stove_daily_monitoring_scope_observed),
  n_household_days_days_after_receipt_table =
    sum(geocene_stoves_monitored_by_days_after_receipt$n_household_days_monitored, na.rm = TRUE),
  n_household_days_with_days_after_first_receiving =
    sum(!is.na(stove_daily_monitoring_scope_observed$days_after_first_receiving)),
  n_household_days_missing_days_after_first_receiving =
    sum(is.na(stove_daily_monitoring_scope_observed$days_after_first_receiving))
)

geocene_days_after_receipt_reconciliation_by_arm <-
  stove_daily_monitoring_scope_observed %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_household_days_monitoring_scope = n(),
    n_household_days_with_days_after_first_receiving =
      sum(!is.na(days_after_first_receiving)),
    n_household_days_missing_days_after_first_receiving =
      sum(is.na(days_after_first_receiving)),
    .groups = "drop"
  ) %>%
  mutate(
    summary_scope = if_else(study_arm_overall == "all_arms", "timepoint_all_arms", "timepoint_arm"),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  ) %>%
  left_join(
    geocene_stoves_monitored_by_days_after_receipt_by_arm %>%
      group_by(timepoint, study_arm_overall) %>%
      summarise(
        n_household_days_days_after_receipt_table =
          sum(n_household_days_monitored, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        timepoint = as.character(timepoint),
        study_arm_overall = as.character(study_arm_overall)
      ),
    by = c("timepoint", "study_arm_overall")
  ) %>%
  mutate(
    n_household_days_days_after_receipt_table =
      replace_na(n_household_days_days_after_receipt_table, 0L)
  ) %>%
  select(names(geocene_days_after_receipt_reconciliation_overall))

geocene_days_after_receipt_reconciliation <- bind_rows(
  geocene_days_after_receipt_reconciliation_overall,
  geocene_days_after_receipt_reconciliation_by_arm
) %>%
  mutate(
    n_household_days_difference =
      n_household_days_monitoring_scope - n_household_days_days_after_receipt_table,
    tables_reconcile = n_household_days_difference == 0,
    reconciliation_note =
      "Days-after-receipt tables include observed stove-use household-days with missing first_receive_lpg_ymd as a blank days_after_first_receiving row so n_household_days_monitored sums to the public monitoring-scope denominator."
  ) %>%
  arrange(summary_scope, timepoint, study_arm_overall)

write_reviewed_csv(
  geocene_monitoring_scope_summary,
  "table_descriptive_geocene_monitoring_scope_summary.csv"
)
write_reviewed_csv(
  geocene_days_after_receipt_reconciliation,
  "table_descriptive_geocene_days_after_receipt_reconciliation.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  geocene_sensor_window_monitoring_scope_summary,
  "table_descriptive_geocene_sensor_window_monitoring_scope_summary.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  geocene_stoves_monitored_by_days_after_receipt,
  "table_descriptive_geocene_stoves_monitored_by_days_after_receipt.csv"
)
write_reviewed_csv(
  geocene_stoves_monitored_by_days_after_receipt_by_arm,
  "table_descriptive_geocene_stoves_monitored_by_days_after_receipt_by_arm.csv"
)
write_reviewed_csv(
  geocene_sensor_window_stoves_by_days_after_receipt,
  "table_descriptive_geocene_sensor_window_stoves_by_days_after_receipt.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  geocene_sensor_window_stoves_by_days_after_receipt_by_arm,
  "table_descriptive_geocene_sensor_window_stoves_by_days_after_receipt_by_arm.csv",
  subfolder = "qa"
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

geocene_lpg_unavailable_event_days <- geocene_lpg_unavailable_event_rows %>%
  distinct(fcn_id, event_date, .keep_all = TRUE) %>%
  mutate(
    date = event_date,
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    days_after_first_receiving = as.numeric(event_date - first_receive_lpg_ymd),
    exclusion_reason = "Program LPG was not available for this arm/timepoint under the analysis rule."
  ) %>%
  select(any_of(c(
    "fcn_id", "hh_id", "date", "timepoint", "study_arm_overall",
    "first_receive_lpg_ymd", "days_after_first_receiving",
    "lpg_enrolled_and_receiving", "lpg_available_for_analysis",
    "fuel_type", "mission_id", "stove_on_min", "start_time", "stop_time",
    "raw_collection_round", "raw_source_file", "exclusion_reason"
  ))) %>%
  arrange(timepoint, study_arm_overall, fcn_id, date)

geocene_lpg_unavailable_event_counts <- geocene_lpg_unavailable_event_rows %>%
  mutate(
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  ) %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_lpg_event_days_not_available_for_analysis = n_distinct(fcn_id, event_date),
    n_lpg_event_rows_not_available_for_analysis = n(),
    n_households_with_lpg_event_days_not_available_for_analysis = n_distinct(fcn_id),
    exclusion_reason = "Program LPG was not available for this arm/timepoint under the analysis rule.",
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall)

geocene_lpg_unavailable_event_summary <- geocene_lpg_unavailable_event_counts %>%
  select(
    timepoint,
    study_arm_overall,
    n_lpg_event_days_not_available_for_analysis,
    n_lpg_event_rows_not_available_for_analysis,
    n_households_with_lpg_event_days_not_available_for_analysis,
    exclusion_reason
  )

write_reviewed_csv(
  geocene_lpg_unavailable_event_days,
  "table_descriptive_geocene_lpg_unavailable_event_days.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  geocene_lpg_unavailable_event_summary,
  "table_descriptive_geocene_lpg_unavailable_event_summary.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  geocene_lpg_unavailable_event_counts,
  "table_descriptive_geocene_lpg_unavailable_event_counts.csv",
  subfolder = "qa"
)

stove_events_without_monitor_day_distinct <- stove_events_without_monitor_day %>%
  mutate(
    timepoint = geocene_timepoint_from_date_reviewed(date),
    study_arm_overall = factor(study_arm_event, levels = arm_levels),
    hh_id = hh_id_event
  ) %>%
  distinct(fcn_id, date, fuel_type, timepoint, study_arm_overall, .keep_all = TRUE)
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
    "event household-days without matching monitor-day denominator",
    "exclusive-use denominator rows without recorded stove use",
    "exclusive LPG rows when LPG unavailable for analysis",
    "baseline intervention rows marked LPG available for analysis",
    "baseline intervention exclusive LPG days",
    "baseline intervention biomass event-days retained",
    "LPG event-days excluded because LPG unavailable for analysis"
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
    nrow(stove_events_without_monitor_day_distinct),
    sum(stove_daily$valid_exclusive_use_denominator & !stove_daily$observed_stove_use_day, na.rm = TRUE),
    sum(
      stove_daily$exclusive_lpg_recalc & !stove_daily$lpg_available_for_analysis,
      na.rm = TRUE
    ),
    sum(
      stove_daily$timepoint == "baseline" &
        stove_daily$study_arm_overall == "intervention" &
        stove_daily$lpg_available_for_analysis,
      na.rm = TRUE
    ),
    sum(
      stove_daily$timepoint == "baseline" &
        stove_daily$study_arm_overall == "intervention" &
        stove_daily$exclusive_lpg_recalc,
      na.rm = TRUE
    ),
    sum(
      stove_daily$timepoint == "baseline" &
        stove_daily$study_arm_overall == "intervention" &
        stove_daily$biomass_recorded,
      na.rm = TRUE
    ),
    nrow(geocene_lpg_unavailable_event_days)
  )
)

write_reviewed_csv(
  qa_checks,
  "table_descriptive_geocene_quality_checks.csv",
  subfolder = "qa"
)

sample_counts <- stove_daily %>%
  filter(observed_stove_use_day) %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_daily_records = n(),
    n_household_days_monitored = n(),
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
  filter(observed_stove_use_day) %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_daily_records = n(),
    n_household_days_monitored = n(),
    n_households = n_distinct(fcn_id),
    n_days_exclusive_denominator =
      sum(valid_exclusive_use_denominator, na.rm = TRUE),
    n_lpg_available_days = sum(lpg_available_for_analysis, na.rm = TRUE),
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
  left_join(
    geocene_lpg_unavailable_event_counts %>%
      select(timepoint, study_arm_overall, n_lpg_event_days_not_available_for_analysis),
    by = c("timepoint", "study_arm_overall")
  ) %>%
  mutate(
    n_lpg_event_days_not_available_for_analysis =
      replace_na(n_lpg_event_days_not_available_for_analysis, 0L),
    lpg_availability_note = case_when(
      study_arm_overall == "intervention" & timepoint == "baseline" ~
        "Program LPG structurally unavailable to intervention households at baseline; exclusive LPG percentage is not applicable.",
      n_lpg_available_days == 0 ~
        "No household-days with program LPG available for analysis.",
      TRUE ~ NA_character_
    )
  ) %>%
  arrange(timepoint, study_arm_overall)

write_reviewed_csv(
  daily_summary,
  "table_descriptive_geocene_daily_summary.csv"
)

post_lpg_exclusive_use_denominator <- stove_daily %>%
  filter(
    lpg_available_for_analysis,
    !is.na(days_after_first_receiving),
    days_after_first_receiving >= 0,
    observed_stove_use_day,
    valid_exclusive_use_denominator
  )

post_lpg_exclusive_use_summary <- post_lpg_exclusive_use_denominator %>%
  summarise(
    summary_scope = "all_arms_all_timepoints_after_lpg_receipt",
    included_arms = paste(
      arm_levels[arm_levels %in% unique(as.character(study_arm_overall))],
      collapse = "; "
    ),
    included_timepoints = paste(
      timepoint_levels[timepoint_levels %in% unique(as.character(timepoint))],
      collapse = "; "
    ),
    n_household_days = n(),
    n_households = n_distinct(fcn_id),
    n_exclusive_lpg_days = sum(exclusive_lpg_recalc, na.rm = TRUE),
    pct_exclusive_lpg_days = if_else(
      n_household_days > 0,
      100 * n_exclusive_lpg_days / n_household_days,
      NA_real_
    ),
    n_exclusive_biomass_days = sum(exclusive_biomass_recalc, na.rm = TRUE),
    pct_exclusive_biomass_days = if_else(
      n_household_days > 0,
      100 * n_exclusive_biomass_days / n_household_days,
      NA_real_
    ),
    n_mixed_use_days = sum(mixed_use_recalc, na.rm = TRUE),
    pct_mixed_use_days = if_else(
      n_household_days > 0,
      100 * n_mixed_use_days / n_household_days,
      NA_real_
    ),
    denominator_note = paste(
      "Denominator is observed stove-use household-days from both study arms and all survey timepoints",
      "on or after the household's recorded first LPG receipt date, restricted to days when",
      "program LPG was available for analysis."
    ),
    .groups = "drop"
  )

post_lpg_exclusive_use_denominator_by_arm_timepoint <-
  post_lpg_exclusive_use_denominator %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_household_days = n(),
    n_households = n_distinct(fcn_id),
    n_exclusive_lpg_days = sum(exclusive_lpg_recalc, na.rm = TRUE),
    n_exclusive_biomass_days = sum(exclusive_biomass_recalc, na.rm = TRUE),
    n_mixed_use_days = sum(mixed_use_recalc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall)

write_reviewed_csv(
  post_lpg_exclusive_use_summary,
  "table_descriptive_geocene_post_lpg_exclusive_use_summary.csv"
)
write_reviewed_csv(
  post_lpg_exclusive_use_denominator_by_arm_timepoint,
  "table_descriptive_geocene_post_lpg_exclusive_use_denominator_by_arm_timepoint.csv",
  subfolder = "qa"
)

if (nrow(post_lpg_exclusive_use_summary) != 1 ||
    post_lpg_exclusive_use_summary$n_household_days[[1]] <= 0) {
  stop("Post-LPG exclusive-use summary has no eligible monitored household-days.", call. = FALSE)
}

if (post_lpg_exclusive_use_summary$n_household_days[[1]] !=
    post_lpg_exclusive_use_denominator_by_arm_timepoint %>%
      filter(study_arm_overall %in% arm_levels) %>%
      pull(n_household_days) %>%
      sum(na.rm = TRUE)) {
  stop("Post-LPG exclusive-use denominator does not reconcile across arm/timepoint QA rows.", call. = FALSE)
}

if (any(!post_lpg_exclusive_use_denominator$observed_stove_use_day, na.rm = TRUE)) {
  stop("Post-LPG exclusive-use denominator includes household-days without recorded stove use.", call. = FALSE)
}

if (any(
  post_lpg_exclusive_use_summary$n_household_days !=
    post_lpg_exclusive_use_summary$n_exclusive_lpg_days +
    post_lpg_exclusive_use_summary$n_exclusive_biomass_days +
    post_lpg_exclusive_use_summary$n_mixed_use_days,
  na.rm = TRUE
)) {
  stop("Post-LPG exclusive-use categories do not sum to the monitored household-day denominator.", call. = FALSE)
}

if (any(
  post_lpg_exclusive_use_denominator_by_arm_timepoint$n_household_days !=
    post_lpg_exclusive_use_denominator_by_arm_timepoint$n_exclusive_lpg_days +
    post_lpg_exclusive_use_denominator_by_arm_timepoint$n_exclusive_biomass_days +
    post_lpg_exclusive_use_denominator_by_arm_timepoint$n_mixed_use_days,
  na.rm = TRUE
)) {
  stop("Post-LPG exclusive-use arm/timepoint categories do not sum to the monitored household-day denominator.", call. = FALSE)
}

post_lpg_30day_exclusive_use_denominator <- stove_daily %>%
  filter(
    lpg_available_for_analysis,
    !is.na(days_after_first_receiving),
    days_after_first_receiving >= 30,
    observed_stove_use_day,
    valid_exclusive_use_denominator
  )

post_lpg_30day_exclusive_use_summary <-
  post_lpg_30day_exclusive_use_denominator %>%
  summarise(
    summary_scope = "all_arms_all_timepoints_30plus_days_after_lpg_receipt",
    included_arms = paste(
      arm_levels[arm_levels %in% unique(as.character(study_arm_overall))],
      collapse = "; "
    ),
    included_timepoints = paste(
      timepoint_levels[timepoint_levels %in% unique(as.character(timepoint))],
      collapse = "; "
    ),
    minimum_days_after_first_receiving = 30L,
    n_household_days = n(),
    n_households = n_distinct(fcn_id),
    n_exclusive_lpg_days = sum(exclusive_lpg_recalc, na.rm = TRUE),
    pct_exclusive_lpg_days = if_else(
      n_household_days > 0,
      100 * n_exclusive_lpg_days / n_household_days,
      NA_real_
    ),
    n_exclusive_biomass_days = sum(exclusive_biomass_recalc, na.rm = TRUE),
    pct_exclusive_biomass_days = if_else(
      n_household_days > 0,
      100 * n_exclusive_biomass_days / n_household_days,
      NA_real_
    ),
    n_mixed_use_days = sum(mixed_use_recalc, na.rm = TRUE),
    pct_mixed_use_days = if_else(
      n_household_days > 0,
      100 * n_mixed_use_days / n_household_days,
      NA_real_
    ),
    denominator_note = paste(
      "Denominator is observed stove-use household-days from both study arms and all survey timepoints",
      "at least 30 days after the household's recorded first LPG receipt date, restricted to days when",
      "program LPG was available for analysis."
    ),
    .groups = "drop"
  )

post_lpg_30day_exclusive_use_denominator_by_arm_timepoint <-
  post_lpg_30day_exclusive_use_denominator %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_household_days = n(),
    n_households = n_distinct(fcn_id),
    n_exclusive_lpg_days = sum(exclusive_lpg_recalc, na.rm = TRUE),
    n_exclusive_biomass_days = sum(exclusive_biomass_recalc, na.rm = TRUE),
    n_mixed_use_days = sum(mixed_use_recalc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall)

write_reviewed_csv(
  post_lpg_30day_exclusive_use_summary,
  "table_descriptive_geocene_post_lpg_30day_exclusive_use_summary.csv"
)
write_reviewed_csv(
  post_lpg_30day_exclusive_use_denominator_by_arm_timepoint,
  "table_descriptive_geocene_post_lpg_30day_exclusive_use_denominator_by_arm_timepoint.csv",
  subfolder = "qa"
)

if (nrow(post_lpg_30day_exclusive_use_summary) != 1 ||
    post_lpg_30day_exclusive_use_summary$n_household_days[[1]] <= 0) {
  stop("Post-LPG 30-day exclusive-use summary has no eligible monitored household-days.", call. = FALSE)
}

if (post_lpg_30day_exclusive_use_summary$n_household_days[[1]] !=
    post_lpg_30day_exclusive_use_denominator_by_arm_timepoint %>%
      filter(study_arm_overall %in% arm_levels) %>%
      pull(n_household_days) %>%
      sum(na.rm = TRUE)) {
  stop("Post-LPG 30-day exclusive-use denominator does not reconcile across arm/timepoint QA rows.", call. = FALSE)
}

if (any(!post_lpg_30day_exclusive_use_denominator$observed_stove_use_day, na.rm = TRUE)) {
  stop("Post-LPG 30-day exclusive-use denominator includes household-days without recorded stove use.", call. = FALSE)
}

if (any(
  post_lpg_30day_exclusive_use_summary$n_household_days !=
    post_lpg_30day_exclusive_use_summary$n_exclusive_lpg_days +
    post_lpg_30day_exclusive_use_summary$n_exclusive_biomass_days +
    post_lpg_30day_exclusive_use_summary$n_mixed_use_days,
  na.rm = TRUE
)) {
  stop("Post-LPG 30-day exclusive-use categories do not sum to the monitored household-day denominator.", call. = FALSE)
}

if (any(
  post_lpg_30day_exclusive_use_denominator_by_arm_timepoint$n_household_days !=
    post_lpg_30day_exclusive_use_denominator_by_arm_timepoint$n_exclusive_lpg_days +
    post_lpg_30day_exclusive_use_denominator_by_arm_timepoint$n_exclusive_biomass_days +
    post_lpg_30day_exclusive_use_denominator_by_arm_timepoint$n_mixed_use_days,
  na.rm = TRUE
)) {
  stop("Post-LPG 30-day exclusive-use arm/timepoint categories do not sum to the monitored household-day denominator.", call. = FALSE)
}

baseline_intervention_summary <- daily_summary %>%
  filter(timepoint == "baseline", study_arm_overall == "intervention")

if (nrow(baseline_intervention_summary) != 1 ||
    baseline_intervention_summary$n_exclusive_lpg_days[[1]] != 0) {
  stop(
    "Baseline intervention Geocene summary must have zero exclusive-LPG days.",
    call. = FALSE
  )
}

if (any(stove_daily$exclusive_lpg_recalc & !stove_daily$lpg_available_for_analysis, na.rm = TRUE)) {
  stop("Exclusive LPG was counted on a day when program LPG was unavailable for analysis.", call. = FALSE)
}

if (any(
  stove_daily$timepoint == "baseline" &
    stove_daily$study_arm_overall == "intervention" &
    stove_daily$lpg_available_for_analysis,
  na.rm = TRUE
)) {
  stop("Baseline intervention rows must not be marked LPG-available for analysis.", call. = FALSE)
}

baseline_intervention_biomass_event_days_raw <- stove_event_day_fuel %>%
  filter(
    fuel_type == "biomass",
    study_arm_event == "intervention",
    geocene_timepoint_from_date_reviewed(date) == "baseline"
  ) %>%
  summarise(n = n_distinct(fcn_id, date), .groups = "drop") %>%
  pull(n)

baseline_intervention_biomass_event_days_retained <- stove_daily %>%
  filter(timepoint == "baseline", study_arm_overall == "intervention", biomass_recorded) %>%
  summarise(n = n_distinct(fcn_id, date), .groups = "drop") %>%
  pull(n)

if (baseline_intervention_biomass_event_days_raw > 0 &&
    baseline_intervention_biomass_event_days_retained == 0) {
  stop("Baseline intervention biomass event-days were dropped during LPG availability correction.", call. = FALSE)
}

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
    lpg_available_for_analysis,
    observed_stove_use_day,
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
    LPG = as.integer(lpg_recorded),
    Biomass = as.integer(biomass_recorded)
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
    y = "Number of stoves with recorded use",
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
    lpg_available_for_analysis,
    observed_stove_use_day,
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
  filter(lpg_available_for_analysis, observed_stove_use_day) %>%
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

# These constants are carried forward from the prior RF105 Figure 2 calculation
# for comparability. The previous project notes mix power and energy units, so
# this output should be treated as a reviewed reproduction of that calculation,
# not a new independent energy model.
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
