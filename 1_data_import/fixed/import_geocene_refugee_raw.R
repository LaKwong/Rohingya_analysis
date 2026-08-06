################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Raw-first refugee Geocene stove-use import
# @Description: Imports Geocene raw exports and derives daily stove-use data.
################################################################################

helper_from_root <- file.path("1_data_import", "fixed", "0_import_raw_helpers.R")
if (file.exists(helper_from_root)) {
  source(helper_from_root)
} else if (!exists("raw_import_project_root", mode = "function")) {
  stop("Run from the project root or source 0_import_raw_helpers.R first.", call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(tidyr)
})

`%notin%` <- Negate(`%in%`)
dataset_scope <- "geocene_refugee_raw"

geocene_timepoint_from_date <- function(date) {
  date <- raw_import_extract_date_any(date)
  timepoint <- raw_import_timepoint_from_date(date)
  early_endline <- is.na(timepoint) &
    !is.na(date) &
    date >= as.Date("2022-01-01") &
    date < as.Date("2022-01-15")
  timepoint[early_endline] <- "endline"
  timepoint
}

geocene_dir <- raw_import_path("2_data_raw", "Geocene_220705")
files <- c(
  events = file.path(geocene_dir, "events_22.csv"),
  mission_logs = file.path(geocene_dir, "mission_logs_22.csv"),
  missions = file.path(geocene_dir, "missions_22.csv"),
  sensors = file.path(geocene_dir, "sensors_22.csv"),
  tags = file.path(geocene_dir, "tags_22.csv")
)

missing_files <- files[!file.exists(files)]
if (length(missing_files)) {
  stop("Missing Geocene raw files: ", paste(missing_files, collapse = "; "), call. = FALSE)
}

df_events <- read.csv(files[["events"]], stringsAsFactors = FALSE, check.names = FALSE)
df_mission_logs <- read.csv(files[["mission_logs"]], stringsAsFactors = FALSE, check.names = FALSE)
df_missions <- read.csv(files[["missions"]], stringsAsFactors = FALSE, check.names = FALSE)
df_sensors <- read.csv(files[["sensors"]], stringsAsFactors = FALSE, check.names = FALSE)
df_tags <- read.csv(files[["tags"]], stringsAsFactors = FALSE, check.names = FALSE)

raw_import_write_rds(
  df_events,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_events_raw.rds")
)
raw_import_write_rds(
  df_missions,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_missions_raw.rds")
)
raw_import_write_rds(
  df_tags,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_tags_raw.rds")
)
raw_import_write_rds(
  df_mission_logs,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_mission_logs_raw.rds")
)
raw_import_write_rds(
  df_sensors,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_sensors_raw.rds")
)

survey_raw_path <- raw_import_path("4_data", "clean_final", "imported_raw", "survey_refugee_household_raw.rds")
if (!file.exists(survey_raw_path)) {
  stop(
    "Missing raw refugee survey import needed for Geocene LPG receipt dates: ",
    survey_raw_path,
    call. = FALSE
  )
}

survey_data <- readRDS(survey_raw_path)
if (!"study_arm_overall" %in% names(survey_data) && "study_arm" %in% names(survey_data)) {
  survey_data$study_arm_overall <- survey_data$study_arm
}
if (!"study_arm_overall" %in% names(survey_data)) {
  survey_data$study_arm_overall <- NA_character_
}
if (!"fcn_id" %in% names(survey_data)) {
  survey_data$fcn_id <- NA_character_
}
survey_data <- survey_data %>%
  select(any_of(c(
    "timepoint", "study_arm_overall", "fcn_id", "camp_id", "block_id",
    "subblock_id", "first_enrolled_lpg", "first_receive_lpg"
  ))) %>%
  mutate(
    fcn_id = as.character(fcn_id),
    study_arm_overall = as.character(study_arm_overall)
  )

df_mission_id_practice <- df_tags %>%
  filter(tag %in% c("practice", "not_normal", "empty")) %>%
  pull(mission_id) %>%
  unique()

df_mission_id_to_analyze_base <- df_tags %>%
  filter(mission_id %notin% df_mission_id_practice) %>%
  mutate(
    study_arm_overall = case_when(
      tag == "study_arm:comparison" ~ "comparison",
      tag == "study_arm:intervention" ~ "intervention",
      tag == "intervention_period:pre_intervention" ~ "intervention",
      tag %in% c("intervention_period:intervention", "intevention_period:intervention") ~ "comparison",
      TRUE ~ NA_character_
    ),
    fuel_type = case_when(
      tag == "fuel_type:biomass" ~ "biomass",
      tag == "fuel_type:lpg" ~ "lpg",
      TRUE ~ NA_character_
    )
  )

mission_id_study_arm_overall <- df_mission_id_to_analyze_base %>%
  filter(!is.na(study_arm_overall)) %>%
  select(mission_id, study_arm_overall) %>%
  distinct()

mission_id_fuel_type <- df_mission_id_to_analyze_base %>%
  filter(!is.na(fuel_type)) %>%
  select(mission_id, fuel_type) %>%
  distinct()

df_mission_id_to_analyze <- mission_id_study_arm_overall %>%
  left_join(mission_id_fuel_type, by = "mission_id")

df_missions_hh_id <- df_mission_id_to_analyze %>%
  left_join(
    df_missions %>%
      separate(
        mission_name,
        into = c("nothing_1", "mission_date_raw", "study_arm_overall_numeric", "hh_id", "mission_start_time_raw"),
        sep = "_",
        remove = FALSE,
        fill = "right",
        extra = "merge"
      ),
    by = "mission_id"
  ) %>%
  mutate(
    hh_id = as.character(hh_id),
    fcn_id = str_extract(hh_id, ".{6}$")
  ) %>%
  select(mission_id, hh_id, fcn_id, mission_date_raw)

df_first_enrolled_lpg_baseline_comparison <- survey_data %>%
  filter(timepoint == "baseline", study_arm_overall == "comparison") %>%
  filter(!is.na(first_enrolled_lpg)) %>%
  mutate(
    first_enrolled_lpg_ymd = dmy(as.character(first_enrolled_lpg)),
    first_receive_lpg_ymd = dmy(as.character(first_receive_lpg))
  ) %>%
  select(fcn_id, first_enrolled_lpg_ymd, first_receive_lpg_ymd)

df_first_enrolled_lpg_midline_intervention <- survey_data %>%
  filter(timepoint == "midline", study_arm_overall == "intervention") %>%
  filter(!is.na(first_enrolled_lpg)) %>%
  mutate(
    first_enrolled_lpg_ymd = mdy(as.character(first_enrolled_lpg)),
    first_receive_lpg_ymd = mdy(as.character(first_receive_lpg))
  ) %>%
  select(fcn_id, first_enrolled_lpg_ymd, first_receive_lpg_ymd)

df_first_enrolled_lpg <- bind_rows(
  df_first_enrolled_lpg_baseline_comparison,
  df_first_enrolled_lpg_midline_intervention
) %>%
  filter(!is.na(fcn_id)) %>%
  distinct()

parse_geocene_time <- function(x) {
  ymd_hms(str_replace(str_replace(as.character(x), "T", " "), "Z$", ""), quiet = TRUE)
}

df_events_stove_on <- df_events %>%
  left_join(df_missions_hh_id, by = "mission_id") %>%
  right_join(df_mission_id_to_analyze, by = "mission_id") %>%
  left_join(df_first_enrolled_lpg, by = "fcn_id") %>%
  mutate(
    start_time = parse_geocene_time(start_time),
    stop_time = parse_geocene_time(stop_time),
    date = as.Date(start_time),
    stove_on_min = as.numeric(difftime(stop_time, start_time, units = "mins")),
    lpg_enrolled_and_receiving = case_when(
      study_arm_overall == "comparison" ~ "receiving LPG through distribution program",
      date < first_receive_lpg_ymd | is.na(first_receive_lpg_ymd) ~ "not yet receiving LPG through distribution program",
      date >= first_receive_lpg_ymd ~ "receiving LPG through distribution program",
      TRUE ~ "not yet receiving LPG through distribution program"
    ),
    lpg_enrolled_and_receiving_previous_rule_order = case_when(
      date < first_receive_lpg_ymd | is.na(first_receive_lpg_ymd) ~ "not yet receiving LPG through distribution program",
      date >= first_receive_lpg_ymd ~ "receiving LPG through distribution program",
      study_arm_overall == "comparison" ~ "receiving LPG through distribution program",
      TRUE ~ "not yet receiving LPG through distribution program"
    ),
    included_by_reviewed_rule_order =
      lpg_enrolled_and_receiving == "receiving LPG through distribution program",
    included_by_previous_rule_order =
      lpg_enrolled_and_receiving_previous_rule_order == "receiving LPG through distribution program",
    included_only_after_comparison_rule_fix =
      included_by_reviewed_rule_order & !included_by_previous_rule_order
  ) %>%
  filter(!is.na(start_time), !is.na(stop_time), !is.na(date), !is.na(fuel_type)) %>%
  distinct()
df_geocene_import_inclusion_audit_by_household <- df_events_stove_on %>%
  group_by(study_arm_overall, fcn_id, hh_id) %>%
  summarise(
    n_events_valid_raw = n(),
    n_household_days_valid_raw = n_distinct(date),
    n_events_included_previous_rule_order =
      sum(included_by_previous_rule_order, na.rm = TRUE),
    n_events_included_reviewed_rule_order =
      sum(included_by_reviewed_rule_order, na.rm = TRUE),
    n_events_excluded_reviewed_rule_order =
      sum(!included_by_reviewed_rule_order, na.rm = TRUE),
    n_events_restored_by_comparison_rule_fix =
      sum(included_only_after_comparison_rule_fix, na.rm = TRUE),
    n_household_days_restored_by_comparison_rule_fix =
      n_distinct(date[included_only_after_comparison_rule_fix]),
    first_restored_date = if (any(included_only_after_comparison_rule_fix, na.rm = TRUE)) {
      min(date[included_only_after_comparison_rule_fix], na.rm = TRUE)
    } else {
      as.Date(NA)
    },
    last_restored_date = if (any(included_only_after_comparison_rule_fix, na.rm = TRUE)) {
      max(date[included_only_after_comparison_rule_fix], na.rm = TRUE)
    } else {
      as.Date(NA)
    },
    missing_first_receive_lpg_ymd_any = any(is.na(first_receive_lpg_ymd)),
    .groups = "drop"
  ) %>%
  arrange(study_arm_overall, fcn_id, hh_id)

df_geocene_import_inclusion_audit_by_arm <- df_geocene_import_inclusion_audit_by_household %>%
  mutate(
    restored_by_comparison_rule_fix =
      n_events_restored_by_comparison_rule_fix > 0,
    valid_restored_fcn_id =
      restored_by_comparison_rule_fix & !is.na(fcn_id) & fcn_id != ""
  ) %>%
  group_by(study_arm_overall) %>%
  summarise(
    n_households = n_distinct(fcn_id),
    n_events_valid_raw = sum(n_events_valid_raw, na.rm = TRUE),
    n_household_days_valid_raw = sum(n_household_days_valid_raw, na.rm = TRUE),
    n_events_included_previous_rule_order =
      sum(n_events_included_previous_rule_order, na.rm = TRUE),
    n_events_included_reviewed_rule_order =
      sum(n_events_included_reviewed_rule_order, na.rm = TRUE),
    n_events_excluded_reviewed_rule_order =
      sum(n_events_excluded_reviewed_rule_order, na.rm = TRUE),
    n_events_restored_by_comparison_rule_fix =
      sum(n_events_restored_by_comparison_rule_fix, na.rm = TRUE),
    n_household_days_restored_by_comparison_rule_fix =
      sum(n_household_days_restored_by_comparison_rule_fix, na.rm = TRUE),
    n_households_restored_by_comparison_rule_fix =
      n_distinct(fcn_id[valid_restored_fcn_id]),
    n_household_rows_restored_by_comparison_rule_fix =
      sum(restored_by_comparison_rule_fix, na.rm = TRUE),
    .groups = "drop"
  )

df_monitor_days_raw <- df_mission_logs %>%
  left_join(df_missions_hh_id, by = "mission_id") %>%
  left_join(df_mission_id_to_analyze, by = "mission_id") %>%
  left_join(df_first_enrolled_lpg, by = "fcn_id") %>%
  mutate(
    phone_time = parse_geocene_time(phone_time),
    meter_time = parse_geocene_time(meter_time),
    date = as.Date(coalesce(phone_time, meter_time)),
    num_samples = suppressWarnings(as.numeric(num_samples)),
    lpg_enrolled_and_receiving = case_when(
      study_arm_overall == "comparison" ~ "receiving LPG through distribution program",
      date < first_receive_lpg_ymd | is.na(first_receive_lpg_ymd) ~ "not yet receiving LPG through distribution program",
      date >= first_receive_lpg_ymd ~ "receiving LPG through distribution program",
      TRUE ~ "not yet receiving LPG through distribution program"
    ),
    community = "refugee",
    data_type = "geocene_stove_monitor_day",
    collection_date = date,
    collection_year = year(date),
    timepoint = geocene_timepoint_from_date(collection_date),
    raw_collection_round = "geocene_220705_raw_exports",
    raw_source_file = "mission_logs_22.csv; missions_22.csv; tags_22.csv",
    raw_source_path = normalizePath(geocene_dir, winslash = "/", mustWork = TRUE)
  ) %>%
  filter(
    !is.na(date),
    !is.na(fcn_id),
    !is.na(fuel_type),
    !is.na(num_samples),
    num_samples > 0
  ) %>%
  distinct(
    study_arm_overall, hh_id, fcn_id, fuel_type, date,
    .keep_all = TRUE
  )

df_monitor_day_denominator_by_household <- df_monitor_days_raw %>%
  group_by(timepoint, study_arm_overall, fcn_id, hh_id, fuel_type) %>%
  summarise(
    n_monitor_stove_days = n(),
    first_monitor_date = min(date, na.rm = TRUE),
    last_monitor_date = max(date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall, fcn_id, fuel_type)

df_monitor_day_denominator_by_arm <- df_monitor_days_raw %>%
  group_by(timepoint, study_arm_overall, fuel_type) %>%
  summarise(
    n_monitor_stove_days = n(),
    n_households = n_distinct(fcn_id),
    first_monitor_date = min(date, na.rm = TRUE),
    last_monitor_date = max(date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall, fuel_type)

df_have_geocene_but_no_survey_data <- df_events_stove_on %>%
  filter(is.na(first_receive_lpg_ymd), study_arm_overall != "comparison") %>%
  distinct(fcn_id, .keep_all = TRUE) %>%
  select(hh_id, fcn_id, date) %>%
  mutate(
    timepoint = geocene_timepoint_from_date(date)
  )

df_have_geocene_but_no_survey_data_fcn_id <- df_have_geocene_but_no_survey_data %>%
  arrange(fcn_id) %>%
  pull(fcn_id)

max_date_mission <- df_events_stove_on %>%
  group_by(mission_id) %>%
  summarise(start = min(start_time), .groups = "drop") %>%
  mutate(max_date = add_with_rollback(start, months(3))) %>%
  select(mission_id, max_date)

df_events_stove_on_lt_3mo <- df_events_stove_on %>%
  left_join(max_date_mission, by = "mission_id") %>%
  filter(stop_time < max_date)

df_geocene_import_inclusion_audit_analysis_eligible_by_household <-
  df_events_stove_on_lt_3mo %>%
  filter(
    !is.na(fcn_id),
    fcn_id != "",
    fcn_id %notin% df_have_geocene_but_no_survey_data_fcn_id
  ) %>%
  group_by(study_arm_overall, fcn_id, hh_id) %>%
  summarise(
    n_events_analysis_eligible = n(),
    n_household_days_analysis_eligible = n_distinct(date),
    n_events_included_previous_rule_order =
      sum(included_by_previous_rule_order, na.rm = TRUE),
    n_events_included_reviewed_rule_order =
      sum(included_by_reviewed_rule_order, na.rm = TRUE),
    n_events_restored_by_comparison_rule_fix =
      sum(included_only_after_comparison_rule_fix, na.rm = TRUE),
    n_household_days_restored_by_comparison_rule_fix =
      n_distinct(date[included_only_after_comparison_rule_fix]),
    first_restored_date = if (any(included_only_after_comparison_rule_fix, na.rm = TRUE)) {
      min(date[included_only_after_comparison_rule_fix], na.rm = TRUE)
    } else {
      as.Date(NA)
    },
    last_restored_date = if (any(included_only_after_comparison_rule_fix, na.rm = TRUE)) {
      max(date[included_only_after_comparison_rule_fix], na.rm = TRUE)
    } else {
      as.Date(NA)
    },
    .groups = "drop"
  ) %>%
  arrange(study_arm_overall, fcn_id, hh_id)

df_geocene_import_inclusion_audit_analysis_eligible_by_arm <-
  df_geocene_import_inclusion_audit_analysis_eligible_by_household %>%
  mutate(
    restored_by_comparison_rule_fix =
      n_events_restored_by_comparison_rule_fix > 0,
    valid_restored_fcn_id =
      restored_by_comparison_rule_fix & !is.na(fcn_id) & fcn_id != ""
  ) %>%
  group_by(study_arm_overall) %>%
  summarise(
    n_households = n_distinct(fcn_id),
    n_events_analysis_eligible = sum(n_events_analysis_eligible, na.rm = TRUE),
    n_household_days_analysis_eligible =
      sum(n_household_days_analysis_eligible, na.rm = TRUE),
    n_events_included_previous_rule_order =
      sum(n_events_included_previous_rule_order, na.rm = TRUE),
    n_events_included_reviewed_rule_order =
      sum(n_events_included_reviewed_rule_order, na.rm = TRUE),
    n_events_restored_by_comparison_rule_fix =
      sum(n_events_restored_by_comparison_rule_fix, na.rm = TRUE),
    n_household_days_restored_by_comparison_rule_fix =
      sum(n_household_days_restored_by_comparison_rule_fix, na.rm = TRUE),
    n_households_restored_by_comparison_rule_fix =
      n_distinct(fcn_id[valid_restored_fcn_id]),
    n_household_rows_restored_by_comparison_rule_fix =
      sum(restored_by_comparison_rule_fix, na.rm = TRUE),
    .groups = "drop"
  )

df_events_stove_on_per_day <- df_events_stove_on_lt_3mo %>%
  filter(fcn_id %notin% df_have_geocene_but_no_survey_data_fcn_id) %>%
  group_by(study_arm_overall, hh_id, fcn_id, fuel_type, date, first_receive_lpg_ymd, lpg_enrolled_and_receiving) %>%
  summarise(n = n(), stove_on_min_sum = sum(stove_on_min, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = fuel_type, values_from = c(n, stove_on_min_sum)) %>%
  mutate(
    stove_on_min_sum_biomass = if ("stove_on_min_sum_biomass" %in% names(.)) as.numeric(stove_on_min_sum_biomass) else NA_real_,
    stove_on_min_sum_lpg = if ("stove_on_min_sum_lpg" %in% names(.)) as.numeric(stove_on_min_sum_lpg) else NA_real_,
    stove_on_min_sum_total = coalesce(stove_on_min_sum_biomass, 0) + coalesce(stove_on_min_sum_lpg, 0),
    stove_on_min_pc_biomass = stove_on_min_sum_biomass / stove_on_min_sum_total * 100,
    stove_on_min_pc_lpg = stove_on_min_sum_lpg / stove_on_min_sum_total * 100,
    days_after_first_receiving = as.numeric(date - first_receive_lpg_ymd),
    months_after_first_receiving = cut(
      days_after_first_receiving,
      breaks = seq(-30, 30 * 36, 30),
      labels = c(-1, seq(0, 35, 1))
    ),
    months_after_first_receiving_numeric = as.numeric(as.character(months_after_first_receiving)),
    exclusive_biomass = if ("n_lpg" %in% names(.)) is.na(n_lpg) else TRUE,
    exclusive_lpg = if ("n_biomass" %in% names(.)) is.na(n_biomass) else TRUE,
    mixed_use = if (all(c("n_biomass", "n_lpg") %in% names(.))) !is.na(n_biomass) & !is.na(n_lpg) else FALSE,
    community = "refugee",
    data_type = "geocene_stove_use_daily",
    collection_date = date,
    collection_year = year(date),
    timepoint = geocene_timepoint_from_date(collection_date),
    raw_collection_round = "geocene_220705_raw_exports",
    raw_source_file = "events_22.csv; missions_22.csv; tags_22.csv",
    raw_source_path = normalizePath(geocene_dir, winslash = "/", mustWork = TRUE)
  ) %>%
  rename(
    cooking_events_with_biomass = any_of("n_biomass"),
    cooking_events_with_lpg = any_of("n_lpg")
  ) %>%
  filter(lpg_enrolled_and_receiving == "receiving LPG through distribution program")

events_path <- raw_import_write_rds(
  df_events_stove_on,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_stove_events_derived_raw.rds")
)
daily_path <- raw_import_write_rds(
  df_events_stove_on_per_day,
  file.path("4_data", "clean_final", "imported_raw", "stove_use_geocene_refugee_daily_raw.rds")
)
monitor_days_path <- raw_import_write_rds(
  df_monitor_days_raw,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_monitor_days_derived_raw.rds")
)
inclusion_audit_household_path <- raw_import_write_csv(
  df_geocene_import_inclusion_audit_by_household,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_import_inclusion_audit_by_household.csv")
)
inclusion_audit_arm_path <- raw_import_write_csv(
  df_geocene_import_inclusion_audit_by_arm,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_import_inclusion_audit_by_arm.csv")
)
inclusion_audit_analysis_household_path <- raw_import_write_csv(
  df_geocene_import_inclusion_audit_analysis_eligible_by_household,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_import_inclusion_audit_analysis_eligible_by_household.csv")
)
inclusion_audit_analysis_arm_path <- raw_import_write_csv(
  df_geocene_import_inclusion_audit_analysis_eligible_by_arm,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_import_inclusion_audit_analysis_eligible_by_arm.csv")
)
monitor_day_household_path <- raw_import_write_csv(
  df_monitor_day_denominator_by_household,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_monitor_day_denominator_by_household.csv")
)
monitor_day_arm_path <- raw_import_write_csv(
  df_monitor_day_denominator_by_arm,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_monitor_day_denominator_by_arm.csv")
)
no_survey_path <- raw_import_write_csv(
  df_have_geocene_but_no_survey_data,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_no_survey_match.csv")
)

manifest <- data.frame(
  dataset_scope = dataset_scope,
  raw_source_path = normalizePath(files, winslash = "/", mustWork = TRUE),
  raw_source_file = basename(files),
  raw_role = names(files),
  raw_collection_round = "geocene_220705_raw_exports",
  default_collection_year = NA_integer_,
  rows_in_raw_file = vapply(files, raw_import_count_csv_rows, integer(1)),
  included_in_raw_import = TRUE,
  exclusion_or_note = "",
  stringsAsFactors = FALSE
)
raw_import_update_manifest(manifest, dataset_scope)

summary <- data.frame(
  dataset = c(
    "geocene_refugee_stove_events_derived_raw",
    "stove_use_geocene_refugee_daily_raw",
    "geocene_refugee_monitor_days_derived_raw",
    "geocene_refugee_import_inclusion_audit_by_household",
    "geocene_refugee_import_inclusion_audit_by_arm",
    "geocene_refugee_import_inclusion_audit_analysis_eligible_by_household",
    "geocene_refugee_import_inclusion_audit_analysis_eligible_by_arm",
    "geocene_refugee_monitor_day_denominator_by_household",
    "geocene_refugee_monitor_day_denominator_by_arm",
    "geocene_refugee_no_survey_match"
  ),
  output_path = c(
    events_path,
    daily_path,
    monitor_days_path,
    inclusion_audit_household_path,
    inclusion_audit_arm_path,
    inclusion_audit_analysis_household_path,
    inclusion_audit_analysis_arm_path,
    monitor_day_household_path,
    monitor_day_arm_path,
    no_survey_path
  ),
  rows = c(
    nrow(df_events_stove_on),
    nrow(df_events_stove_on_per_day),
    nrow(df_monitor_days_raw),
    nrow(df_geocene_import_inclusion_audit_by_household),
    nrow(df_geocene_import_inclusion_audit_by_arm),
    nrow(df_geocene_import_inclusion_audit_analysis_eligible_by_household),
    nrow(df_geocene_import_inclusion_audit_analysis_eligible_by_arm),
    nrow(df_monitor_day_denominator_by_household),
    nrow(df_monitor_day_denominator_by_arm),
    nrow(df_have_geocene_but_no_survey_data)
  ),
  stringsAsFactors = FALSE
)
raw_import_write_csv(
  summary,
  file.path("4_data", "clean_final", "imported_raw", "geocene_refugee_raw_import_summary.csv")
)

message("Wrote ", daily_path)

