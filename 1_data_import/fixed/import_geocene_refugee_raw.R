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

geocene_exports <- tibble(
  source_folder = c("Geocene_210204", "Geocene_220705"),
  raw_collection_round = c("geocene_210204_raw_exports", "geocene_220705_raw_exports"),
  events = c("events.csv", "events_22.csv"),
  mission_logs = c("mission_logs.csv", "mission_logs_22.csv"),
  missions = c("missions.csv", "missions_22.csv"),
  sensors = c("sensors.csv", "sensors_22.csv"),
  tags = c("tags.csv", "tags_22.csv")
) %>%
  mutate(source_path = raw_import_path("2_data_raw", source_folder))

geocene_export_files <- bind_rows(lapply(seq_len(nrow(geocene_exports)), function(ii) {
  export_row <- geocene_exports[ii, ]
  tibble(
    source_folder = export_row$source_folder,
    raw_collection_round = export_row$raw_collection_round,
    source_path = export_row$source_path,
    raw_role = c("events", "mission_logs", "missions", "sensors", "tags"),
    raw_source_file = c(
      export_row$events,
      export_row$mission_logs,
      export_row$missions,
      export_row$sensors,
      export_row$tags
    )
  ) %>%
    mutate(raw_source_path = file.path(source_path, raw_source_file))
}))

missing_files <- geocene_export_files %>%
  filter(!file.exists(raw_source_path))
if (nrow(missing_files) > 0) {
  stop(
    "Missing Geocene raw files: ",
    paste(missing_files$raw_source_path, collapse = "; "),
    call. = FALSE
  )
}

read_geocene_role <- function(raw_role) {
  bind_rows(lapply(seq_len(nrow(geocene_export_files)), function(ii) {
    file_row <- geocene_export_files[ii, ]
    if (file_row$raw_role != raw_role) return(NULL)
    read.csv(file_row$raw_source_path, stringsAsFactors = FALSE, check.names = FALSE) %>%
      mutate(
        source_folder = file_row$source_folder,
        raw_collection_round = file_row$raw_collection_round,
        raw_source_file = file_row$raw_source_file,
        raw_source_path = normalizePath(file_row$raw_source_path, winslash = "/", mustWork = TRUE)
      )
  }))
}

make_mission_key <- function(mission_id, mission_name) {
  mission_id <- str_squish(as.character(mission_id))
  mission_name <- str_squish(as.character(mission_name))
  if_else(
    is.na(mission_name) | mission_name == "",
    mission_id,
    paste(mission_id, mission_name, sep = " | ")
  )
}

collapse_row_values <- function(...) {
  values <- as.data.frame(list(...), stringsAsFactors = FALSE)
  apply(values, 1, function(row_values) {
    row_values <- as.character(row_values)
    row_values <- row_values[!is.na(row_values) & row_values != ""]
    paste(sort(unique(row_values)), collapse = "; ")
  })
}

df_events <- read_geocene_role("events")
df_mission_logs <- read_geocene_role("mission_logs")
df_missions <- read_geocene_role("missions") %>%
  mutate(
    mission_id = str_squish(as.character(mission_id)),
    mission_name = str_squish(as.character(mission_name)),
    mission_key = make_mission_key(mission_id, mission_name)
  )
df_sensors <- read_geocene_role("sensors")
df_tags <- read_geocene_role("tags")

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
    "subblock_id", "first_enrolled_lpg", "first_receive_lpg",
    "raw_collection_round", "raw_survey_version", "raw_source_file",
    "raw_source_path"
  ))) %>%
  mutate(
    timepoint = as.character(timepoint),
    fcn_id = as.character(fcn_id),
    study_arm_overall = as.character(study_arm_overall),
    first_enrolled_lpg = as.character(first_enrolled_lpg),
    first_receive_lpg = as.character(first_receive_lpg)
  )

geocene_lpg_date_correction_audit <- survey_data %>%
  filter(
    timepoint == "midline",
    study_arm_overall == "intervention",
    fcn_id == "123970",
    str_squish(first_enrolled_lpg) %in% c("1/1/1982", "Jan 1, 1982", "1982-01-01")
  ) %>%
  transmute(
    fcn_id,
    timepoint,
    study_arm_overall,
    variable = "first_enrolled_lpg",
    old_value = first_enrolled_lpg,
    corrected_value = "1/1/2019",
    correction_reason = "Raw midline survey value for fcn_id 123970 was confirmed to be a data-entry error; correct first LPG enrollment date is 1/1/2019.",
    raw_collection_round = if ("raw_collection_round" %in% names(.)) raw_collection_round else NA_character_,
    raw_survey_version = if ("raw_survey_version" %in% names(.)) raw_survey_version else NA_character_,
    raw_source_file = if ("raw_source_file" %in% names(.)) raw_source_file else NA_character_,
    raw_source_path = if ("raw_source_path" %in% names(.)) raw_source_path else NA_character_
  )

survey_data <- survey_data %>%
  mutate(
    first_enrolled_lpg = if_else(
      timepoint == "midline" &
        study_arm_overall == "intervention" &
        fcn_id == "123970" &
        str_squish(first_enrolled_lpg) %in% c("1/1/1982", "Jan 1, 1982", "1982-01-01"),
      "1/1/2019",
      first_enrolled_lpg
    )
  )

geocene_lpg_date_correction_audit_path <- raw_import_write_csv(
  geocene_lpg_date_correction_audit,
  file.path(
    "4_data", "clean_final", "imported_raw",
    "geocene_refugee_lpg_date_correction_audit.csv"
  )
)

df_tags_with_mission <- df_tags %>%
  mutate(
    mission_id = str_squish(as.character(mission_id)),
    tag = str_squish(as.character(tag))
  ) %>%
  left_join(
    df_missions %>%
      select(source_folder, mission_id, mission_name, mission_key),
    by = c("source_folder", "mission_id")
  ) %>%
  mutate(
    mission_key = if_else(
      is.na(mission_key) | mission_key == "",
      make_mission_key(mission_id, mission_name),
      mission_key
    ),
    raw_tags_file = raw_source_file,
    raw_tags_path = raw_source_path
  )

df_mission_key_practice <- df_tags_with_mission %>%
  filter(tag %in% c("practice", "not_normal", "empty")) %>%
  distinct(source_folder, mission_key)

df_mission_id_to_analyze_base <- df_tags_with_mission %>%
  anti_join(df_mission_key_practice, by = c("source_folder", "mission_key")) %>%
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
  group_by(source_folder, mission_id, mission_name, mission_key, study_arm_overall) %>%
  summarise(
    raw_tags_file = paste(sort(unique(raw_tags_file)), collapse = "; "),
    raw_tags_path = paste(sort(unique(raw_tags_path)), collapse = "; "),
    .groups = "drop"
  )

mission_id_fuel_type <- df_mission_id_to_analyze_base %>%
  filter(!is.na(fuel_type)) %>%
  group_by(source_folder, mission_id, mission_name, mission_key, fuel_type) %>%
  summarise(
    raw_tags_file_fuel = paste(sort(unique(raw_tags_file)), collapse = "; "),
    raw_tags_path_fuel = paste(sort(unique(raw_tags_path)), collapse = "; "),
    .groups = "drop"
  )

df_mission_id_to_analyze <- mission_id_study_arm_overall %>%
  left_join(
    mission_id_fuel_type,
    by = c("source_folder", "mission_id", "mission_name", "mission_key")
  ) %>%
  mutate(
    raw_tags_file = collapse_row_values(raw_tags_file, raw_tags_file_fuel),
    raw_tags_path = collapse_row_values(raw_tags_path, raw_tags_path_fuel)
  ) %>%
  select(-raw_tags_file_fuel, -raw_tags_path_fuel)

df_mission_metadata <- df_missions %>%
  rename(
    raw_missions_file = raw_source_file,
    raw_missions_path = raw_source_path,
    raw_missions_collection_round = raw_collection_round
  ) %>%
  separate(
    mission_name,
    into = c("nothing_1", "mission_date_raw", "study_arm_overall_numeric", "hh_id", "mission_start_time_raw"),
    sep = "_",
    remove = FALSE,
    fill = "right",
    extra = "merge"
  ) %>%
  mutate(
    hh_id = as.character(hh_id),
    fcn_id = str_extract(hh_id, ".{6}$")
  ) %>%
  select(
    source_folder, mission_id, mission_name, mission_key, hh_id, fcn_id,
    mission_date_raw, raw_missions_file, raw_missions_path,
    raw_missions_collection_round
  )

df_missions_hh_id <- df_mission_id_to_analyze %>%
  left_join(
    df_mission_metadata,
    by = c("source_folder", "mission_id", "mission_name", "mission_key")
  )

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

geocene_lpg_available_for_analysis <- function(study_arm_overall, date, first_receive_lpg_ymd) {
  timepoint <- as.character(geocene_timepoint_from_date(date))
  case_when(
    study_arm_overall == "comparison" ~ TRUE,
    study_arm_overall == "intervention" & timepoint == "baseline" ~ FALSE,
    study_arm_overall == "intervention" &
      timepoint %in% c("midline", "endline") &
      !is.na(first_receive_lpg_ymd) &
      date >= first_receive_lpg_ymd ~ TRUE,
    TRUE ~ FALSE
  )
}

df_events_stove_on <- df_events %>%
  mutate(
    mission_id = str_squish(as.character(mission_id)),
    raw_events_file = raw_source_file,
    raw_events_path = raw_source_path,
    raw_events_collection_round = raw_collection_round
  ) %>%
  left_join(df_missions_hh_id, by = c("source_folder", "mission_id")) %>%
  left_join(df_first_enrolled_lpg, by = "fcn_id") %>%
  mutate(
    start_time = parse_geocene_time(start_time),
    stop_time = parse_geocene_time(stop_time),
    date = as.Date(start_time),
    stove_on_min = as.numeric(difftime(stop_time, start_time, units = "mins")),
    raw_collection_round = collapse_row_values(
      raw_events_collection_round,
      raw_missions_collection_round
    ),
    raw_source_file = collapse_row_values(raw_events_file, raw_missions_file, raw_tags_file),
    raw_source_path = collapse_row_values(raw_events_path, raw_missions_path, raw_tags_path),
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
    lpg_available_for_analysis = geocene_lpg_available_for_analysis(
      study_arm_overall,
      date,
      first_receive_lpg_ymd
    ),
    included_by_reviewed_rule_order =
      lpg_enrolled_and_receiving == "receiving LPG through distribution program",
    included_by_previous_rule_order =
      lpg_enrolled_and_receiving_previous_rule_order == "receiving LPG through distribution program",
    included_only_after_comparison_rule_fix =
      included_by_reviewed_rule_order & !included_by_previous_rule_order
  ) %>%
  filter(!is.na(start_time), !is.na(stop_time), !is.na(date), !is.na(fuel_type)) %>%
  group_by(
    mission_key, study_arm_overall, fuel_type, processor_name, model_name,
    event_kind, start_time, stop_time
  ) %>%
  mutate(
    n_exact_duplicate_source_event_rows = n(),
    n_source_exports_for_event = n_distinct(source_folder),
    source_folder = paste(sort(unique(source_folder)), collapse = "; "),
    raw_collection_round = paste(sort(unique(raw_collection_round)), collapse = "; "),
    raw_source_file = paste(sort(unique(raw_source_file)), collapse = "; "),
    raw_source_path = paste(sort(unique(raw_source_path)), collapse = "; "),
    raw_events_file = paste(sort(unique(raw_events_file)), collapse = "; "),
    raw_missions_file = paste(sort(unique(raw_missions_file)), collapse = "; "),
    raw_tags_file = paste(sort(unique(raw_tags_file)), collapse = "; ")
  ) %>%
  slice(1) %>%
  ungroup() %>%
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
  mutate(
    mission_id = str_squish(as.character(mission_id)),
    raw_mission_logs_file = raw_source_file,
    raw_mission_logs_path = raw_source_path,
    raw_mission_logs_collection_round = raw_collection_round
  ) %>%
  left_join(df_missions_hh_id, by = c("source_folder", "mission_id")) %>%
  left_join(df_first_enrolled_lpg, by = "fcn_id") %>%
  mutate(
    phone_time = parse_geocene_time(phone_time),
    meter_time = parse_geocene_time(meter_time),
    date = as.Date(coalesce(phone_time, meter_time)),
    num_samples = suppressWarnings(as.numeric(num_samples)),
    raw_collection_round = collapse_row_values(
      raw_mission_logs_collection_round,
      raw_missions_collection_round
    ),
    raw_source_file = collapse_row_values(raw_mission_logs_file, raw_missions_file, raw_tags_file),
    raw_source_path = collapse_row_values(raw_mission_logs_path, raw_missions_path, raw_tags_path),
    lpg_enrolled_and_receiving = case_when(
      study_arm_overall == "comparison" ~ "receiving LPG through distribution program",
      date < first_receive_lpg_ymd | is.na(first_receive_lpg_ymd) ~ "not yet receiving LPG through distribution program",
      date >= first_receive_lpg_ymd ~ "receiving LPG through distribution program",
      TRUE ~ "not yet receiving LPG through distribution program"
    ),
    lpg_available_for_analysis = geocene_lpg_available_for_analysis(
      study_arm_overall,
      date,
      first_receive_lpg_ymd
    ),
    community = "refugee",
    data_type = "geocene_stove_monitor_day",
    collection_date = date,
    collection_year = year(date),
    timepoint = geocene_timepoint_from_date(collection_date)
  ) %>%
  filter(
    !is.na(date),
    !is.na(fcn_id),
    !is.na(fuel_type),
    !is.na(num_samples),
    num_samples > 0
  ) %>%
  distinct(
    mission_key, study_arm_overall, hh_id, fcn_id, fuel_type, date,
    phone_time, meter_time, num_samples,
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
  distinct(fcn_id, mission_key, .keep_all = TRUE) %>%
  select(
    source_folder, mission_id, mission_name, mission_key,
    hh_id, fcn_id, date, raw_collection_round, raw_source_file
  ) %>%
  mutate(
    timepoint = geocene_timepoint_from_date(date)
  )

df_have_geocene_but_no_survey_data_fcn_id <- df_have_geocene_but_no_survey_data %>%
  arrange(fcn_id) %>%
  pull(fcn_id)

max_date_mission <- df_events_stove_on %>%
  group_by(mission_key) %>%
  summarise(start = min(start_time), .groups = "drop") %>%
  mutate(max_date = add_with_rollback(start, months(3))) %>%
  select(mission_key, max_date)

df_events_stove_on_lt_3mo <- df_events_stove_on %>%
  left_join(max_date_mission, by = "mission_key") %>%
  filter(stop_time < max_date)

df_geocene_import_inclusion_audit_analysis_eligible_by_household <-
  df_events_stove_on_lt_3mo %>%
  filter(
    !is.na(fcn_id),
    fcn_id != ""
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
    missing_first_receive_lpg_ymd_any = any(is.na(first_receive_lpg_ymd)),
    n_source_mission_keys = n_distinct(mission_key),
    source_folders = paste(sort(unique(source_folder)), collapse = "; "),
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

df_events_stove_on_per_day_source <- df_events_stove_on_lt_3mo %>%
  filter(!is.na(fcn_id), fcn_id != "") %>%
  group_by(
    study_arm_overall, hh_id, fcn_id, date,
    first_receive_lpg_ymd, lpg_enrolled_and_receiving,
    lpg_available_for_analysis
  ) %>%
  summarise(
    source_folder = paste(sort(unique(source_folder)), collapse = "; "),
    source_mission_ids = paste(sort(unique(mission_id)), collapse = "; "),
    source_mission_names = paste(sort(unique(mission_name)), collapse = "; "),
    source_mission_keys = paste(sort(unique(mission_key)), collapse = "; "),
    raw_collection_round = paste(sort(unique(raw_collection_round)), collapse = "; "),
    raw_source_file = paste(sort(unique(raw_source_file)), collapse = "; "),
    raw_source_path = paste(sort(unique(raw_source_path)), collapse = "; "),
    raw_events_file = paste(sort(unique(raw_events_file)), collapse = "; "),
    raw_missions_file = paste(sort(unique(raw_missions_file)), collapse = "; "),
    raw_tags_file = paste(sort(unique(raw_tags_file)), collapse = "; "),
    .groups = "drop"
  )

df_events_stove_on_per_day <- df_events_stove_on_lt_3mo %>%
  filter(!is.na(fcn_id), fcn_id != "") %>%
  group_by(
    study_arm_overall, hh_id, fcn_id, fuel_type, date,
    first_receive_lpg_ymd, lpg_enrolled_and_receiving,
    lpg_available_for_analysis
  ) %>%
  summarise(n = n(), stove_on_min_sum = sum(stove_on_min, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = fuel_type, values_from = c(n, stove_on_min_sum)) %>%
  left_join(
    df_events_stove_on_per_day_source,
    by = c(
      "study_arm_overall", "hh_id", "fcn_id", "date",
      "first_receive_lpg_ymd", "lpg_enrolled_and_receiving",
      "lpg_available_for_analysis"
    )
  ) %>%
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
    timepoint = geocene_timepoint_from_date(collection_date)
  ) %>%
  rename(
    cooking_events_with_biomass = any_of("n_biomass"),
    cooking_events_with_lpg = any_of("n_lpg")
  )

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

manifest <- geocene_export_files %>%
  transmute(
    dataset_scope = dataset_scope,
    raw_source_path = normalizePath(raw_source_path, winslash = "/", mustWork = TRUE),
    raw_source_file = basename(raw_source_path),
    raw_role = raw_role,
    raw_collection_round = raw_collection_round,
    source_folder = source_folder,
    default_collection_year = NA_integer_,
    rows_in_raw_file = vapply(raw_source_path, raw_import_count_csv_rows, integer(1)),
    included_in_raw_import = TRUE,
    exclusion_or_note = ""
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
    "geocene_refugee_lpg_date_correction_audit",
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
    geocene_lpg_date_correction_audit_path,
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
    nrow(geocene_lpg_date_correction_audit),
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
