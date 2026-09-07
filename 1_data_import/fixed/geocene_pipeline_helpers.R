# Shared Geocene event pipeline. Raw files are read-only; identifiers stay private.
source(file.path("1_data_import", "fixed", "0_import_raw_helpers.R"))
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
})

geocene_variants <- c("100_80_5_20", "100_80_5_30")
geocene_private <- raw_import_path("8_restricted", "geocene_pipeline")
dir.create(geocene_private, recursive = TRUE, showWarnings = FALSE)

geocene_write <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (grepl("\\.rds$", path)) saveRDS(x, path) else write_csv(x, path, na = "")
  invisible(path)
}

geocene_read_csv <- function(path) {
  read_csv(path, col_types = cols(.default = col_character()), show_col_types = FALSE)
}

geocene_timepoint <- function(date) {
  x <- raw_import_timepoint_from_date(date)
  # Study-design clarification: Geocene baseline extends through August 2020.
  x[!is.na(date) & date >= as.Date("2019-09-01") & date <= as.Date("2020-08-31")] <- "baseline"
  x[is.na(x) & date >= as.Date("2022-01-01") & date < as.Date("2022-01-15")] <- "endline"
  x
}

geocene_parse_household_label <- function(x) {
  # Survey conventions distinguish camps 8E/8W from block letters in other camps.
  parts <- str_match(str_to_upper(x), "^(8[EW]|[0-9]{1,2})([A-Z])([A-Z]*[0-9]{1,4})([0-9]{6})$")
  tibble(camp_id = parts[, 2], block_id = parts[, 3], subblock_id = parts[, 4], fcn_id = parts[, 5])
}

geocene_parse_missions <- function(missions) {
  # Ignore the name's date: event timestamps determine the analysis timepoint.
  normalized <- str_replace_all(str_to_upper(missions$mission_name), "\\s+", "")
  normalized <- str_replace(normalized, "_([01]),", "_\\1_")
  parts <- str_match(normalized, "^[A-Z]+_?[0-9]+_([01])_([A-Z0-9]+)_")
  household <- geocene_parse_household_label(parts[, 3])
  missions %>% mutate(
    study_arm_overall = recode(parts[, 2], `0` = "intervention", `1` = "comparison"),
    camp_id = household$camp_id, block_id = household$block_id,
    subblock_id = household$subblock_id, fcn_id = household$fcn_id,
    hh_id = if_else(!is.na(fcn_id), paste0(camp_id, block_id, subblock_id, fcn_id), NA_character_),
    metadata_source = if_else(!is.na(fcn_id), "mission_name", NA_character_)
  )
}

geocene_apply_corrections <- function(missions) {
  path <- file.path(geocene_private, "mission_metadata_corrections.csv")
  if (!file.exists(path)) return(missions)
  corrections <- geocene_read_csv(path)
  required <- c("mission_id", "fcn_id", "camp_id", "block_id", "subblock_id", "study_arm_overall", "reason", "reviewed_by")
  stopifnot(all(required %in% names(corrections)), !anyDuplicated(corrections$mission_id))
  stopifnot(all(!is.na(corrections$reason)),
    all(str_detect(corrections$fcn_id, "^[0-9]{6}$")),
    all(corrections$study_arm_overall %in% c("comparison", "intervention")),
    all(!is.na(corrections$camp_id)), all(!is.na(corrections$block_id)), all(!is.na(corrections$subblock_id)))
  for (i in seq_len(nrow(corrections))) {
    at <- which(missions$mission_id == corrections$mission_id[i])
    stopifnot(length(at) == 1L)
    for (column in c("fcn_id", "camp_id", "block_id", "subblock_id", "study_arm_overall")) {
      missions[[column]][at] <- corrections[[column]][i]
    }
    missions$hh_id[at] <- paste0(missions$camp_id[at], missions$block_id[at], missions$subblock_id[at], missions$fcn_id[at])
    missions$metadata_source[at] <- "reviewed_mission_correction"
  }
  geocene_write(corrections %>% mutate(
    review_provenance = if_else(is.na(reviewed_by), "User-supplied correction file confirmed in task", reviewed_by),
    correction_file = basename(path), correction_file_md5 = unname(tools::md5sum(path))
  ), file.path(geocene_private, "mission_metadata_corrections_applied.csv"))
  missions
}

geocene_assign_keys <- function(missions) {
  path <- file.path(geocene_private, "mission_key_crosswalk.csv")
  crosswalk <- if (file.exists(path)) geocene_read_csv(path) else tibble(mission_id = character(), mission_key = character())
  stopifnot(!anyDuplicated(crosswalk$mission_id), !anyDuplicated(crosswalk$mission_key))
  new_ids <- setdiff(sort(unique(missions$mission_id)), crosswalk$mission_id)
  # Random opaque tokens do not encode names, locations, or household numbers.
  if (length(new_ids)) {
    tokens <- vapply(new_ids, function(id) paste0("mission_", paste(sample(c(letters, 0:9), 24, TRUE), collapse = "")), character(1))
    crosswalk <- bind_rows(crosswalk, tibble(mission_id = new_ids, mission_key = tokens))
    stopifnot(!anyDuplicated(crosswalk$mission_key))
    geocene_write(crosswalk, path)
  }
  left_join(missions, crosswalk, by = "mission_id")
}

geocene_import <- function() {
  exports <- crossing(analysis_variant = geocene_variants, fuel_type = c("biomass", "lpg")) %>%
    mutate(source_folder = paste0("geocene_", fuel_type, "_", analysis_variant))
  event_sets <- list()
  checks <- list()
  mission_checks <- list()
  for (i in seq_len(nrow(exports))) {
    spec <- exports[i, ]
    folder <- raw_import_path("2_data_raw", spec$source_folder)
    events <- geocene_read_csv(file.path(folder, "events_by_mission.csv"))
    stopifnot(all(c("mission_id", "mission_name", "device_id", "start_time", "stop_time", "duration_minutes", "processor_label") %in% names(events)))
    events <- events %>% mutate(
      analysis_variant = spec$analysis_variant, fuel_type = spec$fuel_type,
      source_folder = spec$source_folder, raw_source_file = "events_by_mission.csv",
      source_row = row_number(), event_key = paste(spec$analysis_variant, spec$fuel_type, source_row, sep = ":"),
      duration_minutes = as.numeric(duration_minutes),
      total_event_duration_minutes = as.numeric(total_event_duration_minutes)
    )
    summary <- geocene_read_csv(file.path(folder, "events_summary.csv"))
    mission_summary <- geocene_read_csv(file.path(folder, "events_summary_by_mission.csv"))
    checks[[i]] <- tibble(
      analysis_variant = spec$analysis_variant, fuel_type = spec$fuel_type,
      imported_events = nrow(events), expected_events = sum(as.numeric(summary$event_count)),
      imported_minutes = sum(events$duration_minutes), expected_minutes = sum(as.numeric(summary$duration_minutes))
    )
    mission_checks[[i]] <- events %>% group_by(mission_id) %>%
      summarise(imported_events = n(), imported_minutes = sum(duration_minutes), .groups = "drop") %>%
      full_join(mission_summary %>% group_by(mission_id) %>%
        summarise(expected_events = sum(as.numeric(event_count)), expected_minutes = sum(as.numeric(duration_minutes)), .groups = "drop"), by = "mission_id") %>%
      mutate(analysis_variant = spec$analysis_variant, fuel_type = spec$fuel_type)
    event_sets[[i]] <- events
  }
  events <- bind_rows(event_sets)
  checks <- bind_rows(checks)
  mission_checks <- bind_rows(mission_checks)
  geocene_write(checks, file.path(geocene_private, "import_reconciliation.csv"))
  geocene_write(mission_checks, file.path(geocene_private, "mission_import_reconciliation.csv"))
  geocene_write(events, file.path(geocene_private, "events_imported.rds"))
  timestamp_audit <- events %>% mutate(
    parsed_start = ymd_hms(start_time, quiet = TRUE, tz = "UTC"),
    parsed_stop = ymd_hms(stop_time, quiet = TRUE, tz = "UTC"),
    local_date = as.Date(with_tz(parsed_start, "Asia/Dhaka"), tz = "Asia/Dhaka"),
    timestamp_minutes = as.numeric(difftime(parsed_stop, parsed_start, units = "mins")),
    timepoint_from_date = geocene_timepoint(local_date))
  geocene_write(timestamp_audit %>% filter(is.na(parsed_start) | is.na(parsed_stop) | is.na(timepoint_from_date) |
    duration_minutes <= 0 | timestamp_minutes <= 0), file.path(geocene_private, "raw_event_value_issues.csv"))
  geocene_write(timestamp_audit %>% filter(abs(duration_minutes - timestamp_minutes) > 0.01), file.path(geocene_private, "duration_mismatch_audit.csv"))
  stopifnot(all(checks$imported_events == checks$expected_events), all(abs(checks$imported_minutes - checks$expected_minutes) < 0.01))
  stopifnot(all(mission_checks$imported_events == mission_checks$expected_events), all(abs(mission_checks$imported_minutes - mission_checks$expected_minutes) < 0.01))
  missions <- events %>% distinct(mission_id, mission_name, fuel_type, device_id)
  conflicts <- missions %>% count(mission_id) %>% filter(n != 1)
  geocene_write(conflicts, file.path(geocene_private, "mission_identity_conflicts.csv"))
  stopifnot(nrow(conflicts) == 0)
  missions <- missions %>% geocene_parse_missions() %>% geocene_apply_corrections() %>% geocene_assign_keys()
  arm_conflicts <- missions %>% filter(!is.na(fcn_id)) %>% group_by(fcn_id) %>% filter(n_distinct(study_arm_overall) > 1) %>% ungroup()
  geocene_write(arm_conflicts, file.path(geocene_private, "household_arm_conflicts.csv"))
  unresolved <- missions %>% filter(is.na(fcn_id) | !str_detect(fcn_id, "^[0-9]{6}$") | !study_arm_overall %in% c("comparison", "intervention"))
  geocene_write(missions, file.path(geocene_private, "mission_metadata.csv"))
  geocene_write(unresolved, file.path(geocene_private, "unresolved_mission_metadata.csv"))
  if (nrow(unresolved)) stop(nrow(unresolved), " missions need reviewed metadata corrections. See 8_restricted/geocene_pipeline/unresolved_mission_metadata.csv", call. = FALSE)
  geocene_write(left_join(events, missions %>% select(-mission_name, -fuel_type, -device_id), by = "mission_id"), file.path(geocene_private, "events_linked.rds"))
  invisible(checks)
}

geocene_receipt_lookup <- function() {
  path <- raw_import_path("4_data", "clean_final", "imported_raw", "survey_refugee_household_raw.rds")
  survey <- as_tibble(readRDS(path))
  if (!"study_arm_overall" %in% names(survey)) survey$study_arm_overall <- survey$study_arm
  receipt <- survey %>% mutate(across(any_of(c("fcn_id", "study_arm_overall", "timepoint", "first_enrolled_lpg", "first_receive_lpg")), as.character)) %>%
    filter((timepoint == "baseline" & study_arm_overall == "comparison") | (timepoint == "midline" & study_arm_overall == "intervention")) %>%
    mutate(first_enrolled_lpg_original = first_enrolled_lpg,
      first_enrolled_lpg = if_else(fcn_id == "123970" & study_arm_overall == "intervention" & first_enrolled_lpg %in% c("1/1/1982", "Jan 1, 1982", "1982-01-01"), "1/1/2019", first_enrolled_lpg),
      first_receive_lpg_ymd = as.Date(if_else(study_arm_overall == "comparison",
        suppressWarnings(dmy(first_receive_lpg)), suppressWarnings(mdy(first_receive_lpg)))),
      first_enrolled_lpg_ymd = as.Date(if_else(study_arm_overall == "comparison",
        suppressWarnings(dmy(first_enrolled_lpg)), suppressWarnings(mdy(first_enrolled_lpg)))))
  geocene_write(receipt %>% select(any_of(c("fcn_id", "timepoint", "study_arm_overall", "first_receive_lpg", "first_receive_lpg_ymd", "first_enrolled_lpg_original", "first_enrolled_lpg", "first_enrolled_lpg_ymd", "raw_source_file"))), file.path(geocene_private, "receipt_date_audit.csv"))
  receipt %>% filter(!is.na(fcn_id)) %>% group_by(fcn_id, study_arm_overall) %>%
    summarise(receipt_date_conflict = n_distinct(first_receive_lpg_ymd, na.rm = TRUE) > 1,
      first_receive_lpg_ymd = if (n_distinct(first_receive_lpg_ymd, na.rm = TRUE) == 1) first_receive_lpg_ymd[which(!is.na(first_receive_lpg_ymd))[1]] else as.Date(NA), .groups = "drop")
}

geocene_prepare_events <- function(events) {
  events <- events %>% mutate(
    start_time = with_tz(ymd_hms(start_time, quiet = TRUE, tz = "UTC"), "Asia/Dhaka"),
    stop_time = with_tz(ymd_hms(stop_time, quiet = TRUE, tz = "UTC"), "Asia/Dhaka"),
    date = as.Date(start_time, tz = "Asia/Dhaka"),
    timestamp_duration_minutes = as.numeric(difftime(stop_time, start_time, units = "mins")),
    stove_on_min = duration_minutes,
    timepoint_original = geocene_timepoint(date), timepoint_from_date = timepoint_original)
  recodes <- events %>% filter(study_arm_overall == "intervention", timepoint_original == "baseline", fuel_type == "lpg") %>%
    distinct(analysis_variant, fcn_id, date) %>% mutate(recode_baseline_intervention_lpg_date = TRUE)
  events %>% left_join(recodes, by = c("analysis_variant", "fcn_id", "date")) %>%
    mutate(recode_baseline_intervention_lpg_date = replace_na(recode_baseline_intervention_lpg_date, FALSE),
      timepoint = if_else(recode_baseline_intervention_lpg_date, "midline", timepoint_original),
      timepoint_recode_reason = if_else(recode_baseline_intervention_lpg_date, "Baseline intervention household-date with recorded LPG use", NA_character_),
      days_after_first_receiving = as.numeric(date - first_receive_lpg_ymd))
}

geocene_collapse_days <- function(events) {
  daily <- events %>% group_by(analysis_variant, fcn_id, date) %>% summarise(
    hh_id = first(hh_id), study_arm_overall = first(study_arm_overall),
    timepoint = first(timepoint), timepoint_original = first(timepoint_original),
    recode_baseline_intervention_lpg_date = any(recode_baseline_intervention_lpg_date),
    timepoint_recode_reason = first(timepoint_recode_reason),
    first_receive_lpg_ymd = first(first_receive_lpg_ymd),
    receipt_date_conflict = first(receipt_date_conflict),
    source_mission_keys = paste(sort(unique(mission_key)), collapse = ";"),
    cooking_events_with_lpg_zero = sum(fuel_type == "lpg"),
    cooking_events_with_biomass_zero = sum(fuel_type == "biomass"),
    stove_on_min_sum_lpg_zero = sum(stove_on_min[fuel_type == "lpg"]),
    stove_on_min_sum_biomass_zero = sum(stove_on_min[fuel_type == "biomass"]), .groups = "drop") %>%
    mutate(lpg_recorded = cooking_events_with_lpg_zero > 0,
      biomass_recorded = cooking_events_with_biomass_zero > 0,
      observed_stove_use_day = lpg_recorded | biomass_recorded,
      n_stoves_with_recorded_use = as.integer(lpg_recorded) + as.integer(biomass_recorded),
      exclusive_lpg_recalc = lpg_recorded & !biomass_recorded,
      exclusive_biomass_recalc = biomass_recorded & !lpg_recorded,
      mixed_use_recalc = biomass_recorded & lpg_recorded,
      valid_exclusive_use_denominator = observed_stove_use_day,
      stove_on_min_sum_total_zero = stove_on_min_sum_lpg_zero + stove_on_min_sum_biomass_zero,
      stove_on_min_pc_lpg_zero = 100 * stove_on_min_sum_lpg_zero / stove_on_min_sum_total_zero,
      stove_on_min_pc_biomass_zero = 100 * stove_on_min_sum_biomass_zero / stove_on_min_sum_total_zero,
      days_after_first_receiving = as.numeric(date - first_receive_lpg_ymd),
      months_after_first_receiving_numeric = floor(days_after_first_receiving / 30),
      collection_year = year(date), timepoint_from_date = timepoint_original,
      monitor_denominator_source = "event_dates_only_primary_collapsed_household_date")
  stopifnot(!anyDuplicated(daily[c("analysis_variant", "fcn_id", "date")]), all(daily$observed_stove_use_day),
    all(daily$exclusive_lpg_recalc + daily$exclusive_biomass_recalc + daily$mixed_use_recalc == 1),
    !any(daily$study_arm_overall == "intervention" & daily$timepoint == "baseline" & daily$lpg_recorded),
    sum(daily$cooking_events_with_lpg_zero + daily$cooking_events_with_biomass_zero) == nrow(events))
  daily
}

geocene_exclude_missions <- function(events, exclusions) {
  stopifnot(all(c("mission_id", "mission_name", "exclusion_reason") %in% names(exclusions)),
    !anyDuplicated(exclusions[c("mission_id", "mission_name")]), all(!is.na(exclusions$exclusion_reason)))
  list(retained = anti_join(events, exclusions, by = c("mission_id", "mission_name")),
    excluded = inner_join(events, exclusions, by = c("mission_id", "mission_name")))
}

geocene_clean <- function() {
  # Refuse stale linked inputs left behind by a failed import.
  unresolved <- geocene_read_csv(file.path(geocene_private, "unresolved_mission_metadata.csv"))
  if (nrow(unresolved)) stop("Resolve mission metadata before cleaning Geocene data.", call. = FALSE)
  imported <- readRDS(file.path(geocene_private, "events_linked.rds"))
  exclusions <- geocene_read_csv(file.path(geocene_private, "mission_exclusions.csv"))
  partition <- geocene_exclude_missions(imported, exclusions)
  stopifnot(nrow(imported) == nrow(partition$retained) + nrow(partition$excluded))
  geocene_write(partition$excluded, file.path(geocene_private, "excluded_mission_events.rds"))
  exclusion_summary <- partition$excluded %>%
    mutate(date = as.Date(with_tz(ymd_hms(start_time, quiet = TRUE, tz = "UTC"), "Asia/Dhaka"), tz = "Asia/Dhaka")) %>%
    group_by(analysis_variant, study_arm_overall, fuel_type, exclusion_reason) %>%
    summarise(n_excluded_missions = n_distinct(mission_id, mission_name), n_excluded_events = n(),
      n_excluded_household_dates = n_distinct(fcn_id, date), .groups = "drop")
  geocene_write(exclusion_summary, file.path(geocene_private, "mission_exclusion_summary.csv"))
  reconciliation <- imported %>% count(analysis_variant, name = "n_imported_events") %>%
    left_join(count(partition$retained, analysis_variant, name = "n_retained_events"), by = "analysis_variant") %>%
    left_join(count(partition$excluded, analysis_variant, name = "n_excluded_events"), by = "analysis_variant") %>%
    mutate(across(starts_with("n_"), ~replace_na(.x, 0L)), passed = n_imported_events == n_retained_events + n_excluded_events)
  stopifnot(all(reconciliation$passed))
  geocene_write(reconciliation, file.path(geocene_private, "event_exclusion_reconciliation.csv"))
  # Apply mission exclusions before recoding: excluded LPG must not relabel biomass dates.
  events <- partition$retained %>%
    left_join(geocene_receipt_lookup(), by = c("fcn_id", "study_arm_overall")) %>%
    mutate(receipt_date_conflict = replace_na(receipt_date_conflict, FALSE)) %>% geocene_prepare_events()
  problems <- events %>% filter(is.na(start_time) | is.na(stop_time) | is.na(timepoint) | is.na(stove_on_min) |
    stove_on_min <= 0 | timestamp_duration_minutes <= 0)
  duration_differences <- events %>% filter(abs(stove_on_min - timestamp_duration_minutes) > 0.01)
  duplicates <- events %>% group_by(analysis_variant, mission_key, start_time, stop_time) %>% filter(n() > 1) %>% ungroup()
  arm_conflicts <- events %>% distinct(fcn_id, study_arm_overall) %>% count(fcn_id) %>% filter(n > 1)
  geocene_write(problems, file.path(geocene_private, "invalid_event_values.csv"))
  geocene_write(duration_differences, file.path(geocene_private, "duration_mismatch_audit.csv"))
  geocene_write(duplicates, file.path(geocene_private, "duplicate_events.csv"))
  geocene_write(arm_conflicts, file.path(geocene_private, "household_arm_conflicts.csv"))
  if (nrow(problems) || nrow(duplicates) || nrow(arm_conflicts)) stop("Geocene validation needs review; no events were excluded. See restricted QA.", call. = FALSE)
  daily <- geocene_collapse_days(events)
  geocene_write(events, file.path(geocene_private, "events_clean_with_provenance.rds"))
  analysis_events <- events %>% select(analysis_variant, event_key, mission_key, fcn_id, hh_id, study_arm_overall, fuel_type,
    start_time, stop_time, date, stove_on_min, timepoint, timepoint_original, timepoint_from_date,
    recode_baseline_intervention_lpg_date, timepoint_recode_reason, first_receive_lpg_ymd, receipt_date_conflict, days_after_first_receiving)
  for (variant in geocene_variants) {
    root <- raw_import_path("4_data", "clean_final", "geocene", variant)
    geocene_write(filter(analysis_events, analysis_variant == variant), file.path(root, "events.rds"))
    geocene_write(filter(daily, analysis_variant == variant), file.path(root, "household_days.rds"))
    geocene_write(filter(exclusion_summary, analysis_variant == variant), file.path(root, "mission_exclusion_summary.rds"))
  }
  geocene_write(filter(daily, analysis_variant == geocene_variants[1]), raw_import_path("4_data", "clean_final", "stove_use_geocene_refugee_daily.rds"))
  invisible(daily)
}

geocene_export_public <- function(household_lookup = NULL,
                                  private_root = raw_import_path("4_data", "clean_final"),
                                  public_root = raw_import_path("4_data", "clean_final_public"),
                                  crosswalk_dir = geocene_private) {
  # Use the existing household mapping when available, so survey joins remain valid.
  paths <- file.path(crosswalk_dir, "household_key_crosswalk.csv")
  map_path <- paths[file.exists(paths)][1]
  mapping <- if (!is.null(household_lookup)) household_lookup else if (!is.na(map_path)) geocene_read_csv(map_path) else tibble(original = character(), public = character())
  stopifnot(all(c("original", "public") %in% names(mapping)))
  datasets <- lapply(geocene_variants, function(v) lapply(c("events.rds", "household_days.rds"), function(f) readRDS(file.path(private_root, "geocene", v, f))))
  ids <- unique(unlist(lapply(datasets, function(pair) unlist(lapply(pair, function(d) c(d$fcn_id, d$hh_id))))))
  new_ids <- setdiff(ids[!is.na(ids)], mapping$original)
  if (length(new_ids)) mapping <- bind_rows(mapping, tibble(original = new_ids, public = vapply(new_ids, function(id) paste0("geocene_hh_", paste(sample(c(letters, 0:9), 24, TRUE), collapse = "")), character(1))))
  stopifnot(!anyDuplicated(mapping$original), !anyDuplicated(mapping$public))
  geocene_write(mapping, file.path(crosswalk_dir, "household_key_crosswalk.csv"))
  for (i in seq_along(geocene_variants)) for (j in 1:2) {
    d <- datasets[[i]][[j]]
    d$fcn_id <- mapping$public[match(d$fcn_id, mapping$original)]
    d$hh_id <- mapping$public[match(d$hh_id, mapping$original)]
    stopifnot(!anyNA(d$fcn_id), !any(grepl("mission_name|device_id|source_path|camp_id|block_id|subblock_id", names(d))))
    geocene_write(d, file.path(public_root, "geocene", geocene_variants[i], c("events.rds", "household_days.rds")[j]))
    if (i == 1 && j == 2) geocene_write(d, file.path(public_root, "stove_use_geocene_refugee_daily.rds"))
  }
  for (variant in geocene_variants) {
    summary_path <- file.path(private_root, "geocene", variant, "mission_exclusion_summary.rds")
    if (file.exists(summary_path)) geocene_write(readRDS(summary_path), file.path(public_root, "geocene", variant, "mission_exclusion_summary.rds"))
  }
  invisible(TRUE)
}
