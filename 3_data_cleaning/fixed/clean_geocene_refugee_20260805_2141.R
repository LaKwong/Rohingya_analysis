################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Final refugee Geocene stove-use clean dataset
################################################################################

if (!exists("clean_final_project_root", mode = "function")) {
  helper_from_root <- file.path("3_data_cleaning", "fixed", "0_clean_helpers_20260805_2141.R")
  if (file.exists(helper_from_root)) {
    source(helper_from_root)
  } else {
    stop("Run from the Rohingya_analysis project root or source 0_clean_helpers_20260805_2141.R first.", call. = FALSE)
  }
}

dataset_name <- "stove_use_geocene_refugee_daily"
source_rel <- "4_data/clean_final/imported_raw/stove_use_geocene_refugee_daily_raw.rds"
stove <- read_rds_required(source_rel)

stove$community <- "refugee"
stove$data_type <- "geocene_stove_use_daily"
if (!"study_arm_overall" %in% names(stove)) {
  stove$study_arm_overall <- NA_character_
}

recoded <- recode_timepoint_by_timestamp(
  stove,
  date_cols = c("collection_date", "date", "start_date", "mission_date"),
  dataset_name = dataset_name
)
stove <- recoded$data
write_timepoint_summary(make_timepoint_summary(stove, dataset_name), dataset_name)

deidentified <- drop_identifier_columns(stove)
stove <- deidentified$data
stove <- move_columns_first(
  stove,
  c(
    "community", "data_type", "fcn_id", "hh_id", "timepoint",
    "timepoint_original", "collection_date", "collection_year", "timepoint_source_col",
    "raw_collection_round", "raw_source_file", "study_arm_overall"
  )
)

output_path <- write_final_rds(stove, "4_data/clean_final/stove_use_geocene_refugee_daily.rds")
shareable <- make_shareable_dataset(stove, dataset_name)
shareable_path <- write_shareable_rds(shareable$data, "stove_use_geocene_refugee_daily.rds")

monitor_dataset_name <- "stove_use_geocene_refugee_monitor_days"
monitor_source_rel <- "4_data/clean_final/imported_raw/geocene_refugee_monitor_days_derived_raw.rds"
monitor_output_path <- NA_character_
monitor_shareable_path <- NA_character_
monitor_entry <- NULL

if (file.exists(clean_final_path(monitor_source_rel))) {
  monitor_days <- read_rds_required(monitor_source_rel)
  monitor_days$community <- "refugee"
  monitor_days$data_type <- "geocene_stove_monitor_day"

  monitor_recoded <- recode_timepoint_by_timestamp(
    monitor_days,
    date_cols = c("collection_date", "date", "monitor_date", "phone_time", "meter_time"),
    dataset_name = monitor_dataset_name
  )
  monitor_days <- monitor_recoded$data
  write_timepoint_summary(
    make_timepoint_summary(monitor_days, monitor_dataset_name),
    monitor_dataset_name
  )

  monitor_deidentified <- drop_identifier_columns(monitor_days)
  monitor_days <- monitor_deidentified$data
  monitor_days <- move_columns_first(
    monitor_days,
    c(
      "community", "data_type", "fcn_id", "hh_id", "timepoint",
      "timepoint_original", "collection_date", "collection_year",
      "timepoint_source_col", "raw_collection_round", "raw_source_file",
      "study_arm_overall", "fuel_type", "date"
    )
  )

  monitor_output_path <- write_final_rds(
    monitor_days,
    "4_data/clean_final/stove_use_geocene_refugee_monitor_days.rds"
  )
  monitor_shareable <- make_shareable_dataset(monitor_days, monitor_dataset_name)
  monitor_shareable_path <- write_shareable_rds(
    monitor_shareable$data,
    "stove_use_geocene_refugee_monitor_days.rds"
  )

  monitor_entry <- make_inventory_entry(
    dataset_name = monitor_dataset_name,
    data = monitor_days,
    output_path = monitor_output_path,
    source_paths = clean_final_path(monitor_source_rel),
    removed_identifier_columns = monitor_deidentified$removed,
    shareable_output_path = monitor_shareable_path,
    shareable_removed_identifier_columns = monitor_shareable$removed,
    notes = paste(
      "Refugee-only Geocene monitored stove-day denominator rebuilt from Geocene_210204 and Geocene_220705 mission-log exports.",
      "Rows are unique mission-log sample days by household, fuel type, mission_id + mission_name, and date with num_samples > 0.",
      "Use this file as the denominator for monitored-day counts; use stove_use_geocene_refugee_daily.rds for stove-on event summaries."
    )
  )
}

entry <- make_inventory_entry(
  dataset_name = dataset_name,
  data = stove,
  output_path = output_path,
  source_paths = clean_final_path(source_rel),
  removed_identifier_columns = deidentified$removed,
  shareable_output_path = shareable_path,
  shareable_removed_identifier_columns = shareable$removed,
  notes = paste(
    "Refugee-only daily stove-use dataset rebuilt from raw Geocene exports in 2_data_raw/Geocene_210204 and 2_data_raw/Geocene_220705.",
    "No host Geocene output was created because there is no host-community Geocene data.",
    "Timepoint was recoded from the monitoring date."
  )
)
update_inventory(entry)
if (!is.null(monitor_entry)) {
  update_inventory(monitor_entry)
}
write_cleaning_fix_log()

message("Wrote ", output_path)
message("Wrote ", shareable_path)
if (!is.na(monitor_output_path)) message("Wrote ", monitor_output_path)
if (!is.na(monitor_shareable_path)) message("Wrote ", monitor_shareable_path)
