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

entry <- make_inventory_entry(
  dataset_name = dataset_name,
  data = stove,
  output_path = output_path,
  source_paths = clean_final_path(source_rel),
  removed_identifier_columns = deidentified$removed,
  notes = paste(
    "Refugee-only daily stove-use dataset rebuilt from raw Geocene exports in 2_data_raw/Geocene_220705.",
    "No host Geocene output was created because there is no host-community Geocene data.",
    "Timepoint was recoded from the monitoring date."
  )
)
update_inventory(entry)
write_cleaning_fix_log()

message("Wrote ", output_path)
