################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Final host survey related clean datasets
################################################################################

if (!exists("clean_final_project_root", mode = "function")) {
  helper_from_root <- file.path("3_data_cleaning", "fixed", "0_clean_helpers_20260805_2141.R")
  if (file.exists(helper_from_root)) {
    source(helper_from_root)
  } else {
    stop("Run from the Rohingya_analysis project root or source 0_clean_helpers_20260805_2141.R first.", call. = FALSE)
  }
}

clean_related_survey <- function(role, dataset_name, output_file) {
  source_rel <- file.path("4_data", "clean_final", "imported_raw", paste0("survey_host_", role, "_raw.rds"))
  data <- read_rds_required(source_rel)
  data <- data[data$community == "host", , drop = FALSE]
  household_raw <- read_rds_required("4_data/clean_final/imported_raw/survey_host_household_raw.rds")
  data$community <- "host"
  data$study_arm_overall <- "host"
  data$data_type <- paste0("household_survey_", role)

  if ("PARENT_KEY" %in% names(data) && "KEY" %in% names(household_raw)) {
    parent_idx <- match(data$PARENT_KEY, household_raw$KEY)
    if (!"collection_date" %in% names(data)) data$collection_date <- as.Date(NA)
    if (!"collection_year" %in% names(data)) data$collection_year <- NA_integer_
    if (!"timepoint" %in% names(data)) data$timepoint <- NA_character_
    if (!"timepoint_source_col" %in% names(data)) data$timepoint_source_col <- NA_character_
    parent_fill <- is.na(data$collection_date) & !is.na(parent_idx)
    data$collection_date[parent_fill] <- as.Date(household_raw$collection_date[parent_idx[parent_fill]])
    data$collection_year[parent_fill] <- household_raw$collection_year[parent_idx[parent_fill]]
    data$timepoint[parent_fill] <- household_raw$timepoint[parent_idx[parent_fill]]
    data$timepoint_source_col[parent_fill] <- "parent_household_collection_date"
  }

  recoded <- recode_timepoint_by_timestamp(
    data,
    date_cols = c("collection_date", "start_date", "SubmissionDate", "starttime", "endtime", "date", "datetime"),
    dataset_name = dataset_name
  )
  data <- recoded$data
  write_timepoint_summary(make_timepoint_summary(data, dataset_name), dataset_name)

  deidentified <- drop_identifier_columns(data)
  data <- deidentified$data
  data <- move_columns_first(
    data,
    c(
      "community", "data_type", "fcn_id", "hh_id", "uuid", "KEY",
      "timepoint", "timepoint_original", "collection_date", "collection_year", "timepoint_source_col",
      "raw_collection_round", "raw_survey_version", "raw_source_file", "study_arm_overall"
    )
  )

  output_path <- write_final_rds(data, file.path("4_data", "clean_final", output_file))
  shareable <- make_shareable_dataset(data, dataset_name)
  shareable_path <- write_shareable_rds(shareable$data, output_file)
  entry <- make_inventory_entry(
    dataset_name = dataset_name,
    data = data,
    output_path = output_path,
    source_paths = clean_final_path(source_rel),
    removed_identifier_columns = deidentified$removed,
    shareable_output_path = shareable_path,
    shareable_removed_identifier_columns = shareable$removed,
    notes = paste("Host", role, "survey subform rebuilt from raw-first imports.")
  )
  update_inventory(entry)
  message("Wrote ", output_path)
}

clean_related_survey("hh_members", "survey_host_hh_members", "survey_host_hh_members.rds")
clean_related_survey("symptoms", "survey_host_symptoms", "survey_host_symptoms.rds")
clean_related_survey("location", "survey_host_location", "survey_host_location.rds")
write_cleaning_fix_log()
