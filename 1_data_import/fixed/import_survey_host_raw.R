################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Raw-first host survey import
# @Description: Imports host household survey exports directly from 2_data_raw.
################################################################################

helper_from_root <- file.path("1_data_import", "fixed", "0_import_raw_helpers.R")
if (file.exists(helper_from_root)) {
  source(helper_from_root)
} else if (!exists("raw_import_project_root", mode = "function")) {
  stop("Run from the project root or source 0_import_raw_helpers.R first.", call. = FALSE)
}

dataset_scope <- "survey_host_raw"

host_dir <- raw_import_path("2_data_raw", "survey_HOST")
baseline_specs <- raw_import_make_survey_specs(
  paths = c(
    file.path(host_dir, "RohingyaFuelMaster_20200220_Corrected_20200308_HOST.csv"),
    file.path(host_dir, "rohingya_fuel_20200106_v1_host.csv"),
    file.path(host_dir, "rohingya_fuel_v95_endline_Rohingya_host.csv"),
    file.path(host_dir, "rohingya_fuel_v95_endline_Rohingya_host-hh_members.csv"),
    file.path(host_dir, "rohingya_fuel_v95_endline_Rohingya_host-symptoms.csv"),
    file.path(host_dir, "rohingya_fuel_v95_endline_Rohingya_host-location.csv")
  ),
  default_year = 2020L,
  source_group = "baseline_2020_survey_HOST_folder"
)

endline_dir <- host_dir
endline_specs <- raw_import_make_survey_specs(
  paths = c(
    file.path(endline_dir, "rohingya_fuel_v119_host.csv"),
    file.path(endline_dir, "rohingya_fuel_v119_host-hh_members.csv"),
    file.path(endline_dir, "rohingya_fuel_v119_host-symptoms.csv")
  ),
  default_year = 2022L,
  source_group = "endline_2022_survey_HOST_folder"
)

all_specs <- raw_import_bind_rows_fill(list(baseline_specs, endline_specs))
all_specs$source_order <- seq_len(nrow(all_specs))

manifest <- raw_import_file_manifest(all_specs, dataset_scope, included = TRUE)
raw_import_update_manifest(manifest, dataset_scope)

roles <- c("household", "hh_members", "symptoms", "location")
outputs <- list()
for (role in roles) {
  role_specs <- all_specs[all_specs$role == role, , drop = FALSE]
  role_data <- raw_import_read_survey_specs(
    role_specs,
    community = "host",
    data_type_prefix = "household_survey"
  )

  if ("hh_id" %in% names(role_data)) {
    if ("hh_id_original" %in% names(role_data)) {
      role_data$hh_id_original <- ifelse(
        is.na(role_data$hh_id_original) | trimws(as.character(role_data$hh_id_original)) == "",
        as.character(role_data$hh_id),
        as.character(role_data$hh_id_original)
      )
      role_data$hh_id <- NULL
    } else {
      names(role_data)[names(role_data) == "hh_id"] <- "hh_id_original"
    }
  }

  removed_duplicate_uuid_rows <- 0L
  if (identical(role, "household")) {
    deduped <- raw_import_deduplicate_household_uuid(role_data)
    role_data <- deduped$data
    removed_duplicate_uuid_rows <- deduped$removed
  } else if (nrow(role_data)) {
    role_data <- role_data[!duplicated(role_data), , drop = FALSE]
  }

  role_data$community <- "host"
  role_data$study_arm_overall <- "host"
  role_data$raw_import_note <- paste0(
    "Imported from host raw survey CSVs in 2_data_raw/survey_HOST; duplicate uuid rows removed: ",
    removed_duplicate_uuid_rows
  )

  output_rel <- file.path(
    "4_data", "clean_final", "imported_raw",
    paste0("survey_host_", role, "_raw.rds")
  )
  outputs[[role]] <- raw_import_write_rds(role_data, output_rel)
}

summary <- data.frame(
  dataset = names(outputs),
  output_path = unlist(outputs, use.names = FALSE),
  rows = vapply(outputs, function(path) nrow(readRDS(path)), integer(1)),
  stringsAsFactors = FALSE
)
raw_import_write_csv(
  summary,
  file.path("4_data", "clean_final", "imported_raw", "survey_host_raw_import_summary.csv")
)

message("Wrote host raw survey imports:")
message(paste(" -", unlist(outputs), collapse = "\n"))
