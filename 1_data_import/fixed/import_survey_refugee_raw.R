################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Raw-first refugee survey import
# @Description: Imports refugee household survey exports directly from 2_data_raw.
################################################################################

helper_from_root <- file.path("1_data_import", "fixed", "0_import_raw_helpers.R")
if (file.exists(helper_from_root)) {
  source(helper_from_root)
} else if (!exists("raw_import_project_root", mode = "function")) {
  stop("Run from the project root or source 0_import_raw_helpers.R first.", call. = FALSE)
}

dataset_scope <- "survey_refugee_raw"

baseline_files <- raw_import_make_survey_specs(
  paths = raw_import_path("2_data_raw", "survey_baseline", "RohingyaFuelMaster_Corrected_20200419_refugee.csv"),
  default_year = 2020L,
  source_group = "baseline_2019_2020_refugee_master"
)

midline_dir <- raw_import_path("2_data_raw", "survey_midline")
midline_files <- list.files(midline_dir, pattern = "\\.csv$", full.names = TRUE)
midline_files <- midline_files[grepl("^rohingya_fuel_v[0-9]+_endline_Rohingya", basename(midline_files))]
midline_specs <- raw_import_make_survey_specs(
  paths = midline_files,
  default_year = 2021L,
  source_group = "midline_2021_survey_midline_folder"
)

endline_dir <- raw_import_path("2_data_raw", "survey_endline")
endline_files <- list.files(endline_dir, pattern = "\\.csv$", full.names = TRUE)
endline_files <- endline_files[
  grepl("^rohingya_fuel_v(111|112|113|114|115|116)(-|\\.)", basename(endline_files)) &
    !grepl("duplicates", basename(endline_files), ignore.case = TRUE)
]
endline_specs <- raw_import_make_survey_specs(
  paths = endline_files,
  default_year = 2022L,
  source_group = "endline_2022_survey_endline_folder"
)

# The v116 duplicates CSV is a review artifact, not a raw survey input. Do not
# import it into imported_raw outputs.
duplicate_review_files <- list.files(
  endline_dir,
  pattern = "^rohingya_fuel_v116_duplicates\\.csv$",
  full.names = TRUE
)
duplicate_review_specs <- raw_import_make_survey_specs(
  paths = duplicate_review_files,
  default_year = 2022L,
  source_group = "endline_2022_duplicate_review_file"
)

all_specs <- raw_import_bind_rows_fill(list(baseline_files, midline_specs, endline_specs))
all_specs$source_order <- seq_len(nrow(all_specs))

manifest <- raw_import_file_manifest(all_specs, dataset_scope, included = TRUE)
if (nrow(duplicate_review_specs)) {
  manifest <- raw_import_bind_rows_fill(
    list(
      manifest,
      raw_import_file_manifest(
        duplicate_review_specs,
        dataset_scope,
        included = FALSE,
        reason = "Duplicate-review file is excluded from raw imports by cleaning decision."
      )
    )
  )
}
raw_import_update_manifest(manifest, dataset_scope)

stale_duplicate_review_output <- raw_import_path(
  "4_data",
  "clean_final",
  "imported_raw",
  "survey_refugee_duplicate_review_raw.rds"
)
if (file.exists(stale_duplicate_review_output)) {
  unlink(stale_duplicate_review_output)
}

roles <- c("household", "hh_members", "symptoms", "location")
outputs <- list()
for (role in roles) {
  role_specs <- all_specs[all_specs$role == role, , drop = FALSE]
  role_data <- raw_import_read_survey_specs(
    role_specs,
    community = "refugee",
    data_type_prefix = "household_survey"
  )

  removed_duplicate_uuid_rows <- 0L
  if (identical(role, "household")) {
    deduped <- raw_import_deduplicate_household_uuid(role_data)
    role_data <- deduped$data
    removed_duplicate_uuid_rows <- deduped$removed
  } else if (nrow(role_data)) {
    role_data <- role_data[!duplicated(role_data), , drop = FALSE]
  }

  role_data$raw_import_note <- paste0(
    "Imported from refugee raw survey CSVs; duplicate uuid rows removed: ",
    removed_duplicate_uuid_rows,
    "; rohingya_fuel_v116_duplicates.csv excluded as duplicate-review artifact"
  )

  output_rel <- file.path(
    "4_data", "clean_final", "imported_raw",
    paste0("survey_refugee_", role, "_raw.rds")
  )
  outputs[[role]] <- raw_import_write_rds(role_data, output_rel)
}

summary <- data.frame(
  dataset = names(outputs),
  output_path = unlist(outputs, use.names = FALSE),
  rows = vapply(
    names(outputs),
    function(role) nrow(readRDS(outputs[[role]])),
    integer(1)
  ),
  stringsAsFactors = FALSE
)
raw_import_write_csv(
  summary,
  file.path("4_data", "clean_final", "imported_raw", "survey_refugee_raw_import_summary.csv")
)

message("Wrote refugee raw survey imports:")
message(paste(" -", unlist(outputs), collapse = "\n"))