################################################################################
# Create public cleaned RF105 analysis data
#
# Purpose:
#   Create a de-identified cleaned-data folder that contains only the files
#   needed to rerun the reviewed RF105 analyses. Internal audit files, raw import
#   manifests, correction logs, and household-level QA listings are not exported.
#
# Inputs:
#   4_data/clean_final/*.rds
#   4_data/clean_final/geocene/<variant>/{events,household_days}.rds
#
# Outputs:
#   4_data/clean_final_public/*.rds
#   4_data/clean_final_public/imported_raw/*.rds
#   4_data/clean_final_public/public_clean_final_manifest.csv
#   4_data/clean_final_public/public_clean_final_private_public_compatibility.csv
#   4_data/clean_final_public/README_clean_final_public.md
#
# De-identification:
#   - Original fcn_id and hh_id values are replaced with stable public household
#     pseudonyms while retaining the column names needed by the analysis code.
#   - KEY, PARENT_KEY, uuid, monitor, mission, and source-file fields are
#     pseudonymized when retained for joins or reproducibility checks.
#   - Name fields, UNHCR identifiers, camp_id, block_id, subblock_id, enumerator,
#     and derived/raw ID-note fields are removed.
#   - Exact dates and times are retained, per investigator decision, because they
#     are needed for PM2.5 ambient matching and Geocene denominator checks.
################################################################################

required_packages <- c("tidyverse", "digest")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing package(s) for public clean-data export: ",
    paste(missing_packages, collapse = ", "),
    ". Run renv::restore() from the project root, then rerun.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(digest)
})

get_project_root <- function() {
  env_root <- Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(getwd()), winslash = "/", mustWork = FALSE)
}

project_root <- get_project_root()
input_dir <- file.path(project_root, "4_data", "clean_final")
output_dir <- file.path(project_root, "4_data", "clean_final_public")
restricted_dir <- file.path(project_root, "8_restricted", "public_clean_final")

if (!dir.exists(input_dir)) {
  stop("Missing input directory: ", input_dir, call. = FALSE)
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "imported_raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(restricted_dir, recursive = TRUE, showWarnings = FALSE)

root_rds_files <- c(
  "survey_refugee_household.rds",
  "survey_refugee_hh_members.rds",
  "survey_refugee_location.rds",
  "survey_refugee_symptoms.rds",
  "survey_host_household.rds",
  "survey_host_hh_members.rds",
  "survey_host_location.rds",
  "survey_host_symptoms.rds",
  "pm25_pats_refugee_indoor.rds",
  "pm25_pats_refugee_ambient.rds",
  "pm25_pats_refugee_qc_files.rds",
  "stove_use_geocene_refugee_daily.rds"
)

imported_rds_files <- character()

all_input_paths <- c(
  file.path(input_dir, root_rds_files),
  file.path(input_dir, "imported_raw", imported_rds_files)
)
missing_inputs <- all_input_paths[!file.exists(all_input_paths)]
if (length(missing_inputs) > 0) {
  stop("Missing cleaned input file(s): ", paste(missing_inputs, collapse = "; "),
       call. = FALSE)
}

as_clean_character <- function(x) {
  out <- stringr::str_squish(as.character(x))
  out[out == ""] <- NA_character_
  out
}

make_public_lookup <- function(values, prefix) {
  values <- sort(unique(as_clean_character(values)))
  values <- values[!is.na(values)]
  if (!length(values)) {
    return(tibble(original = character(), public = character()))
  }
  hashed <- vapply(values, digest::digest, character(1), algo = "sha256",
                   serialize = FALSE)
  tibble(original = values, hash = hashed) %>%
    arrange(hash) %>%
    mutate(public = sprintf("%s_%05d", prefix, row_number())) %>%
    select(original, public)
}

first_present_column <- function(data, cols) {
  cols <- intersect(cols, names(data))
  if (!length(cols)) return(rep(NA_character_, nrow(data)))
  values <- lapply(cols, function(col) as_clean_character(data[[col]]))
  out <- values[[1]]
  if (length(values) > 1) {
    for (ii in seq_along(values)[-1]) {
      out[is.na(out)] <- values[[ii]][is.na(out)]
    }
  }
  out
}

read_input <- function(path) {
  readRDS(path) %>% as_tibble()
}

all_data_for_maps <- c(
  setNames(file.path(input_dir, root_rds_files), root_rds_files),
  setNames(file.path(input_dir, "imported_raw", imported_rds_files),
           file.path("imported_raw", imported_rds_files))
) %>%
  imap(function(path, rel_path) read_input(path))

household_values <- unlist(lapply(all_data_for_maps, function(data) {
  values <- list()
  if ("fcn_id" %in% names(data)) values$fcn_id <- data$fcn_id
  if ("hh_id" %in% names(data)) values$hh_id <- data$hh_id
  values
}), use.names = FALSE)
household_lookup <- make_public_lookup(household_values, "hh")

record_values <- unlist(lapply(all_data_for_maps, function(data) {
  values <- list()
  if ("KEY" %in% names(data)) values$KEY <- data$KEY
  if ("PARENT_KEY" %in% names(data)) values$PARENT_KEY <- data$PARENT_KEY
  if ("uuid" %in% names(data)) values$uuid <- data$uuid
  values
}), use.names = FALSE)
record_lookup <- make_public_lookup(record_values, "record")

monitor_values <- unlist(lapply(all_data_for_maps, function(data) {
  cols <- intersect(c("PM_monitor", "ambient_site_id", "mission_id", "source_mission_ids_with_events"), names(data))
  unlist(data[cols], use.names = FALSE)
}), use.names = FALSE)
monitor_lookup <- make_public_lookup(monitor_values, "monitor")

source_values <- unlist(lapply(all_data_for_maps, function(data) {
  cols <- intersect(c("raw_source_file", "source_file"), names(data))
  unlist(data[cols], use.names = FALSE)
}), use.names = FALSE)
source_lookup <- make_public_lookup(source_values, "source")

replace_from_lookup <- function(x, lookup, missing_value = NA_character_) {
  x_clean <- as_clean_character(x)
  out <- lookup$public[match(x_clean, lookup$original)]
  out[is.na(x_clean)] <- missing_value
  out
}

columns_to_drop <- function(data) {
  nm <- names(data)
  lower <- str_to_lower(nm)
  drop_patterns <- c(
    "(^|_)name($|_)",
    "unhcr",
    "^camp_id$", "^block_id$", "^subblock_id$",
    "(^|_)camp_id($|_)", "(^|_)block_id($|_)", "(^|_)subblock_id($|_)",
    "^enumerator$",
    "^hh_id_", "^fcn_id_",
    "^hh_id_host$",
    "^raw_source_path$", "^source_path$", "^raw_import_path$",
    "^source_file$"
  )
  drop <- unique(nm[Reduce(`|`, lapply(drop_patterns, function(pattern) str_detect(lower, pattern)))])
  setdiff(drop, c("hh_id_note", "PM_monitor"))
}

pseudonymize_public_data <- function(data) {
  had_fcn_id <- "fcn_id" %in% names(data)
  had_hh_id <- "hh_id" %in% names(data)
  original_fcn_id <- if (had_fcn_id) data$fcn_id else rep(NA_character_, nrow(data))
  original_hh_id <- if (had_hh_id) data$hh_id else rep(NA_character_, nrow(data))

  drop_cols <- columns_to_drop(data)
  data <- data[, setdiff(names(data), drop_cols), drop = FALSE]

  if (had_fcn_id) {
    data$fcn_id <- replace_from_lookup(original_fcn_id, household_lookup)
  }
  if (had_hh_id) {
    # If both original IDs were present, use fcn_id as the canonical public
    # household linkage. This preserves joins while removing original values.
    canonical_source <- dplyr::coalesce(as_clean_character(original_fcn_id),
                                        as_clean_character(original_hh_id))
    data$hh_id <- replace_from_lookup(canonical_source, household_lookup)
  }
  if (had_fcn_id || had_hh_id) {
    data <- data %>% relocate(any_of(c("fcn_id", "hh_id")))
  }

  for (col in intersect(c("KEY", "PARENT_KEY", "uuid"), names(data))) {
    data[[col]] <- replace_from_lookup(data[[col]], record_lookup)
  }
  for (col in intersect(c("PM_monitor", "ambient_site_id", "mission_id"), names(data))) {
    data[[col]] <- replace_from_lookup(data[[col]], monitor_lookup)
  }
  if ("source_mission_ids_with_events" %in% names(data)) {
    data$source_mission_ids_with_events <- "pseudonymized_mission_list"
  }
  for (col in intersect(c("raw_source_file", "source_file"), names(data))) {
    data[[col]] <- replace_from_lookup(data[[col]], source_lookup,
                                       missing_value = "source_not_recorded")
  }
  for (col in intersect(c("hh_id_note", "hh_id_note_raw", "hh_id_note_cleaned_from_file"), names(data))) {
    data[[col]] <- "removed_for_public_release"
  }

  data
}

public_identifier_audit <- function(data, rel_path) {
  nm <- names(data)
  tibble(
    file = rel_path,
    n_rows = nrow(data),
    n_cols = ncol(data),
    has_name_columns = any(str_detect(str_to_lower(nm), "(^|_)name($|_)")),
    has_unhcr_columns = any(str_detect(str_to_lower(nm), "unhcr")),
    has_camp_block_columns = any(str_to_lower(nm) %in% c("camp_id", "block_id", "subblock_id")),
    has_original_fcn_id_values = if ("fcn_id" %in% nm) any(!is.na(data$fcn_id) & !str_detect(as.character(data$fcn_id), "^hh_[0-9]{5}$")) else FALSE,
    has_original_hh_id_values = if ("hh_id" %in% nm) any(!is.na(data$hh_id) & !str_detect(as.character(data$hh_id), "^hh_[0-9]{5}$")) else FALSE
  )
}

collapse_column_names <- function(x) {
  x <- unique(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }
  paste(x, collapse = "; ")
}

public_compatibility_audit <- function(private_data, public_data, rel_path) {
  expected_public_columns <- setdiff(names(private_data), columns_to_drop(private_data))
  missing_columns <- setdiff(expected_public_columns, names(public_data))
  extra_columns <- setdiff(names(public_data), expected_public_columns)
  expected_link_columns <- intersect(
    c("fcn_id", "hh_id", "PARENT_KEY", "study_arm_overall", "study_arm"),
    expected_public_columns
  )
  missing_link_columns <- setdiff(expected_link_columns, names(public_data))

  row_count_matches <- nrow(private_data) == nrow(public_data)
  schema_matches <- length(missing_columns) == 0 && length(extra_columns) == 0
  link_columns_match <- length(missing_link_columns) == 0

  tibble(
    file = rel_path,
    private_n_rows = nrow(private_data),
    public_n_rows = nrow(public_data),
    row_count_match = row_count_matches,
    private_n_cols = ncol(private_data),
    expected_public_n_cols = length(expected_public_columns),
    public_n_cols = ncol(public_data),
    schema_match = schema_matches,
    schema_order_match = identical(names(public_data), expected_public_columns),
    link_columns_expected = collapse_column_names(expected_link_columns),
    link_columns_missing = collapse_column_names(missing_link_columns),
    link_columns_match = link_columns_match,
    missing_public_columns = collapse_column_names(missing_columns),
    extra_public_columns = collapse_column_names(extra_columns)
  )
}
manifest_rows <- list()
compatibility_rows <- list()

i <- 1L
for (file in root_rds_files) {
  input_path <- file.path(input_dir, file)
  output_path <- file.path(output_dir, file)
  data <- read_input(input_path)
  public <- pseudonymize_public_data(data)
  saveRDS(public, output_path)
  manifest_rows[[i]] <- public_identifier_audit(public, file) %>%
    mutate(output_path = normalizePath(output_path, winslash = "/", mustWork = TRUE),
           source_role = "cleaned_analysis_input")
  compatibility_rows[[i]] <- public_compatibility_audit(data, public, file) %>%
    mutate(source_role = "cleaned_analysis_input")
  i <- i + 1L
}

for (file in imported_rds_files) {
  input_path <- file.path(input_dir, "imported_raw", file)
  output_path <- file.path(output_dir, "imported_raw", file)
  data <- read_input(input_path)
  public <- pseudonymize_public_data(data)
  saveRDS(public, output_path)
  rel_path <- file.path("imported_raw", file)
  manifest_rows[[i]] <- public_identifier_audit(public, rel_path) %>%
    mutate(output_path = normalizePath(output_path, winslash = "/", mustWork = TRUE),
           source_role = "derived_geocene_analysis_input")
  compatibility_rows[[i]] <- public_compatibility_audit(data, public, rel_path) %>%
    mutate(source_role = "derived_geocene_analysis_input")
  i <- i + 1L
}

manifest <- bind_rows(manifest_rows) %>%
  select(file, source_role, n_rows, n_cols, everything())

compatibility <- bind_rows(compatibility_rows) %>%
  select(
    file, source_role, private_n_rows, public_n_rows, row_count_match,
    private_n_cols, expected_public_n_cols, public_n_cols,
    schema_match, schema_order_match, link_columns_expected,
    link_columns_missing, link_columns_match,
    missing_public_columns, extra_public_columns
  )

readr::write_csv(
  compatibility,
  file.path(output_dir, "public_clean_final_private_public_compatibility.csv"),
  na = ""
)

if (any(manifest$has_name_columns | manifest$has_unhcr_columns |
        manifest$has_camp_block_columns | manifest$has_original_fcn_id_values |
        manifest$has_original_hh_id_values)) {
  readr::write_csv(manifest, file.path(restricted_dir, "failed_public_clean_final_manifest.csv"), na = "")
  readr::write_csv(compatibility, file.path(restricted_dir, "failed_public_clean_final_private_public_compatibility.csv"), na = "")
  stop("Public cleaned-data export failed privacy checks. See restricted manifest.",
       call. = FALSE)
}

if (any(!compatibility$row_count_match |
        !compatibility$schema_match |
        !compatibility$link_columns_match)) {
  readr::write_csv(
    compatibility,
    file.path(restricted_dir, "failed_public_clean_final_private_public_compatibility.csv"),
    na = ""
  )
  stop(
    "Public cleaned-data export failed private/public row-count or schema compatibility checks. See restricted compatibility report.",
    call. = FALSE
  )
}

readr::write_csv(manifest, file.path(output_dir, "public_clean_final_manifest.csv"), na = "")

readme <- c(
  "# Public Cleaned Analysis Data",
  "",
  "This folder contains the de-identified cleaned data files intended for public sharing and rerunning the reviewed RF105 analyses.",
  "",
  "Included files are the cleaned analysis inputs required by the reviewed RF105 scripts. Internal audit CSVs, raw import manifests, correction logs, and household-level QA listings are intentionally excluded.",
  "",
  "De-identification decisions:",
  "- Original fcn_id and hh_id values were replaced with stable public household pseudonyms while retaining the column names needed by the analysis code.",
  "- Name fields, UNHCR identifiers, camp_id, block_id, subblock_id, enumerator, and derived/raw ID-note fields were removed.",
  "- KEY, PARENT_KEY, uuid, PM monitor, mission, and source-file fields were pseudonymized when retained for joins or reproducibility checks.",
  "- Exact dates and times were retained because they are needed for PM2.5 ambient matching and Geocene denominator checks.",
  "",
  "To run reviewed analyses against this folder without editing code, set:",
  "",
  "```r",
  "Sys.setenv(RF105_CLEAN_DATA_DIR = \"4_data/clean_final_public\")",
  "```",
  "",
  "The file `public_clean_final_manifest.csv` documents the included files and privacy checks.",
  "The file `public_clean_final_private_public_compatibility.csv` verifies row counts and expected public schemas against the private cleaned source files."
)
writeLines(readme, file.path(output_dir, "README_clean_final_public.md"))

message("Wrote public cleaned-data folder: ", normalizePath(output_dir, winslash = "/", mustWork = TRUE))
message("Included RDS files: ", length(root_rds_files) + length(imported_rds_files))
source(file.path("1_data_import", "fixed", "geocene_pipeline_helpers.R"))
geocene_export_public(household_lookup)
