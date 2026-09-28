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

required_packages <- c("tidyverse", "openssl")
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

dir.create(restricted_dir, recursive = TRUE, showWarnings = FALSE)
staging_parent <- file.path(restricted_dir, "staging")
dir.create(staging_parent, recursive = TRUE, showWarnings = FALSE)
staging_dir <- tempfile("clean_final_public_", tmpdir = staging_parent)
if (!dir.create(staging_dir, recursive = FALSE, showWarnings = FALSE)) {
  stop("Could not create restricted public-export staging directory.", call. = FALSE)
}
dir.create(file.path(staging_dir, "imported_raw"), recursive = TRUE, showWarnings = FALSE)

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

generate_random_public_ids <- function(n, prefix, existing = character()) {
  if (n == 0L) return(character())
  generated <- character()
  while (length(generated) < n) {
    batch_size <- max(32L, n - length(generated))
    candidates <- vapply(seq_len(batch_size), function(i) {
      token <- paste(format(openssl::rand_bytes(12L)), collapse = "")
      paste0(prefix, "_", token)
    }, character(1))
    generated <- unique(c(generated, setdiff(candidates, c(existing, generated))))
  }
  generated[seq_len(n)]
}

load_or_extend_public_lookup <- function(values, prefix, crosswalk_path) {
  values <- sort(unique(as_clean_character(values)))
  values <- values[!is.na(values)]

  lookup <- if (file.exists(crosswalk_path)) {
    readr::read_csv(
      crosswalk_path,
      col_types = readr::cols(.default = readr::col_character()),
      show_col_types = FALSE
    )
  } else {
    tibble(original = character(), public = character())
  }
  if (!all(c("original", "public") %in% names(lookup))) {
    stop("Restricted crosswalk has an invalid schema: ", crosswalk_path, call. = FALSE)
  }
  lookup <- lookup %>%
    transmute(original = as_clean_character(original), public = as_clean_character(public))
  expected_pattern <- paste0("^", prefix, "_[a-z0-9]{24}$")
  if (anyNA(lookup$original) || anyNA(lookup$public) ||
      anyDuplicated(lookup$original) || anyDuplicated(lookup$public) ||
      any(!str_detect(lookup$public, expected_pattern))) {
    stop("Restricted crosswalk failed uniqueness or format checks: ", crosswalk_path,
         call. = FALSE)
  }

  new_values <- setdiff(values, lookup$original)
  if (length(new_values)) {
    lookup <- bind_rows(
      lookup,
      tibble(
        original = new_values,
        public = generate_random_public_ids(length(new_values), prefix, lookup$public)
      )
    )
  }
  lookup <- arrange(lookup, original)
  dir.create(dirname(crosswalk_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(lookup, crosswalk_path, na = "")
  lookup
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
household_lookup <- load_or_extend_public_lookup(
  household_values,
  "hh",
  file.path(restricted_dir, "household_key_crosswalk.csv")
)

record_values <- unlist(lapply(all_data_for_maps, function(data) {
  values <- list()
  if ("KEY" %in% names(data)) values$KEY <- data$KEY
  if ("PARENT_KEY" %in% names(data)) values$PARENT_KEY <- data$PARENT_KEY
  if ("uuid" %in% names(data)) values$uuid <- data$uuid
  values
}), use.names = FALSE)
record_lookup <- load_or_extend_public_lookup(
  record_values,
  "record",
  file.path(restricted_dir, "record_key_crosswalk.csv")
)

monitor_values <- unlist(lapply(all_data_for_maps, function(data) {
  cols <- intersect(c("PM_monitor", "ambient_site_id", "mission_id", "source_mission_ids_with_events"), names(data))
  unlist(data[cols], use.names = FALSE)
}), use.names = FALSE)
monitor_lookup <- load_or_extend_public_lookup(
  monitor_values,
  "monitor",
  file.path(restricted_dir, "monitor_key_crosswalk.csv")
)

source_values <- unlist(lapply(all_data_for_maps, function(data) {
  cols <- intersect(c("raw_source_file", "source_file"), names(data))
  unlist(data[cols], use.names = FALSE)
}), use.names = FALSE)
source_lookup <- load_or_extend_public_lookup(
  source_values,
  "source",
  file.path(restricted_dir, "source_key_crosswalk.csv")
)

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
    "^instancename$", "^instanceid$",
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
  if (had_fcn_id && had_hh_id) {
    canonical_rows <- !is.na(as_clean_character(original_fcn_id))
    if (any(data$fcn_id[canonical_rows] != data$hh_id[canonical_rows], na.rm = TRUE)) {
      stop("Public fcn_id and hh_id mappings disagree for a canonical household.",
           call. = FALSE)
    }
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
    has_instance_columns = any(str_to_lower(nm) %in% c("instancename", "instanceid")),
    has_original_fcn_id_values = if ("fcn_id" %in% nm) any(!is.na(data$fcn_id) & !str_detect(as.character(data$fcn_id), "^hh_[a-z0-9]{24}$")) else FALSE,
    has_original_hh_id_values = if ("hh_id" %in% nm) any(!is.na(data$hh_id) & !str_detect(as.character(data$hh_id), "^hh_[a-z0-9]{24}$")) else FALSE
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

public_output_path <- function(rel_path) {
  str_replace_all(file.path("4_data", "clean_final_public", rel_path), "\\\\", "/")
}

i <- 1L
for (file in root_rds_files) {
  input_path <- file.path(input_dir, file)
  output_path <- file.path(staging_dir, file)
  data <- read_input(input_path)
  public <- pseudonymize_public_data(data)
  saveRDS(public, output_path)
  manifest_rows[[i]] <- public_identifier_audit(public, file) %>%
    mutate(output_path = public_output_path(file),
           source_role = "cleaned_analysis_input")
  compatibility_rows[[i]] <- public_compatibility_audit(data, public, file) %>%
    mutate(source_role = "cleaned_analysis_input")
  i <- i + 1L
}

for (file in imported_rds_files) {
  input_path <- file.path(input_dir, "imported_raw", file)
  output_path <- file.path(staging_dir, "imported_raw", file)
  data <- read_input(input_path)
  public <- pseudonymize_public_data(data)
  saveRDS(public, output_path)
  rel_path <- file.path("imported_raw", file)
  manifest_rows[[i]] <- public_identifier_audit(public, rel_path) %>%
    mutate(output_path = public_output_path(rel_path),
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

if (any(manifest$has_name_columns | manifest$has_unhcr_columns |
        manifest$has_camp_block_columns | manifest$has_instance_columns |
        manifest$has_original_fcn_id_values |
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

readme <- c(
  "# Public Cleaned Analysis Data",
  "",
  "This folder contains the de-identified cleaned data files intended for public sharing and rerunning the reviewed RF105 analyses.",
  "",
  "Included files are the cleaned analysis inputs required by the reviewed RF105 scripts. Internal audit CSVs, raw import manifests, correction logs, and household-level QA listings are intentionally excluded.",
  "",
  "De-identification decisions:",
  "- Original fcn_id and hh_id values were replaced with persistent random public household pseudonyms while retaining the column names needed by the analysis code. The private crosswalk is stored only under 8_restricted.",
  "- Name fields, UNHCR identifiers, camp_id, block_id, subblock_id, enumerator, and derived/raw ID-note fields were removed.",
  "- ODK instanceName and instanceID fields were removed because they can embed original household identifiers.",
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
writeLines(readme, file.path(staging_dir, "README_clean_final_public.md"))

source(file.path(project_root, "1_data_import", "fixed", "geocene_pipeline_helpers.R"))
geocene_export_public(
  household_lookup = household_lookup,
  private_root = input_dir,
  public_root = staging_dir,
  crosswalk_dir = restricted_dir
)

geocene_expected_files <- unlist(lapply(geocene_variants, function(variant) {
  rel_root <- file.path("geocene", variant)
  expected <- file.path(rel_root, c("events.rds", "household_days.rds"))
  private_summary <- file.path(input_dir, "geocene", variant, "mission_exclusion_summary.rds")
  if (file.exists(private_summary)) {
    expected <- c(expected, file.path(rel_root, "mission_exclusion_summary.rds"))
  }
  expected
}), use.names = FALSE)

for (rel_path in geocene_expected_files) {
  private <- read_input(file.path(input_dir, rel_path))
  public <- read_input(file.path(staging_dir, rel_path))
  source_role <- if (basename(rel_path) == "mission_exclusion_summary.rds") {
    "geocene_exclusion_summary"
  } else {
    "derived_geocene_analysis_input"
  }
  manifest <- bind_rows(
    manifest,
    public_identifier_audit(public, rel_path) %>%
      mutate(output_path = public_output_path(rel_path), source_role = source_role)
  )
  compatibility <- bind_rows(
    compatibility,
    public_compatibility_audit(private, public, rel_path) %>%
      mutate(source_role = source_role)
  )
}
manifest <- manifest %>%
  select(file, source_role, n_rows, n_cols, everything()) %>%
  arrange(file)
compatibility <- compatibility %>%
  select(
    file, source_role, private_n_rows, public_n_rows, row_count_match,
    private_n_cols, expected_public_n_cols, public_n_cols,
    schema_match, schema_order_match, link_columns_expected,
    link_columns_missing, link_columns_match,
    missing_public_columns, extra_public_columns
  ) %>%
  arrange(file)

if (any(manifest$has_name_columns | manifest$has_unhcr_columns |
        manifest$has_camp_block_columns | manifest$has_instance_columns |
        manifest$has_original_fcn_id_values |
        manifest$has_original_hh_id_values)) {
  readr::write_csv(manifest, file.path(restricted_dir, "failed_public_clean_final_manifest.csv"), na = "")
  readr::write_csv(compatibility, file.path(restricted_dir, "failed_public_clean_final_private_public_compatibility.csv"), na = "")
  stop("Public Geocene export failed privacy checks. See restricted manifest.",
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
    "Public Geocene export failed private/public row-count or schema compatibility checks. See restricted compatibility report.",
    call. = FALSE
  )
}
readr::write_csv(manifest, file.path(staging_dir, "public_clean_final_manifest.csv"), na = "")
readr::write_csv(
  compatibility,
  file.path(staging_dir, "public_clean_final_private_public_compatibility.csv"),
  na = ""
)

expected_public_files <- sort(unique(c(
  root_rds_files,
  file.path("imported_raw", imported_rds_files),
  geocene_expected_files,
  "public_clean_final_manifest.csv",
  "public_clean_final_private_public_compatibility.csv",
  "README_clean_final_public.md"
)))
expected_public_files <- str_replace_all(expected_public_files, "\\\\", "/")
actual_public_files <- list.files(
  staging_dir,
  recursive = TRUE,
  all.files = TRUE,
  full.names = FALSE,
  include.dirs = FALSE,
  no.. = TRUE
) %>%
  str_replace_all("\\\\", "/") %>%
  sort()

file_audit <- full_join(
  tibble(file = expected_public_files, expected = TRUE),
  tibble(file = actual_public_files, present = TRUE),
  by = "file"
) %>%
  mutate(expected = replace_na(expected, FALSE), present = replace_na(present, FALSE)) %>%
  arrange(file)
readr::write_csv(
  file_audit,
  file.path(restricted_dir, "staged_public_file_audit.csv"),
  na = ""
)
if (any(!file_audit$expected | !file_audit$present)) {
  stop(
    "Fresh public-export staging directory contains missing or unexpected files. ",
    "See restricted staged_public_file_audit.csv.",
    call. = FALSE
  )
}

staged_privacy_audit <- map_dfr(actual_public_files[str_ends(actual_public_files, ".rds")], function(rel_path) {
  public_identifier_audit(read_input(file.path(staging_dir, rel_path)), rel_path)
})
if (any(staged_privacy_audit$has_name_columns |
        staged_privacy_audit$has_unhcr_columns |
        staged_privacy_audit$has_camp_block_columns |
        staged_privacy_audit$has_instance_columns |
        staged_privacy_audit$has_original_fcn_id_values |
        staged_privacy_audit$has_original_hh_id_values)) {
  readr::write_csv(
    staged_privacy_audit,
    file.path(restricted_dir, "failed_staged_public_privacy_audit.csv"),
    na = ""
  )
  stop("Fresh public-export staging directory failed privacy checks.", call. = FALSE)
}

publish_staged_directory <- function(staged, destination, restricted) {
  destination_parent <- normalizePath(dirname(destination), winslash = "/", mustWork = TRUE)
  destination_path <- normalizePath(destination, winslash = "/", mustWork = FALSE)
  if (!identical(tolower(dirname(destination_path)), tolower(destination_parent)) ||
      basename(destination_path) != "clean_final_public") {
    stop("Refusing to replace an unexpected public-output path: ", destination,
         call. = FALSE)
  }
  if (file.exists(destination) && !dir.exists(destination)) {
    stop("Public-output path exists but is not a directory: ", destination, call. = FALSE)
  }

  archived <- NA_character_
  if (dir.exists(destination)) {
    archive_root <- file.path(restricted, "previous_public_exports")
    dir.create(archive_root, recursive = TRUE, showWarnings = FALSE)
    archived <- tempfile("clean_final_public_", tmpdir = archive_root)
    if (!file.rename(destination, archived)) {
      stop("Could not archive the prior public export; staged files were not published.",
           call. = FALSE)
    }
  }
  if (!file.rename(staged, destination)) {
    if (!is.na(archived) && !dir.exists(destination)) {
      file.rename(archived, destination)
    }
    stop("Could not publish the staged public export; the prior export was restored when possible.",
         call. = FALSE)
  }
  archived
}

archived_output <- publish_staged_directory(staging_dir, output_dir, restricted_dir)
message("Wrote fresh public cleaned-data folder: ", normalizePath(output_dir, winslash = "/", mustWork = TRUE))
if (!is.na(archived_output)) {
  message("Archived prior public folder under restricted storage: ", archived_output)
}
message("Included RDS files: ", sum(str_ends(actual_public_files, ".rds")))
