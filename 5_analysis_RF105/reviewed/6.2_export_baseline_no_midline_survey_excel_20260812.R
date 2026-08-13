# Export identified survey rows for refugee households observed at baseline but not midline
#
# Purpose: Create a restricted Excel workbook for follow-up QA of households that
#          have cleaned baseline survey data but no cleaned midline survey data.
#          This includes direct identifiers and must remain in 8_restricted.
# Inputs:  4_data/clean_final/survey_refugee_household.rds
#          4_data/clean_final/imported_raw/survey_refugee_household_raw.rds
# Outputs: 8_restricted/RF105_reviewed_20260812/identified_tables/
#          survey_refugee_baseline_no_midline_identifiers.xlsx
#          survey_refugee_baseline_no_midline_excel_export_verification.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(writexl)
  library(openxlsx)
})

get_script_dir <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_path <- command_args[startsWith(command_args, file_arg)]
  if (length(script_path) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", script_path[1]), winslash = "/", mustWork = TRUE)))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    return(dirname(normalizePath(rstudioapi::getActiveDocumentContext()$path, winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

script_dir <- get_script_dir()
source(file.path(script_dir, "0_RF105_config_20260805_2213.R"))

stop_if_missing <- function(path, label) {
  if (!file.exists(path)) {
    stop(label, " does not exist: ", path, call. = FALSE)
  }
}

clean_excel_text <- function(x) {
  x <- as.character(x)
  x <- gsub("[\001-\010\013\014\016-\037]", "", x)
  x
}

excel_safe <- function(df) {
  df %>% mutate(across(where(is.character), clean_excel_text))
}

ensure_columns <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA_character_
    }
  }
  df
}

collapse_unique <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    return(NA_character_)
  }
  paste(sort(unique(x)), collapse = " | ")
}

write_restricted_xlsx <- function(sheets, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sheets <- lapply(sheets, excel_safe)
  writexl::write_xlsx(sheets, path)
  invisible(path)
}

verify_xlsx <- function(path, expected) {
  workbook <- openxlsx::loadWorkbook(path)
  sheet_names <- openxlsx::sheets(workbook)
  observed <- tibble(
    workbook = basename(path),
    sheet = sheet_names,
    observed_data_rows = vapply(sheet_names, function(sheet) {
      nrow(openxlsx::read.xlsx(path, sheet = sheet, colNames = TRUE))
    }, integer(1)),
    observed_columns = vapply(sheet_names, function(sheet) {
      ncol(openxlsx::read.xlsx(path, sheet = sheet, colNames = TRUE))
    }, integer(1))
  )

  observed %>%
    left_join(expected, by = "sheet") %>%
    mutate(
      rows_match_expected = observed_data_rows == expected_data_rows,
      columns_match_expected = observed_columns == expected_columns
    )
}

survey_clean_path <- file_survey_refugee_household
survey_raw_path <- file.path(dir_clean_final, "imported_raw", "survey_refugee_household_raw.rds")

stop_if_missing(survey_clean_path, "Cleaned refugee household survey file")
stop_if_missing(survey_raw_path, "Imported raw refugee household survey file")

cleaned_survey_source <- readRDS(survey_clean_path)
cleaned_population <- cleaned_survey_source %>%
  add_rf105_aliases() %>%
  make_analysis_population()

cleaned_survey <- cleaned_population$all_deduplicated %>%
  clean_timepoint_arm() %>%
  mutate(
    fcn_id_clean = str_squish(as.character(fcn_id)),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  ) %>%
  filter(!is.na(fcn_id_clean), fcn_id_clean != "")

participant_presence <- cleaned_survey %>%
  distinct(fcn_id_clean, timepoint, study_arm_overall) %>%
  group_by(fcn_id_clean) %>%
  summarise(
    observed_at_baseline = any(timepoint == "baseline"),
    observed_at_midline = any(timepoint == "midline"),
    observed_at_endline = any(timepoint == "endline"),
    arm = first(na.omit(study_arm_overall[timepoint == "baseline"]), default = NA_character_),
    .groups = "drop"
  ) %>%
  mutate(arm = if_else(is.na(arm), "missing", arm))

target_households <- participant_presence %>%
  filter(observed_at_baseline, !observed_at_midline) %>%
  arrange(arm, fcn_id_clean)

target_survey_rows <- cleaned_survey %>%
  filter(fcn_id_clean %in% target_households$fcn_id_clean) %>%
  arrange(study_arm_overall, fcn_id_clean, factor(timepoint, levels = timepoint_levels))

raw_household <- readRDS(survey_raw_path) %>%
  ensure_columns(c(
    "fcn_id", "study_arm_overall", "timepoint", "name_respondent", "target_child_name",
    "name_hh_head", "UNHCR_id", "camp_id", "block_id", "subblock_id", "hh_id",
    "raw_source_file", "raw_source_path", "raw_survey_version", "raw_collection_round"
  )) %>%
  clean_timepoint_arm() %>%
  mutate(
    fcn_id_clean = str_squish(as.character(fcn_id)),
    timepoint = as.character(timepoint)
  )

raw_identifier_summary <- raw_household %>%
  filter(fcn_id_clean %in% target_households$fcn_id_clean) %>%
  group_by(fcn_id_clean, timepoint) %>%
  summarise(
    raw_name_respondent = collapse_unique(name_respondent),
    raw_target_child_name = collapse_unique(target_child_name),
    raw_name_hh_head = collapse_unique(name_hh_head),
    raw_UNHCR_id = collapse_unique(UNHCR_id),
    raw_camp_id = collapse_unique(camp_id),
    raw_block_id = collapse_unique(block_id),
    raw_subblock_id = collapse_unique(subblock_id),
    raw_hh_id = collapse_unique(hh_id),
    raw_source_file_for_identifiers = collapse_unique(raw_source_file),
    raw_source_path_for_identifiers = collapse_unique(raw_source_path),
    raw_survey_version_for_identifiers = collapse_unique(raw_survey_version),
    raw_collection_round_for_identifiers = collapse_unique(raw_collection_round),
    raw_identifier_rows_n = n(),
    .groups = "drop"
  )

cleaned_output_rows <- target_survey_rows %>%
  ensure_columns(c(
    "fcn_id", "study_arm_overall", "timepoint", "UNHCR_id", "camp_id", "block_id",
    "subblock_id", "hh_id", "raw_source_file", "raw_source_path", "raw_survey_version",
    "raw_collection_round"
  )) %>%
  transmute(
    fcn_id = fcn_id_clean,
    arm = study_arm_overall,
    timepoint,
    UNHCR_id = as.character(UNHCR_id),
    camp = as.character(camp_id),
    block = as.character(block_id),
    `sub-block` = as.character(subblock_id),
    hh_id = as.character(hh_id),
    cleaned_raw_source_file = as.character(raw_source_file),
    cleaned_raw_source_path = as.character(raw_source_path),
    cleaned_raw_survey_version = as.character(raw_survey_version),
    cleaned_raw_collection_round = as.character(raw_collection_round)
  ) %>%
  left_join(raw_identifier_summary, by = c("fcn_id" = "fcn_id_clean", "timepoint")) %>%
  mutate(
    respondent_name = coalesce(raw_name_respondent, NA_character_),
    target_child_name = coalesce(raw_target_child_name, NA_character_),
    household_head_name = coalesce(raw_name_hh_head, NA_character_),
    UNHCR_id = coalesce(UNHCR_id, raw_UNHCR_id),
    camp = coalesce(camp, raw_camp_id),
    block = coalesce(block, raw_block_id),
    `sub-block` = coalesce(`sub-block`, raw_subblock_id),
    hh_id = coalesce(hh_id, raw_hh_id)
  ) %>%
  select(
    fcn_id, arm, timepoint, respondent_name, target_child_name, household_head_name,
    UNHCR_id, camp, block, `sub-block`, hh_id,
    cleaned_raw_source_file, cleaned_raw_source_path, cleaned_raw_survey_version,
    cleaned_raw_collection_round, raw_source_file_for_identifiers,
    raw_source_path_for_identifiers, raw_survey_version_for_identifiers,
    raw_collection_round_for_identifiers, raw_identifier_rows_n
  ) %>%
  arrange(arm, fcn_id, factor(timepoint, levels = timepoint_levels))

household_summary <- target_households %>%
  left_join(
    cleaned_output_rows %>%
      count(fcn_id, timepoint, name = "cleaned_rows_n") %>%
      pivot_wider(names_from = timepoint, values_from = cleaned_rows_n, values_fill = 0,
                  names_prefix = "cleaned_rows_"),
    by = c("fcn_id_clean" = "fcn_id")
  ) %>%
  ensure_columns(c("cleaned_rows_baseline", "cleaned_rows_midline", "cleaned_rows_endline")) %>%
  transmute(
    fcn_id = fcn_id_clean,
    arm,
    observed_at_baseline,
    observed_at_midline,
    observed_at_endline,
    cleaned_rows_baseline = coalesce(as.integer(cleaned_rows_baseline), 0L),
    cleaned_rows_midline = coalesce(as.integer(cleaned_rows_midline), 0L),
    cleaned_rows_endline = coalesce(as.integer(cleaned_rows_endline), 0L)
  ) %>%
  arrange(arm, fcn_id)

metadata <- tibble(
  field = c(
    "created_at",
    "created_by_script",
    "cleaned_survey_source",
    "raw_identifier_source",
    "target_definition",
    "households_exported",
    "survey_rows_exported",
    "privacy_note"
  ),
  value = c(
    as.character(Sys.time()),
    normalizePath(file.path(script_dir, "6.2_export_baseline_no_midline_survey_excel_20260812.R"), winslash = "/", mustWork = TRUE),
    normalizePath(survey_clean_path, winslash = "/", mustWork = TRUE),
    normalizePath(survey_raw_path, winslash = "/", mustWork = TRUE),
    "Distinct fcn_id values with cleaned baseline survey data and no cleaned midline survey data; all available cleaned survey rows for those fcn_id values are included.",
    as.character(nrow(target_households)),
    as.character(nrow(cleaned_output_rows)),
    "Contains direct identifiers and should remain in 8_restricted. Do not commit or share outside approved study workflows."
  )
)

restricted_output_dir <- file.path(dir_restricted_reviewed, "identified_tables")
baseline_no_midline_xlsx <- file.path(restricted_output_dir, "survey_refugee_baseline_no_midline_identifiers.xlsx")
verification_csv <- file.path(restricted_output_dir, "survey_refugee_baseline_no_midline_excel_export_verification.csv")

write_restricted_xlsx(
  list(
    baseline_no_midline_survey = cleaned_output_rows,
    target_household_summary = household_summary,
    metadata = metadata
  ),
  baseline_no_midline_xlsx
)

verification <- verify_xlsx(
  baseline_no_midline_xlsx,
  tibble(
    sheet = c("baseline_no_midline_survey", "target_household_summary", "metadata"),
    expected_data_rows = c(nrow(cleaned_output_rows), nrow(household_summary), nrow(metadata)),
    expected_columns = c(ncol(cleaned_output_rows), ncol(household_summary), ncol(metadata))
  )
)

write_csv(verification, verification_csv)

if (!all(verification$rows_match_expected, verification$columns_match_expected)) {
  stop("Excel verification failed for one or more sheets. See: ", verification_csv, call. = FALSE)
}

message("Wrote restricted baseline-no-midline survey export: ", baseline_no_midline_xlsx)
message("Wrote verification file: ", verification_csv)
message("Households exported: ", nrow(target_households))
message("Survey rows exported: ", nrow(cleaned_output_rows))