################################################################################
# RF105 restricted midline survey Excel exports
#
# Purpose:
#   Create restricted Excel workbooks for internal review of the cleaned midline
#   refugee household survey data and the midline households that do not have a
#   cleaned baseline household survey record.
#
# Inputs:
#   4_data/clean_final/survey_refugee_household.rds
#   4_data/clean_final/imported_raw/survey_refugee_household_raw.rds
#
# Outputs, restricted internal use only:
#   8_restricted/RF105_reviewed_YYYYMMDD/identified_tables/
#     survey_refugee_midline_cleaned.xlsx
#     survey_refugee_midline_no_baseline_identifiers.xlsx
#
# Notes:
#   - Participant flow and target fcn_id selection use one deduplicated household
#     record per fcn_id-timepoint, matching make_analysis_population().
#   - The second workbook includes direct identifiers and names for manual review;
#     it must not be copied into public or shareable outputs.
################################################################################

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)

  if (length(file_arg) == 1) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg),
                                 winslash = "/", mustWork = FALSE)))
  }

  source_files <- vapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile)) frame$ofile else NA_character_
  }, character(1))
  source_files <- source_files[!is.na(source_files)]

  if (length(source_files) > 0) {
    return(dirname(normalizePath(source_files[[length(source_files)]],
                                 winslash = "/", mustWork = FALSE)))
  }

  getwd()
}

script_dir <- get_script_dir()
config_file_candidates <- c(
  file.path(script_dir, "0_RF105_config_20260805_2213.R"),
  file.path(getwd(), "5_analysis_RF105", "reviewed", "0_RF105_config_20260805_2213.R"),
  file.path(Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = ""),
            "5_analysis_RF105", "reviewed", "0_RF105_config_20260805_2213.R")
)
config_file <- config_file_candidates[file.exists(config_file_candidates)][1]

if (is.na(config_file)) {
  stop("Could not find 0_RF105_config_20260805_2213.R. Set ROHINGYA_ANALYSIS_ROOT.", call. = FALSE)
}

source(config_file)

if (!requireNamespace("writexl", quietly = TRUE)) {
  stop("Install package `writexl` before creating Excel outputs.", call. = FALSE)
}
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Install package `openxlsx` before verifying Excel outputs.", call. = FALSE)
}

collapse_unique <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  paste(sort(unique(x)), collapse = "; ")
}

excel_safe <- function(df) {
  out <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- make.unique(names(out), sep = "_")

  out[] <- lapply(out, function(x) {
    if (inherits(x, "POSIXt")) {
      return(format(x, "%Y-%m-%d %H:%M:%S"))
    }
    if (inherits(x, "Date")) {
      return(as.character(x))
    }
    if (is.factor(x)) {
      return(as.character(x))
    }
    if (is.list(x)) {
      return(vapply(x, function(value) {
        value <- unlist(value, recursive = TRUE, use.names = FALSE)
        value <- stringr::str_squish(as.character(value))
        value <- value[!is.na(value) & value != ""]
        if (length(value) == 0) "" else paste(value, collapse = "; ")
      }, character(1)))
    }
    if (is.character(x)) {
      return(gsub("[\001-\010\013\014\016-\037]", "", x))
    }
    x
  })

  out
}

write_restricted_xlsx <- function(sheets, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  sheets <- lapply(sheets, excel_safe)
  writexl::write_xlsx(sheets, path = path, format_headers = TRUE)
  message("Wrote restricted Excel workbook: ", path)
  invisible(path)
}

verify_xlsx <- function(path, sheets) {
  workbook <- openxlsx::loadWorkbook(path)
  sheet_names <- names(workbook)
  missing_sheets <- setdiff(names(sheets), sheet_names)
  if (length(missing_sheets) > 0) {
    stop(
      "Workbook verification failed for ", path,
      "; missing sheets: ", paste(missing_sheets, collapse = ", "),
      call. = FALSE
    )
  }
  tibble(
    output_file = normalizePath(path, winslash = "/", mustWork = FALSE),
    sheet_name = names(sheets),
    expected_data_rows = vapply(sheets, nrow, integer(1)),
    expected_columns = vapply(sheets, ncol, integer(1)),
    sheet_present = TRUE,
    verified_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
}

cleaned_survey_source <- readRDS(file_survey_refugee_household)
cleaned_survey_for_flow <- cleaned_survey_source %>%
  add_rf105_aliases()

survey_population <- make_analysis_population(cleaned_survey_for_flow)
cleaned_survey <- survey_population$all_deduplicated %>%
  clean_timepoint_arm() %>%
  mutate(
    fcn_id_clean = stringr::str_squish(as.character(fcn_id)),
    fcn_id_clean = na_if(fcn_id_clean, "")
  ) %>%
  filter(
    !is.na(fcn_id_clean),
    !is.na(timepoint),
    timepoint %in% timepoint_levels,
    !is.na(study_arm_overall)
  )

cleaned_midline <- cleaned_survey_source %>%
  clean_timepoint_arm() %>%
  mutate(
    fcn_id_clean = stringr::str_squish(as.character(fcn_id)),
    fcn_id_clean = na_if(fcn_id_clean, "")
  ) %>%
  filter(timepoint == "midline") %>%
  arrange(study_arm_overall, fcn_id_clean) %>%
  select(-fcn_id_clean)

participant_presence <- cleaned_survey %>%
  distinct(fcn_id_clean, timepoint, study_arm_overall) %>%
  group_by(fcn_id_clean) %>%
  summarise(
    baseline = any(timepoint == "baseline"),
    midline = any(timepoint == "midline"),
    endline = any(timepoint == "endline"),
    baseline_arm = first(study_arm_overall[timepoint == "baseline" & !is.na(study_arm_overall)], default = NA),
    first_observed_arm = first(study_arm_overall[!is.na(study_arm_overall)], default = NA),
    .groups = "drop"
  ) %>%
  mutate(
    study_arm_overall = coalesce(baseline_arm, first_observed_arm),
    study_arm_overall = factor(as.character(study_arm_overall), levels = arm_levels)
  )

target_midline_no_baseline <- participant_presence %>%
  filter(!baseline, midline) %>%
  arrange(study_arm_overall, fcn_id_clean) %>%
  transmute(
    fcn_id_clean,
    study_arm_overall = as.character(study_arm_overall),
    observed_at_endline = endline
  )

target_cleaned_midline <- cleaned_survey %>%
  filter(timepoint == "midline", fcn_id_clean %in% target_midline_no_baseline$fcn_id_clean) %>%
  arrange(study_arm_overall, fcn_id_clean)

raw_household_file <- file.path(dir_clean_final, "imported_raw", "survey_refugee_household_raw.rds")
raw_household <- readRDS(raw_household_file) %>%
  clean_timepoint_arm() %>%
  mutate(
    fcn_id_clean = stringr::str_squish(as.character(fcn_id)),
    fcn_id_clean = na_if(fcn_id_clean, "")
  ) %>%
  filter(timepoint == "midline", fcn_id_clean %in% target_midline_no_baseline$fcn_id_clean)

raw_name_summary <- raw_household %>%
  group_by(fcn_id_clean) %>%
  summarise(
    name_respondent = collapse_unique(.data[["name_respondent"]]),
    name_hh_head = collapse_unique(.data[["name_hh_head"]]),
    target_child_name = collapse_unique(.data[["target_child_name"]]),
    raw_source_file_for_names = collapse_unique(.data[["raw_source_file"]]),
    raw_name_rows_n = dplyr::n(),
    .groups = "drop"
  )

restricted_identifier_fields <- target_cleaned_midline %>%
  transmute(
    fcn_id = fcn_id_clean,
    study_arm_overall = as.character(study_arm_overall),
    timepoint = as.character(timepoint),
    hh_id = as.character(hh_id),
    UNHCR_id = as.character(UNHCR_id),
    camp_id = as.character(camp_id),
    block_id = as.character(block_id),
    subblock_id = as.character(subblock_id)
  ) %>%
  left_join(raw_name_summary, by = c("fcn_id" = "fcn_id_clean")) %>%
  left_join(target_midline_no_baseline, by = c("fcn_id" = "fcn_id_clean", "study_arm_overall")) %>%
  select(
    fcn_id, study_arm_overall, timepoint, observed_at_endline,
    name_respondent, target_child_name, name_hh_head,
    UNHCR_id, camp_id, block_id, subblock_id, hh_id,
    raw_source_file_for_names, raw_name_rows_n
  ) %>%
  arrange(study_arm_overall, fcn_id)

script_path <- file.path(script_dir, "6.1_export_midline_survey_excel_20260812.R")

metadata <- tibble(
  field = c(
    "generated_at",
    "source_cleaned_data",
    "source_raw_data_for_names",
    "script",
    "timepoint_filter",
    "participant_definition",
    "midline_no_baseline_rule",
    "restriction"
  ),
  value = c(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    normalizePath(file_survey_refugee_household, winslash = "/", mustWork = FALSE),
    normalizePath(raw_household_file, winslash = "/", mustWork = FALSE),
    normalizePath(script_path, winslash = "/", mustWork = FALSE),
    "timepoint == midline",
    "Workbook 1 exports cleaned midline rows as stored in survey_refugee_household.rds; the 12-household target set uses one deduplicated household record per fcn_id-timepoint via make_analysis_population()",
    "households observed at midline and not observed at baseline in cleaned survey_refugee_household.rds",
    "restricted_internal_only; includes direct household identifiers and/or names"
  )
)

restricted_output_dir <- file.path(dir_restricted_reviewed, "identified_tables")
midline_cleaned_xlsx <- file.path(restricted_output_dir, "survey_refugee_midline_cleaned.xlsx")
midline_no_baseline_xlsx <- file.path(restricted_output_dir, "survey_refugee_midline_no_baseline_identifiers.xlsx")
verification_xlsx_csv <- file.path(restricted_output_dir, "survey_refugee_midline_excel_export_verification.csv")

midline_cleaned_sheets <- list(
  cleaned_midline_survey = cleaned_midline,
  metadata = metadata
)
midline_no_baseline_sheets <- list(
  midline_no_baseline = restricted_identifier_fields,
  metadata = metadata
)

write_restricted_xlsx(midline_cleaned_sheets, midline_cleaned_xlsx)
write_restricted_xlsx(midline_no_baseline_sheets, midline_no_baseline_xlsx)

verification <- bind_rows(
  verify_xlsx(midline_cleaned_xlsx, midline_cleaned_sheets),
  verify_xlsx(midline_no_baseline_xlsx, midline_no_baseline_sheets)
)
readr::write_csv(verification, verification_xlsx_csv, na = "")
message("Wrote restricted Excel verification: ", verification_xlsx_csv)
message("Midline survey rows exported: ", nrow(cleaned_midline))
message("Midline-without-baseline households exported: ", nrow(restricted_identifier_fields))