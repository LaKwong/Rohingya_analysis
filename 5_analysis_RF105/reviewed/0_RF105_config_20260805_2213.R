################################################################################
# RF105 reviewed analysis configuration
#
# Purpose:
#   Shared paths and helper functions for reviewed RF105 companion analyses.
#
# Expected use:
#   Run from 5_analysis_RF105/reviewed/ or run with:
#     Sys.setenv(ROHINGYA_ANALYSIS_ROOT = "G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis")
#
# Inputs:
#   4_data/clean_final/*.rds
#
# Outputs:
#   6_figures/RF105_reviewed_YYYYMMDD/                 manuscript figures
#   7_tables/RF105_reviewed_YYYYMMDD/                  shareable tables
#   7_tables/RF105_reviewed_YYYYMMDD/release/          release checklist
#   8_restricted/RF105_reviewed_YYYYMMDD/qa/           internal ID-level QA
#
# Notes:
#   These helpers keep the existing tidyverse/script workflow while making
#   inputs, outputs, QA checks, and sensitivity analyses explicit.
################################################################################

required_packages <- c(
  "here", "tidyverse", "readxl", "janitor", "lubridate", "broom", "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install required packages before running reviewed RF105 scripts: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(lubridate)
  library(broom)
  library(scales)
  library(here)
})

`%notin%` <- Negate(`%in%`)

date_stamp <- format(Sys.Date(), "%Y%m%d")

get_config_dir <- function() {
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

find_project_root <- function() {
  env_root <- Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = FALSE))
  }

  config_dir <- get_config_dir()
  candidate_roots <- c(
    normalizePath(file.path(config_dir, "..", ".."),
                  winslash = "/", mustWork = FALSE),
    normalizePath(here::here(), winslash = "/", mustWork = FALSE)
  )

  for (candidate_root in unique(candidate_roots)) {
    if (dir.exists(file.path(candidate_root, "4_data", "clean_final"))) {
      return(candidate_root)
    }
  }

  normalizePath(here::here(), winslash = "/", mustWork = FALSE)
}

project_root <- find_project_root()

clean_data_dir_override <- Sys.getenv("RF105_CLEAN_DATA_DIR", unset = "")
dir_clean_final <- if (nzchar(clean_data_dir_override)) {
  normalizePath(clean_data_dir_override, winslash = "/", mustWork = FALSE)
} else {
  file.path(project_root, "4_data", "clean_final")
}
dir_figures_reviewed <- file.path(
  project_root, "6_figures", paste0("RF105_reviewed_", date_stamp)
)
dir_tables_reviewed <- file.path(
  project_root, "7_tables", paste0("RF105_reviewed_", date_stamp)
)
dir_tables_qa <- file.path(dir_tables_reviewed, "qa")
dir_tables_release <- file.path(dir_tables_reviewed, "release")
dir_restricted_reviewed <- file.path(
  project_root, "8_restricted", paste0("RF105_reviewed_", date_stamp)
)
dir_restricted_qa <- file.path(dir_restricted_reviewed, "qa")
dir.create(dir_figures_reviewed, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tables_reviewed, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tables_qa, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tables_release, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_restricted_qa, recursive = TRUE, showWarnings = FALSE)
file_survey_refugee_household <- file.path(
  dir_clean_final, "survey_refugee_household.rds"
)
file_pm25_indoor <- file.path(
  dir_clean_final, "pm25_pats_refugee_indoor.rds"
)
file_pm25_ambient <- file.path(
  dir_clean_final, "pm25_pats_refugee_ambient.rds"
)
file_stove_daily <- file.path(
  dir_clean_final, "stove_use_geocene_refugee_daily.rds"
)

file_stove_monitor_days <- file.path(
  dir_clean_final, "stove_use_geocene_refugee_monitor_days.rds"
)
timepoint_levels <- c("baseline", "midline", "endline")
arm_levels <- c("comparison", "intervention")

as_ordered_timepoint <- function(x, extra_levels = character()) {
  x_clean <- str_squish(str_to_lower(as.character(x)))
  factor(
    x_clean,
    levels = c(timepoint_levels, extra_levels),
    ordered = TRUE
  )
}

timepoint_collection_years <- c(
  baseline = 2019L,
  midline = 2020L,
  endline = 2022L
)

timepoint_collection_year <- function(x) {
  tp <- as.character(as_ordered_timepoint(x))
  years <- unname(timepoint_collection_years[tp])
  as.integer(years)
}

timepoint_label_with_year <- function(x, title_case = TRUE) {
  tp <- as.character(as_ordered_timepoint(x))
  years <- timepoint_collection_year(tp)
  label_tp <- if (isTRUE(title_case)) str_to_title(tp) else tp
  ifelse(
    is.na(tp),
    NA_character_,
    ifelse(is.na(years), label_tp, paste0(label_tp, " (", years, ")"))
  )
}

timepoint_label_with_year_levels <- timepoint_label_with_year(timepoint_levels)

exchange_bdt_per_usd <- c(
  baseline = 84.88,
  midline = 84.74,
  endline = 93.45
)

restricted_qa_fields <- c(
  "fcn_id", "hh_id", "UNHCR_id", "UNHCR_card", "uuid", "KEY", "PARENT_KEY",
  "camp_id", "block_id", "subblock_id", "collection_date",
  "collection_dates_all_raw", "start_date", "end_date", "date", "datetime",
  "submission_time", "raw_source_file", "raw_source_files_all_raw",
  "raw_collection_round", "raw_collection_rounds_all_raw",
  "raw_survey_version", "raw_survey_versions_all_raw",
  "raw_source_path", "source_path", "source_file", "file_name"
)

restricted_field_patterns <- c(
  "^name($|_)", "^name_(respondent|hh_head|mahji)$", "target_child_name",
  "^fcn_id($|_)",
  "^hh_id($|_)",
  "UNHCR", "unhcr", "uuid", "^KEY$", "^PARENT_KEY$",
  "^camp_id$", "^block_id$", "^subblock_id$",
  "^collection_date($|_)", "^start_date$", "^end_date$",
  "^date$", "^datetime$", "submission_time",
  "raw_source", "^source_file$", "file_name", "source_path"
)

restricted_fields_present <- function(x) {
  direct <- intersect(names(x), restricted_qa_fields)
  patterned <- unique(unlist(lapply(
    restricted_field_patterns,
    function(pattern) grep(pattern, names(x), ignore.case = TRUE, value = TRUE)
  )))
  unique(c(direct, patterned))
}

restricted_output_dir <- function(subfolder = NULL) {
  if (is.null(subfolder)) {
    file.path(dir_restricted_reviewed, "identified_tables")
  } else if (identical(subfolder, "qa")) {
    dir_restricted_qa
  } else {
    file.path(dir_restricted_reviewed, subfolder)
  }
}

manifest_subfolder_value <- function(subfolder = NULL) {
  if (is.null(subfolder)) "." else subfolder
}

write_restricted_output_manifest <- function(filename, public_path, restricted_path,
                                             restricted_fields, subfolder = NULL) {
  manifest_file <- file.path(dir_tables_release, "table_release_restricted_output_manifest.csv")
  new_row <- tibble(
    filename = filename,
    public_table_path = normalizePath(public_path, winslash = "/", mustWork = FALSE),
    restricted_table_path = normalizePath(restricted_path, winslash = "/", mustWork = FALSE),
    requested_subfolder = manifest_subfolder_value(subfolder),
    restricted_fields = paste(restricted_fields, collapse = "; "),
    release_status = "restricted_internal_only",
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )

  if (file.exists(manifest_file)) {
    existing <- suppressMessages(readr::read_csv(manifest_file, show_col_types = FALSE))
    existing <- existing %>%
      filter(.data$filename != new_row$filename | .data$requested_subfolder != new_row$requested_subfolder)
    new_row <- bind_rows(existing, new_row)
  }

  readr::write_csv(new_row, manifest_file, na = "")
  invisible(manifest_file)
}

remove_restricted_output_manifest_entry <- function(filename, subfolder = NULL) {
  manifest_file <- file.path(dir_tables_release, "table_release_restricted_output_manifest.csv")
  if (!file.exists(manifest_file)) return(invisible(FALSE))

  existing <- suppressMessages(readr::read_csv(manifest_file, show_col_types = FALSE))
  if (!nrow(existing)) return(invisible(FALSE))

  target_subfolder <- manifest_subfolder_value(subfolder)
  existing_subfolder <- existing$requested_subfolder
  same_subfolder <- (is.na(existing_subfolder) & is.na(target_subfolder)) |
    (!is.na(existing_subfolder) & !is.na(target_subfolder) & existing_subfolder == target_subfolder)
  keep <- !(existing$filename == filename & same_subfolder)
  updated <- existing[keep %in% TRUE, , drop = FALSE]
  readr::write_csv(updated, manifest_file, na = "")
  invisible(nrow(updated) < nrow(existing))
}

write_reviewed_csv <- function(x, filename, subfolder = NULL) {
  public_dir <- if (is.null(subfolder)) {
    dir_tables_reviewed
  } else {
    file.path(dir_tables_reviewed, subfolder)
  }
  public_file <- file.path(public_dir, filename)
  restricted_fields <- restricted_fields_present(x)

  if (length(restricted_fields) > 0) {
    out_dir <- restricted_output_dir(subfolder)
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_file <- file.path(out_dir, filename)
    readr::write_csv(x, out_file, na = "")
    write_restricted_output_manifest(
      filename = filename,
      public_path = public_file,
      restricted_path = out_file,
      restricted_fields = restricted_fields,
      subfolder = subfolder
    )
    message(
      "Restricted fields detected (", paste(restricted_fields, collapse = ", "),
      "); wrote restricted table only: ", out_file
    )
    return(invisible(out_file))
  }

  dir.create(public_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, public_file, na = "")
  remove_restricted_output_manifest_entry(filename, subfolder)
  message("Wrote table: ", public_file)
  invisible(public_file)
}

quarantine_restricted_public_csvs <- function() {
  if (!dir.exists(dir_tables_reviewed)) return(invisible(tibble()))
  csv_files <- list.files(dir_tables_reviewed, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  csv_files <- csv_files[!grepl("/release/", normalizePath(csv_files, winslash = "/", mustWork = FALSE))]
  if (!length(csv_files)) return(invisible(tibble()))

  moved <- lapply(csv_files, function(csv_file) {
    header <- tryCatch(names(readr::read_csv(csv_file, n_max = 0, show_col_types = FALSE)), error = function(e) character())
    restricted_fields <- restricted_fields_present(setNames(as.list(rep(NA, length(header))), header))
    if (!length(restricted_fields)) return(NULL)

    rel <- substr(normalizePath(csv_file, winslash = "/", mustWork = FALSE),
                  nchar(normalizePath(dir_tables_reviewed, winslash = "/", mustWork = FALSE)) + 2L,
                  nchar(normalizePath(csv_file, winslash = "/", mustWork = FALSE)))
    dest <- file.path(dir_restricted_reviewed, "quarantined_from_7_tables", rel)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(dest)) file.remove(dest)
    file.rename(csv_file, dest)
    write_restricted_output_manifest(
      filename = basename(csv_file),
      public_path = csv_file,
      restricted_path = dest,
      restricted_fields = restricted_fields,
      subfolder = dirname(rel)
    )
    tibble(public_table_path = csv_file, restricted_table_path = dest,
           restricted_fields = paste(restricted_fields, collapse = "; "))
  })

  moved <- bind_rows(Filter(Negate(is.null), moved))
  if (nrow(moved) > 0) {
    message("Moved ", nrow(moved), " restricted CSV(s) out of public 7_tables outputs.")
  }
  invisible(moved)
}

quarantine_restricted_public_csvs()

resolve_reviewed_or_restricted_csv <- function(filename, restricted_subfolder = "identified_tables") {
  candidate_paths <- c(
    file.path(dir_tables_reviewed, filename),
    file.path(dir_restricted_reviewed, restricted_subfolder, filename),
    file.path(dir_restricted_qa, filename)
  )
  candidate_paths <- candidate_paths[file.exists(candidate_paths)]
  if (length(candidate_paths) == 0) {
    stop(
      "Required reviewed result is missing from public and restricted output folders: ", filename,
      call. = FALSE
    )
  }
  candidate_paths[[1]]
}

write_restricted_qa_csv <- function(x, filename, subfolder = NULL,
                                    reason = NULL) {
  out_dir <- if (is.null(subfolder)) {
    dir_restricted_qa
  } else {
    file.path(dir_restricted_qa, subfolder)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, filename)
  restricted_fields <- restricted_fields_present(x)
  if (length(restricted_fields) == 0) {
    warning(
      "Restricted QA output has no known restricted identifier/date/source fields: ",
      out_file,
      call. = FALSE
    )
  }
  readr::write_csv(x, out_file, na = "")
  message("Wrote restricted QA file: ", out_file)

  if (!is.null(reason)) {
    metadata <- tibble(
      output_file = normalizePath(out_file, winslash = "/", mustWork = FALSE),
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      restricted_fields = paste(restricted_fields, collapse = "; "),
      reason = reason,
      release_status = "restricted_internal_only"
    )
    readr::write_csv(metadata, paste0(out_file, ".metadata.csv"), na = "")
  }

  invisible(out_file)
}

make_rf105_release_checklist <- function(extra_items = NULL) {
  checklist <- tibble(
    item = c(
      "Confirm shareable outputs exclude direct household identifiers",
      "Confirm ID-level QA is stored only under 8_restricted",
      "Confirm restricted QA folder is excluded from git/public release",
      "Confirm tables and figures can be regenerated from reviewed scripts",
      "Confirm no raw, restricted, or legacy-review paths are included in release"
    ),
    status = "required_before_public_release",
    location = c(
      dir_tables_reviewed,
      dir_restricted_qa,
      file.path(project_root, ".gitignore"),
      file.path(project_root, "5_analysis_RF105", "reviewed"),
      project_root
    ),
    notes = c(
      "Search release files for fcn_id, hh_id, uuid, raw source file names, and date-rich QA listings before sharing.",
      "This folder is for internal reproducibility checks only and should not be copied into public release bundles.",
      "The project .gitignore should include 8_restricted/**.",
      "Rerun the reviewed workflow from import through analysis before final release.",
      "Release bundles should include reviewed/final code, shareable tables/figures, and de-identified data only."
    )
  )

  if (!is.null(extra_items)) {
    checklist <- bind_rows(checklist, extra_items)
  }

  checklist
}

write_rf105_release_checklist <- function(extra_items = NULL,
                                          filename = "table_release_checklist.csv") {
  write_reviewed_csv(
    make_rf105_release_checklist(extra_items),
    filename,
    subfolder = "release"
  )
}
save_reviewed_plot <- function(plot, filename, width = 8, height = 5,
                               units = "in", dpi = 300, bg = "white") {
  out_file <- file.path(dir_figures_reviewed, filename)
  ggplot2::ggsave(
    filename = out_file,
    plot = plot,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    bg = bg
  )
  message("Wrote figure: ", out_file)
  invisible(out_file)
}

clean_timepoint_arm <- function(df) {
  df %>%
    mutate(
      timepoint = str_squish(str_to_lower(as.character(timepoint))),
      study_arm_overall = str_squish(str_to_lower(as.character(study_arm_overall))),
      timepoint = as_ordered_timepoint(timepoint),
      study_arm_overall = factor(study_arm_overall, levels = arm_levels)
    )
}

make_yn <- function(x) {
  x_chr <- str_squish(str_to_lower(as.character(x)))
  suppressWarnings(x_num <- as.numeric(x_chr))

  case_when(
    is.na(x) ~ NA_integer_,
    !is.na(x_num) & x_num %in% c(77, 88, 99) ~ NA_integer_,
    !is.na(x_num) ~ as.integer(x_num > 0),
    x_chr %in% c("yes", "y", "true", "present") ~ 1L,
    x_chr %in% c("no", "n", "false", "absent") ~ 0L,
    TRUE ~ NA_integer_
  )
}


# Severe asthma is defined as current child wheeze plus disturbed speech during
# wheeze. Keep true missing severity responses distinct from structurally skipped
# severity questions among children with no wheeze, then create an explicit
# skip-as-no sensitivity variable.
derive_child_severe_asthma_vars <- function(df,
                                            wheeze_var = "target_child_wheezing_yn",
                                            speech_var = "target_child_distrubed_speech_yn") {
  if (!all(c(wheeze_var, speech_var) %in% names(df))) {
    return(df)
  }

  wheeze <- as.integer(df[[wheeze_var]])
  speech <- as.integer(df[[speech_var]])
  speech_skip_as_no <- dplyr::case_when(
    wheeze == 0 & is.na(speech) ~ 0L,
    TRUE ~ speech
  )

  severe_na_preserving <- dplyr::case_when(
    wheeze == 1 & speech == 1 ~ 1L,
    wheeze == 1 & speech == 0 ~ 0L,
    wheeze == 0 & !is.na(speech) ~ 0L,
    TRUE ~ NA_integer_
  )

  severe_skip_as_no <- dplyr::case_when(
    wheeze == 1 & speech_skip_as_no == 1 ~ 1L,
    wheeze == 1 & speech_skip_as_no == 0 ~ 0L,
    wheeze == 0 ~ 0L,
    TRUE ~ NA_integer_
  )

  df$target_child_disturbed_speech_missing_type <- dplyr::case_when(
    !is.na(speech) ~ "observed_disturbed_speech",
    wheeze == 0 ~ "structural_skip_no_wheeze",
    wheeze == 1 ~ "true_missing_among_wheeze",
    is.na(wheeze) ~ "missing_wheeze_or_unknown",
    TRUE ~ "unclassified"
  )
  df$target_child_distrubed_speech_yn_na_preserving <- speech
  df$target_child_disturbed_speech_yn_na_preserving <- speech
  df$target_child_distrubed_speech_yn_skip_as_no <- speech_skip_as_no
  df$target_child_disturbed_speech_yn_skip_as_no <- speech_skip_as_no
  df$target_child_severe_asthma_na_preserving <- severe_na_preserving
  df$target_child_severe_asthma_skip_as_no <- severe_skip_as_no
  df$target_child_severe_asthma <- severe_na_preserving

  df
}
income_30_component_vars <- c(
  "income_cash_ngo",
  "income_own_business",
  "income_wage_labor",
  "income_skill_labor",
  "income_selling_wood",
  "income_abroad",
  "income_humanitarian_asst",
  "income_handicrafts_tailoring",
  "income_farming"
)

make_nonnegative_amount <- function(x) {
  suppressWarnings(out <- as.numeric(x))
  out[out %in% c(77, 88, 99)] <- NA_real_
  out[out < 0] <- NA_real_
  out
}

sum_income_30_components <- function(df) {
  present_vars <- income_30_component_vars[income_30_component_vars %in% names(df)]
  if (length(present_vars) == 0) {
    return(rep(NA_real_, nrow(df)))
  }

  income_components <- as.data.frame(lapply(present_vars, function(var) {
    make_nonnegative_amount(df[[var]])
  }))

  n_nonmissing_components <- rowSums(!is.na(income_components))
  total <- rowSums(income_components, na.rm = TRUE)
  total[n_nonmissing_components == 0] <- NA_real_
  total
}

fill_total_income_30_from_components <- function(df) {
  component_total <- sum_income_30_components(df)
  reported_total <- if ("total_income_30" %in% names(df)) {
    make_nonnegative_amount(df$total_income_30)
  } else {
    rep(NA_real_, nrow(df))
  }

  df$total_income_30_reported <- reported_total
  df$total_income_30_component_sum <- component_total
  df$total_income_30 <- dplyr::coalesce(reported_total, component_total)
  df
}

add_rf105_aliases <- function(df) {
  # Existing RF105 scripts used these older variable names. The clean_final
  # survey file uses shorter names, so create aliases rather than changing
  # every downstream analysis line.
  if ("fuel_30_scraps" %in% names(df) &&
      "fuel_30_gather_scraps" %notin% names(df)) {
    df$fuel_30_gather_scraps <- df$fuel_30_scraps
  }

  if ("fuel_ever_scraps" %in% names(df) &&
      "fuel_ever_gather_scraps" %notin% names(df)) {
    df$fuel_ever_gather_scraps <- df$fuel_ever_scraps
  }
  any_yn_from <- function(vars) {
    vars <- vars[vars %in% names(df)]
    if (length(vars) == 0) {
      return(rep(NA_integer_, nrow(df)))
    }
    mat <- do.call(cbind, lapply(vars, function(var) make_yn(df[[var]])))
    out <- as.integer(rowSums(mat == 1, na.rm = TRUE) > 0)
    out[rowSums(!is.na(mat)) == 0] <- NA_integer_
    out
  }

  if ("fuel_30_lpg" %notin% names(df)) {
    df$fuel_30_lpg <- any_yn_from(c("fuel_30_receive_lpg", "fuel_30_buy_lpg"))
  }

  if ("fuel_30_wood" %notin% names(df)) {
    df$fuel_30_wood <- any_yn_from(c(
      "fuel_30_collect_wood", "fuel_30_buy_wood", "fuel_30_receive_wood"
    ))
  }

  if ("fuel_30_charcoal" %notin% names(df)) {
    df$fuel_30_charcoal <- any_yn_from(c(
      "fuel_30_receive_crh", "fuel_30_buy_crh"
    ))
  }

  if ("fuel_use_non_lpg_ever" %in% names(df) &&
      "fuel_use_non_lpg" %notin% names(df)) {
    df$fuel_use_non_lpg <- df$fuel_use_non_lpg_ever
  }

  if ("fuel_cant_afford_2wk" %in% names(df) &&
      "fuel_cant_afford" %notin% names(df)) {
    df$fuel_cant_afford <- df$fuel_cant_afford_2wk
  }

  if ("gather_scraps_dead" %in% names(df) &&
      "gather_wood_dead" %notin% names(df)) {
    df$gather_wood_dead <- df$gather_scraps_dead
  }

  if ("gather_wood_dead" %in% names(df) &&
      "gather_scraps_dead" %notin% names(df)) {
    df$gather_scraps_dead <- df$gather_wood_dead
  }

  if ("cook_to_sell" %in% names(df) &&
      "cook_to_sell_percent" %notin% names(df)) {
    df$cook_to_sell_percent <- df$cook_to_sell
  }

  if ("cook_to_sell_percent" %in% names(df) &&
      "cook_to_sell" %notin% names(df)) {
    df$cook_to_sell <- df$cook_to_sell_percent
  }

  if ("cook_sell_yesterday" %in% names(df) &&
      "cook_sell_days_week" %notin% names(df)) {
    df$cook_sell_days_week <- df$cook_sell_yesterday
  }

  if ("plastic_cook" %notin% names(df) &&
      any(c("burn_plastic_types", "burn_plastic_reason") %in% names(df))) {
    plastic_fields <- c("burn_plastic_types", "burn_plastic_reason")
    plastic_fields <- plastic_fields[plastic_fields %in% names(df)]
    plastic_mat <- do.call(cbind, lapply(plastic_fields, function(var) {
      value <- str_squish(as.character(df[[var]]))
      !is.na(df[[var]]) & value != ""
    }))
    df$plastic_cook <- as.integer(rowSums(plastic_mat, na.rm = TRUE) > 0)
  }

  df <- fill_total_income_30_from_components(df)

  if ("total_income_30" %in% names(df) && "income" %notin% names(df)) {
    df$income <- df$total_income_30
  }

  if ("debt_total" %in% names(df) && "debt_yn" %notin% names(df)) {
    df$debt_yn <- make_yn(df$debt_total)
  } else if ("debt" %in% names(df) && "debt_yn" %notin% names(df)) {
    df$debt_yn <- make_yn(df$debt)
  }

  asset_roots <- c(
    "electric_fan", "smartphone", "mobile_phone", "mattress", "blanket",
    "mosquito_net", "umbrella", "chair_bench", "table", "shovel",
    "sickle", "weaving_tool", "chicken_duck_pigeon"
  )

  for (root in asset_roots) {
    yn_name <- paste0(root, "_yn")
    if (root %in% names(df) && yn_name %notin% names(df)) {
      df[[yn_name]] <- make_yn(df[[root]])
    }
  }

  df
}

flag_missing_vars <- function(df, vars, context) {
  tibble(
    context = context,
    variable = vars,
    available = vars %in% names(df)
  ) %>%
    mutate(
      status = if_else(available, "available", "missing_from_clean_final")
    )
}

make_analysis_population <- function(df, id_var = "fcn_id") {
  stopifnot(id_var %in% names(df))

  df_clean <- df %>%
    clean_timepoint_arm() %>%
    filter(
      !is.na(.data[[id_var]]),
      .data[[id_var]] != "",
      !is.na(timepoint),
      timepoint %in% timepoint_levels
    )

  duplicate_records <- df_clean %>%
    add_count(.data[[id_var]], timepoint, name = "n_records_for_id_timepoint") %>%
    filter(n_records_for_id_timepoint > 1) %>%
    select(any_of(c(
      id_var, "hh_id", "timepoint", "study_arm_overall", "camp_id",
      "block_id", "subblock_id", "start_date", "end_date",
      "submission_time", "raw_source_file", "n_records_for_id_timepoint"
    ))) %>%
    arrange(.data[[id_var]], timepoint)

  arrange_cols <- intersect(
    c(id_var, "timepoint", "start_date", "end_date", "submission_time"),
    names(df_clean)
  )

  # Keep the first record after a deterministic sort. This mirrors the old
  # "one household per timepoint" analysis expectation while preserving a QA
  # file with all records that were collapsed.
  df_dedup <- df_clean %>%
    arrange(across(all_of(arrange_cols))) %>%
    group_by(.data[[id_var]], timepoint) %>%
    slice(1) %>%
    ungroup()

  complete_ids <- df_dedup %>%
    distinct(.data[[id_var]], timepoint) %>%
    count(.data[[id_var]], name = "n_timepoints") %>%
    filter(n_timepoints == length(timepoint_levels)) %>%
    pull(.data[[id_var]])

  df_complete <- df_dedup %>%
    filter(.data[[id_var]] %in% complete_ids)

  baseline_followup_status <- df_dedup %>%
    filter(timepoint == "baseline") %>%
    mutate(
      three_survey_participant = .data[[id_var]] %in% complete_ids,
      attrition_status = if_else(
        three_survey_participant,
        "participated_in_all_3_surveys",
        "lost_before_endline"
      )
    )

  sample_counts <- df_dedup %>%
    mutate(three_survey_participant = .data[[id_var]] %in% complete_ids) %>%
    count(timepoint, study_arm_overall, three_survey_participant, name = "n") %>%
    arrange(timepoint, study_arm_overall, desc(three_survey_participant))

  list(
    all_deduplicated = df_dedup,
    complete_3_survey = df_complete,
    baseline_followup_status = baseline_followup_status,
    duplicate_records = duplicate_records,
    sample_counts = sample_counts
  )
}

format_mean_sd <- function(x, digits = 1) {
  if (all(is.na(x))) {
    return("")
  }
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f)"),
    mean(x, na.rm = TRUE),
    sd(x, na.rm = TRUE)
  )
}

format_n_pct <- function(x, digits = 1) {
  denom <- sum(!is.na(x))
  n_yes <- sum(x == 1, na.rm = TRUE)
  if (denom == 0) {
    return("")
  }
  sprintf(paste0("%d (%.", digits, "f%%)"), n_yes, 100 * n_yes / denom)
}

format_categories <- function(x, digits = 1) {
  x <- as.character(x)
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    return("")
  }
  tab <- sort(table(x), decreasing = TRUE)
  paste(
    sprintf(
      paste0("%s: %d (%.", digits, "f%%)"),
      names(tab), as.integer(tab), 100 * as.integer(tab) / sum(tab)
    ),
    collapse = "; "
  )
}

is_binary_var <- function(x) {
  x_nonmissing <- x[!is.na(x)]
  if (length(x_nonmissing) == 0) {
    return(FALSE)
  }
  all(unique(x_nonmissing) %in% c(0, 1, FALSE, TRUE))
}

format_p <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

calc_group_p <- function(df, var, group_var) {
  dat <- df %>%
    filter(!is.na(.data[[var]]), !is.na(.data[[group_var]]))

  if (n_distinct(dat[[group_var]]) < 2 || nrow(dat) == 0) {
    return(NA_real_)
  }

  x <- dat[[var]]
  g <- as.factor(dat[[group_var]])

  out <- tryCatch({
    if (is.numeric(x) && !is_binary_var(x)) {
      if (nlevels(g) == 2) {
        t.test(x ~ g)$p.value
      } else {
        summary(aov(x ~ g))[[1]][["Pr(>F)"]][1]
      }
    } else {
      suppressWarnings(chisq.test(table(x, g))$p.value)
    }
  }, error = function(e) NA_real_)

  as.numeric(out)
}

make_characteristics_table <- function(df, vars, labels = NULL, group_var = NULL,
                                       binary_vars = character()) {
  vars <- vars[vars %in% names(df)]
  labels <- labels[vars]
  labels[is.na(labels)] <- vars[is.na(labels)]

  summarise_one_group <- function(dat, group_name) {
    map_dfr(vars, function(var) {
      x <- dat[[var]]
      is_binary <- var %in% binary_vars || is_binary_var(x)

      value <- if (is.numeric(x) && !is_binary) {
        format_mean_sd(x)
      } else if (is_binary) {
        format_n_pct(as.integer(x))
      } else {
        format_categories(x)
      }

      tibble(
        variable = var,
        characteristic = labels[[var]],
        group = group_name,
        value = value
      )
    })
  }

  if (is.null(group_var)) {
    summarise_one_group(df, "overall") %>%
      select(variable, characteristic, overall = value)
  } else {
    groups <- split(df, df[[group_var]], drop = TRUE)
    group_table <- imap_dfr(groups, summarise_one_group) %>%
      pivot_wider(names_from = group, values_from = value)

    p_table <- tibble(
      variable = vars,
      p_value = map_dbl(vars, ~ calc_group_p(df, .x, group_var))
    ) %>%
      mutate(p_value = format_p(p_value))

    group_table %>%
      left_join(p_table, by = "variable") %>%
      relocate(p_value, .after = last_col())
  }
}

