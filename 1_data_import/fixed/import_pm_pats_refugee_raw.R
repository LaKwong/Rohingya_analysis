################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Raw-first refugee PATS+ PM2.5 import
# @Description: Imports PATS+ CSVs directly from 2_data_raw sensor folders.
################################################################################

helper_from_root <- file.path("1_data_import", "fixed", "0_import_raw_helpers.R")
if (file.exists(helper_from_root)) {
  source(helper_from_root)
} else if (!exists("raw_import_project_root", mode = "function")) {
  stop("Run from the project root or source 0_import_raw_helpers.R first.", call. = FALSE)
}

dataset_scope <- "pm25_pats_refugee_raw"

pats_roots <- data.frame(
  path = c(
    raw_import_path("2_data_raw", "ALL DATA_BASELINE_2020_220703"),
    raw_import_path("2_data_raw", "ALL DATA_MIDLINE_2021_220703"),
    raw_import_path("2_data_raw", "ALL DATA_ENDLINE_2022_220703"),
    raw_import_path("2_data_raw", "ALL PM 2.5 Outdoor data")
  ),
  source_group = c(
    "baseline_2019_2020_sensor_folder",
    "midline_2021_sensor_folder",
    "endline_2022_sensor_folder",
    "outdoor_pm25_sensor_folder"
  ),
  default_year = c(2020L, 2021L, 2022L, NA_integer_),
  source_type = c(
    "household_sensor_folder",
    "household_sensor_folder",
    "household_sensor_folder",
    "ambient_outdoor_folder"
  ),
  stringsAsFactors = FALSE
)

ambient_filename_pattern <- paste(
  c(
    "school[s]?",
    "skool[s]?",
    "mosque[s]?",
    "mosjid[s]?",
    "masjid[s]?",
    "outside",
    "out[ _-]*side",
    "outdoor[s]?",
    "out[ _-]*door[s]?"
  ),
  collapse = "|"
)

raw_pm_has_ambient_filename <- function(path) {
  grepl(ambient_filename_pattern, basename(path), ignore.case = TRUE, perl = TRUE)
}

raw_pm_file_note <- function(file_base, source_type) {
  if (grepl("QC", file_base, ignore.case = TRUE)) {
    "qc"
  } else if (grepl("school[s]?|skool[s]?", file_base, ignore.case = TRUE, perl = TRUE)) {
    "school"
  } else if (grepl("mosque[s]?|mosjid[s]?|masjid[s]?", file_base, ignore.case = TRUE, perl = TRUE)) {
    "mosque"
  } else if (grepl("outside|out[ _-]*side|outdoor[s]?|out[ _-]*door[s]?", file_base, ignore.case = TRUE, perl = TRUE)) {
    "outside"
  } else if (identical(source_type, "ambient_outdoor_folder")) {
    "ambient"
  } else {
    "normal"
  }
}

list_pats_files <- function(root, source_type) {
  if (!dir.exists(root)) return(character())
  files <- list.files(root, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (identical(source_type, "ambient_outdoor_folder")) {
    return(files)
  }
  files <- files[grepl("PATS\\+", files, ignore.case = TRUE)]
  files[!raw_pm_has_ambient_filename(files)]
}

list_ambient_named_household_files <- function(root, source_type) {
  if (!dir.exists(root) || !identical(source_type, "household_sensor_folder")) {
    return(character())
  }
  files <- list.files(root, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  files <- files[grepl("PATS\\+", files, ignore.case = TRUE)]
  files[raw_pm_has_ambient_filename(files)]
}

pats_files <- unlist(
  Map(list_pats_files, pats_roots$path, pats_roots$source_type),
  use.names = FALSE
)
pats_files <- normalizePath(pats_files, winslash = "/", mustWork = TRUE)

excluded_ambient_named_files <- unlist(
  Map(list_ambient_named_household_files, pats_roots$path, pats_roots$source_type),
  use.names = FALSE
)
excluded_ambient_named_files <- normalizePath(excluded_ambient_named_files, winslash = "/", mustWork = TRUE)

hapex_files <- unlist(
  lapply(
    pats_roots$path[pats_roots$source_type == "household_sensor_folder"],
    function(root) {
      if (!dir.exists(root)) return(character())
      files <- list.files(root, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
      files[grepl("HAPEX|Hapex", files, ignore.case = TRUE)]
    }
  ),
  use.names = FALSE
)

root_lookup <- function(path, column) {
  idx <- which(vapply(pats_roots$path, function(root) startsWith(path, normalizePath(root, winslash = "/", mustWork = TRUE)), logical(1)))
  if (!length(idx)) {
    if (identical(column, "default_year")) return(NA_integer_)
    return(NA_character_)
  }
  pats_roots[[column]][idx[1]]
}

pats_col_names <- c(
  "dateTime", "V_power", "degC_sys", "degC_air", "RH_air",
  "degC_CO", "CO_PPM", "status", "ref_sigDel", "low20avg",
  "high320avg", "motion", "CO_mV", "ignore", "iButton_Temp",
  "PM_Estimate"
)

parse_pats_file <- function(path) {
  data <- raw_import_read_csv_character(
    path,
    skip = 31,
    header = FALSE,
    col_names = pats_col_names
  )
  value_cols <- intersect(pats_col_names, names(data))
  if (length(value_cols)) {
    keep <- rowSums(!is.na(data[value_cols])) > 0
    data <- data[keep, , drop = FALSE]
  }
  if (!nrow(data)) {
    return(NULL)
  }

  file_base <- tools::file_path_sans_ext(basename(path))
  file_parts <- strsplit(file_base, "_", fixed = TRUE)[[1]]
  raw_round <- root_lookup(path, "source_group")
  source_type <- root_lookup(path, "source_type")
  is_ambient_outdoor <- identical(source_type, "ambient_outdoor_folder")
  date_info <- raw_import_collection_timestamp(data, date_cols = "dateTime")
  file_date <- raw_import_extract_date_any(file_base)
  collection_date <- date_info$date
  collection_source <- date_info$source_col
  missing_date <- is.na(collection_date) & !is.na(file_date[1])
  collection_date[missing_date] <- file_date[1]
  collection_source[missing_date] <- "raw_source_file_date"
  collection_year <- as.integer(format(collection_date, "%Y"))

  study_arm_overall <- if (grepl("/Intervention/", path, ignore.case = TRUE)) {
    "intervention"
  } else if (grepl("/Comparison/", path, ignore.case = TRUE)) {
    "comparison"
  } else {
    NA_character_
  }

  note <- raw_pm_file_note(file_base, source_type)

  data$community <- "refugee"
  data$data_type <- "pm25_pats_raw"
  data$raw_collection_round <- raw_round
  data$raw_source_type <- source_type
  data$raw_source_file <- basename(path)
  data$raw_source_path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  data$file_name <- file_base
  data$study_arm_overall <- study_arm_overall
  data$study_arm <- study_arm_overall
  data$note <- note
  data$location_type <- ifelse(is_ambient_outdoor || note %in% c("school", "mosque", "outside", "ambient"), "ambient", "indoor")
  data$include_in_indoor_final <- !is_ambient_outdoor & note == "normal"
  data$include_in_ambient_final <- is_ambient_outdoor || note %in% c("school", "mosque", "outside", "ambient")
  data$PM_monitor <- regmatches(file_base, regexpr("PM[0-9A-Za-z]+", file_base))
  if (is_ambient_outdoor) {
    data$hh_id <- NA_character_
    data$fcn_id <- NA_character_
  } else {
    data$hh_id <- if (length(file_parts) >= 5) file_parts[5] else NA_character_
    data$fcn_id <- ifelse(!is.na(data$hh_id), substr(data$hh_id, pmax(1, nchar(data$hh_id) - 5), nchar(data$hh_id)), NA_character_)
  }
  data$collection_date <- collection_date
  data$collection_year <- collection_year
  data$timepoint_source_col <- collection_source
  data$timepoint <- raw_import_timepoint_from_date(data$collection_date)

  numeric_cols <- setdiff(pats_col_names, "dateTime")
  for (col in intersect(numeric_cols, names(data))) {
    data[[col]] <- raw_import_numeric(data[[col]])
  }
  data
}

message("Importing ", length(pats_files), " PATS+/outdoor raw CSV files.")
pats_data <- raw_import_bind_rows_fill(lapply(pats_files, parse_pats_file))
pats_data <- pats_data[!duplicated(pats_data), , drop = FALSE]

output_path <- raw_import_write_rds(
  pats_data,
  file.path("4_data", "clean_final", "imported_raw", "pm25_pats_refugee_raw.rds")
)

pats_manifest <- data.frame(
  dataset_scope = dataset_scope,
  raw_source_path = pats_files,
  raw_source_file = basename(pats_files),
  raw_role = ifelse(
    vapply(pats_files, function(path) identical(root_lookup(path, "source_type"), "ambient_outdoor_folder"), logical(1)),
    "ambient_outdoor_pm25",
    "pats_plus_pm25"
  ),
  raw_collection_round = vapply(pats_files, root_lookup, character(1), column = "source_group"),
  default_collection_year = vapply(pats_files, root_lookup, integer(1), column = "default_year"),
  rows_in_raw_file = vapply(
    pats_files,
    function(path) {
      data <- raw_import_read_csv_character(path, skip = 31, header = FALSE, col_names = pats_col_names)
      value_cols <- intersect(pats_col_names, names(data))
      if (!length(value_cols)) return(0L)
      sum(rowSums(!is.na(data[value_cols])) > 0)
    },
    integer(1)
  ),
  included_in_raw_import = TRUE,
  exclusion_or_note = "",
  stringsAsFactors = FALSE
)

if (length(excluded_ambient_named_files)) {
  excluded_ambient_manifest <- data.frame(
    dataset_scope = dataset_scope,
    raw_source_path = excluded_ambient_named_files,
    raw_source_file = basename(excluded_ambient_named_files),
    raw_role = "pats_plus_pm25_ambient_filename_excluded",
    raw_collection_round = vapply(excluded_ambient_named_files, root_lookup, character(1), column = "source_group"),
    default_collection_year = vapply(excluded_ambient_named_files, root_lookup, integer(1), column = "default_year"),
    rows_in_raw_file = vapply(
      excluded_ambient_named_files,
      function(path) {
        data <- raw_import_read_csv_character(path, skip = 31, header = FALSE, col_names = pats_col_names)
        value_cols <- intersect(pats_col_names, names(data))
        if (!length(value_cols)) return(0L)
        sum(rowSums(!is.na(data[value_cols])) > 0)
      },
      integer(1)
    ),
    included_in_raw_import = FALSE,
    exclusion_or_note = "Excluded from household PATS+ import because filename indicates ambient/outdoor monitoring; dedicated outdoor PM2.5 folder is imported separately.",
    stringsAsFactors = FALSE
  )
  pats_manifest <- raw_import_bind_rows_fill(list(pats_manifest, excluded_ambient_manifest))
}

if (length(hapex_files)) {
  hapex_manifest <- data.frame(
    dataset_scope = dataset_scope,
    raw_source_path = normalizePath(hapex_files, winslash = "/", mustWork = TRUE),
    raw_source_file = basename(hapex_files),
    raw_role = "hapex_personal_pm25",
    raw_collection_round = vapply(normalizePath(hapex_files, winslash = "/", mustWork = TRUE), root_lookup, character(1), column = "source_group"),
    default_collection_year = vapply(normalizePath(hapex_files, winslash = "/", mustWork = TRUE), root_lookup, integer(1), column = "default_year"),
    rows_in_raw_file = vapply(hapex_files, raw_import_count_csv_rows, integer(1)),
    included_in_raw_import = FALSE,
    exclusion_or_note = "Excluded from final pipeline by project decision: HAPEX personal PM2.5 monitors were damaged/less reliable and are outside the requested final outputs.",
    stringsAsFactors = FALSE
  )
  pats_manifest <- raw_import_bind_rows_fill(list(pats_manifest, hapex_manifest))
  raw_import_write_csv(
    hapex_manifest,
    file.path("4_data", "clean_final", "imported_raw", "hapex_exclusion_manifest.csv")
  )
}

raw_import_update_manifest(pats_manifest, dataset_scope)

summary <- data.frame(
  dataset = "pm25_pats_refugee_raw",
  output_path = output_path,
  rows = nrow(pats_data),
  columns = ncol(pats_data),
  raw_files_imported = length(pats_files),
  stringsAsFactors = FALSE
)
raw_import_write_csv(
  summary,
  file.path("4_data", "clean_final", "imported_raw", "pm25_pats_refugee_raw_import_summary.csv")
)

message("Wrote ", output_path)


