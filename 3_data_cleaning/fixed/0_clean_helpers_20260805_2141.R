################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Helpers for separated final clean datasets
# @Description: Shared utilities for final host/refugee clean-data scripts.
################################################################################

clean_final_project_root <- function() {
  env_root <- Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = "")
  if (nzchar(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = TRUE))
  }

  current <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  repeat {
    if (
      file.exists(file.path(current, ".here")) ||
        file.exists(file.path(current, "Rohingya_analysis.Rproj"))
    ) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop(
        "Could not find the Rohingya_analysis project root. ",
        "Run from the project root or set ROHINGYA_ANALYSIS_ROOT.",
        call. = FALSE
      )
    }
    current <- parent
  }
}

clean_final_path <- function(...) {
  file.path(clean_final_project_root(), ...)
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

ensure_parent_dir <- function(path) {
  ensure_dir(dirname(path))
}

as_clean_character <- function(x) {
  x <- as.character(x)
  x[trimws(x) == ""] <- NA_character_
  x
}

timepoint_date_windows <- function() {
  data.frame(
    timepoint = c("baseline", "midline", "endline"),
    start_date = as.Date(c("2019-09-01", "2020-09-01", "2022-01-15")),
    end_date = as.Date(c("2020-04-15", "2020-12-15", "2022-08-15")),
    stringsAsFactors = FALSE
  )
}

collection_date_reference <- function() {
  data.frame(
    data_source = rep(c("household_survey", "indoor_pm25", "geocene_stove_use"), each = 3L),
    timepoint = rep(c("baseline", "midline", "endline"), times = 3L),
    start_date = as.Date(c(
      "2019-09-13", "2020-09-17", "2022-05-17",
      "2019-09-14", "2020-10-01", "2022-02-03",
      "2019-11-22", "2020-10-01", "2022-04-01"
    )),
    end_date = as.Date(c(
      "2019-11-04", "2020-11-30", "2022-07-30",
      "2019-11-17", "2020-10-26", "2022-06-07",
      "2020-03-31", "2020-10-29", "2022-06-12"
    )),
    stringsAsFactors = FALSE
  )
}

extract_date_any <- function(x) {
  if (inherits(x, "POSIXt") || inherits(x, "Date")) {
    return(as.Date(x))
  }

  x_chr <- as_clean_character(x)
  out <- as.Date(rep(NA_character_, length(x_chr)))

  formats <- c(
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
    "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M",
    "%m/%d/%y %H:%M:%S", "%m/%d/%y %H:%M",
    "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M",
    "%d/%m/%y %H:%M:%S", "%d/%m/%y %H:%M",
    "%b %d, %Y %I:%M:%S %p", "%b %d, %Y %I:%M %p",
    "%B %d, %Y %I:%M:%S %p", "%B %d, %Y %I:%M %p",
    "%b %d, %Y", "%B %d, %Y",
    "%Y-%m-%d", "%Y/%m/%d", "%Y%m%d",
    "%m/%d/%Y", "%m/%d/%y",
    "%d/%m/%Y", "%d/%m/%y"
  )

  for (fmt in formats) {
    idx <- which(is.na(out) & !is.na(x_chr))
    if (!length(idx)) break
    parsed <- suppressWarnings(as.POSIXct(x_chr[idx], format = fmt, tz = "UTC"))
    parsed_year <- as.integer(format(parsed, "%Y"))
    ok <- !is.na(parsed) & !is.na(parsed_year) & parsed_year >= 1900L & parsed_year <= 2100L
    if (any(ok)) {
      out[idx[ok]] <- as.Date(parsed[ok])
    }
  }

  idx <- which(is.na(out) & !is.na(x_chr))
  if (length(idx)) {
    abbrev_month_date <- grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", x_chr[idx])
    if (any(abbrev_month_date)) {
      values <- x_chr[idx[abbrev_month_date]]
      parsed <- suppressWarnings(as.POSIXct(values, format = "%d-%b-%Y", tz = "UTC"))
      parsed_year <- as.integer(format(parsed, "%Y"))
      bad <- is.na(parsed) | is.na(parsed_year) | parsed_year < 1900L | parsed_year > 2100L
      if (any(bad)) {
        parsed[bad] <- suppressWarnings(as.POSIXct(values[bad], format = "%d-%b-%y", tz = "UTC"))
      }
      parsed_year <- as.integer(format(parsed, "%Y"))
      ok <- !is.na(parsed) & !is.na(parsed_year) & parsed_year >= 1900L & parsed_year <= 2100L
      if (any(ok)) {
        out[idx[abbrev_month_date][ok]] <- as.Date(parsed[ok])
      }
    }
  }

  idx <- which(is.na(out) & !is.na(x_chr))
  if (length(idx)) {
    date_pos <- regexpr("20[0-9]{6}", x_chr[idx])
    has_match <- date_pos > 0
    if (any(has_match)) {
      date_match <- rep(NA_character_, length(idx))
      date_match[has_match] <- sub(".*(20[0-9]{6}).*", "\\1", x_chr[idx][has_match])
      parsed <- suppressWarnings(as.Date(date_match, format = "%Y%m%d"))
      ok <- !is.na(parsed)
      if (any(ok)) {
        out[idx[ok]] <- parsed[ok]
      }
    }
  }

  out
}

extract_year_any <- function(x) {
  parsed_date <- extract_date_any(x)
  as.integer(format(parsed_date, "%Y"))
}

timepoint_from_date <- function(date) {
  date <- extract_date_any(date)
  out <- rep(NA_character_, length(date))
  windows <- timepoint_date_windows()

  for (i in seq_len(nrow(windows))) {
    in_window <- !is.na(date) &
      date >= windows$start_date[i] &
      date <= windows$end_date[i]
    out[in_window] <- windows$timepoint[i]
  }

  out
}

timepoint_from_year <- function(year) {
  # A year alone is no longer enough to classify a study round because 2020
  # spans baseline and midline windows. Keep this conservative for older calling patterns.
  rep(NA_character_, length(year))
}

force_timepoint_from_raw_round <- function(data) {
  # Folder names are retained as provenance, but no longer override parsed timestamps.
  data
}

recode_timepoint_by_timestamp <- function(data, date_cols, dataset_name) {
  candidate_cols <- intersect(date_cols, names(data))
  collection_date <- as.Date(rep(NA_character_, nrow(data)))
  source_col <- rep(NA_character_, nrow(data))

  for (col in candidate_cols) {
    parsed_date <- extract_date_any(data[[col]])
    fill <- is.na(collection_date) & !is.na(parsed_date)
    collection_date[fill] <- parsed_date[fill]
    source_col[fill] <- col
  }

  old_timepoint <- if ("timepoint" %in% names(data)) {
    as_clean_character(data$timepoint)
  } else {
    rep(NA_character_, nrow(data))
  }
  new_timepoint <- timepoint_from_date(collection_date)
  final_timepoint <- ifelse(!is.na(new_timepoint), new_timepoint, old_timepoint)

  data$timepoint_original <- old_timepoint
  data$collection_date <- collection_date
  data$collection_year <- as.integer(format(collection_date, "%Y"))
  data$timepoint_source_col <- source_col
  data$timepoint <- final_timepoint

  summary <- data.frame(
    dataset = dataset_name,
    timepoint_original = ifelse(is.na(old_timepoint), NA_character_, old_timepoint),
    collection_date = collection_date,
    collection_year = data$collection_year,
    timepoint_final = final_timepoint,
    n = 1L,
    stringsAsFactors = FALSE
  )
  summary$collection_date <- as.character(summary$collection_date)
  summary$collection_year <- as.character(summary$collection_year)
  summary$timepoint_original[is.na(summary$timepoint_original)] <- "(missing)"
  summary$timepoint_final[is.na(summary$timepoint_final)] <- "(missing)"
  summary$collection_date[is.na(summary$collection_date)] <- "(missing)"
  summary$collection_year[is.na(summary$collection_year)] <- "(missing)"
  summary <- aggregate(
    n ~ dataset + timepoint_original + collection_date + collection_year + timepoint_final,
    data = summary,
    FUN = sum
  )
  summary$collection_date[summary$collection_date == "(missing)"] <- NA_character_
  summary$collection_year <- suppressWarnings(as.integer(summary$collection_year))

  list(data = data, summary = summary)
}

recode_timepoint_by_year <- function(data, date_cols, dataset_name) {
  recode_timepoint_by_timestamp(data, date_cols, dataset_name)
}

identifier_columns <- function(data, keep = character()) {
  patterns <- c(
    "(^|_)name($|_)",
    "phone",
    "mobile",
    "nat_id",
    "national",
    "consent_form",
    "signature",
    "gps",
    "latitude",
    "longitude",
    "^lat$",
    "^lon$",
    "photo",
    "image",
    "^SubmissionDate$",
    "^starttime$",
    "^endtime$",
    "^deviceid$",
    "^upazila_id$",
    "^union_id$",
    "^ward_id$",
    "^village_id$",
    "^house_id$",
    "^raw_source_path$",
    "^source_path$"
  )
  candidates <- unique(unlist(lapply(
    patterns,
    function(pattern) grep(pattern, names(data), ignore.case = TRUE, value = TRUE)
  )))
  setdiff(candidates, keep)
}

drop_identifier_columns <- function(data, keep = character()) {
  defaults <- c(
    "community", "data_type", "timepoint", "timepoint_original",
    "collection_date", "collection_year", "timepoint_source_col", "fcn_id", "hh_id",
    "hh_id_short", "unique_id", "PM_monitor", "study_arm",
    "study_arm_overall", "location_type"
  )
  removed <- identifier_columns(data, keep = unique(c(defaults, keep)))
  if (length(removed)) {
    data[removed] <- NULL
  }
  list(data = data, removed = removed)
}

first_nonmissing_id <- function(data, cols) {
  cols <- intersect(cols, names(data))
  out <- rep(NA_character_, nrow(data))
  if (!length(cols)) return(out)

  for (col in cols) {
    value <- as_clean_character(data[[col]])
    fill <- is.na(out) & !is.na(value)
    out[fill] <- value[fill]
  }
  out
}

make_anonymous_id <- function(x, prefix) {
  x <- as_clean_character(x)
  values <- unique(x[!is.na(x)])
  out <- rep(NA_character_, length(x))
  if (!length(values)) return(out)

  lookup <- stats::setNames(
    sprintf("%s_%05d", prefix, seq_along(values)),
    values
  )
  out[!is.na(x)] <- unname(lookup[x[!is.na(x)]])
  out
}

shareable_identifier_columns <- function(data, keep = character()) {
  patterns <- c(
    "(^|_)name($|_)",
    "^target_(child|respondent)_(current|new)$",
    "^respondent_sl$",
    "(^|_)dob($|_)",
    "date_of_birth",
    "phone",
    "mobile",
    "nat_id",
    "national",
    "UNHCR",
    "unhcr",
    "consent_form",
    "signature",
    "gps",
    "latitude",
    "longitude",
    "^lat$",
    "^lon$",
    "photo",
    "image",
    "^KEY$",
    "^PARENT_KEY$",
    "uuid",
    "^instanceID$",
    "^instanceName$",
    "^fcn_id($|_)",
    "^hh_id($|_)",
    "^hh_id_short$",
    "^unique_id$",
    "^PM_monitor$",
    "^raw_source_file$",
    "^raw_source_path$",
    "^source_path$",
    "^source_file$",
    "^SubmissionDate$",
    "^starttime$",
    "^endtime$",
    "^deviceid$",
    "^upazila_id$",
    "^union_id$",
    "^ward_id$",
    "^village_id$",
    "^house_id$"
  )

  candidates <- unique(unlist(lapply(
    patterns,
    function(pattern) grep(pattern, names(data), ignore.case = TRUE, value = TRUE)
  )))
  setdiff(candidates, keep)
}

make_shareable_dataset <- function(
    data,
    dataset_name,
    keep = character(),
    household_id_cols = c("fcn_id", "hh_id", "PARENT_KEY", "KEY", "unique_id"),
    record_id_cols = c("KEY", "uuid", "unique_id")) {
  household_id_source <- first_nonmissing_id(data, household_id_cols)
  record_id_source <- first_nonmissing_id(data, record_id_cols)

  if (any(!is.na(household_id_source))) {
    data$anon_household_id <- make_anonymous_id(
      paste(dataset_name, household_id_source, sep = "::"),
      "hh"
    )
  }

  if (any(!is.na(record_id_source))) {
    data$anon_record_id <- make_anonymous_id(
      paste(dataset_name, record_id_source, sep = "::"),
      "record"
    )
  } else {
    data$anon_record_id <- sprintf("record_%05d", seq_len(nrow(data)))
  }

  removed <- shareable_identifier_columns(data, keep = keep)
  if (length(removed)) {
    data[removed] <- NULL
  }
  data <- move_columns_first(data, c("anon_household_id", "anon_record_id"))

  list(data = data, removed = removed)
}

move_columns_first <- function(data, cols) {
  cols <- intersect(cols, names(data))
  data[c(cols, setdiff(names(data), cols))]
}

write_final_rds <- function(data, relative_path) {
  path <- clean_final_path(relative_path)
  ensure_parent_dir(path)
  saveRDS(data, path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

write_shareable_rds <- function(data, relative_path) {
  remaining_identifiers <- shareable_identifier_columns(data)
  if (length(remaining_identifiers)) {
    stop(
      "Shareable output still contains identifier-like columns: ",
      paste(remaining_identifiers, collapse = ", "),
      call. = FALSE
    )
  }
  output_file <- basename(relative_path)
  write_final_rds(data, file.path("4_data", "clean_final", "shareable", output_file))
}


make_timepoint_summary <- function(data, dataset_name) {
  old_timepoint <- if ("timepoint_original" %in% names(data)) {
    as_clean_character(data$timepoint_original)
  } else {
    rep(NA_character_, nrow(data))
  }
  collection_date <- if ("collection_date" %in% names(data)) as.Date(data$collection_date) else as.Date(rep(NA_character_, nrow(data)))
  year <- if ("collection_year" %in% names(data)) data$collection_year else as.integer(format(collection_date, "%Y"))
  final_timepoint <- if ("timepoint" %in% names(data)) as_clean_character(data$timepoint) else rep(NA_character_, nrow(data))

  summary <- data.frame(
    dataset = dataset_name,
    timepoint_original = ifelse(is.na(old_timepoint), NA_character_, old_timepoint),
    collection_date = collection_date,
    collection_year = year,
    timepoint_final = final_timepoint,
    n = 1L,
    stringsAsFactors = FALSE
  )
  summary$collection_date <- as.character(summary$collection_date)
  summary$collection_year <- as.character(summary$collection_year)
  summary$timepoint_original[is.na(summary$timepoint_original)] <- "(missing)"
  summary$timepoint_final[is.na(summary$timepoint_final)] <- "(missing)"
  summary$collection_date[is.na(summary$collection_date)] <- "(missing)"
  summary$collection_year[is.na(summary$collection_year)] <- "(missing)"
  summary <- aggregate(
    n ~ dataset + timepoint_original + collection_date + collection_year + timepoint_final,
    data = summary,
    FUN = sum
  )
  summary$collection_date[summary$collection_date == "(missing)"] <- NA_character_
  summary$collection_year <- suppressWarnings(as.integer(summary$collection_year))
  summary
}
write_timepoint_summary <- function(summary, dataset_name) {
  path <- clean_final_path(
    "4_data", "clean_final",
    paste0("timepoint_changes_", dataset_name, ".csv")
  )
  ensure_parent_dir(path)
  write.csv(summary, path, row.names = FALSE, na = "")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

compact_counts <- function(x) {
  x <- as_clean_character(x)
  x[is.na(x)] <- "(missing)"
  if (!length(x)) return("")
  counts <- sort(table(x), decreasing = TRUE)
  paste(paste(names(counts), as.integer(counts), sep = "="), collapse = "; ")
}

make_inventory_entry <- function(
    dataset_name,
    data,
    output_path,
    source_paths,
    removed_identifier_columns,
    privacy_level = "restricted_internal",
    shareable_output_path = NA_character_,
    shareable_removed_identifier_columns = character(),
    notes) {
  data.frame(
    dataset_name = dataset_name,
    output_path = normalizePath(output_path, winslash = "/", mustWork = TRUE),
    privacy_level = privacy_level,
    shareable_output_path = if (is.na(shareable_output_path)) {
      NA_character_
    } else {
      normalizePath(shareable_output_path, winslash = "/", mustWork = TRUE)
    },
    rows = nrow(data),
    columns = ncol(data),
    communities = if ("community" %in% names(data)) compact_counts(data$community) else "",
    timepoints = if ("timepoint" %in% names(data)) compact_counts(data$timepoint) else "",
    data_type = if ("data_type" %in% names(data)) compact_counts(data$data_type) else "",
    source_paths = paste(normalizePath(source_paths, winslash = "/", mustWork = TRUE), collapse = " | "),
    removed_identifier_column_count = length(removed_identifier_columns),
    removed_identifier_columns = paste(removed_identifier_columns, collapse = "; "),
    shareable_removed_identifier_column_count = length(shareable_removed_identifier_columns),
    shareable_removed_identifier_columns = paste(shareable_removed_identifier_columns, collapse = "; "),
    notes = notes,
    stringsAsFactors = FALSE
  )
}

update_inventory <- function(entry) {
  path <- clean_final_path("4_data", "clean_final", "dataset_inventory.csv")
  ensure_parent_dir(path)

  if (file.exists(path)) {
    inventory <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    inventory <- inventory[inventory$dataset_name != entry$dataset_name, , drop = FALSE]
    missing_inventory_cols <- setdiff(names(entry), names(inventory))
    missing_entry_cols <- setdiff(names(inventory), names(entry))
    for (col in missing_inventory_cols) inventory[[col]] <- NA_character_
    for (col in missing_entry_cols) entry[[col]] <- NA_character_
    inventory <- inventory[names(entry)]
    inventory <- rbind(inventory, entry)
  } else {
    inventory <- entry
  }

  inventory <- inventory[order(inventory$dataset_name), , drop = FALSE]
  write.csv(inventory, path, row.names = FALSE, na = "")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

read_rds_required <- function(relative_path) {
  path <- clean_final_path(relative_path)
  if (!file.exists(path)) {
    stop("Required input is missing: ", path, call. = FALSE)
  }
  readRDS(path)
}

read_csv_required <- function(relative_path) {
  path <- clean_final_path(relative_path)
  if (!file.exists(path)) {
    stop("Required input is missing: ", path, call. = FALSE)
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

write_cleaning_fix_log <- function() {
  docs_dir <- clean_final_path("docs")
  ensure_dir(docs_dir)
  inventory_path <- clean_final_path("4_data", "clean_final", "dataset_inventory.csv")
  archive_path <- clean_final_path("docs", "archive_manifest.csv")

  inventory_text <- if (file.exists(inventory_path)) {
    inventory <- read.csv(inventory_path, stringsAsFactors = FALSE, check.names = FALSE)
    lines <- apply(
      inventory,
      1,
      function(row) {
        paste0(
          "- `", row[["dataset_name"]], "`: ",
          row[["rows"]], " rows, ", row[["columns"]], " columns; ",
          "timepoints: ", row[["timepoints"]]
        )
      }
    )
    paste(lines, collapse = "\n")
  } else {
    "- Dataset inventory has not been generated yet."
  }

  archive_text <- if (file.exists(archive_path)) {
    archive <- read.csv(archive_path, stringsAsFactors = FALSE, check.names = FALSE)
    paste0("- Archive manifest records ", nrow(archive), " moved or reviewed files.")
  } else {
    "- Archive manifest has not been generated yet."
  }

  log_text <- c(
    "# Cleaning fixes log",
    "",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    "## Fixes implemented",
    "",
    "- Added a raw-first import layer under `1_data_import/fixed/` and a separated final-cleaning pipeline under `3_data_cleaning/fixed/` as a self-contained cleaning workflow.",
    "- Wrote raw-derived imports to `4_data/clean_final/imported_raw/` and final analysis datasets to `4_data/clean_final/` so existing non-clean_final `4_data` files remain untouched.",
    "- Reclassified survey and stove-use timepoints by parsed collection timestamps. The PM2.5 final cleaner uses PM-specific round labels: raw household sensor folders take precedence, otherwise dates are labeled as baseline 2019-08-01 to 2019-12-01, midline 2020-09-01 to 2020-11-15, and endline 2022-01-01 to 2022-06-15.",
    "- Kept refugee and host survey workflows separate, including separate raw survey imports and final survey cleaners.",
    "- Treated PATS+ PM2.5 and Geocene stove-use data as refugee-only; no host PATS+ or host Geocene outputs were created.",
    "- Excluded HAPEX personal PM2.5 from final outputs, following the project decision about damaged or less reliable monitors.",
    "- Removed direct identifier columns from final datasets while preserving stable analysis IDs needed for joins.",
    "- Uses a final host-cleaning script with corrected host household-member import logic.",
    "- Standardized PATS+ ambient labels so school files map to the comparison ambient site and mosque files map to the intervention ambient site.",
    "- Corrected the earlier final-pipeline mistake by rebuilding final outputs from raw imports rather than relying on intermediate RDS/CSV files outside clean_final.",
    "- Explicitly included the 2022 refugee survey folder `2_data_raw/survey_endline`, the 2022 host survey folder `2_data_raw/survey_endline_HOST`, and the 2022 sensor folder `2_data_raw/ALL DATA_ENDLINE_2022_220703`.",
    "- Applied the baseline household correction workbook and structured endline refugee survey review workbooks inside `3_data_cleaning/fixed/clean_survey_refugee_20260805_2141.R`; correction audit CSVs are written under `4_data/clean_final/`.",
    "- Treats root `4_data/clean_final/*.rds` files as restricted internal outputs when they retain linkage identifiers needed for cleaning, joins, or audits; shareable de-identified RDS copies are written under `4_data/clean_final/shareable/` with direct identifiers, raw form keys, UUIDs, UNHCR-like fields, household IDs, and source filenames removed.",
    "",
    "## Resulting datasets",
    "",
    inventory_text,
    "",
    "## Archive handling",
    "",
    archive_text,
    "",
    "## Notes",
    "",
    "- Existing outputs outside `4_data/clean_final/` were preserved.",
    "- `4_data_pre_cleaning_changes` was left untouched and treated as an archival snapshot unless a future script explicitly depends on it.",
    "- Exact row and column effects are recorded in `4_data/clean_final/dataset_inventory.csv` and the `timepoint_changes_*.csv` files; timepoint-change files include the parsed `collection_date` used for classification."
  )

  path <- file.path(docs_dir, "cleaning_fixes_log.md")
  writeLines(log_text, path, useBytes = TRUE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}



