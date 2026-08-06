################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Helpers for raw-first final imports
# @Description: Shared utilities for importing raw data into auditable RDS files.
################################################################################

raw_import_project_root <- function() {
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

raw_import_path <- function(...) {
  file.path(raw_import_project_root(), ...)
}

raw_import_ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

raw_import_ensure_parent_dir <- function(path) {
  raw_import_ensure_dir(dirname(path))
}

raw_import_clean_character <- function(x) {
  x <- as.character(x)
  x[trimws(x) == ""] <- NA_character_
  x
}

raw_import_timepoint_date_windows <- function() {
  data.frame(
    timepoint = c("baseline", "midline", "endline"),
    start_date = as.Date(c("2019-09-01", "2020-09-01", "2022-01-15")),
    end_date = as.Date(c("2020-04-15", "2020-12-15", "2022-08-15")),
    stringsAsFactors = FALSE
  )
}

raw_import_collection_date_reference <- function() {
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

raw_import_extract_date_any <- function(x) {
  if (inherits(x, "POSIXt") || inherits(x, "Date")) {
    return(as.Date(x))
  }

  x_chr <- raw_import_clean_character(x)
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

raw_import_extract_year_any <- function(x) {
  parsed_date <- raw_import_extract_date_any(x)
  as.integer(format(parsed_date, "%Y"))
}

raw_import_timepoint_from_date <- function(date) {
  date <- raw_import_extract_date_any(date)
  out <- rep(NA_character_, length(date))
  windows <- raw_import_timepoint_date_windows()

  for (i in seq_len(nrow(windows))) {
    in_window <- !is.na(date) &
      date >= windows$start_date[i] &
      date <= windows$end_date[i]
    out[in_window] <- windows$timepoint[i]
  }

  out
}

raw_import_timepoint_from_year <- function(year) {
  # A year alone is no longer enough to classify a study round because 2020
  # spans baseline and midline windows. Keep this conservative for legacy calls.
  rep(NA_character_, length(year))
}

raw_import_collection_timestamp <- function(data, date_cols, default_date = as.Date(NA)) {
  date <- as.Date(rep(NA_character_, nrow(data)))
  source_col <- rep(NA_character_, nrow(data))
  for (col in intersect(date_cols, names(data))) {
    parsed <- raw_import_extract_date_any(data[[col]])
    fill <- is.na(date) & !is.na(parsed)
    date[fill] <- parsed[fill]
    source_col[fill] <- col
  }
  if (!is.na(default_date)) {
    fill <- is.na(date)
    date[fill] <- as.Date(default_date)
    source_col[fill] <- "raw_default_date"
  }
  list(
    date = date,
    year = as.integer(format(date, "%Y")),
    timepoint = raw_import_timepoint_from_date(date),
    source_col = source_col
  )
}

raw_import_collection_year <- function(data, date_cols, default_year = NA_integer_) {
  # Compatibility wrapper. Timepoints should use parsed dates, not year-only rules.
  info <- raw_import_collection_timestamp(data, date_cols = date_cols)
  if (!is.na(default_year)) {
    fill <- is.na(info$year)
    info$year[fill] <- as.integer(default_year)
    info$source_col[fill] <- "raw_folder_default_year"
  }
  list(year = info$year, date = info$date, timepoint = info$timepoint, source_col = info$source_col)
}

raw_import_read_csv_character <- function(path, skip = 0, header = TRUE, col_names = NULL) {
  if (!file.exists(path)) {
    stop("Missing raw input file: ", path, call. = FALSE)
  }

  data <- tryCatch(
    read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character",
      na.strings = c("", "NA"),
      skip = skip,
      header = header,
      fill = TRUE,
      fileEncoding = "UTF-8-BOM"
    ),
    error = function(e) {
      read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        colClasses = "character",
        na.strings = c("", "NA"),
        skip = skip,
        header = header,
        fill = TRUE
      )
    }
  )

  if (!is.null(col_names)) {
    if (ncol(data) < length(col_names)) {
      for (i in seq_len(length(col_names) - ncol(data))) {
        data[[paste0("missing_column_", i)]] <- NA_character_
      }
    }
    names(data)[seq_along(col_names)] <- col_names
  }
  nm <- names(data)
  blank_names <- is.na(nm) | !nzchar(nm)
  if (any(blank_names)) {
    nm[blank_names] <- paste0("unnamed_col_", which(blank_names))
  }
  names(data) <- make.unique(nm, sep = "__dup")
  data
}

raw_import_bind_rows_fill <- function(datasets) {
  datasets <- Filter(Negate(is.null), datasets)
  if (!length(datasets)) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  all_names <- unique(unlist(lapply(datasets, names), use.names = FALSE))
  all_names <- all_names[!is.na(all_names) & nzchar(all_names)]
  datasets <- lapply(
    datasets,
    function(data) {
      missing <- setdiff(all_names, names(data))
      for (col in missing) {
        data[[col]] <- NA
      }
      data[all_names]
    }
  )
  do.call(rbind, datasets)
}

raw_import_detect_survey_role <- function(path) {
  file <- basename(path)
  if (grepl("duplicates", file, ignore.case = TRUE)) {
    "duplicate_review"
  } else if (grepl("-hh_members\\.csv$", file, ignore.case = TRUE)) {
    "hh_members"
  } else if (grepl("-symptoms\\.csv$", file, ignore.case = TRUE)) {
    "symptoms"
  } else if (grepl("-location\\.csv$", file, ignore.case = TRUE)) {
    "location"
  } else {
    "household"
  }
}

raw_import_survey_version <- function(path) {
  file <- basename(path)
  match <- regexpr("v[0-9]+", file, ignore.case = TRUE)
  if (match[1] < 0) return(NA_character_)
  version <- regmatches(file, match)
  ifelse(nzchar(version), version, NA_character_)
}
raw_import_study_arm_overall <- function(data, community) {
  if (identical(community, "host")) {
    return(rep("host", nrow(data)))
  }

  arm <- if ("study_arm" %in% names(data)) tolower(trimws(as.character(data$study_arm))) else rep(NA_character_, nrow(data))
  camp <- if ("camp_id" %in% names(data)) toupper(trimws(as.character(data$camp_id))) else rep(NA_character_, nrow(data))
  out <- rep(NA_character_, nrow(data))

  out[arm %in% c("1", "3", "7", "intervention", "post-intervention", "pre-intervention")] <- "intervention"
  out[arm %in% c("2", "6", "8", "comparison", "intervention follow-up")] <- "comparison"
  out[arm %in% c("5", "host")] <- "host_in_refugee_form"
  out[is.na(out) & camp %in% c("8W", "9", "10")] <- "intervention"
  out[is.na(out) & camp %in% c("3", "4", "5", "8E", "18")] <- "comparison"
  out
}

raw_import_count_csv_rows <- function(path) {
  if (!file.exists(path)) return(NA_integer_)
  max(0L, length(readLines(path, warn = FALSE)) - 1L)
}

raw_import_file_manifest <- function(specs, dataset_scope, included = TRUE, reason = "") {
  if (!nrow(specs)) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  data.frame(
    dataset_scope = dataset_scope,
    raw_source_path = normalizePath(specs$path, winslash = "/", mustWork = TRUE),
    raw_source_file = basename(specs$path),
    raw_role = specs$role,
    raw_collection_round = specs$source_group,
    default_collection_year = specs$default_year,
    rows_in_raw_file = vapply(specs$path, raw_import_count_csv_rows, integer(1)),
    included_in_raw_import = included,
    exclusion_or_note = reason,
    stringsAsFactors = FALSE
  )
}

raw_import_update_manifest <- function(manifest, dataset_scope) {
  path <- raw_import_path("4_data", "clean_final", "raw_import_manifest.csv")
  raw_import_ensure_parent_dir(path)
  if (file.exists(path)) {
    existing <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    existing <- existing[existing$dataset_scope != dataset_scope, , drop = FALSE]
    manifest <- raw_import_bind_rows_fill(list(existing, manifest))
  }
  write.csv(manifest, path, row.names = FALSE, na = "")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

raw_import_make_survey_specs <- function(paths, default_year, source_group) {
  paths <- normalizePath(paths[file.exists(paths)], winslash = "/", mustWork = TRUE)
  data.frame(
    path = paths,
    role = vapply(paths, raw_import_detect_survey_role, character(1)),
    default_year = as.integer(default_year),
    source_group = source_group,
    stringsAsFactors = FALSE
  )
}

raw_import_read_survey_specs <- function(specs, community, data_type_prefix) {
  out <- vector("list", nrow(specs))
  for (i in seq_len(nrow(specs))) {
    data <- raw_import_read_csv_character(specs$path[i])
    if (!nrow(data)) {
      out[[i]] <- NULL
      next
    }
    date_info <- raw_import_collection_timestamp(
      data,
      date_cols = c("start_date", "SubmissionDate", "starttime", "endtime", "date", "datetime")
    )
    file_date <- raw_import_extract_date_any(basename(specs$path[i]))
    missing_date <- is.na(date_info$date) & !is.na(file_date[1])
    date_info$date[missing_date] <- file_date[1]
    date_info$year[missing_date] <- as.integer(format(file_date[1], "%Y"))
    date_info$timepoint[missing_date] <- raw_import_timepoint_from_date(file_date[1])
    date_info$source_col[missing_date] <- "raw_source_file_date"
    data$community <- community
    data$study_arm_overall <- raw_import_study_arm_overall(data, community)
    data$data_type <- paste(data_type_prefix, specs$role[i], sep = "_")
    data$raw_role <- specs$role[i]
    data$raw_collection_round <- specs$source_group[i]
    data$raw_survey_version <- raw_import_survey_version(specs$path[i])
    data$raw_source_file <- basename(specs$path[i])
    data$raw_source_path <- normalizePath(specs$path[i], winslash = "/", mustWork = TRUE)
    data$raw_source_order <- i
    data$collection_date <- date_info$date
    data$collection_year <- date_info$year
    data$timepoint_source_col <- date_info$source_col
    data$timepoint <- date_info$timepoint
    out[[i]] <- data
  }
  raw_import_bind_rows_fill(out)
}

raw_import_deduplicate_household_uuid <- function(data) {
  if (!nrow(data)) {
    return(list(data = data, removed = 0L, key_used = NA_character_))
  }

  key_col <- NA_character_
  for (candidate in c("KEY", "uuid")) {
    if (!candidate %in% names(data)) next
    key <- raw_import_clean_character(data[[candidate]])
    nonmissing <- key[!is.na(key)]
    if (!length(nonmissing)) next
    unique_ratio <- length(unique(nonmissing)) / length(nonmissing)
    if (identical(candidate, "KEY") || unique_ratio >= 0.80) {
      key_col <- candidate
      break
    }
  }

  if (is.na(key_col)) {
    return(list(data = data, removed = 0L, key_used = NA_character_))
  }

  key <- raw_import_clean_character(data[[key_col]])
  data$.raw_row_order <- seq_len(nrow(data))
  keep_missing_key <- is.na(key)
  keyed <- data[!keep_missing_key, , drop = FALSE]
  if (!nrow(keyed)) {
    data$.raw_row_order <- NULL
    return(list(data = data, removed = 0L, key_used = key_col))
  }

  keyed$.dedupe_key <- key[!keep_missing_key]
  keyed <- keyed[order(keyed$.dedupe_key, keyed$raw_source_order, keyed$.raw_row_order), , drop = FALSE]
  keep <- !duplicated(keyed$.dedupe_key, fromLast = TRUE)
  removed <- sum(!keep)
  keyed <- keyed[keep, , drop = FALSE]
  keyed$.dedupe_key <- NULL

  unkeyed <- data[keep_missing_key, , drop = FALSE]
  out <- raw_import_bind_rows_fill(list(keyed, unkeyed))
  out <- out[order(out$.raw_row_order), , drop = FALSE]
  out$.raw_row_order <- NULL
  row.names(out) <- NULL
  list(data = out, removed = removed, key_used = key_col)
}
raw_import_write_rds <- function(data, relative_path) {
  path <- raw_import_path(relative_path)
  raw_import_ensure_parent_dir(path)
  saveRDS(data, path)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

raw_import_write_csv <- function(data, relative_path) {
  path <- raw_import_path(relative_path)
  raw_import_ensure_parent_dir(path)
  write.csv(data, path, row.names = FALSE, na = "")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

raw_import_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}


