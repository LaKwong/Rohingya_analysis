################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Final refugee PATS+ PM2.5 clean datasets
# @Description: Raw-first final cleaner with PM cleaning decisions implemented directly
#   from 3_data_cleaning/4_clean_pm_data.R.
################################################################################

if (!exists("clean_final_project_root", mode = "function")) {
  helper_from_root <- file.path("3_data_cleaning", "fixed", "0_clean_helpers_20260805_2141.R")
  if (file.exists(helper_from_root)) {
    source(helper_from_root)
  } else {
    stop("Run from the Rohingya_analysis project root or source 0_clean_helpers_20260805_2141.R first.", call. = FALSE)
  }
}

source_rel <- "4_data/clean_final/imported_raw/pm25_pats_refugee_raw.rds"
pm <- read_rds_required(source_rel)

pm_audit_rows <- list()
record_pm_audit <- function(rule_id, rows_affected, note, source_script = "3_data_cleaning/4_clean_pm_data.R") {
  pm_audit_rows[[length(pm_audit_rows) + 1L]] <<- data.frame(
    rule_id = rule_id,
    rows_affected = as.integer(rows_affected),
    note = note,
    source_script = source_script,
    stringsAsFactors = FALSE
  )
}

clean_pm_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NAN", "NULL", "null")] <- NA_character_
  x
}

coalesce_pm_chr <- function(primary, fallback) {
  primary <- clean_pm_chr(primary)
  fallback <- clean_pm_chr(fallback)
  ifelse(!is.na(primary), primary, fallback)
}

parse_pm_datetime <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = "Asia/Dhaka"))
  }

  x_chr <- clean_pm_chr(x)
  out <- as.POSIXct(rep(NA_real_, length(x_chr)), origin = "1970-01-01", tz = "Asia/Dhaka")
  formats <- c(
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
    "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M",
    "%m/%d/%y %H:%M:%S", "%m/%d/%y %H:%M"
  )

  for (fmt in formats) {
    idx <- which(is.na(out) & !is.na(x_chr))
    if (!length(idx)) break
    parsed <- suppressWarnings(as.POSIXct(x_chr[idx], format = fmt, tz = "Asia/Dhaka"))
    out[idx[!is.na(parsed)]] <- parsed[!is.na(parsed)]
  }

  out
}

round_pm_posix <- function(x, units) {
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "Asia/Dhaka")
  idx <- !is.na(x)
  out[idx] <- round(x[idx], units = units)
  out
}

pm_posix_hour <- function(x) {
  out <- rep(NA_integer_, length(x))
  idx <- !is.na(x)
  if (any(idx)) out[idx] <- as.POSIXlt(x[idx], tz = "Asia/Dhaka")$hour
  out
}

pm_posix_min <- function(x) {
  out <- rep(NA_integer_, length(x))
  idx <- !is.na(x)
  if (any(idx)) out[idx] <- as.POSIXlt(x[idx], tz = "Asia/Dhaka")$min
  out
}

make_pm_nearest_time <- function(hour, minute) {
  out <- as.POSIXct(rep(NA_real_, length(hour)), origin = "1970-01-01", tz = "Asia/Dhaka")
  idx <- !is.na(hour) & !is.na(minute)
  out[idx] <- as.POSIXct(
    sprintf("2000-01-01 %02d:%02d:00", hour[idx], minute[idx]),
    tz = "Asia/Dhaka"
  )
  out
}

pm_last_six <- function(x) {
  x <- clean_pm_chr(x)
  out <- rep(NA_character_, length(x))
  idx <- !is.na(x)
  out[idx] <- substr(x[idx], pmax(1L, nchar(x[idx]) - 5L), nchar(x[idx]))
  out
}

parse_pm_filename <- function(file_name) {
  file_name <- clean_pm_chr(file_name)
  file_name_clean <- sub("^[^_]{2,3}_", "", file_name)
  parts <- strsplit(file_name_clean, "_", fixed = TRUE)

  get_part <- function(i) {
    vapply(
      parts,
      function(x) if (length(x) >= i) x[i] else NA_character_,
      character(1)
    )
  }

  data.frame(
    file_name_clean = file_name_clean,
    pm_monitor_from_file = get_part(1),
    file_date_token = get_part(2),
    study_arm_code = get_part(3),
    hh_id_note_from_file = vapply(
      parts,
      function(x) {
        if (length(x) < 4L) return(NA_character_)
        paste(x[4:length(x)], collapse = "_")
      },
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

normalize_pm_hh_note <- function(x) {
  x <- clean_pm_chr(x)
  x_lower <- tolower(x)

  x[!is.na(x_lower) & x_lower == "4fpp11_school"] <- "4EPP11_school"
  x[!is.na(x_lower) & grepl("^(8wi18|8wdi18)_(mosjid|mosque)$", x_lower)] <- "8WI18_mosque"
  x[!is.na(x_lower) & grepl("^(10d12|10dd12|10gg09)_(mosjid|mosque)$", x_lower)] <- "10GG9_mosque"

  x
}

split_pm_hh_note <- function(x) {
  x <- clean_pm_chr(x)
  parts <- strsplit(x, "_", fixed = TRUE)
  hh_id <- vapply(
    parts,
    function(value) if (length(value) >= 1L && !is.na(value[1])) value[1] else NA_character_,
    character(1)
  )
  note <- vapply(
    parts,
    function(value) {
      if (length(value) < 2L || is.na(value[2])) return(NA_character_)
      paste(value[2:length(value)], collapse = "_")
    },
    character(1)
  )
  note <- tolower(clean_pm_chr(note))
  note[is.na(note)] <- "normal"

  data.frame(hh_id_from_file = hh_id, note_from_file = note, stringsAsFactors = FALSE)
}

rebuild_pm_hh_note <- function(hh_or_site_id, note) {
  hh_or_site_id <- clean_pm_chr(hh_or_site_id)
  note <- clean_pm_chr(note)
  ifelse(!is.na(hh_or_site_id) & !is.na(note), paste(hh_or_site_id, note, sep = "_"), NA_character_)
}

recode_pm_study_arm <- function(study_arm_code, note) {
  code <- toupper(clean_pm_chr(study_arm_code))
  note <- tolower(clean_pm_chr(note))
  out <- rep(NA_character_, length(code))
  out[code == "0"] <- "intervention"
  out[code %in% c("1", "I")] <- "comparison"
  out[note == "school"] <- "comparison"
  out[note == "mosque"] <- "intervention"
  out
}

set_pm_insufficient_data <- function(data, timepoint, hh_id_notes) {
  idx <- data$timepoint == timepoint & data$hh_id_note %in% hh_id_notes
  idx[is.na(idx)] <- FALSE
  record_pm_audit(
    paste0("reviewed_", timepoint, "_insufficient_data"),
    sum(idx, na.rm = TRUE),
    paste("Relabel short-duration household PATS+ files as insufficient data:", paste(hh_id_notes, collapse = "; "))
  )
  if (any(idx, na.rm = TRUE)) {
    data$note_clean[idx] <- "insufficient data"
    data$note[idx] <- "insufficient data"
    data$hh_id_note[idx] <- rebuild_pm_hh_note(data$hh_id[idx], data$note_clean[idx])
  }
  data
}

pm_current_cleaner <- "3_data_cleaning/fixed/clean_pm_pats_refugee_20260805_2141.R"
pm_timepoint_levels <- c("baseline", "midline", "endline")

ordered_pm_timepoint <- function(x) {
  factor(clean_pm_chr(x), levels = pm_timepoint_levels, ordered = TRUE)
}
pm_timepoint_windows <- data.frame(
  timepoint = pm_timepoint_levels,
  start_date = as.Date(c("2019-08-01", "2020-09-01", "2022-01-01")),
  end_date = as.Date(c("2019-12-01", "2020-11-15", "2022-06-15")),
  stringsAsFactors = FALSE
)

pm_timepoint_from_date <- function(date) {
  date <- as.Date(date)
  out <- rep(NA_character_, length(date))
  for (i in seq_len(nrow(pm_timepoint_windows))) {
    idx <- !is.na(date) &
      date >= pm_timepoint_windows$start_date[i] &
      date <= pm_timepoint_windows$end_date[i]
    out[idx] <- pm_timepoint_windows$timepoint[i]
  }
  out
}

parse_pm_date_token <- function(x) {
  x <- clean_pm_chr(x)
  out <- as.Date(rep(NA_character_, length(x)))
  if (!length(x)) return(out)

  match_8 <- regexpr("20[0-9]{6}", x, perl = TRUE)
  has_8 <- !is.na(x) & match_8 > 0
  if (any(has_8)) {
    token_8 <- rep(NA_character_, length(x))
    token_8[has_8] <- substring(
      x[has_8],
      match_8[has_8],
      match_8[has_8] + attr(match_8, "match.length")[has_8] - 1L
    )
    parsed_8 <- suppressWarnings(as.Date(token_8, format = "%Y%m%d"))
    fill <- is.na(out) & !is.na(parsed_8)
    out[fill] <- parsed_8[fill]
  }

  match_6 <- regexpr("(?<![0-9])[0-9]{6}(?![0-9])", x, perl = TRUE)
  has_6 <- is.na(out) & !is.na(x) & match_6 > 0
  if (any(has_6)) {
    token_6 <- rep(NA_character_, length(x))
    token_6[has_6] <- substring(
      x[has_6],
      match_6[has_6],
      match_6[has_6] + attr(match_6, "match.length")[has_6] - 1L
    )
    parsed_6 <- suppressWarnings(as.Date(token_6, format = "%y%m%d"))
    fill <- is.na(out) & !is.na(parsed_6)
    out[fill] <- parsed_6[fill]
  }

  out
}

extract_pm_collection_date <- function(data) {
  collection_date <- as.Date(rep(NA_character_, nrow(data)))
  source_col <- rep(NA_character_, nrow(data))

  # Timepoint labels must come from dates embedded in file names, not from
  # parent folder names or batch/export dates in raw_source_path.
  token_cols <- intersect(c("file_date_token", "raw_source_file", "file_name"), names(data))
  for (col in token_cols) {
    parsed <- parse_pm_date_token(data[[col]])
    idx <- is.na(collection_date) & !is.na(parsed)
    collection_date[idx] <- parsed[idx]
    source_col[idx] <- col
  }

  list(date = collection_date, source_col = source_col)
}
recode_pm_timepoint <- function(data, dataset_name) {
  old_timepoint <- if ("timepoint" %in% names(data)) clean_pm_chr(data$timepoint) else rep(NA_character_, nrow(data))
  collection <- extract_pm_collection_date(data)
  final_timepoint <- pm_timepoint_from_date(collection$date)
  source_col <- ifelse(!is.na(collection$date), collection$source_col, NA_character_)

  data$timepoint_original <- old_timepoint
  data$collection_date <- collection$date
  data$collection_year <- as.integer(format(collection$date, "%Y"))
  data$timepoint_source_col <- source_col
  data$timepoint <- ordered_pm_timepoint(final_timepoint)

  summary <- data.frame(
    dataset = dataset_name,
    timepoint_original = old_timepoint,
    collection_date = as.character(collection$date),
    collection_year = as.character(data$collection_year),
    timepoint_source_col = source_col,
    timepoint_final = final_timepoint,
    raw_collection_round = if ("raw_collection_round" %in% names(data)) clean_pm_chr(data$raw_collection_round) else NA_character_,
    n = 1L,
    stringsAsFactors = FALSE
  )
  summary[is.na(summary)] <- "(missing)"
  summary <- aggregate(
    n ~ dataset + timepoint_original + collection_date + collection_year + timepoint_source_col + timepoint_final + raw_collection_round,
    data = summary,
    FUN = sum
  )
  summary$collection_date[summary$collection_date == "(missing)"] <- NA_character_

  record_pm_audit(
    paste0("reviewed_pm_timepoint_labeling_", gsub("[^A-Za-z0-9]+", "_", dataset_name)),
    nrow(data),
    paste(
      "Label PM2.5 rows only from dates parsed from filename fields/file_date_token;",
      "do not use raw household source-folder labels or folder years for PM2.5 timepoints.",
      "Date windows are baseline 2019-08-01 to 2019-12-01, midline 2020-09-01 to 2020-11-15,",
      "and endline 2022-01-01 to 2022-06-15. Rows without a filename-derived date in these windows keep timepoint = NA."
    ),
    source_script = pm_current_cleaner
  )

  list(data = data, summary = summary)
}
safe_pm_mean <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

safe_pm_gmean <- function(x) {
  x <- x[!is.na(x) & x > 0]
  if (!length(x)) return(NA_real_)
  exp(mean(log(x)))
}

safe_pm_quantile <- function(x, prob) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE, type = 7))
}

safe_pm_min <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  min(x)
}

safe_pm_max <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  max(x)
}

pm_distinct_count <- function(x) {
  x <- clean_pm_chr(x)
  length(unique(x[!is.na(x)]))
}

make_pm_quality_summary <- function(data, dataset_name, group_cols) {
  group_cols <- intersect(group_cols, names(data))
  if (!length(group_cols)) {
    data$.pm_quality_group <- "all"
    group_cols <- ".pm_quality_group"
  }

  metric_names <- c(
    "n_rows", "n_monitor_files", "n_households", "n_ambient_sites",
    "n_missing_datetime", "n_duplicate_datetime_rows", "start_datetime", "end_datetime", "duration_hours",
    "n_missing_pm25", "mean_pm25_ug_m3", "gmean_pm25_ug_m3", "median_pm25_ug_m3",
    "p95_pm25_ug_m3", "p99_pm25_ug_m3", "max_pm25_ug_m3", "max_pm25_uncensored_ug_m3",
    "n_uncensored_pm_lt_10", "n_uncensored_pm_gt_30000", "n_pm_ge_400", "n_pm_ge_1000",
    "n_pm_ge_5000", "pct_pm_ge_1000", "pct_pm_ge_5000", "n_low_v_power_after_filter",
    "min_v_power"
  )

  if (!nrow(data)) {
    out <- data.frame(dataset = character(), stringsAsFactors = FALSE)
    for (col in group_cols) out[[col]] <- character()
    for (col in metric_names) out[[col]] <- numeric()
    return(out)
  }

  key_data <- data[group_cols]
  key_data[] <- lapply(key_data, function(x) {
    x <- clean_pm_chr(x)
    x[is.na(x)] <- "(missing)"
    x
  })
  split_key <- interaction(key_data, drop = TRUE, sep = "\r", lex.order = TRUE)
  split_idx <- split(seq_len(nrow(data)), split_key, drop = TRUE)

  rows <- lapply(split_idx, function(idx) {
    block <- data[idx, , drop = FALSE]
    key_values <- key_data[idx[1L], , drop = FALSE]
    pm <- if ("pm25_ug_m3" %in% names(block)) suppressWarnings(as.numeric(block$pm25_ug_m3)) else suppressWarnings(as.numeric(block$PM_Estimate))
    pm_uncensored <- if ("PM_Estimate_uncensored" %in% names(block)) suppressWarnings(as.numeric(block$PM_Estimate_uncensored)) else pm
    dt <- if ("dateTime" %in% names(block)) as.POSIXct(block$dateTime, tz = "Asia/Dhaka") else as.POSIXct(rep(NA_real_, nrow(block)), origin = "1970-01-01", tz = "Asia/Dhaka")
    v_power <- if ("V_power" %in% names(block)) suppressWarnings(as.numeric(block$V_power)) else rep(NA_real_, nrow(block))
    valid_dt <- dt[!is.na(dt)]
    valid_pm_n <- sum(!is.na(pm))
    start_dt <- if (length(valid_dt)) min(valid_dt) else as.POSIXct(NA_real_, origin = "1970-01-01", tz = "Asia/Dhaka")
    end_dt <- if (length(valid_dt)) max(valid_dt) else as.POSIXct(NA_real_, origin = "1970-01-01", tz = "Asia/Dhaka")
    duplicate_dt <- if (length(valid_dt)) sum(duplicated(valid_dt) | duplicated(valid_dt, fromLast = TRUE)) else 0L

    data.frame(
      dataset = dataset_name,
      key_values,
      n_rows = length(idx),
      n_monitor_files = if ("raw_source_file" %in% names(block)) pm_distinct_count(block$raw_source_file) else NA_integer_,
      n_households = if ("hh_id" %in% names(block)) pm_distinct_count(block$hh_id) else NA_integer_,
      n_ambient_sites = if ("ambient_site_id" %in% names(block)) pm_distinct_count(block$ambient_site_id) else NA_integer_,
      n_missing_datetime = sum(is.na(dt)),
      n_duplicate_datetime_rows = duplicate_dt,
      start_datetime = as.character(start_dt),
      end_datetime = as.character(end_dt),
      duration_hours = if (length(valid_dt)) as.numeric(difftime(end_dt, start_dt, units = "hours")) else NA_real_,
      n_missing_pm25 = sum(is.na(pm)),
      mean_pm25_ug_m3 = safe_pm_mean(pm),
      gmean_pm25_ug_m3 = safe_pm_gmean(pm),
      median_pm25_ug_m3 = safe_pm_quantile(pm, 0.50),
      p95_pm25_ug_m3 = safe_pm_quantile(pm, 0.95),
      p99_pm25_ug_m3 = safe_pm_quantile(pm, 0.99),
      max_pm25_ug_m3 = safe_pm_max(pm),
      max_pm25_uncensored_ug_m3 = safe_pm_max(pm_uncensored),
      n_uncensored_pm_lt_10 = sum(!is.na(pm_uncensored) & pm_uncensored < 10),
      n_uncensored_pm_gt_30000 = sum(!is.na(pm_uncensored) & pm_uncensored > 30000),
      n_pm_ge_400 = sum(!is.na(pm) & pm >= 400),
      n_pm_ge_1000 = sum(!is.na(pm) & pm >= 1000),
      n_pm_ge_5000 = sum(!is.na(pm) & pm >= 5000),
      pct_pm_ge_1000 = if (valid_pm_n > 0) 100 * sum(!is.na(pm) & pm >= 1000) / valid_pm_n else NA_real_,
      pct_pm_ge_5000 = if (valid_pm_n > 0) 100 * sum(!is.na(pm) & pm >= 5000) / valid_pm_n else NA_real_,
      n_low_v_power_after_filter = sum(!is.na(v_power) & v_power <= 3.6),
      min_v_power = safe_pm_min(v_power),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

write_pm_data_quality_outputs <- function(data, dataset_name) {
  slug <- gsub("(^_+|_+$)", "", gsub("[^A-Za-z0-9]+", "_", dataset_name))
  by_file <- make_pm_quality_summary(
    data,
    dataset_name,
    c(
      "data_type", "raw_collection_round", "raw_source_type", "location_type", "timepoint",
      "study_arm_overall", "study_arm", "note_clean", "hh_id", "fcn_id", "hh_id_note",
      "ambient_site_id", "PM_monitor", "raw_source_file"
    )
  )
  by_timepoint <- make_pm_quality_summary(
    data,
    dataset_name,
    c("data_type", "raw_collection_round", "raw_source_type", "location_type", "timepoint", "study_arm_overall", "note_clean")
  )

  by_file_path <- clean_final_path("4_data", "clean_final", paste0(slug, "_data_quality_by_file_internal.csv"))
  by_timepoint_path <- clean_final_path("4_data", "clean_final", paste0(slug, "_data_quality_by_timepoint_internal.csv"))
  ensure_parent_dir(by_file_path)
  write.csv(by_file, by_file_path, row.names = FALSE, na = "")
  write.csv(by_timepoint, by_timepoint_path, row.names = FALSE, na = "")

  record_pm_audit(
    paste0("reviewed_pm_data_quality_check_", slug),
    nrow(data),
    paste(
      "Wrote internal PM2.5 data quality checks by file and by timepoint/study-arm group for", dataset_name,
      "including monitoring duration, missing date/time and PM values, duplicate timestamps, censored PM values,",
      "very high PM count summaries, and low-voltage checks after the V_power > 3.6 filter."
    ),
    source_script = pm_current_cleaner
  )

  list(
    by_file_path = normalizePath(by_file_path, winslash = "/", mustWork = TRUE),
    by_timepoint_path = normalizePath(by_timepoint_path, winslash = "/", mustWork = TRUE)
  )
}

make_pm_household_count_summary <- function(data, dataset_name) {
  timepoints <- c("baseline", "midline", "endline")
  arms <- c("intervention", "comparison")
  scaffold <- expand.grid(
    timepoint = timepoints,
    study_arm_overall = arms,
    stringsAsFactors = FALSE
  )

  eligible <- data[
    clean_pm_chr(data$timepoint) %in% timepoints &
      clean_pm_chr(data$study_arm_overall) %in% arms &
      !is.na(clean_pm_chr(data$hh_id)),
    ,
    drop = FALSE
  ]

  household_counts <- data.frame(
    timepoint = character(),
    study_arm_overall = character(),
    n_households = integer(),
    stringsAsFactors = FALSE
  )
  row_counts <- data.frame(
    timepoint = character(),
    study_arm_overall = character(),
    n_pm_rows = integer(),
    stringsAsFactors = FALSE
  )
  file_counts <- data.frame(
    timepoint = character(),
    study_arm_overall = character(),
    n_monitor_files = integer(),
    stringsAsFactors = FALSE
  )

  if (nrow(eligible)) {
    household_keys <- unique(eligible[c("timepoint", "study_arm_overall", "hh_id")])
    household_counts <- aggregate(
      hh_id ~ timepoint + study_arm_overall,
      data = household_keys,
      FUN = length
    )
    names(household_counts)[names(household_counts) == "hh_id"] <- "n_households"

    eligible$.pm_row_count <- 1L
    row_counts <- aggregate(
      .pm_row_count ~ timepoint + study_arm_overall,
      data = eligible,
      FUN = sum
    )
    names(row_counts)[names(row_counts) == ".pm_row_count"] <- "n_pm_rows"

    if ("raw_source_file" %in% names(eligible)) {
      file_keys <- unique(eligible[c("timepoint", "study_arm_overall", "raw_source_file")])
      file_counts <- aggregate(
        raw_source_file ~ timepoint + study_arm_overall,
        data = file_keys,
        FUN = length
      )
      names(file_counts)[names(file_counts) == "raw_source_file"] <- "n_monitor_files"
    }
  }

  out <- merge(scaffold, household_counts, by = c("timepoint", "study_arm_overall"), all.x = TRUE)
  out <- merge(out, row_counts, by = c("timepoint", "study_arm_overall"), all.x = TRUE)
  out <- merge(out, file_counts, by = c("timepoint", "study_arm_overall"), all.x = TRUE)
  out$n_households[is.na(out$n_households)] <- 0L
  out$n_pm_rows[is.na(out$n_pm_rows)] <- 0L
  out$n_monitor_files[is.na(out$n_monitor_files)] <- 0L
  out$dataset <- dataset_name
  out$count_unit <- "distinct hh_id with cleaned indoor normal PATS+ rows"
  out <- out[order(match(out$timepoint, timepoints), match(out$study_arm_overall, arms)), ]
  row.names(out) <- NULL
  out[c("dataset", "timepoint", "study_arm_overall", "n_households", "n_monitor_files", "n_pm_rows", "count_unit")]
}

write_pm_household_count_summary <- function(summary) {
  path <- clean_final_path(
    "4_data",
    "clean_final",
    "pm25_pats_refugee_indoor_household_counts_by_arm_timepoint.csv"
  )
  ensure_parent_dir(path)
  write.csv(summary, path, row.names = FALSE, na = "")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

# Remove exact repeated raw rows before applying row-level cleaning rules. This
# only drops records where every imported column is identical to a previous row.
pm_exact_duplicate_rows <- duplicated(pm)
pm_exact_duplicate_rows[is.na(pm_exact_duplicate_rows)] <- FALSE
exact_duplicate_summary_path <- clean_final_path(
  "4_data", "clean_final", "pm25_pats_refugee_exact_duplicate_rows_removed_internal.csv"
)
ensure_parent_dir(exact_duplicate_summary_path)

duplicate_summary_cols <- intersect(
  c("raw_collection_round", "raw_source_type", "raw_source_file", "file_name", "dateTime", "PM_Estimate"),
  names(pm)
)
if (any(pm_exact_duplicate_rows, na.rm = TRUE) && length(duplicate_summary_cols) > 0) {
  duplicate_summary <- pm[pm_exact_duplicate_rows, duplicate_summary_cols, drop = FALSE]
  duplicate_summary[] <- lapply(duplicate_summary, function(x) {
    x <- clean_pm_chr(x)
    x[is.na(x)] <- "(missing)"
    x
  })
  duplicate_summary$.duplicate_rows_removed <- 1L
  duplicate_summary <- aggregate(
    .duplicate_rows_removed ~ .,
    data = duplicate_summary,
    FUN = sum
  )
  names(duplicate_summary)[names(duplicate_summary) == ".duplicate_rows_removed"] <- "n_exact_duplicate_rows_removed"
} else {
  duplicate_summary <- data.frame(
    n_exact_duplicate_rows_removed = integer(),
    stringsAsFactors = FALSE
  )
}
write.csv(duplicate_summary, exact_duplicate_summary_path, row.names = FALSE, na = "")
record_pm_audit(
  "reviewed_drop_exact_duplicate_import_rows",
  sum(pm_exact_duplicate_rows, na.rm = TRUE),
  paste(
    "Drop exact repeated imported PATS+ rows before cleaning, keeping the first occurrence.",
    "A row is removed only if every imported column is identical to a previous row, including timestamp and PM2.5 value."
  ),
  source_script = pm_current_cleaner
)
if (any(pm_exact_duplicate_rows, na.rm = TRUE)) {
  pm <- pm[!pm_exact_duplicate_rows, , drop = FALSE]
}
pm$community <- "refugee"

if ("V_power" %in% names(pm)) {
  pm$V_power <- suppressWarnings(as.numeric(pm$V_power))
  keep_voltage <- !is.na(pm$V_power) & pm$V_power > 3.6
  record_pm_audit(
    "reviewed_filter_battery_voltage_gt_3_6",
    sum(!keep_voltage),
    "Filter out PATS+ rows with V_power <= 3.6 or missing, matching the PM manual note in the reviewed cleaner."
  )
  pm <- pm[keep_voltage, , drop = FALSE]
}

parsed_file <- parse_pm_filename(pm$file_name)
pm$file_name_clean <- parsed_file$file_name_clean
pm$file_date_token <- parsed_file$file_date_token
pm$study_arm_code <- parsed_file$study_arm_code
pm$hh_id_note_raw <- parsed_file$hh_id_note_from_file
pm$hh_id_note_cleaned_from_file <- normalize_pm_hh_note(pm$hh_id_note_raw)
record_pm_audit(
  "reviewed_filename_site_recodes",
  sum(!is.na(pm$hh_id_note_raw) & pm$hh_id_note_raw != pm$hh_id_note_cleaned_from_file),
  "Apply manual recodes for 4FPP11 school, 8WDI18/8WI18 mosjid/mosque, and 10D12/10DD12/10GG09 mosque filenames."
)

split_file <- split_pm_hh_note(pm$hh_id_note_cleaned_from_file)
pm$note_clean <- split_file$note_from_file
pm$note_clean <- coalesce_pm_chr(pm$note_clean, tolower(clean_pm_chr(pm$note)))
pm$note_clean[is.na(pm$note_clean)] <- "normal"
pm$note <- pm$note_clean

if (!"raw_source_type" %in% names(pm)) pm$raw_source_type <- NA_character_
is_ambient_outdoor <- pm$raw_source_type == "ambient_outdoor_folder"
is_ambient_outdoor[is.na(is_ambient_outdoor)] <- FALSE

if (!"hh_id_original" %in% names(pm) && "hh_id" %in% names(pm)) pm$hh_id_original <- as.character(pm$hh_id)
if (!"fcn_id_original" %in% names(pm) && "fcn_id" %in% names(pm)) pm$fcn_id_original <- as.character(pm$fcn_id)

pm$ambient_site_id <- ifelse(is_ambient_outdoor, split_file$hh_id_from_file, NA_character_)
pm$hh_id <- ifelse(is_ambient_outdoor, NA_character_, split_file$hh_id_from_file)
pm$hh_id <- ifelse(
  !is_ambient_outdoor & is.na(pm$hh_id) & "hh_id_original" %in% names(pm),
  clean_pm_chr(pm$hh_id_original),
  pm$hh_id
)
pm$fcn_id <- ifelse(is_ambient_outdoor, NA_character_, pm_last_six(pm$hh_id))
pm$hh_id_note <- rebuild_pm_hh_note(ifelse(is_ambient_outdoor, pm$ambient_site_id, pm$hh_id), pm$note_clean)

pm$dateTime <- parse_pm_datetime(pm$dateTime)
record_pm_audit(
  "reviewed_parse_datetime",
  sum(!is.na(pm$dateTime)),
  "Parse dateTime and create rounded minute/hour and time-of-day fields."
)
pm$dateTime_min <- round_pm_posix(pm$dateTime, "mins")
pm$dateTime_hour <- round_pm_posix(pm$dateTime, "hours")
pm$dateTime_min_dhaka <- as.POSIXct(pm$dateTime_min, tz = "Asia/Dhaka")
pm$dateTime_hour_dhaka <- as.POSIXct(pm$dateTime_hour, tz = "Asia/Dhaka")
pm$nearest_min <- make_pm_nearest_time(pm_posix_hour(pm$dateTime_hour), pm_posix_min(pm$dateTime_min))
pm$nearest_hour <- make_pm_nearest_time(pm_posix_hour(pm$dateTime_hour), rep(0L, nrow(pm)))

recoded_pm_all <- recode_pm_timepoint(pm, "pm25_pats_refugee_all_rows")
pm <- recoded_pm_all$data
write_timepoint_summary(recoded_pm_all$summary, "pm25_pats_refugee_all_rows")

parsed_study_arm <- recode_pm_study_arm(pm$study_arm_code, pm$note_clean)
pm$study_arm <- coalesce_pm_chr(parsed_study_arm, pm$study_arm)
pm$study_arm_overall <- coalesce_pm_chr(parsed_study_arm, pm$study_arm_overall)
pm$study_arm_overall[pm$note_clean == "school"] <- "comparison"
pm$study_arm_overall[pm$note_clean == "mosque"] <- "intervention"
pm$study_arm[pm$note_clean == "school"] <- "comparison"
pm$study_arm[pm$note_clean == "mosque"] <- "intervention"

pm <- set_pm_insufficient_data(
  pm,
  "baseline",
  c("10G108351_normal", "8wDI21101274_normal", "4E277068_normal")
)
pm <- set_pm_insufficient_data(
  pm,
  "midline",
  c("4EPP15184706_normal", "4EPP11277012_normal", "8WDI20101573_normal")
)
pm <- set_pm_insufficient_data(
  pm,
  "endline",
  c(
    "4FUU14293862_normal", "10FF31201201_normal", "4EPP11302711_normal",
    "4EPP15175472_normal", "9GG29123665_normal", "9GG29115465_normal",
    "4FUU14296441_normal"
  )
)

idx_10gg09_normal <- pm$timepoint == "endline" & pm$hh_id_note == "10GG09_normal"
idx_10gg09_normal[is.na(idx_10gg09_normal)] <- FALSE
record_pm_audit(
  "reviewed_endline_10gg09_normal_to_10gg9",
  sum(idx_10gg09_normal, na.rm = TRUE),
  "Correct endline household ID 10GG09_normal to 10GG9_normal."
)
if (any(idx_10gg09_normal, na.rm = TRUE)) {
  pm$hh_id[idx_10gg09_normal] <- "10GG9"
  pm$fcn_id[idx_10gg09_normal] <- pm_last_six(pm$hh_id[idx_10gg09_normal])
  pm$hh_id_note[idx_10gg09_normal] <- "10GG9_normal"
}

idx_4e179029_qc <- pm$hh_id_note == "4E179029_qc"
idx_4e179029_qc[is.na(idx_4e179029_qc)] <- FALSE
record_pm_audit(
  "reviewed_qc_4e179029_to_normal",
  sum(idx_4e179029_qc, na.rm = TRUE),
  "Rename 4E179029_qc to normal because no companion non-QC file exists."
)
if (any(idx_4e179029_qc, na.rm = TRUE)) {
  pm$note_clean[idx_4e179029_qc] <- "normal"
  pm$note[idx_4e179029_qc] <- "normal"
  pm$hh_id_note[idx_4e179029_qc] <- "4E179029_normal"
}

pm$location_type <- ifelse(
  is_ambient_outdoor | pm$note_clean %in% c("school", "mosque", "outside", "ambient"),
  "ambient",
  "indoor"
)
pm$include_in_indoor_final <- pm$location_type == "indoor" & pm$note_clean == "normal"
pm$include_in_ambient_final <- pm$location_type == "ambient"

if ("PM_monitor" %in% names(pm)) {
  pm$PM_monitor <- coalesce_pm_chr(pm$PM_monitor, parsed_file$pm_monitor_from_file)
} else {
  pm$PM_monitor <- parsed_file$pm_monitor_from_file
}

pm$PM_Estimate <- suppressWarnings(as.numeric(pm$PM_Estimate))
pm$PM_Estimate_uncensored <- pm$PM_Estimate
low_pm <- !is.na(pm$PM_Estimate) & pm$PM_Estimate < 10
high_pm <- !is.na(pm$PM_Estimate) & pm$PM_Estimate > 30000
record_pm_audit(
  "reviewed_censor_pm_estimate_lower_10",
  sum(low_pm),
  "Replace PM_Estimate values below 10 ug/m3 with 10."
)
record_pm_audit(
  "reviewed_censor_pm_estimate_upper_30000",
  sum(high_pm),
  "Replace PM_Estimate values above 30000 ug/m3 with 30000."
)
pm$PM_Estimate[low_pm] <- 10
pm$PM_Estimate[high_pm] <- 30000
pm$pm25_ug_m3 <- pm$PM_Estimate

pm_audit_path <- clean_final_path("4_data", "clean_final", "pm_pats_refugee_reviewed_cleaning_audit.csv")
ensure_parent_dir(pm_audit_path)

pm_quality_outputs <- write_pm_data_quality_outputs(pm, "pm25_pats_refugee_cleaning_all_rows")

pm_audit <- if (length(pm_audit_rows)) do.call(rbind, pm_audit_rows) else data.frame(stringsAsFactors = FALSE)
write.csv(pm_audit, pm_audit_path, row.names = FALSE, na = "")

pm_qc_files <- unique(pm[pm$note_clean == "qc", intersect(c("timepoint", "study_arm", "study_arm_overall", "hh_id", "fcn_id", "hh_id_note", "PM_monitor", "raw_source_file"), names(pm)), drop = FALSE])
qc_path <- write_final_rds(pm_qc_files, "4_data/clean_final/pm25_pats_refugee_qc_files.rds")
shareable_qc <- make_shareable_dataset(pm_qc_files, "pm25_pats_refugee_qc_files")
shareable_qc_path <- write_shareable_rds(shareable_qc$data, "pm25_pats_refugee_qc_files.rds")
entry_qc <- make_inventory_entry(
  dataset_name = "pm25_pats_refugee_qc_files",
  data = pm_qc_files,
  output_path = qc_path,
  source_paths = clean_final_path(source_rel),
  removed_identifier_columns = character(),
  shareable_output_path = shareable_qc_path,
  shareable_removed_identifier_columns = shareable_qc$removed,
  notes = paste(
    "QC PATS+ file list rebuilt from raw-first imports after applying the reviewed 4E179029 QC-to-normal correction.",
    "This mirrors the PM QC file-list output while keeping it under 4_data/clean_final/."
  )
)
update_inventory(entry_qc)

indoor <- pm[
  pm$include_in_indoor_final &
    pm$study_arm_overall %in% c("intervention", "comparison"),
  ,
  drop = FALSE
]
indoor$data_type <- "pm25_pats_indoor"

write_timepoint_summary(make_timepoint_summary(indoor, "pm25_pats_refugee_indoor"), "pm25_pats_refugee_indoor")
indoor_quality_outputs <- write_pm_data_quality_outputs(indoor, "pm25_pats_refugee_indoor_final")

indoor_household_counts <- make_pm_household_count_summary(indoor, "pm25_pats_refugee_indoor")
indoor_household_counts_path <- write_pm_household_count_summary(indoor_household_counts)

deidentified_indoor <- drop_identifier_columns(indoor, keep = c("PM_monitor"))
indoor <- deidentified_indoor$data
indoor <- move_columns_first(
  indoor,
  c(
    "community", "data_type", "hh_id", "fcn_id", "hh_id_note", "timepoint", "timepoint_original",
    "collection_date", "collection_year", "timepoint_source_col", "dateTime", "dateTime_min", "nearest_min",
    "dateTime_hour", "nearest_hour", "raw_collection_round", "raw_source_file", "study_arm_overall", "study_arm",
    "location_type", "note_clean", "pm25_ug_m3", "PM_Estimate", "PM_Estimate_uncensored", "PM_monitor"
  )
)

indoor_path <- write_final_rds(indoor, "4_data/clean_final/pm25_pats_refugee_indoor.rds")
shareable_indoor <- make_shareable_dataset(indoor, "pm25_pats_refugee_indoor")
shareable_indoor_path <- write_shareable_rds(shareable_indoor$data, "pm25_pats_refugee_indoor.rds")
entry_indoor <- make_inventory_entry(
  dataset_name = "pm25_pats_refugee_indoor",
  data = indoor,
  output_path = indoor_path,
  source_paths = clean_final_path(source_rel),
  removed_identifier_columns = deidentified_indoor$removed,
  shareable_output_path = shareable_indoor_path,
  shareable_removed_identifier_columns = shareable_indoor$removed,
  notes = paste(
    "Refugee household indoor PATS+ PM2.5 data rebuilt from raw-first imports in 2_data_raw.",
    "PM cleaning decisions implemented directly in this file: exact repeated imported row removal, V_power > 3.6 filtering, filename parsing, dateTime rounding, manual filename recodes, insufficient-data labels, 4E179029 QC correction, and PM_Estimate censoring to 10-30000 ug/m3.",
    "PM2.5 timepoints are assigned only from dates parsed from filename fields/file_date_token using PM-specific date windows: baseline 2019-08-01 to 2019-12-01, midline 2020-09-01 to 2020-11-15, and endline 2022-01-01 to 2022-06-15.",
    "Normal household files only are included; QC, insufficient-data, school, mosque, mosjid, outside, and outdoor rows are excluded from the primary indoor output. No manual high-PM household flags or exclusions are applied; all included rows use the same PM_Estimate censoring rule.",
    "The import explicitly includes ALL DATA_ENDLINE_2022_220703."
  )
)
update_inventory(entry_indoor)

entry_indoor_counts <- make_inventory_entry(
  dataset_name = "pm25_pats_refugee_indoor_household_counts_by_arm_timepoint",
  data = indoor_household_counts,
  output_path = indoor_household_counts_path,
  source_paths = c(clean_final_path(source_rel), indoor_path),
  removed_identifier_columns = character(),
  privacy_level = "shareable_aggregate",
  shareable_output_path = indoor_household_counts_path,
  notes = paste(
    "Summary table giving the number of unique refugee households with cleaned indoor PATS+ PM2.5 data by study arm and timepoint.",
    "Counts are based on distinct hh_id values in pm25_pats_refugee_indoor after reviewed cleaning and final inclusion rules."
  )
)
update_inventory(entry_indoor_counts)

ambient <- pm[
  pm$include_in_ambient_final,
  ,
  drop = FALSE
]
ambient$data_type <- "pm25_pats_ambient"
ambient$hh_id[ambient$raw_source_type == "ambient_outdoor_folder"] <- NA_character_
ambient$fcn_id[ambient$raw_source_type == "ambient_outdoor_folder"] <- NA_character_

write_timepoint_summary(make_timepoint_summary(ambient, "pm25_pats_refugee_ambient"), "pm25_pats_refugee_ambient")
ambient_quality_outputs <- write_pm_data_quality_outputs(ambient, "pm25_pats_refugee_ambient_final")

deidentified_ambient <- drop_identifier_columns(ambient, keep = c("PM_monitor", "location_type", "ambient_site_id", "hh_id_note"))
ambient <- deidentified_ambient$data
ambient <- move_columns_first(
  ambient,
  c(
    "community", "data_type", "hh_id", "fcn_id", "hh_id_note", "ambient_site_id", "timepoint", "timepoint_original",
    "collection_date", "collection_year", "timepoint_source_col", "dateTime", "dateTime_min", "nearest_min",
    "dateTime_hour", "nearest_hour", "raw_collection_round", "raw_source_file", "study_arm_overall", "study_arm",
    "location_type", "note_clean", "pm25_ug_m3", "PM_Estimate", "PM_Estimate_uncensored", "PM_monitor"
  )
)

ambient_path <- write_final_rds(ambient, "4_data/clean_final/pm25_pats_refugee_ambient.rds")
shareable_ambient <- make_shareable_dataset(ambient, "pm25_pats_refugee_ambient")
shareable_ambient_path <- write_shareable_rds(shareable_ambient$data, "pm25_pats_refugee_ambient.rds")
entry_ambient <- make_inventory_entry(
  dataset_name = "pm25_pats_refugee_ambient",
  data = ambient,
  output_path = ambient_path,
  source_paths = clean_final_path(source_rel),
  removed_identifier_columns = deidentified_ambient$removed,
  shareable_output_path = shareable_ambient_path,
  shareable_removed_identifier_columns = shareable_ambient$removed,
  notes = paste(
    "Refugee ambient PATS+ PM2.5 data rebuilt from raw-first imports in 2_data_raw.",
    "Dedicated outdoor PM2.5 files are imported from ALL PM 2.5 Outdoor data; these files do not have household IDs, so hh_id and fcn_id are set to NA in the final ambient output.",
    "Household-folder files with school, mosque, mosjid, outside, outdoor, or close filename variants are excluded at import so they do not enter the indoor clean dataset.",
    "PM cleaning decisions implemented directly in this file include V_power filtering, dateTime rounding, manual site-name recodes, and PM_Estimate censoring to 10-30000 ug/m3.",
    "PM2.5 ambient timepoints are assigned only from dates parsed from filename fields/file_date_token using the PM-specific date windows, not from household source-folder labels.",
    "No host PATS+ output was created. HAPEX remains excluded and documented in hapex_exclusion_manifest.csv."
  )
)
update_inventory(entry_ambient)

pm_audit <- if (length(pm_audit_rows)) do.call(rbind, pm_audit_rows) else data.frame(stringsAsFactors = FALSE)
write.csv(pm_audit, pm_audit_path, row.names = FALSE, na = "")

write_cleaning_fix_log()

message("Wrote ", indoor_path)
message("Wrote ", shareable_indoor_path)
message("Wrote ", ambient_path)
message("Wrote ", shareable_ambient_path)
message("Wrote ", qc_path)
message("Wrote ", shareable_qc_path)
message("Wrote ", indoor_household_counts_path)
message("Wrote ", normalizePath(pm_audit_path, winslash = "/", mustWork = TRUE))
message("Wrote ", normalizePath(exact_duplicate_summary_path, winslash = "/", mustWork = TRUE))
message("Wrote ", pm_quality_outputs$by_file_path)
message("Wrote ", indoor_quality_outputs$by_file_path)
message("Wrote ", ambient_quality_outputs$by_file_path)
