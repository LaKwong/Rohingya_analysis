################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Audit active survey analysis identifiers
# @Description: Uses hh_id for HOST survey data and fcn_id for Rohingya refugee
#   survey data because refugee analysis files use fcn_id as the household key.
################################################################################

if (!exists("clean_final_project_root", mode = "function")) {
  helper_from_root <- file.path("3_data_cleaning", "fixed", "0_clean_helpers_20260805_2141.R")
  if (file.exists(helper_from_root)) {
    source(helper_from_root)
  } else {
    stop("Run from the Rohingya_analysis project root or source 0_clean_helpers_20260805_2141.R first.", call. = FALSE)
  }
}

run_date <- Sys.getenv("AUDIT_RUN_DATE", unset = format(Sys.Date(), "%Y%m%d"))
out_dir <- clean_final_path("3_data_cleaning", "Errors to resolve")
ensure_dir(out_dir)

survey_specs <- data.frame(
  dataset_id = c(
    "4_data/clean_final/survey_host_household.rds",
    "4_data/clean_final/survey_refugee_household.rds"
  ),
  community = c("host", "refugee"),
  identifier_field = c("hh_id", "fcn_id"),
  expected_pattern = c("^T0[0-9]{4}$", "^[0-9]{6}$"),
  pattern_description = c("T0 followed by four digits", "six digits"),
  row_unit_hint = c(
    "host household survey; hh_id is the household key",
    "refugee household survey; fcn_id is the analysis household key"
  ),
  stringsAsFactors = FALSE
)

clean_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NAN", "NULL", "null", "N/A", "n/a")] <- NA_character_
  x
}

collapse_unique <- function(x, max_n = 12L) {
  x <- unique(clean_chr(x))
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  if (length(x) > max_n) {
    paste0(paste(x[seq_len(max_n)], collapse = "; "), "; ... +", length(x) - max_n, " more")
  } else {
    paste(x, collapse = "; ")
  }
}

source_context <- function(df, rows) {
  cols <- intersect(
    c("raw_source_file", "raw_survey_version", "raw_collection_round", "raw_source_path", "source_file", "source_path"),
    names(df)
  )
  if (!length(cols) || !length(rows)) return(NA_character_)
  vals <- vapply(
    cols,
    function(col) {
      value <- collapse_unique(df[[col]][rows], 8L)
      if (is.na(value)) NA_character_ else paste0(col, "=", value)
    },
    character(1)
  )
  vals <- vals[!is.na(vals)]
  if (!length(vals)) NA_character_ else paste(vals, collapse = " | ")
}

bind_fill <- function(xs) {
  xs <- xs[vapply(xs, function(x) !is.null(x) && nrow(x) > 0L, logical(1))]
  if (!length(xs)) return(data.frame())
  cols <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    missing <- setdiff(cols, names(x))
    for (col in missing) x[[col]] <- NA_character_
    x[cols]
  })
  do.call(rbind, xs)
}

summaries <- list()
problems <- list()
duplicates <- list()

for (i in seq_len(nrow(survey_specs))) {
  spec <- survey_specs[i, ]
  path <- clean_final_path(spec$dataset_id)
  if (!file.exists(path)) {
    warning("Skipping missing dataset: ", spec$dataset_id, call. = FALSE)
    next
  }

  df <- readRDS(path)
  if (!is.data.frame(df)) {
    warning("Skipping non-data-frame RDS: ", spec$dataset_id, call. = FALSE)
    next
  }
  if (!spec$identifier_field %in% names(df)) {
    warning("Skipping ", spec$dataset_id, "; missing identifier field ", spec$identifier_field, call. = FALSE)
    next
  }

  id_value <- clean_chr(df[[spec$identifier_field]])
  n <- nrow(df)
  missing_id <- is.na(id_value)
  placeholder_id <- !missing_id & toupper(id_value) %in% c("0", "00", "000", "X", "XX", "NONE", "UNKNOWN", "UNK", ".", "-")
  malformed_id <- !missing_id & !placeholder_id & !grepl(spec$expected_pattern, id_value)

  duplicate_dataset_groups <- data.frame()
  duplicate_timepoint_groups <- data.frame()
  valid_idx <- which(!missing_id & !placeholder_id & !malformed_id)
  if (length(valid_idx)) {
    dataset_tab <- table(id_value[valid_idx])
    dataset_tab <- dataset_tab[dataset_tab > 1L]
    if (length(dataset_tab)) {
      duplicate_dataset_groups <- do.call(rbind, lapply(names(dataset_tab), function(id) {
        rows <- which(id_value == id)
        data.frame(
          dataset_id = spec$dataset_id,
          community = spec$community,
          identifier_field = spec$identifier_field,
          duplicate_level = "dataset",
          timepoint = NA_character_,
          identifier_value = id,
          n_rows = length(rows),
          row_indices_sample = paste(head(rows, 25L), collapse = ";"),
          raw_source_context = source_context(df, rows),
          stringsAsFactors = FALSE
        )
      }))
    }
  }

  duplicate_timepoint_rows <- rep(FALSE, n)
  if ("timepoint" %in% names(df)) {
    timepoint <- clean_chr(df$timepoint)
    ok <- !missing_id & !placeholder_id & !malformed_id & !is.na(timepoint)
    if (any(ok)) {
      key <- paste(timepoint[ok], id_value[ok], sep = "||")
      timepoint_tab <- table(key)
      timepoint_tab <- timepoint_tab[timepoint_tab > 1L]
      if (length(timepoint_tab)) {
        duplicate_timepoint_groups <- do.call(rbind, lapply(names(timepoint_tab), function(key_value) {
          parts <- strsplit(key_value, "\\|\\|")[[1]]
          rows <- which(timepoint == parts[1] & id_value == parts[2])
          duplicate_timepoint_rows[rows] <<- TRUE
          data.frame(
            dataset_id = spec$dataset_id,
            community = spec$community,
            identifier_field = spec$identifier_field,
            duplicate_level = "timepoint",
            timepoint = parts[1],
            identifier_value = parts[2],
            n_rows = length(rows),
            row_indices_sample = paste(head(rows, 25L), collapse = ";"),
            raw_source_context = source_context(df, rows),
            stringsAsFactors = FALSE
          )
        }))
      }
    }
  }

  duplicate_groups <- bind_fill(list(duplicate_dataset_groups, duplicate_timepoint_groups))
  if (nrow(duplicate_groups)) duplicates[[length(duplicates) + 1L]] <- duplicate_groups

  row_issue <- missing_id | placeholder_id | malformed_id | duplicate_timepoint_rows
  summaries[[length(summaries) + 1L]] <- data.frame(
    dataset_id = spec$dataset_id,
    community = spec$community,
    row_unit_hint = spec$row_unit_hint,
    identifier_field = spec$identifier_field,
    expected_pattern = spec$pattern_description,
    n_rows = n,
    n_missing_id = sum(missing_id),
    n_placeholder_id = sum(placeholder_id),
    n_malformed_id = sum(malformed_id),
    duplicate_id_groups_dataset = nrow(duplicate_dataset_groups),
    duplicate_id_groups_timepoint = nrow(duplicate_timepoint_groups),
    n_row_level_problem_rows = sum(row_issue),
    raw_sources_for_row_level_problems = source_context(df, which(row_issue)),
    raw_sources_for_dataset_duplicate_links = if (nrow(duplicate_dataset_groups)) source_context(df, which(id_value %in% duplicate_dataset_groups$identifier_value)) else NA_character_,
    stringsAsFactors = FALSE
  )

  if (any(row_issue)) {
    idx <- which(row_issue)
    issue_types <- vapply(idx, function(row) {
      flags <- c(
        missing_id = missing_id[row],
        placeholder_id = placeholder_id[row],
        malformed_id = malformed_id[row],
        duplicate_id_within_timepoint = duplicate_timepoint_rows[row]
      )
      paste(names(flags)[flags], collapse = ";")
    }, character(1))

    context_cols <- intersect(
      c(
        "timepoint", "study_arm", "study_arm_overall", "raw_source_file", "raw_survey_version",
        "raw_collection_round", "camp_id", "block_id", "subblock_id", "fcn_id", "hh_id",
        "hh_id_source", "hh_id_analysis_note", "KEY", "uuid"
      ),
      names(df)
    )
    problem_rows <- data.frame(
      dataset_id = spec$dataset_id,
      community = spec$community,
      row_unit_hint = spec$row_unit_hint,
      identifier_field = spec$identifier_field,
      row_index = idx,
      identifier_value = id_value[idx],
      issue_types = issue_types,
      raw_source_context = vapply(idx, function(row) source_context(df, row), character(1)),
      stringsAsFactors = FALSE
    )
    for (col in context_cols) problem_rows[[col]] <- as.character(df[[col]][idx])
    problems[[length(problems) + 1L]] <- problem_rows
  }
}

summary_df <- bind_fill(summaries)
problems_df <- bind_fill(problems)
duplicates_df <- bind_fill(duplicates)

summary_path <- file.path(out_dir, paste0("survey_unique_id_active_audit_summary_", run_date, ".csv"))
problems_path <- file.path(out_dir, paste0("survey_unique_id_active_problem_rows_", run_date, ".csv"))
duplicates_path <- file.path(out_dir, paste0("survey_unique_id_active_duplicate_groups_", run_date, ".csv"))
report_path <- file.path(out_dir, paste0("survey_unique_id_active_audit_report_", run_date, ".md"))

write.csv(summary_df, summary_path, row.names = FALSE, na = "")
write.csv(problems_df, problems_path, row.names = FALSE, na = "")
write.csv(duplicates_df, duplicates_path, row.names = FALSE, na = "")

report <- c(
  "# Active Survey Unique ID Audit",
  "",
  paste0("Run date: ", run_date),
  "",
  "Identifier rule:",
  "- HOST household survey: audit `hh_id`.",
  "- Rohingya refugee household survey: audit `fcn_id`, because refugee analysis files use `fcn_id` as the unique household identifier.",
  "- Dataset-level repeats across timepoints are expected for panel data; row-level duplicate problems are duplicates within the same timepoint.",
  "",
  "## Files Written",
  paste0("- `", summary_path, "`"),
  paste0("- `", problems_path, "`"),
  paste0("- `", duplicates_path, "`"),
  "",
  "## Dataset Summary"
)

if (nrow(summary_df)) {
  for (row in seq_len(nrow(summary_df))) {
    report <- c(
      report,
      "",
      paste0("### `", summary_df$dataset_id[row], "`"),
      paste0("- Identifier audited: `", summary_df$identifier_field[row], "` (", summary_df$expected_pattern[row], ")"),
      paste0("- Rows: ", summary_df$n_rows[row]),
      paste0("- Missing identifier: ", summary_df$n_missing_id[row]),
      paste0("- Placeholder identifier: ", summary_df$n_placeholder_id[row]),
      paste0("- Malformed identifier: ", summary_df$n_malformed_id[row]),
      paste0("- Duplicate groups across dataset: ", summary_df$duplicate_id_groups_dataset[row], " (expected when the same household appears at multiple timepoints)"),
      paste0("- Duplicate groups within timepoint: ", summary_df$duplicate_id_groups_timepoint[row]),
      paste0("- Row-level problem rows: ", summary_df$n_row_level_problem_rows[row])
    )
    if (!is.na(summary_df$raw_sources_for_row_level_problems[row])) {
      report <- c(report, paste0("- Raw sources for row-level problems: ", summary_df$raw_sources_for_row_level_problems[row]))
    }
  }
}

writeLines(report, report_path)

cat("summary ", summary_path, "\n", sep = "")
cat("problems ", problems_path, "\n", sep = "")
cat("duplicates ", duplicates_path, "\n", sep = "")
cat("report ", report_path, "\n", sep = "")
print(summary_df)