################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Final refugee household survey clean dataset
################################################################################

if (!exists("clean_final_project_root", mode = "function")) {
  helper_from_root <- file.path("3_data_cleaning", "fixed", "0_clean_final_helpers.R")
  if (file.exists(helper_from_root)) {
    source(helper_from_root)
  } else {
    stop("Run from the Rohingya_analysis project root or source 0_clean_final_helpers.R first.", call. = FALSE)
  }
}

source(clean_final_path("3_data_cleaning", "fixed", "legacy_refugee_manual_corrections.R"))

clean_refugee_exclusion_value <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[x %in% c("", "NA", "NAN", "NULL")] <- NA_character_
  x
}

append_refugee_exclusion_reason <- function(existing, reason) {
  ifelse(
    is.na(existing) | existing == "",
    reason,
    paste(existing, reason, sep = ";")
  )
}

write_refugee_household_exclusion_audit <- function(data, row_idx) {
  audit_path <- clean_final_path(
    "4_data",
    "clean_final",
    "survey_refugee_household_exclusion_audit.csv"
  )
  audit <- data.frame(
    row_index = row_idx,
    KEY = if ("KEY" %in% names(data)) data$KEY[row_idx] else NA_character_,
    raw_source_file = if ("raw_source_file" %in% names(data)) data$raw_source_file[row_idx] else NA_character_,
    timepoint = if ("timepoint" %in% names(data)) data$timepoint[row_idx] else NA_character_,
    study_arm = if ("study_arm" %in% names(data)) data$study_arm[row_idx] else NA_character_,
    fcn_id = if ("fcn_id" %in% names(data)) data$fcn_id[row_idx] else NA_character_,
    hh_id = if ("hh_id" %in% names(data)) data$hh_id[row_idx] else NA_character_,
    exclusion_reason = data$refugee_cleaning_exclusion_reason[row_idx],
    stringsAsFactors = FALSE
  )
  ensure_parent_dir(audit_path)
  write.csv(audit, audit_path, row.names = FALSE, na = "")
  audit_path
}
dataset_name <- "survey_refugee_household"
source_rel <- "4_data/clean_final/imported_raw/survey_refugee_household_raw.rds"
survey <- read_rds_required(source_rel)
apply_refugee_household_correction_workbook <- function(data) {
  workbook_rel <- file.path(
    "2_data_raw",
    "survey_baseline",
    "Rohingya HH Data_Correction_Saeed_20210124.xlsx"
  )
  workbook_path <- clean_final_path(workbook_rel)
  if (!file.exists(workbook_path)) {
    stop("Required household correction workbook is missing: ", workbook_path, call. = FALSE)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package `readxl` is required to apply household correction workbook.", call. = FALSE)
  }

  lookup <- as.data.frame(
    readxl::read_excel(
      workbook_path,
      sheet = "Rohingyafuelmaster_hh_data",
      col_types = "text"
    ),
    stringsAsFactors = FALSE
  )

  required <- c(
    "camp_id", "block_id", "subblock_id", "fcn_id", "Rand_nu", "hh_id",
    "name_hh_head", "name_respondent", "target_child_name"
  )
  missing_lookup <- setdiff(required, names(lookup))
  missing_data <- setdiff(required, names(data))
  if (length(missing_lookup) || length(missing_data)) {
    stop(
      "Cannot apply correction workbook; missing columns. ",
      "Workbook: ", paste(missing_lookup, collapse = ", "),
      "; data: ", paste(missing_data, collapse = ", "),
      call. = FALSE
    )
  }

  clean_key_value <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- gsub("\\s+", " ", x)
    x[x %in% c("", "NA", "NAN", "NULL")] <- NA_character_
    x
  }

  make_key <- function(df, cols) {
    do.call(
      paste,
      c(lapply(cols, function(col) clean_key_value(df[[col]])), sep = "||")
    )
  }

  match_unique_lookup <- function(df, lookup_df, cols, eligible = rep(TRUE, nrow(df))) {
    data_key <- make_key(df, cols)
    lookup_key <- make_key(lookup_df, cols)
    lookup_counts <- table(lookup_key, useNA = "no")
    unique_lookup_keys <- names(lookup_counts)[lookup_counts == 1L]
    idx <- match(data_key, lookup_key)
    idx[!(data_key %in% unique_lookup_keys) | !eligible] <- NA_integer_
    idx
  }

  primary_key_cols <- c(
    "camp_id", "block_id", "subblock_id", "fcn_id",
    "name_hh_head", "name_respondent", "target_child_name"
  )
  primary_idx <- match_unique_lookup(data, lookup, primary_key_cols)

  fcn_clean <- clean_key_value(data$fcn_id)
  missing_or_placeholder_fcn <- is.na(fcn_clean) | fcn_clean %in% c("X", "0", "00")
  secondary_key_cols <- c(
    "camp_id", "block_id", "subblock_id",
    "name_hh_head", "name_respondent", "target_child_name"
  )
  secondary_idx <- match_unique_lookup(
    data,
    lookup,
    secondary_key_cols,
    eligible = is.na(primary_idx) & missing_or_placeholder_fcn
  )

  lookup_idx <- primary_idx
  lookup_idx[is.na(lookup_idx)] <- secondary_idx[is.na(lookup_idx)]
  matched <- !is.na(lookup_idx)

  corrected <- data
  correction_cols <- c("camp_id", "block_id", "subblock_id", "fcn_id", "Rand_nu", "hh_id")
  audit <- data.frame(
    row_index = which(matched),
    match_rule = ifelse(!is.na(primary_idx[matched]), "primary_full_id_name_key", "secondary_missing_fcn_name_key"),
    raw_source_file = if ("raw_source_file" %in% names(data)) data$raw_source_file[matched] else NA_character_,
    KEY = if ("KEY" %in% names(data)) data$KEY[matched] else NA_character_,
    stringsAsFactors = FALSE
  )

  changed_any <- rep(FALSE, sum(matched))
  for (col in correction_cols) {
    old <- as.character(data[[col]][matched])
    new <- as.character(lookup[[col]][lookup_idx[matched]])
    changed <- ifelse(is.na(old), "<NA>", old) != ifelse(is.na(new), "<NA>", new)
    audit[[paste0(col, "_old")]] <- old
    audit[[paste0(col, "_corrected")]] <- new
    audit[[paste0(col, "_changed")]] <- changed
    changed_any <- changed_any | changed
    corrected[[col]][matched] <- new
  }

  audit <- audit[changed_any, , drop = FALSE]
  audit$correction_workbook <- normalizePath(workbook_path, winslash = "/", mustWork = TRUE)
  audit$correction_sheet <- "Rohingyafuelmaster_hh_data"

  audit_path <- clean_final_path(
    "4_data",
    "clean_final",
    "survey_refugee_household_correction_audit.csv"
  )
  ensure_parent_dir(audit_path)
  write.csv(audit, audit_path, row.names = FALSE, na = "")

  corrected
}


apply_refugee_endline_review_corrections <- function(data, identity_data = data) {
  review_dir <- clean_final_path("2_data_raw", "survey_endline_data review")
  if (!dir.exists(review_dir)) {
    stop("Required endline review folder is missing: ", review_dir, call. = FALSE)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package `readxl` is required to apply endline review corrections.", call. = FALSE)
  }
  if (nrow(data) != nrow(identity_data)) {
    stop("Endline correction identity data must have the same row count as data.", call. = FALSE)
  }
  if (!"raw_source_file" %in% names(identity_data)) {
    stop("Cannot apply endline review corrections; raw_source_file is missing.", call. = FALSE)
  }

  clean_text <- function(x) {
    x <- trimws(as.character(x))
    x[x %in% c("", "NA", "NaN", "NAN", "NULL", "null")] <- NA_character_
    x
  }

  normalize_id <- function(x) {
    x <- clean_text(x)
    x <- toupper(gsub("\\s+", "", x))
    x[x %in% c("", "NA", "NAN", "NULL")] <- NA_character_
    x
  }

  parse_review_row <- function(x) {
    x <- clean_text(x)
    value <- suppressWarnings(as.numeric(x))
    out <- rep(NA_integer_, length(value))
    ok <- !is.na(value) & is.finite(value) & abs(value - round(value)) < 1e-6
    out[ok] <- as.integer(round(value[ok]))
    out
  }

  get_first_col <- function(df, candidates) {
    exact <- candidates[candidates %in% names(df)]
    if (length(exact)) return(exact[1])
    lower_names <- tolower(names(df))
    for (candidate in tolower(candidates)) {
      idx <- match(candidate, lower_names)
      if (!is.na(idx)) return(names(df)[idx])
    }
    NA_character_
  }

  get_cell <- function(df, col, row) {
    if (is.na(col) || !col %in% names(df)) return(NA_character_)
    clean_text(df[[col]][row])
  }

  valid_review_id <- function(x, label) {
    x <- normalize_id(x)
    !is.na(x) & !(x %in% toupper(label))
  }

  find_data_col <- function(qid) {
    qid <- clean_text(qid)[1]
    if (is.na(qid)) return(NA_character_)
    idx <- match(qid, names(data))
    if (!is.na(idx)) return(names(data)[idx])
    idx <- match(tolower(qid), tolower(names(data)))
    if (!is.na(idx)) return(names(data)[idx])
    NA_character_
  }

  numeric_like <- function(x) {
    x <- clean_text(x)
    !is.na(x) & grepl("^[-+]?[0-9]+(\\.[0-9]+)?$", x)
  }

  review_note_response <- function(value) {
    value <- tolower(clean_text(value))
    if (is.na(value)) return(FALSE)
    grepl(
      paste(
        c(
          "these are fine", "do not show", "is not other", "problem fixed",
          "skip this row", "it is correct", "yes correct", "redo the survey",
          "please check", "sorry for mistake", "one means"
        ),
        collapse = "|"
      ),
      value
    )
  }

  source_row_number <- ave(
    seq_len(nrow(identity_data)),
    identity_data$raw_source_file,
    FUN = seq_along
  )

  audit_rows <- list()
  add_audit <- function(
    workbook, sheet, review_row_index, source_file, raw_row_number, qid, variable,
    match_status, skip_reason, old_value, corrected_value, original_response,
    response, resolution, remarks, change_made, notes
  ) {
    audit_rows[[length(audit_rows) + 1L]] <<- data.frame(
      correction_workbook = normalizePath(workbook, winslash = "/", mustWork = TRUE),
      correction_sheet = sheet,
      review_row_index = review_row_index,
      raw_source_file = source_file,
      raw_row_number = raw_row_number,
      qid = qid,
      variable = variable,
      match_status = match_status,
      skip_reason = skip_reason,
      old_value = old_value,
      corrected_value = corrected_value,
      original_response = original_response,
      response = response,
      resolution = resolution,
      remarks = remarks,
      change_made = change_made,
      notes = notes,
      stringsAsFactors = FALSE
    )
  }

  match_target_rows <- function(source_file, review_row_number, review_hh_id, review_fcn_id) {
    in_source <- identity_data$raw_source_file == source_file
    if (!is.na(review_row_number)) {
      idx <- which(in_source & source_row_number == review_row_number)
      return(idx)
    }

    idx <- which(in_source)
    if (length(idx) && "hh_id" %in% names(identity_data) && valid_review_id(review_hh_id, "hh_id")) {
      idx <- idx[normalize_id(identity_data$hh_id[idx]) == normalize_id(review_hh_id)]
    }
    if (length(idx) && "fcn_id" %in% names(identity_data) && valid_review_id(review_fcn_id, "fcn_id")) {
      idx <- idx[normalize_id(identity_data$fcn_id[idx]) == normalize_id(review_fcn_id)]
    }
    idx
  }

  identity_matches <- function(row_idx, review_hh_id, review_fcn_id) {
    checks <- logical(0)
    if ("hh_id" %in% names(identity_data) && valid_review_id(review_hh_id, "hh_id")) {
      checks <- c(checks, normalize_id(identity_data$hh_id[row_idx]) == normalize_id(review_hh_id))
    }
    if ("fcn_id" %in% names(identity_data) && valid_review_id(review_fcn_id, "fcn_id")) {
      checks <- c(checks, normalize_id(identity_data$fcn_id[row_idx]) == normalize_id(review_fcn_id))
    }
    if (!length(checks)) return(TRUE)
    any(checks, na.rm = TRUE)
  }

  review_files <- list.files(review_dir, pattern = "\\.xlsx$", full.names = TRUE)
  corrected <- data

  for (workbook_path in review_files) {
    workbook_name <- basename(workbook_path)
    version <- regmatches(workbook_name, regexpr("v[0-9]+", workbook_name, ignore.case = TRUE))
    version <- if (length(version)) tolower(version) else NA_character_
    source_file <- if (!is.na(version)) paste0("rohingya_fuel_", version, ".csv") else NA_character_

    sheets <- readxl::excel_sheets(workbook_path)
    if (identical(version, "v119")) {
      add_audit(
        workbook_path, NA_character_, NA_integer_, source_file, NA_integer_, NA_character_,
        NA_character_, "skipped", "host_review_workbook_not_applied_to_refugee_cleaner",
        NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
        NA_character_, NA_character_, NA_character_
      )
      next
    }

    if (!"review" %in% sheets) {
      note_sheet <- sheets[1]
      notes <- as.data.frame(
        readxl::read_excel(workbook_path, sheet = note_sheet, col_types = "text", .name_repair = "unique"),
        stringsAsFactors = FALSE
      )
      if (!nrow(notes)) {
        add_audit(
          workbook_path, note_sheet, NA_integer_, source_file, NA_integer_, NA_character_,
          NA_character_, "skipped", "no_structured_review_sheet",
          NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
          NA_character_, NA_character_, NA_character_
        )
        next
      }
      for (i in seq_len(nrow(notes))) {
        note_text <- paste(clean_text(unlist(notes[i, , drop = TRUE])), collapse = " | ")
        if (is.na(note_text) || !nzchar(note_text)) next
        add_audit(
          workbook_path, note_sheet, i, source_file, NA_integer_, NA_character_,
          NA_character_, "skipped", "unstructured_note_needs_manual_review",
          NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
          NA_character_, NA_character_, note_text
        )
      }
      next
    }

    review <- as.data.frame(
      readxl::read_excel(workbook_path, sheet = "review", col_types = "text", .name_repair = "unique"),
      stringsAsFactors = FALSE
    )
    if (!nrow(review)) next

    row_col <- get_first_col(review, "Row")
    qid_col <- get_first_col(review, "QID")
    hh_col <- get_first_col(review, "hh_id")
    fcn_col <- get_first_col(review, "fcn_id")
    original_col <- get_first_col(review, "original_response")
    response_col <- get_first_col(review, c("Response", "Responses"))
    resolution_col <- get_first_col(review, "Resolution")
    remarks_col <- get_first_col(review, "Remarks")
    change_col <- get_first_col(review, "Change Made")
    notes_col <- get_first_col(review, "Notes")

    for (i in seq_len(nrow(review))) {
      qid <- get_cell(review, qid_col, i)
      variable <- find_data_col(qid)
      review_row_number <- parse_review_row(get_cell(review, row_col, i))
      review_hh_id <- get_cell(review, hh_col, i)
      review_fcn_id <- get_cell(review, fcn_col, i)
      original_response <- get_cell(review, original_col, i)
      response <- get_cell(review, response_col, i)
      resolution <- get_cell(review, resolution_col, i)
      remarks <- get_cell(review, remarks_col, i)
      change_made <- get_cell(review, change_col, i)
      notes <- get_cell(review, notes_col, i)
      candidate <- if (!is.na(resolution)) resolution else response
      has_resolution <- !is.na(resolution)

      if (is.na(qid)) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "skipped", "missing_qid", NA_character_, candidate, original_response, response, resolution, remarks, change_made, notes)
        next
      }
      if (is.na(variable)) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "skipped", "qid_not_found_in_clean_data", NA_character_, candidate, original_response, response, resolution, remarks, change_made, notes)
        next
      }
      if (is.na(candidate)) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "skipped", "missing_correction_value", NA_character_, candidate, original_response, response, resolution, remarks, change_made, notes)
        next
      }

      target_rows <- match_target_rows(source_file, review_row_number, review_hh_id, review_fcn_id)
      if (length(target_rows) != 1L) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "skipped", paste0("target_row_count_", length(target_rows)), NA_character_, candidate, original_response, response, resolution, remarks, change_made, notes)
        next
      }
      target_row <- target_rows[1]
      if (!identity_matches(target_row, review_hh_id, review_fcn_id)) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "skipped", "row_identity_mismatch", as.character(corrected[[variable]][target_row]), candidate, original_response, response, resolution, remarks, change_made, notes)
        next
      }

      old_value <- as.character(corrected[[variable]][target_row])
      if (!has_resolution && review_note_response(candidate)) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "skipped", "response_is_review_note_not_data_value", old_value, candidate, original_response, response, resolution, remarks, change_made, notes)
        next
      }
      if (!has_resolution && numeric_like(old_value) && grepl("[A-Za-z]", candidate) && (grepl("\\s", candidate) || nchar(candidate) > 8L)) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "skipped", "non_numeric_response_for_numeric_field", old_value, candidate, original_response, response, resolution, remarks, change_made, notes)
        next
      }

      if (
        numeric_like(old_value) && numeric_like(candidate) &&
          isTRUE(all.equal(as.numeric(old_value), as.numeric(candidate)))
      ) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "unchanged_numeric_equivalent", NA_character_, old_value, candidate, original_response, response, resolution, remarks, change_made, notes)
      } else if (identical(ifelse(is.na(old_value), "<NA>", old_value), ifelse(is.na(candidate), "<NA>", candidate))) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "unchanged_already_correct", NA_character_, old_value, candidate, original_response, response, resolution, remarks, change_made, notes)
      } else {
        corrected[[variable]][target_row] <- candidate
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "applied", NA_character_, old_value, candidate, original_response, response, resolution, remarks, change_made, notes)
      }
    }
  }

  audit <- if (length(audit_rows)) {
    do.call(rbind, audit_rows)
  } else {
    data.frame(stringsAsFactors = FALSE)
  }
  audit_path <- clean_final_path(
    "4_data",
    "clean_final",
    "survey_refugee_household_endline_review_correction_audit.csv"
  )
  ensure_parent_dir(audit_path)
  write.csv(audit, audit_path, row.names = FALSE, na = "")

  corrected
}

survey <- survey[survey$community == "refugee", , drop = FALSE]
if ("study_arm_overall" %in% names(survey)) {
  survey <- survey[is.na(survey$study_arm_overall) | !(survey$study_arm_overall %in% c("host", "host_in_refugee_form")), , drop = FALSE]
}

survey$refugee_cleaning_exclusion_reason <- NA_character_
if ("hh_id" %in% names(survey)) {
  # Drop this unresolved row: the correct hh_id and fcn_id could not be determined.
  bad_hh_id_idx <- which(clean_refugee_exclusion_value(survey$hh_id) == "8WDI21X")
  if (length(bad_hh_id_idx)) {
    survey$refugee_cleaning_exclusion_reason[bad_hh_id_idx] <- append_refugee_exclusion_reason(
      survey$refugee_cleaning_exclusion_reason[bad_hh_id_idx],
      "drop_8wDI21x_correct_hh_id_and_fcn_id_could_not_be_determined"
    )
  }
}
survey_before_endline_review_identity <- survey
survey <- apply_refugee_household_correction_workbook(survey)
survey <- apply_refugee_endline_review_corrections(survey, identity_data = survey_before_endline_review_identity)
survey <- apply_refugee_legacy_manual_corrections(survey)

survey$community <- "refugee"
survey$data_type <- "household_survey"

recoded <- recode_timepoint_by_timestamp(
  survey,
  date_cols = c("collection_date", "start_date", "SubmissionDate", "starttime", "endtime", "date", "datetime"),
  dataset_name = dataset_name
)
survey <- recoded$data
if (!"fcn_id" %in% names(survey)) survey$fcn_id <- NA_character_
missing_fcn_idx <- which(is.na(clean_refugee_exclusion_value(survey$fcn_id)))
if (length(missing_fcn_idx)) {
  survey$refugee_cleaning_exclusion_reason[missing_fcn_idx] <- append_refugee_exclusion_reason(
    survey$refugee_cleaning_exclusion_reason[missing_fcn_idx],
    "missing_fcn_id_declined_participation"
  )
}

# For duplicate refugee endline fcn_id values, keep the first completed survey
# and arbitrarily drop the second survey from the cleaned dataset.
duplicate_endline_drop_idx <- integer(0)
if (all(c("timepoint", "fcn_id") %in% names(survey))) {
  eligible_endline_idx <- which(
    survey$timepoint == "endline" &
      is.na(survey$refugee_cleaning_exclusion_reason) &
      !is.na(clean_refugee_exclusion_value(survey$fcn_id))
  )
  if (length(eligible_endline_idx)) {
    fcn_endline <- clean_refugee_exclusion_value(survey$fcn_id[eligible_endline_idx])
    duplicate_fcn <- names(table(fcn_endline))[table(fcn_endline) > 1L]
    if (length(duplicate_fcn)) {
      completion_date <- extract_date_any(survey$collection_date)
      if (all(is.na(completion_date)) && "start_date" %in% names(survey)) {
        completion_date <- extract_date_any(survey$start_date)
      }
      for (fcn in duplicate_fcn) {
        group_idx <- eligible_endline_idx[fcn_endline == fcn]
        group_order <- order(
          completion_date[group_idx],
          group_idx,
          na.last = TRUE
        )
        drop_idx <- group_idx[group_order[-1L]]
        duplicate_endline_drop_idx <- c(duplicate_endline_drop_idx, drop_idx)
      }
    }
  }
}
if (length(duplicate_endline_drop_idx)) {
  survey$refugee_cleaning_exclusion_reason[duplicate_endline_drop_idx] <- append_refugee_exclusion_reason(
    survey$refugee_cleaning_exclusion_reason[duplicate_endline_drop_idx],
    "duplicate_endline_fcn_id_arbitrarily_drop_second_survey_from_cleaned_dataset"
  )
}

duplicate_endline_drop_count <- length(duplicate_endline_drop_idx)
excluded_refugee_idx <- which(!is.na(survey$refugee_cleaning_exclusion_reason))
excluded_refugee_count <- length(excluded_refugee_idx)
refugee_exclusion_audit_path <- write_refugee_household_exclusion_audit(survey, excluded_refugee_idx)
if (excluded_refugee_count) {
  survey <- survey[-excluded_refugee_idx, , drop = FALSE]
}
survey$refugee_cleaning_exclusion_reason <- NULL
write_timepoint_summary(make_timepoint_summary(survey, dataset_name), dataset_name)

deidentified <- drop_identifier_columns(survey)
survey <- deidentified$data
survey <- move_columns_first(
  survey,
  c(
    "community", "data_type", "fcn_id", "hh_id", "unique_id", "uuid",
    "timepoint", "timepoint_original", "collection_date", "collection_year",
    "timepoint_source_col", "raw_collection_round", "raw_survey_version",
    "raw_source_file", "study_arm_overall", "study_arm"
  )
)

output_path <- write_final_rds(survey, "4_data/clean_final/survey_refugee_household.rds")

entry <- make_inventory_entry(
  dataset_name = dataset_name,
  data = survey,
  output_path = output_path,
  source_paths = clean_final_path(source_rel),
  removed_identifier_columns = deidentified$removed,
  notes = paste(
    "Refugee household survey final dataset rebuilt from raw-first imports in 2_data_raw.",
    "Baseline household IDs were corrected using 2_data_raw/survey_baseline/Rohingya HH Data_Correction_Saeed_20210124.xlsx; manual legacy corrections from 3_data_cleaning/1_clean_Rohingya_data.R were ported using intervention/comparison study_arm names; structured endline review corrections were applied from 2_data_raw/survey_endline_data review.",
    "Includes 2022 refugee survey files from survey_endline and 2020 midline files from survey_midline despite misleading endline filenames.",
    "Timepoint was recoded from parsed collection timestamps using inclusive windows: baseline 2019-09-01 to 2020-04-15; midline 2020-09-01 to 2020-12-15; endline 2022-01-15 to 2022-08-15.",
    paste0("Excluded refugee household rows with missing fcn_id, unresolved 8wDI21x hh_id, or duplicate endline fcn_id second surveys: ", excluded_refugee_count, ". Duplicate endline second surveys arbitrarily dropped: ", duplicate_endline_drop_count, ". Audit: ", refugee_exclusion_audit_path, ".")
  )
)
update_inventory(entry)
write_cleaning_fix_log()

message("Wrote ", output_path)
