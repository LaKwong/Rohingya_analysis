################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Final host household survey clean dataset
################################################################################

if (!exists("clean_final_project_root", mode = "function")) {
  helper_from_root <- file.path("3_data_cleaning", "fixed", "0_clean_helpers_20260805_2141.R")
  if (file.exists(helper_from_root)) {
    source(helper_from_root)
  } else {
    stop("Run from the Rohingya_analysis project root or source 0_clean_helpers_20260805_2141.R first.", call. = FALSE)
  }
}

dataset_name <- "survey_host_household"
source_rel <- "4_data/clean_final/imported_raw/survey_host_household_raw.rds"
survey <- read_rds_required(source_rel)

survey <- survey[survey$community == "host", , drop = FALSE]
survey$community <- "host"
survey$data_type <- "household_survey"
survey$study_arm_overall <- "host"

if ("hh_id" %in% names(survey)) {
  if (!"hh_id_original" %in% names(survey)) {
    survey$hh_id_original <- as_clean_character(survey$hh_id)
  }
  survey$hh_id <- NULL
}

extract_host_t0_id <- function(x) {
  x <- as_clean_character(x)
  out <- rep(NA_character_, length(x))
  matched <- regexec("(?i)^\\s*T.*?([0-9]{4})\\s*$", x, perl = TRUE)
  parts <- regmatches(x, matched)
  has_match <- lengths(parts) >= 2L
  out[has_match] <- paste0(
    "T0",
    vapply(parts[has_match], function(value) value[2], character(1))
  )
  out
}

append_issue <- function(existing, new_issue) {
  ifelse(
    is.na(existing) | existing == "",
    new_issue,
    paste(existing, new_issue, sep = ";")
  )
}

clean_host_link_text <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(trimws(x))
  x[x %in% c("", "na", "nan", "n/a", "null")] <- NA_character_
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\s+", " ", trimws(x))
  x[x == ""] <- NA_character_
  x
}

clean_host_link_id <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(trimws(x))
  x[x %in% c("", "na", "nan", "n/a", "null")] <- NA_character_
  x <- gsub("[^a-z0-9]+", "", x)
  x <- gsub("^0+([0-9]+)$", "\\1", x)
  x[x == ""] <- NA_character_
  x
}

host_similarity_matrix <- function(a, b) {
  a0 <- ifelse(is.na(a), "", a)
  b0 <- ifelse(is.na(b), "", b)
  distance <- adist(a0, b0, partial = FALSE, ignore.case = TRUE)
  denominator <- outer(nchar(a0), nchar(b0), pmax)
  denominator[denominator == 0] <- 1
  similarity <- 1 - distance / denominator
  similarity[outer(is.na(a) | a == "", is.na(b) | b == "", `|`)] <- NA_real_
  similarity
}

host_exact_matrix <- function(a, b) {
  outer(
    a,
    b,
    Vectorize(function(x, y) !is.na(x) && !is.na(y) && x == y)
  )
}

write_host_match_csv <- function(data, filename) {
  out_path <- clean_final_path("3_data_cleaning", "Errors to resolve", filename)
  ensure_parent_dir(out_path)
  write.csv(data, out_path, row.names = FALSE, na = "")
  out_path
}

match_endline_host_hh_ids <- function(survey) {
  required_cols <- c(
    "timepoint", "hh_id", "name_respondent", "name_hh_head", "village_id",
    "ward_id", "house_id", "serial_id", "nat_id"
  )
  missing_cols <- setdiff(required_cols, names(survey))
  for (col in missing_cols) survey[[col]] <- NA_character_

  baseline_idx <- which(survey$timepoint == "baseline" & !is.na(survey$hh_id))
  endline_missing_idx <- which(survey$timepoint == "endline" & is.na(survey$hh_id))

  empty_candidate_cols <- c(
    "end_row", "base_row", "candidate_hh_id", "rank", "score", "score_gap",
    "name_resp_sim", "name_head_sim", "name_best_sim", "village_sim",
    "ward_exact", "house_or_serial_exact", "nat_id_exact"
  )

  if (!length(baseline_idx) || !length(endline_missing_idx)) {
    empty <- as.data.frame(setNames(replicate(length(empty_candidate_cols), logical(0), simplify = FALSE), empty_candidate_cols))
    return(list(data = survey, candidates = empty, accepted = empty, unresolved = empty))
  }

  base <- survey[baseline_idx, , drop = FALSE]
  endline <- survey[endline_missing_idx, , drop = FALSE]

  base$name_respondent_clean <- clean_host_link_text(base$name_respondent)
  base$name_hh_head_clean <- clean_host_link_text(base$name_hh_head)
  base$village_id_clean <- clean_host_link_text(base$village_id)
  base$ward_id_clean <- clean_host_link_id(base$ward_id)
  base$house_id_clean <- clean_host_link_id(base$house_id)
  base$serial_id_clean <- clean_host_link_id(base$serial_id)
  base$nat_id_clean <- clean_host_link_id(base$nat_id)

  endline$name_respondent_clean <- clean_host_link_text(endline$name_respondent)
  endline$village_id_clean <- clean_host_link_text(endline$village_id)
  endline$ward_id_clean <- clean_host_link_id(endline$ward_id)
  endline$house_id_clean <- clean_host_link_id(endline$house_id)
  endline$serial_id_clean <- clean_host_link_id(endline$serial_id)
  endline$nat_id_clean <- clean_host_link_id(endline$nat_id)

  name_resp_sim <- host_similarity_matrix(endline$name_respondent_clean, base$name_respondent_clean)
  name_head_sim <- host_similarity_matrix(endline$name_respondent_clean, base$name_hh_head_clean)
  name_best_sim <- pmax(name_resp_sim, name_head_sim, na.rm = TRUE)
  name_best_sim[!is.finite(name_best_sim)] <- NA_real_
  village_sim <- host_similarity_matrix(endline$village_id_clean, base$village_id_clean)
  ward_exact <- host_exact_matrix(endline$ward_id_clean, base$ward_id_clean)
  nat_id_exact <- host_exact_matrix(endline$nat_id_clean, base$nat_id_clean)

  house_or_serial_exact <- outer(
    seq_len(nrow(endline)),
    seq_len(nrow(base)),
    Vectorize(function(i, j) {
      endline_values <- c(endline$house_id_clean[i], endline$serial_id_clean[i])
      baseline_values <- c(base$house_id_clean[j], base$serial_id_clean[j])
      endline_values <- endline_values[!is.na(endline_values) & endline_values != ""]
      baseline_values <- baseline_values[!is.na(baseline_values) & baseline_values != ""]
      length(intersect(endline_values, baseline_values)) > 0
    })
  )

  score <- matrix(0, nrow = nrow(endline), ncol = nrow(base))
  score <- score + ifelse(is.na(name_best_sim), 0, 50 * name_best_sim)
  score <- score + ifelse(is.na(village_sim), 0, 20 * village_sim)
  score <- score + ifelse(ward_exact, 10, 0)
  score <- score + ifelse(house_or_serial_exact, 20, 0)
  score <- score + ifelse(nat_id_exact, 50, 0)
  score <- score + ifelse(!is.na(name_best_sim) & name_best_sim >= 0.92, 15, 0)
  score <- score + ifelse(!is.na(village_sim) & village_sim >= 0.85, 5, 0)

  rows <- vector("list", nrow(endline) * 3L)
  row_counter <- 1L
  for (i in seq_len(nrow(endline))) {
    ranked <- order(score[i, ], decreasing = TRUE)[seq_len(min(3L, nrow(base)))]
    score_gap <- if (length(ranked) >= 2L) score[i, ranked[1L]] - score[i, ranked[2L]] else NA_real_
    for (rank in seq_along(ranked)) {
      j <- ranked[rank]
      rows[[row_counter]] <- data.frame(
        end_row = endline_missing_idx[i],
        base_row = baseline_idx[j],
        candidate_hh_id = base$hh_id[j],
        rank = rank,
        score = score[i, j],
        score_gap = score_gap,
        name_resp_sim = name_resp_sim[i, j],
        name_head_sim = name_head_sim[i, j],
        name_best_sim = name_best_sim[i, j],
        village_sim = village_sim[i, j],
        ward_exact = ward_exact[i, j],
        house_or_serial_exact = house_or_serial_exact[i, j],
        nat_id_exact = nat_id_exact[i, j],
        stringsAsFactors = FALSE
      )
      row_counter <- row_counter + 1L
    }
  }

  candidates <- do.call(rbind, rows)
  top1 <- candidates[candidates$rank == 1L, , drop = FALSE]
  accepted_rule <- with(
    top1,
    (nat_id_exact & score_gap >= 10 & (ward_exact | house_or_serial_exact | village_sim >= 0.80)) |
      (score_gap >= 10 & ward_exact & name_best_sim >= 0.92 & (house_or_serial_exact | village_sim >= 0.80)) |
      (score_gap >= 8 & ward_exact & house_or_serial_exact & name_best_sim >= 0.75 & village_sim >= 0.75) |
      (score_gap >= 10 & house_or_serial_exact & name_best_sim >= 0.92 & village_sim >= 0.80)
  )
  accepted <- top1[accepted_rule, , drop = FALSE]

  # Keep one endline assignment per baseline hh_id, favoring the strongest score/gap.
  accepted <- accepted[order(accepted$candidate_hh_id, -accepted$score, -accepted$score_gap), , drop = FALSE]
  accepted <- accepted[!duplicated(accepted$candidate_hh_id), , drop = FALSE]
  accepted <- accepted[order(accepted$end_row), , drop = FALSE]
  unresolved <- top1[!(top1$end_row %in% accepted$end_row), , drop = FALSE]

  if (nrow(accepted)) {
    survey$hh_id[accepted$end_row] <- accepted$candidate_hh_id
    survey$hh_id_source[accepted$end_row] <- "matched_baseline_host_endline"
    survey$hh_id_analysis_note[accepted$end_row] <- "hh_id matched to baseline HOST record using identifying fields"
    survey$hh_id_match_base_row[accepted$end_row] <- accepted$base_row
    survey$hh_id_match_score[accepted$end_row] <- accepted$score
    survey$hh_id_match_gap[accepted$end_row] <- accepted$score_gap
  }

  list(
    data = survey,
    candidates = candidates,
    accepted = accepted,
    unresolved = unresolved
  )
}

if (!"house_id" %in% names(survey)) survey$house_id <- NA_character_
if (!"serial_id" %in% names(survey)) survey$serial_id <- NA_character_

house_id_t0 <- extract_host_t0_id(survey$house_id)
serial_id_t0 <- extract_host_t0_id(survey$serial_id)
survey$hh_id <- ifelse(!is.na(house_id_t0), house_id_t0, serial_id_t0)
survey$hh_id_source <- ifelse(
  !is.na(house_id_t0),
  "house_id",
  ifelse(!is.na(serial_id_t0), "serial_id", NA_character_)
)
survey$hh_id_analysis_note <- NA_character_
survey$hh_id_match_base_row <- NA_integer_
survey$hh_id_match_score <- NA_real_
survey$hh_id_match_gap <- NA_real_

recoded <- recode_timepoint_by_timestamp(
  survey,
  date_cols = c("collection_date", "start_date", "SubmissionDate", "starttime", "endtime", "date", "datetime"),
  dataset_name = dataset_name
)
survey <- recoded$data

baseline_host_idx <- which(survey$timepoint == "baseline")
manual_row_98_idx <- 98L
baseline_generated_note <- "hh_id made up for purposes of analysis"

if (manual_row_98_idx %in% baseline_host_idx) {
  survey$hh_id[manual_row_98_idx] <- "T09001"
  survey$hh_id_source[manual_row_98_idx] <- "assigned_baseline_host_analysis"
  survey$hh_id_analysis_note[manual_row_98_idx] <- baseline_generated_note
} else {
  warning(
    "Requested host row 98 was not classified as baseline, so it was not relabeled to T09001.",
    call. = FALSE
  )
}

missing_baseline_idx <- which(survey$timepoint == "baseline" & is.na(survey$hh_id))
if (length(missing_baseline_idx) > 0L) {
  generated_ids <- sprintf("T09%03d", seq.int(2L, length.out = length(missing_baseline_idx)))
  conflict_idx <- which(
    survey$hh_id %in% generated_ids &
      !(seq_len(nrow(survey)) %in% missing_baseline_idx)
  )
  if (length(conflict_idx) > 0L) {
    stop(
      "Generated baseline host hh_id values conflict with existing hh_id values: ",
      paste(unique(survey$hh_id[conflict_idx]), collapse = ", "),
      call. = FALSE
    )
  }
  survey$hh_id[missing_baseline_idx] <- generated_ids
  survey$hh_id_source[missing_baseline_idx] <- "assigned_missing_baseline_host_analysis"
  survey$hh_id_analysis_note[missing_baseline_idx] <- baseline_generated_note
}

endline_match <- match_endline_host_hh_ids(survey)
survey <- endline_match$data
write_host_match_csv(
  endline_match$candidates,
  "host_endline_missing_hh_id_candidate_matches_20260717.csv"
)
write_host_match_csv(
  endline_match$accepted,
  "host_endline_hh_id_accepted_matches_20260717.csv"
)
write_host_match_csv(
  endline_match$unresolved,
  "host_endline_hh_id_unresolved_candidate_matches_20260717.csv"
)

missing_hh_id_count <- sum(is.na(survey$hh_id))
duplicate_hh_id_count <- sum(duplicated(survey$hh_id[!is.na(survey$hh_id)]))
generated_baseline_hh_id_count <- sum(survey$hh_id_analysis_note == baseline_generated_note, na.rm = TRUE)
matched_endline_hh_id_count <- sum(survey$hh_id_source == "matched_baseline_host_endline", na.rm = TRUE)

host_hh_id_audit <- data.frame(
  row_index = seq_len(nrow(survey)),
  raw_source_file = if ("raw_source_file" %in% names(survey)) survey$raw_source_file else NA_character_,
  KEY = if ("KEY" %in% names(survey)) survey$KEY else NA_character_,
  timepoint = if ("timepoint" %in% names(survey)) survey$timepoint else NA_character_,
  hh_id_original = if ("hh_id_original" %in% names(survey)) survey$hh_id_original else NA_character_,
  house_id = as_clean_character(survey$house_id),
  serial_id = as_clean_character(survey$serial_id),
  house_id_t0 = extract_host_t0_id(survey$house_id),
  serial_id_t0 = extract_host_t0_id(survey$serial_id),
  hh_id = survey$hh_id,
  hh_id_source = survey$hh_id_source,
  hh_id_analysis_note = survey$hh_id_analysis_note,
  hh_id_match_base_row = survey$hh_id_match_base_row,
  hh_id_match_score = survey$hh_id_match_score,
  hh_id_match_gap = survey$hh_id_match_gap,
  issue = ifelse(is.na(survey$hh_id), "missing_no_t0_token_in_house_id_or_serial_id", NA_character_),
  stringsAsFactors = FALSE
)

analysis_idx <- which(!is.na(host_hh_id_audit$hh_id_analysis_note))
if (length(analysis_idx)) {
  generated_idx <- analysis_idx[host_hh_id_audit$hh_id_analysis_note[analysis_idx] == baseline_generated_note]
  matched_idx <- analysis_idx[host_hh_id_audit$hh_id_source[analysis_idx] == "matched_baseline_host_endline"]
  if (length(generated_idx)) {
    host_hh_id_audit$issue[generated_idx] <- append_issue(
      host_hh_id_audit$issue[generated_idx],
      "generated_baseline_host_hh_id_for_analysis"
    )
  }
  if (length(matched_idx)) {
    host_hh_id_audit$issue[matched_idx] <- append_issue(
      host_hh_id_audit$issue[matched_idx],
      "matched_endline_host_hh_id_from_baseline"
    )
  }
}

duplicate_keys <- paste(host_hh_id_audit$timepoint, host_hh_id_audit$hh_id, sep = "__")
duplicate_idx <- which(
  !is.na(host_hh_id_audit$hh_id) &
    (duplicated(duplicate_keys) | duplicated(duplicate_keys, fromLast = TRUE))
)
if (length(duplicate_idx)) {
  host_hh_id_audit$issue[duplicate_idx] <- append_issue(
    host_hh_id_audit$issue[duplicate_idx],
    "duplicate_hh_id_within_timepoint"
  )
}

audit_path <- clean_final_path(
  "4_data",
  "clean_final",
  "survey_host_household_hh_id_derivation_audit.csv"
)
ensure_parent_dir(audit_path)
write.csv(host_hh_id_audit, audit_path, row.names = FALSE, na = "")

if (missing_hh_id_count > 0L) {
  warning(
    "Derived final host `hh_id` from T0#### tokens in `house_id` or `serial_id`; ",
    missing_hh_id_count,
    " host rows still have unresolved missing `hh_id` after baseline assignments and endline matching.",
    call. = FALSE
  )
}
write_timepoint_summary(make_timepoint_summary(survey, dataset_name), dataset_name)

deidentified <- drop_identifier_columns(survey)
survey <- deidentified$data
survey <- move_columns_first(
  survey,
  c(
    "community", "data_type", "hh_id_short", "fcn_id", "hh_id", "hh_id_original",
    "hh_id_source", "hh_id_analysis_note", "hh_id_match_base_row",
    "hh_id_match_score", "hh_id_match_gap", "unique_id", "uuid",
    "timepoint", "timepoint_original", "collection_date", "collection_year",
    "timepoint_source_col", "raw_collection_round", "raw_survey_version",
    "raw_source_file", "study_arm_overall", "study_arm"
  )
)

output_path <- write_final_rds(survey, "4_data/clean_final/survey_host_household.rds")

entry <- make_inventory_entry(
  dataset_name = dataset_name,
  data = survey,
  output_path = output_path,
  source_paths = clean_final_path(source_rel),
  removed_identifier_columns = deidentified$removed,
  notes = paste(
    "Host household survey final dataset rebuilt from raw-first imports in 2_data_raw.",
    "Raw host hh_id is preserved as hh_id_original; final hh_id is derived from normalized T0#### tokens in house_id first, then serial_id.",
    "For baseline HOST records only, row 98 was relabeled to T09001 and baseline rows missing hh_id were assigned sequential T09002+ IDs for analysis.",
    paste0("Baseline HOST hh_id values made up for analysis: ", generated_baseline_hh_id_count, "."),
    paste0("Endline HOST hh_id values matched to baseline using identifying fields: ", matched_endline_hh_id_count, "."),
    paste0("Unresolved host rows with missing hh_id after endline matching: ", missing_hh_id_count, "."),
    paste0("Duplicate derived host hh_id values after baseline/endline cleaning: ", duplicate_hh_id_count, "."),
    "Includes host survey files from survey_HOST and fixes the host household-member import typo.",
    "No refugee records are included. Timepoint was recoded from parsed collection timestamps."
  )
)
update_inventory(entry)
write_cleaning_fix_log()

message("Wrote ", output_path)