################################################################################
# RF105 reviewed fcn_id presence check by arm and timepoint
#
# Purpose:
#   Compare household fcn_id values and source file names across baseline,
#   midline, and endline for each RF105 study arm.
#
# Input:
#   4_data/clean_final/survey_refugee_household.rds
#
# Outputs:
#   Shareable aggregate QA:
#     7_tables/RF105_reviewed_YYYYMMDD/qa/table_qa_fcn_arm_timepoint_counts.csv
#     7_tables/RF105_reviewed_YYYYMMDD/qa/table_qa_fcn_set_differences.csv
#     7_tables/RF105_reviewed_YYYYMMDD/qa/table_qa_fcn_allocation_master_summary.csv
#   Restricted internal QA, not for public release:
#     8_restricted/RF105_reviewed_YYYYMMDD/qa/restricted_qa_fcn_presence_long.csv
#     8_restricted/RF105_reviewed_YYYYMMDD/qa/restricted_qa_fcn_presence_wide.csv
#     8_restricted/RF105_reviewed_YYYYMMDD/qa/restricted_qa_fcn_allocation_reconciliation.csv
#     8_restricted/RF105_reviewed_YYYYMMDD/qa/restricted_qa_fcn_intervention_endline_ids.csv
#     8_restricted/RF105_reviewed_YYYYMMDD/qa/restricted_qa_fcn_comparison_midline_ids.csv
#   Release checklist:
#     7_tables/RF105_reviewed_YYYYMMDD/release/table_release_checklist.csv
#
# Notes:
#   Two kinds of set differences are written:
#     1. same_arm: target-arm fcn_id values present at the target timepoint
#        but not present in the same arm at the comparison timepoint(s).
#        This identifies arm reassignment/coding changes.
#     2. any_arm: target-arm fcn_id values present at the target timepoint
#        but with no record at the comparison timepoint(s), regardless of arm.
#        This identifies true missing timepoint records.
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
            "5_analysis_RF105", "reviewed", "0_RF105_config_20260805_2213.R"),
  "G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/5_analysis_RF105/reviewed/0_RF105_config_20260805_2213.R"
)
config_file <- config_file_candidates[file.exists(config_file_candidates)][1]

if (is.na(config_file)) {
  stop("Could not find 0_RF105_config_20260805_2213.R. Set ROHINGYA_ANALYSIS_ROOT.")
}

source(config_file)
# Optional suffix for reruns when an output CSV is open/locked, e.g.:
# Sys.setenv(RF105_AUDIT_OUTPUT_SUFFIX = "_baseline_arm_derived")
audit_output_suffix <- Sys.getenv("RF105_AUDIT_OUTPUT_SUFFIX", unset = "")
audit_output_file <- function(filename) {
  if (!nzchar(audit_output_suffix)) {
    return(filename)
  }

  sub("\\.csv$", paste0(audit_output_suffix, ".csv"), filename)
}

collapse_unique <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x <- x[!is.na(x) & x != ""]

  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(sort(unique(x)), collapse = "; ")
}

ensure_cols <- function(df, vars) {
  missing_vars <- vars[vars %notin% names(df)]

  for (var in missing_vars) {
    df[[var]] <- NA_character_
  }

  df
}

read_allocation_master <- function(path = file.path(
  project_root,
  "2_data_raw",
  "RohingyaFuelMaster_hh_data - Copy.xlsx"
)) {
  if (!file.exists(path)) {
    stop("Allocation master file not found: ", path, call. = FALSE)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package `readxl` is required for allocation-master reconciliation.", call. = FALSE)
  }

  allocation_raw <- readxl::read_excel(path, sheet = 1, col_types = "text") %>%
    as_tibble() %>%
    ensure_cols(c(
      "study_arm", "fcn_id", "hh_id", "camp_id", "block_id", "subblock_id",
      "name_hh_head", "name_respondent", "target_child_name"
    )) %>%
    mutate(
      fcn_id = str_squish(as.character(fcn_id)),
      fcn_id = na_if(fcn_id, ""),
      allocation_master_fcn_id_original = fcn_id,
      fcn_id = recode(fcn_id, "999999" = "101595", .default = fcn_id),
      allocation_master_fcn_id_recode_note = if_else(
        allocation_master_fcn_id_original == "999999",
        "Reconciled allocation-master placeholder 999999 to fcn_id 101595 based on geocene_data_but_no_survey_error_fix_fcn_id.xlsx.",
        NA_character_
      ),
      allocation_study_arm_label_raw = str_squish(str_to_lower(as.character(study_arm))),
      allocation_study_arm_label_raw = na_if(allocation_study_arm_label_raw, "")
    ) %>%
    filter(!is.na(fcn_id))

  derive_allocation_arm <- function(labels) {
    labels <- labels[!is.na(labels) & labels != ""]
    has_intervention_labels <- any(labels %in% c("pre-intervention", "post-intervention"))
    has_comparison_labels <- any(labels %in% c("intervention", "intervention follow-up"))

    case_when(
      has_intervention_labels ~ "intervention",
      has_comparison_labels ~ "comparison",
      TRUE ~ NA_character_
    )
  }

  derive_allocation_status <- function(labels) {
    labels <- labels[!is.na(labels) & labels != ""]
    has_intervention_labels <- any(labels %in% c("pre-intervention", "post-intervention"))
    has_comparison_labels <- any(labels %in% c("intervention", "intervention follow-up"))

    case_when(
      has_intervention_labels & has_comparison_labels ~
        "mixed_old_labels_resolved_to_intervention",
      has_intervention_labels ~
        "old_pre_post_intervention_labels_resolved_to_intervention",
      has_comparison_labels ~
        "old_intervention_followup_labels_resolved_to_comparison",
      TRUE ~ "no_recognized_allocation_label"
    )
  }

  allocation_raw %>%
    group_by(fcn_id) %>%
    summarise(
      allocation_master_path = normalizePath(path, winslash = "/", mustWork = TRUE),
      allocation_master_rows = n(),
      allocation_master_fcn_id_originals = collapse_unique(allocation_master_fcn_id_original),
      allocation_master_fcn_id_recode_notes = collapse_unique(allocation_master_fcn_id_recode_note),
      allocation_master_arm = derive_allocation_arm(allocation_study_arm_label_raw),
      allocation_master_status = derive_allocation_status(allocation_study_arm_label_raw),
      allocation_master_study_arm_labels = collapse_unique(allocation_study_arm_label_raw),
      allocation_master_hh_ids = collapse_unique(hh_id),
      allocation_master_camp_ids = collapse_unique(camp_id),
      allocation_master_block_ids = collapse_unique(block_id),
      allocation_master_subblock_ids = collapse_unique(subblock_id),
      allocation_master_names_available = any(
        !is.na(name_hh_head) | !is.na(name_respondent) | !is.na(target_child_name)
      ),
      .groups = "drop"
    )
}

make_allocation_master_reconciliation <- function(df) {
  allocation_master <- read_allocation_master()

  survey_for_reconciliation <- df %>%
    ensure_cols(c(
      "fcn_id", "hh_id", "timepoint", "study_arm_overall", "camp_id",
      "block_id", "subblock_id", "collection_date", "raw_source_file"
    )) %>%
    mutate(
      fcn_id = str_squish(as.character(fcn_id)),
      fcn_id = na_if(fcn_id, ""),
      study_arm_overall = str_squish(str_to_lower(as.character(study_arm_overall))),
      study_arm_overall = na_if(study_arm_overall, "")
    ) %>%
    filter(!is.na(fcn_id))

  cleaned_arm_by_fcn <- survey_for_reconciliation %>%
    group_by(fcn_id) %>%
    summarise(
      cleaned_survey_rows = n(),
      cleaned_timepoints = collapse_unique(as.character(timepoint)),
      cleaned_study_arm_values = collapse_unique(study_arm_overall),
      cleaned_n_distinct_study_arm_values = n_distinct(study_arm_overall, na.rm = TRUE),
      cleaned_study_arm_overall = if_else(
        cleaned_n_distinct_study_arm_values == 1L,
        first(study_arm_overall[!is.na(study_arm_overall)]),
        NA_character_
      ),
      cleaned_hh_ids = collapse_unique(hh_id),
      cleaned_camp_ids = collapse_unique(camp_id),
      cleaned_block_ids = collapse_unique(block_id),
      cleaned_subblock_ids = collapse_unique(subblock_id),
      cleaned_collection_dates = collapse_unique(collection_date),
      cleaned_raw_source_files = collapse_unique(raw_source_file),
      .groups = "drop"
    )

  full_join(cleaned_arm_by_fcn, allocation_master, by = "fcn_id") %>%
    mutate(
      reconciliation_status = case_when(
        is.na(cleaned_survey_rows) & !is.na(allocation_master_rows) ~
          "in_allocation_master_not_cleaned_survey",
        !is.na(cleaned_survey_rows) & is.na(allocation_master_rows) ~
          "in_cleaned_survey_not_allocation_master",
        cleaned_n_distinct_study_arm_values > 1L ~
          "cleaned_survey_conflicting_arms_for_fcn_id",
        is.na(allocation_master_arm) ~
          "allocation_master_no_recognized_arm",
        cleaned_study_arm_overall == allocation_master_arm ~
          "matched_cleaned_to_allocation_master",
        TRUE ~ "mismatch_cleaned_vs_allocation_master"
      ),
      allocation_reconciliation_note = case_when(
        reconciliation_status == "matched_cleaned_to_allocation_master" ~
          "Cleaned study_arm_overall agrees with the fcn_id allocation master.",
        reconciliation_status == "mismatch_cleaned_vs_allocation_master" ~
          "Cleaned study_arm_overall disagrees with the fcn_id allocation master; review before analysis.",
        reconciliation_status == "cleaned_survey_conflicting_arms_for_fcn_id" ~
          "The cleaned survey assigns more than one study_arm_overall to this fcn_id.",
        reconciliation_status == "in_cleaned_survey_not_allocation_master" ~
          "This cleaned fcn_id is not present in the allocation master.",
        reconciliation_status == "in_allocation_master_not_cleaned_survey" ~
          "This allocation-master fcn_id is not present in the cleaned survey file.",
        TRUE ~ "Allocation-master labels could not be resolved to intervention/comparison."
      )
    ) %>%
    arrange(reconciliation_status, allocation_master_arm, cleaned_study_arm_overall, fcn_id)
}
make_population_listing <- function(df, population_name, source_file_summary) {
  optional_vars <- c(
    "hh_id", "uuid", "collection_date", "raw_source_file",
    "raw_collection_round", "raw_survey_version", "camp_id", "block_id",
    "subblock_id", "start_date", "end_date", "submission_time"
  )

  df %>%
    ensure_cols(optional_vars) %>%
    mutate(
      population = population_name,
      timepoint = as.character(timepoint),
      study_arm_overall = as.character(study_arm_overall)
    ) %>%
    select(
      population, fcn_id, hh_id, uuid, timepoint, study_arm_overall,
      collection_date, raw_source_file, raw_collection_round,
      raw_survey_version, camp_id, block_id, subblock_id, start_date,
      end_date, submission_time
    ) %>%
    left_join(source_file_summary, by = c("fcn_id", "timepoint")) %>%
    arrange(population, study_arm_overall, fcn_id, match(timepoint, timepoint_levels))
}

ids_in_arm_timepoint <- function(df, arm, timepoints) {
  df %>%
    filter(study_arm_overall == arm, timepoint %in% timepoints) %>%
    distinct(fcn_id) %>%
    pull(fcn_id)
}

ids_in_any_arm_timepoint <- function(df, timepoints) {
  df %>%
    filter(timepoint %in% timepoints) %>%
    distinct(fcn_id) %>%
    pull(fcn_id)
}

get_set_difference_ids <- function(df, target_arm, present_timepoint,
                                   absent_timepoints, comparison_basis) {
  present_ids <- ids_in_arm_timepoint(df, target_arm, present_timepoint)

  comparison_ids <- if (comparison_basis == "same_arm") {
    ids_in_arm_timepoint(df, target_arm, absent_timepoints)
  } else if (comparison_basis == "any_arm") {
    ids_in_any_arm_timepoint(df, absent_timepoints)
  } else {
    stop("comparison_basis must be 'same_arm' or 'any_arm'.")
  }

  setdiff(present_ids, comparison_ids)
}

make_set_difference_details <- function(df, population_name, target_arm,
                                        present_timepoint, absent_timepoints,
                                        comparison_basis, comparison_name,
                                        presence_wide) {
  out_ids <- get_set_difference_ids(
    df = df,
    target_arm = target_arm,
    present_timepoint = present_timepoint,
    absent_timepoints = absent_timepoints,
    comparison_basis = comparison_basis
  )

  presence_wide %>%
    filter(population == population_name, fcn_id %in% out_ids) %>%
    mutate(
      comparison_name = comparison_name,
      comparison_basis = comparison_basis,
      target_arm = target_arm,
      present_timepoint = present_timepoint,
      absent_timepoints = paste(absent_timepoints, collapse = "; "),
      n_ids_in_set = length(out_ids),
      .before = population
    ) %>%
    arrange(comparison_name, comparison_basis, fcn_id)
}

make_set_difference_count <- function(df, population_name, target_arm,
                                      present_timepoint, absent_timepoints,
                                      comparison_basis, comparison_name) {
  out_ids <- get_set_difference_ids(
    df = df,
    target_arm = target_arm,
    present_timepoint = present_timepoint,
    absent_timepoints = absent_timepoints,
    comparison_basis = comparison_basis
  )

  tibble(
    population = population_name,
    comparison_name = comparison_name,
    comparison_basis = comparison_basis,
    target_arm = target_arm,
    present_timepoint = present_timepoint,
    absent_timepoints = paste(absent_timepoints, collapse = "; "),
    n_ids = length(out_ids)
  )
}

################################################################################
# Read and prepare the cleaned household data
################################################################################

survey_data_raw <- readRDS(file_survey_refugee_household) %>%
  add_rf105_aliases()

survey_records <- survey_data_raw %>%
  clean_timepoint_arm() %>%
  filter(
    !is.na(fcn_id),
    fcn_id != "",
    !is.na(timepoint),
    timepoint %in% timepoint_levels
  ) %>%
  ensure_cols(c(
    "collection_date", "raw_source_file", "raw_collection_round",
    "raw_survey_version"
  )) %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  )

# Keep all raw source file names for each fcn_id/timepoint. The analysis
# population below is deduplicated, but this summary preserves the file
# provenance if multiple raw records were collapsed.
source_file_summary <- survey_records %>%
  group_by(fcn_id, timepoint) %>%
  summarise(
    n_raw_records_for_fcn_timepoint = n(),
    study_arm_values_all_raw = collapse_unique(study_arm_overall),
    collection_dates_all_raw = collapse_unique(collection_date),
    raw_source_files_all_raw = collapse_unique(raw_source_file),
    raw_collection_rounds_all_raw = collapse_unique(raw_collection_round),
    raw_survey_versions_all_raw = collapse_unique(raw_survey_version),
    .groups = "drop"
  )

analysis_population <- make_analysis_population(survey_data_raw, id_var = "fcn_id")

write_restricted_qa_csv(
  analysis_population$duplicate_records,
  audit_output_file("restricted_qa_fcn_duplicate_records.csv"),
  reason = "Duplicate fcn_id/timepoint listings include household identifiers and source-file provenance for internal QA."
)
population_datasets <- list(
  all_deduplicated = analysis_population$all_deduplicated %>%
    mutate(timepoint = as.character(timepoint),
           study_arm_overall = as.character(study_arm_overall)),
  complete_3_survey = analysis_population$complete_3_survey %>%
    mutate(timepoint = as.character(timepoint),
           study_arm_overall = as.character(study_arm_overall))
)

allocation_reconciliation <- make_allocation_master_reconciliation(
  population_datasets$all_deduplicated
)

allocation_reconciliation_summary <- allocation_reconciliation %>%
  count(
    reconciliation_status,
    allocation_master_status,
    allocation_master_arm,
    cleaned_study_arm_overall,
    name = "n_fcn_id"
  ) %>%
  arrange(reconciliation_status, allocation_master_status,
          allocation_master_arm, cleaned_study_arm_overall)
presence_long <- bind_rows(
  make_population_listing(
    population_datasets$all_deduplicated,
    "all_deduplicated",
    source_file_summary
  ),
  make_population_listing(
    population_datasets$complete_3_survey,
    "complete_3_survey",
    source_file_summary
  )
)

arm_timepoint_counts <- presence_long %>%
  distinct(population, fcn_id, timepoint, study_arm_overall) %>%
  count(population, timepoint, study_arm_overall, name = "n_fcn_id") %>%
  arrange(population, match(timepoint, timepoint_levels), study_arm_overall)

presence_wide <- presence_long %>%
  select(
    population, fcn_id, timepoint, study_arm_overall, collection_date,
    raw_source_file, raw_source_files_all_raw, n_raw_records_for_fcn_timepoint
  ) %>%
  pivot_wider(
    names_from = timepoint,
    values_from = c(
      study_arm_overall, collection_date, raw_source_file,
      raw_source_files_all_raw, n_raw_records_for_fcn_timepoint
    ),
    names_glue = "{timepoint}_{.value}",
    values_fill = list(n_raw_records_for_fcn_timepoint = 0)
  ) %>%
  arrange(population, fcn_id)

################################################################################
# Requested set differences
################################################################################

set_difference_counts <- bind_rows(lapply(names(population_datasets), function(pop) {
  df <- population_datasets[[pop]]

  bind_rows(
    make_set_difference_count(
      df = df,
      population_name = pop,
      target_arm = "intervention",
      present_timepoint = "endline",
      absent_timepoints = c("baseline", "midline"),
      comparison_basis = "same_arm",
      comparison_name = "intervention_endline_not_baseline_midline"
    ),
    make_set_difference_count(
      df = df,
      population_name = pop,
      target_arm = "intervention",
      present_timepoint = "endline",
      absent_timepoints = c("baseline", "midline"),
      comparison_basis = "any_arm",
      comparison_name = "intervention_endline_not_baseline_midline"
    ),
    make_set_difference_count(
      df = df,
      population_name = pop,
      target_arm = "comparison",
      present_timepoint = "midline",
      absent_timepoints = "endline",
      comparison_basis = "same_arm",
      comparison_name = "comparison_midline_not_endline"
    ),
    make_set_difference_count(
      df = df,
      population_name = pop,
      target_arm = "comparison",
      present_timepoint = "midline",
      absent_timepoints = "endline",
      comparison_basis = "any_arm",
      comparison_name = "comparison_midline_not_endline"
    )
  )
}))

intervention_endline_not_baseline_midline <- bind_rows(
  lapply(names(population_datasets), function(pop) {
    df <- population_datasets[[pop]]

    bind_rows(
      make_set_difference_details(
        df = df,
        population_name = pop,
        target_arm = "intervention",
        present_timepoint = "endline",
        absent_timepoints = c("baseline", "midline"),
        comparison_basis = "same_arm",
        comparison_name = "intervention_endline_not_baseline_midline",
        presence_wide = presence_wide
      ),
      make_set_difference_details(
        df = df,
        population_name = pop,
        target_arm = "intervention",
        present_timepoint = "endline",
        absent_timepoints = c("baseline", "midline"),
        comparison_basis = "any_arm",
        comparison_name = "intervention_endline_not_baseline_midline",
        presence_wide = presence_wide
      )
    )
  })
)

comparison_midline_not_endline <- bind_rows(
  lapply(names(population_datasets), function(pop) {
    df <- population_datasets[[pop]]

    bind_rows(
      make_set_difference_details(
        df = df,
        population_name = pop,
        target_arm = "comparison",
        present_timepoint = "midline",
        absent_timepoints = "endline",
        comparison_basis = "same_arm",
        comparison_name = "comparison_midline_not_endline",
        presence_wide = presence_wide
      ),
      make_set_difference_details(
        df = df,
        population_name = pop,
        target_arm = "comparison",
        present_timepoint = "midline",
        absent_timepoints = "endline",
        comparison_basis = "any_arm",
        comparison_name = "comparison_midline_not_endline",
        presence_wide = presence_wide
      )
    )
  })
)

################################################################################
# Write reproducible QA outputs
################################################################################

restricted_qa_manifest <- tibble(
  output_file = c(
    audit_output_file("restricted_qa_fcn_duplicate_records.csv"),
    audit_output_file("restricted_qa_fcn_presence_long.csv"),
    audit_output_file("restricted_qa_fcn_presence_wide.csv"),
    audit_output_file("restricted_qa_fcn_allocation_reconciliation.csv"),
    audit_output_file("restricted_qa_fcn_intervention_endline_ids.csv"),
    audit_output_file("restricted_qa_fcn_comparison_midline_ids.csv")
  ),
  restricted_location = file.path(dir_restricted_qa, output_file),
  purpose = c(
    "List duplicate fcn_id/timepoint records retained for internal cleaning QA.",
    "List fcn_id presence by arm and survey timepoint with dates and source files.",
    "Wide fcn_id presence matrix used to check timepoint overlap and arm coding.",
    "Compare cleaned study arm against the allocation master at fcn_id level.",
    "Identify intervention fcn_id values present at endline but not prior timepoints.",
    "Identify comparison fcn_id values present at midline but not endline."
  ),
  release_status = "restricted_internal_only"
)

write_reviewed_csv(
  arm_timepoint_counts,
  audit_output_file("table_qa_fcn_arm_timepoint_counts.csv"),
  subfolder = "qa"
)

write_reviewed_csv(
  allocation_reconciliation_summary,
  audit_output_file("table_qa_fcn_allocation_master_summary.csv"),
  subfolder = "qa"
)

write_reviewed_csv(
  set_difference_counts,
  audit_output_file("table_qa_fcn_set_differences.csv"),
  subfolder = "qa"
)

write_reviewed_csv(
  restricted_qa_manifest,
  audit_output_file("table_release_restricted_qa_manifest.csv"),
  subfolder = "release"
)

write_restricted_qa_csv(
  presence_long,
  audit_output_file("restricted_qa_fcn_presence_long.csv"),
  reason = "Contains fcn_id, collection dates, source file names, and location-like survey fields."
)

write_restricted_qa_csv(
  presence_wide,
  audit_output_file("restricted_qa_fcn_presence_wide.csv"),
  reason = "Contains fcn_id-level timepoint and source-file presence across survey rounds."
)

write_restricted_qa_csv(
  allocation_reconciliation,
  audit_output_file("restricted_qa_fcn_allocation_reconciliation.csv"),
  reason = "Contains fcn_id-level allocation-master reconciliation details."
)

write_restricted_qa_csv(
  intervention_endline_not_baseline_midline,
  audit_output_file("restricted_qa_fcn_intervention_endline_ids.csv"),
  reason = "Contains fcn_id-level intervention households present at endline only."
)

write_restricted_qa_csv(
  comparison_midline_not_endline,
  audit_output_file("restricted_qa_fcn_comparison_midline_ids.csv"),
  reason = "Contains fcn_id-level comparison households present at midline but not endline."
)

write_rf105_release_checklist(
  tibble(
    item = c(
      "Review the restricted fcn_id QA manifest before release",
      "Do not include restricted fcn_id QA files in manuscript/shareable outputs"
    ),
    status = "required_before_public_release",
    location = c(
      file.path(dir_tables_release, audit_output_file("table_release_restricted_qa_manifest.csv")),
      dir_restricted_qa
    ),
    notes = c(
      "The manifest names restricted internal QA files without exposing household-level rows.",
      "These files contain fcn_id values, household IDs, dates, location-like fields, and raw source-file provenance."
    )
  )
)
allocation_blocking_statuses <- c(
  "mismatch_cleaned_vs_allocation_master",
  "cleaned_survey_conflicting_arms_for_fcn_id",
  "allocation_master_no_recognized_arm"
)
allocation_blocking_n <- allocation_reconciliation %>%
  filter(reconciliation_status %in% allocation_blocking_statuses) %>%
  nrow()
if (allocation_blocking_n > 0) {
  stop(
    "Allocation-master reconciliation found ", allocation_blocking_n,
    " blocking study-arm issue(s). Review restricted_qa_fcn_allocation_reconciliation.csv in the restricted QA folder.",
    call. = FALSE
  )
}

allocation_missing_master_n <- allocation_reconciliation %>%
  filter(reconciliation_status == "in_cleaned_survey_not_allocation_master") %>%
  nrow()
if (allocation_missing_master_n > 0) {
  warning(
    "Allocation-master reconciliation found ", allocation_missing_master_n,
    " cleaned fcn_id value(s) not present in the allocation master; see QA output.",
    call. = FALSE
  )
}
message("fcn_id presence checks complete.")



