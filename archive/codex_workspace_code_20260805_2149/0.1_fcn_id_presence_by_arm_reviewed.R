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
#   7_tables/RF105_reviewed_YYYYMMDD/qa/fcn_id_arm_timepoint_counts_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/fcn_id_presence_long_by_arm_timepoint_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/fcn_id_presence_wide_by_timepoint_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/fcn_id_set_difference_counts_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/intervention_endline_not_baseline_midline_ids_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/comparison_midline_not_endline_ids_reviewed.csv
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
  file.path(script_dir, "0_RF105_reviewed_config.R"),
  file.path(getwd(), "5_analysis_RF105", "0_RF105_reviewed_config.R"),
  file.path(Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = ""),
            "5_analysis_RF105", "0_RF105_reviewed_config.R"),
  "G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/5_analysis_RF105/0_RF105_reviewed_config.R"
)
config_file <- config_file_candidates[file.exists(config_file_candidates)][1]

if (is.na(config_file)) {
  stop("Could not find 0_RF105_reviewed_config.R. Set ROHINGYA_ANALYSIS_ROOT.")
}

source(config_file)

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

write_reviewed_csv(
  analysis_population$duplicate_records,
  "fcn_id_duplicate_timepoint_records_reviewed.csv",
  subfolder = "qa"
)

population_datasets <- list(
  all_deduplicated = analysis_population$all_deduplicated %>%
    mutate(timepoint = as.character(timepoint),
           study_arm_overall = as.character(study_arm_overall)),
  complete_3_survey = analysis_population$complete_3_survey %>%
    mutate(timepoint = as.character(timepoint),
           study_arm_overall = as.character(study_arm_overall))
)

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

write_reviewed_csv(
  arm_timepoint_counts,
  "fcn_id_arm_timepoint_counts_reviewed.csv",
  subfolder = "qa"
)

write_reviewed_csv(
  presence_long,
  "fcn_id_presence_long_by_arm_timepoint_reviewed.csv",
  subfolder = "qa"
)

write_reviewed_csv(
  presence_wide,
  "fcn_id_presence_wide_by_timepoint_reviewed.csv",
  subfolder = "qa"
)

write_reviewed_csv(
  set_difference_counts,
  "fcn_id_set_difference_counts_reviewed.csv",
  subfolder = "qa"
)

write_reviewed_csv(
  intervention_endline_not_baseline_midline,
  "intervention_endline_not_baseline_midline_ids_reviewed.csv",
  subfolder = "qa"
)

write_reviewed_csv(
  comparison_midline_not_endline,
  "comparison_midline_not_endline_ids_reviewed.csv",
  subfolder = "qa"
)

message("fcn_id presence checks complete.")
