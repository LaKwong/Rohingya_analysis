################################################################################
# RF105 reviewed DRDID benchmark for rDiD/XGBoost results
#
# Purpose:
#   Benchmark the reviewed reverse difference-in-differences (rDiD) XGBoost
#   estimates from 4_rdid_xgboost_20260805_2213.R against the CRAN DRDID
#   panel-data doubly robust DiD estimator.
#
# Inputs:
#   4_data/clean_final/survey_refugee_household.rds
#   4_data/clean_final/survey_refugee_hh_members.rds
#   8_restricted/RF105_reviewed_YYYYMMDD/identified_tables/table_descriptive_pm25_household_timepoint_internal.csv
#   7_tables/RF105_reviewed_YYYYMMDD/table_rDiD_xgboost_all_results.csv
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/table_DRDID_all_results.csv
#   7_tables/RF105_reviewed_YYYYMMDD/table_DRDID_rDID_benchmark.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/table_DRDID_package_versions.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/table_DRDID_estimand_target_note.md
#
# Estimand target:
#   DRDID::drdid_panel() estimates ATT for the group coded D = 1.
#   The reviewed rDiD estimator in 4_rdid_xgboost_20260805_2213.R sets
#   A = 1 for intervention but targets the comparison-arm population through
#   pi0 = mean(1 - A), so its target is ATC for the original
#   intervention-minus-comparison contrast.
#
#   This benchmark therefore reports two DRDID orientations:
#     1. ATT: D = A, native DRDID ATT for original intervention households.
#     2. ATC: D = 1 - A, then sign-flipped so the contrast is still
#        intervention minus comparison for original comparison households.
#
#   The rDiD/XGBoost benchmark table uses the ATC-aligned DRDID estimate.
#   No ATE is estimated here.
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
config_file <- file.path(script_dir, "0_RF105_config_20260805_2213.R")
if (!file.exists(config_file)) {
  config_file <- file.path("5_analysis_RF105", "reviewed", "0_RF105_config_20260805_2213.R")
}
source(config_file)

ensure_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "Package ", package, " is required for the RF105 DRDID benchmark. ",
      "Run renv::restore() from the project root, then rerun this script.",
      call. = FALSE
    )
  }
}

ensure_package("DRDID")

suppressPackageStartupMessages({
  library(DRDID)
})

safe_write_reviewed_csv <- function(x, filename, subfolder = NULL) {
  tryCatch(
    write_reviewed_csv(x, filename, subfolder = subfolder),
    error = function(e) {
      fallback_filename <- str_replace(filename, "\\.csv$", paste0("_refreshed_", date_stamp, ".csv"))
      warning(
        "Could not overwrite ", filename, "; writing refreshed copy ",
        fallback_filename, ". Original error: ", conditionMessage(e),
        call. = FALSE
      )
      write_reviewed_csv(x, fallback_filename, subfolder = subfolder)
    }
  )
}

safe_write_lines <- function(lines, out_file) {
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  tryCatch(
    writeLines(lines, out_file),
    error = function(e) {
      fallback_file <- sub("\\.md$", paste0("_refreshed_", date_stamp, ".md"), out_file)
      warning(
        "Could not overwrite ", out_file, "; writing refreshed copy ",
        fallback_file, ". Original error: ", conditionMessage(e),
        call. = FALSE
      )
      writeLines(lines, fallback_file)
    }
  )
}

drdid_package_versions <- tibble(
  package = c("R", "DRDID", "tidyverse", "dplyr", "readr"),
  version = c(
    as.character(getRversion()),
    as.character(utils::packageVersion("DRDID")),
    as.character(utils::packageVersion("tidyverse")),
    as.character(utils::packageVersion("dplyr")),
    as.character(utils::packageVersion("readr"))
  )
)

safe_write_reviewed_csv(
  drdid_package_versions,
  "table_DRDID_package_versions.csv",
  subfolder = "qa"
)

################################################################################
# Reuse reviewed rDiD data preparation without rerunning XGBoost estimators
################################################################################

rdid_source_script <- file.path(script_dir, "4_rdid_xgboost_20260805_2213.R")
if (!file.exists(rdid_source_script)) {
  stop("Could not find reviewed rDiD source script: ", rdid_source_script, call. = FALSE)
}

rdid_source_lines <- readLines(rdid_source_script, warn = FALSE)
prep_start <- grep("^# Helper functions", rdid_source_lines)
prep_end <- grep("^severe_asthma_coding_audit", rdid_source_lines)

if (length(prep_start) == 0 || length(prep_end) == 0 || prep_end[[1]] <= prep_start[[1]]) {
  stop(
    "Could not locate the reviewed rDiD preparation block in ",
    rdid_source_script,
    call. = FALSE
  )
}

# The preparation block defines helper functions and derives survey_model_data.
# Temporarily no-op table writes so this benchmark does not regenerate rDiD QA
# tables before running the DRDID comparison.
write_benchmark_csv <- safe_write_reviewed_csv
safe_write_reviewed_csv <- function(...) invisible(NULL)
eval(parse(text = rdid_source_lines[prep_start[[1]]:(prep_end[[1]] - 1)]))
safe_write_reviewed_csv <- write_benchmark_csv

################################################################################
# Existing rDiD result table and outcome specifications
################################################################################

find_latest_rdid_results <- function() {
  explicit_file <- Sys.getenv("RF105_RDID_RESULTS_FILE", unset = "")
  if (nzchar(explicit_file)) {
    if (!file.exists(explicit_file)) {
      stop("RF105_RDID_RESULTS_FILE does not exist: ", explicit_file, call. = FALSE)
    }
    return(normalizePath(explicit_file, winslash = "/", mustWork = FALSE))
  }

  same_day_file <- file.path(dir_tables_reviewed, "table_rDiD_xgboost_all_results.csv")
  if (file.exists(same_day_file)) {
    return(normalizePath(same_day_file, winslash = "/", mustWork = FALSE))
  }

  candidate_dirs <- list.dirs(file.path(project_root, "7_tables"), recursive = FALSE, full.names = TRUE)
  candidate_dirs <- candidate_dirs[grepl("^RF105_reviewed_[0-9]{8}$", basename(candidate_dirs))]
  candidate_files <- file.path(candidate_dirs, "table_rDiD_xgboost_all_results.csv")
  candidate_files <- candidate_files[file.exists(candidate_files)]

  if (length(candidate_files) == 0) {
    stop(
      "No table_rDiD_xgboost_all_results.csv file found. Run ",
      "4_rdid_xgboost_20260805_2213.R first or set RF105_RDID_RESULTS_FILE.",
      call. = FALSE
    )
  }

  candidate_files[[order(basename(dirname(candidate_files)), decreasing = TRUE)[[1]]]]
}

rdid_results_file <- find_latest_rdid_results()
rdid_xgboost_reference <- readr::read_csv(rdid_results_file, show_col_types = FALSE) %>%
  filter(estimator == "rDID_XGBoost") %>%
  mutate(
    rdid_target_estimand = "ATC",
    rdid_target_population = "comparison_arm_households",
    rdid_treatment_contrast = "intervention_minus_comparison",
    rdid_target_note = paste(
      "The reviewed reverse-DID estimator targets the comparison-arm",
      "population through pi0 = mean(1 - A), with A = 1 for intervention."
    )
  )

outcome_specs <- rdid_xgboost_reference %>%
  distinct(
    contrast, population, followup_timepoint, outcome, outcome_label, domain,
    outcome_type, unit, outcome_source, minimum_arm_households
  ) %>%
  arrange(contrast, domain, outcome)

################################################################################
# PM2.5 canonical household-timepoint outcomes
################################################################################

pm_adjusted_file <- file.path(
  project_root,
  "8_restricted",
  paste0("RF105_reviewed_", date_stamp),
  "identified_tables",
  "table_descriptive_pm25_household_timepoint_internal.csv"
)
if (!file.exists(pm_adjusted_file)) {
  stop(
    "Same-run canonical PM2.5 file not found: ", pm_adjusted_file, ". Run ",
    "3_descriptive_outcomes_20260805_2213.R before 5_drDiD_comparison_20260805_2213.R.",
    call. = FALSE
  )
}

pm_required_cols <- c(
  "fcn_id", "timepoint", "study_arm_overall",
  "pm25_ambient_excess_f000", "pm25_ambient_excess_f025",
  "pm25_ambient_excess_f050", "pm25_ambient_excess_f075",
  "pm25_ambient_excess_f100"
)
pm_adjusted_raw <- readr::read_csv(pm_adjusted_file, show_col_types = FALSE)
pm_missing_cols <- setdiff(pm_required_cols, names(pm_adjusted_raw))
if (length(pm_missing_cols) > 0) {
  stop("Canonical PM2.5 file is missing: ", paste(pm_missing_cols, collapse = ", "), call. = FALSE)
}

pm_duplicate_keys <- pm_adjusted_raw %>%
  transmute(fcn_id = as.character(fcn_id), timepoint = as.character(timepoint)) %>%
  count(fcn_id, timepoint) %>%
  filter(is.na(fcn_id) | fcn_id == "" | is.na(timepoint) | n != 1)
if (nrow(pm_duplicate_keys) > 0) {
  stop("Canonical PM2.5 file must contain exactly one row per nonmissing fcn_id-timepoint.", call. = FALSE)
}

pm_household <- pm_adjusted_raw %>%
  clean_timepoint_arm() %>%
  transmute(
    fcn_id = as.character(fcn_id),
    timepoint,
    study_arm_overall = as.character(study_arm_overall),
    pm25_ambient_excess_default = as_number(pm25_ambient_excess_f075),
    pm25_ambient_excess_f000 = as_number(pm25_ambient_excess_f000),
    pm25_ambient_excess_f025 = as_number(pm25_ambient_excess_f025),
    pm25_ambient_excess_f050 = as_number(pm25_ambient_excess_f050),
    pm25_ambient_excess_f100 = as_number(pm25_ambient_excess_f100)
  )

if (any(is.na(pm_household$timepoint)) || any(!pm_household$study_arm_overall %in% arm_levels)) {
  stop("Canonical PM2.5 file contains an invalid timepoint or study arm.", call. = FALSE)
}

pm_outcome_cols <- c(
  "pm25_ambient_excess_default", "pm25_ambient_excess_f000",
  "pm25_ambient_excess_f025", "pm25_ambient_excess_f050",
  "pm25_ambient_excess_f100"
)
pm_complete_patterns <- pm_household %>% transmute(across(all_of(pm_outcome_cols), is.na)) %>% distinct()
if (nrow(pm_complete_patterns) != 1 || any(unlist(pm_complete_patterns[1, ]))) {
  stop("Canonical PM2.5 outcomes do not share one complete analytic population.", call. = FALSE)
}
################################################################################
# Panel construction and DRDID wrappers
################################################################################

xvars <- c("hh_size", "hh_per_structure")
min_rdid_arm_households <- as.integer(Sys.getenv(
  "RF105_RDID_MIN_ARM_HOUSEHOLDS",
  unset = "25"
))

baseline_covars <- survey_model_data %>%
  filter(timepoint == "baseline") %>%
  transmute(
    fcn_id = as.character(fcn_id),
    A = if_else(study_arm_overall == "intervention", 1, 0, missing = NA_real_),
    hh_size = as_number(hh_size),
    hh_per_structure = as_number(hh_per_structure)
  ) %>%
  group_by(fcn_id) %>%
  summarise(
    A = as_number(first_nonmissing(A)),
    across(all_of(xvars), ~ as_number(first_nonmissing(.x))),
    .groups = "drop"
  )

make_outcome_panel <- function(outcome_data, outcome_name, followup_timepoint) {
  baseline_y <- outcome_data %>%
    filter(timepoint == "baseline") %>%
    transmute(
      fcn_id = as.character(fcn_id),
      Z = as_number(.data[[outcome_name]])
    )

  followup_y <- outcome_data %>%
    filter(timepoint == followup_timepoint) %>%
    transmute(
      fcn_id = as.character(fcn_id),
      Y = as_number(.data[[outcome_name]])
    )

  baseline_covars %>%
    inner_join(baseline_y, by = "fcn_id") %>%
    inner_join(followup_y, by = "fcn_id") %>%
    filter(!is.na(A), !is.na(Z), !is.na(Y))
}

impute_drdid_covariates <- function(dat, x_vars) {
  dat %>%
    mutate(across(all_of(x_vars), ~ {
      x <- as_number(.x)
      fill <- mean(x, na.rm = TRUE)
      if (is.na(fill) || is.nan(fill)) {
        fill <- 0
      }
      ifelse(is.na(x), fill, x)
    }))
}

estimate_significance_local <- function(estimate, conf_low, conf_high, p_value) {
  case_when(
    is.na(estimate) | is.na(conf_low) | is.na(conf_high) ~ NA,
    conf_low > 0 | conf_high < 0 ~ TRUE,
    !is.na(p_value) & p_value < 0.05 ~ TRUE,
    TRUE ~ FALSE
  )
}

format_est_ci_local <- function(estimate, conf_low, conf_high) {
  if_else(
    is.na(estimate) | is.na(conf_low) | is.na(conf_high),
    NA_character_,
    sprintf("%.2f [%.2f, %.2f]", estimate, conf_low, conf_high)
  )
}

run_drdid_orientation <- function(panel, x_vars, orientation) {
  panel <- panel %>%
    filter(!is.na(Z), !is.na(Y), !is.na(A)) %>%
    impute_drdid_covariates(x_vars)

  n <- nrow(panel)
  n_intervention <- sum(panel$A == 1, na.rm = TRUE)
  n_comparison <- sum(panel$A == 0, na.rm = TRUE)

  if (orientation == "original_ATT") {
    target_estimand <- "ATT"
    target_population <- "intervention_arm_households"
    native_drdid_target <- "ATT for D = A, where A = 1 is original intervention"
    target_note <- paste(
      "Native DRDID ATT for original intervention households.",
      "This is informative but not target-aligned to the reviewed rDiD estimator."
    )
  } else if (orientation == "original_ATC_aligned") {
    target_estimand <- "ATC"
    target_population <- "comparison_arm_households"
    native_drdid_target <- "ATT after recoding D = 1 - A, then sign-flipped"
    target_note <- paste(
      "DRDID natively estimates the ATT for recoded comparison households.",
      "The estimate is sign-flipped so the contrast is original intervention minus original comparison,",
      "matching the reviewed rDiD/XGBoost target."
    )
  } else {
    stop("Unknown DRDID orientation: ", orientation, call. = FALSE)
  }

  if (n_intervention < min_rdid_arm_households ||
      n_comparison < min_rdid_arm_households) {
    return(list(
      estimate = NA_real_, se = NA_real_, conf.low = NA_real_,
      conf.high = NA_real_, p.value = NA_real_, n = n,
      target_estimand = target_estimand,
      target_population = target_population,
      treatment_contrast = "intervention_minus_comparison",
      native_drdid_target = native_drdid_target,
      note = paste0(
        "not estimated: DRDID panel below minimum arm size of ",
        min_rdid_arm_households, " households per arm; intervention=",
        n_intervention, ", comparison=", n_comparison
      )
    ))
  }

  X <- data.matrix(panel[, x_vars, drop = FALSE])

  if (orientation == "original_ATT") {
    D <- as_number(panel$A)
    sign_multiplier <- 1
  } else {
    D <- 1 - as_number(panel$A)
    sign_multiplier <- -1
  }

  fit <- tryCatch(
    DRDID::drdid_panel(
      y1 = as_number(panel$Y),
      y0 = as_number(panel$Z),
      D = D,
      covariates = X,
      boot = FALSE,
      inffunc = TRUE
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(list(
      estimate = NA_real_, se = NA_real_, conf.low = NA_real_,
      conf.high = NA_real_, p.value = NA_real_, n = n,
      target_estimand = target_estimand,
      target_population = target_population,
      treatment_contrast = "intervention_minus_comparison",
      native_drdid_target = native_drdid_target,
      note = paste("DRDID failed:", conditionMessage(fit))
    ))
  }

  raw_estimate <- as_number(fit$ATT)
  raw_lci <- as_number(fit$lci)
  raw_uci <- as_number(fit$uci)
  se <- as_number(fit$se)

  if (sign_multiplier == 1) {
    estimate <- raw_estimate
    conf_low <- raw_lci
    conf_high <- raw_uci
  } else {
    estimate <- -raw_estimate
    conf_low <- -raw_uci
    conf_high <- -raw_lci
  }

  p_value <- ifelse(is.na(se) || se == 0, NA_real_, 2 * pnorm(-abs(estimate / se)))

  list(
    estimate = estimate,
    se = se,
    conf.low = conf_low,
    conf.high = conf_high,
    p.value = p_value,
    n = n,
    target_estimand = target_estimand,
    target_population = target_population,
    treatment_contrast = "intervention_minus_comparison",
    native_drdid_target = native_drdid_target,
    note = target_note
  )
}

format_drdid_row <- function(res, outcome_info, estimator, orientation, panel) {
  scale_factor <- if (outcome_info$outcome_type == "binary") 100 else 1

  tibble(
    contrast = outcome_info$contrast,
    population = outcome_info$population,
    followup_timepoint = outcome_info$followup_timepoint,
    estimator = estimator,
    drdid_orientation = orientation,
    target_estimand = res$target_estimand,
    target_population = res$target_population,
    treatment_contrast = res$treatment_contrast,
    native_drdid_target = res$native_drdid_target,
    outcome = outcome_info$outcome,
    outcome_label = outcome_info$outcome_label,
    domain = outcome_info$domain,
    outcome_type = outcome_info$outcome_type,
    unit = outcome_info$unit,
    outcome_source = outcome_info$outcome_source,
    estimate = scale_factor * res$estimate,
    se = scale_factor * res$se,
    conf.low = scale_factor * res$conf.low,
    conf.high = scale_factor * res$conf.high,
    p.value = res$p.value,
    statistically_significant = estimate_significance_local(
      res$estimate, res$conf.low, res$conf.high, res$p.value
    ),
    estimate_ci = format_est_ci_local(
      scale_factor * res$estimate,
      scale_factor * res$conf.low,
      scale_factor * res$conf.high
    ),
    sample_size = res$n,
    n_households = res$n,
    n_intervention = sum(panel$A == 1, na.rm = TRUE),
    n_comparison = sum(panel$A == 0, na.rm = TRUE),
    minimum_arm_households = min_rdid_arm_households,
    passes_minimum_arm_households = n_intervention >= min_rdid_arm_households &
      n_comparison >= min_rdid_arm_households,
    note = res$note
  )
}

run_drdid_for_spec <- function(outcome_info) {
  outcome_data <- if (outcome_info$outcome_source == "survey_clean_final") {
    survey_model_data
  } else if (str_detect(outcome_info$outcome_source, "pm25_ambient_adjusted")) {
    pm_household
  } else {
    stop("Unsupported outcome_source for DRDID benchmark: ", outcome_info$outcome_source)
  }

  message("Running DRDID ", outcome_info$contrast, ": ", outcome_info$outcome)
  panel <- make_outcome_panel(
    outcome_data = outcome_data,
    outcome_name = outcome_info$outcome,
    followup_timepoint = outcome_info$followup_timepoint
  )

  att_res <- run_drdid_orientation(panel, xvars, "original_ATT")
  atc_res <- run_drdid_orientation(panel, xvars, "original_ATC_aligned")

  bind_rows(
    format_drdid_row(att_res, outcome_info, "DRDID_panel", "original_ATT", panel),
    format_drdid_row(atc_res, outcome_info, "DRDID_panel", "original_ATC_aligned", panel)
  )
}

################################################################################
# Run DRDID benchmarks and compare with rDiD/XGBoost
################################################################################

drdid_results_all <- purrr::map_dfr(seq_len(nrow(outcome_specs)), function(i) {
  run_drdid_for_spec(outcome_specs[i, ])
}) %>%
  arrange(contrast, domain, outcome, drdid_orientation)

safe_write_reviewed_csv(
  drdid_results_all,
  "table_DRDID_all_results.csv"
)

drdid_atc_aligned <- drdid_results_all %>%
  filter(drdid_orientation == "original_ATC_aligned") %>%
  select(
    contrast, followup_timepoint, outcome,
    drdid_estimator = estimator,
    drdid_target_estimand = target_estimand,
    drdid_target_population = target_population,
    drdid_treatment_contrast = treatment_contrast,
    drdid_native_target = native_drdid_target,
    drdid_estimate = estimate,
    drdid_se = se,
    drdid_conf.low = conf.low,
    drdid_conf.high = conf.high,
    drdid_p.value = p.value,
    drdid_statistically_significant = statistically_significant,
    drdid_estimate_ci = estimate_ci,
    drdid_sample_size = sample_size,
    drdid_n_intervention = n_intervention,
    drdid_n_comparison = n_comparison,
    drdid_note = note
  )

rdid_drdid_benchmark <- rdid_xgboost_reference %>%
  select(
    contrast, population, followup_timepoint, outcome, outcome_label, domain,
    outcome_type, unit, outcome_source,
    rdid_estimator = estimator,
    rdid_target_estimand, rdid_target_population, rdid_treatment_contrast,
    rdid_estimate = estimate,
    rdid_se = se,
    rdid_conf.low = conf.low,
    rdid_conf.high = conf.high,
    rdid_p.value = p.value,
    rdid_statistically_significant = statistically_significant,
    rdid_estimate_ci = estimate_ci,
    rdid_sample_size = sample_size,
    rdid_n_intervention = n_intervention,
    rdid_n_comparison = n_comparison,
    rdid_note = note,
    rdid_target_note
  ) %>%
  left_join(drdid_atc_aligned, by = c("contrast", "followup_timepoint", "outcome")) %>%
  mutate(
    estimate_difference_drdid_minus_rdid = drdid_estimate - rdid_estimate,
    absolute_estimate_difference = abs(estimate_difference_drdid_minus_rdid),
    relative_difference_to_abs_rdid = if_else(
      !is.na(rdid_estimate) & abs(rdid_estimate) > 0,
      estimate_difference_drdid_minus_rdid / abs(rdid_estimate),
      NA_real_
    ),
    direction_comparison = case_when(
      is.na(rdid_estimate) | is.na(drdid_estimate) ~ "direction_unavailable",
      rdid_estimate == 0 | drdid_estimate == 0 ~ "one_estimate_zero",
      sign(rdid_estimate) == sign(drdid_estimate) ~ "same_direction",
      TRUE ~ "opposite_direction"
    ),
    significance_comparison = case_when(
      is.na(rdid_statistically_significant) | is.na(drdid_statistically_significant) ~
        "significance_unavailable",
      rdid_statistically_significant & drdid_statistically_significant ~
        "significant_in_rdid_and_drdid",
      rdid_statistically_significant & !drdid_statistically_significant ~
        "significant_in_rdid_only",
      !rdid_statistically_significant & drdid_statistically_significant ~
        "significant_in_drdid_only",
      TRUE ~ "not_significant_in_either"
    ),
    ci_overlap = case_when(
      is.na(rdid_conf.low) | is.na(rdid_conf.high) |
        is.na(drdid_conf.low) | is.na(drdid_conf.high) ~ NA,
      pmax(rdid_conf.low, drdid_conf.low) <= pmin(rdid_conf.high, drdid_conf.high) ~ TRUE,
      TRUE ~ FALSE
    ),
    benchmark_note = paste(
      "DRDID benchmark uses the ATC-aligned orientation: D = 1 - A",
      "with sign flip, so both estimators target original intervention minus",
      "comparison among original comparison-arm households. DRDID native ATT",
      "rows are available in table_DRDID_all_results.csv. No ATE is estimated."
    )
  ) %>%
  arrange(contrast, domain, outcome_type, outcome_label)

safe_write_reviewed_csv(
  rdid_drdid_benchmark,
  "table_DRDID_rDID_benchmark.csv"
)

estimand_note_file <- file.path(
  dir_tables_qa,
  "table_DRDID_estimand_target_note.md"
)

safe_write_lines(
  c(
    "# DRDID benchmark estimand target note",
    "",
    paste0("Generated: ", Sys.time()),
    "",
    "## What DRDID estimates",
    "",
    paste(
      "DRDID::drdid_panel() estimates the average treatment effect on the treated",
      "(ATT) for the group coded D = 1 in a two-period panel DiD setup."
    ),
    "",
    "## What the reviewed rDiD/XGBoost estimator targets",
    "",
    paste(
      "The reviewed rDiD/XGBoost script codes A = 1 for intervention households",
      "and A = 0 for comparison households. Its reverse-DID influence expression",
      "uses pi0 = mean(1 - A), so the target population is the original",
      "comparison arm. With estimates signed as intervention minus comparison,",
      "the reviewed rDiD estimand is ATC, not ATT or ATE."
    ),
    "",
    "## Benchmark orientation used here",
    "",
    paste(
      "For the target-aligned benchmark, this script recodes D = 1 - A before",
      "calling DRDID::drdid_panel(), then sign-flips the resulting native ATT.",
      "The reported DRDID benchmark therefore estimates ATC for the original",
      "intervention-minus-comparison contrast and is directly comparable to the",
      "reviewed rDiD/XGBoost results."
    ),
    "",
    "## ATT and ATE status",
    "",
    paste(
      "The script also writes native DRDID ATT rows using D = A for transparency,",
      "but those rows are not the target-aligned benchmark. No ATE is estimated",
      "or benchmarked in this file."
    ),
    "",
    paste0("Reviewed rDiD input table: ", rdid_results_file),
    paste0("Ambient-adjusted PM2.5 input table: ", pm_adjusted_file)
  ),
  estimand_note_file
)
message("Wrote QA note: ", estimand_note_file)

benchmark_summary <- rdid_drdid_benchmark %>%
  summarise(
    n_benchmarked_rows = n(),
    n_drdid_estimated = sum(!is.na(drdid_estimate)),
    n_same_direction = sum(direction_comparison == "same_direction", na.rm = TRUE),
    n_opposite_direction = sum(direction_comparison == "opposite_direction", na.rm = TRUE),
    n_significant_in_rdid_only = sum(significance_comparison == "significant_in_rdid_only", na.rm = TRUE),
    n_significant_in_drdid_only = sum(significance_comparison == "significant_in_drdid_only", na.rm = TRUE),
    n_significant_in_both = sum(significance_comparison == "significant_in_rdid_and_drdid", na.rm = TRUE),
    median_absolute_estimate_difference = median(absolute_estimate_difference, na.rm = TRUE)
  )

safe_write_reviewed_csv(
  benchmark_summary,
  "table_DRDID_rDID_benchmark_summary.csv",
  subfolder = "qa"
)

message("DRDID benchmark complete.")
