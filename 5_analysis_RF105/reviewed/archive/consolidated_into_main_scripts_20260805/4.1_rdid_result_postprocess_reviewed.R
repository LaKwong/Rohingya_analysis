################################################################################
# RF105 reviewed rDiD/XGBoost result post-processing
#
# Purpose:
#   Rebuild downstream rDiD result tables and figures from the saved all-results
#   CSV files written by 4_rdid_xgboost_reviewed.R. This script does not refit
#   models. It is useful after adding legacy outcomes because the XGBoost fitting
#   step is slow, while comparison tables, split tables, QA counts, and figures
#   can be regenerated quickly from the completed all-results files.
#
# Inputs:
#   7_tables/RF105_reviewed_YYYYMMDD/rdid_xgboost_all_results_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/rdid_glm_sensitivity_all_results_reviewed.csv
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/rdid_xgboost_*_results_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/rdid_glm_sensitivity_*_results_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/rdid_xgboost_primary_vs_secondary_comparison_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/rdid_sample_counts_by_outcome_reviewed.csv
#   6_figures/RF105_reviewed_YYYYMMDD/rdid_xgboost_*_estimates_reviewed.png
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
config_file <- file.path(script_dir, "0_RF105_reviewed_config.R")
if (!file.exists(config_file)) {
  config_file <- file.path("5_analysis_RF105", "reviewed", "0_RF105_reviewed_config.R")
}
source(config_file)

read_required_reviewed_csv <- function(filename, subfolder = NULL) {
  in_dir <- if (is.null(subfolder)) dir_tables_reviewed else file.path(dir_tables_reviewed, subfolder)
  in_file <- file.path(in_dir, filename)
  if (!file.exists(in_file)) {
    stop("Required rDiD results file not found: ", in_file, call. = FALSE)
  }
  readr::read_csv(in_file, show_col_types = FALSE)
}

estimate_significance <- function(estimate, conf_low, conf_high, p_value) {
  case_when(
    !is.na(p_value) ~ p_value < 0.05,
    !is.na(conf_low) & !is.na(conf_high) ~ conf_low > 0 | conf_high < 0,
    TRUE ~ NA
  )
}

ensure_columns <- function(df, columns) {
  for (column in columns) {
    if (!column %in% names(df)) {
      df[[column]] <- NA
    }
  }
  df
}

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

rdid_xgboost_results <- read_required_reviewed_csv("rdid_xgboost_all_results_reviewed.csv")
rdid_glm_results <- read_required_reviewed_csv("rdid_glm_sensitivity_all_results_reviewed.csv")

required_result_columns <- c(
  "contrast", "population", "followup_timepoint", "outcome", "outcome_label",
  "domain", "outcome_type", "unit", "outcome_source", "estimate", "se",
  "conf.low", "conf.high", "p.value", "statistically_significant",
  "estimate_ci", "sample_size", "n_households", "n_intervention",
  "n_comparison", "note"
)

missing_xgb_columns <- setdiff(required_result_columns, names(rdid_xgboost_results))
missing_glm_columns <- setdiff(required_result_columns, names(rdid_glm_results))
if (length(missing_xgb_columns) > 0) {
  stop("XGBoost all-results file is missing columns: ", paste(missing_xgb_columns, collapse = ", "), call. = FALSE)
}
if (length(missing_glm_columns) > 0) {
  stop("GLM sensitivity all-results file is missing columns: ", paste(missing_glm_columns, collapse = ", "), call. = FALSE)
}

safe_write_reviewed_csv(
  filter(rdid_xgboost_results, contrast == "primary_baseline_midline"),
  "rdid_xgboost_primary_baseline_midline_results_reviewed.csv"
)
safe_write_reviewed_csv(
  filter(rdid_xgboost_results, contrast == "secondary_baseline_endline"),
  "rdid_xgboost_secondary_baseline_endline_results_reviewed.csv"
)
safe_write_reviewed_csv(
  filter(rdid_glm_results, contrast == "primary_baseline_midline"),
  "rdid_glm_sensitivity_primary_baseline_midline_results_reviewed.csv"
)
safe_write_reviewed_csv(
  filter(rdid_glm_results, contrast == "secondary_baseline_endline"),
  "rdid_glm_sensitivity_secondary_baseline_endline_results_reviewed.csv"
)

rdid_xgboost_primary_secondary_comparison <- rdid_xgboost_results %>%
  filter(contrast %in% c("primary_baseline_midline", "secondary_baseline_endline")) %>%
  mutate(
    analysis = recode(
      contrast,
      primary_baseline_midline = "primary_baseline_midline",
      secondary_baseline_endline = "secondary_baseline_endline"
    )
  ) %>%
  select(
    outcome, outcome_label, domain, outcome_type, unit, outcome_source,
    analysis, estimate, se, conf.low, conf.high, p.value,
    statistically_significant, estimate_ci, sample_size, n_intervention,
    n_comparison, note
  ) %>%
  pivot_wider(
    names_from = analysis,
    values_from = c(
      estimate, se, conf.low, conf.high, p.value, statistically_significant,
      estimate_ci, sample_size, n_intervention, n_comparison, note
    ),
    names_glue = "{.value}_{analysis}"
  ) %>%
  ensure_columns(c(
    "estimate_primary_baseline_midline", "estimate_secondary_baseline_endline",
    "statistically_significant_primary_baseline_midline",
    "statistically_significant_secondary_baseline_endline"
  )) %>%
  mutate(
    estimate_difference_secondary_minus_primary =
      estimate_secondary_baseline_endline - estimate_primary_baseline_midline,
    absolute_estimate_difference = abs(estimate_difference_secondary_minus_primary),
    significance_comparison = case_when(
      is.na(statistically_significant_primary_baseline_midline) |
        is.na(statistically_significant_secondary_baseline_endline) ~ "significance_unavailable",
      statistically_significant_primary_baseline_midline &
        statistically_significant_secondary_baseline_endline ~ "significant_in_primary_and_secondary",
      statistically_significant_primary_baseline_midline &
        !statistically_significant_secondary_baseline_endline ~ "significant_in_primary_only",
      !statistically_significant_primary_baseline_midline &
        statistically_significant_secondary_baseline_endline ~ "significant_in_secondary_only",
      TRUE ~ "not_significant_in_either"
    ),
    direction_comparison = case_when(
      is.na(estimate_primary_baseline_midline) | is.na(estimate_secondary_baseline_endline) ~ "direction_unavailable",
      sign(estimate_primary_baseline_midline) == sign(estimate_secondary_baseline_endline) ~ "same_direction",
      estimate_primary_baseline_midline == 0 | estimate_secondary_baseline_endline == 0 ~ "one_estimate_zero",
      TRUE ~ "opposite_direction"
    ),
    comparison_note = paste(
      "Estimate difference is descriptive only; the workflow does not estimate",
      "the covariance needed for a formal test comparing the primary and",
      "secondary rDiD estimates."
    )
  ) %>%
  arrange(domain, outcome_type, outcome_label)

safe_write_reviewed_csv(
  rdid_xgboost_primary_secondary_comparison,
  "rdid_xgboost_primary_vs_secondary_comparison_reviewed.csv"
)

rdid_sample_counts <- rdid_xgboost_results %>%
  select(
    contrast, population, followup_timepoint, outcome, outcome_label, domain,
    outcome_source, sample_size, n_households, n_intervention, n_comparison, note
  )
safe_write_reviewed_csv(rdid_sample_counts, "rdid_sample_counts_by_outcome_reviewed.csv", subfolder = "qa")

make_rdid_plot <- function(results, contrast_name, outcome_type_filter) {
  plot_data <- results %>%
    filter(
      contrast == contrast_name,
      outcome_type == outcome_type_filter,
      !is.na(estimate), !is.na(conf.low), !is.na(conf.high)
    ) %>%
    mutate(
      outcome_label = forcats::fct_reorder(outcome_label, estimate),
      significant_label = if_else(
        coalesce(statistically_significant, FALSE),
        "p < 0.05", "p >= 0.05 or unavailable"
      )
    )

  if (nrow(plot_data) == 0) {
    return(NULL)
  }

  ggplot(plot_data, aes(x = estimate, y = outcome_label, xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 0, color = "grey55", linewidth = 0.4) +
    geom_errorbar(aes(color = significant_label), orientation = "y", width = 0.18, linewidth = 0.55) +
    geom_point(aes(color = significant_label), size = 1.9) +
    facet_grid(domain ~ unit, scales = "free", space = "free_y") +
    scale_color_manual(values = c("p < 0.05" = "#D55E00", "p >= 0.05 or unavailable" = "#4E79A7")) +
    labs(x = "rDiD/XGBoost estimate with 95% CI", y = NULL, color = NULL) +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text.y = element_text(angle = 0)
    )
}

plot_specs <- tribble(
  ~contrast, ~outcome_type, ~filename, ~width, ~height,
  "primary_baseline_midline", "binary", "rdid_xgboost_primary_baseline_midline_binary_estimates_reviewed.png", 11, 22,
  "primary_baseline_midline", "continuous", "rdid_xgboost_primary_baseline_midline_continuous_estimates_reviewed.png", 9, 6,
  "secondary_baseline_endline", "binary", "rdid_xgboost_secondary_baseline_endline_binary_estimates_reviewed.png", 11, 22,
  "secondary_baseline_endline", "continuous", "rdid_xgboost_secondary_baseline_endline_continuous_estimates_reviewed.png", 9, 6
)

pwalk(plot_specs, function(contrast, outcome_type, filename, width, height) {
  fig <- make_rdid_plot(rdid_xgboost_results, contrast, outcome_type)
  if (!is.null(fig)) {
    save_reviewed_plot(fig, filename, width = width, height = height)
  }
})

find_previous_reviewed_file <- function(filename) {
  reviewed_dirs <- list.dirs(file.path(project_root, "7_tables"), full.names = TRUE, recursive = FALSE)
  reviewed_dirs <- reviewed_dirs[
    str_detect(basename(reviewed_dirs), "^RF105_reviewed_") &
      normalizePath(reviewed_dirs, winslash = "/", mustWork = FALSE) !=
        normalizePath(dir_tables_reviewed, winslash = "/", mustWork = FALSE)
  ]
  candidates <- file.path(reviewed_dirs, filename)
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) == 0) {
    return(NA_character_)
  }
  candidates[[order(candidates, decreasing = TRUE)[[1]]]]
}

standardize_previous_results <- function() {
  previous_health_file <- find_previous_reviewed_file("health_unadjusted_did_sensitivity_reviewed.csv")
  previous_pm_file <- find_previous_reviewed_file("pm25_household_mean_unadjusted_did_sensitivity.csv")
  out <- list()

  if (!is.na(previous_health_file)) {
    out$health <- readr::read_csv(previous_health_file, show_col_types = FALSE) %>%
      mutate(
        previous_file = previous_health_file,
        previous_estimator = "previous_reviewed_unadjusted_did",
        previous_population = if ("population" %in% names(.)) population else NA_character_,
        contrast = case_when(
          comparison_timepoint == "midline" ~ "primary_baseline_midline",
          comparison_timepoint == "endline" ~ "secondary_baseline_endline",
          TRUE ~ NA_character_
        ),
        previous_significant = estimate_significance(estimate, conf.low, conf.high, p.value)
      ) %>%
      select(
        outcome, contrast, previous_estimator, previous_file, previous_population,
        previous_estimate = estimate, previous_conf.low = conf.low,
        previous_conf.high = conf.high, previous_p.value = p.value,
        previous_significant
      )
  }

  if (!is.na(previous_pm_file)) {
    out$pm <- readr::read_csv(previous_pm_file, show_col_types = FALSE) %>%
      mutate(
        outcome = recode(outcome, mean_pm25 = "pm25_mean_lod10"),
        previous_file = previous_pm_file,
        previous_estimator = "previous_reviewed_unadjusted_did",
        previous_population = if ("population" %in% names(.)) population else NA_character_,
        contrast = case_when(
          comparison_timepoint == "midline" ~ "primary_baseline_midline",
          comparison_timepoint == "endline" ~ "secondary_baseline_endline",
          TRUE ~ NA_character_
        ),
        previous_significant = estimate_significance(estimate, conf.low, conf.high, p.value)
      ) %>%
      select(
        outcome, contrast, previous_estimator, previous_file, previous_population,
        previous_estimate = estimate, previous_conf.low = conf.low,
        previous_conf.high = conf.high, previous_p.value = p.value,
        previous_significant
      )
  }

  if (length(out) == 0) {
    return(tibble(
      outcome = character(), contrast = character(), previous_estimator = character(),
      previous_file = character(), previous_population = character(),
      previous_estimate = numeric(), previous_conf.low = numeric(),
      previous_conf.high = numeric(), previous_p.value = numeric(),
      previous_significant = logical()
    ))
  }

  bind_rows(out) %>%
    filter(!is.na(contrast)) %>%
    group_by(outcome, contrast) %>%
    arrange(desc(coalesce(previous_population, "") == "all_deduplicated_records"), previous_population, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
}

previous_results <- standardize_previous_results()
significance_change_audit <- rdid_xgboost_results %>%
  select(
    outcome, outcome_label, contrast, domain, unit,
    new_estimate = estimate, new_conf.low = conf.low, new_conf.high = conf.high,
    new_p.value = p.value, new_significant = statistically_significant
  ) %>%
  left_join(previous_results, by = c("outcome", "contrast")) %>%
  mutate(
    comparison_status = case_when(
      is.na(previous_estimator) ~ "no_matching_previous_result",
      is.na(previous_significant) | is.na(new_significant) ~ "significance_unknown",
      previous_significant == new_significant ~ "significance_unchanged",
      previous_significant & !new_significant ~ "no_longer_statistically_significant",
      !previous_significant & new_significant ~ "became_statistically_significant",
      TRUE ~ "significance_unknown"
    )
  ) %>%
  arrange(contrast, comparison_status, domain, outcome)

safe_write_reviewed_csv(significance_change_audit, "rdid_original_vs_new_significance_change_audit.csv")

changed_significance <- significance_change_audit %>%
  filter(comparison_status %in% c("no_longer_statistically_significant", "became_statistically_significant"))

summary_lines <- c(
  "# RF105 rDiD/XGBoost Original-vs-New Results Summary",
  "",
  paste0("Generated: ", Sys.time()),
  "",
  "## What Changed",
  "",
  "The reviewed workflow uses reverse difference-in-differences (rDiD) with cross-fit XGBoost nuisance models for outcomes that were previously assessed with DiD-style models in the reviewed scripts.",
  "",
  "Primary analysis is baseline-to-midline among households with both baseline and midline outcome data. Secondary analysis is baseline-to-endline among households with both baseline and endline outcome data.",
  "",
  "## Comparison Source",
  "",
  if (nrow(previous_results) == 0) {
    "No previous reviewed DiD output files were found for automated significance comparison."
  } else {
    paste0("Automated comparisons used previous reviewed output files for ", n_distinct(previous_results$outcome), " matched outcomes where available.")
  },
  "",
  "## Statistical Significance",
  "",
  paste0("Matched rows with unchanged significance: ", sum(significance_change_audit$comparison_status == "significance_unchanged", na.rm = TRUE)),
  paste0("Rows that became statistically significant: ", sum(significance_change_audit$comparison_status == "became_statistically_significant", na.rm = TRUE)),
  paste0("Rows that were no longer statistically significant: ", sum(significance_change_audit$comparison_status == "no_longer_statistically_significant", na.rm = TRUE)),
  paste0("Rows without a matching previous result: ", sum(significance_change_audit$comparison_status == "no_matching_previous_result", na.rm = TRUE)),
  "",
  "See `rdid_original_vs_new_significance_change_audit.csv` for row-level estimates, confidence intervals, p-values, and significance-change flags.",
  "",
  "## Rows With Changed Statistical Significance",
  ""
)

if (nrow(changed_significance) == 0) {
  summary_lines <- c(summary_lines, "No matched rows changed statistical significance.")
} else {
  changed_lines <- changed_significance %>%
    mutate(line = paste0("- ", contrast, ": ", outcome_label, " (", comparison_status, ")")) %>%
    pull(line)
  summary_lines <- c(summary_lines, changed_lines)
}

summary_file <- file.path(dir_tables_reviewed, "rdid_original_vs_new_results_summary.md")
writeLines(summary_lines, summary_file)
message("Wrote summary document: ", summary_file)

message("RF105 reviewed rDiD/XGBoost result post-processing complete.")