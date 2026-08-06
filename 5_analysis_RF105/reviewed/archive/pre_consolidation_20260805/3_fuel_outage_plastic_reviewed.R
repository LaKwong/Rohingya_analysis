################################################################################
# RF105 reviewed fuel use, LPG outage, and plastic burning analyses
#
# Purpose:
#   Recreate the fuel-use summaries from the RF105A scripts using the final
#   cleaned household survey file, while documenting variable availability,
#   sample counts, and sensitivity analyses.
#
# Input:
#   4_data/clean_final/survey_refugee_household.rds
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/fuel_*.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/fuel_*.csv
#   6_figures/RF105_reviewed_YYYYMMDD/fuel_*.png
#
# Sensitivity analyses:
#   1. Complete three-survey household population versus all deduplicated
#      household-timepoint records.
#   2. Non-LPG cooking frequency exclusions at >60, >100, >200 days/month,
#      and no upper exclusion.
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

survey_data_raw <- readRDS(file_survey_refugee_household) %>%
  add_rf105_aliases()

analysis_population <- make_analysis_population(survey_data_raw, id_var = "fcn_id")

write_reviewed_csv(
  analysis_population$sample_counts,
  "fuel_sample_counts_after_deduplication.csv",
  subfolder = "qa"
)

################################################################################
# Fuel variable definitions
################################################################################

fuel_ever_vars <- c(
  "fuel_ever_lpg",
  "fuel_ever_wood",
  "fuel_ever_dung",
  "fuel_ever_sawdust",
  "fuel_ever_crop",
  "fuel_ever_gather_scraps",
  "fuel_ever_kero",
  "fuel_ever_charcoal",
  "fuel_ever_plastic",
  "fuel_ever_other"
)

fuel_30_vars <- c(
  "fuel_30_lpg",
  "fuel_30_wood",
  "fuel_30_dung",
  "fuel_30_sawdust",
  "fuel_30_crop",
  "fuel_30_gather_scraps",
  "fuel_30_kero",
  "fuel_30_charcoal",
  "fuel_30_plastic",
  "fuel_30_other"
)

fuel_labels <- c(
  fuel_ever_lpg = "LPG",
  fuel_ever_wood = "Wood",
  fuel_ever_dung = "Animal dung",
  fuel_ever_sawdust = "Sawdust",
  fuel_ever_crop = "Crop residue",
  fuel_ever_gather_scraps = "Gathered scraps",
  fuel_ever_kero = "Kerosene",
  fuel_ever_charcoal = "Charcoal",
  fuel_ever_plastic = "Plastic",
  fuel_ever_other = "Other",
  fuel_30_lpg = "LPG",
  fuel_30_wood = "Wood",
  fuel_30_dung = "Animal dung",
  fuel_30_sawdust = "Sawdust",
  fuel_30_crop = "Crop residue",
  fuel_30_gather_scraps = "Gathered scraps",
  fuel_30_kero = "Kerosene",
  fuel_30_charcoal = "CRH/charcoal",
  fuel_30_plastic = "Plastic",
  fuel_30_other = "Other"
)

fuel_related_vars <- c(
  fuel_ever_vars,
  fuel_30_vars,
  "lpg_runout",
  "lpg_runout_days",
  "fuel_use_non_lpg",
  "fuel_use_non_lpg_freq_cook",
  "plastic_cook",
  "fuel_cant_afford"
)

write_reviewed_csv(
  flag_missing_vars(
    survey_data_raw,
    fuel_related_vars,
    "RF105 fuel-use variables after clean_final aliases"
  ),
  "fuel_variable_availability.csv",
  subfolder = "qa"
)

summarise_fuel_use <- function(df, fuel_vars, recall_period, population_label) {
  fuel_vars_available <- fuel_vars[fuel_vars %in% names(df)]

  df %>%
    select(any_of(c("fcn_id", "timepoint", "study_arm_overall", fuel_vars_available))) %>%
    pivot_longer(
      cols = all_of(fuel_vars_available),
      names_to = "fuel_variable",
      values_to = "used_raw",
      values_transform = list(used_raw = as.character)
    ) %>%
    mutate(
      used = make_yn(used_raw),
      fuel_type = recode(fuel_variable, !!!fuel_labels, .default = fuel_variable),
      recall_period = recall_period,
      population = population_label
    ) %>%
    group_by(population, recall_period, timepoint, study_arm_overall,
             fuel_variable, fuel_type) %>%
    summarise(
      n_nonmissing = sum(!is.na(used)),
      n_used = sum(used == 1, na.rm = TRUE),
      pct_used = 100 * n_used / n_nonmissing,
      .groups = "drop"
    ) %>%
    mutate(pct_used = if_else(is.nan(pct_used), NA_real_, pct_used)) %>%
    arrange(population, recall_period, fuel_type, timepoint, study_arm_overall)
}

survey_complete <- analysis_population$complete_3_survey
survey_all_dedup <- analysis_population$all_deduplicated

fuel_ever_complete <- summarise_fuel_use(
  survey_complete, fuel_ever_vars, "ever", "complete_3_survey"
)

fuel_30_complete <- summarise_fuel_use(
  survey_complete, fuel_30_vars, "past_30_days", "complete_3_survey"
)

fuel_30_all_dedup <- summarise_fuel_use(
  survey_all_dedup, fuel_30_vars, "past_30_days", "all_deduplicated_records"
)

fuel_summary <- bind_rows(
  fuel_ever_complete,
  fuel_30_complete,
  fuel_30_all_dedup
)

write_reviewed_csv(fuel_summary, "fuel_use_summary_reviewed.csv")

################################################################################
# Fuel-use figure for complete three-survey households
################################################################################

fuel_plot_data <- fuel_30_complete %>%
  filter(!is.na(pct_used)) %>%
  mutate(
    fuel_type = fct_reorder(fuel_type, pct_used, .fun = max, .desc = TRUE)
  )

fig_fuel_30 <- ggplot(
  fuel_plot_data,
  aes(x = timepoint, y = pct_used, fill = study_arm_overall)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  facet_wrap(~ fuel_type, ncol = 5) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 25),
    labels = label_number(suffix = "%")
  ) +
  scale_fill_manual(
    values = c(comparison = "#4E79A7", intervention = "#F28E2B"),
    na.translate = FALSE
  ) +
  labs(
    x = NULL,
    y = "Households reporting fuel use in past 30 days",
    fill = "Study arm"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

save_reviewed_plot(fig_fuel_30, "fuel_use_past_30_days_reviewed.png",
                   width = 10, height = 6)

################################################################################
# LPG outage summaries
################################################################################

runout_vars <- c("lpg_runout", "lpg_runout_days")
runout_vars_available <- runout_vars[runout_vars %in% names(survey_complete)]

if (length(runout_vars_available) > 0) {
  lpg_runout_summary <- survey_complete %>%
    mutate(
      lpg_runout_yn = if ("lpg_runout" %in% names(.)) make_yn(lpg_runout) else NA_integer_,
      lpg_runout_days_num = if ("lpg_runout_days" %in% names(.)) {
        suppressWarnings(as.numeric(lpg_runout_days))
      } else {
        NA_real_
      }
    ) %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise(
      n_households = n(),
      n_lpg_runout_nonmissing = sum(!is.na(lpg_runout_yn)),
      n_lpg_runout = sum(lpg_runout_yn == 1, na.rm = TRUE),
      pct_lpg_runout = 100 * n_lpg_runout / n_lpg_runout_nonmissing,
      mean_lpg_runout_days = mean(lpg_runout_days_num, na.rm = TRUE),
      median_lpg_runout_days = median(lpg_runout_days_num, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pct_lpg_runout = if_else(is.nan(pct_lpg_runout), NA_real_, pct_lpg_runout),
      mean_lpg_runout_days = if_else(
        is.nan(mean_lpg_runout_days), NA_real_, mean_lpg_runout_days
      )
    )

  write_reviewed_csv(lpg_runout_summary, "fuel_lpg_runout_summary_reviewed.csv")
}

################################################################################
# Plastic and non-LPG cooking sensitivity analyses
################################################################################

non_lpg_vars <- c(
  "fuel_use_non_lpg",
  "fuel_use_non_lpg_freq_cook",
  "plastic_cook",
  "fuel_cant_afford"
)

non_lpg_vars_available <- non_lpg_vars[non_lpg_vars %in% names(survey_complete)]

if (length(non_lpg_vars_available) > 0) {
  non_lpg_base <- survey_complete %>%
    mutate(
      non_lpg_yn = if ("fuel_use_non_lpg" %in% names(.)) {
        make_yn(fuel_use_non_lpg)
      } else {
        NA_integer_
      },
      plastic_cook_yn = if ("plastic_cook" %in% names(.)) {
        make_yn(plastic_cook)
      } else {
        NA_integer_
      },
      fuel_cant_afford_yn = if ("fuel_cant_afford" %in% names(.)) {
        make_yn(fuel_cant_afford)
      } else {
        NA_integer_
      },
      non_lpg_cook_days = if ("fuel_use_non_lpg_freq_cook" %in% names(.)) {
        suppressWarnings(as.numeric(fuel_use_non_lpg_freq_cook))
      } else {
        NA_real_
      }
    )

  threshold_grid <- tibble(
    exclusion_rule = c(
      "exclude_non_lpg_days_gt_60",
      "exclude_non_lpg_days_gt_100",
      "exclude_non_lpg_days_gt_200_main",
      "no_upper_exclusion"
    ),
    max_non_lpg_days = c(60, 100, 200, Inf)
  )

  non_lpg_sensitivity <- threshold_grid %>%
    mutate(data = map(max_non_lpg_days, function(max_days) {
      if (is.infinite(max_days)) {
        non_lpg_base
      } else {
        non_lpg_base %>%
          filter(is.na(non_lpg_cook_days) | non_lpg_cook_days <= max_days)
      }
    })) %>%
    select(exclusion_rule, max_non_lpg_days, data) %>%
    unnest(data) %>%
    group_by(exclusion_rule, max_non_lpg_days, timepoint, study_arm_overall) %>%
    summarise(
      n_households = n(),
      n_non_lpg_nonmissing = sum(!is.na(non_lpg_yn)),
      n_non_lpg = sum(non_lpg_yn == 1, na.rm = TRUE),
      pct_non_lpg = 100 * n_non_lpg / n_non_lpg_nonmissing,
      n_plastic_nonmissing = sum(!is.na(plastic_cook_yn)),
      n_plastic = sum(plastic_cook_yn == 1, na.rm = TRUE),
      pct_plastic = 100 * n_plastic / n_plastic_nonmissing,
      n_cannot_afford_nonmissing = sum(!is.na(fuel_cant_afford_yn)),
      n_cannot_afford = sum(fuel_cant_afford_yn == 1, na.rm = TRUE),
      pct_cannot_afford = 100 * n_cannot_afford / n_cannot_afford_nonmissing,
      mean_non_lpg_cook_days = mean(non_lpg_cook_days, na.rm = TRUE),
      median_non_lpg_cook_days = median(non_lpg_cook_days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(
      starts_with("pct_"),
      ~ if_else(is.nan(.x), NA_real_, .x)
    ))

  write_reviewed_csv(
    non_lpg_sensitivity,
    "fuel_non_lpg_plastic_sensitivity_reviewed.csv"
  )
}

