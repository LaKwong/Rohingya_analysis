################################################################################
# RF105 reviewed stove-monitor uptake and cooking-time analyses
#
# Purpose:
#   Recreate LPG/biomass stove-use summaries using the final cleaned daily
#   Geocene stove-use file.
#
# Input:
#   4_data/clean_final/stove_use_geocene_refugee_daily.rds
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/stove_*.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/stove_*.csv
#   6_figures/RF105_reviewed_YYYYMMDD/stove_*.png
#
# Sensitivity analyses:
#   1. Main analysis uses all cleaned daily records.
#   2. Sensitivity excludes the first 7, 14, 30, and 60 days after first
#      receiving LPG, because the earliest monitored days may reflect uptake
#      transition behavior rather than stable use.
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
  config_file <- file.path("5_analysis_RF105", "0_RF105_reviewed_config.R")
}
source(config_file)

stove_data_raw <- readRDS(file_stove_daily) %>%
  clean_timepoint_arm()

stove_required_vars <- c(
  "fcn_id",
  "hh_id",
  "timepoint",
  "study_arm_overall",
  "date",
  "days_after_first_receiving",
  "months_after_first_receiving_numeric",
  "cooking_events_with_lpg",
  "cooking_events_with_biomass",
  "stove_on_min_sum_lpg",
  "stove_on_min_sum_biomass",
  "stove_on_min_sum_total",
  "exclusive_biomass",
  "exclusive_lpg",
  "mixed_use"
)

write_reviewed_csv(
  flag_missing_vars(
    stove_data_raw,
    stove_required_vars,
    "RF105 daily Geocene stove-use variables"
  ),
  "stove_variable_availability.csv",
  subfolder = "qa"
)

stove_data <- stove_data_raw %>%
  mutate(
    date = as.Date(date),
    days_after_first_receiving =
      suppressWarnings(as.numeric(days_after_first_receiving)),
    months_after_first_receiving_numeric =
      suppressWarnings(as.numeric(months_after_first_receiving_numeric)),
    cooking_events_with_lpg =
      suppressWarnings(as.numeric(cooking_events_with_lpg)),
    cooking_events_with_biomass =
      suppressWarnings(as.numeric(cooking_events_with_biomass)),
    stove_on_min_sum_lpg =
      suppressWarnings(as.numeric(stove_on_min_sum_lpg)),
    stove_on_min_sum_biomass =
      suppressWarnings(as.numeric(stove_on_min_sum_biomass)),
    stove_on_min_sum_total =
      suppressWarnings(as.numeric(stove_on_min_sum_total)),
    exclusive_biomass = as.logical(exclusive_biomass),
    exclusive_lpg = as.logical(exclusive_lpg),
    mixed_use = as.logical(mixed_use)
  ) %>%
  filter(!is.na(date), !is.na(timepoint), !is.na(study_arm_overall))

stove_duplicate_days <- stove_data %>%
  add_count(fcn_id, date, name = "n_records_for_fcn_date") %>%
  filter(n_records_for_fcn_date > 1) %>%
  select(
    fcn_id, hh_id, date, timepoint, study_arm_overall,
    n_records_for_fcn_date, raw_source_file
  ) %>%
  arrange(fcn_id, date)

write_reviewed_csv(
  stove_duplicate_days,
  "stove_duplicate_fcn_id_dates.csv",
  subfolder = "qa"
)

stove_sample_counts <- stove_data %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_daily_records = n(),
    n_households = n_distinct(fcn_id),
    first_monitoring_date = min(date, na.rm = TRUE),
    last_monitoring_date = max(date, na.rm = TRUE),
    .groups = "drop"
  )

write_reviewed_csv(stove_sample_counts, "stove_sample_counts_reviewed.csv")

################################################################################
# Daily stove-use summaries
################################################################################

stove_daily_summary <- stove_data %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_daily_records = n(),
    n_households = n_distinct(fcn_id),
    n_exclusive_lpg_days = sum(exclusive_lpg, na.rm = TRUE),
    pct_exclusive_lpg_days = 100 * n_exclusive_lpg_days / n_daily_records,
    n_exclusive_biomass_days = sum(exclusive_biomass, na.rm = TRUE),
    pct_exclusive_biomass_days = 100 * n_exclusive_biomass_days / n_daily_records,
    n_mixed_use_days = sum(mixed_use, na.rm = TRUE),
    pct_mixed_use_days = 100 * n_mixed_use_days / n_daily_records,
    mean_lpg_events_per_day = mean(cooking_events_with_lpg, na.rm = TRUE),
    mean_biomass_events_per_day = mean(cooking_events_with_biomass, na.rm = TRUE),
    mean_lpg_minutes_per_day = mean(stove_on_min_sum_lpg, na.rm = TRUE),
    mean_biomass_minutes_per_day = mean(stove_on_min_sum_biomass, na.rm = TRUE),
    mean_total_stove_minutes_per_day = mean(stove_on_min_sum_total, na.rm = TRUE),
    .groups = "drop"
  )

write_reviewed_csv(stove_daily_summary, "stove_daily_summary_reviewed.csv")

################################################################################
# Sensitivity: exclude early days after first LPG receipt
################################################################################

early_day_thresholds <- tibble(
  sensitivity = c(
    "main_all_days",
    "exclude_first_7_days",
    "exclude_first_14_days",
    "exclude_first_30_days",
    "exclude_first_60_days"
  ),
  min_days_after_first_receiving = c(-Inf, 7, 14, 30, 60)
)

stove_early_day_sensitivity <- early_day_thresholds %>%
  mutate(data = map(min_days_after_first_receiving, function(min_days) {
    if (is.infinite(min_days)) {
      stove_data
    } else {
      stove_data %>%
        filter(
          is.na(days_after_first_receiving) |
            days_after_first_receiving >= min_days
        )
    }
  })) %>%
  unnest(data) %>%
  group_by(sensitivity, min_days_after_first_receiving,
           timepoint, study_arm_overall) %>%
  summarise(
    n_daily_records = n(),
    n_households = n_distinct(fcn_id),
    pct_exclusive_lpg_days = 100 * sum(exclusive_lpg, na.rm = TRUE) / n(),
    pct_exclusive_biomass_days = 100 * sum(exclusive_biomass, na.rm = TRUE) / n(),
    pct_mixed_use_days = 100 * sum(mixed_use, na.rm = TRUE) / n(),
    mean_lpg_minutes_per_day = mean(stove_on_min_sum_lpg, na.rm = TRUE),
    mean_biomass_minutes_per_day = mean(stove_on_min_sum_biomass, na.rm = TRUE),
    mean_total_stove_minutes_per_day = mean(stove_on_min_sum_total, na.rm = TRUE),
    .groups = "drop"
  )

write_reviewed_csv(
  stove_early_day_sensitivity,
  "stove_early_day_sensitivity_reviewed.csv"
)

################################################################################
# Month-after-receipt summaries and figures
################################################################################

stove_month_summary <- stove_data %>%
  filter(!is.na(months_after_first_receiving_numeric)) %>%
  group_by(months_after_first_receiving_numeric, study_arm_overall) %>%
  summarise(
    n_daily_records = n(),
    n_households = n_distinct(fcn_id),
    pct_exclusive_lpg_days = 100 * sum(exclusive_lpg, na.rm = TRUE) / n(),
    pct_exclusive_biomass_days = 100 * sum(exclusive_biomass, na.rm = TRUE) / n(),
    pct_mixed_use_days = 100 * sum(mixed_use, na.rm = TRUE) / n(),
    mean_lpg_minutes_per_day = mean(stove_on_min_sum_lpg, na.rm = TRUE),
    mean_biomass_minutes_per_day = mean(stove_on_min_sum_biomass, na.rm = TRUE),
    mean_total_stove_minutes_per_day = mean(stove_on_min_sum_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_households >= 3)

write_reviewed_csv(stove_month_summary, "stove_month_summary_reviewed.csv")

fig_exclusive_lpg <- ggplot(
  stove_month_summary,
  aes(
    x = months_after_first_receiving_numeric,
    y = pct_exclusive_lpg_days,
    color = study_arm_overall
  )
) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.7) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = label_number(suffix = "%")
  ) +
  scale_color_manual(
    values = c(comparison = "#4E79A7", intervention = "#F28E2B"),
    na.translate = FALSE
  ) +
  labs(
    x = "Months after first receiving LPG",
    y = "Daily records with exclusive LPG use",
    color = "Study arm"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

save_reviewed_plot(
  fig_exclusive_lpg,
  "stove_exclusive_lpg_by_month_reviewed.png",
  width = 7,
  height = 4.5
)

stove_minutes_long <- stove_month_summary %>%
  select(
    months_after_first_receiving_numeric,
    study_arm_overall,
    mean_lpg_minutes_per_day,
    mean_biomass_minutes_per_day
  ) %>%
  pivot_longer(
    cols = c(mean_lpg_minutes_per_day, mean_biomass_minutes_per_day),
    names_to = "fuel_type",
    values_to = "mean_minutes_per_day"
  ) %>%
  mutate(
    fuel_type = recode(
      fuel_type,
      mean_lpg_minutes_per_day = "LPG",
      mean_biomass_minutes_per_day = "Biomass"
    )
  )

fig_stove_minutes <- ggplot(
  stove_minutes_long,
  aes(
    x = months_after_first_receiving_numeric,
    y = mean_minutes_per_day,
    color = fuel_type
  )
) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ study_arm_overall) +
  scale_color_manual(values = c(LPG = "#F28E2B", Biomass = "#4E79A7")) +
  labs(
    x = "Months after first receiving LPG",
    y = "Mean stove-on minutes per day",
    color = "Fuel type"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

save_reviewed_plot(
  fig_stove_minutes,
  "stove_minutes_by_fuel_and_month_reviewed.png",
  width = 7,
  height = 4.5
)

stove_events_long <- stove_data %>%
  select(
    fcn_id, timepoint, study_arm_overall, date,
    cooking_events_with_lpg, cooking_events_with_biomass
  ) %>%
  pivot_longer(
    cols = c(cooking_events_with_lpg, cooking_events_with_biomass),
    names_to = "fuel_type",
    values_to = "cooking_events"
  ) %>%
  mutate(
    fuel_type = recode(
      fuel_type,
      cooking_events_with_lpg = "LPG",
      cooking_events_with_biomass = "Biomass"
    )
  )

stove_events_summary <- stove_events_long %>%
  group_by(timepoint, study_arm_overall, fuel_type) %>%
  summarise(
    n_daily_records = sum(!is.na(cooking_events)),
    n_households = n_distinct(fcn_id[!is.na(cooking_events)]),
    mean_events_per_day = mean(cooking_events, na.rm = TRUE),
    median_events_per_day = median(cooking_events, na.rm = TRUE),
    p25_events_per_day = quantile(cooking_events, 0.25, na.rm = TRUE),
    p75_events_per_day = quantile(cooking_events, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

write_reviewed_csv(stove_events_summary, "stove_events_summary_reviewed.csv")

