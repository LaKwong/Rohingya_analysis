################################################################################
# PM2.5 Anomaly Household Comparison Figure
#
# Purpose:
#   Create a de-identified diagnostic figure comparing the two manually reviewed
#   endline PM2.5 anomaly traces with the five non-anomaly endline household
#   monitoring windows that have the next-highest peak PM2.5 values, plus the
#   average time-aligned PM2.5 pattern across all endline households.
#
# Inputs:
#   4_data/clean_final/pm25_pats_refugee_indoor.rds
#
# Outputs:
#   6_figures/RF105_reviewed_YYYYMMDD/fig_pm25_anomaly_household_comparison.png
#   6_figures/RF105_reviewed_YYYYMMDD/fig_pm25_anomaly_household_comparison.tiff
#   7_tables/RF105_reviewed_YYYYMMDD/qa/table_pm25_anomaly_comparison_window_key_internal.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/table_pm25_anomaly_comparison_hourly_trace_internal.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/table_pm25_anomaly_comparison_all_household_average.csv
################################################################################

required_packages <- c("dplyr", "ggplot2", "lubridate", "readr", "scales", "tidyr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(scales)
library(tidyr)
timepoint_levels <- c("baseline", "midline", "endline")
as_ordered_timepoint <- function(x) {
  factor(trimws(tolower(as.character(x))), levels = timepoint_levels, ordered = TRUE)
}

project_root <- "G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis"
analysis_date <- format(Sys.Date(), "%Y%m%d")
figure_dir <- file.path(project_root, "6_figures", paste0("RF105_reviewed_", analysis_date))
table_dir <- file.path(project_root, "7_tables", paste0("RF105_reviewed_", analysis_date))
qa_dir <- file.path(table_dir, "qa")

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

input_pm_path <- file.path(
  project_root,
  "4_data", "clean_final", "pm25_pats_refugee_indoor_anomaly_retained_sensitivity.rds"
)
if (!file.exists(input_pm_path)) {
  stop("Input file not found: ", input_pm_path)
}

pm <- readRDS(input_pm_path)
required_columns <- c(
  "timepoint", "study_arm_overall", "hh_id", "fcn_id", "raw_source_file",
  "PM_monitor", "dateTime", "pm25_ug_m3", "pm_anomaly_review_flag"
)
missing_columns <- setdiff(required_columns, names(pm))
if (length(missing_columns) > 0) {
  stop("Missing required columns in PM dataset: ", paste(missing_columns, collapse = ", "))
}

reviewed_anomaly_hh_ids <- c("10FF33107012", "4EPP10280794")
window_vars <- c("timepoint", "study_arm_overall", "hh_id", "fcn_id", "raw_source_file", "PM_monitor")

pm_endline <- pm %>%
  mutate(
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    hh_id = as.character(hh_id),
    fcn_id = as.character(fcn_id),
    raw_source_file = as.character(raw_source_file),
    PM_monitor = as.character(PM_monitor),
    dateTime = as.POSIXct(dateTime, tz = "UTC"),
    pm25_ug_m3 = as.numeric(pm25_ug_m3),
    pm_anomaly_review_flag = as.logical(pm_anomaly_review_flag)
  ) %>%
  filter(
    timepoint == "endline",
    study_arm_overall %in% c("intervention", "comparison"),
    !is.na(dateTime),
    !is.na(pm25_ug_m3),
    is.finite(pm25_ug_m3),
    pm25_ug_m3 > 0,
    !is.na(hh_id),
    nzchar(hh_id),
    !is.na(raw_source_file),
    nzchar(raw_source_file)
  )

if (nrow(pm_endline) == 0) {
  stop("No eligible endline indoor PM2.5 rows found in anomaly-retained dataset.")
}

window_summary <- pm_endline %>%
  group_by(across(all_of(window_vars))) %>%
  summarise(
    n_rows = n(),
    start_datetime = min(dateTime, na.rm = TRUE),
    end_datetime = max(dateTime, na.rm = TRUE),
    duration_hours = as.numeric(difftime(max(dateTime, na.rm = TRUE), min(dateTime, na.rm = TRUE), units = "hours")),
    mean_pm25_ug_m3 = mean(pm25_ug_m3, na.rm = TRUE),
    median_pm25_ug_m3 = median(pm25_ug_m3, na.rm = TRUE),
    p95_pm25_ug_m3 = as.numeric(stats::quantile(pm25_ug_m3, 0.95, na.rm = TRUE, names = FALSE)),
    p99_pm25_ug_m3 = as.numeric(stats::quantile(pm25_ug_m3, 0.99, na.rm = TRUE, names = FALSE)),
    max_pm25_ug_m3 = max(pm25_ug_m3, na.rm = TRUE),
    pct_pm_ge_1000 = mean(pm25_ug_m3 >= 1000, na.rm = TRUE) * 100,
    pct_pm_ge_5000 = mean(pm25_ug_m3 >= 5000, na.rm = TRUE) * 100,
    any_pm_anomaly_review_flag = any(pm_anomaly_review_flag, na.rm = TRUE),
    .groups = "drop"
  )

reviewed_anomaly_windows <- window_summary %>%
  filter(hh_id %in% reviewed_anomaly_hh_ids) %>%
  arrange(desc(max_pm25_ug_m3), desc(p99_pm25_ug_m3), desc(mean_pm25_ug_m3)) %>%
  mutate(
    selection_group = "Reviewed anomaly trace",
    selection_rank = row_number(),
    display_label_base = paste0("Reviewed anomaly ", selection_rank)
  )

if (nrow(reviewed_anomaly_windows) != length(reviewed_anomaly_hh_ids)) {
  warning(
    "Expected ", length(reviewed_anomaly_hh_ids),
    " reviewed anomaly windows, found ", nrow(reviewed_anomaly_windows), "."
  )
}

next_highest_windows <- window_summary %>%
  filter(!hh_id %in% reviewed_anomaly_hh_ids) %>%
  arrange(desc(max_pm25_ug_m3), desc(p99_pm25_ug_m3), desc(mean_pm25_ug_m3)) %>%
  slice_head(n = 5) %>%
  mutate(
    selection_group = "Top non-anomaly peak trace",
    selection_rank = row_number(),
    display_label_base = paste0("Top non-anomaly peak ", selection_rank)
  )

selected_windows <- bind_rows(reviewed_anomaly_windows, next_highest_windows) %>%
  mutate(
    display_label = paste0(
      display_label_base,
      "\npeak=", scales::comma(round(max_pm25_ug_m3)),
      " ug/m3; arm=", study_arm_overall
    ),
    display_order = row_number()
  )

if (nrow(selected_windows) == 0) {
  stop("No anomaly or high-peak comparison windows selected.")
}

readr::write_csv(
  selected_windows,
  file.path(qa_dir, "table_pm25_anomaly_comparison_window_key_internal.csv"),
  na = ""
)

pm_endline_with_window_start <- pm_endline %>%
  inner_join(
    window_summary %>%
      select(all_of(window_vars), start_datetime_window = start_datetime),
    by = window_vars
  ) %>%
  mutate(
    elapsed_hour = as.numeric(difftime(dateTime, start_datetime_window, units = "hours")),
    elapsed_hour_bin = floor(elapsed_hour)
  ) %>%
  filter(elapsed_hour_bin >= 0)

all_household_hours <- pm_endline_with_window_start %>%
  group_by(across(all_of(window_vars)), elapsed_hour_bin) %>%
  summarise(
    pm25_household_hour_mean = mean(pm25_ug_m3, na.rm = TRUE),
    n_minute_records = n(),
    .groups = "drop"
  )

all_household_average <- all_household_hours %>%
  group_by(elapsed_hour_bin) %>%
  summarise(
    n_household_hours = n(),
    n_households = n_distinct(hh_id),
    mean_pm25_ug_m3 = mean(pm25_household_hour_mean, na.rm = TRUE),
    median_pm25_ug_m3 = median(pm25_household_hour_mean, na.rm = TRUE),
    p25_pm25_ug_m3 = as.numeric(stats::quantile(pm25_household_hour_mean, 0.25, na.rm = TRUE, names = FALSE)),
    p75_pm25_ug_m3 = as.numeric(stats::quantile(pm25_household_hour_mean, 0.75, na.rm = TRUE, names = FALSE)),
    .groups = "drop"
  )

selected_household_hours <- all_household_hours %>%
  inner_join(
    selected_windows %>%
      select(all_of(window_vars), selection_group, selection_rank, display_label, display_order),
    by = window_vars
  ) %>%
  arrange(display_order, elapsed_hour_bin)

readr::write_csv(
  selected_household_hours,
  file.path(qa_dir, "table_pm25_anomaly_comparison_hourly_trace_internal.csv"),
  na = ""
)
readr::write_csv(
  all_household_average,
  file.path(qa_dir, "table_pm25_anomaly_comparison_all_household_average.csv"),
  na = ""
)

plot_hour_max <- min(72, max(selected_household_hours$elapsed_hour_bin, na.rm = TRUE))
selected_plot <- selected_household_hours %>%
  filter(elapsed_hour_bin <= plot_hour_max) %>%
  mutate(
    display_label = factor(display_label, levels = selected_windows$display_label),
    selection_group = factor(
      selection_group,
      levels = c("Reviewed anomaly trace", "Top non-anomaly peak trace")
    )
  )
average_plot <- tidyr::crossing(
  display_label = factor(selected_windows$display_label, levels = selected_windows$display_label),
  all_household_average %>% filter(elapsed_hour_bin <= plot_hour_max)
)

fig_pm_anomaly_comparison <- ggplot() +
  geom_ribbon(
    data = average_plot,
    aes(x = elapsed_hour_bin, ymin = p25_pm25_ug_m3, ymax = p75_pm25_ug_m3),
    fill = "grey75",
    alpha = 0.35
  ) +
  geom_line(
    data = average_plot,
    aes(x = elapsed_hour_bin, y = mean_pm25_ug_m3),
    color = "black",
    linewidth = 0.55
  ) +
  geom_line(
    data = selected_plot,
    aes(x = elapsed_hour_bin, y = pm25_household_hour_mean, color = selection_group),
    linewidth = 0.75,
    alpha = 0.95
  ) +
  facet_wrap(vars(display_label), ncol = 1) +
  scale_color_manual(
    values = c(
      "Reviewed anomaly trace" = "#B2182B",
      "Top non-anomaly peak trace" = "#2166AC"
    ),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = seq(0, plot_hour_max, by = 12),
    limits = c(0, plot_hour_max),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_log10(
    breaks = c(10, 30, 100, 300, 1000, 3000, 10000, 30000),
    labels = scales::comma,
    limits = c(10, 30000),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  labs(
    title = "PM2.5 traces for reviewed anomalies and high-peak comparison households",
    subtitle = "Colored lines show household-hour means; black line shows the mean across all endline household-hours; gray band shows the all-household IQR.",
    x = "Hours since household monitor started",
    y = "Indoor PM2.5 (ug/m3, log10 scale)",
    caption = paste(
      "Comparison traces are the five non-anomaly endline household monitoring windows with the highest peak PM2.5,",
      "ranking ties by 99th percentile and mean PM2.5. Household IDs are stored only in the internal key table."
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    plot.title.position = "plot",
    strip.background = element_rect(fill = "grey92", color = "grey70"),
    strip.text = element_text(face = "bold", size = 9, hjust = 0),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, size = 8),
    plot.subtitle = element_text(size = 9)
  )

png_path <- file.path(figure_dir, "fig_pm25_anomaly_household_comparison.png")
tiff_path <- file.path(figure_dir, "fig_pm25_anomaly_household_comparison.tiff")
ggsave(png_path, fig_pm_anomaly_comparison, width = 8.5, height = 11, dpi = 300)
ggsave(tiff_path, fig_pm_anomaly_comparison, width = 8.5, height = 11, dpi = 300, compression = "lzw")

message("Wrote figure: ", normalizePath(png_path, winslash = "/", mustWork = TRUE))
message("Wrote figure: ", normalizePath(tiff_path, winslash = "/", mustWork = TRUE))
message("Wrote key table: ", normalizePath(file.path(qa_dir, "table_pm25_anomaly_comparison_window_key_internal.csv"), winslash = "/", mustWork = TRUE))