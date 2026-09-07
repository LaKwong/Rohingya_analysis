stove_midline_intervention <- stove_daily %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = c(arm_levels, "all_arms", "missing_study_arm"))
  ) %>%
  filter(timepoint == "midline", study_arm_overall == "intervention") %>%
  mutate(
    days_after_first_receiving = as_number(days_after_first_receiving),
    months_after_first_receiving = as_number(months_after_first_receiving_numeric),
    collection_year = lubridate::year(as.Date(date)),
    stove_on_min_sum_lpg_zero = as_number(stove_on_min_sum_lpg_zero),
    stove_on_min_sum_biomass_zero = as_number(stove_on_min_sum_biomass_zero),
    exclusive_lpg_recalc = as.integer(as.logical(exclusive_lpg_recalc))
  ) %>%
  filter(!is.na(days_after_first_receiving), days_after_first_receiving >= 0)

stove_midline_collection_years <- collapse_collection_years(
  stove_midline_intervention$collection_year
)

stove_exclusive_plot_data <- stove_midline_intervention %>%
  filter(!is.na(months_after_first_receiving), !is.na(exclusive_lpg_recalc)) %>%
  group_by(months_after_first_receiving) %>%
  summarise(
    collection_years = collapse_collection_years(collection_year),
    n_daily_records = n(),
    n_households = n_distinct(fcn_id),
    n_exclusive_lpg_days = sum(exclusive_lpg_recalc == 1, na.rm = TRUE),
    proportion_exclusive_lpg_days = mean(exclusive_lpg_recalc == 1, na.rm = TRUE),
    percent_exclusive_lpg_days = 100 * proportion_exclusive_lpg_days,
    .groups = "drop"
  ) %>%
  add_prop_ci("n_exclusive_lpg_days", "n_daily_records") %>%
  arrange(months_after_first_receiving)

write_reviewed_csv(
  stove_exclusive_plot_data,
  "table_descriptive_stove_exclusive_plot_data.csv"
)

fig_stove_exclusive <- ggplot(
  stove_exclusive_plot_data,
  aes(x = months_after_first_receiving, y = proportion_exclusive_lpg_days)
) +
  geom_line(color = "#138B87", linewidth = 0.8) +
  geom_point(color = "#138B87", size = 2) +
  geom_text(aes(y = 1.05, label = n_daily_records), size = 3) +
  annotate(
    "text",
    x = min(stove_exclusive_plot_data$months_after_first_receiving, na.rm = TRUE) - 0.4,
    y = 1.05,
    label = "n =",
    hjust = 1,
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1.08)) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(
    x = month_axis_label(
      "Months after first receiving LPG through free distribution program",
      stove_midline_collection_years
    ),
    y = "Percent of days household exclusively used LPG when cooking"
  ) +
  coord_cartesian(clip = "off")

save_plot_if_data(
  stove_exclusive_plot_data,
  fig_stove_exclusive,
  "fig_descriptive_stove_exclusive_midline.tiff",
  width = 7,
  height = 6
)

stove_minutes_plot_data <- stove_midline_intervention %>%
  filter(!is.na(days_after_first_receiving)) %>%
  transmute(
    fcn_id,
    hh_id,
    date,
    collection_year,
    timepoint,
    study_arm_overall,
    days_after_first_receiving,
    lpg = stove_on_min_sum_lpg_zero,
    biomass = stove_on_min_sum_biomass_zero
  ) %>%
  pivot_longer(
    cols = c(lpg, biomass),
    names_to = "fuel_type",
    values_to = "stove_on_min_sum"
  ) %>%
  filter(!is.na(stove_on_min_sum)) %>%
  mutate(
    fuel_type = factor(fuel_type, levels = c("lpg", "biomass")),
    fuel_type_label = recode(as.character(fuel_type), lpg = "LPG", biomass = "Biomass")
  )

stove_minutes_summary <- stove_minutes_plot_data %>%
  group_by(fuel_type, fuel_type_label) %>%
  summarise(
    collection_years = collapse_collection_years(collection_year),
    n_daily_records = n(),
    n_households = n_distinct(fcn_id),
    mean_minutes = mean(stove_on_min_sum, na.rm = TRUE),
    median_minutes = median(stove_on_min_sum, na.rm = TRUE),
    sd_minutes = sd(stove_on_min_sum, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label_x = quantile(stove_minutes_plot_data$days_after_first_receiving, 0.95,
                       na.rm = TRUE, names = FALSE),
    label_y = mean_minutes + 20,
    label = paste0(round(mean_minutes, 1), " min")
  )

write_reviewed_csv(
  stove_minutes_plot_data,
  "table_descriptive_stove_minutes_plot_data.csv"
)
write_reviewed_csv(
  stove_minutes_summary,
  "table_descriptive_stove_minutes_summary.csv"
)

fig_stove_minutes <- ggplot(
  stove_minutes_plot_data,
  aes(x = days_after_first_receiving, y = stove_on_min_sum, color = fuel_type)
) +
  geom_jitter(alpha = 0.45, shape = 16, width = 4, height = 0) +
  geom_hline(
    data = stove_minutes_summary,
    aes(yintercept = mean_minutes, color = fuel_type),
    linewidth = 0.7,
    show.legend = FALSE
  ) +
  geom_text(
    data = stove_minutes_summary,
    aes(x = label_x, y = label_y, label = label, color = fuel_type),
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  scale_color_manual(
    name = "Fuel type",
    breaks = c("lpg", "biomass"),
    labels = c("LPG", "Biomass"),
    values = stove_colors
  ) +
  theme_bw() +
  labs(
    x = "Days after first receiving LPG through free distribution program",
    y = "Minutes of use"
  )

save_plot_if_data(
  stove_minutes_plot_data,
  fig_stove_minutes,
  "fig_descriptive_stove_minutes_midline.tiff",
  width = 7,
  height = 6
)
