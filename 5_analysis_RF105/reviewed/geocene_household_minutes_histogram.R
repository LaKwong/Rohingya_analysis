# Household totals across all monitored dates; 60-minute, left-closed bins.
# Called by geocene_run_analysis for both primary and sensitivity data.
household_minutes <- geocene_household_stove_minutes(stove_daily)
household_minutes_bins <- geocene_household_minutes_bins(household_minutes)
stopifnot(sum(household_minutes_bins$n_households) == nrow(household_minutes),
  isTRUE(all.equal(sum(household_minutes$total_stove_use_minutes), sum(stove_daily$stove_on_min_sum_total_zero))))
write_reviewed_csv(household_minutes, "table_descriptive_geocene_household_total_stove_minutes.csv")
write_reviewed_csv(household_minutes_bins, "table_descriptive_geocene_household_stove_minutes_histogram_bins.csv")
if (nrow(household_minutes)) {
  fig_household_minutes <- ggplot(household_minutes_bins,
    aes(x = bin_start_minutes + 30, y = n_households)) +
    geom_col(width = 60, fill = "#0072B2") +
    scale_x_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.01))) +
    scale_y_continuous(breaks = scales::breaks_width(1), expand = expansion(mult = c(0, 0.05))) +
    labs(title = "Total recorded stove-use time per household",
      subtitle = paste0(nrow(household_minutes), " households; all monitoring dates; 60-minute bins"),
      x = "Total recorded stove-use time (minutes)", y = "Number of households",
      caption = "LPG and biomass minutes summed, including overlapping use. Not sensor deployment duration.") +
    theme_bw(base_size = 11) + theme(panel.grid.minor = element_blank())
  save_reviewed_plot(fig_household_minutes, "fig_descriptive_geocene_household_total_stove_minutes_histogram.png", width = 10, height = 6)
}
