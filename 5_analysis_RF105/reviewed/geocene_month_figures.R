# Monthly summaries retain the original filenames; receipt-date subsets are explicit.
stove_month_summary <- geocene_use_summary(
  geocene_all_arms(filter(stove_daily, !is.na(days_after_first_receiving), days_after_first_receiving >= 0)),
  c("months_after_first_receiving_numeric", "study_arm_overall"))
write_reviewed_csv(stove_month_summary, "table_descriptive_stove_month_summary.csv")
plot_month <- stove_month_summary %>% filter(n_households >= 3)
if (nrow(plot_month)) {
  p <- ggplot(plot_month, aes(months_after_first_receiving_numeric, pct_exclusive_lpg_days, color = study_arm_overall)) +
    geom_point() + scale_color_manual(values = c(comparison = "#4E79A7", intervention = "#F28E2B", all_arms = "#555555")) +
    scale_y_continuous(limits = c(0, 100)) + theme_bw() +
    labs(x = "Months after first receiving LPG", y = "Household-days with exclusive LPG use (%)", color = "Study arm")
  save_reviewed_plot(p, "fig_descriptive_stove_exclusive_lpg_month.png", width = 7, height = 4.5)
  minutes <- plot_month %>% select(months_after_first_receiving_numeric, study_arm_overall, mean_lpg_minutes_per_day, mean_biomass_minutes_per_day) %>%
    pivot_longer(c(mean_lpg_minutes_per_day, mean_biomass_minutes_per_day), names_to = "fuel_type", values_to = "minutes") %>%
    mutate(fuel_type = recode(fuel_type, mean_lpg_minutes_per_day = "LPG", mean_biomass_minutes_per_day = "Biomass"))
  p <- ggplot(minutes, aes(months_after_first_receiving_numeric, minutes, color = fuel_type)) + geom_point() +
    facet_wrap(~study_arm_overall) + theme_bw() + scale_color_manual(values = c(LPG = "#0072B2", Biomass = "#D55E00")) +
    labs(x = "Months after first receiving LPG", y = "Mean stove-use minutes per household-day", color = "Fuel")
  save_reviewed_plot(p, "fig_descriptive_stove_minutes_fuel_month.png", width = 7, height = 4.5)
}
