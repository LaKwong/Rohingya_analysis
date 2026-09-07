min_household_days_for_day_plot <- 6

df_days_receive <- stove_daily %>%
  mutate(
    before_after = if_else(
      days_after_first_receiving < 0,
      "Days Before Intervention",
      "Days After Receiving"
    ),
    before_after = factor(
      before_after,
      levels = c("Days Before Intervention", "Days After Receiving")
    )
  ) %>%
  filter(
    observed_stove_use_day,
    before_after == "Days After Receiving",
    !is.na(days_after_first_receiving)
  )

fig2_day_summary <- df_days_receive %>%
  group_by(days_after_first_receiving, before_after) %>%
  summarise(
    n_household_days = n(),
    n_households = n_distinct(fcn_id),
    mean_lpg_minutes_per_day =
      mean(stove_on_min_sum_lpg_zero, na.rm = TRUE),
    mean_biomass_minutes_per_day =
      mean(stove_on_min_sum_biomass_zero, na.rm = TRUE),
    mean_pct_lpg_minutes =
      mean(stove_on_min_pc_lpg_zero, na.rm = TRUE),
    mean_pct_biomass_minutes =
      mean(stove_on_min_pc_biomass_zero, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_household_days >= min_household_days_for_day_plot)

fig2_minutes_data <- fig2_day_summary %>%
  select(
    days_after_first_receiving,
    before_after,
    n_household_days,
    mean_lpg_minutes_per_day,
    mean_biomass_minutes_per_day
  ) %>%
  pivot_longer(
    cols = c(mean_lpg_minutes_per_day, mean_biomass_minutes_per_day),
    names_to = "stove",
    values_to = "average_minutes_of_use"
  ) %>%
  mutate(
    stove = recode(
      stove,
      mean_lpg_minutes_per_day = "LPG",
      mean_biomass_minutes_per_day = "Biomass"
    )
  )

fig2_percent_data <- fig2_day_summary %>%
  select(
    days_after_first_receiving,
    before_after,
    n_household_days,
    mean_pct_lpg_minutes,
    mean_pct_biomass_minutes
  ) %>%
  pivot_longer(
    cols = c(mean_pct_lpg_minutes, mean_pct_biomass_minutes),
    names_to = "stove",
    values_to = "average_percent_use"
  ) %>%
  mutate(
    stove = recode(
      stove,
      mean_pct_lpg_minutes = "LPG",
      mean_pct_biomass_minutes = "Biomass"
    )
  )

fig2_monitor_household_days <- df_days_receive %>%
  count(days_after_first_receiving, before_after, name = "n_household_days")

fig2_monitor_data <- df_days_receive %>%
  transmute(
    days_after_first_receiving,
    before_after,
    LPG = as.integer(lpg_recorded),
    Biomass = as.integer(biomass_recorded)
  ) %>%
  pivot_longer(
    cols = c(LPG, Biomass),
    names_to = "stove",
    values_to = "stove_count"
  ) %>%
  group_by(days_after_first_receiving, before_after, stove) %>%
  summarise(
    number_stoves_monitored = sum(stove_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    fig2_monitor_household_days,
    by = c("days_after_first_receiving", "before_after")
  ) %>%
  mutate(
    n_household_days = replace_na(n_household_days, 0L)
  ) %>%
  filter(n_household_days >= min_household_days_for_day_plot)

write_reviewed_csv(
  bind_rows(
    fig2_minutes_data %>%
      transmute(
        panel = "average_minutes_of_use",
        days_after_first_receiving,
        before_after,
        stove,
        n_household_days,
        value = average_minutes_of_use
      ),
    fig2_percent_data %>%
      transmute(
        panel = "average_percent_use",
        days_after_first_receiving,
        before_after,
        stove,
        n_household_days,
        value = average_percent_use
      ),
    fig2_monitor_data %>%
      transmute(
        panel = "number_stoves_monitored",
        days_after_first_receiving,
        before_after,
        stove,
        n_household_days,
        value = number_stoves_monitored
      )
  ),
  "table_descriptive_stove_composite_plot_data.csv"
)

stove_colors <- c(LPG = "#0072B2", Biomass = "#D55E00")

fig2_minutes <- ggplot(
  fig2_minutes_data,
  aes(x = days_after_first_receiving,
      y = average_minutes_of_use,
      color = stove)
) +
  geom_point(alpha = 0.75, size = 1.3) +
  scale_color_manual(values = stove_colors) +
  labs(
    x = NULL,
    y = "Average minutes of use",
    color = "Stove"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

fig2_percent <- ggplot(
  fig2_percent_data,
  aes(x = days_after_first_receiving,
      y = average_percent_use,
      color = stove)
) +
  geom_point(alpha = 0.75, size = 1.3) +
  scale_color_manual(values = stove_colors) +
  scale_y_continuous(limits = c(0, 100), labels = label_number(suffix = "%")) +
  labs(
    x = NULL,
    y = "Average percent of daily cooking time",
    color = "Stove"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

fig2_monitored <- ggplot(
  fig2_monitor_data,
  aes(x = days_after_first_receiving,
      y = number_stoves_monitored,
      color = stove)
) +
  geom_point(alpha = 0.75, size = 1.3) +
  scale_color_manual(values = stove_colors) +
  labs(
    x = "Days after first receiving LPG through free distribution program",
    y = "Number of stoves with recorded use",
    color = "Stove"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

fig2_combined <- gridExtra::arrangeGrob(
  fig2_minutes,
  fig2_percent,
  fig2_monitored,
  ncol = 1,
  heights = c(1, 1, 1)
)

save_reviewed_plot(
  fig2_combined,
  "fig_descriptive_stove_use_composite_panel.png",
  width = 7,
  height = 8
)

save_reviewed_plot(
  fig2_minutes,
  "fig_descriptive_stove_minutes_by_day.png",
  width = 7,
  height = 4.5
)
save_reviewed_plot(
  fig2_percent,
  "fig_descriptive_stove_use_percent_by_day.png",
  width = 7,
  height = 4.5
)
save_reviewed_plot(
  fig2_monitored,
  "fig_descriptive_stoves_monitored_by_day.png",
  width = 7,
  height = 4.5
)

################################################################################
# Exclusive LPG use by month after receipt
################################################################################

exclusive_use_by_month_hh <- stove_daily %>%
  filter(
    observed_stove_use_day,
    days_after_first_receiving >= 0,
    !is.na(months_after_first_receiving_numeric)
  ) %>%
  group_by(
    fcn_id,
    study_arm_overall,
    months_after_first_receiving_numeric
  ) %>%
  summarise(
    n_daily_records = n(),
    n_days_exclusive_denominator =
      sum(valid_exclusive_use_denominator, na.rm = TRUE),
    pct_days_exclusive_lpg = if_else(
      n_days_exclusive_denominator > 0,
      100 * sum(exclusive_lpg_recalc, na.rm = TRUE) / n_days_exclusive_denominator,
      NA_real_
    ),
    .groups = "drop"
  )

exclusive_use_by_month_summary <- exclusive_use_by_month_hh %>%
  group_by(months_after_first_receiving_numeric) %>%
  summarise(
    n_households = n_distinct(fcn_id),
    mean_pct_days_exclusive_lpg =
      mean(pct_days_exclusive_lpg, na.rm = TRUE),
    median_pct_days_exclusive_lpg =
      median(pct_days_exclusive_lpg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_households >= 3)

write_reviewed_csv(
  exclusive_use_by_month_hh,
  "table_descriptive_stove_exclusive_household_month.csv"
)
write_reviewed_csv(
  exclusive_use_by_month_summary,
  "table_descriptive_stove_exclusive_month_summary.csv"
)

fig_exclusive_lpg_month <- ggplot(
  exclusive_use_by_month_hh %>%
    semi_join(
      exclusive_use_by_month_summary,
      by = "months_after_first_receiving_numeric"
    ),
  aes(
    x = months_after_first_receiving_numeric,
    y = pct_days_exclusive_lpg
  )
) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.45, color = "#4E79A7") +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 17,
    size = 2,
    color = "black"
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom = "linerange",
    color = "black"
  ) +
  scale_y_continuous(limits = c(0, 105), labels = label_number(suffix = "%")) +
  labs(
    x = "Months after first receiving LPG through free distribution program",
    y = "Household-days with exclusive LPG use"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank())

save_reviewed_plot(
  fig_exclusive_lpg_month,
  "fig_descriptive_exclusive_lpg_by_month.png",
  width = 7,
  height = 4.5
)

################################################################################
# Supplemental stove-use table by days after receipt
################################################################################

make_days_after_group <- function(days_after_first_receiving) {
  case_when(
    is.na(days_after_first_receiving) ~ NA_character_,
    days_after_first_receiving < 0 ~ "not_yet_received",
    days_after_first_receiving <= 30 ~ "0-30",
    days_after_first_receiving <= 60 ~ "31-60",
    days_after_first_receiving <= 90 ~ "61-90",
    days_after_first_receiving <= 120 ~ "91-120",
    days_after_first_receiving <= 150 ~ "121-150",
    days_after_first_receiving <= 180 ~ "151-180",
    days_after_first_receiving <= 210 ~ "181-210",
    days_after_first_receiving > 210 ~ "211+",
    TRUE ~ NA_character_
  )
}

days_after_levels <- c(
  "not_yet_received", "0-30", "31-60", "61-90", "91-120",
  "121-150", "151-180", "181-210", "211+"
)

supplement_day_data <- stove_daily %>%
  mutate(
    days_after_group = factor(
      make_days_after_group(days_after_first_receiving),
      levels = days_after_levels
    ),
    pct_lpg_events = if_else(
      cooking_events_with_lpg_zero + cooking_events_with_biomass_zero > 0,
      100 * cooking_events_with_lpg_zero /
        (cooking_events_with_lpg_zero + cooking_events_with_biomass_zero),
      NA_real_
    ),
    pct_lpg_minutes = stove_on_min_pc_lpg_zero,
    pct_lpg_minutes_group = case_when(
      is.na(pct_lpg_minutes) ~ NA_character_,
      pct_lpg_minutes == 0 ~ "0",
      pct_lpg_minutes > 0 & pct_lpg_minutes < 20 ~ "1-19",
      pct_lpg_minutes >= 20 & pct_lpg_minutes < 40 ~ "20-39",
      pct_lpg_minutes >= 40 & pct_lpg_minutes < 60 ~ "40-59",
      pct_lpg_minutes >= 60 & pct_lpg_minutes < 80 ~ "60-79",
      pct_lpg_minutes >= 80 & pct_lpg_minutes < 100 ~ "80-99",
      pct_lpg_minutes == 100 ~ "100",
      TRUE ~ NA_character_
    ),
    pct_lpg_minutes_group = factor(
      pct_lpg_minutes_group,
      levels = c("0", "1-19", "20-39", "40-59", "60-79", "80-99", "100")
    )
  ) %>%
  filter(observed_stove_use_day) %>%
  filter(!is.na(days_after_group))

supplement_day_summary <- supplement_day_data %>%
  group_by(days_after_group) %>%
  summarise(
    n_household_days = n(),
    n_households = n_distinct(fcn_id),
    mean_pct_lpg_events = mean(pct_lpg_events, na.rm = TRUE),
    median_pct_lpg_events = median(pct_lpg_events, na.rm = TRUE),
    mean_pct_lpg_minutes = mean(pct_lpg_minutes, na.rm = TRUE),
    median_pct_lpg_minutes = median(pct_lpg_minutes, na.rm = TRUE),
    n_days_exclusive_denominator =
      sum(valid_exclusive_use_denominator, na.rm = TRUE),
    pct_exclusive_lpg_days = if_else(
      n_days_exclusive_denominator > 0,
      100 * sum(exclusive_lpg_recalc, na.rm = TRUE) / n_days_exclusive_denominator,
      NA_real_
    ),
    pct_exclusive_biomass_days = if_else(
      n_days_exclusive_denominator > 0,
      100 * sum(exclusive_biomass_recalc, na.rm = TRUE) / n_days_exclusive_denominator,
      NA_real_
    ),
    pct_mixed_use_days = if_else(
      n_days_exclusive_denominator > 0,
      100 * sum(mixed_use_recalc, na.rm = TRUE) / n_days_exclusive_denominator,
      NA_real_
    ),
    .groups = "drop"
  )

supplement_pct_lpg_distribution <- supplement_day_data %>%
  filter(!is.na(pct_lpg_minutes_group)) %>%
  count(days_after_group, pct_lpg_minutes_group, name = "n_household_days") %>%
  group_by(days_after_group) %>%
  mutate(
    pct_household_days =
      100 * n_household_days / sum(n_household_days, na.rm = TRUE)
  ) %>%
  ungroup()

write_reviewed_csv(
  supplement_day_summary,
  "table_descriptive_stove_day_summary.csv"
)
write_reviewed_csv(
  supplement_pct_lpg_distribution,
  "table_descriptive_stove_lpg_distribution.csv"
)

################################################################################
# Energy-conversion summaries from the RF105 Fig2 script
################################################################################

# These constants are carried forward from the prior RF105 Figure 2 calculation
# for comparability. The previous project notes mix power and energy units, so
# this output should be treated as a reviewed reproduction of that calculation,
# not a new independent energy model.
lpg_efficiency <- 0.67
biomass_efficiency <- 0.128
conv_mj <- 3.6
power_wood <- 6.824 / conv_mj
power_lpg <- 3.4 / conv_mj

energy_day <- df_days_receive %>%
  mutate(
    biomass_energy_mj =
      (stove_on_min_sum_biomass_zero / 60 * power_wood) *
      biomass_efficiency,
    lpg_energy_mj =
      (stove_on_min_sum_lpg_zero / 60 * power_lpg) *
      lpg_efficiency,
    total_energy_mj = biomass_energy_mj + lpg_energy_mj
  )

energy_summary <- energy_day %>%
  summarise(
    n_household_days = n(),
    n_households = n_distinct(fcn_id),
    mean_lpg_energy_mj = mean(lpg_energy_mj, na.rm = TRUE),
    median_lpg_energy_mj = median(lpg_energy_mj, na.rm = TRUE),
    mean_biomass_energy_mj = mean(biomass_energy_mj, na.rm = TRUE),
    median_biomass_energy_mj = median(biomass_energy_mj, na.rm = TRUE),
    mean_total_energy_mj = mean(total_energy_mj, na.rm = TRUE),
    median_total_energy_mj = median(total_energy_mj, na.rm = TRUE)
  )

write_reviewed_csv(
  energy_summary,
  "table_descriptive_stove_energy_summary.csv"
)

energy_plot_data <- energy_day %>%
  filter(days_after_first_receiving %in% fig2_day_summary$days_after_first_receiving) %>%
  group_by(days_after_first_receiving) %>%
  summarise(
    n_household_days = n(),
    LPG = mean(lpg_energy_mj, na.rm = TRUE),
    Biomass = mean(biomass_energy_mj, na.rm = TRUE),
    Total = mean(total_energy_mj, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(LPG, Biomass, Total),
    names_to = "energy_type",
    values_to = "mean_daily_energy_mj"
  )

write_reviewed_csv(
  energy_plot_data,
  "table_descriptive_stove_energy_plot_data.csv"
)

fig_energy_day <- ggplot(
  energy_plot_data,
  aes(
    x = days_after_first_receiving,
    y = mean_daily_energy_mj,
    color = energy_type
  )
) +
  geom_point(alpha = 0.75, size = 1.3) +
  scale_color_manual(
    values = c(LPG = "#0072B2", Biomass = "#D55E00", Total = "#4D4D4D")
  ) +
  labs(
    x = "Days after first receiving LPG through free distribution program",
    y = "Mean daily energy reaching cooking pot (MJ)",
    color = "Energy type"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

save_reviewed_plot(
  fig_energy_day,
  "fig_descriptive_stove_energy_by_day.png",
  width = 7,
  height = 4.5
)

message("RF105 reviewed combined Geocene stove-use analysis complete.")
