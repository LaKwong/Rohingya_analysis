align_ggplot_widths <- function(...) {
  grobs <- lapply(list(...), ggplotGrob)
  max_width <- do.call(
    grid::unit.pmax,
    lapply(grobs, function(grob) grob$widths[2:5])
  )
  lapply(grobs, function(grob) {
    grob$widths[2:5] <- as.list(max_width)
    grob
  })
}

stove_composite_data <- stove_daily %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = c(arm_levels, "all_arms", "missing_study_arm"))
  ) %>%
  mutate(
    date = as.Date(date),
    collection_year = lubridate::year(date),
    days_after_first_receiving = as_number(days_after_first_receiving),
    weeks_after_first_receiving = floor(days_after_first_receiving / 7),
    months_after_first_receiving_display = days_after_first_receiving / 30,
    period30_after_first_receiving = floor(days_after_first_receiving / 30),
    period30_midpoint_days = period30_after_first_receiving * 30 + 15,
    period30_midpoint_weeks = period30_midpoint_days / 7,
    period30_midpoint_months = period30_midpoint_days / 30,
    observed_stove_use_day = as.logical(observed_stove_use_day),
    valid_exclusive_use_denominator =
      as.logical(valid_exclusive_use_denominator),
    lpg_recorded = as.logical(lpg_recorded),
    biomass_recorded = as.logical(biomass_recorded),
    exclusive_lpg_recalc = as.logical(exclusive_lpg_recalc),
    mixed_use_recalc = as.logical(mixed_use_recalc),
    stove_on_min_sum_lpg_zero = as_number(stove_on_min_sum_lpg_zero),
    stove_on_min_sum_biomass_zero =
      as_number(stove_on_min_sum_biomass_zero)
  ) %>%
  filter(
    observed_stove_use_day,
    days_after_first_receiving >= 0,
    !is.na(days_after_first_receiving)
  )

if (nrow(stove_composite_data) > 0) {
  stove_composite_month_breaks <- seq(
    0,
    max(
      6,
      ceiling(max(stove_composite_data$months_after_first_receiving_display,
                  na.rm = TRUE) / 6) * 6
    ),
    by = 6
  )
  stove_composite_colors <- c(Biomass = "#D55E00", LPG = "#0072B2")
  stove_energy_colors <- c(
    biomass = "#D55E00",
    lpg = "#0072B2",
    `mixed use` = "#6A51A3"
  )
  stove_composite_x_label <-
    "Months after first receiving LPG through free distribution program"

  make_stove_period_summary <- function(df, period_var, period_type) {
    df %>%
      group_by(period_value = .data[[period_var]]) %>%
      summarise(
        collection_years = collapse_collection_years(collection_year),
        n_household_days = n(),
        n_households = n_distinct(fcn_id),
        n_lpg_stoves_monitored = sum(lpg_recorded, na.rm = TRUE),
        n_biomass_stoves_monitored = sum(biomass_recorded, na.rm = TRUE),
        mean_lpg_minutes_per_day =
          mean(stove_on_min_sum_lpg_zero, na.rm = TRUE),
        mean_biomass_minutes_per_day =
          mean(stove_on_min_sum_biomass_zero, na.rm = TRUE),
        n_days_exclusive_denominator =
          sum(valid_exclusive_use_denominator, na.rm = TRUE),
        n_exclusive_lpg_days = sum(exclusive_lpg_recalc, na.rm = TRUE),
        pct_exclusive_lpg_days = if_else(
          n_days_exclusive_denominator > 0,
          100 * n_exclusive_lpg_days / n_days_exclusive_denominator,
          NA_real_
        ),
        .groups = "drop"
      ) %>%
      mutate(period_type = period_type, .before = 1) %>%
      arrange(period_value)
  }

  make_stove_monitored_plot_data <- function(df, period_var) {
    df %>%
      transmute(
        period_value = .data[[period_var]],
        Biomass = as.integer(biomass_recorded),
        LPG = as.integer(lpg_recorded)
      ) %>%
      pivot_longer(
        cols = c(Biomass, LPG),
        names_to = "stove",
        values_to = "stove_count"
      ) %>%
      group_by(period_value, stove) %>%
      summarise(
        number_stoves_monitored = sum(stove_count, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(stove = factor(stove, levels = c("Biomass", "LPG")))
  }

  make_stove_minutes_plot_data <- function(df, period_var) {
    df %>%
      group_by(period_value = .data[[period_var]]) %>%
      summarise(
        n_household_days = n(),
        Biomass = mean(stove_on_min_sum_biomass_zero, na.rm = TRUE),
        LPG = mean(stove_on_min_sum_lpg_zero, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(
        cols = c(Biomass, LPG),
        names_to = "stove",
        values_to = "average_minutes_of_use"
      ) %>%
      mutate(stove = factor(stove, levels = c("Biomass", "LPG")))
  }

  stove_use_composite_day_summary <- make_stove_period_summary(
    stove_composite_data,
    "days_after_first_receiving",
    "days_after_first_receiving"
  )
  stove_use_composite_week_summary <- make_stove_period_summary(
    stove_composite_data,
    "weeks_after_first_receiving",
    "weeks_after_first_receiving"
  )

  stove_monitored_day_plot_data <- make_stove_monitored_plot_data(
    stove_composite_data,
    "days_after_first_receiving"
  ) %>%
    mutate(months_after_first_receiving_plot = period_value / 30)
  stove_monitored_week_plot_data <- make_stove_monitored_plot_data(
    stove_composite_data,
    "weeks_after_first_receiving"
  ) %>%
    mutate(months_after_first_receiving_plot = period_value * 7 / 30)
  stove_minutes_day_plot_data <- make_stove_minutes_plot_data(
    stove_composite_data,
    "days_after_first_receiving"
  ) %>%
    mutate(months_after_first_receiving_plot = period_value / 30)
  stove_minutes_week_plot_data <- make_stove_minutes_plot_data(
    stove_composite_data,
    "weeks_after_first_receiving"
  ) %>%
    mutate(months_after_first_receiving_plot = period_value * 7 / 30)

  stove_exclusive_household_day_plot_data <- stove_composite_data %>%
    filter(
      valid_exclusive_use_denominator,
      !is.na(exclusive_lpg_recalc),
      !is.na(period30_after_first_receiving)
    ) %>%
    transmute(
      days_after_first_receiving,
      weeks_after_first_receiving,
      months_after_first_receiving_display,
      period30_after_first_receiving,
      period30_midpoint_days,
      period30_midpoint_weeks,
      period30_midpoint_months,
      exclusive_lpg_day = as.integer(exclusive_lpg_recalc),
      percent_exclusive_lpg_day = 100 * exclusive_lpg_day
    )

  stove_exclusive_30day_summary <- stove_composite_data %>%
    filter(
      valid_exclusive_use_denominator,
      !is.na(exclusive_lpg_recalc),
      !is.na(period30_after_first_receiving)
    ) %>%
    group_by(
      period30_after_first_receiving,
      period30_midpoint_days,
      period30_midpoint_weeks,
      period30_midpoint_months
    ) %>%
    summarise(
      n_daily_records = n(),
      n_households = n_distinct(fcn_id),
      n_exclusive_lpg_days = sum(exclusive_lpg_recalc, na.rm = TRUE),
      percent_exclusive_lpg_days =
        100 * n_exclusive_lpg_days / n_daily_records,
      .groups = "drop"
    ) %>%
    add_prop_ci("n_exclusive_lpg_days", "n_daily_records") %>%
    arrange(period30_after_first_receiving)

  write_reviewed_csv(
    stove_use_composite_day_summary,
    "table_descriptive_stove_use_composite_day_summary.csv"
  )
  write_reviewed_csv(
    stove_use_composite_week_summary,
    "table_descriptive_stove_use_composite_week_summary.csv"
  )
  write_reviewed_csv(
    stove_exclusive_30day_summary,
    "table_descriptive_stove_use_composite_30day_exclusive_summary.csv"
  )

  # Constants carried forward from the prior RF105 energy figure calculation.
  lpg_efficiency <- 0.67
  biomass_efficiency <- 0.128
  conv_mj <- 3.6
  power_wood <- 6.824 / conv_mj
  power_lpg <- 3.4 / conv_mj

  stove_energy_method_plot_data <- bind_rows(
    stove_composite_data %>%
      mutate(
        biomass_energy_mj =
          (stove_on_min_sum_biomass_zero / 60) * power_wood,
        lpg_energy_mj = (stove_on_min_sum_lpg_zero / 60) * power_lpg,
        mixed_use_energy_mj = if_else(
          mixed_use_recalc,
          biomass_energy_mj + lpg_energy_mj,
          NA_real_
        )
      ) %>%
      transmute(
        energy_metric = "Daily energy consumed",
        biomass = biomass_energy_mj,
        lpg = lpg_energy_mj,
        `mixed use` = mixed_use_energy_mj
      ),
    stove_composite_data %>%
      mutate(
        biomass_energy_mj =
          (stove_on_min_sum_biomass_zero / 60) *
          power_wood * biomass_efficiency,
        lpg_energy_mj =
          (stove_on_min_sum_lpg_zero / 60) * power_lpg * lpg_efficiency,
        mixed_use_energy_mj = if_else(
          mixed_use_recalc,
          biomass_energy_mj + lpg_energy_mj,
          NA_real_
        )
      ) %>%
      transmute(
        energy_metric = "Daily energy reaching the pot",
        biomass = biomass_energy_mj,
        lpg = lpg_energy_mj,
        `mixed use` = mixed_use_energy_mj
      )
  ) %>%
    pivot_longer(
      cols = c(biomass, lpg, `mixed use`),
      names_to = "cooking_method",
      values_to = "energy_mj"
    ) %>%
    filter(!is.na(energy_mj), energy_mj > 0) %>%
    mutate(
      energy_metric = factor(
        energy_metric,
        levels = c("Daily energy consumed", "Daily energy reaching the pot")
      ),
      cooking_method = factor(
        cooking_method,
        levels = c("biomass", "lpg", "mixed use")
      )
    )

  write_reviewed_csv(
    stove_energy_method_plot_data %>%
      group_by(energy_metric, cooking_method) %>%
      summarise(
        n_household_days = n(),
        mean_energy_mj = mean(energy_mj, na.rm = TRUE),
        sd_energy_mj = if_else(n() > 1, sd(energy_mj, na.rm = TRUE), NA_real_),
        se_energy_mj = if_else(
          n() > 1,
          sd_energy_mj / sqrt(n_household_days),
          NA_real_
        ),
        ci_lower_energy_mj = if_else(
          n_household_days > 1,
          mean_energy_mj -
            qt(0.975, df = n_household_days - 1) * se_energy_mj,
          NA_real_
        ),
        ci_upper_energy_mj = if_else(
          n_household_days > 1,
          mean_energy_mj +
            qt(0.975, df = n_household_days - 1) * se_energy_mj,
          NA_real_
        ),
        median_energy_mj = median(energy_mj, na.rm = TRUE),
        p25_energy_mj = quantile(
          energy_mj, 0.25, na.rm = TRUE, names = FALSE
        ),
        p75_energy_mj = quantile(
          energy_mj, 0.75, na.rm = TRUE, names = FALSE
        ),
        min_energy_mj = min(energy_mj, na.rm = TRUE),
        max_energy_mj = max(energy_mj, na.rm = TRUE),
        .groups = "drop"
      ),
    "table_descriptive_stove_energy_by_cooking_method_summary.csv"
  )

  make_stove_monitored_plot <- function(plot_data, x_breaks, x_label,
                                        show_legend = TRUE) {
    y_minor_breaks <- seq(
      0,
      max(
        5,
        ceiling(max(plot_data$number_stoves_monitored, na.rm = TRUE) / 5) * 5
      ),
      by = 5
    )

    ggplot(
      plot_data,
      aes(
        x = months_after_first_receiving_plot,
        y = number_stoves_monitored,
        color = stove
      )
    ) +
      geom_point(alpha = 0.55, size = 1.2) +
      scale_x_continuous(breaks = x_breaks, name = x_label) +
      scale_y_continuous(minor_breaks = y_minor_breaks) +
      scale_color_manual(values = stove_composite_colors) +
      labs(
        x = x_label,
        y = "Number of\nstoves monitored",
        color = "Fuel type"
      ) +
      theme_bw(base_size = 9) +
      theme(
        legend.position = if (isTRUE(show_legend)) "top" else "none",
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(2, 5.5, 2, 5.5)
      )
  }

  make_stove_minutes_plot <- function(plot_data, x_breaks, x_label) {
    y_minor_breaks <- seq(
      0,
      max(
        50,
        ceiling(max(plot_data$average_minutes_of_use, na.rm = TRUE) / 50) * 50
      ),
      by = 50
    )

    ggplot(
      plot_data,
      aes(
        x = months_after_first_receiving_plot,
        y = average_minutes_of_use,
        color = stove
      )
    ) +
      geom_point(alpha = 0.55, size = 1.2) +
      scale_x_continuous(breaks = x_breaks, name = x_label) +
      scale_y_continuous(minor_breaks = y_minor_breaks) +
      scale_color_manual(values = stove_composite_colors) +
      labs(
        x = x_label,
        y = "Av. minutes\nstove use per day",
        color = "Fuel type"
      ) +
      theme_bw(base_size = 9) +
      theme(
        legend.position = "none",
        panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(2, 5.5, 2, 5.5)
      )
  }

  make_stove_exclusive_plot <- function(dot_data, summary_data, x_breaks,
                                        x_label) {
    ggplot() +
      geom_jitter(
        data = dot_data,
        aes(
          x = months_after_first_receiving_display,
          y = percent_exclusive_lpg_day
        ),
        width = 0.01,
        height = 0,
        alpha = 0.22,
        size = 0.55,
        color = "#0072B2"
      ) +
      geom_errorbar(
        data = summary_data,
        aes(
          x = period30_midpoint_months,
          ymin = ci_lower,
          ymax = ci_upper
        ),
        width = 0.15,
        color = "black"
      ) +
      geom_point(
        data = summary_data,
        aes(
          x = period30_midpoint_months,
          y = percent_exclusive_lpg_days
        ),
        shape = 17,
        size = 2.2,
        color = "black"
      ) +
      scale_x_continuous(breaks = x_breaks, name = x_label) +
      scale_y_continuous(
        labels = scales::label_number(suffix = "%"),
        limits = c(0, 100)
      ) +
      labs(
        x = x_label,
        y = "Percent of time household\ncooked exclusively with LPG"
      ) +
      theme_bw(base_size = 9) +
      theme(
        legend.position = "none",
        panel.grid.minor = element_blank(),
        plot.margin = margin(2, 5.5, 2, 5.5)
      )
  }

  fig_stove_monitored_day <- make_stove_monitored_plot(
    stove_monitored_day_plot_data,
    stove_composite_month_breaks,
    NULL,
    show_legend = TRUE
  )
  fig_stove_minutes_day <- make_stove_minutes_plot(
    stove_minutes_day_plot_data,
    stove_composite_month_breaks,
    NULL
  )
  fig_stove_exclusive_day <- make_stove_exclusive_plot(
    stove_exclusive_household_day_plot_data,
    stove_exclusive_30day_summary,
    stove_composite_month_breaks,
    stove_composite_x_label
  )

  fig_stove_monitored_week <- make_stove_monitored_plot(
    stove_monitored_week_plot_data,
    stove_composite_month_breaks,
    NULL,
    show_legend = TRUE
  )
  fig_stove_minutes_week <- make_stove_minutes_plot(
    stove_minutes_week_plot_data,
    stove_composite_month_breaks,
    NULL
  )
  fig_stove_exclusive_week <- make_stove_exclusive_plot(
    stove_exclusive_household_day_plot_data,
    stove_exclusive_30day_summary,
    stove_composite_month_breaks,
    stove_composite_x_label
  )

  fig_energy_consumed_method <- ggplot(
    stove_energy_method_plot_data %>%
      filter(energy_metric == "Daily energy consumed"),
    aes(x = cooking_method, y = energy_mj, color = cooking_method)
  ) +
    geom_boxplot(alpha = 0.5, outlier.alpha = 0.45) +
    scale_color_manual(values = stove_energy_colors, drop = FALSE) +
    scale_y_continuous(
      breaks = seq(0, 20, by = 5),
      minor_breaks = seq(0, 20, by = 1)
    ) +
    coord_cartesian(ylim = c(0, 20)) +
    labs(
      x = "cooking method",
      y = "Daily energy consumed (MJ/hh/day)"
    ) +
    theme_bw(base_size = 9) +
    theme(
      legend.position = "none",
      panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
      panel.grid.minor.x = element_blank(),
      plot.margin = margin(2, 5.5, 2, 5.5)
    )

  fig_energy_pot_method <- ggplot(
    stove_energy_method_plot_data %>%
      filter(energy_metric == "Daily energy reaching the pot"),
    aes(x = cooking_method, y = energy_mj, color = cooking_method)
  ) +
    geom_boxplot(alpha = 0.5, outlier.alpha = 0.45) +
    scale_color_manual(values = stove_energy_colors, drop = FALSE) +
    scale_y_continuous(
      breaks = seq(0, 20, by = 5),
      minor_breaks = seq(0, 20, by = 1)
    ) +
    coord_cartesian(ylim = c(0, 20)) +
    labs(
      x = "cooking method",
      y = "Daily energy that reached the pot (MJ/hh/day)"
    ) +
    theme_bw(base_size = 9) +
    theme(
      legend.position = "none",
      panel.grid.minor.y = element_line(color = "grey90", linewidth = 0.25),
      panel.grid.minor.x = element_blank(),
      plot.margin = margin(2, 5.5, 2, 5.5)
    )

  make_stove_use_panel <- function(monitored_plot, minutes_plot, exclusive_plot) {
    gridExtra::arrangeGrob(
      grobs = align_ggplot_widths(
        monitored_plot,
        minutes_plot,
        exclusive_plot
      ),
      ncol = 1,
      heights = c(1.3, 1, 1.3)
    )
  }

  make_stove_energy_composite <- function(stove_use_panel) {
    energy_panel <- gridExtra::arrangeGrob(
      grobs = align_ggplot_widths(
        fig_energy_consumed_method,
        fig_energy_pot_method
      ),
      ncol = 2
    )

    gridExtra::arrangeGrob(
      gridExtra::arrangeGrob(
        grid::textGrob(
          "A", x = 0, hjust = 0,
          gp = grid::gpar(fontface = "bold", fontsize = 12)
        ),
        stove_use_panel,
        ncol = 1,
        heights = c(0.08, 1)
      ),
      gridExtra::arrangeGrob(
        grid::textGrob(
          "B", x = 0, hjust = 0,
          gp = grid::gpar(fontface = "bold", fontsize = 12)
        ),
        energy_panel,
        ncol = 1,
        heights = c(0.1, 1)
      ),
      ncol = 1,
      heights = c(3, 2)
    )
  }

  fig_stove_use_energy_composite_day <- make_stove_energy_composite(
    make_stove_use_panel(
      fig_stove_monitored_day,
      fig_stove_minutes_day,
      fig_stove_exclusive_day
    )
  )
  fig_stove_use_energy_composite_week <- make_stove_energy_composite(
    make_stove_use_panel(
      fig_stove_monitored_week,
      fig_stove_minutes_week,
      fig_stove_exclusive_week
    )
  )

  save_reviewed_plot(
    fig_stove_use_energy_composite_day,
    "fig_descriptive_stove_use_composite_panel.png",
    width = 8,
    height = 11
  )
  save_reviewed_plot(
    fig_stove_use_energy_composite_day,
    "fig_descriptive_stove_use_composite_panel_days.png",
    width = 8,
    height = 11
  )
  save_reviewed_plot(
    fig_stove_use_energy_composite_week,
    "fig_descriptive_stove_use_composite_panel_weeks.png",
    width = 8,
    height = 11
  )
} else {
  message("Skipped composite stove-use energy panel: no eligible stove-use records.")
}
################################################################################
# Stove-use figures for the manuscript midline Geocene plots
