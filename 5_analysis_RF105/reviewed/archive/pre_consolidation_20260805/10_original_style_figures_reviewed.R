################################################################################
# RF105 reviewed original-style figure generation
#
# Purpose:
#   Recreate reviewed analogues of selected draft/manuscript figures using the
#   checked RF105 data products and clean_final inputs. These figures preserve
#   the visual intent of the original draft figures while making inputs,
#   denominators, and output locations explicit.
#
# Inputs:
#   4_data/clean_final/survey_refugee_household.rds
#   4_data/clean_final/pm25_pats_refugee_indoor.rds
#   7_tables/RF105_reviewed_YYYYMMDD/geocene_stove_use_daily_analysis_dataset_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/descriptive_harassment_reviewed.csv
#   7_tables/RF105_reviewed_YYYYMMDD/descriptive_physical_health_outcomes_reviewed.csv
#
# Outputs:
#   Figures in 6_figures/RF105_reviewed_YYYYMMDD/:
#     stove_exclusive_use_since_month_received_midline_reviewed.tiff
#     stove_used_min_per_day_midline_reviewed.tiff
#     fig_time_respondent_more_less_reviewed.png
#     fig_time_child_more_less_reviewed.png
#     harassment_reviewed.tiff
#     PM_by_hour_of_day_log_smooth_span_0.3_reviewed.png
#     fig_physical_health_reviewed.png
#     fig_FCS_HDDS_data_reviewed.tiff
#     fig_lpg_willingness_to_pay_reviewed.png
#
#   Tables in 7_tables/RF105_reviewed_YYYYMMDD/:
#     original_style_*_reviewed.csv
#     qa/original_style_figure_*_reviewed.csv
#
# Notes:
#   This script intentionally does not run any impact models. Intervention impact
#   estimates are produced in 4_rdid_xgboost_reviewed.R. This file is for the
#   descriptive/original-style manuscript figures only.
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

Sys.setenv(TZ = "Asia/Dhaka")

################################################################################
# Local helpers
################################################################################

as_number <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

var_present <- function(df, var) {
  var %in% names(df)
}

num_col <- function(df, var) {
  if (var_present(df, var)) {
    as_number(df[[var]])
  } else {
    rep(NA_real_, nrow(df))
  }
}

row_sum_vars <- function(df, vars, cap_at = NULL) {
  vars <- vars[vars %in% names(df)]
  if (length(vars) == 0) {
    return(rep(NA_real_, nrow(df)))
  }

  mat <- do.call(cbind, lapply(vars, function(var) as_number(df[[var]])))
  out <- rowSums(mat, na.rm = TRUE)
  out[rowSums(!is.na(mat)) == 0] <- NA_real_

  if (!is.null(cap_at)) {
    out <- pmin(out, cap_at)
  }

  out
}

hdds_food_group <- function(df, vars) {
  vars <- vars[vars %in% names(df)]
  if (length(vars) == 0) {
    return(rep(NA_integer_, nrow(df)))
  }

  mat <- do.call(cbind, lapply(vars, function(var) as_number(df[[var]])))
  out <- as.integer(rowSums(mat > 0, na.rm = TRUE) > 0)
  out[rowSums(!is.na(mat)) == 0] <- NA_integer_
  out
}

coalesce_vars <- function(df, vars) {
  vars <- vars[vars %in% names(df)]
  if (length(vars) == 0) {
    return(rep(NA, nrow(df)))
  }

  out <- df[[vars[[1]]]]
  if (length(vars) > 1) {
    for (var in vars[-1]) {
      out <- dplyr::coalesce(out, df[[var]])
    }
  }
  out
}

prop_ci_percent <- function(x, n) {
  if (is.na(n) || n <= 0 || is.na(x)) {
    return(c(NA_real_, NA_real_))
  }

  ci <- tryCatch(
    suppressWarnings(stats::prop.test(x = x, n = n, correct = FALSE)$conf.int),
    error = function(e) c(NA_real_, NA_real_)
  )
  100 * ci
}

add_prop_ci <- function(df, x_var, n_var, lower_name = "ci_lower", upper_name = "ci_upper") {
  if (nrow(df) == 0) {
    df[[lower_name]] <- numeric(0)
    df[[upper_name]] <- numeric(0)
    return(df)
  }

  ci <- t(mapply(prop_ci_percent, df[[x_var]], df[[n_var]]))
  df[[lower_name]] <- ci[, 1]
  df[[upper_name]] <- ci[, 2]
  df
}

save_plot_if_data <- function(data, plot, filename, width, height,
                              units = "in", dpi = 300) {
  if (nrow(data) == 0) {
    message("Skipped figure with no plot data: ", filename)
    return(invisible(NA_character_))
  }

  save_reviewed_plot(plot, filename, width = width, height = height,
                     units = units, dpi = dpi)
}

read_reviewed_csv_required <- function(filename) {
  file <- file.path(dir_tables_reviewed, filename)
  if (!file.exists(file)) {
    stop(
      "Required reviewed table is missing: ", file,
      "\nRun 00_run_RF105_reviewed.R or the upstream reviewed script first."
    )
  }
  readr::read_csv(file, show_col_types = FALSE)
}

arm_colors <- c(comparison = "#430154", intervention = "#138B87")
stove_colors <- c(lpg = "#0072B2", biomass = "#D55E00")
change_colors <- c(more = "#2F8F5B", less = "#B6463A")

original_figure_targets <- tibble(
  original_figure = c(
    "stove_exclusive_use_since_month_received_midline.tiff",
    "stove_used_min_per_day_midline.tiff",
    "fig_time_child_more_less.png",
    "fig_time_respondent_more_less.png",
    "harassment.tiff",
    "PM_by_hour_of_day_log_smooth_span_0.3.png",
    "fig_physical_health (1).png",
    "fig_FCS_HDDS_data (1).tiff",
    "fig_lpg_willingness_to_pay.png"
  ),
  reviewed_figure = c(
    "stove_exclusive_use_since_month_received_midline_reviewed.tiff",
    "stove_used_min_per_day_midline_reviewed.tiff",
    "fig_time_child_more_less_reviewed.png",
    "fig_time_respondent_more_less_reviewed.png",
    "harassment_reviewed.tiff",
    "PM_by_hour_of_day_log_smooth_span_0.3_reviewed.png",
    "fig_physical_health_reviewed.png",
    "fig_FCS_HDDS_data_reviewed.tiff",
    "fig_lpg_willingness_to_pay_reviewed.png"
  ),
  original_file = file.path(project_root, "6_figures", "original", original_figure),
  original_exists = file.exists(original_file),
  output_folder = dir_figures_reviewed
)

write_reviewed_csv(
  original_figure_targets,
  "original_style_figure_targets_reviewed.csv",
  subfolder = "qa"
)

################################################################################
# Load reviewed inputs
################################################################################

survey_raw <- readRDS(file_survey_refugee_household) %>%
  add_rf105_aliases()

survey_population <- make_analysis_population(survey_raw)
survey <- survey_population$all_deduplicated

stove_daily_file <- file.path(
  dir_tables_reviewed,
  "geocene_stove_use_daily_analysis_dataset_reviewed.csv"
)
if (!file.exists(stove_daily_file)) {
  stop(
    "Missing reviewed Geocene stove-use table: ", stove_daily_file,
    "\nRun 8_geocene_stove_use_combined_reviewed.R before this script."
  )
}
stove_daily <- readr::read_csv(stove_daily_file, show_col_types = FALSE)

pm_indoor <- readRDS(file_pm25_indoor)

################################################################################
# Stove-use figures like the original midline Geocene plots
################################################################################

stove_midline_intervention <- stove_daily %>%
  clean_timepoint_arm() %>%
  filter(timepoint == "midline", study_arm_overall == "intervention") %>%
  mutate(
    days_after_first_receiving = as_number(days_after_first_receiving),
    months_after_first_receiving = as_number(months_after_first_receiving_numeric),
    stove_on_min_sum_lpg_zero = as_number(stove_on_min_sum_lpg_zero),
    stove_on_min_sum_biomass_zero = as_number(stove_on_min_sum_biomass_zero),
    exclusive_lpg_recalc = as.integer(as.logical(exclusive_lpg_recalc))
  )

stove_exclusive_plot_data <- stove_midline_intervention %>%
  filter(!is.na(months_after_first_receiving), !is.na(exclusive_lpg_recalc)) %>%
  group_by(months_after_first_receiving) %>%
  summarise(
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
  "original_style_stove_exclusive_midline_plot_data_reviewed.csv"
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
    x = "Months after first receiving LPG through free distribution program",
    y = "Percent of days household exclusively used LPG when cooking"
  ) +
  coord_cartesian(clip = "off")

save_plot_if_data(
  stove_exclusive_plot_data,
  fig_stove_exclusive,
  "stove_exclusive_use_since_month_received_midline_reviewed.tiff",
  width = 7,
  height = 6
)

stove_minutes_plot_data <- stove_midline_intervention %>%
  filter(!is.na(days_after_first_receiving)) %>%
  transmute(
    fcn_id,
    hh_id,
    date,
    timepoint,
    study_arm_overall,
    raw_source_file,
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
  "original_style_stove_minutes_midline_plot_data_reviewed.csv"
)
write_reviewed_csv(
  stove_minutes_summary,
  "original_style_stove_minutes_midline_summary_reviewed.csv"
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
  "stove_used_min_per_day_midline_reviewed.tiff",
  width = 7,
  height = 6
)

################################################################################
# Time-use figures like fig_time_respondent_more_less and fig_time_child_more_less
################################################################################

make_time_more_less_summary <- function(df, time_vars) {
  plot_data_long <- purrr::pmap_dfr(
    time_vars,
    function(source_variable, source_variables, outcome_label, display_order) {
      tibble(
        fcn_id = df$fcn_id,
        timepoint = df$timepoint,
        study_arm_overall = df$study_arm_overall,
        source_variable = source_variable,
        source_variables_used = paste(source_variables, collapse = "; "),
        outcome_label = outcome_label,
        display_order = display_order,
        response_code = as_number(coalesce_vars(df, source_variables))
      )
    }
  ) %>%
    mutate(
      change = case_when(
        response_code == 1 ~ "more",
        response_code == 2 ~ "same",
        response_code == 3 ~ "less",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(change))

  all_denominators <- plot_data_long %>%
    group_by(source_variable, source_variables_used, outcome_label, display_order) %>%
    summarise(n_all_nonmissing = n(), .groups = "drop")

  plot_data_long %>%
    filter(change %in% c("more", "less")) %>%
    count(source_variable, source_variables_used, outcome_label, display_order, change,
          name = "n_category") %>%
    group_by(source_variable, source_variables_used, outcome_label, display_order) %>%
    mutate(
      n_more_less = sum(n_category),
      percent = 100 * n_category / n_more_less,
      signed_percent = if_else(change == "less", -percent, percent)
    ) %>%
    ungroup() %>%
    left_join(all_denominators, by = c(
      "source_variable", "source_variables_used", "outcome_label", "display_order"
    )) %>%
    add_prop_ci("n_category", "n_more_less") %>%
    mutate(
      signed_ci_lower = if_else(change == "less", -ci_upper, ci_lower),
      signed_ci_upper = if_else(change == "less", -ci_lower, ci_upper),
      change = factor(change, levels = c("less", "more")),
      outcome_label = factor(outcome_label, levels = time_vars$outcome_label)
    ) %>%
    arrange(display_order, change)
}

respondent_time_vars <- tibble::tibble(
  source_variable = c(
    "time_harvesting_wood", "time_gathering_nonwood_items", "time_cooking",
    "time_selling_food", "time_washing_dishes", "time_washing_clothes",
    "time_collecting_water", "time_unskilled_labor", "time_employment_ngo",
    "time_accompanying_children", "time_caring_for_children",
    "time_caring_for_others", "time_eating", "time_learning",
    "time_nothing", "time_socializing", "time_sleeping"
  ),
  source_variables = list(
    "time_harvesting_wood",
    c("time_gathering_nonwood_items", "time_gathering_non.wood_items", "time_gathering_non-wood_items"),
    "time_cooking",
    "time_selling_food",
    "time_washing_dishes",
    "time_washing_clothes",
    "time_collecting_water",
    "time_unskilled_labor",
    "time_employment_ngo",
    "time_accompanying_children",
    "time_caring_for_children",
    "time_caring_for_others",
    "time_eating",
    "time_learning",
    "time_nothing",
    "time_socializing",
    "time_sleeping"
  ),
  outcome_label = c(
    "Harvesting wood", "Gathering non-wood items", "Cooking",
    "Selling food", "Washing dishes", "Washing clothes",
    "Collecting water", "Unskilled labor", "NGO employment",
    "Accompanying children", "Caring for children",
    "Caring for others", "Eating", "Learning",
    "Doing nothing", "Socializing", "Sleeping"
  ),
  display_order = seq_len(17)
)

child_time_vars <- tibble::tibble(
  source_variable = c(
    "time_child_harvesting_wood", "time_child_gathering_nonwood_items",
    "time_child_cooking", "time_child_cleaning", "time_child_collecting_water",
    "time_child_school", "time_child_nothing", "time_child_socializing",
    "time_child_sleeping"
  ),
  source_variables = list(
    "time_child_harvesting_wood",
    c("time_child_gathering_nonwood_items", "time_child_gathering_nonwood_ite",
      "time_child_gathering_non.wood_items", "time_child_gathering_non-wood_items"),
    "time_child_cooking",
    "time_child_cleaning",
    "time_child_collecting_water",
    "time_child_school",
    "time_child_nothing",
    "time_child_socializing",
    "time_child_sleeping"
  ),
  outcome_label = c(
    "Harvesting wood", "Gathering non-wood items", "Cooking", "Cleaning",
    "Collecting water", "Going to school", "Doing nothing", "Socializing",
    "Sleeping"
  ),
  display_order = seq_len(9)
)

survey_midline_intervention <- survey %>%
  filter(timepoint == "midline", study_arm_overall == "intervention")

respondent_time_plot_data <- make_time_more_less_summary(
  survey_midline_intervention,
  respondent_time_vars
)
child_time_plot_data <- make_time_more_less_summary(
  survey_midline_intervention,
  child_time_vars
)

write_reviewed_csv(
  respondent_time_plot_data,
  "original_style_time_respondent_more_less_reviewed.csv"
)
write_reviewed_csv(
  child_time_plot_data,
  "original_style_time_child_more_less_reviewed.csv"
)

fig_time_respondent <- ggplot(
  respondent_time_plot_data,
  aes(x = outcome_label, y = signed_percent / 100, fill = change)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = signed_ci_lower / 100, ymax = signed_ci_upper / 100),
    position = position_dodge(width = 0.75), width = 0.2
  ) +
  geom_hline(yintercept = 0, color = "grey35") +
  scale_fill_manual(values = change_colors, labels = c(less = "Less", more = "More")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Females reported change in time use after their households started receiving LPG",
    x = "Activity",
    y = "Percent of more/less responses",
    fill = "Time spent"
  )

save_plot_if_data(
  respondent_time_plot_data,
  fig_time_respondent,
  "fig_time_respondent_more_less_reviewed.png",
  width = 14,
  height = 6
)

fig_time_child <- ggplot(
  child_time_plot_data,
  aes(x = outcome_label, y = signed_percent / 100, fill = change)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = signed_ci_lower / 100, ymax = signed_ci_upper / 100),
    position = position_dodge(width = 0.75), width = 0.2
  ) +
  geom_hline(yintercept = 0, color = "grey35") +
  scale_fill_manual(values = change_colors, labels = c(less = "Less", more = "More")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Caregiver-reported change in time use among children after their households started receiving LPG",
    x = "Activity",
    y = "Percent of more/less responses",
    fill = "Time spent"
  )

save_plot_if_data(
  child_time_plot_data,
  fig_time_child,
  "fig_time_child_more_less_reviewed.png",
  width = 11,
  height = 5.5
)

################################################################################
# Harassment figure like the original harassment draft plot
################################################################################

harassment_detail <- read_reviewed_csv_required("descriptive_harassment_reviewed.csv")

harassment_plot_data <- harassment_detail %>%
  filter(
    timepoint == "midline",
    fuel_type %in% c("gather_scraps", "collect_wood", "receive_lpg"),
    harassment_type %in% c("insult", "belittle", "scare", "push", "hit", "kick", "choke", "weapon"),
    n_nonmissing > 0,
    !is.na(percent_harassed)
  ) %>%
  mutate(
    fuel_type_label = factor(
      fuel_type,
      levels = c("gather_scraps", "collect_wood", "receive_lpg"),
      labels = c("Gather scraps", "Collect wood", "Receive LPG")
    ),
    harassment_category_label = factor(
      harassment_category,
      levels = c("Verbal/emotional", "Physical"),
      labels = c(
        "verbal (insulted, belittled, scared)",
        "physical (pushed, hit, kicked, choked, faced a weapon)"
      )
    ),
    harassment_type_label = factor(
      harassment_type,
      levels = c("insult", "belittle", "scare", "push", "hit", "kick", "choke", "weapon"),
      labels = c("insulted", "belittled", "scared", "pushed", "hit", "kicked", "choked", "weapon threat")
    ),
    demographic_label = factor(
      demographic,
      levels = c("m", "w", "b", "g"),
      labels = c("Men", "Women", "Boys", "Girls")
    ),
    proportion_harassed = percent_harassed / 100,
    ci_lower_prop = ci_lower / 100,
    ci_upper_prop = ci_upper / 100
  )

write_reviewed_csv(
  harassment_plot_data,
  "original_style_harassment_plot_data_reviewed.csv"
)

fig_harassment <- ggplot(
  harassment_plot_data,
  aes(x = harassment_type_label, y = proportion_harassed, fill = demographic_label)
) +
  geom_col(position = position_dodge(width = 0.9), width = 0.75) +
  geom_linerange(
    aes(ymin = ci_lower_prop, ymax = ci_upper_prop),
    position = position_dodge(width = 0.9)
  ) +
  facet_wrap(harassment_category_label ~ fuel_type_label, ncol = 3, scales = "free_x") +
  scale_fill_manual(
    values = c(Men = "#31688E", Women = "#35B779", Boys = "#FDE725", Girls = "#440154"),
    drop = FALSE,
    name = "Demographic"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    panel.spacing = grid::unit(1.5, "lines")
  ) +
  labs(
    x = "Type of harassment",
    y = "Percentage of item respondents reporting harassment while collecting fuel"
  )

save_plot_if_data(
  harassment_plot_data,
  fig_harassment,
  "harassment_reviewed.tiff",
  width = 12,
  height = 7
)

################################################################################
# PM2.5 by hour of day, using reviewed clean indoor PM data
################################################################################

pm_hourly_plot_data <- pm_indoor %>%
  clean_timepoint_arm() %>%
  filter(
    !is.na(timepoint),
    !is.na(study_arm_overall),
    !is.na(PM_Estimate),
    PM_Estimate > 0,
    !is.na(nearest_min)
  ) %>%
  mutate(
    nearest_min_60 = lubridate::floor_date(nearest_min, "60 minutes"),
    PM_Estimate = as_number(PM_Estimate)
  ) %>%
  group_by(timepoint, study_arm_overall, nearest_min_60) %>%
  summarise(
    n_records = n(),
    n_households = n_distinct(fcn_id),
    n_source_files = n_distinct(raw_source_file),
    PM_Estimate_av = mean(PM_Estimate, na.rm = TRUE),
    PM_Estimate_sd = sd(PM_Estimate, na.rm = TRUE),
    se_pm_estimate = PM_Estimate_sd / sqrt(n_records),
    lower_ci = pmax(0.1, PM_Estimate_av - 1.96 * se_pm_estimate),
    upper_ci = pmax(0.1, PM_Estimate_av + 1.96 * se_pm_estimate),
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall, nearest_min_60)

write_reviewed_csv(
  pm_hourly_plot_data,
  "original_style_pm_by_hour_plot_data_reviewed.csv"
)

fig_pm_day <- ggplot(
  pm_hourly_plot_data,
  aes(x = nearest_min_60, y = PM_Estimate_av, group = study_arm_overall)
) +
  geom_line(aes(color = study_arm_overall), linewidth = 1) +
  geom_ribbon(
    aes(ymin = lower_ci, ymax = upper_ci, fill = study_arm_overall),
    alpha = 0.25,
    color = NA
  ) +
  geom_hline(yintercept = 35, color = "red", linetype = 2) +
  geom_hline(yintercept = 75, color = "black", linetype = 4) +
  geom_hline(yintercept = 500, color = "black", linetype = 6) +
  scale_x_datetime(
    labels = scales::date_format("%H:%M", tz = "Asia/Dhaka"),
    breaks = scales::date_breaks("6 hours"),
    expand = c(0, 0)
  ) +
  scale_y_log10(
    breaks = c(10, 25, 100, 300, 500, 1000),
    labels = c(10, 25, 100, 300, 500, 1000)
  ) +
  scale_color_manual(values = arm_colors, name = "Study arm") +
  scale_fill_manual(values = arm_colors, name = "Study arm") +
  annotation_logticks(sides = "l") +
  facet_wrap(~ timepoint) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.background = element_blank(),
    panel.spacing = grid::unit(2, "lines"),
    panel.border = element_rect(color = "black", fill = NA),
    strip.background = element_blank()
  ) +
  labs(
    title = "PM2.5 measurement by hour of the day",
    x = "Time",
    y = "PM2.5 (ug/m3)"
  )

save_plot_if_data(
  pm_hourly_plot_data,
  fig_pm_day,
  "PM_by_hour_of_day_log_smooth_span_0.3_reviewed.png",
  width = 10,
  height = 6
)

################################################################################
# Physical-health figure like the original child/caregiver figure
################################################################################

physical_health_plot_data <- read_reviewed_csv_required(
  "descriptive_physical_health_outcomes_reviewed.csv"
) %>%
  filter(n_nonmissing > 0, !is.na(percent)) %>%
  mutate(
    respondent_group = factor(respondent_group, levels = c("Child", "Caregiver")),
    facet_label = factor(
      paste(respondent_group, outcome_label, sep = ": "),
      levels = paste(
        respondent_group[order(display_order)],
        outcome_label[order(display_order)],
        sep = ": "
      ) %>% unique()
    )
  )

write_reviewed_csv(
  physical_health_plot_data,
  "original_style_physical_health_plot_data_reviewed.csv"
)

fig_physical_health <- ggplot(
  physical_health_plot_data,
  aes(x = timepoint, y = percent, fill = study_arm_overall)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  facet_wrap(~ facet_label, nrow = 2) +
  scale_fill_manual(values = arm_colors, drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    strip.text = element_text(size = 8)
  ) +
  labs(x = "Timepoint", y = "Percent reporting outcome", fill = "Study arm")

save_plot_if_data(
  physical_health_plot_data,
  fig_physical_health,
  "fig_physical_health_reviewed.png",
  width = 16,
  height = 8
)

################################################################################
# Food consumption score and dietary diversity figure like original FCS/HDDS plot
################################################################################

survey_food <- survey %>%
  mutate(
    fcs_staple = row_sum_vars(., c(
      "rice_adults_week", "bread_adults_week", "corn_adults_week",
      "potatoes_adults_week"
    ), cap_at = 7),
    fcs_pulses = row_sum_vars(., "lentils_adults_week", cap_at = 7),
    fcs_veggies = row_sum_vars(., "veggies_adults_week", cap_at = 7),
    fcs_fruit = row_sum_vars(., "fruit_adults_week", cap_at = 7),
    fcs_meat_fish = row_sum_vars(., c(
      "eggs_adults_week", "fish_adults_week", "poultry_adults_week",
      "goat_sheep_adults_week", "beef_adults_week"
    ), cap_at = 7),
    fcs_dairy = row_sum_vars(., "dairy_adults_week", cap_at = 7),
    fcs_sugar = row_sum_vars(., "sugar_adults_week", cap_at = 7),
    fcs_oil = row_sum_vars(., "oil_adults_week", cap_at = 7),
    fcs = row_sum_vars(
      tibble(
        fcs_staple_weight = fcs_staple * 2,
        fcs_pulses_weight = fcs_pulses * 3,
        fcs_veggies_weight = fcs_veggies * 1,
        fcs_fruit_weight = fcs_fruit * 1,
        fcs_meat_fish_weight = fcs_meat_fish * 4,
        fcs_dairy_weight = fcs_dairy * 4,
        fcs_sugar_weight = fcs_sugar * 0.5,
        fcs_oil_weight = fcs_oil * 0.5
      ),
      c(
        "fcs_staple_weight", "fcs_pulses_weight", "fcs_veggies_weight",
        "fcs_fruit_weight", "fcs_meat_fish_weight", "fcs_dairy_weight",
        "fcs_sugar_weight", "fcs_oil_weight"
      )
    ),
    fcs_category = case_when(
      is.na(fcs) ~ NA_character_,
      fcs <= 21 ~ "poor",
      fcs <= 35 ~ "borderline",
      fcs > 35 ~ "acceptable"
    ),
    hdds_cereals = hdds_food_group(., c(
      "food_consump_adult_day/1", "food_consump_adult_day/2",
      "food_consump_adult_day/3"
    )),
    hdds_tubers = hdds_food_group(., "food_consump_adult_day/4"),
    hdds_pulses = hdds_food_group(., "food_consump_adult_day/5"),
    hdds_eggs = hdds_food_group(., "food_consump_adult_day/6"),
    hdds_milk = hdds_food_group(., "food_consump_adult_day/7"),
    hdds_veggies = hdds_food_group(., "food_consump_adult_day/8"),
    hdds_fruit = hdds_food_group(., "food_consump_adult_day/9"),
    hdds_fish = hdds_food_group(., "food_consump_adult_day/10"),
    hdds_meat = hdds_food_group(., c(
      "food_consump_adult_day/11", "food_consump_adult_day/12",
      "food_consump_adult_day/13"
    )),
    hdds_oil = hdds_food_group(., "food_consump_adult_day/14"),
    hdds_sugar = hdds_food_group(., "food_consump_adult_day/15"),
    hdds_no_misc = row_sum_vars(
      tibble(
        hdds_cereals, hdds_tubers, hdds_pulses, hdds_eggs, hdds_milk,
        hdds_veggies, hdds_fruit, hdds_fish, hdds_meat, hdds_oil,
        hdds_sugar
      ),
      c(
        "hdds_cereals", "hdds_tubers", "hdds_pulses", "hdds_eggs",
        "hdds_milk", "hdds_veggies", "hdds_fruit", "hdds_fish",
        "hdds_meat", "hdds_oil", "hdds_sugar"
      )
    ),
    hdds_assume_misc_1 = if_else(!is.na(hdds_no_misc), hdds_no_misc + 1, NA_real_)
  )

fcs_hdds_plot_data <- survey_food %>%
  select(
    fcn_id,
    timepoint,
    study_arm_overall,
    hdds_no_misc,
    hdds_assume_misc_1,
    fcs,
    fcs_category
  ) %>%
  filter(!is.na(fcs), !is.na(timepoint), !is.na(study_arm_overall))

fcs_hdds_summary <- fcs_hdds_plot_data %>%
  group_by(timepoint, study_arm_overall, fcs_category) %>%
  summarise(
    n = n(),
    fcs_mean = mean(fcs, na.rm = TRUE),
    fcs_median = median(fcs, na.rm = TRUE),
    hdds_mean = mean(hdds_assume_misc_1, na.rm = TRUE),
    .groups = "drop"
  )

write_reviewed_csv(
  fcs_hdds_plot_data,
  "original_style_fcs_hdds_plot_data_reviewed.csv"
)
write_reviewed_csv(
  fcs_hdds_summary,
  "original_style_fcs_hdds_summary_reviewed.csv"
)

fig_fcs_hdds <- ggplot(
  fcs_hdds_plot_data,
  aes(x = fcs, fill = study_arm_overall)
) +
  geom_bar(aes(y = after_stat(count / sum(count)))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_vline(xintercept = 35, linetype = "dashed") +
  geom_vline(xintercept = 21.5, linetype = "dashed") +
  scale_fill_manual(
    name = "Study arm",
    breaks = c("intervention", "comparison"),
    labels = c("Intervention", "Comparison"),
    values = c(intervention = "#138B87", comparison = "#430154")
  ) +
  theme_bw() +
  labs(x = "Food consumption score", y = "percentage of households") +
  facet_grid(timepoint ~ ., scales = "fixed")

save_plot_if_data(
  fcs_hdds_plot_data,
  fig_fcs_hdds,
  "fig_FCS_HDDS_data_reviewed.tiff",
  width = 10,
  height = 6
)

################################################################################
# LPG willingness-to-pay figure like original programmatic evaluation plot
################################################################################

lpg_wtp_plot_data <- survey %>%
  mutate(
    lpg_willingness_to_pay_bdt = num_col(., "lpg_willingness_to_pay"),
    exchange_bdt_per_usd = exchange_bdt_per_usd[as.character(timepoint)],
    lpg_willingness_to_pay_usd = lpg_willingness_to_pay_bdt / exchange_bdt_per_usd
  ) %>%
  filter(timepoint == "endline", !is.na(lpg_willingness_to_pay_usd)) %>%
  select(
    fcn_id,
    timepoint,
    study_arm_overall,
    lpg_willingness_to_pay_bdt,
    exchange_bdt_per_usd,
    lpg_willingness_to_pay_usd
  )

lpg_wtp_summary <- lpg_wtp_plot_data %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_nonmissing = n(),
    mean_usd = mean(lpg_willingness_to_pay_usd, na.rm = TRUE),
    median_usd = median(lpg_willingness_to_pay_usd, na.rm = TRUE),
    p25_usd = quantile(lpg_willingness_to_pay_usd, 0.25, na.rm = TRUE),
    p75_usd = quantile(lpg_willingness_to_pay_usd, 0.75, na.rm = TRUE),
    min_usd = min(lpg_willingness_to_pay_usd, na.rm = TRUE),
    max_usd = max(lpg_willingness_to_pay_usd, na.rm = TRUE),
    .groups = "drop"
  )

write_reviewed_csv(
  lpg_wtp_plot_data,
  "original_style_lpg_willingness_to_pay_plot_data_reviewed.csv"
)
write_reviewed_csv(
  lpg_wtp_summary,
  "original_style_lpg_willingness_to_pay_summary_reviewed.csv"
)

fig_lpg_willingness_to_pay <- ggplot(lpg_wtp_plot_data) +
  geom_boxplot(aes(y = lpg_willingness_to_pay_usd), width = 0.35) +
  geom_hline(yintercept = 12.24, linetype = 3, color = "blue") +
  geom_hline(yintercept = 12.24 / 2, linetype = 2, color = "blue") +
  theme_bw() +
  labs(
    title = "Willingness to pay for 12 kg tank of LPG (2021 currency)",
    y = "Willingness to pay (USD)"
  )

save_plot_if_data(
  lpg_wtp_plot_data,
  fig_lpg_willingness_to_pay,
  "fig_lpg_willingness_to_pay_reviewed.png",
  width = 6,
  height = 5
)

################################################################################
# Output coverage audit
################################################################################

figure_coverage <- original_figure_targets %>%
  mutate(
    reviewed_file = file.path(dir_figures_reviewed, reviewed_figure),
    reviewed_exists = file.exists(reviewed_file),
    status = case_when(
      reviewed_exists ~ "reviewed_analogue_created",
      TRUE ~ "reviewed_analogue_missing"
    )
  )

write_reviewed_csv(
  figure_coverage,
  "original_style_figure_output_coverage_reviewed.csv",
  subfolder = "qa"
)

message("Original-style reviewed figure generation complete.")
