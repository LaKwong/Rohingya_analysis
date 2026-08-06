################################################################################
# RF105 reviewed descriptive outcome summaries
#
# Purpose:
#   Create checked descriptive tables and figures for requested RF105 outcomes.
#   This script contains no inferential DiD/rDiD models. It summarizes observed
#   cleaned household survey data by study arm and timepoint.
#
# Inputs:
#   4_data/clean_final/survey_refugee_household.rds
#
# Outputs:
#   Tables:  7_tables/RF105_reviewed_YYYYMMDD/descriptive_*_reviewed.csv
#   Figures: 6_figures/RF105_reviewed_YYYYMMDD/descriptive_*_reviewed.png
#   QA:      7_tables/RF105_reviewed_YYYYMMDD/qa/descriptive_requested_output_coverage_reviewed.csv
#
# Notes:
#   - Uses one deduplicated household record per fcn_id-timepoint, matching the
#     reviewed RF105 helper function make_analysis_population().
#   - FCS and HDDS calculations follow the original script:
#     5_analysis/1_Rohingya_hh_survey_analysis/6.1_food_security.R
#   - Time-use variables are self-reported categorical changes after LPG
#     (more/same/less), not measured minutes.
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

yn_col <- function(df, var) {
  if (var_present(df, var)) {
    make_yn(df[[var]])
  } else {
    rep(NA_integer_, nrow(df))
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

calc_prop_summary <- function(df, value_var = "value") {
  df %>%
    group_by(timepoint, study_arm_overall, outcome_group, outcome_name,
             outcome_label, source_variable, unit, population) %>%
    summarise(
      n_total = n(),
      n_nonmissing = sum(!is.na(.data[[value_var]])),
      n_yes = sum(.data[[value_var]] == 1, na.rm = TRUE),
      proportion = if_else(n_nonmissing > 0, n_yes / n_nonmissing, NA_real_),
      percent = 100 * proportion,
      se = if_else(
        n_nonmissing > 0,
        sqrt(proportion * (1 - proportion) / n_nonmissing),
        NA_real_
      ),
      ci_lower = pmax(0, 100 * (proportion - 1.96 * se)),
      ci_upper = pmin(100, 100 * (proportion + 1.96 * se)),
      .groups = "drop"
    ) %>%
    arrange(outcome_group, outcome_name, timepoint, study_arm_overall)
}

mean_safe <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

sd_safe <- function(x) {
  if (sum(!is.na(x)) < 2) NA_real_ else sd(x, na.rm = TRUE)
}

median_safe <- function(x) {
  if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
}

quantile_safe <- function(x, prob) {
  if (all(is.na(x))) NA_real_ else quantile(x, prob, na.rm = TRUE, names = FALSE)
}

min_safe <- function(x) {
  if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}

max_safe <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}
continuous_ci_lower <- function(mean_value, se_value, n_value) {
  if (is.na(n_value) || n_value <= 1 || is.na(se_value) || is.na(mean_value)) {
    return(NA_real_)
  }
  mean_value - qt(0.975, n_value - 1) * se_value
}

continuous_ci_upper <- function(mean_value, se_value, n_value) {
  if (is.na(n_value) || n_value <= 1 || is.na(se_value) || is.na(mean_value)) {
    return(NA_real_)
  }
  mean_value + qt(0.975, n_value - 1) * se_value
}
calc_continuous_summary <- function(df) {
  df %>%
    group_by(timepoint, study_arm_overall, outcome_group, outcome_name,
             outcome_label, source_variable, unit, population) %>%
    summarise(
      n_total = n(),
      n_nonmissing = sum(!is.na(value)),
      mean = mean_safe(value),
      sd = sd_safe(value),
      median = median_safe(value),
      p25 = quantile_safe(value, 0.25),
      p75 = quantile_safe(value, 0.75),
      min = min_safe(value),
      max = max_safe(value),
      se = if_else(n_nonmissing > 1, sd / sqrt(n_nonmissing), NA_real_),
      ci_lower = continuous_ci_lower(mean, se, n_nonmissing),
      ci_upper = continuous_ci_upper(mean, se, n_nonmissing),
      .groups = "drop"
    ) %>%
    mutate(
      across(c(mean, sd, median, p25, p75, min, max),
             ~ ifelse(is.infinite(.x) | is.nan(.x), NA_real_, .x))
    ) %>%
    arrange(outcome_group, outcome_name, timepoint, study_arm_overall)
}

summarise_binary_vars <- function(df, var_labels, outcome_group, unit = "percent") {
  var_labels %>%
    mutate(
      data = map(source_variable, function(var) {
        tibble(
          fcn_id = df$fcn_id,
          timepoint = df$timepoint,
          study_arm_overall = df$study_arm_overall,
          value = yn_col(df, var)
        )
      })
    ) %>%
    unnest(data) %>%
    mutate(
      outcome_group = outcome_group,
      unit = unit,
      population = "all_deduplicated_household_timepoint_records"
    ) %>%
    calc_prop_summary()
}

summarise_continuous_vars <- function(df, var_labels, outcome_group, unit) {
  var_labels %>%
    mutate(
      data = map(source_variable, function(var) {
        tibble(
          fcn_id = df$fcn_id,
          timepoint = df$timepoint,
          study_arm_overall = df$study_arm_overall,
          value = num_col(df, var)
        )
      })
    ) %>%
    unnest(data) %>%
    mutate(
      outcome_group = outcome_group,
      unit = unit,
      population = "all_deduplicated_household_timepoint_records"
    ) %>%
    calc_continuous_summary()
}

select_multi_response <- function(df, base_var, code) {
  code <- as.character(code)
  candidate_vars <- c(
    paste0(base_var, "/", code),
    paste0(base_var, ".", code),
    paste0(base_var, code)
  )
  candidate_vars <- candidate_vars[candidate_vars %in% names(df)]

  if (length(candidate_vars) > 0) {
    source_variable <- candidate_vars[[1]]
    value <- make_yn(df[[source_variable]])
    return(list(value = value, source_variable = source_variable))
  }

  if (base_var %in% names(df)) {
    x_chr <- str_squish(as.character(df[[base_var]]))
    x_chr[x_chr == ""] <- NA_character_
    value <- if_else(
      is.na(x_chr),
      NA_integer_,
      as.integer(str_detect(paste0(" ", x_chr, " "), paste0("(^|\\s)", code, "(\\s|$)")))
    )
    return(list(value = value, source_variable = base_var))
  }

  list(value = rep(NA_integer_, nrow(df)), source_variable = NA_character_)
}

summarise_multi_select <- function(df, base_var, option_labels, outcome_group,
                                   unit = "percent") {
  map_dfr(seq_len(nrow(option_labels)), function(i) {
    selected <- select_multi_response(df, base_var, option_labels$option_code[[i]])
    tibble(
      fcn_id = df$fcn_id,
      timepoint = df$timepoint,
      study_arm_overall = df$study_arm_overall,
      outcome_group = outcome_group,
      outcome_name = paste0(base_var, "_", option_labels$option_code[[i]]),
      outcome_label = option_labels$option_label[[i]],
      source_variable = selected$source_variable,
      unit = unit,
      population = "all_deduplicated_household_timepoint_records",
      value = selected$value
    )
  }) %>%
    calc_prop_summary()
}

summarise_categories <- function(df, var_labels, outcome_group, unit = "percent") {
  var_labels %>%
    mutate(
      data = map(source_variable, function(var) {
        if (!var %in% names(df)) {
          return(tibble(
            fcn_id = df$fcn_id,
            timepoint = df$timepoint,
            study_arm_overall = df$study_arm_overall,
            category_value = NA_character_
          ))
        }
        tibble(
          fcn_id = df$fcn_id,
          timepoint = df$timepoint,
          study_arm_overall = df$study_arm_overall,
          category_value = str_squish(as.character(df[[var]]))
        )
      })
    ) %>%
    unnest(data) %>%
    mutate(
      category_value = na_if(category_value, ""),
      outcome_group = outcome_group,
      unit = unit,
      population = "all_deduplicated_household_timepoint_records"
    ) %>%
    filter(!is.na(category_value)) %>%
    group_by(timepoint, study_arm_overall, outcome_group, outcome_name,
             outcome_label, source_variable, category_value, unit, population) %>%
    summarise(n_category = n(), .groups = "drop_last") %>%
    mutate(
      n_nonmissing = sum(n_category),
      proportion = n_category / n_nonmissing,
      percent = 100 * proportion
    ) %>%
    ungroup() %>%
    arrange(outcome_group, outcome_name, timepoint, study_arm_overall, category_value)
}

write_plot_if_data <- function(plot_data, plot, filename, width = 10, height = 6) {
  if (nrow(plot_data) > 0) {
    save_reviewed_plot(plot, filename, width = width, height = height)
  }
}

arm_colors <- c(comparison = "#3B6EA8", intervention = "#C94C4C")
change_colors <- c(more = "#2F8F5B", same = "#9AA0A6", less = "#B6463A")

survey_raw <- readr::read_rds(file_survey_refugee_household) %>%
  add_rf105_aliases()

analysis_population <- make_analysis_population(survey_raw, id_var = "fcn_id")
survey <- analysis_population$all_deduplicated

write_reviewed_csv(
  analysis_population$sample_counts,
  "descriptive_analysis_population_counts_reviewed.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  analysis_population$duplicate_records,
  "descriptive_duplicate_household_timepoint_records_reviewed.csv",
  subfolder = "qa"
)

################################################################################
# 1. Cooking fuel used in the past 30 days
################################################################################

fuel_30_labels <- tibble(
  source_variable = c(
    "fuel_30_gather_scraps", "fuel_30_collect_wood", "fuel_30_receive_wood",
    "fuel_30_buy_wood", "fuel_30_receive_lpg", "fuel_30_buy_lpg",
    "fuel_30_receive_crh", "fuel_30_buy_crh", "fuel_30_other",
    "fuel_30_lpg", "fuel_30_wood", "fuel_30_charcoal"
  ),
  outcome_name = source_variable,
  outcome_label = c(
    "Gathered scraps/leaves/twigs", "Collected firewood", "Received firewood",
    "Bought firewood", "Received LPG", "Bought LPG",
    "Received CRH/charcoal", "Bought CRH/charcoal", "Other fuel",
    "Any LPG", "Any firewood", "Any CRH/charcoal"
  )
) %>%
  filter(source_variable %in% names(survey))

fuel_30_summary <- summarise_binary_vars(
  survey, fuel_30_labels, "fuel_used_past_30_days"
)
write_reviewed_csv(
  fuel_30_summary,
  "descriptive_fuel_use_past_30_days_by_arm_timepoint_reviewed.csv"
)

fuel_30_plot_data <- fuel_30_summary %>%
  filter(n_nonmissing > 0, !is.na(percent)) %>%
  filter(source_variable %in% c(
    "fuel_30_gather_scraps", "fuel_30_collect_wood", "fuel_30_receive_wood",
    "fuel_30_buy_wood", "fuel_30_receive_lpg", "fuel_30_buy_lpg",
    "fuel_30_receive_crh", "fuel_30_buy_crh", "fuel_30_other"
  ))

fig_fuel_30 <- ggplot(
  fuel_30_plot_data,
  aes(x = timepoint, y = percent, fill = study_arm_overall)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.75), width = 0.2
  ) +
  facet_wrap(~ outcome_label, ncol = 3) +
  scale_fill_manual(values = arm_colors, drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(
    x = "Timepoint",
    y = "Households using fuel in past 30 days",
    fill = "Study arm"
  )
write_plot_if_data(
  fuel_30_plot_data,
  fig_fuel_30,
  "descriptive_fuel_use_past_30_days_by_arm_timepoint_reviewed.png",
  width = 11,
  height = 7
)

################################################################################
# 2. Usable duration of a 12 kg LPG cylinder by household size
################################################################################

lpg_days_data <- survey %>%
  transmute(
    fcn_id,
    timepoint,
    study_arm_overall,
    hh_size = num_col(., "hh_size"),
    lpg_days_possible = num_col(., "lpg_days_possible")
  ) %>%
  filter(!is.na(hh_size), !is.na(lpg_days_possible))

lpg_days_summary <- lpg_days_data %>%
  group_by(timepoint, study_arm_overall, hh_size) %>%
  summarise(
    n_total = n(),
    mean_days = mean(lpg_days_possible, na.rm = TRUE),
    sd_days = sd(lpg_days_possible, na.rm = TRUE),
    median_days = median(lpg_days_possible, na.rm = TRUE),
    p25_days = quantile(lpg_days_possible, 0.25, na.rm = TRUE, names = FALSE),
    p75_days = quantile(lpg_days_possible, 0.75, na.rm = TRUE, names = FALSE),
    min_days = min(lpg_days_possible, na.rm = TRUE),
    max_days = max(lpg_days_possible, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall, hh_size)
write_reviewed_csv(
  lpg_days_summary,
  "descriptive_lpg_days_possible_by_household_size_reviewed.csv"
)

lpg_days_qa <- lpg_days_data %>%
  summarise(
    n_nonmissing = n(),
    n_hh_size_zero_or_less = sum(hh_size <= 0, na.rm = TRUE),
    n_lpg_days_zero_or_less = sum(lpg_days_possible <= 0, na.rm = TRUE),
    n_lpg_days_over_120 = sum(lpg_days_possible > 120, na.rm = TRUE),
    min_days = min(lpg_days_possible, na.rm = TRUE),
    max_days = max(lpg_days_possible, na.rm = TRUE)
  )
write_reviewed_csv(
  lpg_days_qa,
  "descriptive_lpg_days_possible_range_qa_reviewed.csv",
  subfolder = "qa"
)

fig_lpg_days <- ggplot(
  lpg_days_data %>% filter(hh_size > 0, lpg_days_possible > 0),
  aes(x = hh_size, y = lpg_days_possible, color = study_arm_overall)
) +
  geom_jitter(width = 0.15, height = 0, alpha = 0.25, size = 1.6) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 0.8) +
  facet_wrap(~ timepoint) +
  scale_color_manual(values = arm_colors, drop = FALSE) +
  theme_classic() +
  labs(
    x = "Household size",
    y = "Days a 12 kg LPG cylinder typically lasted",
    color = "Study arm"
  )
write_plot_if_data(
  lpg_days_data,
  fig_lpg_days,
  "descriptive_lpg_days_possible_by_household_size_reviewed.png",
  width = 10,
  height = 5.5
)

################################################################################
# 3. Livelihood training and use of skills
################################################################################

livelihood_binary_labels <- tibble(
  source_variable = c(
    "livlihood_training_ever", "livlihood_training_SAFE",
    "income_skill_labor", "stove_training", "safety_stove_training"
  ),
  outcome_name = source_variable,
  outcome_label = c(
    "Any livelihood training ever",
    "Livelihood training from SAFE",
    "Income from skilled labor",
    "Received stove training",
    "Received safety/stove training"
  )
) %>%
  filter(source_variable %in% names(survey))

livelihood_training_labels <- tibble(
  option_code = c(as.character(0:8), "66"),
  option_label = c(
    "No livelihood training",
    paste0("Livelihood training option ", 1:8),
    "Other livelihood training"
  )
)

livelihood_skill_barrier_labels <- tibble(
  option_code = c(as.character(0:9), "66"),
  option_label = c(
    "No barrier reported",
    paste0("Skill-use barrier option ", 1:9),
    "Other skill-use barrier"
  )
)

livelihood_binary_summary <- summarise_binary_vars(
  survey, livelihood_binary_labels, "livelihood_training_and_skill_use"
)
livelihood_training_summary <- summarise_multi_select(
  survey, "livlihood_training", livelihood_training_labels,
  "livelihood_training_type"
)
livelihood_skills_not_summary <- summarise_multi_select(
  survey, "livlihood_skills_not", livelihood_skill_barrier_labels,
  "livelihood_skill_use_barriers"
)
livelihood_skill_freq_summary <- summarise_categories(
  survey,
  tibble(
    source_variable = "livlihood_skills_freq",
    outcome_name = "livlihood_skills_freq",
    outcome_label = "Frequency of using livelihood skills"
  ),
  "livelihood_skill_use_frequency"
)

livelihood_summary <- bind_rows(
  livelihood_binary_summary %>% mutate(category_value = NA_character_, n_category = NA_integer_),
  livelihood_training_summary %>% mutate(category_value = NA_character_, n_category = NA_integer_),
  livelihood_skills_not_summary %>% mutate(category_value = NA_character_, n_category = NA_integer_),
  livelihood_skill_freq_summary %>% mutate(
    n_total = NA_integer_, n_yes = NA_integer_, se = NA_real_,
    ci_lower = NA_real_, ci_upper = NA_real_
  )
)
write_reviewed_csv(
  livelihood_summary,
  "descriptive_livelihood_training_skills_reviewed.csv"
)

livelihood_plot_data <- livelihood_binary_summary %>%
  filter(n_nonmissing > 0, !is.na(percent)) %>%
  filter(source_variable %in% c("livlihood_training_ever", "income_skill_labor"))
fig_livelihood <- ggplot(
  livelihood_plot_data,
  aes(x = timepoint, y = percent, fill = study_arm_overall)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.75), width = 0.2
  ) +
  facet_wrap(~ outcome_label) +
  scale_fill_manual(values = arm_colors, drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  labs(x = "Timepoint", y = "Percent of households", fill = "Study arm")
write_plot_if_data(
  livelihood_plot_data,
  fig_livelihood,
  "descriptive_livelihood_training_skills_reviewed.png",
  width = 9,
  height = 5
)

################################################################################
# 4-5. Strategies used to cope with shortages of food and fuel
################################################################################

food_coping_labels <- tibble(
  option_code = c(as.character(1:13), "66", "88"),
  option_label = c(
    "Borrowed food or relied on relatives/friends",
    "Reduced food per meal",
    "Reduced meals per day",
    "Skipped all meals on some days",
    "Restricted adult food so children under 5 could eat",
    "Sold household goods",
    "Purchased food on credit",
    "Borrowed money",
    "Reduced health/education expenditures",
    "Spent savings",
    "Worked for money to buy food",
    "Sold or consumed livestock",
    "Exchanged food for other food",
    "Other",
    "Did not need to manage"
  )
)

fuel_coping_labels <- tibble(
  option_code = c(as.character(1:15), "66", "88"),
  option_label = c(
    "Borrowed fuel (wording originally borrowed food)",
    "Reduced food per meal",
    "Reduced meals per day",
    "Skipped all meals on some days",
    "Restricted adult food so children under 5 could eat",
    "Sold household goods",
    "Purchased fuel on credit",
    "Borrowed money",
    "Reduced health/education expenditures",
    "Spent savings",
    "Worked for money to buy fuel",
    "Sold livestock to purchase fuel",
    "Sold food to purchase fuel",
    "Ate food that did not need cooking",
    "Ate food that was not fully cooked",
    "Other",
    "Did not need to manage"
  )
)

food_shortage_binary <- tibble(
  source_variable = c(
    "food_insufficient_nutrition", "food_didnt_want", "food_cant_afford_2wk",
    "borrow_food", "reduce_food", "reduce_meals_lack_food",
    "not_eat_lack_food", "restrict_food"
  ),
  outcome_name = source_variable,
  outcome_label = c(
    "Food insufficient for nutrition",
    "Ate food not wanted",
    "Could not afford food in prior 2 weeks",
    "Borrowed food",
    "Reduced amount eaten per meal",
    "Reduced meals per day",
    "Skipped all meals on some days",
    "Restricted adult food"
  )
) %>%
  filter(source_variable %in% names(survey))

fuel_shortage_binary <- tibble(
  source_variable = c(
    "fuel_cant_afford_2wk", "fuel_cant_afford", "borrow_fuel", "reduce_fuel",
    "reduce_meals_lack_fuel", "not_eat_lack_fuel"
  ),
  outcome_name = source_variable,
  outcome_label = c(
    "Could not afford fuel in prior 2 weeks",
    "Could not afford fuel",
    "Borrowed fuel",
    "Reduced fuel use",
    "Reduced meals per day due to fuel shortage",
    "Skipped eating due to fuel shortage"
  )
) %>%
  distinct(source_variable, .keep_all = TRUE) %>%
  filter(source_variable %in% names(survey))

food_coping_summary <- bind_rows(
  summarise_binary_vars(survey, food_shortage_binary, "food_shortage_status_and_coping"),
  summarise_multi_select(survey, "food_cant_afford_action", food_coping_labels,
                         "food_shortage_coping_actions")
)
write_reviewed_csv(
  food_coping_summary,
  "descriptive_food_shortage_coping_reviewed.csv"
)

fuel_coping_summary <- bind_rows(
  summarise_binary_vars(survey, fuel_shortage_binary, "fuel_shortage_status_and_coping"),
  summarise_multi_select(survey, "fuel_cant_afford_action", fuel_coping_labels,
                         "fuel_shortage_coping_actions")
)
write_reviewed_csv(
  fuel_coping_summary,
  "descriptive_fuel_shortage_coping_reviewed.csv"
)

food_coping_plot_data <- food_coping_summary %>%
  filter(n_nonmissing > 0, !is.na(percent)) %>%
  filter(source_variable %in% c(
    "borrow_food", "reduce_food", "reduce_meals_lack_food",
    "not_eat_lack_food", "restrict_food"
  ))
fig_food_coping <- ggplot(
  food_coping_plot_data,
  aes(x = timepoint, y = percent, fill = study_arm_overall)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  facet_wrap(~ outcome_label, ncol = 3) +
  scale_fill_manual(values = arm_colors, drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = "Timepoint", y = "Percent of households", fill = "Study arm")
write_plot_if_data(
  food_coping_plot_data,
  fig_food_coping,
  "descriptive_food_shortage_coping_reviewed.png",
  width = 10,
  height = 6
)

fuel_coping_plot_data <- fuel_coping_summary %>%
  filter(n_nonmissing > 0, !is.na(percent)) %>%
  filter(source_variable %in% c(
    "borrow_fuel", "reduce_fuel", "reduce_meals_lack_fuel", "not_eat_lack_fuel"
  ))
fig_fuel_coping <- ggplot(
  fuel_coping_plot_data,
  aes(x = timepoint, y = percent, fill = study_arm_overall)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  facet_wrap(~ outcome_label, ncol = 2) +
  scale_fill_manual(values = arm_colors, drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = "Timepoint", y = "Percent of households", fill = "Study arm")
write_plot_if_data(
  fuel_coping_plot_data,
  fig_fuel_coping,
  "descriptive_fuel_shortage_coping_reviewed.png",
  width = 9,
  height = 6
)

################################################################################
# 6 and 16. Food insecurity and dietary diversity
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
    fcs_food_insecure = case_when(
      is.na(fcs_category) ~ NA_integer_,
      fcs_category %in% c("poor", "borderline") ~ 1L,
      fcs_category == "acceptable" ~ 0L
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

food_insecurity_summary <- bind_rows(
  summarise_continuous_vars(
    survey_food,
    tibble(source_variable = "fcs", outcome_name = "fcs", outcome_label = "Food Consumption Score"),
    "food_insecurity_fcs",
    "score"
  ),
  summarise_binary_vars(
    survey_food,
    tibble(
      source_variable = "fcs_food_insecure",
      outcome_name = "fcs_food_insecure",
      outcome_label = "Poor or borderline FCS"
    ),
    "food_insecurity_fcs",
    "percent"
  )
)

fcs_category_summary <- survey_food %>%
  filter(!is.na(fcs_category)) %>%
  count(timepoint, study_arm_overall, fcs_category, name = "n_category") %>%
  group_by(timepoint, study_arm_overall) %>%
  mutate(
    n_nonmissing = sum(n_category),
    percent = 100 * n_category / n_nonmissing
  ) %>%
  ungroup() %>%
  mutate(
    outcome_group = "food_insecurity_fcs_category",
    outcome_name = "fcs_category",
    outcome_label = "FCS category",
    source_variable = "fcs_category",
    unit = "percent",
    population = "all_deduplicated_household_timepoint_records"
  )

write_reviewed_csv(
  bind_rows(
    food_insecurity_summary %>% mutate(fcs_category = NA_character_, n_category = NA_integer_),
    fcs_category_summary %>% mutate(
      n_total = NA_integer_, n_yes = NA_integer_, proportion = NA_real_, se = NA_real_,
      ci_lower = NA_real_, ci_upper = NA_real_, mean = NA_real_, sd = NA_real_,
      median = NA_real_, p25 = NA_real_, p75 = NA_real_, min = NA_real_, max = NA_real_
    )
  ),
  "descriptive_food_insecurity_fcs_reviewed.csv"
)

fig_fcs <- ggplot(
  fcs_category_summary,
  aes(x = timepoint, y = percent, fill = fcs_category)
) +
  geom_col(width = 0.7) +
  facet_wrap(~ study_arm_overall) +
  scale_fill_manual(values = c(poor = "#B6463A", borderline = "#E6A33A", acceptable = "#2F8F5B")) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  labs(x = "Timepoint", y = "Households", fill = "FCS category")
write_plot_if_data(
  fcs_category_summary,
  fig_fcs,
  "descriptive_food_insecurity_fcs_reviewed.png",
  width = 8,
  height = 5
)

dietary_diversity_summary <- summarise_continuous_vars(
  survey_food,
  tibble(
    source_variable = c("hdds_no_misc", "hdds_assume_misc_1"),
    outcome_name = c("hdds_no_misc", "hdds_assume_misc_1"),
    outcome_label = c(
      "HDDS without miscellaneous group",
      "HDDS assuming miscellaneous group equals 1"
    )
  ),
  "dietary_diversity",
  "score"
)
write_reviewed_csv(
  dietary_diversity_summary,
  "descriptive_dietary_diversity_reviewed.csv"
)

fig_hdds <- ggplot(
  dietary_diversity_summary %>% filter(source_variable == "hdds_assume_misc_1", n_nonmissing > 0, !is.na(mean)),
  aes(x = timepoint, y = mean, fill = study_arm_overall)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.75), width = 0.2
  ) +
  scale_fill_manual(values = arm_colors, drop = FALSE) +
  theme_classic() +
  labs(
    x = "Timepoint",
    y = "Mean household dietary diversity score",
    fill = "Study arm"
  )
write_plot_if_data(
  dietary_diversity_summary,
  fig_hdds,
  "descriptive_dietary_diversity_reviewed.png",
  width = 8,
  height = 5
)

################################################################################
# 7 and 18. Asthma, severe asthma, and physical health outcomes
################################################################################

add_health_descriptive_vars <- function(df) {
  health_vars <- c(
    "target_child_eye_red", "target_child_eye_itch", "target_child_cough",
    "target_child_resp_rate", "target_child_fever", "target_child_weight_loss",
    "target_child_lethargy", "target_child_clinic_resp", "target_child_wheezing",
    "target_child_disturbed_sleep", "target_child_distrubed_speech",
    "respondent_cough", "respondent_wheezing", "respondent_disturbed_sleep",
    "respondent_disturbed_speech", "respondent_eye_red", "respondent_eye_itch",
    "respondent_eye_sore", "respondent_headache", "respondent_backache",
    "resp_rate_reported_respondent", "weight_loss_reported_respondent"
  )

  for (var in intersect(health_vars, names(df))) {
    yn_name <- paste0(var, "_yn")
    if (!yn_name %in% names(df)) {
      df[[yn_name]] <- make_yn(df[[var]])
    }
  }

  if ("target_child_distrubed_speech_yn" %in% names(df) &&
      "target_child_disturbed_speech_yn" %notin% names(df)) {
    df$target_child_disturbed_speech_yn <- df$target_child_distrubed_speech_yn
  }

  if (all(c("target_child_wheezing_yn", "target_child_distrubed_speech_yn") %in% names(df))) {
    df <- df %>%
      mutate(
        target_child_asthma = case_when(
          target_child_wheezing_yn == 1 ~ 1L,
          target_child_wheezing_yn == 0 ~ 0L,
          TRUE ~ NA_integer_
        ),
        target_child_severe_asthma = case_when(
          target_child_wheezing_yn == 1 & target_child_distrubed_speech_yn == 1 ~ 1L,
          target_child_wheezing_yn == 0 | target_child_distrubed_speech_yn == 0 ~ 0L,
          TRUE ~ NA_integer_
        )
      )
  }

  df
}

survey_health <- add_health_descriptive_vars(survey)

asthma_labels <- tibble(
  source_variable = c("target_child_asthma", "target_child_severe_asthma"),
  outcome_name = source_variable,
  outcome_label = c("Child asthma proxy", "Child severe asthma proxy")
) %>%
  filter(source_variable %in% names(survey_health))

asthma_summary <- summarise_binary_vars(
  survey_health, asthma_labels, "asthma_and_severe_asthma"
)
write_reviewed_csv(
  asthma_summary,
  "descriptive_child_asthma_reviewed.csv"
)

fig_asthma <- ggplot(
  asthma_summary %>% filter(n_nonmissing > 0, !is.na(percent)),
  aes(x = timepoint, y = percent, fill = study_arm_overall)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.75), width = 0.2
  ) +
  facet_wrap(~ outcome_label) +
  scale_fill_manual(values = arm_colors, drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  labs(x = "Timepoint", y = "Percent of children", fill = "Study arm")
write_plot_if_data(
  asthma_summary,
  fig_asthma,
  "descriptive_child_asthma_reviewed.png",
  width = 8,
  height = 5
)

physical_health_labels <- tibble(
  source_variable = c(
    "target_child_cough_yn",
    "target_child_resp_rate_yn",
    "target_child_wheezing_yn",
    "target_child_eye_red_yn",
    "target_child_eye_itch_yn",
    "target_child_lethargy_yn",
    "target_child_weight_loss_yn",
    "target_child_fever_yn",
    "target_child_clinic_resp_yn",
    "respondent_cough_yn",
    "resp_rate_reported_respondent_yn",
    "respondent_wheezing_yn",
    "respondent_eye_red_yn",
    "respondent_eye_itch_yn",
    "respondent_eye_sore_yn",
    "weight_loss_reported_respondent_yn",
    "respondent_headache_yn",
    "respondent_backache_yn"
  ),
  outcome_name = source_variable,
  outcome_label = c(
    "Cough",
    "Increased respiratory rate today",
    "Current wheeze",
    "Red eyes",
    "Itchy eyes",
    "Lethargy",
    "Unexplained weight loss in 3 mo.",
    "Fever",
    "Clinic visit for respiratory complaint",
    "Cough",
    "Increased respiratory rate today",
    "Current wheeze",
    "Red eyes",
    "Itchy eyes",
    "Sore eyes",
    "Unexplained weight loss in 3 mo.",
    "Headache",
    "Backache"
  ),
  respondent_group = c(rep("Child", 9), rep("Caregiver", 9)),
  display_order = seq_len(18)
) %>%
  filter(source_variable %in% names(survey_health))

physical_health_summary <- summarise_binary_vars(
  survey_health,
  physical_health_labels %>% select(source_variable, outcome_name, outcome_label),
  "physical_health_outcomes"
) %>%
  left_join(
    physical_health_labels %>% select(source_variable, respondent_group, display_order),
    by = "source_variable"
  ) %>%
  arrange(display_order, timepoint, study_arm_overall)
write_reviewed_csv(
  physical_health_summary,
  "descriptive_physical_health_outcomes_reviewed.csv"
)

physical_health_plot_data <- physical_health_summary %>%
  filter(n_nonmissing > 0, !is.na(percent)) %>%
  mutate(
    facet_label = factor(
      paste(respondent_group, outcome_label, sep = ": "),
      levels = paste(
        physical_health_labels$respondent_group,
        physical_health_labels$outcome_label,
        sep = ": "
      )
    )
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
write_plot_if_data(
  physical_health_plot_data,
  fig_physical_health,
  "descriptive_physical_health_child_caregiver_reviewed.png",
  width = 16,
  height = 8
)

################################################################################
# 8-12. Time collecting fuel and time-use changes
################################################################################

fuel_time_data <- survey %>%
  transmute(
    fcn_id,
    timepoint,
    study_arm_overall,
    collect_wood_walk_hr = num_col(., "collect_wood_walk_hr"),
    receive_lpg_walk = num_col(., "receive_lpg_walk"),
    receive_lpg_wait = num_col(., "receive_lpg_wait"),
    buy_lpg_walk = num_col(., "buy_lpg_walk"),
    buy_lpg_wait = num_col(., "buy_lpg_wait"),
    receive_crh_walk = num_col(., "receive_crh_walk"),
    receive_crh_wait = num_col(., "receive_crh_wait")
  ) %>%
  mutate(
    receive_lpg_walk_wait = receive_lpg_walk + receive_lpg_wait,
    buy_lpg_walk_wait = buy_lpg_walk + buy_lpg_wait,
    receive_crh_walk_wait = receive_crh_walk + receive_crh_wait
  ) %>%
  pivot_longer(
    cols = c(
      collect_wood_walk_hr, receive_lpg_walk_wait,
      buy_lpg_walk_wait, receive_crh_walk_wait
    ),
    names_to = "fuel_collection_activity",
    values_to = "value"
  ) %>%
  mutate(
    value = if_else(value < 0.01, NA_real_, value),
    outcome_group = "fuel_collection_time",
    outcome_name = fuel_collection_activity,
    outcome_label = recode(
      fuel_collection_activity,
      collect_wood_walk_hr = "Collect firewood: walking time",
      receive_lpg_walk_wait = "Receive LPG: walking plus waiting time",
      buy_lpg_walk_wait = "Buy LPG: walking plus waiting time",
      receive_crh_walk_wait = "Receive CRH/charcoal: walking plus waiting time"
    ),
    source_variable = fuel_collection_activity,
    unit = "hours",
    population = "all_deduplicated_household_timepoint_records"
  )

fuel_time_summary <- calc_continuous_summary(fuel_time_data)
write_reviewed_csv(
  fuel_time_summary,
  "descriptive_fuel_collection_time_reviewed.csv"
)

fig_fuel_time <- ggplot(
  fuel_time_data %>% filter(!is.na(value)),
  aes(x = timepoint, y = value, fill = study_arm_overall)
) +
  geom_boxplot(outlier.alpha = 0.3) +
  facet_wrap(~ outcome_label, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = arm_colors, drop = FALSE) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = "Timepoint", y = "Hours", fill = "Study arm")
write_plot_if_data(
  fuel_time_data %>% filter(!is.na(value)),
  fig_fuel_time,
  "descriptive_fuel_collection_time_reviewed.png",
  width = 10,
  height = 7
)

time_change_labels <- tibble(
  source_variable = c(
    "time_harvesting_wood",
    "time_cooking",
    "time_eating",
    "time_caring_for_children",
    "time_child_school"
  ),
  outcome_name = source_variable,
  outcome_label = c(
    "Time collecting/harvesting wood",
    "Time cooking",
    "Time eating (proxy for requested self-care; no exact self-care variable found)",
    "Time caring for children",
    "Children's time going to school"
  )
) %>%
  filter(source_variable %in% names(survey))

time_change_data <- time_change_labels %>%
  mutate(
    data = map(source_variable, function(var) {
      tibble(
        fcn_id = survey$fcn_id,
        timepoint = survey$timepoint,
        study_arm_overall = survey$study_arm_overall,
        more_less_code = str_squish(as.character(survey[[var]]))
      )
    })
  ) %>%
  unnest(data) %>%
  mutate(
    more_less = case_when(
      more_less_code == "1" ~ "more",
      more_less_code == "2" ~ "same",
      more_less_code == "3" ~ "less",
      TRUE ~ NA_character_
    ),
    more_less = factor(more_less, levels = c("more", "same", "less")),
    outcome_group = "time_use_change_after_lpg",
    unit = "percent",
    population = "all_deduplicated_household_timepoint_records"
  ) %>%
  filter(!is.na(more_less))

time_change_summary <- time_change_data %>%
  count(timepoint, study_arm_overall, outcome_group, outcome_name, outcome_label,
        source_variable, unit, population, more_less, name = "n_category") %>%
  group_by(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, source_variable, unit, population) %>%
  mutate(
    n_nonmissing = sum(n_category),
    percent = 100 * n_category / n_nonmissing
  ) %>%
  ungroup() %>%
  arrange(outcome_name, timepoint, study_arm_overall, more_less)
write_reviewed_csv(
  time_change_summary,
  "descriptive_time_use_change_reviewed.csv"
)

fig_time_change <- ggplot(
  time_change_summary,
  aes(x = timepoint, y = percent, fill = more_less)
) +
  geom_col(width = 0.7) +
  facet_grid(outcome_label ~ study_arm_overall) +
  scale_fill_manual(values = change_colors, drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1), strip.text.y = element_text(angle = 0)) +
  labs(x = "Timepoint", y = "Households reporting change", fill = "Time spent")
write_plot_if_data(
  time_change_summary,
  fig_time_change,
  "descriptive_time_use_change_reviewed.png",
  width = 12,
  height = 8
)

################################################################################
# 13-15. Household expenditures
################################################################################

survey_expenditures <- survey %>%
  mutate(
    exchange_bdt_per_usd = exchange_bdt_per_usd[as.character(timepoint)],
    spent_food_bdt = num_col(., "spent_food"),
    buy_wood_cost_bdt = num_col(., "buy_wood_cost"),
    spent_hh_items_bdt = num_col(., "spent_hh_items"),
    spent_hygiene_bdt = num_col(., "spent_hygiene"),
    spent_tobacco_pan_bdt = num_col(., "spent_tobacco_pan"),
    spent_transport_bdt = num_col(., "spent_transport"),
    other_expenditures_bdt = num_col(., "other_expenditures"),
    spent_other_than_firewood_food_bdt = row_sum_vars(
      tibble(
        spent_hh_items_bdt, spent_hygiene_bdt, spent_tobacco_pan_bdt,
        spent_transport_bdt, other_expenditures_bdt
      ),
      c(
        "spent_hh_items_bdt", "spent_hygiene_bdt", "spent_tobacco_pan_bdt",
        "spent_transport_bdt", "other_expenditures_bdt"
      )
    ),
    spent_food_usd = spent_food_bdt / exchange_bdt_per_usd,
    buy_wood_cost_usd = buy_wood_cost_bdt / exchange_bdt_per_usd,
    spent_other_than_firewood_food_usd = spent_other_than_firewood_food_bdt / exchange_bdt_per_usd
  )

expenditure_labels <- tibble(
  source_variable = c(
    "buy_wood_cost_usd", "spent_food_usd",
    "spent_other_than_firewood_food_usd"
  ),
  outcome_name = source_variable,
  outcome_label = c(
    "Money spent on firewood",
    "Money spent on food",
    "Money spent on items other than firewood and food"
  )
)

expenditure_summary <- summarise_continuous_vars(
  survey_expenditures,
  expenditure_labels,
  "household_expenditures",
  "USD per month or recall period as cleaned"
)
write_reviewed_csv(
  expenditure_summary,
  "descriptive_expenditures_reviewed.csv"
)

fig_expenditures <- ggplot(
  expenditure_summary %>% filter(n_nonmissing > 0, !is.na(mean)),
  aes(x = timepoint, y = mean, fill = study_arm_overall)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = pmax(0, ci_lower), ymax = ci_upper),
    position = position_dodge(width = 0.75), width = 0.2
  ) +
  facet_wrap(~ outcome_label, scales = "free_y") +
  scale_fill_manual(values = arm_colors, drop = FALSE) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = "Timepoint", y = "Mean amount in USD", fill = "Study arm")
write_plot_if_data(
  expenditure_summary,
  fig_expenditures,
  "descriptive_expenditures_reviewed.png",
  width = 10,
  height = 5.5
)

################################################################################
# 17. Harassment while collecting/procuring fuel
################################################################################

# The draft harassment script intentionally did not use baseline harassment
# variables for causal comparisons because the baseline recall period was not
# harmonized with the follow-up "ever since arriving at camp" wording. This
# descriptive section therefore reports observed baseline and midline prevalence
# with explicit source-variable notes; rDiD estimability is audited in
# 4_rdid_xgboost_reviewed.R rather than modeled here.

harassment_type_lookup <- tribble(
  ~harassment_type, ~harassment_type_label, ~harassment_category, ~display_order,
  "insult", "Insulted", "Verbal/emotional", 1,
  "belittle", "Belittled", "Verbal/emotional", 2,
  "scare", "Scared", "Verbal/emotional", 3,
  "push", "Pushed", "Physical", 4,
  "hit", "Hit", "Physical", 5,
  "kick", "Kicked", "Physical", 6,
  "choke", "Choked", "Physical", 7,
  "weapon", "Threatened with a weapon", "Physical", 8,
  "sex_lang", "Sexual language", "Sexual", 9,
  "sex_contact", "Sexual contact", "Sexual", 10,
  "sex_rumor", "Sexual rumors", "Sexual", 11,
  "clothing_pull", "Clothing pulled", "Sexual", 12,
  "sex_corner", "Sexual cornering", "Sexual", 13
)

harassment_category_levels <- c("Verbal/emotional", "Physical", "Sexual", "Any harassment")

harassment_fuel_lookup <- tribble(
  ~fuel_type, ~fuel_type_label,
  "gather_scraps", "Gather scraps/leaves/twigs",
  "collect_wood", "Collect firewood",
  "buy_wood", "Buy firewood",
  "receive_wood", "Receive firewood",
  "receive_lpg", "Receive LPG",
  "buy_lpg", "Buy LPG",
  "receive_crh", "Receive CRH/charcoal",
  "buy_crh", "Buy CRH/charcoal"
)

harassment_demographic_lookup <- tribble(
  ~demographic, ~demographic_label,
  "w", "Women",
  "g", "Girls",
  "m", "Men",
  "b", "Boys"
)

harassment_prop_ci <- function(x, n) {
  if (is.na(n) || n <= 0 || is.na(x)) {
    return(c(NA_real_, NA_real_))
  }

  ci <- tryCatch(
    suppressWarnings(stats::prop.test(x = x, n = n, correct = FALSE)$conf.int),
    error = function(e) c(NA_real_, NA_real_)
  )
  100 * ci
}

add_harassment_ci <- function(df) {
  if (nrow(df) == 0) {
    df$ci_lower <- numeric(0)
    df$ci_upper <- numeric(0)
    return(df)
  }

  ci <- t(mapply(harassment_prop_ci, df$n_harassed, df$n_nonmissing))
  df$ci_lower <- ci[, 1]
  df$ci_upper <- ci[, 2]
  df
}

summarise_harassment_binary <- function(df, grouping_vars) {
  if (nrow(df) == 0) {
    return(tibble())
  }

  df %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      n_total = n(),
      n_nonmissing = sum(!is.na(harassed)),
      n_harassed = sum(harassed == 1, na.rm = TRUE),
      percent_harassed = if_else(n_nonmissing > 0, 100 * n_harassed / n_nonmissing, NA_real_),
      .groups = "drop"
    ) %>%
    add_harassment_ci()
}

harassment_household_sources <- bind_rows(
  harassment_type_lookup %>%
    transmute(
      source_variable = paste0(harassment_type, "_hh"),
      harassment_type,
      harassment_type_label,
      harassment_category,
      display_order,
      source_form = "baseline_no_ever_suffix",
      intended_timepoint = "baseline",
      recall_note = paste(
        "Baseline source has no _ever suffix and the draft harassment code",
        "flags these variables as not usable for causal comparison because",
        "the recall period was not specified."
      )
    ),
  harassment_type_lookup %>%
    transmute(
      source_variable = paste0(harassment_type, "_hh_ever"),
      harassment_type,
      harassment_type_label,
      harassment_category,
      display_order,
      source_form = "midline_ever_suffix",
      intended_timepoint = "midline",
      recall_note = paste(
        "Midline source uses _ever wording for events since arriving at camp.",
        "These variables are descriptive only because baseline wording differs."
      )
    )
) %>%
  mutate(available = source_variable %in% names(survey))

harassment_fuel_person_sources <- tidyr::expand_grid(
  harassment_fuel_lookup,
  harassment_type_lookup,
  harassment_demographic_lookup
) %>%
  mutate(
    source_variable = paste(fuel_type, harassment_type, demographic, "ever", sep = "_"),
    source_form = "midline_fuel_person_ever",
    intended_timepoint = "midline",
    recall_note = paste(
      "Fuel/person harassment detail variables are available with _ever wording",
      "at midline only in clean_final. The same-timepoint person-by-fuel",
      "collector denominator used in the draft script is not available in",
      "clean_final, so the reviewed denominator is nonmissing item responses."
    ),
    available = source_variable %in% names(survey)
  )

harassment_all_sources <- bind_rows(
  harassment_household_sources %>% mutate(variable_role = "household_harassment_type"),
  harassment_fuel_person_sources %>% mutate(variable_role = "fuel_person_harassment_detail")
)

harassment_available_vars <- harassment_all_sources %>%
  filter(available) %>%
  pull(source_variable) %>%
  unique()

harassment_variable_counts <- tibble()
if (length(harassment_available_vars) > 0) {
  harassment_variable_counts <- survey %>%
    select(timepoint, study_arm_overall, all_of(harassment_available_vars)) %>%
    pivot_longer(
      cols = all_of(harassment_available_vars),
      names_to = "source_variable",
      values_to = "value"
    ) %>%
    mutate(value_num = as_number(value)) %>%
    group_by(source_variable, timepoint, study_arm_overall) %>%
    summarise(
      n_nonmissing = sum(!is.na(value)),
      n_positive = sum(!is.na(value_num) & value_num > 0),
      .groups = "drop"
    )
}

harassment_variable_availability <- harassment_all_sources %>%
  select(variable_role, source_variable, source_form, intended_timepoint,
         harassment_category, harassment_type, harassment_type_label,
         fuel_type, fuel_type_label, demographic, demographic_label,
         recall_note, available) %>%
  left_join(harassment_variable_counts, by = "source_variable") %>%
  mutate(
    n_nonmissing = replace_na(n_nonmissing, 0L),
    n_positive = replace_na(n_positive, 0L)
  ) %>%
  arrange(variable_role, source_variable, timepoint, study_arm_overall)

write_reviewed_csv(
  harassment_variable_availability,
  "descriptive_harassment_variable_availability_reviewed.csv",
  subfolder = "qa"
)

household_harassment_vars <- harassment_household_sources %>%
  filter(available) %>%
  pull(source_variable)

harassment_household_long <- tibble()
if (length(household_harassment_vars) > 0) {
  harassment_household_long <- survey %>%
    select(fcn_id, timepoint, study_arm_overall, all_of(household_harassment_vars)) %>%
    pivot_longer(
      cols = all_of(household_harassment_vars),
      names_to = "source_variable",
      values_to = "times_reported"
    ) %>%
    left_join(harassment_household_sources, by = "source_variable") %>%
    mutate(
      times_reported = as_number(times_reported),
      harassed = case_when(
        is.na(times_reported) ~ NA_integer_,
        times_reported > 0 ~ 1L,
        times_reported == 0 ~ 0L,
        TRUE ~ NA_integer_
      ),
      outcome_group = "harassment_household_type",
      outcome_name = harassment_type,
      outcome_label = harassment_type_label,
      population = "all_deduplicated_household_timepoint_records"
    )
}

harassment_household_type_summary <- summarise_harassment_binary(
  harassment_household_long,
  c("timepoint", "study_arm_overall", "outcome_group", "outcome_name",
    "outcome_label", "harassment_category", "source_variable", "source_form",
    "recall_note", "population")
)

if (nrow(harassment_household_type_summary) > 0) {
  harassment_household_type_summary <- harassment_household_type_summary %>%
    left_join(
      harassment_type_lookup %>% select(outcome_name = harassment_type, display_order),
      by = "outcome_name"
    ) %>%
    arrange(display_order, timepoint, study_arm_overall) %>%
    filter(n_nonmissing > 0) %>%
    select(-display_order)
}

write_reviewed_csv(
  harassment_household_type_summary,
  "descriptive_harassment_household_type_reviewed.csv"
)

harassment_household_category_long <- tibble()
if (nrow(harassment_household_long) > 0) {
  harassment_household_category_long <- harassment_household_long %>%
    group_by(fcn_id, timepoint, study_arm_overall, harassment_category) %>%
    summarise(
      n_nonmissing_components = sum(!is.na(harassed)),
      harassed = case_when(
        any(harassed == 1, na.rm = TRUE) ~ 1L,
        n_nonmissing_components > 0 ~ 0L,
        TRUE ~ NA_integer_
      ),
      source_variable = paste(sort(unique(source_variable[!is.na(times_reported)])), collapse = "; "),
      source_form = paste(sort(unique(source_form[!is.na(times_reported)])), collapse = "; "),
      recall_note = paste(sort(unique(recall_note[!is.na(times_reported)])), collapse = " "),
      .groups = "drop"
    ) %>%
    bind_rows(
      harassment_household_long %>%
        group_by(fcn_id, timepoint, study_arm_overall) %>%
        summarise(
          harassment_category = "Any harassment",
          n_nonmissing_components = sum(!is.na(harassed)),
          harassed = case_when(
            any(harassed == 1, na.rm = TRUE) ~ 1L,
            n_nonmissing_components > 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          source_variable = paste(sort(unique(source_variable[!is.na(times_reported)])), collapse = "; "),
          source_form = paste(sort(unique(source_form[!is.na(times_reported)])), collapse = "; "),
          recall_note = paste(sort(unique(recall_note[!is.na(times_reported)])), collapse = " "),
          .groups = "drop"
        )
    ) %>%
    mutate(
      outcome_group = "harassment_household_category",
      outcome_name = str_replace_all(str_to_lower(harassment_category), "[^a-z0-9]+", "_"),
      outcome_name = str_replace(outcome_name, "_$", ""),
      outcome_label = harassment_category,
      population = "all_deduplicated_household_timepoint_records"
    )
}

harassment_household_category_summary <- summarise_harassment_binary(
  harassment_household_category_long,
  c("timepoint", "study_arm_overall", "outcome_group", "outcome_name",
    "outcome_label", "harassment_category", "source_variable", "source_form",
    "recall_note", "population")
) %>%
  mutate(
    harassment_category = factor(harassment_category, levels = harassment_category_levels),
    outcome_label = factor(outcome_label, levels = harassment_category_levels)
  ) %>%
  arrange(harassment_category, timepoint, study_arm_overall) %>%
  filter(n_nonmissing > 0)

write_reviewed_csv(
  harassment_household_category_summary,
  "descriptive_harassment_household_category_reviewed.csv"
)

fuel_person_harassment_vars <- harassment_fuel_person_sources %>%
  filter(available) %>%
  pull(source_variable)

harassment_fuel_person_long <- tibble()
if (length(fuel_person_harassment_vars) > 0) {
  harassment_fuel_person_long <- survey %>%
    select(fcn_id, timepoint, study_arm_overall, all_of(fuel_person_harassment_vars)) %>%
    pivot_longer(
      cols = all_of(fuel_person_harassment_vars),
      names_to = "source_variable",
      values_to = "times_reported"
    ) %>%
    left_join(harassment_fuel_person_sources, by = "source_variable") %>%
    mutate(
      times_reported = as_number(times_reported),
      harassed = case_when(
        is.na(times_reported) ~ NA_integer_,
        times_reported > 0 ~ 1L,
        times_reported == 0 ~ 0L,
        TRUE ~ NA_integer_
      ),
      outcome_group = "harassment_fuel_person_detail",
      population = "all_deduplicated_household_timepoint_records"
    )
}

harassment_fuel_person_summary <- harassment_fuel_person_long %>%
  group_by(timepoint, study_arm_overall, outcome_group, fuel_type, fuel_type_label,
           harassment_category, harassment_type, harassment_type_label, display_order,
           demographic, demographic_label, source_variable, source_form, recall_note,
           population) %>%
  summarise(
    n_total = n(),
    n_nonmissing = sum(!is.na(harassed)),
    n_harassed = sum(harassed == 1, na.rm = TRUE),
    percent_harassed = if_else(n_nonmissing > 0, 100 * n_harassed / n_nonmissing, NA_real_),
    mean_times_reported = mean_safe(times_reported),
    .groups = "drop"
  ) %>%
  add_harassment_ci() %>%
  arrange(fuel_type, display_order, demographic, timepoint, study_arm_overall) %>%
  filter(n_nonmissing > 0)

write_reviewed_csv(
  harassment_fuel_person_summary,
  "descriptive_harassment_fuel_person_reviewed.csv"
)

# Backward-compatible reviewed harassment table: detailed fuel/person harassment,
# matching the structure of the earlier reviewed output but with clearer notes.
write_reviewed_csv(
  harassment_fuel_person_summary,
  "descriptive_harassment_reviewed.csv"
)

harassment_category_plot_data <- harassment_household_category_summary %>%
  filter(n_nonmissing > 0) %>%
  mutate(
    outcome_label = factor(as.character(outcome_label), levels = harassment_category_levels),
    timepoint = factor(timepoint, levels = timepoint_levels)
  )

fig_harassment_category <- ggplot(
  harassment_category_plot_data,
  aes(x = timepoint, y = percent_harassed, fill = study_arm_overall)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = pmax(0, ci_lower), ymax = ci_upper),
    position = position_dodge(width = 0.75), width = 0.2
  ) +
  facet_wrap(~ outcome_label, ncol = 4) +
  scale_fill_manual(values = arm_colors, drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(
    x = "Timepoint",
    y = "Households reporting harassment",
    fill = "Study arm"
  )
write_plot_if_data(
  harassment_category_plot_data,
  fig_harassment_category,
  "descriptive_harassment_household_category_reviewed.png",
  width = 10,
  height = 5.5
)

harassment_plot_data <- harassment_fuel_person_summary %>%
  filter(
    timepoint == "midline",
    fuel_type %in% c("gather_scraps", "collect_wood", "receive_lpg"),
    harassment_category %in% c("Verbal/emotional", "Physical"),
    n_nonmissing > 0
  ) %>%
  mutate(
    harassment_type_label = factor(
      harassment_type_label,
      levels = harassment_type_lookup$harassment_type_label
    ),
    facet_label = paste(harassment_category, fuel_type_label, sep = " - ")
  )

fig_harassment <- ggplot(
  harassment_plot_data,
  aes(x = harassment_type_label, y = percent_harassed, fill = demographic_label)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(
    aes(ymin = pmax(0, ci_lower), ymax = ci_upper),
    position = position_dodge(width = 0.75), width = 0.2
  ) +
  facet_wrap(~ facet_label, scales = "free_x", ncol = 3) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(
    x = "Type of harassment",
    y = "Percent reporting harassment",
    fill = "Person"
  )
write_plot_if_data(
  harassment_plot_data,
  fig_harassment,
  "descriptive_harassment_reviewed.png",
  width = 12,
  height = 7
)

################################################################################# Coverage table and methods notes
################################################################################

coverage <- tribble(
  ~item, ~requested_output, ~table_file, ~figure_file, ~status, ~note,
  1, "Types of cooking fuel used in past 30 days by arm/timepoint", "descriptive_fuel_use_past_30_days_by_arm_timepoint_reviewed.csv", "descriptive_fuel_use_past_30_days_by_arm_timepoint_reviewed.png", "complete", "Uses clean_final fuel_30 variables plus reviewed aliases for any LPG/wood/CRH.",
  2, "Usable duration of 12 kg LPG cylinder by household size", "descriptive_lpg_days_possible_by_household_size_reviewed.csv", "descriptive_lpg_days_possible_by_household_size_reviewed.png", "complete", "Uses hh_size and lpg_days_possible; QA table flags zero and >120 day values.",
  3, "Livelihood training and use of skills", "descriptive_livelihood_training_skills_reviewed.csv", "descriptive_livelihood_training_skills_reviewed.png", "complete_with_label_limitations", "Training/skill barrier option text was not preserved in clean_final; code outputs option codes and broad labels.",
  4, "Strategies used to cope with shortage of food", "descriptive_food_shortage_coping_reviewed.csv", "descriptive_food_shortage_coping_reviewed.png", "complete", "Coping labels copied from 3_data_cleaning/1.5_define_vector_columns.R.",
  5, "Strategies used to cope with shortage of fuel", "descriptive_fuel_shortage_coping_reviewed.csv", "descriptive_fuel_shortage_coping_reviewed.png", "complete", "Coping labels copied from 3_data_cleaning/1.5_define_vector_columns.R; code 1 note retained.",
  6, "Food insecurity", "descriptive_food_insecurity_fcs_reviewed.csv", "descriptive_food_insecurity_fcs_reviewed.png", "complete", "Recalculates FCS from weekly food-frequency variables and categorizes poor/borderline/acceptable.",
  7, "Asthma and severe asthma", "descriptive_child_asthma_reviewed.csv", "descriptive_child_asthma_reviewed.png", "complete", "Uses reviewed child wheeze proxy and severe asthma proxy from wheeze plus disturbed speech.",
  8, "Time collecting fuel", "descriptive_fuel_collection_time_reviewed.csv", "descriptive_fuel_collection_time_reviewed.png", "complete", "Summarizes walking/waiting time variables in hours; values <0.01 treated as missing as in old script.",
  9, "Time cooking", "descriptive_time_use_change_reviewed.csv", "descriptive_time_use_change_reviewed.png", "complete", "Categorical more/same/less change variable time_cooking.",
  10, "Time caring for self", "descriptive_time_use_change_reviewed.csv", "descriptive_time_use_change_reviewed.png", "partial", "No exact time_caring_for_self variable found in clean_final; time_eating is shown as a proxy and flagged in the label.",
  11, "Time caring for children", "descriptive_time_use_change_reviewed.csv", "descriptive_time_use_change_reviewed.png", "complete", "Categorical more/same/less change variable time_caring_for_children.",
  12, "Time for children to go to school", "descriptive_time_use_change_reviewed.csv", "descriptive_time_use_change_reviewed.png", "complete", "Categorical more/same/less change variable time_child_school.",
  13, "Money spent on firewood", "descriptive_expenditures_reviewed.csv", "descriptive_expenditures_reviewed.png", "complete", "Uses buy_wood_cost converted from BDT to USD using reviewed exchange-rate table.",
  14, "Money spent on food", "descriptive_expenditures_reviewed.csv", "descriptive_expenditures_reviewed.png", "complete", "Uses spent_food converted from BDT to USD using reviewed exchange-rate table.",
  15, "Money spent on items other than firewood and food", "descriptive_expenditures_reviewed.csv", "descriptive_expenditures_reviewed.png", "complete", "Uses sum of spent_hh_items, spent_hygiene, spent_tobacco_pan, spent_transport, and other_expenditures when available.",
  16, "Dietary diversity", "descriptive_dietary_diversity_reviewed.csv", "descriptive_dietary_diversity_reviewed.png", "complete", "Recalculates HDDS from past-24-hour food group variables; baseline may be structurally missing.",
  17, "Harassment", "descriptive_harassment_household_category_reviewed.csv; descriptive_harassment_fuel_person_reviewed.csv; descriptive_harassment_reviewed.csv", "descriptive_harassment_household_category_reviewed.png; descriptive_harassment_reviewed.png", "complete_descriptive_only", "Household-level category summaries are shown by arm/timepoint. Detailed fuel/person harassment is midline-only in clean_final and uses nonmissing item responses as the denominator; rDiD estimability is audited separately.",
  18, "Physical health outcomes with child outcomes on top and caregiver outcomes on bottom", "descriptive_physical_health_outcomes_reviewed.csv", "descriptive_physical_health_child_caregiver_reviewed.png", "complete", "Figure facet order is the 9 requested child outcomes followed by 9 requested caregiver outcomes."
)
write_reviewed_csv(
  coverage,
  "descriptive_requested_output_coverage_reviewed.csv",
  subfolder = "qa"
)

methods_notes <- c(
  "# RF105 Reviewed Descriptive Outputs",
  "",
  paste0("Generated on ", Sys.Date(), " by 5_analysis_RF105/reviewed/6_descriptive_outcomes_reviewed.R."),
  "",
  "Input data: 4_data/clean_final/survey_refugee_household.rds.",
  "Population: one deduplicated household record per fcn_id-timepoint using make_analysis_population().",
  "",
  "Important notes:",
  "- These are descriptive summaries only; no DiD/rDiD/statistical modeling is performed in this script.",
  "- FCS and HDDS are recalculated using the original food security script as the reference.",
  "- Time-use outcomes are categorical more/same/less changes, not measured minutes.",
  "- No exact clean_final variable named time_caring_for_self was found. The script summarizes time_eating as a proxy and flags this in the coverage QA table.",
  "- Harassment household summaries use all nonmissing household item responses. Detailed fuel/person harassment variables are midline-only in clean_final; the reviewed denominator is nonmissing item responses, not the archived external collector-denominator CSV used by the draft script."
)
readr::write_lines(methods_notes, file.path(dir_tables_qa, "descriptive_methods_notes_reviewed.md"))
message("Wrote QA notes: ", file.path(dir_tables_qa, "descriptive_methods_notes_reviewed.md"))

message("RF105 reviewed descriptive analyses complete.")
