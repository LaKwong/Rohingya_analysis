################################################################################
# RF105 reviewed supplemental legacy survey outcomes
#
# Purpose:
#   Add aggregate descriptive outputs for outcome families that appeared in the
#   older Rohingya household and individual survey analysis folders but were not
#   already represented in the reviewed RF105 scripts.
#
# Inputs:
#   4_data/clean_final/survey_refugee_household.rds
#   4_data/clean_final/survey_refugee_hh_members.rds
#   4_data/clean_final/survey_refugee_symptoms.rds
#
# Outputs:
#   Tables:  7_tables/RF105_reviewed_YYYYMMDD/additional_legacy_*_reviewed.csv
#   Figures: 6_figures/RF105_reviewed_YYYYMMDD/additional_legacy_*_reviewed.png
#   QA:      7_tables/RF105_reviewed_YYYYMMDD/qa/additional_legacy_*_reviewed.csv
#
# Notes:
#   - This file is descriptive only. Old DiD-style outcomes that are comparable
#     over time are estimated in 4_rdid_xgboost_reviewed.R using rDiD/XGBoost.
#   - Exports aggregate summaries only; no fcn_id-level table is written here.
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

num_col <- function(df, var) {
  if (var %in% names(df)) as_number(df[[var]]) else rep(NA_real_, nrow(df))
}

clean_numeric <- function(x, nonnegative = FALSE) {
  out <- as_number(x)
  out[out %in% c(77, 88, 99)] <- NA_real_
  if (nonnegative) out[out < 0] <- NA_real_
  out
}

pretty_label <- function(x) {
  x %>%
    str_replace_all("/", " option ") %>%
    str_replace_all("_", " ") %>%
    str_squish() %>%
    str_to_sentence()
}

make_var_table <- function(vars, source_script, outcome_family, output_file,
                           labels = NULL, notes = "") {
  labels <- labels %||% pretty_label(vars)
  tibble(
    source_variable = vars,
    outcome_name = vars,
    outcome_label = labels,
    source_script = source_script,
    outcome_family = outcome_family,
    reviewed_output = output_file,
    note = notes
  )
}

summarise_binary_vars <- function(df, var_table, population = "households") {
  var_table <- var_table %>% filter(source_variable %in% names(df))
  if (nrow(var_table) == 0) {
    return(tibble())
  }

  map_dfr(seq_len(nrow(var_table)), function(i) {
    var <- var_table$source_variable[[i]]
    df %>%
      transmute(timepoint, study_arm_overall, value = make_yn(.data[[var]])) %>%
      group_by(timepoint, study_arm_overall) %>%
      summarise(
        n_total = n(),
        n_nonmissing = sum(!is.na(value)),
        n_yes = sum(value == 1, na.rm = TRUE),
        percent = if_else(n_nonmissing > 0, 100 * n_yes / n_nonmissing, NA_real_),
        se = if_else(n_nonmissing > 0,
                     sqrt((percent / 100) * (1 - percent / 100) / n_nonmissing),
                     NA_real_),
        ci_lower = pmax(0, 100 * (percent / 100 - qnorm(0.975) * se)),
        ci_upper = pmin(100, 100 * (percent / 100 + qnorm(0.975) * se)),
        .groups = "drop"
      ) %>%
      mutate(
        outcome_group = var_table$outcome_family[[i]],
        outcome_name = var_table$outcome_name[[i]],
        outcome_label = var_table$outcome_label[[i]],
        source_variable = var,
        source_script = var_table$source_script[[i]],
        unit = "percent",
        population = population
      )
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, source_variable, source_script, unit, population,
           n_total, n_nonmissing, n_yes, percent, ci_lower, ci_upper) %>%
    arrange(outcome_group, outcome_name, timepoint, study_arm_overall)
}

summarise_continuous_vars <- function(df, var_table, population = "households") {
  var_table <- var_table %>% filter(source_variable %in% names(df))
  if (nrow(var_table) == 0) {
    return(tibble())
  }

  map_dfr(seq_len(nrow(var_table)), function(i) {
    var <- var_table$source_variable[[i]]
    df %>%
      transmute(timepoint, study_arm_overall,
                value = clean_numeric(.data[[var]], nonnegative = TRUE)) %>%
      group_by(timepoint, study_arm_overall) %>%
      summarise(
        n_total = n(),
        n_nonmissing = sum(!is.na(value)),
        mean = mean(value, na.rm = TRUE),
        sd = sd(value, na.rm = TRUE),
        median = median(value, na.rm = TRUE),
        p25 = quantile(value, 0.25, na.rm = TRUE, names = FALSE),
        p75 = quantile(value, 0.75, na.rm = TRUE, names = FALSE),
        min = min(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE),
        se = sd / sqrt(n_nonmissing),
        ci_lower = mean - qnorm(0.975) * se,
        ci_upper = mean + qnorm(0.975) * se,
        .groups = "drop"
      ) %>%
      mutate(
        across(c(mean, sd, median, p25, p75, min, max, ci_lower, ci_upper),
               ~ ifelse(is.infinite(.x) | is.nan(.x), NA_real_, .x)),
        outcome_group = var_table$outcome_family[[i]],
        outcome_name = var_table$outcome_name[[i]],
        outcome_label = var_table$outcome_label[[i]],
        source_variable = var,
        source_script = var_table$source_script[[i]],
        unit = var_table$unit[[i]],
        population = population
      )
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, source_variable, source_script, unit, population,
           n_total, n_nonmissing, mean, sd, median, p25, p75, min, max,
           ci_lower, ci_upper) %>%
    arrange(outcome_group, outcome_name, timepoint, study_arm_overall)
}

summarise_categorical_vars <- function(df, var_table, population = "households") {
  var_table <- var_table %>% filter(source_variable %in% names(df))
  if (nrow(var_table) == 0) {
    return(tibble())
  }

  map_dfr(seq_len(nrow(var_table)), function(i) {
    var <- var_table$source_variable[[i]]
    df %>%
      transmute(
        timepoint,
        study_arm_overall,
        value = str_squish(as.character(.data[[var]])),
        value = na_if(value, ""),
        value = na_if(value, "NA")
      ) %>%
      group_by(timepoint, study_arm_overall) %>%
      mutate(n_total = n(), n_nonmissing = sum(!is.na(value))) %>%
      ungroup() %>%
      filter(!is.na(value)) %>%
      count(timepoint, study_arm_overall, value, n_total, n_nonmissing,
            name = "n_category") %>%
      mutate(
        percent = if_else(n_nonmissing > 0, 100 * n_category / n_nonmissing, NA_real_),
        outcome_group = var_table$outcome_family[[i]],
        outcome_name = var_table$outcome_name[[i]],
        outcome_label = var_table$outcome_label[[i]],
        source_variable = var,
        source_script = var_table$source_script[[i]],
        category_value = value,
        unit = "percent",
        population = population
      )
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, source_variable, source_script, category_value,
           unit, population, n_total, n_nonmissing, n_category, percent) %>%
    arrange(outcome_group, outcome_name, timepoint, study_arm_overall,
            category_value)
}

write_plot_if_data <- function(data, plot, filename, width, height) {
  if (nrow(data) == 0) {
    message("No rows available for figure: ", filename)
    return(invisible(NULL))
  }
  save_reviewed_plot(plot, filename, width = width, height = height)
}

arm_colors <- c(comparison = "#3B6EA8", intervention = "#C94C4C")

survey_raw <- readr::read_rds(file_survey_refugee_household) %>%
  add_rf105_aliases()

analysis_population <- make_analysis_population(survey_raw, id_var = "fcn_id")
survey <- analysis_population$all_deduplicated %>%
  mutate(
    child_sex_label = case_when(
      str_to_lower(as.character(target_child_sex)) %in% c("0", "male", "boy") ~ "male",
      str_to_lower(as.character(target_child_sex)) %in% c("1", "female", "girl") ~ "female",
      TRUE ~ NA_character_
    ),
    target_child_mid_arm_circ_av_clean = clean_numeric(target_child_mid_arm_circ_av, TRUE),
    target_child_muac_lt145 = case_when(
      is.na(target_child_mid_arm_circ_av_clean) ~ NA_integer_,
      target_child_mid_arm_circ_av_clean < 145 ~ 1L,
      TRUE ~ 0L
    ),
    target_child_muac_below_fiorentino = case_when(
      is.na(target_child_mid_arm_circ_av_clean) | is.na(target_child_months) |
        is.na(child_sex_label) ~ NA_integer_,
      child_sex_label == "male" & target_child_months < 24 &
        target_child_mid_arm_circ_av_clean < 139 ~ 1L,
      child_sex_label == "male" & target_child_months >= 24 &
        target_child_mid_arm_circ_av_clean < 144 ~ 1L,
      child_sex_label == "female" & target_child_months < 24 &
        target_child_mid_arm_circ_av_clean < 136 ~ 1L,
      child_sex_label == "female" & target_child_months >= 24 &
        target_child_mid_arm_circ_av_clean < 142 ~ 1L,
      TRUE ~ 0L
    ),
    target_child_muac_who_mam_or_sam = case_when(
      is.na(target_child_mid_arm_circ_av_clean) ~ NA_integer_,
      target_child_mid_arm_circ_av_clean < 125 ~ 1L,
      TRUE ~ 0L
    ),
    target_child_muac_who_sam = case_when(
      is.na(target_child_mid_arm_circ_av_clean) ~ NA_integer_,
      target_child_mid_arm_circ_av_clean < 115 ~ 1L,
      TRUE ~ 0L
    ),
    window_number_zero = case_when(
      is.na(num_col(., "window_number")) ~ NA_integer_,
      num_col(., "window_number") == 0 ~ 1L,
      TRUE ~ 0L
    ),
    plastic_burn_any_yn = case_when(
      is.na(num_col(., "burn_plastic_frequency")) ~ NA_integer_,
      num_col(., "burn_plastic_frequency") > 0 ~ 1L,
      TRUE ~ 0L
    ),
    plastic_burn_gt1_yn = case_when(
      is.na(num_col(., "burn_plastic_frequency")) ~ NA_integer_,
      num_col(., "burn_plastic_frequency") > 1 ~ 1L,
      TRUE ~ 0L
    ),
    spent_food_pct = case_when(
      is.na(num_col(., "spent_food")) | is.na(num_col(., "spent_total_month")) ~ NA_real_,
      num_col(., "spent_food") < 0 | num_col(., "spent_total_month") <= 0 ~ NA_real_,
      num_col(., "spent_food") > num_col(., "spent_total_month") ~ NA_real_,
      TRUE ~ num_col(., "spent_food") / num_col(., "spent_total_month")
    ),
    total_income_30_usd = num_col(., "total_income_30") /
      exchange_bdt_per_usd[as.character(timepoint)],
    spent_total_month_usd = num_col(., "spent_total_month") /
      exchange_bdt_per_usd[as.character(timepoint)],
    debt_total_usd = num_col(., "debt_total") /
      exchange_bdt_per_usd[as.character(timepoint)]
  )

################################################################################
# Household outcome dictionaries and outputs
################################################################################

sleep_binary <- make_var_table(
  c("sleep_bad_dreams"),
  "11.1_sleep_questions.R", "sleep", "additional_legacy_household_binary_outcomes_reviewed.csv",
  labels = c("Bad dreams")
)

muac_binary <- make_var_table(
  c("target_child_arm_measurements_yn", "target_child_muac_lt145",
    "target_child_muac_below_fiorentino", "target_child_muac_who_mam_or_sam",
    "target_child_muac_who_sam"),
  "12_Child_MUAC_analysis.R", "child_muac", "additional_legacy_household_binary_outcomes_reviewed.csv",
  labels = c("Child MUAC measurements available", "Average MUAC <145 mm",
             "MUAC below Fiorentino sex/age cutoff", "MUAC <125 mm", "MUAC <115 mm")
)

ventilation_binary <- make_var_table(
  c("window_number_zero", "window_kitchen", "window_kitchen_use"),
  "8_temp_ventilation_radiation.R", "ventilation", "additional_legacy_household_binary_outcomes_reviewed.csv",
  labels = c("No windows reported", "Kitchen has a window", "Kitchen window is used")
)

who_cooks <- make_var_table(
  c("cook_who_w", "cook_who_g", "cook_who_m", "cook_who_b"),
  "4.3_who_cooks.R", "who_cooks", "additional_legacy_household_binary_outcomes_reviewed.csv",
  labels = c("Women cook", "Girls cook", "Men cook", "Boys cook")
)

stove_use <- make_var_table(
  c("stove_boil_drink", "stove_boil_bathe", "stove_reason_stay_warm",
    "stove_reason_cook_together", "stove_reason_sell_food"),
  "3.2.2_stove_uses.R", "stove_use", "additional_legacy_household_binary_outcomes_reviewed.csv",
  labels = c("Boils drinking water", "Boils bathing water", "Uses stove to stay warm",
             "Uses stove to cook for/with others", "Uses stove to cook food to sell")
)

forest_use <- make_var_table(
  c("gather_wood", "forest_collect_not_wood", "reason_forest_food",
    "reason_forest_med", "reason_forest_shelter", "reason_forest_privacy",
    "reason_forest_defacation", "reason_forest_leisure", "reason_forest_other"),
  "3.4_forest_use.R", "forest_use", "additional_legacy_household_binary_outcomes_reviewed.csv",
  labels = c("Collected firewood for non-household cooking", "Collected non-wood forest products",
             "Forest reason: food", "Forest reason: medicine", "Forest reason: shelter material",
             "Forest reason: privacy", "Forest reason: defecation", "Forest reason: leisure",
             "Forest reason: other")
)

plastic_use <- make_var_table(
  c("plastic_burn_any_yn", "plastic_burn_gt1_yn", "burn_plastic_types/1",
    "burn_plastic_types/2", "burn_plastic_types/3", "burn_plastic_types/66",
    "burn_plastic_reason/1", "burn_plastic_reason/2", "burn_plastic_reason/3"),
  "3.3_plastic_burning_new.R", "plastic_burning", "additional_legacy_household_binary_outcomes_reviewed.csv",
  labels = c("Burned plastic at least once per week", "Burned plastic more than once per week",
             "Burned plastic type 1", "Burned plastic type 2", "Burned plastic type 3",
             "Burned other plastic type", "Burned plastic because other fuel unavailable",
             "Burned plastic to avoid fuel costs", "Burned plastic to dispose of trash")
)

programmatic <- make_var_table(
  c("lpg_prior_use", "stove_training", "safety_stove_training",
    "training_type_video", "training_type_demonstration", "training_type_handout",
    "training_type_other", "lpg_no_training_learn", "lpg_safety_visit",
    "lpg_afraid", "lpg_gas_leak", "lpg_child_burn",
    "receive_lpg_women_rq", "receive_lpg_require_opinion", "receive_lpg_carry",
    "credit_access", "credit_relatives", "credit_charities", "credit_village_head",
    "credit_lender", "credit_bank", "credit_cooperative", "credit_borrow_money_food", "debt"),
  "10_programmatic evaluation.R; 5_hh_expenditures_debt.R",
  "programmatic_credit_debt", "additional_legacy_household_binary_outcomes_reviewed.csv"
)

repair <- make_var_table(
  c(paste0("lpg_stove_repair/", c(0:5, 66)),
    paste0("lpg_cylinder_repair/", c(0:6, 66)),
    paste0("lpg_repair_details/", 1:5)),
  "10_programmatic evaluation.R", "lpg_repairs", "additional_legacy_household_binary_outcomes_reviewed.csv"
)

fuel_person_vars <- crossing(
  fuel = c("gather_scraps", "collect_wood", "buy_wood", "receive_wood",
           "receive_lpg", "buy_lpg", "receive_crh", "buy_crh"),
  person = c("w", "g", "m", "b")
) %>%
  mutate(source_variable = paste(fuel, person, sep = "_")) %>%
  pull(source_variable)

fuel_person <- make_var_table(
  fuel_person_vars,
  source_script = "3.1_fuel_procurement.R",
  outcome_family = "fuel_procurement_who",
  output_file = "additional_legacy_fuel_procurement_who_reviewed.csv"
)

household_binary_vars <- bind_rows(
  sleep_binary, muac_binary, ventilation_binary, who_cooks, stove_use,
  forest_use, plastic_use, programmatic, repair, fuel_person
)

household_continuous_vars <- tribble(
  ~source_variable, ~unit, ~source_script,
  "sleep_hours", "hours", "11.1_sleep_questions.R",
  "sleep_fall_asleep_min", "minutes", "11.1_sleep_questions.R",
  "target_child_mid_arm_circ_av_clean", "mm", "12_Child_MUAC_analysis.R",
  "window_number", "count", "8_temp_ventilation_radiation.R",
  "traditional_use_yesterday", "uses", "3.2.2_stove_uses.R",
  "LPG_use_yesterday", "uses", "3.2.2_stove_uses.R",
  "boil_yesterday_times", "times", "3.2.2_stove_uses.R",
  "cook_sell_days_week", "days per week", "3.2.2_stove_uses.R",
  "cook_to_sell_percent", "percent category", "3.2.2_stove_uses.R",
  "forest_wood_fee", "BDT", "3.1_fuel_procurement.R; 3.4_forest_use.R",
  "cost_forest_not_wood", "BDT", "3.4_forest_use.R",
  "buy_wood_cost_bundle", "BDT", "3.1_fuel_procurement.R",
  "burn_plastic_frequency", "times per week", "3.3_plastic_burning_new.R",
  "receive_lpg_walk", "hours", "3.1_fuel_procurement.R",
  "receive_lpg_wait", "hours", "3.1_fuel_procurement.R",
  "buy_lpg_walk", "hours", "3.1_fuel_procurement.R",
  "buy_lpg_wait", "hours", "3.1_fuel_procurement.R",
  "receive_crh_walk", "hours", "3.1_fuel_procurement.R",
  "receive_crh_wait", "hours", "3.1_fuel_procurement.R",
  "receive_wood_wait", "hours", "3.1_fuel_procurement.R",
  "lpg_repair_costs", "BDT", "10_programmatic evaluation.R",
  "lpg_willingness_to_pay", "BDT", "10_programmatic evaluation.R",
  "total_income_30_usd", "USD", "0_data_analysis_Rohingya.Rmd; 5.1_income_expenditures.R",
  "spent_total_month_usd", "USD", "0_data_analysis_Rohingya.Rmd; 5_hh_expenditures_debt.R",
  "spent_food_pct", "proportion", "5.1_income_expenditures.R",
  "debt_total_usd", "USD", "5_hh_expenditures_debt.R"
) %>%
  mutate(
    outcome_name = source_variable,
    outcome_label = pretty_label(source_variable),
    outcome_family = "additional_legacy_household_continuous",
    reviewed_output = "additional_legacy_household_continuous_outcomes_reviewed.csv",
    note = ""
  )

household_categorical_vars <- make_var_table(
  c("sleep_quality", "sleep_bad_dreams", "window_door_wall", "gather_wood_dead",
    "gather_wood_reason", "buy_wood_reason", "refill_time_lpg_missed",
    "lpg_extra_use", "lpg_afraid_why", "fire_why", "fire_consequence",
    "lpg_repair_details", "reason_forest_other_specified"),
  "legacy household descriptive scripts",
  "additional_legacy_household_categorical",
  "additional_legacy_household_categorical_outcomes_reviewed.csv"
)

household_binary_summary <- summarise_binary_vars(survey, household_binary_vars)
write_reviewed_csv(
  household_binary_summary,
  "additional_legacy_household_binary_outcomes_reviewed.csv"
)

household_continuous_summary <- summarise_continuous_vars(survey, household_continuous_vars)
write_reviewed_csv(
  household_continuous_summary,
  "additional_legacy_household_continuous_outcomes_reviewed.csv"
)

household_categorical_summary <- summarise_categorical_vars(survey, household_categorical_vars)
write_reviewed_csv(
  household_categorical_summary,
  "additional_legacy_household_categorical_outcomes_reviewed.csv"
)

fuel_procurement_who_summary <- household_binary_summary %>%
  filter(outcome_group == "fuel_procurement_who")
write_reviewed_csv(
  fuel_procurement_who_summary,
  "additional_legacy_fuel_procurement_who_reviewed.csv"
)

binary_plot_data <- household_binary_summary %>%
  filter(source_variable %in% c(
    "target_child_arm_measurements_yn", "target_child_muac_below_fiorentino",
    "window_number_zero", "cook_who_w", "cook_who_m", "plastic_burn_any_yn",
    "lpg_afraid", "lpg_gas_leak", "credit_access", "debt"
  ), n_nonmissing > 0, !is.na(percent))

fig_household_binary <- ggplot(
  binary_plot_data,
  aes(x = timepoint, y = percent, fill = study_arm_overall)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.75), width = 0.2) +
  facet_wrap(~ outcome_label, ncol = 2) +
  scale_fill_manual(values = arm_colors, drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = "Timepoint", y = "Percent", fill = "Study arm")
write_plot_if_data(binary_plot_data, fig_household_binary,
                   "additional_legacy_household_binary_outcomes_reviewed.png",
                   width = 10, height = 9)

continuous_plot_data <- household_continuous_summary %>%
  filter(source_variable %in% c(
    "sleep_hours", "target_child_mid_arm_circ_av_clean", "window_number",
    "traditional_use_yesterday", "LPG_use_yesterday", "boil_yesterday_times",
    "forest_wood_fee", "burn_plastic_frequency", "spent_total_month_usd",
    "total_income_30_usd"
  ), n_nonmissing > 0, !is.na(mean))

fig_household_continuous <- ggplot(
  continuous_plot_data,
  aes(x = timepoint, y = mean, color = study_arm_overall,
      group = study_arm_overall)
) +
  geom_hline(yintercept = 0, color = "grey85") +
  geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper),
                  position = position_dodge(width = 0.25)) +
  geom_line(position = position_dodge(width = 0.25), linewidth = 0.6) +
  facet_wrap(~ outcome_label, scales = "free_y", ncol = 2) +
  scale_color_manual(values = arm_colors, drop = FALSE) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = "Timepoint", y = "Mean with 95% CI", color = "Study arm")
write_plot_if_data(continuous_plot_data, fig_household_continuous,
                   "additional_legacy_household_continuous_outcomes_reviewed.png",
                   width = 10, height = 9)

################################################################################
# Individual/member descriptive outputs
################################################################################

file_survey_refugee_hh_members <- file.path(
  dir_clean_final, "survey_refugee_hh_members.rds"
)
file_survey_refugee_symptoms <- file.path(
  dir_clean_final, "survey_refugee_symptoms.rds"
)

members <- readr::read_rds(file_survey_refugee_hh_members) %>%
  clean_timepoint_arm() %>%
  mutate(
    age_yrs = num_col(., "age_yrs"),
    hours_outside = num_col(., "hours_outside"),
    sex_label = case_when(
      str_to_lower(as.character(sex)) %in% c("0", "male", "m") ~ "Male",
      str_to_lower(as.character(sex)) %in% c("1", "female", "f") ~ "Female",
      TRUE ~ "Missing/unknown"
    ),
    age_group = case_when(
      is.na(age_yrs) ~ "Missing age",
      age_yrs < 18 ~ "Child (<18 years)",
      TRUE ~ "Adult (18+ years)"
    )
  )

member_timepoint_availability <- members %>%
  count(timepoint, study_arm_overall, name = "n_member_rows") %>%
  arrange(timepoint, study_arm_overall)
write_reviewed_csv(
  member_timepoint_availability,
  "additional_legacy_member_timepoint_availability_reviewed.csv",
  subfolder = "qa"
)

member_demographics_summary <- members %>%
  group_by(timepoint, study_arm_overall, age_group, sex_label) %>%
  summarise(
    n_members = n(),
    mean_age_yrs = mean(age_yrs, na.rm = TRUE),
    sd_age_yrs = sd(age_yrs, na.rm = TRUE),
    mean_hours_outside = mean(hours_outside, na.rm = TRUE),
    sd_hours_outside = sd(hours_outside, na.rm = TRUE),
    n_nonmissing_hours_outside = sum(!is.na(hours_outside)),
    .groups = "drop"
  ) %>%
  mutate(across(c(mean_age_yrs, sd_age_yrs, mean_hours_outside, sd_hours_outside),
                ~ ifelse(is.nan(.x), NA_real_, .x))) %>%
  arrange(timepoint, study_arm_overall, age_group, sex_label)
write_reviewed_csv(
  member_demographics_summary,
  "additional_legacy_member_demographics_hours_outside_reviewed.csv"
)

location_labels <- tribble(
  ~option_code, ~outcome_label,
  "1", "Mosque",
  "2", "School / temporary learning center",
  "3", "Madrassa",
  "4", "Women's center / safe space",
  "5", "Healthcare facility",
  "6", "Restaurant",
  "7", "Another person's home",
  "8", "Other indoor location",
  "9", "Market",
  "10", "Tea stall",
  "11", "Play outside",
  "12", "Toilet",
  "13", "Collect water / wash / dishes",
  "14", "Collect food/fuel",
  "15", "Islamic activities",
  "16", "Shop",
  "17", "NGO volunteer",
  "18", "Office",
  "19", "Daily labor outside",
  "66", "Other",
  "88", "Other"
) %>%
  mutate(source_variable = paste0("location_outside_house_1/", option_code)) %>%
  distinct(source_variable, .keep_all = TRUE)

location_summary <- location_labels %>%
  filter(source_variable %in% names(members)) %>%
  pmap_dfr(function(option_code, outcome_label, source_variable) {
    members %>%
      transmute(timepoint, study_arm_overall, age_group, sex_label,
                value = make_yn(.data[[source_variable]])) %>%
      group_by(timepoint, study_arm_overall, age_group, sex_label) %>%
      summarise(
        n_members = n(),
        n_nonmissing = sum(!is.na(value)),
        n_yes = sum(value == 1, na.rm = TRUE),
        percent = if_else(n_nonmissing > 0, 100 * n_yes / n_nonmissing, NA_real_),
        .groups = "drop"
      ) %>%
      mutate(
        outcome_group = "individual_locations_visited",
        outcome_name = paste0("location_outside_house_", option_code),
        outcome_label = outcome_label,
        source_variable = source_variable,
        source_script = "2_Rohingya_individual_survey_analysis/1_data_analysis_individual_Rohingya.Rmd"
      )
  }) %>%
  arrange(outcome_name, timepoint, study_arm_overall, age_group, sex_label)
write_reviewed_csv(
  location_summary,
  "additional_legacy_member_locations_visited_reviewed.csv"
)

location_plot_data <- location_summary %>%
  filter(
    outcome_label %in% c("School / temporary learning center", "Market", "Collect food/fuel",
                         "Healthcare facility", "Daily labor outside", "Play outside"),
    age_group != "Missing age",
    sex_label != "Missing/unknown",
    n_nonmissing > 0
  )
fig_member_locations <- ggplot(
  location_plot_data,
  aes(x = timepoint, y = percent, fill = study_arm_overall)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  facet_grid(outcome_label ~ age_group + sex_label) +
  scale_fill_manual(values = arm_colors, drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = "Timepoint", y = "Percent of members", fill = "Study arm")
write_plot_if_data(location_plot_data, fig_member_locations,
                   "additional_legacy_member_locations_visited_reviewed.png",
                   width = 14, height = 10)

symptoms <- readr::read_rds(file_survey_refugee_symptoms) %>%
  clean_timepoint_arm() %>%
  mutate(
    age_group = "Age not linked in clean_final symptoms file",
    sex_label = "Sex not linked in clean_final symptoms file"
  )

symptom_labels <- tribble(
  ~option_code, ~outcome_label,
  "1", "Fever",
  "2", "Dry cough",
  "3", "Wet cough or sputum",
  "4", "Shortness of breath",
  "5", "Sore throat",
  "6", "Headache",
  "7", "Diarrhea",
  "8", "Fatigue or malaise",
  "9", "Body aches",
  "10", "Runny nose or nasal congestion",
  "11", "Loss of taste or smell",
  "12", "Itchy/red eyes",
  "13", "Rash",
  "14", "Unexplained bruising",
  "15", "None of the above"
) %>%
  mutate(source_variable = paste0("symptoms_7/", option_code))

symptom_summary <- symptom_labels %>%
  filter(source_variable %in% names(symptoms)) %>%
  pmap_dfr(function(option_code, outcome_label, source_variable) {
    symptoms %>%
      transmute(timepoint, study_arm_overall, age_group, sex_label,
                value = make_yn(.data[[source_variable]])) %>%
      group_by(timepoint, study_arm_overall, age_group, sex_label) %>%
      summarise(
        n_members = n(),
        n_nonmissing = sum(!is.na(value)),
        n_yes = sum(value == 1, na.rm = TRUE),
        percent = if_else(n_nonmissing > 0, 100 * n_yes / n_nonmissing, NA_real_),
        .groups = "drop"
      ) %>%
      mutate(
        outcome_group = "individual_covid_like_symptoms",
        outcome_name = paste0("symptoms_7_", option_code),
        outcome_label = outcome_label,
        source_variable = source_variable,
        source_script = "2_Rohingya_individual_survey_analysis/1_data_analysis_individual_Rohingya.Rmd"
      )
  }) %>%
  arrange(outcome_name, timepoint, study_arm_overall, age_group, sex_label)
write_reviewed_csv(
  symptom_summary,
  "additional_legacy_member_covid_like_symptoms_reviewed.csv"
)

symptom_plot_data <- symptom_summary %>%
  filter(
    outcome_label %in% c("Fever", "Dry cough", "Wet cough or sputum",
                         "Shortness of breath", "Headache", "Itchy/red eyes"),
    age_group != "Missing age",
    sex_label != "Missing/unknown",
    n_nonmissing > 0
  )
fig_member_symptoms <- ggplot(
  symptom_plot_data,
  aes(x = timepoint, y = percent, fill = study_arm_overall)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  facet_grid(outcome_label ~ age_group + sex_label) +
  scale_fill_manual(values = arm_colors, drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  labs(x = "Timepoint", y = "Percent of members", fill = "Study arm")
write_plot_if_data(symptom_plot_data, fig_member_symptoms,
                   "additional_legacy_member_covid_like_symptoms_reviewed.png",
                   width = 14, height = 10)

################################################################################
# QA and source-outcome coverage
################################################################################

household_coverage <- bind_rows(household_binary_vars, household_continuous_vars,
                                household_categorical_vars) %>%
  transmute(
    source_script,
    outcome_family,
    source_variable,
    reviewed_output,
    status = if_else(source_variable %in% names(survey),
                     "summarized", "not_available_in_clean_final"),
    note = case_when(
      source_variable == "reason_forest_defacation" ~
        "Uses clean_final spelling; old scripts sometimes used reason_forest_defecation.",
      source_variable == "target_child_muac_below_fiorentino" ~
        "Derived using sex- and age-specific Fiorentino cutoffs described in the old MUAC comments.",
      TRUE ~ note
    )
  )

known_unavailable_or_represented <- tribble(
  ~source_script, ~outcome_family, ~source_variable, ~reviewed_output, ~status, ~note,
  "3.4_forest_use.R", "household_binary", "reason_forest_defecation", "additional_legacy_household_binary_outcomes_reviewed.csv", "represented_by_clean_final_spelling", "clean_final uses reason_forest_defacation.",
  "0_data_analysis_Rohingya.Rmd", "rdid", "income", "4_rdid_xgboost_reviewed.R", "included_as_total_income_30_usd", "Baseline total_income_30 is calculated from income components in add_rf105_aliases(); descriptive summaries are included here and rDiD is estimated in 4_rdid_xgboost_reviewed.R.",
  "0_data_analysis_Rohingya.Rmd", "rdid", "spent_total_month_with_6mo_monthly", "4_rdid_xgboost_reviewed.R", "represented_by_spent_total_month", "The exact old variable is absent from clean_final; rDiD uses spent_total_month."
)

member_coverage <- bind_rows(
  location_labels %>%
    transmute(source_script = "2_Rohingya_individual_survey_analysis/1_data_analysis_individual_Rohingya.Rmd",
              outcome_family = "individual_locations", source_variable,
              reviewed_output = "additional_legacy_member_locations_visited_reviewed.csv",
              status = if_else(source_variable %in% names(members), "summarized", "not_available_in_clean_final"),
              note = ""),
  symptom_labels %>%
    transmute(source_script = "2_Rohingya_individual_survey_analysis/1_data_analysis_individual_Rohingya.Rmd",
              outcome_family = "individual_covid_like_symptoms", source_variable,
              reviewed_output = "additional_legacy_member_covid_like_symptoms_reviewed.csv",
              status = if_else(source_variable %in% names(symptoms), "summarized", "not_available_in_clean_final"),
              note = "")
)

additional_legacy_coverage <- bind_rows(
  household_coverage,
  known_unavailable_or_represented,
  member_coverage
) %>%
  arrange(source_script, outcome_family, source_variable)
write_reviewed_csv(
  additional_legacy_coverage,
  "additional_legacy_outcome_coverage_reviewed.csv",
  subfolder = "qa"
)

source_script_audit <- tribble(
  ~source_script, ~reviewed_addition, ~notes,
  "0_data_analysis_Rohingya.Rmd", "Additional rDiD outcomes added to 4_rdid_xgboost_reviewed.R, including total_income_30_usd.", "Baseline total_income_30 is calculated from the specified income component variables when the aggregate field is missing.",
  "3.1_fuel_procurement.R", "Fuel-procurement person, walk/wait, forest fee, and bundle-cost summaries.", "The old output used household counts by demographic; reviewed output keeps one row per household-timepoint and aggregate denominators.",
  "3.2.2_stove_uses.R", "Stove purpose/use descriptive outcomes and fuel-use rDiD additions.", "cook_sell_days_week is aliased from cook_sell_yesterday; cook_to_sell_percent is aliased from cook_to_sell.",
  "3.3_plastic_burning_new.R", "Plastic-burning frequency and reasons/types; plastic_burn_gt1_yn added to rDiD.", "The old script filtered one LPG-runout variable without clear justification; reviewed descriptive outputs do not apply that filter.",
  "3.4_forest_use.R", "Forest-use descriptive outcomes.", "reason_forest_defacation spelling follows clean_final; gather_wood_dead is aliased from gather_scraps_dead.",
  "4.3_who_cooks.R", "Who-cooks descriptive outcomes.", "Outputs are by arm and timepoint instead of a TableOne object only.",
  "5.1_income_expenditures.R", "Economic descriptive outcomes plus spent_food_pct and total_income_30_usd rDiD additions.", "Reviewed spent_food_pct corrects the old logical if_else expression; baseline total_income_30 is calculated from component income variables.",
  "5_hh_expenditures_debt.R", "Credit/debt, income/expenditure, repair, and willingness-to-pay descriptive outputs; additional economic rDiD outcomes.", "Outputs use clean_final and reviewed output folders.",
  "8_temp_ventilation_radiation.R", "Window and ventilation descriptive outcomes.", "No temperature or radiation variables with clear names were found in survey_refugee_household.rds.",
  "10_programmatic evaluation.R", "LPG training, safety, refill, repair, and willingness-to-pay summaries.", "Date-difference calculations from the exploratory script are not repeated because date formats/denominators need a separate programmatic QA pass.",
  "11.1_sleep_questions.R", "Sleep questionnaire descriptive outcomes.", "The original script summarized sleep_hours, sleep_bad_dreams, and sleep_quality.",
  "12_Child_MUAC_analysis.R", "Child MUAC measurement availability and derived MUAC flags.", "Includes Fiorentino cutoffs from the old comments plus WHO <125 and <115 mm flags.",
  "2_Rohingya_individual_survey_analysis/1_data_analysis_individual_Rohingya.Rmd", "Member demographics, hours outside, locations visited, and COVID-like symptoms.", "Member/symptom denominators are individual rows, not household rows. Symptom records cannot be validly linked to member age/sex from clean_final, so symptom outputs are arm/timepoint summaries only."
)
write_reviewed_csv(
  source_script_audit,
  "additional_legacy_source_script_audit_reviewed.csv",
  subfolder = "qa"
)

methods_notes <- c(
  "# Additional Legacy Survey Outcomes",
  "",
  paste0("Generated on ", Sys.Date(), " by 5_analysis_RF105/reviewed/9_additional_legacy_survey_outcomes_reviewed.R."),
  "",
  "This script adds aggregate descriptive outputs for older household and individual survey outcome families not already present in the main reviewed RF105 scripts.",
  "Old simple DiD/model outcomes that are comparable over time were added to 4_rdid_xgboost_reviewed.R rather than modeled here.",
  "No fcn_id-level table is exported by this script.",
  "",
  "Important limitations:",
  "- Baseline total_income_30 is calculated as the sum of income_cash_ngo, income_own_business, income_wage_labor, income_skill_labor, income_selling_wood, income_abroad, income_humanitarian_asst, income_handicrafts_tailoring, and income_farming when the aggregate field is missing.",
  "- cook_sell_days_week is aliased from cook_sell_yesterday, cook_to_sell_percent is aliased from cook_to_sell, and gather_wood_dead is aliased from gather_scraps_dead. reason_forest_defecation is represented by the clean_final spelling reason_forest_defacation.",
  "- Member/location/symptom outputs use individual-level denominators and should not be compared directly with household percentages.",
  "- Symptom records cannot be validly linked to member age/sex from clean_final, so symptom summaries are by study arm and timepoint only."
)
readr::write_lines(
  methods_notes,
  file.path(dir_tables_qa, "additional_legacy_methods_notes_reviewed.md")
)
message("Wrote QA notes: ", file.path(dir_tables_qa, "additional_legacy_methods_notes_reviewed.md"))


