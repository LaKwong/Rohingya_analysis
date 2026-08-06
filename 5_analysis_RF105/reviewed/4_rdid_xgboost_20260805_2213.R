################################################################################
# RF105 reviewed reverse difference-in-differences (rDiD) with XGBoost
#
# Purpose:
#   Run the checked RF105B rDiD analyses using the final cleaned data. This file
#   is based on the earlier current_rdid_xgboost_code workflow, but it
#   removes exploratory code, stale input paths, duplicate functions/outcomes,
#   and superseded interaction-model analysis blocks.
#
# Inputs:
#   4_data/clean_final/survey_refugee_household.rds
#   4_data/clean_final/survey_refugee_hh_members.rds
#   4_data/clean_final/pm25_pats_refugee_indoor.rds
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/rdid_*.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/rdid_*.csv
#   6_figures/RF105_reviewed_YYYYMMDD/rdid_*.png
#
# Primary analysis:
#   Baseline-to-midline rDiD/XGBoost among households with baseline and midline
#   outcome data, regardless of endline participation.
#
# Secondary analysis:
#   Baseline-to-endline rDiD/XGBoost among households with baseline and endline
#   outcome data, regardless of midline participation.
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
config_file <- file.path(script_dir, "0_RF105_config_20260805_2213.R")
if (!file.exists(config_file)) {
  config_file <- file.path("5_analysis_RF105", "reviewed", "0_RF105_config_20260805_2213.R")
}
source(config_file)

################################################################################
# rDiD-specific package setup
################################################################################

ensure_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    message("Installing missing package: ", package)
    install.packages(package, repos = "https://cloud.r-project.org")
  }

  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      "Package ", package, " is required for RF105 rDiD/XGBoost analyses. ",
      "Install it and rerun this script."
    )
  }
}

ensure_package("xgboost")

suppressPackageStartupMessages({
  library(xgboost)
})

safe_write_reviewed_csv <- function(x, filename, subfolder = NULL) {
  tryCatch(
    write_reviewed_csv(x, filename, subfolder = subfolder),
    error = function(e) {
      fallback_filename <- str_replace(filename, "\\.csv$", paste0("_refreshed_", date_stamp, ".csv"))
      warning(
        "Could not overwrite ", filename, "; writing refreshed copy ",
        fallback_filename, ". Original error: ", conditionMessage(e),
        call. = FALSE
      )
      write_reviewed_csv(x, fallback_filename, subfolder = subfolder)
    }
  )
}

rdid_package_versions <- tibble(
  package = c("R", "xgboost", "tidyverse", "dplyr", "ggplot2"),
  version = c(
    as.character(getRversion()),
    as.character(utils::packageVersion("xgboost")),
    as.character(utils::packageVersion("tidyverse")),
    as.character(utils::packageVersion("dplyr")),
    as.character(utils::packageVersion("ggplot2"))
  )
)

safe_write_reviewed_csv(
  rdid_package_versions,
  "table_rDiD_package_versions.csv",
  subfolder = "qa"
)

rdid_xgboost_tuning_settings <- tibble(
  parameter = c("max_nrounds", "early_stopping_rounds", "nthread", "max_depth_grid", "eta_grid"),
  value = c(
    Sys.getenv("RF105_XGB_MAX_NROUNDS", unset = "100"),
    Sys.getenv("RF105_XGB_EARLY_STOP", unset = "10"),
    Sys.getenv("RF105_XGB_NTHREAD", unset = "2"),
    Sys.getenv("RF105_XGB_DEPTHS", unset = "2"),
    Sys.getenv("RF105_XGB_ETAS", unset = "0.05")
  )
)

safe_write_reviewed_csv(
  rdid_xgboost_tuning_settings,
  "table_rDiD_xgboost_tuning.csv",
  subfolder = "qa"
)

################################################################################
# Helper functions
################################################################################

first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA)
  }
  x[[1]]
}

as_number <- function(x) {
  suppressWarnings(as.numeric(x))
}

row_sum_vars <- function(df, vars) {
  vars <- vars[vars %in% names(df)]
  if (length(vars) == 0) {
    return(rep(NA_real_, nrow(df)))
  }

  mat <- as.data.frame(lapply(vars, function(var) as_number(df[[var]])))
  nonmissing <- rowSums(!is.na(mat))
  out <- rowSums(mat, na.rm = TRUE)
  out[nonmissing == 0] <- NA_real_
  out
}

single_num_var <- function(df, var) {
  if (var %in% names(df)) {
    as_number(df[[var]])
  } else {
    rep(NA_real_, nrow(df))
  }
}

cap_at_7 <- function(x) {
  ifelse(is.na(x), NA_real_, pmin(x, 7))
}

fcs_weekly_input_vars <- c(
  "rice_adults_week", "bread_adults_week", "corn_adults_week",
  "potatoes_adults_week", "lentils_adults_week", "veggies_adults_week",
  "fruit_adults_week", "eggs_adults_week", "fish_adults_week",
  "poultry_adults_week", "goat_sheep_adults_week", "beef_adults_week",
  "dairy_adults_week", "sugar_adults_week", "oil_adults_week"
)

calculate_fcs_from_weekly_vars <- function(df) {
  df %>%
    mutate(
      # Food Consumption Score (FCS) uses 7-day recall variables.
      # This mapping is implemented directly below for reproducible FCS/HDDS derivation.
      # 6.1_food_security.R and WFP FCS guidance: cap each food group at
      # 7 days, then multiply by the standard food-group weights.
      staple = cap_at_7(row_sum_vars(., c(
        "rice_adults_week", "bread_adults_week", "corn_adults_week",
        "potatoes_adults_week"
      ))),
      pulses = cap_at_7(single_num_var(., "lentils_adults_week")),
      veggies = cap_at_7(single_num_var(., "veggies_adults_week")),
      fruit = cap_at_7(single_num_var(., "fruit_adults_week")),
      meat_fish = cap_at_7(row_sum_vars(., c(
        "eggs_adults_week", "fish_adults_week", "poultry_adults_week",
        "goat_sheep_adults_week", "beef_adults_week"
      ))),
      dairy = cap_at_7(single_num_var(., "dairy_adults_week")),
      sugar = cap_at_7(single_num_var(., "sugar_adults_week")),
      oil = cap_at_7(single_num_var(., "oil_adults_week"))
    ) %>%
    mutate(
      staple_weight = staple * 2,
      pulses_weight = pulses * 3,
      veggies_weight = veggies * 1,
      fruit_weight = fruit * 1,
      meat_fish_weight = meat_fish * 4,
      milk_weight = dairy * 4,
      sugar_weight = sugar * 0.5,
      oil_weight = oil * 0.5,
      fcs = staple_weight + pulses_weight + veggies_weight + fruit_weight +
        meat_fish_weight + milk_weight + sugar_weight + oil_weight,
      fcs_category = case_when(
        is.na(fcs) ~ NA_character_,
        fcs <= 21 ~ "poor",
        fcs >= 21.5 & fcs <= 35 ~ "borderline",
        fcs > 35 ~ "acceptable"
      ),
      fcs_category = ordered(
        fcs_category,
        levels = c("poor", "borderline", "acceptable")
      ),
      fcs_binary = case_when(
        is.na(fcs) ~ NA_integer_,
        fcs <= 35 ~ 1L,
        fcs > 35 ~ 0L
      )
    )
}
make_yn_rdid <- function(x) {
  x_chr <- str_squish(str_to_lower(as.character(x)))
  x_num <- as_number(x_chr)

  case_when(
    is.na(x) ~ NA_integer_,
    !is.na(x_num) & x_num %in% c(77, 88, 99) ~ NA_integer_,
    !is.na(x_num) ~ as.integer(x_num > 0),
    x_chr %in% c("yes", "y", "true", "present") ~ 1L,
    x_chr %in% c("no", "n", "false", "absent") ~ 0L,
    TRUE ~ NA_integer_
  )
}

coalesce_vectors <- function(values) {
  if (length(values) == 0) {
    return(NULL)
  }

  out <- values[[1]]
  if (length(values) == 1) {
    return(out)
  }

  for (i in seq_along(values)[-1]) {
    out <- dplyr::coalesce(out, values[[i]])
  }
  out
}

make_nonnegative_number <- function(x) {
  out <- as_number(x)
  out[out %in% c(77, 88, 99)] <- NA_real_
  out[out < 0] <- NA_real_
  out
}

multi_select_code_yn <- function(df, base_var, option_code,
                                 asked_var = NULL) {
  option_code <- as.character(option_code)
  candidate_vars <- c(
    paste0(base_var, option_code),
    paste0(base_var, "/", option_code),
    paste0(base_var, ".", option_code)
  )
  candidate_vars <- candidate_vars[candidate_vars %in% names(df)]

  value_list <- lapply(candidate_vars, function(var) make_yn_rdid(df[[var]]))

  if (base_var %in% names(df)) {
    raw_chr <- str_squish(str_to_lower(as.character(df[[base_var]])))
    parsed <- ifelse(
      is.na(raw_chr) | raw_chr == "",
      NA_integer_,
      as.integer(str_detect(
        raw_chr,
        paste0("(^|[^0-9])", option_code, "([^0-9]|$)")
      ))
    )
    value_list <- c(value_list, list(parsed))
  }

  if (length(value_list) == 0) {
    value <- rep(NA_integer_, nrow(df))
  } else {
    value <- coalesce_vectors(value_list)
  }

  if (!is.null(asked_var) && asked_var %in% names(df)) {
    asked <- make_yn_rdid(df[[asked_var]])
    value <- case_when(
      asked == 0 ~ 0L,
      asked == 1 ~ value,
      TRUE ~ NA_integer_
    )
  }

  as.integer(value)
}

estimate_significance <- function(estimate, conf_low, conf_high, p_value) {
  case_when(
    !is.na(p_value) ~ p_value < 0.05,
    !is.na(conf_low) & !is.na(conf_high) ~ conf_low > 0 | conf_high < 0,
    TRUE ~ NA
  )
}

format_est_ci <- function(estimate, conf_low, conf_high) {
  ifelse(
    is.na(estimate) | is.na(conf_low) | is.na(conf_high),
    NA_character_,
    sprintf("%.2f [%.2f, %.2f]", estimate, conf_low, conf_high)
  )
}

################################################################################
# Outcome derivations from clean_final survey data
################################################################################

derive_rdid_survey_outcomes <- function(df) {
  health_roots <- c(
    "target_child_clinic_resp",
    "target_child_wheezing",
    "target_child_distrubed_speech",
    "target_child_disturbed_sleep",
    "target_child_eye_red",
    "target_child_eye_itch",
    "target_child_cough",
    "target_child_fever",
    "target_child_resp_rate",
    "target_child_weight_loss",
    "target_child_lethargy",
    "respondent_cough",
    "respondent_wheezing",
    "respondent_disturbed_sleep",
    "respondent_disturbed_speech",
    "respondent_eye_red",
    "respondent_eye_itch",
    "respondent_eye_sore",
    "respondent_headache",
    "respondent_backache",
    "suicidal_thoughts_30"
  )

  for (root in health_roots) {
    yn_name <- paste0(root, "_yn")
    if (root %in% names(df)) {
      df[[yn_name]] <- make_yn_rdid(df[[root]])
    }
  }

  if ("target_child_distrubed_speech_yn" %in% names(df)) {
    df$target_child_distrubed_speech_yn <- replace_na(
      df$target_child_distrubed_speech_yn, 0L
    )
  }

  if ("respondent_disturbed_speech_yn" %in% names(df)) {
    df$respondent_disturbed_speech_yn <- replace_na(
      df$respondent_disturbed_speech_yn, 0L
    )
  }

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
      ),
      exchange_rate = exchange_bdt_per_usd[as.character(timepoint)],
      fuel_30_gather_scraps_yn = make_yn_rdid(single_num_var(., "fuel_30_gather_scraps")),
      fuel_30_collect_wood_yn = make_yn_rdid(single_num_var(., "fuel_30_collect_wood")),
      fuel_30_buy_wood_yn = make_yn_rdid(single_num_var(., "fuel_30_buy_wood")),
      fuel_30_receive_wood_yn = make_yn_rdid(single_num_var(., "fuel_30_receive_wood")),
      fuel_30_receive_lpg_yn = make_yn_rdid(single_num_var(., "fuel_30_receive_lpg")),
      fuel_30_buy_lpg_yn = make_yn_rdid(single_num_var(., "fuel_30_buy_lpg")),
      fuel_30_receive_crh_yn = make_yn_rdid(single_num_var(., "fuel_30_receive_crh")),
      fuel_30_buy_crh_yn = make_yn_rdid(single_num_var(., "fuel_30_buy_crh")),
      fuel_30_other_yn = make_yn_rdid(single_num_var(., "fuel_30_other")),
      collect_wood_times_week = dplyr::coalesce(
        single_num_var(., "collect_wood_times_week"),
        single_num_var(., "times_wood_day")
      ),
      collect_wood_walk_hr_clean = make_nonnegative_number(
        single_num_var(., "collect_wood_walk_hr")
      ),
      collect_wood_walk_hr_clean = if_else(
        collect_wood_walk_hr_clean < 0.01,
        NA_real_,
        collect_wood_walk_hr_clean
      ),
      plastic_burn_gt1_yn = case_when(
        is.na(single_num_var(., "burn_plastic_frequency")) ~ NA_integer_,
        single_num_var(., "burn_plastic_frequency") > 1 ~ 1L,
        TRUE ~ 0L
      ),
      boil_yesterday_times_clean = make_nonnegative_number(
        single_num_var(., "boil_yesterday_times")
      ),
      spent_food_bdt = make_nonnegative_number(single_num_var(., "spent_food")),
      buy_wood_cost_bdt = make_nonnegative_number(single_num_var(., "buy_wood_cost")),
      spent_total_month_bdt = make_nonnegative_number(single_num_var(., "spent_total_month")),
      spent_tobacco_pan_bdt = make_nonnegative_number(single_num_var(., "spent_tobacco_pan")),
      spent_food_usd = spent_food_bdt / exchange_rate,
      buy_wood_cost_usd = buy_wood_cost_bdt / exchange_rate,
      spent_total_month_usd = spent_total_month_bdt / exchange_rate,
      spent_tobacco_pan_usd = spent_tobacco_pan_bdt / exchange_rate,
      spent_food_pct = case_when(
        is.na(spent_food_bdt) | is.na(spent_total_month_bdt) ~ NA_real_,
        spent_food_bdt < 0 | spent_total_month_bdt <= 0 ~ NA_real_,
        spent_food_bdt > spent_total_month_bdt ~ NA_real_,
        TRUE ~ spent_food_bdt / spent_total_month_bdt
      ),
      total_income_30_bdt = make_nonnegative_number(single_num_var(., "total_income_30")),
      total_income_30_usd = total_income_30_bdt / exchange_rate,
      income_wage_labor_usd = make_nonnegative_number(
        single_num_var(., "income_wage_labor")
      ) / exchange_rate,
      income_cash_ngo_usd = make_nonnegative_number(
        single_num_var(., "income_cash_ngo")
      ) / exchange_rate,
      income_skill_labor_bdt = make_nonnegative_number(
        single_num_var(., "income_skill_labor")
      ),
      income_skill_labor_usd = income_skill_labor_bdt / exchange_rate,
      income_skill_labor_any = case_when(
        is.na(income_skill_labor_bdt) ~ NA_integer_,
        income_skill_labor_bdt > 0 ~ 1L,
        TRUE ~ 0L
      ),
      food_cant_afford_2wk_yn = make_yn_rdid(
        single_num_var(., "food_cant_afford_2wk")
      ),
      fuel_cant_afford_2wk_yn = make_yn_rdid(
        single_num_var(., "fuel_cant_afford_2wk")
      )
    )

  for (code in c(as.character(1:13), "66")) {
    df[[paste0("food_coping_action_", code)]] <- multi_select_code_yn(
      df,
      "food_cant_afford_action",
      code,
      asked_var = "food_cant_afford_2wk"
    )
  }

  for (code in c(as.character(1:15), "66")) {
    df[[paste0("fuel_coping_action_", code)]] <- multi_select_code_yn(
      df,
      "fuel_cant_afford_action",
      code,
      asked_var = "fuel_cant_afford_2wk"
    )
  }

  df <- calculate_fcs_from_weekly_vars(df)

  mental_health_good_vars <- c("happy", "enjoyed_life", "self_worth", "hopeful")
  mental_health_bad_vars <- c(
    "bothered", "sick", "no_appetite", "diff_concentrating",
    "restless_sleep", "exert_effort", "less_talkative",
    "feeling_disliked", "unfriendly_people", "fearful", "lonely",
    "crying_spells", "cant_get_going", "feeling_down", "life_failure",
    "depressed"
  )
  cesd_vars <- c(mental_health_bad_vars, mental_health_good_vars)
  cesd_vars <- cesd_vars[cesd_vars %in% names(df)]

  if (length(cesd_vars) > 0) {
    cesd_mat <- as.data.frame(lapply(cesd_vars, function(var) as_number(df[[var]])))
    cesd_missing_items <- rowSums(is.na(cesd_mat))
    df$CES_D_score <- rowSums(cesd_mat)
    df$CES_D_score[cesd_missing_items > 0] <- NA_real_
    df$CES_D_o16_score <- case_when(
      is.na(df$CES_D_score) ~ NA_integer_,
      df$CES_D_score > 16 ~ 1L,
      TRUE ~ 0L
    )
  } else {
    df$CES_D_score <- NA_real_
    df$CES_D_o16_score <- NA_integer_
  }

  df
}

survey_raw <- readRDS(file_survey_refugee_household) %>%
  add_rf105_aliases() %>%
  clean_timepoint_arm()

analysis_population <- make_analysis_population(survey_raw, id_var = "fcn_id")

file_survey_refugee_hh_members <- file.path(
  dir_clean_final, "survey_refugee_hh_members.rds"
)

hh_member_timepoint_availability <- readRDS(file_survey_refugee_hh_members) %>%
  clean_timepoint_arm() %>%
  count(timepoint, name = "n_member_rows")

safe_write_reviewed_csv(
  hh_member_timepoint_availability,
  "table_rDiD_member_timepoint_availability.csv",
  subfolder = "qa"
)

unavailable_baseline_covariates <- tibble(
  covariate = c("head_age_yrs", "head_edu_yrs"),
  prior_source = "survey_refugee_hh_members.rds, hhh_relation == 1",
  reason_not_used = paste(
    "Household-member records in clean_final are available for midline/endline",
    "but not baseline, so these cannot be baseline covariates for rDiD."
  )
)

safe_write_reviewed_csv(
  unavailable_baseline_covariates,
  "table_rDiD_unavailable_covariates.csv",
  subfolder = "qa"
)

survey_model_data <- analysis_population$all_deduplicated %>%
  derive_rdid_survey_outcomes()

total_income_30_derivation_audit <- survey_model_data %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_records = n(),
    n_reported_total_income_30 = sum(!is.na(total_income_30_reported)),
    n_component_sum_income_30 = sum(!is.na(total_income_30_component_sum)),
    n_final_total_income_30 = sum(!is.na(total_income_30)),
    n_filled_from_components = sum(
      is.na(total_income_30_reported) & !is.na(total_income_30_component_sum)
    ),
    mean_total_income_30_bdt = mean(total_income_30, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_total_income_30_bdt = ifelse(
      is.nan(mean_total_income_30_bdt), NA_real_, mean_total_income_30_bdt
    ),
    component_variables = paste(income_30_component_vars, collapse = "; ")
  )

safe_write_reviewed_csv(
  total_income_30_derivation_audit,
  "table_rDiD_income_derivation_audit.csv",
  subfolder = "qa"
)

fcs_weekly_variable_availability <- flag_missing_vars(
  survey_raw,
  fcs_weekly_input_vars,
  "RF105 weekly FCS inputs"
)

safe_write_reviewed_csv(
  fcs_weekly_variable_availability,
  "table_rDiD_fcs_variable_availability.csv",
  subfolder = "qa"
)

fcs_recalculated_scores <- survey_model_data %>%
  select(
    any_of(c(
      "study_arm_overall", "timepoint", "fcn_id", "KEY", "collection_date"
    )),
    any_of(fcs_weekly_input_vars),
    staple, pulses, veggies, fruit, meat_fish, dairy, sugar, oil,
    staple_weight, pulses_weight, veggies_weight, fruit_weight,
    meat_fish_weight, milk_weight, sugar_weight, oil_weight,
    fcs, fcs_category, fcs_binary
  )

safe_write_reviewed_csv(
  fcs_recalculated_scores,
  "table_rDiD_fcs_household_scores.csv"
)

fcs_recalculated_summary <- fcs_recalculated_scores %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_records = n(),
    n_households = n_distinct(fcn_id),
    n_nonmissing_fcs = sum(!is.na(fcs)),
    mean_fcs = mean(fcs, na.rm = TRUE),
    sd_fcs = sd(fcs, na.rm = TRUE),
    n_poor = sum(fcs_category == "poor", na.rm = TRUE),
    n_borderline = sum(fcs_category == "borderline", na.rm = TRUE),
    n_acceptable = sum(fcs_category == "acceptable", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_fcs = ifelse(is.nan(mean_fcs), NA_real_, mean_fcs),
    sd_fcs = ifelse(is.nan(sd_fcs), NA_real_, sd_fcs),
    pct_poor = ifelse(n_nonmissing_fcs > 0, 100 * n_poor / n_nonmissing_fcs, NA_real_),
    pct_borderline = ifelse(
      n_nonmissing_fcs > 0,
      100 * n_borderline / n_nonmissing_fcs,
      NA_real_
    ),
    pct_acceptable = ifelse(
      n_nonmissing_fcs > 0,
      100 * n_acceptable / n_nonmissing_fcs,
      NA_real_
    )
  )

safe_write_reviewed_csv(
  fcs_recalculated_summary,
  "table_rDiD_fcs_arm_summary.csv"
)

cesd_input_vars <- c(
  "bothered", "sick", "no_appetite", "diff_concentrating",
  "restless_sleep", "exert_effort", "less_talkative",
  "feeling_disliked", "unfriendly_people", "fearful", "lonely",
  "crying_spells", "cant_get_going", "feeling_down", "life_failure",
  "depressed", "happy", "enjoyed_life", "self_worth", "hopeful"
)
cesd_present_vars <- cesd_input_vars[cesd_input_vars %in% names(survey_model_data)]

cesd_item_missingness <- survey_model_data %>%
  select(
    any_of(c(
      "study_arm_overall", "timepoint", "fcn_id", "KEY", "collection_date",
      "CES_D_score", "CES_D_o16_score"
    ))
  ) %>%
  mutate(
    n_cesd_items_expected = length(cesd_input_vars),
    n_cesd_items_present = length(cesd_present_vars),
    n_cesd_items_missing = if (length(cesd_present_vars) > 0) {
      rowSums(is.na(survey_model_data[, cesd_present_vars, drop = FALSE]))
    } else {
      NA_integer_
    }
  )

safe_write_reviewed_csv(
  cesd_item_missingness,
  "table_rDiD_cesd_item_missingness.csv",
  subfolder = "qa"
)
################################################################################
# Outcome list
################################################################################

survey_outcomes <- tribble(
  ~outcome, ~outcome_label, ~domain, ~outcome_type, ~unit,
  "target_child_eye_itch_yn", "Child itchy eyes", "Child health", "binary", "percentage_points",
  "target_child_eye_red_yn", "Child red eyes", "Child health", "binary", "percentage_points",
  "target_child_clinic_resp_yn", "Child clinic visit for respiratory complaint", "Child health", "binary", "percentage_points",
  "target_child_wheezing_yn", "Child wheeze", "Child health", "binary", "percentage_points",
  "target_child_distrubed_speech_yn", "Child disturbed speech during wheeze", "Child health", "binary", "percentage_points",
  "target_child_cough_yn", "Child persistent cough", "Child health", "binary", "percentage_points",
  "target_child_fever_yn", "Child fever", "Child health", "binary", "percentage_points",
  "target_child_resp_rate_yn", "Child fast breathing", "Child health", "binary", "percentage_points",
  "target_child_lethargy_yn", "Child lethargy", "Child health", "binary", "percentage_points",
  "target_child_weight_loss_yn", "Child unexplained weight loss", "Child health", "binary", "percentage_points",
  "target_child_asthma", "Child asthma proxy", "Child health", "binary", "percentage_points",
  "target_child_severe_asthma", "Child severe asthma proxy", "Child health", "binary", "percentage_points",
  "respondent_disturbed_speech_yn", "Respondent disturbed speech during wheeze", "Respondent health", "binary", "percentage_points",
  "respondent_eye_red_yn", "Respondent red eyes", "Respondent health", "binary", "percentage_points",
  "respondent_eye_itch_yn", "Respondent itchy eyes", "Respondent health", "binary", "percentage_points",
  "respondent_eye_sore_yn", "Respondent sore eyes", "Respondent health", "binary", "percentage_points",
  "respondent_wheezing_yn", "Respondent wheeze", "Respondent health", "binary", "percentage_points",
  "respondent_cough_yn", "Respondent cough", "Respondent health", "binary", "percentage_points",
  "respondent_disturbed_sleep_yn", "Respondent disturbed sleep during wheeze", "Respondent health", "binary", "percentage_points",
  "respondent_headache_yn", "Respondent headache", "Respondent health", "binary", "percentage_points",
  "respondent_backache_yn", "Respondent backache", "Respondent health", "binary", "percentage_points",
  "fcs", "Food consumption score", "Food security", "continuous", "score",
  "fcs_binary", "Food consumption score <=35", "Food security", "binary", "percentage_points",
  "hdds_assume_misc_1", "Household dietary diversity score", "Food security", "continuous", "score",
  "fuel_30_gather_scraps_yn", "Household gathered scraps/leaves/twigs in past 30 days", "Fuel procurement", "binary", "percentage_points",
  "fuel_30_collect_wood_yn", "Household collected firewood in past 30 days", "Fuel procurement", "binary", "percentage_points",
  "fuel_30_buy_wood_yn", "Household bought firewood in past 30 days", "Fuel procurement", "binary", "percentage_points",
  "fuel_30_receive_wood_yn", "Household received firewood in past 30 days", "Fuel procurement", "binary", "percentage_points",
  "fuel_30_receive_lpg_yn", "Household received LPG in past 30 days", "Fuel procurement", "binary", "percentage_points",
  "fuel_30_buy_lpg_yn", "Household bought LPG in past 30 days", "Fuel procurement", "binary", "percentage_points",
  "fuel_30_receive_crh_yn", "Household received CRH/charcoal in past 30 days", "Fuel procurement", "binary", "percentage_points",
  "fuel_30_buy_crh_yn", "Household bought CRH/charcoal in past 30 days", "Fuel procurement", "binary", "percentage_points",
  "fuel_30_other_yn", "Household used other fuel in past 30 days", "Fuel procurement", "binary", "percentage_points",
  "plastic_burn_gt1_yn", "Household burned plastic more than once per week", "Fuel procurement", "binary", "percentage_points",
  "collect_wood_times_week", "Firewood collection trips per week", "Fuel procurement", "continuous", "trips_per_week",
  "collect_wood_walk_hr_clean", "Walking time to collect firewood", "Time use", "continuous", "hours",
  "boil_yesterday_times_clean", "Times boiled water yesterday", "Stove use", "continuous", "times",
  "spent_food_usd", "Food expenditures", "Expenditures", "continuous", "USD",
  "buy_wood_cost_usd", "Wood expenditures", "Expenditures", "continuous", "USD",
  "spent_total_month_usd", "Total monthly expenditures", "Expenditures", "continuous", "USD",
  "spent_tobacco_pan_usd", "Tobacco/pan expenditures", "Expenditures", "continuous", "USD",
  "spent_food_pct", "Food expenditures as share of total expenditures", "Expenditures", "continuous", "proportion",
  "total_income_30_usd", "Total household income in past 30 days", "Income", "continuous", "USD",
  "income_wage_labor_usd", "Wage-labor income", "Income", "continuous", "USD",
  "income_cash_ngo_usd", "NGO cash income", "Income", "continuous", "USD",
  "food_cant_afford_2wk_yn", "Household could not afford food in past 2 weeks", "Food coping", "binary", "percentage_points",
  "food_coping_action_1", "Food coping: borrowed food or relied on relatives/friends", "Food coping", "binary", "percentage_points",
  "food_coping_action_2", "Food coping: reduced food per meal", "Food coping", "binary", "percentage_points",
  "food_coping_action_3", "Food coping: reduced meals per day", "Food coping", "binary", "percentage_points",
  "food_coping_action_4", "Food coping: skipped all meals on some days", "Food coping", "binary", "percentage_points",
  "food_coping_action_5", "Food coping: restricted adult food so children under 5 could eat", "Food coping", "binary", "percentage_points",
  "food_coping_action_6", "Food coping: sold household goods", "Food coping", "binary", "percentage_points",
  "food_coping_action_7", "Food coping: purchased food on credit", "Food coping", "binary", "percentage_points",
  "food_coping_action_8", "Food coping: borrowed money", "Food coping", "binary", "percentage_points",
  "food_coping_action_9", "Food coping: reduced health or education expenditures", "Food coping", "binary", "percentage_points",
  "food_coping_action_10", "Food coping: spent savings", "Food coping", "binary", "percentage_points",
  "food_coping_action_11", "Food coping: worked for money to buy food", "Food coping", "binary", "percentage_points",
  "food_coping_action_12", "Food coping: sold or consumed livestock", "Food coping", "binary", "percentage_points",
  "food_coping_action_13", "Food coping: exchanged food for other food", "Food coping", "binary", "percentage_points",
  "food_coping_action_66", "Food coping: other strategy", "Food coping", "binary", "percentage_points",
  "fuel_cant_afford_2wk_yn", "Household could not afford fuel in past 2 weeks", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_1", "Fuel coping: borrowed fuel", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_2", "Fuel coping: reduced food per meal", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_3", "Fuel coping: reduced meals per day", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_4", "Fuel coping: skipped all meals on some days", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_5", "Fuel coping: restricted adult food so children under 5 could eat", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_6", "Fuel coping: sold household goods", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_7", "Fuel coping: purchased fuel on credit", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_8", "Fuel coping: borrowed money", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_9", "Fuel coping: reduced health or education expenditures", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_10", "Fuel coping: spent savings", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_11", "Fuel coping: worked for money to buy fuel", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_12", "Fuel coping: sold livestock to purchase fuel", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_13", "Fuel coping: sold food to purchase fuel", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_14", "Fuel coping: ate food that did not need cooking", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_15", "Fuel coping: ate food that was not fully cooked", "Fuel coping", "binary", "percentage_points",
  "fuel_coping_action_66", "Fuel coping: other strategy", "Fuel coping", "binary", "percentage_points",
  "income_skill_labor_any", "Any skilled-labor income", "Livelihoods", "binary", "percentage_points",
  "income_skill_labor_usd", "Skilled-labor income", "Livelihoods", "continuous", "USD",
  "CES_D_o16_score", "CES-D score >16", "Mental health", "binary", "percentage_points",
  "suicidal_thoughts_30_yn", "Suicidal thoughts in past 30 days", "Mental health", "binary", "percentage_points"
) %>%
  mutate(outcome_source = "survey_clean_final")

survey_outcome_availability <- flag_missing_vars(
  survey_model_data,
  survey_outcomes$outcome,
  "RF105 rDiD survey outcomes"
)

safe_write_reviewed_csv(
  survey_outcome_availability,
  "table_rDiD_outcome_variable_availability.csv",
  subfolder = "qa"
)

survey_outcomes <- survey_outcomes %>%
  filter(outcome %in% survey_outcome_availability$variable[survey_outcome_availability$available])

rf105_source_outcome_audit <- tribble(
  ~source_outcome, ~reviewed_outcome, ~source_file, ~reviewed_status, ~calculation_note,
"target_child_eye_itch_yn", "target_child_eye_itch_yn", "implemented_in_current_rdid_script", "included", "Binary yes/no derived from target_child_eye_itch using the current binary recode: 0 = no, positive values = yes.",
"target_child_eye_red_yn", "target_child_eye_red_yn", "implemented_in_current_rdid_script", "included", "Binary yes/no derived from target_child_eye_red using the current binary recode.",
"target_child_clinic_resp_yn", "target_child_clinic_resp_yn", "implemented_in_current_rdid_script", "included", "Binary yes/no derived from target_child_clinic_resp; refused/don't know values are set to missing.",
"target_child_wheezing_yn", "target_child_wheezing_yn", "implemented_in_current_rdid_script", "included", "Binary yes/no derived from target_child_wheezing; refused/don't know values are set to missing.",
"target_child_distrubed_speech_yn", "target_child_distrubed_speech_yn", "implemented_in_current_rdid_script", "included", "Matches RF105B XGBoost code by setting skipped/missing disturbed-speech responses to 0 before modeling.",
"respondent_disturbed_speech_yn", "respondent_disturbed_speech_yn", "implemented_in_current_rdid_script", "included", "Matches RF105B XGBoost code by setting skipped/missing disturbed-speech responses to 0 before modeling.",
"respondent_eye_red_yn", "respondent_eye_red_yn", "implemented_in_current_rdid_script", "included", "Binary any-symptom indicator derived from respondent_eye_red.",
"respondent_eye_itch_yn", "respondent_eye_itch_yn", "implemented_in_current_rdid_script", "included", "Binary any-symptom indicator derived from respondent_eye_itch.",
"respondent_eye_sore_yn", "respondent_eye_sore_yn", "implemented_in_current_rdid_script", "included", "Binary any-symptom indicator derived from respondent_eye_sore.",
"respondent_wheezing_yn", "respondent_wheezing_yn", "implemented_in_current_rdid_script", "included", "Binary yes/no derived from respondent_wheezing.",
"target_child_resp_rate", "target_child_resp_rate_yn", "implemented_in_current_rdid_script", "included_as_binary", "Raw survey values are yes/no/don't know/refused; reviewed workflow models the cleaned binary indicator.",
"target_child_cough", "target_child_cough_yn", "implemented_in_current_rdid_script", "included_as_binary", "Raw survey values are yes/no/don't know/refused; reviewed workflow models the cleaned binary indicator.",
"target_child_fever", "target_child_fever_yn", "implemented_in_current_rdid_script", "included_as_binary", "Raw survey values are yes/no/don't know/refused; reviewed workflow models the cleaned binary indicator.",
"target_child_lethargy", "target_child_lethargy_yn", "implemented_in_current_rdid_script", "included_as_binary", "Added after audit because this child general-health outcome was in the requested outcome audit.",
"target_child_weight_loss", "target_child_weight_loss_yn", "implemented_in_current_rdid_script", "included_as_binary", "Added after audit because this child general-health outcome was in the requested outcome audit.",
"respondent_cough", "respondent_cough_yn", "implemented_in_current_rdid_script", "included_as_binary", "Added after audit because this respondent respiratory outcome was in the requested outcome audit.",
"respondent_disturbed_sleep", "respondent_disturbed_sleep_yn", "implemented_in_current_rdid_script", "included_as_binary", "Added after audit; keep sample-size columns visible because this skip-pattern variable has many missing values.",
"respondent_headache", "respondent_headache_yn", "implemented_in_current_rdid_script", "included_as_binary", "Added after audit because this respondent non-respiratory symptom was in the requested outcome audit.",
"respondent_backache", "respondent_backache_yn", "implemented_in_current_rdid_script", "included_as_binary", "Added after audit because this respondent non-respiratory symptom was in the requested outcome audit.",
"fcs", "fcs", "implemented_in_current_rdid_script", "included_corrected", "Recalculated from weekly food-group variables, cap at 7 days, apply WFP weights, and classify using <=21, 21.5-35, >35 cutoffs.",
"fcs_binary", "fcs_binary", "implemented_in_current_rdid_script", "included_corrected", "Binary food insecurity indicator equals 1 for poor/borderline FCS (<=35) and 0 for acceptable FCS (>35).",
"hdds_assume_misc_1", "hdds_assume_misc_1", "implemented_in_current_rdid_script", "included_corrected", "Uses clean_final baseline-compatible HDDS assuming miscellaneous group equals 1. The cleaner derives HDDS from weekly adult food-frequency variables using the embedded HDDS food-group mapping because past-24-hour HDDS inputs are not available at baseline.",
"spent_food", "spent_food_usd", "implemented_in_current_rdid_script", "included_corrected", "Converted from BDT to USD using baseline 84.88, midline 84.74, and endline 93.45 BDT/USD.",
"buy_wood_cost", "buy_wood_cost_usd", "implemented_in_current_rdid_script", "included_corrected", "Converted from BDT to USD using baseline 84.88, midline 84.74, and endline 93.45 BDT/USD.",
"fuel_30_gather_scraps", "fuel_30_gather_scraps_yn", "implemented_in_current_rdid_script", "included_as_alias", "Derived from the reviewed fuel_30_gather_scraps alias created from fuel_30_scraps when needed.",
"fuel_30_buy_wood", "fuel_30_buy_wood_yn", "implemented_in_current_rdid_script", "included", "Binary yes/no derived with refused/don't know values set to missing.",
"fuel_30_receive_wood", "fuel_30_receive_wood_yn", "implemented_in_current_rdid_script", "included", "Binary yes/no derived with refused/don't know values set to missing.",
"fuel_30_receive_lpg", "fuel_30_receive_lpg_yn", "implemented_in_current_rdid_script", "included", "Binary yes/no derived with refused/don't know values set to missing.",
"fuel_30_buy_lpg", "fuel_30_buy_lpg_yn", "implemented_in_current_rdid_script", "included", "Binary yes/no derived with refused/don't know values set to missing.",
"fuel_30_receive_crh", "fuel_30_receive_crh_yn", "implemented_in_current_rdid_script", "included", "Binary yes/no derived with refused/don't know values set to missing.",
"fuel_30_buy_crh", "fuel_30_buy_crh_yn", "implemented_in_current_rdid_script", "included", "Binary yes/no derived with refused/don't know values set to missing.",
"fuel_30_other", "fuel_30_other_yn", "implemented_in_current_rdid_script", "included", "Binary yes/no derived with refused/don't know values set to missing.",
"burn_plastic_frequency", "plastic_burn_gt1_yn", "implemented_in_current_rdid_script", "included_corrected", "Matches the reviewed plastic_burn_yn definition: burn_plastic_frequency > 1.",
"boil_yesterday_times", "boil_yesterday_times_clean", "implemented_in_current_rdid_script", "included", "Non-negative number of times the household boiled water yesterday.",
"income_wage_labor", "income_wage_labor_usd", "implemented_in_current_rdid_script", "included_corrected", "Converted from BDT to USD using timepoint-specific exchange rates.",
"income_cash_ngo", "income_cash_ngo_usd", "implemented_in_current_rdid_script", "included_corrected", "Converted from BDT to USD using timepoint-specific exchange rates.",
"spent_total_month_with_6mo_monthly", "spent_total_month_usd", "implemented_in_current_rdid_script", "included_with_clean_final_rename", "The exact prior variable is absent from clean_final; reviewed code uses spent_total_month, the available cleaned total monthly expenditure field.",
"spent_tobacco_pan", "spent_tobacco_pan_usd", "implemented_in_current_rdid_script", "included_corrected", "Converted from BDT to USD using timepoint-specific exchange rates.",
"spent_food_pct", "spent_food_pct", "implemented_in_current_rdid_script", "included_corrected", "Reviewed code calculates spent_food / spent_total_month when both values are nonmissing and total spending is >0; the prior if_else expression returned a logical value for most positive food-spending records.",
"total_income_30", "total_income_30_usd", "implemented_in_current_rdid_script", "included_corrected", "Baseline total_income_30 is calculated as the sum of income_cash_ngo, income_own_business, income_wage_labor, income_skill_labor, income_selling_wood, income_abroad, income_humanitarian_asst, income_handicrafts_tailoring, and income_farming when the aggregate is missing; follow-up reported aggregates are preserved when nonmissing. Converted from BDT to USD using timepoint-specific exchange rates.",
"CES_D_o16_score", "CES_D_o16_score", "implemented_in_current_rdid_script", "included_corrected", "CES-D score is the sum of all 20 cleaned CES-D items; reviewed code now sets the score to missing if any item is missing, matching rowwise sum() behavior.",
"suicidal_thoughts_30_yn", "suicidal_thoughts_30_yn", "implemented_in_current_rdid_script", "included", "Binary indicator equals 0 for never and 1 for any nonzero frequency.",
"target_child_asthma", "target_child_asthma", "implemented_in_current_rdid_script", "included", "Binary asthma proxy equals child wheeze.",
"target_child_severe_asthma", "target_child_severe_asthma", "implemented_in_current_rdid_script", "included_corrected", "Corrected to wheeze AND disturbed speech only; the reviewed script had incorrectly included disturbed sleep.",
  "respondent_resp_rate", NA_character_, "1.5_define_vector_columns.R; host respiratory scripts", "not_available_in_clean_final", "Column was requested in the outcome audit but is not present in survey_refugee_household.rds.",
  "respondent_weight_loss", NA_character_, "1.5_define_vector_columns.R; host respiratory scripts", "not_available_in_clean_final", "Column was requested in the outcome audit but is not present in survey_refugee_household.rds.",
"respondent_eye_red", "respondent_eye_red_yn", "implemented_in_current_rdid_script", "represented_as_binary", "The old exploratory DiD file also included raw ordinal/frequency symptom variables; reviewed RF105B rDiD models the binary any-symptom indicators used by the RF105B XGBoost workflow.",
"respondent_eye_itch", "respondent_eye_itch_yn", "implemented_in_current_rdid_script", "represented_as_binary", "The old exploratory DiD file also included raw ordinal/frequency symptom variables; reviewed RF105B rDiD models the binary any-symptom indicators used by the RF105B XGBoost workflow.",
"respondent_eye_sore", "respondent_eye_sore_yn", "implemented_in_current_rdid_script", "represented_as_binary", "The old exploratory DiD file also included raw ordinal/frequency symptom variables; reviewed RF105B rDiD models the binary any-symptom indicators used by the RF105B XGBoost workflow.",
"respondent_wheezing", "respondent_wheezing_yn", "implemented_in_current_rdid_script", "represented_as_binary", "The old exploratory DiD file also included raw yes/no variables; reviewed RF105B rDiD models the cleaned binary indicator."
) %>%
  mutate(
    reviewed_outcome_available = if_else(
      is.na(reviewed_outcome),
      FALSE,
      reviewed_outcome %in% survey_outcome_availability$variable[
        survey_outcome_availability$available
      ]
    ),
    modeled_in_reviewed_rdid = if_else(
      is.na(reviewed_outcome),
      FALSE,
      reviewed_outcome %in% survey_outcomes$outcome
    )
  )

safe_write_reviewed_csv(
  rf105_source_outcome_audit,
  "table_rDiD_outcome_code_audit.csv",
  subfolder = "qa"
)

################################################################################
# Harassment rDiD estimability audit
################################################################################

# The draft harassment script warns not to use the baseline harassment variables
# for causal comparison because their recall period was not harmonized with the
# follow-up "ever since arriving at camp" wording. The reviewed workflow therefore
# writes rDiD result-shaped non-estimability rows rather than fabricating modeled
# intervention effects from non-comparable measurements.

rdid_harassment_type_lookup <- tribble(
  ~harassment_type, ~harassment_category, ~harassment_type_label,
  "insult", "Verbal/emotional", "Insulted",
  "belittle", "Verbal/emotional", "Belittled",
  "scare", "Verbal/emotional", "Scared",
  "push", "Physical", "Pushed",
  "hit", "Physical", "Hit",
  "kick", "Physical", "Kicked",
  "choke", "Physical", "Choked",
  "weapon", "Physical", "Threatened with a weapon",
  "sex_lang", "Sexual", "Sexual language",
  "sex_contact", "Sexual", "Sexual contact",
  "sex_rumor", "Sexual", "Sexual rumors",
  "clothing_pull", "Sexual", "Clothing pulled",
  "sex_corner", "Sexual", "Sexual cornering"
)

rdid_harassment_category_levels <- c("Verbal/emotional", "Physical", "Sexual", "Any harassment")

rdid_harassment_sources <- bind_rows(
  rdid_harassment_type_lookup %>%
    transmute(
      source_variable = paste0(harassment_type, "_hh"),
      harassment_type,
      harassment_category,
      harassment_type_label,
      source_form = "baseline_no_ever_suffix",
      intended_timepoint = "baseline",
      recall_note = paste(
        "Baseline source has no _ever suffix; draft harassment code says not",
        "to use baseline harassment variables because the recall period was not specified."
      )
    ),
  rdid_harassment_type_lookup %>%
    transmute(
      source_variable = paste0(harassment_type, "_hh_ever"),
      harassment_type,
      harassment_category,
      harassment_type_label,
      source_form = "midline_ever_suffix",
      intended_timepoint = "midline",
      recall_note = paste(
        "Midline source uses _ever wording for events since arriving at camp;",
        "there are no nonmissing endline household harassment _ever variables in clean_final."
      )
    )
) %>%
  mutate(available = source_variable %in% names(survey_model_data))

rdid_harassment_available_vars <- rdid_harassment_sources %>%
  filter(available) %>%
  pull(source_variable)

rdid_harassment_variable_availability <- tibble()
rdid_harassment_long <- tibble()
if (length(rdid_harassment_available_vars) > 0) {
  rdid_harassment_long <- survey_model_data %>%
    select(fcn_id, timepoint, study_arm_overall, all_of(rdid_harassment_available_vars)) %>%
    pivot_longer(
      cols = all_of(rdid_harassment_available_vars),
      names_to = "source_variable",
      values_to = "value"
    ) %>%
    left_join(rdid_harassment_sources, by = "source_variable") %>%
    mutate(
      value_num = as_number(value),
      harassed = case_when(
        is.na(value_num) ~ NA_integer_,
        value_num > 0 ~ 1L,
        value_num == 0 ~ 0L,
        TRUE ~ NA_integer_
      )
    )

  rdid_harassment_variable_availability <- rdid_harassment_long %>%
    group_by(source_variable, source_form, intended_timepoint, harassment_type,
             harassment_type_label, harassment_category, timepoint, study_arm_overall) %>%
    summarise(
      n_nonmissing = sum(!is.na(harassed)),
      n_harassed = sum(harassed == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    right_join(rdid_harassment_sources, by = c(
      "source_variable", "source_form", "intended_timepoint", "harassment_type",
      "harassment_type_label", "harassment_category"
    )) %>%
    mutate(
      n_nonmissing = replace_na(n_nonmissing, 0L),
      n_harassed = replace_na(n_harassed, 0L)
    ) %>%
    arrange(source_variable, timepoint, study_arm_overall)
}

safe_write_reviewed_csv(
  rdid_harassment_variable_availability,
  "table_rDiD_harassment_variable_audit.csv",
  subfolder = "qa"
)

rdid_harassment_category_long <- tibble()
if (nrow(rdid_harassment_long) > 0) {
  rdid_harassment_category_long <- rdid_harassment_long %>%
    group_by(fcn_id, timepoint, study_arm_overall, harassment_category) %>%
    summarise(
      n_nonmissing_components = sum(!is.na(harassed)),
      harassed = case_when(
        any(harassed == 1, na.rm = TRUE) ~ 1L,
        n_nonmissing_components > 0 ~ 0L,
        TRUE ~ NA_integer_
      ),
      source_form = paste(sort(unique(source_form[!is.na(value_num)])), collapse = "; "),
      source_variables = paste(sort(unique(source_variable[!is.na(value_num)])), collapse = "; "),
      recall_note = paste(sort(unique(recall_note[!is.na(value_num)])), collapse = " "),
      .groups = "drop"
    ) %>%
    bind_rows(
      rdid_harassment_long %>%
        group_by(fcn_id, timepoint, study_arm_overall) %>%
        summarise(
          harassment_category = "Any harassment",
          n_nonmissing_components = sum(!is.na(harassed)),
          harassed = case_when(
            any(harassed == 1, na.rm = TRUE) ~ 1L,
            n_nonmissing_components > 0 ~ 0L,
            TRUE ~ NA_integer_
          ),
          source_form = paste(sort(unique(source_form[!is.na(value_num)])), collapse = "; "),
          source_variables = paste(sort(unique(source_variable[!is.na(value_num)])), collapse = "; "),
          recall_note = paste(sort(unique(recall_note[!is.na(value_num)])), collapse = " "),
          .groups = "drop"
        )
    )
}

rdid_harassment_candidate_summary <- rdid_harassment_category_long %>%
  group_by(timepoint, study_arm_overall, harassment_category, source_form,
           source_variables, recall_note) %>%
  summarise(
    n_total = n(),
    n_nonmissing = sum(!is.na(harassed)),
    n_harassed = sum(harassed == 1, na.rm = TRUE),
    percent_harassed = if_else(n_nonmissing > 0, 100 * n_harassed / n_nonmissing, NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    harassment_category = factor(harassment_category, levels = rdid_harassment_category_levels)
  ) %>%
  arrange(harassment_category, timepoint, study_arm_overall)

safe_write_reviewed_csv(
  rdid_harassment_candidate_summary,
  "table_rDiD_harassment_prevalence_audit.csv",
  subfolder = "qa"
)

rdid_harassment_totals <- rdid_harassment_candidate_summary %>%
  mutate(harassment_category = as.character(harassment_category)) %>%
  group_by(harassment_category, timepoint) %>%
  summarise(
    n_nonmissing_total = sum(n_nonmissing, na.rm = TRUE),
    n_harassed_total = sum(n_harassed, na.rm = TRUE),
    percent_harassed_total = if_else(
      n_nonmissing_total > 0,
      100 * n_harassed_total / n_nonmissing_total,
      NA_real_
    ),
    source_form = paste(sort(unique(source_form[n_nonmissing > 0])), collapse = "; "),
    .groups = "drop"
  )

get_harassment_total <- function(category, timepoint_value, column) {
  value <- rdid_harassment_totals %>%
    filter(harassment_category == category, timepoint == timepoint_value) %>%
    pull(all_of(column))

  if (length(value) == 0) {
    if (str_starts(column, "n_")) {
      return(0)
    }
    return(NA)
  }

  value[[1]]
}

rdid_harassment_contrasts <- tribble(
  ~contrast, ~baseline_timepoint, ~followup_timepoint,
  "primary_baseline_midline", "baseline", "midline",
  "secondary_baseline_endline", "baseline", "endline"
)

rdid_harassment_estimability <- tidyr::expand_grid(
  rdid_harassment_contrasts,
  tibble(
    harassment_category = rdid_harassment_category_levels,
    requested_outcome = paste0(
      c("Verbal/emotional", "Physical", "Sexual", "Any harassment"),
      " harassment prevalence"
    )
  )
) %>%
  rowwise() %>%
  mutate(
    baseline_n_nonmissing = as.integer(get_harassment_total(
      harassment_category, baseline_timepoint, "n_nonmissing_total"
    )),
    baseline_n_harassed = as.integer(get_harassment_total(
      harassment_category, baseline_timepoint, "n_harassed_total"
    )),
    baseline_percent_harassed = as.numeric(get_harassment_total(
      harassment_category, baseline_timepoint, "percent_harassed_total"
    )),
    followup_n_nonmissing = as.integer(get_harassment_total(
      harassment_category, followup_timepoint, "n_nonmissing_total"
    )),
    followup_n_harassed = as.integer(get_harassment_total(
      harassment_category, followup_timepoint, "n_harassed_total"
    )),
    followup_percent_harassed = as.numeric(get_harassment_total(
      harassment_category, followup_timepoint, "percent_harassed_total"
    )),
    baseline_source_form = as.character(get_harassment_total(
      harassment_category, baseline_timepoint, "source_form"
    )),
    followup_source_form = as.character(get_harassment_total(
      harassment_category, followup_timepoint, "source_form"
    )),
    estimator = "rDiD/XGBoost",
    outcome_type = "binary",
    unit = "percentage_points",
    estimate = NA_real_,
    se = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    p_value = NA_real_,
    significant = NA,
    rdid_status = case_when(
      followup_n_nonmissing == 0 ~ "not_rdid_estimable_no_followup_harassment_data",
      baseline_n_nonmissing == 0 ~ "not_rdid_estimable_no_baseline_harassment_data",
      baseline_source_form != followup_source_form ~ "not_rdid_estimable_noncomparable_recall_periods",
      TRUE ~ "not_rdid_estimable_review_required"
    ),
    calculation_note = case_when(
      rdid_status == "not_rdid_estimable_no_followup_harassment_data" ~ paste(
        "No nonmissing household-level harassment data are available at", followup_timepoint,
        "in clean_final, so this contrast cannot be estimated."
      ),
      rdid_status == "not_rdid_estimable_noncomparable_recall_periods" ~ paste(
        "Baseline uses no-_ever harassment variables that the draft code flags as",
        "not comparable because the recall period was not specified; midline uses",
        "_ever variables for events since arriving at camp. Estimate intentionally",
        "left missing."
      ),
      TRUE ~ "Harassment rDiD not estimated; see source-form and missingness diagnostics."
    ),
    population = "deduplicated_households_with_candidate_harassment_data"
  ) %>%
  ungroup() %>%
  select(
    contrast, requested_outcome, harassment_category, estimator, outcome_type, unit,
    estimate, se, conf_low, conf_high, p_value, significant, rdid_status,
    baseline_timepoint, followup_timepoint, baseline_n_nonmissing,
    baseline_n_harassed, baseline_percent_harassed, followup_n_nonmissing,
    followup_n_harassed, followup_percent_harassed, baseline_source_form,
    followup_source_form, population, calculation_note
  )

safe_write_reviewed_csv(
  rdid_harassment_estimability,
  "table_rDiD_harassment_estimability.csv"
)
safe_write_reviewed_csv(
  rdid_harassment_estimability,
  "table_rDiD_harassment_estimability.csv",
  subfolder = "qa"
)

requested_rdid_outcome_coverage <- tribble(
  ~request_item, ~requested_outcome, ~rdid_status, ~modeled_outcomes, ~input_variables, ~calculation_note,
  1, "Asthma and severe asthma", "modeled", "target_child_asthma; target_child_severe_asthma", "target_child_wheezing; target_child_distrubed_speech", "Child asthma proxy is child wheeze. Child severe-asthma proxy is child wheeze AND disturbed speech, matching the reviewed RF105B XGBoost logic.",
  2, "Proportion of households collecting firewood", "modeled", "fuel_30_collect_wood_yn", "fuel_30_collect_wood", "Binary household indicator for firewood collection in the past 30 days.",
  3, "Amount of firewood collected", "partially_modeled_available_frequency_measure_only", "collect_wood_times_week", "times_wood_day; collect_wood_times_week alias in cleaning code", "clean_final does not contain a comparable baseline/follow-up weight, bundle, or volume measure of collected firewood. The reviewed rDiD model uses the available collection-frequency variable: trips to collect wood per week.",
  4, "Time spent collecting fuel", "partially_modeled_firewood_walking_time_only", "collect_wood_walk_hr_clean", "collect_wood_walk_hr", "The comparable baseline/follow-up measure is walking time to collect firewood. Broader LPG/CRH walking and waiting measures are not available at baseline in the same form for rDiD.",
  5, "Time spent cooking", "not_rdid_estimable", NA_character_, "time_cooking", "time_cooking is not observed in both arms at baseline and follow-up in clean_final; it is mainly a categorical change item rather than a repeated baseline-to-follow-up outcome.",
  6, "Money spent on firewood", "modeled", "buy_wood_cost_usd", "buy_wood_cost; exchange_bdt_per_usd", "Monthly wood expenditure converted from BDT to USD using project exchange rates.",
  7, "Money spent on food", "modeled", "spent_food_usd", "spent_food; exchange_bdt_per_usd", "Food expenditure converted from BDT to USD using project exchange rates.",
  8, "Food security", "modeled", "fcs; fcs_binary", "weekly adult food-consumption variables", "Food Consumption Score recalculated from weekly food-group variables using WFP weights and modeled as continuous FCS and poor/borderline FCS <=35.",
  9, "Dietary diversity", "modeled", "hdds_assume_misc_1", "clean_final hdds_assume_misc_1; weekly adult food-frequency variables", "HDDS assuming miscellaneous group equals 1 is now available at baseline, midline, and endline from clean_final and is modeled as a continuous rDiD outcome.",
  10, "Coping strategies", "modeled", paste(c("food_cant_afford_2wk_yn", paste0("food_coping_action_", c(1:13, 66)), "fuel_cant_afford_2wk_yn", paste0("fuel_coping_action_", c(1:15, 66))), collapse = "; "), "food_cant_afford_2wk; food_cant_afford_action*; fuel_cant_afford_2wk; fuel_cant_afford_action*", "Models include overall food/fuel shortage indicators and individual shortage-management strategies. Households without the relevant shortage are coded 0 for strategy use; missing strategy data among households with a shortage remain missing.",
  11, "Verbal, physical, sexual, and combined harassment prevalence", "not_rdid_estimable_documented", NA_character_, "baseline *_hh variables; midline *_hh_ever variables; no nonmissing endline household harassment variables", "Reviewed code writes table_rDiD_harassment_estimability.csv with result-shaped NA rDiD rows for verbal/emotional, physical, sexual, and any harassment. Estimates are intentionally not modeled because the draft harassment code says baseline recall was not specified, midline uses _ever since-arrival wording, and endline harassment data are missing in clean_final.",
  12, "Livelihood training and use of skills", "partially_modeled_use_of_skilled_labor_only", "income_skill_labor_any; income_skill_labor_usd", "income_skill_labor; livlihood_training_ever; livlihood_skills_freq; livlihood_training_SAFE", "Use of skilled labor is represented by any skilled-labor income and skilled-labor income amount. Livelihood training variables are midline/endline only with no baseline values, so training uptake and training-related skill use cannot be estimated with baseline-to-follow-up rDiD."
) %>%
  rowwise() %>%
  mutate(
    modeled_outcomes_all_in_script = if (is.na(modeled_outcomes)) {
      NA
    } else {
      all(str_squish(unlist(str_split(modeled_outcomes, ";"))) %in% survey_outcomes$outcome)
    }
  ) %>%
  ungroup()

safe_write_reviewed_csv(
  requested_rdid_outcome_coverage,
  "table_rDiD_requested_outcome_coverage.csv",
  subfolder = "qa"
)

outcome_audit_summary_file <- file.path(
  dir_tables_qa,
  "table_rDiD_outcome_audit_summary.md"
)

writeLines(
  c(
    "# RF105 reviewed rDiD outcome-code audit",
    "",
    paste("Generated:", Sys.Date()),
    "",
    "Outcome derivation provenance:",
    "- All modeled outcome derivations used by this workflow are implemented directly in this script or read from 4_data/clean_final.",
    "",
    "Corrections made in the reviewed workflow:",
    "- FCS is recalculated from weekly food-group variables using embedded caps, weights, and category cutoffs.",
    "- Dietary diversity is modeled using clean_final hdds_assume_misc_1, which is baseline-compatible and derived in the cleaner from weekly food-frequency variables using the embedded HDDS food-group mapping.",
    "- CES-D now requires all 20 cleaned CES-D items to be nonmissing, using complete-case CES-D scoring.",
    "- The severe-asthma proxy now uses the embedded RF105 severe-asthma definition: child wheeze AND disturbed speech. The earlier reviewed script had also counted disturbed sleep.",
    "- Food and wood expenditures now use the project exchange rates defined in the active RF105 configuration: 84.88, 84.74, and 93.45 BDT/USD for baseline, midline, and endline.",
    "- Additional requested health outcomes were added as binary rDiD outcomes where the clean_final columns are available: child lethargy, child weight loss, respondent cough, respondent disturbed sleep, respondent headache, and respondent backache.",
    "",
    "Documented discrepancies or limitations:",
    "- respondent_resp_rate and respondent_weight_loss were requested in the outcome audit but are not present in survey_refugee_household.rds, so they cannot be modeled from clean_final.",
    "- Harassment rDiD estimates are intentionally not modeled. Baseline harassment variables lack harmonized recall wording, midline variables use _ever since-arrival wording, and endline household harassment variables are not nonmissing in clean_final. See table_rDiD_harassment_estimability.csv.",
    "- This rDiD workflow models cleaned binary symptom indicators and records those mappings in the audit CSV.",
    "",
    "See table_rDiD_outcome_code_audit.csv for the row-level outcome mapping.",
    "See table_rDiD_requested_outcome_coverage.csv for the explicit modeled/not-estimable status of each requested intervention-impact outcome."
  ),
  outcome_audit_summary_file
)
message("Wrote QA summary: ", outcome_audit_summary_file)

################################################################################
# PM2.5 household-timepoint outcome
################################################################################

find_ambient_adjusted_pm_file <- function() {
  same_day_dir <- file.path(
    project_root,
    "7_tables",
    paste0("pm25_ambient_adjusted_", date_stamp)
  )
  same_day_file <- file.path(
    same_day_dir,
    "table_rDiD_pm25_panel_internal.csv"
  )

  if (file.exists(same_day_file)) {
    return(tibble(
      ambient_adjusted_table_dir = same_day_dir,
      ambient_adjusted_pm_file = same_day_file,
      source_recency = "same_date_as_reviewed_rdid_run"
    ))
  }

  candidate_dirs <- list.dirs(file.path(project_root, "7_tables"),
                              full.names = TRUE, recursive = FALSE)
  candidate_dirs <- candidate_dirs[
    str_detect(basename(candidate_dirs), "^pm25_ambient_adjusted_")
  ]
  candidate_files <- file.path(
    candidate_dirs,
    "table_rDiD_pm25_panel_internal.csv"
  )
  candidate_files <- candidate_files[file.exists(candidate_files)]

  if (length(candidate_files) == 0) {
    stop(
      "No ambient-adjusted PM2.5 household-timepoint file found. Run ",
      "5_analysis_RF105/6_PM_analysis/3_pm25_ambient_adjusted_analysis_20260805_2213.R before ",
      "4_rdid_xgboost_20260805_2213.R so the reviewed PM rDiD uses adjusted data.",
      call. = FALSE
    )
  }

  latest_file <- candidate_files[[order(basename(dirname(candidate_files)),
                                        decreasing = TRUE)[[1]]]]
  tibble(
    ambient_adjusted_table_dir = dirname(latest_file),
    ambient_adjusted_pm_file = latest_file,
    source_recency = "latest_available_prior_ambient_adjusted_run"
  )
}

pm_adjusted_source <- find_ambient_adjusted_pm_file()
pm_adjusted_file <- pm_adjusted_source$ambient_adjusted_pm_file[[1]]

pm_adjusted_required_vars <- c(
  "fcn_id", "timepoint", "study_arm_overall", "collection_date_min",
  "collection_date_max", "n_windows", "n_monitor_files",
  "n_pm_observations", "mean_ambient_coverage_prop",
  "indoor_minus_ambient_material_default", "indoor_minus_ambient_f025",
  "indoor_minus_ambient_f050", "indoor_minus_ambient_f100"
)

pm_adjusted_raw <- readr::read_csv(pm_adjusted_file, show_col_types = FALSE)
pm_adjusted_missing_vars <- setdiff(pm_adjusted_required_vars, names(pm_adjusted_raw))
if (length(pm_adjusted_missing_vars) > 0) {
  stop(
    "Ambient-adjusted PM2.5 file is missing required column(s): ",
    paste(pm_adjusted_missing_vars, collapse = ", "),
    call. = FALSE
  )
}

pm_household <- pm_adjusted_raw %>%
  clean_timepoint_arm() %>%
  transmute(
    fcn_id = as.character(fcn_id),
    timepoint,
    study_arm_overall = as.character(study_arm_overall),
    collection_date_min = as.Date(collection_date_min),
    collection_date_max = as.Date(collection_date_max),
    n_windows = as_number(n_windows),
    n_monitor_files = as_number(n_monitor_files),
    pm25_n_observations = as_number(n_pm_observations),
    mean_ambient_coverage_prop = as_number(mean_ambient_coverage_prop),
    pm25_ambient_excess_default = as_number(indoor_minus_ambient_material_default),
    pm25_ambient_excess_f025 = as_number(indoor_minus_ambient_f025),
    pm25_ambient_excess_f050 = as_number(indoor_minus_ambient_f050),
    pm25_ambient_excess_f100 = as_number(indoor_minus_ambient_f100)
  ) %>%
  filter(
    !is.na(fcn_id),
    fcn_id != "",
    !is.na(timepoint),
    !is.na(study_arm_overall)
  ) %>%
  group_by(fcn_id, timepoint) %>%
  summarise(
    study_arm_overall = as.character(first_nonmissing(study_arm_overall)),
    collection_date_min = if (all(is.na(collection_date_min))) as.Date(NA) else min(collection_date_min, na.rm = TRUE),
    collection_date_max = if (all(is.na(collection_date_max))) as.Date(NA) else max(collection_date_max, na.rm = TRUE),
    n_windows = sum(n_windows, na.rm = TRUE),
    n_monitor_files = sum(n_monitor_files, na.rm = TRUE),
    pm25_n_observations = sum(pm25_n_observations, na.rm = TRUE),
    mean_ambient_coverage_prop = mean(mean_ambient_coverage_prop, na.rm = TRUE),
    across(starts_with("pm25_ambient_excess_"), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    across(starts_with("pm25_ambient_excess_"), ~ ifelse(is.nan(.x), NA_real_, .x)),
    mean_ambient_coverage_prop = ifelse(
      is.nan(mean_ambient_coverage_prop),
      NA_real_,
      mean_ambient_coverage_prop
    )
  )

pm_adjusted_source_audit <- pm_adjusted_source %>%
  mutate(
    pm_data_source_for_reviewed_rdid = "ambient_adjusted_household_timepoint",
    generating_script = file.path(
      project_root,
      "5_analysis",
      "6_PM_analysis",
      "3_pm25_ambient_adjusted_analysis_20260805_2213.R"
    ),
    reviewed_pm_outcome_primary = "pm25_ambient_excess_default",
    reviewed_pm_outcome_primary_definition = paste(
      "time-weighted household mean indoor PM2.5 minus 0.75 times",
      "concurrent ambient PM2.5; generated by",
      "3_pm25_ambient_adjusted_analysis_20260805_2213.R"
    ),
    reviewed_pm_outcome_sensitivity = paste(
      c("pm25_ambient_excess_f025", "pm25_ambient_excess_f050",
        "pm25_ambient_excess_f100"),
      collapse = "; "
    ),
    reviewed_rdid_uses_raw_indoor_pm = FALSE
  )

safe_write_reviewed_csv(
  pm_adjusted_source_audit,
  "table_rDiD_pm25_data_source.csv",
  subfolder = "qa"
)

pm_household_counts <- pm_household %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_households = n_distinct(fcn_id),
    n_windows = sum(n_windows, na.rm = TRUE),
    n_monitor_files = sum(n_monitor_files, na.rm = TRUE),
    n_pm_observations = sum(pm25_n_observations, na.rm = TRUE),
    mean_ambient_coverage_prop = mean(mean_ambient_coverage_prop, na.rm = TRUE),
    n_nonmissing_primary_adjusted_pm = sum(!is.na(pm25_ambient_excess_default)),
    min_collection_date = min(collection_date_min, na.rm = TRUE),
    max_collection_date = max(collection_date_max, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    mean_ambient_coverage_prop = ifelse(
      is.nan(mean_ambient_coverage_prop),
      NA_real_,
      mean_ambient_coverage_prop
    )
  ) %>%
  arrange(timepoint, study_arm_overall)

safe_write_reviewed_csv(
  pm_household_counts,
  "table_rDiD_pm25_household_counts.csv",
  subfolder = "qa"
)

pm_missing_timepoint_counts <- pm_adjusted_raw %>%
  mutate(
    fcn_id = as.character(fcn_id),
    timepoint = str_squish(str_to_lower(as.character(timepoint))),
    study_arm_overall = str_squish(str_to_lower(as.character(study_arm_overall)))
  ) %>%
  filter(
    !is.na(fcn_id),
    fcn_id != "",
    !(timepoint %in% timepoint_levels),
    !is.na(indoor_minus_ambient_material_default)
  ) %>%
  summarise(
    n_rows = n(),
    n_households = n_distinct(fcn_id),
    .groups = "drop"
  )

safe_write_reviewed_csv(
  pm_missing_timepoint_counts,
  "table_rDiD_pm25_missing_timepoints.csv",
  subfolder = "qa"
)

pm_outcomes <- tribble(
  ~outcome, ~outcome_label, ~domain, ~outcome_type, ~unit, ~outcome_source,
  "pm25_ambient_excess_default", "Ambient-adjusted indoor PM2.5, default 0.75 ambient factor", "PM2.5 exposure", "continuous", "ug/m3", "pm25_ambient_adjusted_household_timepoint",
  "pm25_ambient_excess_f025", "Ambient-adjusted indoor PM2.5, sensitivity 0.25 ambient factor", "PM2.5 exposure", "continuous", "ug/m3", "pm25_ambient_adjusted_household_timepoint_sensitivity",
  "pm25_ambient_excess_f050", "Ambient-adjusted indoor PM2.5, sensitivity 0.50 ambient factor", "PM2.5 exposure", "continuous", "ug/m3", "pm25_ambient_adjusted_household_timepoint_sensitivity",
  "pm25_ambient_excess_f100", "Ambient-adjusted indoor PM2.5, sensitivity 1.00 ambient factor", "PM2.5 exposure", "continuous", "ug/m3", "pm25_ambient_adjusted_household_timepoint_sensitivity"
)
################################################################################
# rDiD analysis panels
################################################################################

xvars <- c("hh_size", "hh_per_structure")

baseline_covars <- survey_model_data %>%
  filter(timepoint == "baseline") %>%
  transmute(
    fcn_id = as.character(fcn_id),
    A = if_else(study_arm_overall == "intervention", 1, 0, missing = NA_real_),
    hh_size = as_number(hh_size),
    hh_per_structure = as_number(hh_per_structure)
  ) %>%
  group_by(fcn_id) %>%
  summarise(
    A = as_number(first_nonmissing(A)),
    across(all_of(xvars), ~ as_number(first_nonmissing(.x))),
    .groups = "drop"
  )

covariate_missingness <- baseline_covars %>%
  summarise(
    across(all_of(c("A", xvars)), ~ sum(is.na(.x)), .names = "n_missing_{.col}")
  ) %>%
  pivot_longer(everything(), names_to = "measure", values_to = "n_missing") %>%
  mutate(n_households = nrow(baseline_covars))

safe_write_reviewed_csv(
  covariate_missingness,
  "table_rDiD_covariate_missingness.csv",
  subfolder = "qa"
)

make_pm_panel_ids <- function(followup_timepoint, contrast_label) {
  baseline_pm <- pm_household %>%
    filter(timepoint == "baseline") %>%
    select(
      fcn_id,
      baseline_pm_arm = study_arm_overall,
      baseline_pm_date_min = collection_date_min,
      baseline_pm_date_max = collection_date_max,
      baseline_pm_monitor_files = n_monitor_files,
      baseline_pm_observations = pm25_n_observations
    )

  followup_pm <- pm_household %>%
    filter(timepoint == followup_timepoint) %>%
    select(
      fcn_id,
      followup_pm_arm = study_arm_overall,
      followup_pm_date_min = collection_date_min,
      followup_pm_date_max = collection_date_max,
      followup_pm_monitor_files = n_monitor_files,
      followup_pm_observations = pm25_n_observations
    )

  baseline_covars %>%
    mutate(
      baseline_survey_arm = case_when(
        A == 1 ~ "intervention",
        A == 0 ~ "comparison",
        TRUE ~ NA_character_
      )
    ) %>%
    inner_join(baseline_pm, by = "fcn_id") %>%
    inner_join(followup_pm, by = "fcn_id") %>%
    mutate(
      contrast = contrast_label,
      followup_timepoint = followup_timepoint
    ) %>%
    select(
      contrast, followup_timepoint, fcn_id, baseline_survey_arm,
      baseline_pm_arm, followup_pm_arm,
      baseline_pm_date_min, baseline_pm_date_max,
      followup_pm_date_min, followup_pm_date_max,
      baseline_pm_monitor_files, followup_pm_monitor_files,
      baseline_pm_observations, followup_pm_observations
    )
}

pm_panel_id_audit <- bind_rows(
  make_pm_panel_ids("midline", "primary_baseline_midline"),
  make_pm_panel_ids("endline", "secondary_baseline_endline")
)

pm_panel_counts <- pm_panel_id_audit %>%
  group_by(contrast, followup_timepoint, baseline_survey_arm, baseline_pm_arm, followup_pm_arm) %>%
  summarise(
    n_households = n_distinct(fcn_id),
    n_baseline_monitor_files = sum(baseline_pm_monitor_files, na.rm = TRUE),
    n_followup_monitor_files = sum(followup_pm_monitor_files, na.rm = TRUE),
    n_baseline_pm_observations = sum(baseline_pm_observations, na.rm = TRUE),
    n_followup_pm_observations = sum(followup_pm_observations, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(contrast, baseline_survey_arm, baseline_pm_arm, followup_pm_arm)

safe_write_reviewed_csv(
  pm_panel_counts,
  "table_rDiD_pm25_paired_counts.csv",
  subfolder = "qa"
)
safe_write_reviewed_csv(
  pm_panel_id_audit,
  "table_rDiD_pm25_paired_households.csv",
  subfolder = "qa"
)

make_outcome_panel <- function(outcome_data, outcome_name, followup_timepoint,
                               source_label) {
  baseline_y <- outcome_data %>%
    filter(timepoint == "baseline") %>%
    transmute(
      fcn_id = as.character(fcn_id),
      Z = as_number(.data[[outcome_name]])
    )

  followup_y <- outcome_data %>%
    filter(timepoint == followup_timepoint) %>%
    transmute(
      fcn_id = as.character(fcn_id),
      Y = as_number(.data[[outcome_name]])
    )

  baseline_covars %>%
    inner_join(baseline_y, by = "fcn_id") %>%
    inner_join(followup_y, by = "fcn_id") %>%
    filter(!is.na(A), !is.na(Z), !is.na(Y)) %>%
    mutate(outcome_source = source_label)
}

################################################################################
# Estimators
################################################################################

xgb_xfit <- function(X_tr, y_tr, X_te, objective,
                     depths = as.numeric(strsplit(Sys.getenv("RF105_XGB_DEPTHS", unset = "2"), ",")[[1]]), etas = as.numeric(strsplit(Sys.getenv("RF105_XGB_ETAS", unset = "0.05"), ",")[[1]]),
                     max_nrounds = as.integer(Sys.getenv("RF105_XGB_MAX_NROUNDS", unset = "100")), early_stopping_rounds = as.integer(Sys.getenv("RF105_XGB_EARLY_STOP", unset = "10")),
                     seed = 1) {
  keep <- !is.na(y_tr)
  X_tr <- X_tr[keep, , drop = FALSE]
  y_tr <- y_tr[keep]

  if (nrow(X_tr) < 5) {
    return(rep(mean(y_tr, na.rm = TRUE), nrow(X_te)))
  }

  if (objective == "binary:logistic" && length(unique(y_tr)) < 2) {
    return(rep(mean(y_tr, na.rm = TRUE), nrow(X_te)))
  }

  d <- xgb.DMatrix(X_tr, label = y_tr, missing = NA)
  metric <- if (objective == "reg:squarederror") "rmse" else "logloss"
  log_col <- paste0("test_", metric, "_mean")
  cv_nfold <- min(3, nrow(X_tr))
  best <- list(score = Inf, pars = NULL, nrounds = NULL)

  for (mxd in depths) {
    for (eta in etas) {
      pars <- list(
        objective = objective,
        max_depth = mxd,
        eta = eta,
        nthread = as.integer(Sys.getenv("RF105_XGB_NTHREAD", unset = "2")),
        verbosity = 0
      )

      set.seed(seed)
      cv <- tryCatch(
        xgb.cv(
          params = pars,
          data = d,
          nrounds = max_nrounds,
          nfold = cv_nfold,
          early_stopping_rounds = early_stopping_rounds,
          verbose = 0,
          metrics = metric
        ),
        error = function(e) NULL
      )

      if (is.null(cv) || !(log_col %in% names(cv$evaluation_log))) {
        next
      }

      i <- which.min(cv$evaluation_log[[log_col]])
      score <- cv$evaluation_log[[log_col]][i]
      if (!is.na(score) && score < best$score) {
        best <- list(score = score, pars = pars, nrounds = i)
      }
    }
  }

  if (is.null(best$pars) || is.null(best$nrounds)) {
    return(rep(mean(y_tr, na.rm = TRUE), nrow(X_te)))
  }

  mod <- xgb.train(
    params = best$pars,
    data = d,
    nrounds = best$nrounds,
    verbose = 0
  )
  predict(mod, xgb.DMatrix(X_te, missing = NA))
}

dml_drdid_reverse_xgb <- function(dat, x_vars, K = 5, seed = 1) {
  dat <- dat %>%
    filter(!is.na(Z), !is.na(Y), !is.na(A))

  n <- nrow(dat)
  arm_counts <- table(dat$A)

  if (n < 10 || length(arm_counts) < 2 || min(arm_counts) < 2) {
    return(list(
      estimate = NA_real_, se = NA_real_, conf.low = NA_real_,
      conf.high = NA_real_, p.value = NA_real_, n = n,
      note = "insufficient data for rDiD/XGBoost"
    ))
  }

  set.seed(seed)
  X <- data.matrix(dat[, x_vars, drop = FALSE])
  A <- as_number(dat$A)
  D <- as_number(dat$Y - dat$Z)
  K_eff <- min(K, n, as.integer(min(arm_counts)))
  folds <- sample(rep(seq_len(K_eff), length.out = n))

  m1_hat <- numeric(n)
  p_hat <- numeric(n)

  for (k in seq_len(K_eff)) {
    te <- which(folds == k)
    tr <- which(folds != k)
    i_tr <- tr[A[tr] == 1]

    m1_hat[te] <- xgb_xfit(
      X[i_tr, , drop = FALSE],
      D[i_tr],
      X[te, , drop = FALSE],
      "reg:squarederror",
      seed = seed + k
    )

    p_hat[te] <- xgb_xfit(
      X[tr, , drop = FALSE],
      A[tr],
      X[te, , drop = FALSE],
      "binary:logistic",
      seed = seed + k
    )
  }

  p_hat <- pmin(pmax(p_hat, 0.01), 0.99)

  pi0 <- mean(1 - A)
  psi_vec <- (A - p_hat) / p_hat * (D - m1_hat) / pi0
  psi <- mean(psi_vec)
  phi <- psi_vec - ((1 - A) / pi0) * psi
  se <- sd(phi) / sqrt(n)
  conf <- psi + c(-1, 1) * qnorm(0.975) * se
  p_value <- ifelse(is.na(se) || se == 0, NA_real_, 2 * pnorm(-abs(psi / se)))

  list(
    estimate = psi,
    se = se,
    conf.low = conf[[1]],
    conf.high = conf[[2]],
    p.value = p_value,
    n = n,
    note = "rDiD DML-DR estimator with cross-fit XGBoost nuisance models"
  )
}

impute_xvars_for_glm <- function(dat, x_vars) {
  dat %>%
    mutate(across(all_of(x_vars), ~ {
      x <- as_number(.x)
      fill <- mean(x, na.rm = TRUE)
      if (is.na(fill) || is.nan(fill)) {
        fill <- 0
      }
      ifelse(is.na(x), fill, x)
    }))
}

drdid_reverse_glm <- function(dat, x_vars) {
  dat <- dat %>%
    filter(!is.na(Z), !is.na(Y), !is.na(A)) %>%
    impute_xvars_for_glm(x_vars)

  n <- nrow(dat)
  arm_counts <- table(dat$A)

  if (n < 10 || length(arm_counts) < 2 || min(arm_counts) < 2) {
    return(list(
      estimate = NA_real_, se = NA_real_, conf.low = NA_real_,
      conf.high = NA_real_, p.value = NA_real_, n = n,
      note = "insufficient data for rDiD/GLM"
    ))
  }

  X <- data.matrix(dat[, x_vars, drop = FALSE])
  A <- as_number(dat$A)
  D <- as_number(dat$Y - dat$Z)

  m1_fit <- tryCatch(
    glm(D ~ ., data = data.frame(D = D, X)[A == 1, , drop = FALSE],
        family = gaussian()),
    error = function(e) NULL
  )
  p_fit <- tryCatch(
    glm(A ~ ., data = data.frame(A = A, X), family = binomial()),
    error = function(e) NULL
  )

  if (is.null(m1_fit) || is.null(p_fit)) {
    return(list(
      estimate = NA_real_, se = NA_real_, conf.low = NA_real_,
      conf.high = NA_real_, p.value = NA_real_, n = n,
      note = "rDiD/GLM nuisance model failed"
    ))
  }

  m1 <- predict(m1_fit, newdata = data.frame(X), type = "response")
  p_hat <- pmin(pmax(predict(p_fit, newdata = data.frame(X), type = "response"),
                     0.01), 0.99)

  pi0 <- mean(1 - A)
  psi_vec <- (A - p_hat) / p_hat * (D - m1) / pi0
  psi <- mean(psi_vec)
  phi <- psi_vec - ((1 - A) / pi0) * psi
  se <- sd(phi) / sqrt(n)
  conf <- psi + c(-1, 1) * qnorm(0.975) * se
  p_value <- ifelse(is.na(se) || se == 0, NA_real_, 2 * pnorm(-abs(psi / se)))

  list(
    estimate = psi,
    se = se,
    conf.low = conf[[1]],
    conf.high = conf[[2]],
    p.value = p_value,
    n = n,
    note = "rDiD estimator with GLM nuisance models"
  )
}

format_result_row <- function(res, outcome_info, contrast, followup_timepoint,
                              estimator, panel) {
  scale_factor <- if (outcome_info$outcome_type == "binary") 100 else 1

  tibble(
    contrast = contrast,
    population = case_when(
      contrast == "primary_baseline_midline" ~
        "households_with_baseline_and_midline_outcome_data",
      contrast == "secondary_baseline_endline" ~
        "households_with_baseline_and_endline_outcome_data",
      TRUE ~ NA_character_
    ),
    followup_timepoint = followup_timepoint,
    estimator = estimator,
    outcome = outcome_info$outcome,
    outcome_label = outcome_info$outcome_label,
    domain = outcome_info$domain,
    outcome_type = outcome_info$outcome_type,
    unit = outcome_info$unit,
    outcome_source = outcome_info$outcome_source,
    estimate = scale_factor * res$estimate,
    se = scale_factor * res$se,
    conf.low = scale_factor * res$conf.low,
    conf.high = scale_factor * res$conf.high,
    p.value = res$p.value,
    statistically_significant = estimate_significance(
      res$estimate, res$conf.low, res$conf.high, res$p.value
    ),
    estimate_ci = format_est_ci(
      scale_factor * res$estimate,
      scale_factor * res$conf.low,
      scale_factor * res$conf.high
    ),
    sample_size = res$n,
    n_households = res$n,
    n_intervention = sum(panel$A == 1, na.rm = TRUE),
    n_comparison = sum(panel$A == 0, na.rm = TRUE),
    note = res$note
  )
}

run_rdid_contrast <- function(outcome_data, outcome_defs, followup_timepoint,
                              contrast, source_label) {
  map_dfr(seq_len(nrow(outcome_defs)), function(i) {
    outcome_info <- outcome_defs[i, ]
    message("Running rDiD ", contrast, ": ", outcome_info$outcome)
    panel <- make_outcome_panel(
      outcome_data = outcome_data,
      outcome_name = outcome_info$outcome,
      followup_timepoint = followup_timepoint,
      source_label = source_label
    )

    res_xgb <- dml_drdid_reverse_xgb(panel, xvars)
    res_glm <- drdid_reverse_glm(panel, xvars)

    bind_rows(
      format_result_row(
        res_xgb, outcome_info, contrast, followup_timepoint,
        "rDID_XGBoost", panel
      ),
      format_result_row(
        res_glm, outcome_info, contrast, followup_timepoint,
        "rDID_GLM_sensitivity", panel
      )
    )
  })
}

################################################################################
# Run primary and secondary analyses
################################################################################

survey_results <- bind_rows(
  run_rdid_contrast(
    survey_model_data,
    survey_outcomes,
    "midline",
    "primary_baseline_midline",
    "survey_clean_final"
  ),
  run_rdid_contrast(
    survey_model_data,
    survey_outcomes,
    "endline",
    "secondary_baseline_endline",
    "survey_clean_final"
  )
)

pm_results <- bind_rows(
  run_rdid_contrast(
    pm_household,
    pm_outcomes,
    "midline",
    "primary_baseline_midline",
    "pm25_indoor_household_mean"
  ),
  run_rdid_contrast(
    pm_household,
    pm_outcomes,
    "endline",
    "secondary_baseline_endline",
    "pm25_indoor_household_mean"
  )
)

rdid_results_all <- bind_rows(survey_results, pm_results) %>%
  arrange(contrast, domain, outcome, estimator)

rdid_xgboost_results <- rdid_results_all %>%
  filter(estimator == "rDID_XGBoost")

rdid_glm_results <- rdid_results_all %>%
  filter(estimator == "rDID_GLM_sensitivity")

safe_write_reviewed_csv(
  rdid_xgboost_results,
  "table_rDiD_xgboost_all_results.csv"
)
safe_write_reviewed_csv(
  rdid_glm_results,
  "table_rDiD_glm_sensitivity_results.csv"
)

safe_write_reviewed_csv(
  filter(rdid_xgboost_results, contrast == "primary_baseline_midline"),
  "table_rDiD_primary_xgboost_results.csv"
)
safe_write_reviewed_csv(
  filter(rdid_xgboost_results, contrast == "secondary_baseline_endline"),
  "table_rDiD_secondary_xgboost_results.csv"
)
rdid_xgboost_primary_secondary_comparison <- rdid_xgboost_results %>%
  filter(contrast %in% c(
    "primary_baseline_midline",
    "secondary_baseline_endline"
  )) %>%
  mutate(
    analysis = recode(
      contrast,
      primary_baseline_midline = "primary_baseline_midline",
      secondary_baseline_endline = "secondary_baseline_endline"
    )
  ) %>%
  select(
    outcome, outcome_label, domain, outcome_type, unit, outcome_source,
    analysis, estimate, se, conf.low, conf.high, p.value,
    statistically_significant, estimate_ci, sample_size, n_intervention,
    n_comparison, note
  ) %>%
  pivot_wider(
    names_from = analysis,
    values_from = c(
      estimate, se, conf.low, conf.high, p.value,
      statistically_significant, estimate_ci, sample_size, n_intervention,
      n_comparison, note
    ),
    names_glue = "{.value}_{analysis}"
  ) %>%
  mutate(
    estimate_difference_secondary_minus_primary =
      estimate_secondary_baseline_endline - estimate_primary_baseline_midline,
    absolute_estimate_difference = abs(estimate_difference_secondary_minus_primary),
    significance_comparison = case_when(
      is.na(statistically_significant_primary_baseline_midline) |
        is.na(statistically_significant_secondary_baseline_endline) ~
        "significance_unavailable",
      statistically_significant_primary_baseline_midline &
        statistically_significant_secondary_baseline_endline ~
        "significant_in_primary_and_secondary",
      statistically_significant_primary_baseline_midline &
        !statistically_significant_secondary_baseline_endline ~
        "significant_in_primary_only",
      !statistically_significant_primary_baseline_midline &
        statistically_significant_secondary_baseline_endline ~
        "significant_in_secondary_only",
      TRUE ~ "not_significant_in_either"
    ),
    direction_comparison = case_when(
      is.na(estimate_primary_baseline_midline) |
        is.na(estimate_secondary_baseline_endline) ~ "direction_unavailable",
      sign(estimate_primary_baseline_midline) ==
        sign(estimate_secondary_baseline_endline) ~ "same_direction",
      estimate_primary_baseline_midline == 0 |
        estimate_secondary_baseline_endline == 0 ~ "one_estimate_zero",
      TRUE ~ "opposite_direction"
    ),
    comparison_note = paste(
      "Estimate difference is descriptive only; the workflow does not estimate",
      "the covariance needed for a formal test comparing the primary and",
      "secondary rDiD estimates."
    )
  ) %>%
  arrange(domain, outcome_type, outcome_label)

safe_write_reviewed_csv(
  rdid_xgboost_primary_secondary_comparison,
  "table_rDiD_primary_secondary_comparison.csv"
)

safe_write_reviewed_csv(
  filter(rdid_glm_results, contrast == "primary_baseline_midline"),
  "table_rDiD_primary_glm_sensitivity.csv"
)
safe_write_reviewed_csv(
  filter(rdid_glm_results, contrast == "secondary_baseline_endline"),
  "table_rDiD_secondary_glm_sensitivity.csv"
)

rdid_sample_counts <- rdid_xgboost_results %>%
  select(
    contrast, population, followup_timepoint, outcome, outcome_label, domain,
    outcome_source, sample_size, n_households, n_intervention, n_comparison, note
  )

safe_write_reviewed_csv(
  rdid_sample_counts,
  "table_rDiD_outcome_sample_counts.csv",
  subfolder = "qa"
)

format_pm_count_line <- function(counts_df, timepoint_value, arm_value) {
  row <- counts_df %>%
    filter(timepoint == timepoint_value, study_arm_overall == arm_value) %>%
    slice(1)
  if (nrow(row) == 0) {
    return(paste0("- ", timepoint_value, " ", arm_value, ": no rows found"))
  }
  paste0(
    "- ", timepoint_value, " ", arm_value, ": ",
    row$n_households, " households, ", row$n_windows, " ambient-matched windows, ",
    row$n_monitor_files, " monitor files, ", row$n_pm_observations,
    " indoor PM rows; mean ambient coverage ",
    round(100 * row$mean_ambient_coverage_prop, 1), "% (",
    row$min_collection_date, " to ", row$max_collection_date, ")."
  )
}

format_pm_panel_line <- function(panel_df, contrast_value, arm_value) {
  row <- panel_df %>%
    filter(contrast == contrast_value, baseline_survey_arm == arm_value) %>%
    slice(1)
  if (nrow(row) == 0) {
    return(paste0("- ", contrast_value, " ", arm_value, ": no paired PM households found"))
  }
  paste0(
    "- ", contrast_value, " ", arm_value, ": ", row$n_households,
    " paired households, ", row$n_baseline_monitor_files,
    " baseline monitor files, ", row$n_followup_monitor_files,
    " follow-up monitor files."
  )
}

format_pm_result_line <- function(results_df, contrast_value) {
  row <- results_df %>%
    filter(outcome == "pm25_ambient_excess_default", contrast == contrast_value) %>%
    slice(1)
  if (nrow(row) == 0) {
    return(paste0("- ", contrast_value, ": no adjusted PM2.5 XGBoost result found"))
  }
  significance_text <- ifelse(
    isTRUE(row$statistically_significant),
    "statistically significant",
    "not statistically significant"
  )
  paste0(
    "- ", contrast_value, ": ambient-adjusted estimate ", round(row$estimate, 1),
    " ug/m3 (95% CI ", round(row$conf.low, 1), " to ",
    round(row$conf.high, 1), "; p=", signif(row$p.value, 3),
    "), ", significance_text, "; n=", row$sample_size,
    " (", row$n_intervention, " intervention, ", row$n_comparison,
    " comparison)."
  )
}

pm_missing_timepoint_note <- if (nrow(pm_missing_timepoint_counts) == 0) {
  "No nonmissing ambient-adjusted household PM2.5 rows used by the rDiD preparation have missing or invalid timepoint."
} else {
  paste0(
    "Rows with missing or invalid timepoint remain; see `table_rDiD_pm25_missing_timepoints.csv` for details. Total rows: ",
    sum(pm_missing_timepoint_counts$n_rows, na.rm = TRUE), "."
  )
}

pm_audit_summary_file <- file.path(
  dir_tables_reviewed,
  "qa",
  "table_rDiD_pm25_panel_audit.md"
)
writeLines(
  c(
    "# Ambient-adjusted PM2.5 rDiD Panel Audit",
    "",
    paste0("Generated: ", Sys.time()),
    "",
    "## Adjusted PM2.5 Data Source",
    "",
    paste(
      "The reviewed PM2.5 rDiD/XGBoost analysis uses the ambient-adjusted",
      "household-timepoint file generated by",
      "5_analysis_RF105/6_PM_analysis/3_pm25_ambient_adjusted_analysis_20260805_2213.R.",
      "The primary PM outcome is household indoor PM2.5 minus 0.75 times",
      "concurrent ambient PM2.5; 0.25, 0.50, and 1.00 ambient factors are",
      "included as sensitivity outcomes."
    ),
    paste0("Ambient-adjusted input file: ", pm_adjusted_file),
    "",
    pm_missing_timepoint_note,
    "",
    "## Ambient-adjusted PM2.5 Counts",
    "",
    format_pm_count_line(pm_household_counts, "baseline", "comparison"),
    format_pm_count_line(pm_household_counts, "baseline", "intervention"),
    format_pm_count_line(pm_household_counts, "midline", "comparison"),
    format_pm_count_line(pm_household_counts, "midline", "intervention"),
    format_pm_count_line(pm_household_counts, "endline", "comparison"),
    format_pm_count_line(pm_household_counts, "endline", "intervention"),
    "",
    "## Paired PM2.5 rDiD Panels",
    "",
    format_pm_panel_line(pm_panel_counts, "primary_baseline_midline", "comparison"),
    format_pm_panel_line(pm_panel_counts, "primary_baseline_midline", "intervention"),
    format_pm_panel_line(pm_panel_counts, "secondary_baseline_endline", "comparison"),
    format_pm_panel_line(pm_panel_counts, "secondary_baseline_endline", "intervention"),
    "",
    "## PM2.5 rDiD/XGBoost Results",
    "",
    format_pm_result_line(rdid_xgboost_results, "primary_baseline_midline"),
    format_pm_result_line(rdid_xgboost_results, "secondary_baseline_endline"),
    "",
    "Detailed rows are in `table_rDiD_pm25_data_source.csv`, `table_rDiD_pm25_household_counts.csv`, `table_rDiD_pm25_paired_counts.csv`, `table_rDiD_pm25_paired_households.csv`, and `table_rDiD_xgboost_all_results.csv`."
  ),
  pm_audit_summary_file
)
message("Wrote QA summary: ", pm_audit_summary_file)

################################################################################
# Figures
################################################################################

make_rdid_plot <- function(results, contrast_name, outcome_type_filter) {
  plot_data <- results %>%
    filter(
      estimator == "rDID_XGBoost",
      contrast == contrast_name,
      outcome_type == outcome_type_filter,
      !is.na(estimate),
      !is.na(conf.low),
      !is.na(conf.high)
    ) %>%
    mutate(
      outcome_label = fct_reorder(outcome_label, estimate),
      significant_label = if_else(
        coalesce(statistically_significant, FALSE),
        "p < 0.05",
        "p >= 0.05 or unavailable"
      )
    )

  if (nrow(plot_data) == 0) {
    return(NULL)
  }

  ggplot(
    plot_data,
    aes(x = estimate, y = outcome_label, xmin = conf.low, xmax = conf.high)
  ) +
    geom_vline(xintercept = 0, color = "grey55", linewidth = 0.4) +
    geom_errorbar(aes(color = significant_label), orientation = "y", width = 0.18, linewidth = 0.55) +
    geom_point(aes(color = significant_label), size = 1.9) +
    facet_grid(domain ~ unit, scales = "free", space = "free_y") +
    scale_color_manual(
      values = c("p < 0.05" = "#D55E00", "p >= 0.05 or unavailable" = "#4E79A7")
    ) +
    labs(
      x = "rDiD/XGBoost estimate with 95% CI",
      y = NULL,
      color = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text.y = element_text(angle = 0)
    )
}

plot_specs <- tribble(
  ~contrast, ~outcome_type, ~filename, ~width, ~height,
  "primary_baseline_midline", "binary", "fig_rDiD_primary_binary_estimates.png", 11, 16,
  "primary_baseline_midline", "continuous", "fig_rDiD_primary_continuous_estimates.png", 9, 4,
  "secondary_baseline_endline", "binary", "fig_rDiD_secondary_binary_estimates.png", 11, 16,
  "secondary_baseline_endline", "continuous", "fig_rDiD_secondary_continuous_estimates.png", 9, 4
)

pwalk(plot_specs, function(contrast, outcome_type, filename, width, height) {
  fig <- make_rdid_plot(rdid_results_all, contrast, outcome_type)
  if (!is.null(fig)) {
    save_reviewed_plot(fig, filename, width = width, height = height)
  }
})

################################################################################
# Compare new rDiD/XGBoost results to previous reviewed outputs when available
################################################################################

previous_results <- tibble(
  outcome = character(),
  contrast = character(),
  previous_estimator = character(),
  previous_population = character(),
  previous_estimate = numeric(),
  previous_conf.low = numeric(),
  previous_conf.high = numeric(),
  previous_p.value = numeric(),
  previous_significant = logical()
)

significance_change_audit <- rdid_xgboost_results %>%
  select(
    outcome, outcome_label, contrast, domain, unit,
    new_estimate = estimate,
    new_conf.low = conf.low,
    new_conf.high = conf.high,
    new_p.value = p.value,
    new_significant = statistically_significant
  ) %>%
  left_join(previous_results, by = c("outcome", "contrast")) %>%
  mutate(
    comparison_status = case_when(
      is.na(previous_estimator) ~ "no_matching_previous_result",
      is.na(previous_significant) | is.na(new_significant) ~ "significance_unknown",
      previous_significant == new_significant ~ "significance_unchanged",
      previous_significant & !new_significant ~ "no_longer_statistically_significant",
      !previous_significant & new_significant ~ "became_statistically_significant",
      TRUE ~ "significance_unknown"
    )
  ) %>%
  arrange(contrast, comparison_status, domain, outcome)

safe_write_reviewed_csv(
  significance_change_audit,
  "table_rDiD_significance_change_audit.csv"
)

changed_significance <- significance_change_audit %>%
  filter(comparison_status %in% c(
    "no_longer_statistically_significant",
    "became_statistically_significant"
  ))

summary_lines <- c(
  "# RF105 rDiD/XGBoost Previous-vs-Current Results Summary",
  "",
  paste0("Generated: ", Sys.time()),
  "",
  "## What Changed",
  "",
  "The reviewed workflow now uses reverse difference-in-differences (rDiD) with cross-fit XGBoost nuisance models for all eligible impact outcomes.",
  "",
  "Primary analysis is baseline-to-midline among households with both baseline and midline outcome data. Secondary analysis is baseline-to-endline among households with both baseline and endline outcome data.",
  "",
  "## Comparison Source",
  "",
  if (nrow(previous_results) == 0) {
    "No previous result files are read by this self-contained workflow; automated significance comparison is therefore not performed."
  } else {
    paste0(
      "Automated comparisons used previous result rows for ",
      n_distinct(previous_results$outcome), " matched outcomes where available."
    )
  },
  "",
  "## Statistical Significance",
  "",
  paste0(
    "Matched rows with unchanged significance: ",
    sum(significance_change_audit$comparison_status == "significance_unchanged", na.rm = TRUE)
  ),
  paste0(
    "Rows that became statistically significant: ",
    sum(significance_change_audit$comparison_status == "became_statistically_significant", na.rm = TRUE)
  ),
  paste0(
    "Rows that were no longer statistically significant: ",
    sum(significance_change_audit$comparison_status == "no_longer_statistically_significant", na.rm = TRUE)
  ),
  paste0(
    "Rows without a matching previous result: ",
    sum(significance_change_audit$comparison_status == "no_matching_previous_result", na.rm = TRUE)
  ),
  "",
  "See `table_rDiD_significance_change_audit.csv` for current estimates, confidence intervals, p-values, and comparison-status flags.",
  "",
  "## Rows With Changed Statistical Significance",
  ""
)

if (nrow(changed_significance) == 0) {
  summary_lines <- c(summary_lines, "No matched rows changed statistical significance.")
} else {
  changed_lines <- changed_significance %>%
    mutate(
      line = paste0(
        "- ", contrast, ": ", outcome_label, " (", comparison_status, ")"
      )
    ) %>%
    pull(line)
  summary_lines <- c(summary_lines, changed_lines)
}

summary_file <- file.path(
  dir_tables_reviewed,
  "table_rDiD_results_comparison_summary.md"
)
writeLines(summary_lines, summary_file)
message("Wrote summary document: ", summary_file)

################################################################################
# Consolidated rDiD post-processing section
#
# This section refreshes split result tables, comparison tables, QA counts,
# rDiD figures, and significance-change summaries from the all-results files
# produced above. Keeping it in this file ensures all rDiD-related code lives
# in 4_rdid_xgboost_20260805_2213.R.
################################################################################
local({
################################################################################
# RF105 reviewed rDiD/XGBoost result post-processing
#
# Purpose:
#   Rebuild downstream rDiD result tables and figures from the saved all-results
#   CSV files written by 4_rdid_xgboost_20260805_2213.R. This script does not refit
#   models. It is useful after adding supplemental outcomes because the XGBoost fitting
#   step is slow, while comparison tables, split tables, QA counts, and figures
#   can be regenerated quickly from the completed all-results files.
#
# Inputs:
#   7_tables/RF105_reviewed_YYYYMMDD/table_rDiD_xgboost_all_results.csv
#   7_tables/RF105_reviewed_YYYYMMDD/table_rDiD_glm_sensitivity_results.csv
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/table_rDiD_*_results.csv
#   7_tables/RF105_reviewed_YYYYMMDD/table_rDiD_*_glm_sensitivity.csv
#   7_tables/RF105_reviewed_YYYYMMDD/table_rDiD_primary_secondary_comparison.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/table_rDiD_outcome_sample_counts.csv
#   6_figures/RF105_reviewed_YYYYMMDD/fig_rDiD_*_estimates.png
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
config_file <- file.path(script_dir, "0_RF105_config_20260805_2213.R")
if (!file.exists(config_file)) {
  config_file <- file.path("5_analysis_RF105", "reviewed", "0_RF105_config_20260805_2213.R")
}
source(config_file)

read_required_reviewed_csv <- function(filename, subfolder = NULL) {
  in_dir <- if (is.null(subfolder)) dir_tables_reviewed else file.path(dir_tables_reviewed, subfolder)
  in_file <- file.path(in_dir, filename)
  if (!file.exists(in_file)) {
    stop("Required rDiD results file not found: ", in_file, call. = FALSE)
  }
  readr::read_csv(in_file, show_col_types = FALSE)
}

estimate_significance <- function(estimate, conf_low, conf_high, p_value) {
  case_when(
    !is.na(p_value) ~ p_value < 0.05,
    !is.na(conf_low) & !is.na(conf_high) ~ conf_low > 0 | conf_high < 0,
    TRUE ~ NA
  )
}

ensure_columns <- function(df, columns) {
  for (column in columns) {
    if (!column %in% names(df)) {
      df[[column]] <- NA
    }
  }
  df
}

safe_write_reviewed_csv <- function(x, filename, subfolder = NULL) {
  tryCatch(
    write_reviewed_csv(x, filename, subfolder = subfolder),
    error = function(e) {
      fallback_filename <- str_replace(filename, "\\.csv$", paste0("_refreshed_", date_stamp, ".csv"))
      warning(
        "Could not overwrite ", filename, "; writing refreshed copy ",
        fallback_filename, ". Original error: ", conditionMessage(e),
        call. = FALSE
      )
      write_reviewed_csv(x, fallback_filename, subfolder = subfolder)
    }
  )
}

rdid_xgboost_results <- read_required_reviewed_csv("table_rDiD_xgboost_all_results.csv")
rdid_glm_results <- read_required_reviewed_csv("table_rDiD_glm_sensitivity_results.csv")

required_result_columns <- c(
  "contrast", "population", "followup_timepoint", "outcome", "outcome_label",
  "domain", "outcome_type", "unit", "outcome_source", "estimate", "se",
  "conf.low", "conf.high", "p.value", "statistically_significant",
  "estimate_ci", "sample_size", "n_households", "n_intervention",
  "n_comparison", "note"
)

missing_xgb_columns <- setdiff(required_result_columns, names(rdid_xgboost_results))
missing_glm_columns <- setdiff(required_result_columns, names(rdid_glm_results))
if (length(missing_xgb_columns) > 0) {
  stop("XGBoost all-results file is missing columns: ", paste(missing_xgb_columns, collapse = ", "), call. = FALSE)
}
if (length(missing_glm_columns) > 0) {
  stop("GLM sensitivity all-results file is missing columns: ", paste(missing_glm_columns, collapse = ", "), call. = FALSE)
}

safe_write_reviewed_csv(
  filter(rdid_xgboost_results, contrast == "primary_baseline_midline"),
  "table_rDiD_primary_xgboost_results.csv"
)
safe_write_reviewed_csv(
  filter(rdid_xgboost_results, contrast == "secondary_baseline_endline"),
  "table_rDiD_secondary_xgboost_results.csv"
)
safe_write_reviewed_csv(
  filter(rdid_glm_results, contrast == "primary_baseline_midline"),
  "table_rDiD_primary_glm_sensitivity.csv"
)
safe_write_reviewed_csv(
  filter(rdid_glm_results, contrast == "secondary_baseline_endline"),
  "table_rDiD_secondary_glm_sensitivity.csv"
)

rdid_xgboost_primary_secondary_comparison <- rdid_xgboost_results %>%
  filter(contrast %in% c("primary_baseline_midline", "secondary_baseline_endline")) %>%
  mutate(
    analysis = recode(
      contrast,
      primary_baseline_midline = "primary_baseline_midline",
      secondary_baseline_endline = "secondary_baseline_endline"
    )
  ) %>%
  select(
    outcome, outcome_label, domain, outcome_type, unit, outcome_source,
    analysis, estimate, se, conf.low, conf.high, p.value,
    statistically_significant, estimate_ci, sample_size, n_intervention,
    n_comparison, note
  ) %>%
  pivot_wider(
    names_from = analysis,
    values_from = c(
      estimate, se, conf.low, conf.high, p.value, statistically_significant,
      estimate_ci, sample_size, n_intervention, n_comparison, note
    ),
    names_glue = "{.value}_{analysis}"
  ) %>%
  ensure_columns(c(
    "estimate_primary_baseline_midline", "estimate_secondary_baseline_endline",
    "statistically_significant_primary_baseline_midline",
    "statistically_significant_secondary_baseline_endline"
  )) %>%
  mutate(
    estimate_difference_secondary_minus_primary =
      estimate_secondary_baseline_endline - estimate_primary_baseline_midline,
    absolute_estimate_difference = abs(estimate_difference_secondary_minus_primary),
    significance_comparison = case_when(
      is.na(statistically_significant_primary_baseline_midline) |
        is.na(statistically_significant_secondary_baseline_endline) ~ "significance_unavailable",
      statistically_significant_primary_baseline_midline &
        statistically_significant_secondary_baseline_endline ~ "significant_in_primary_and_secondary",
      statistically_significant_primary_baseline_midline &
        !statistically_significant_secondary_baseline_endline ~ "significant_in_primary_only",
      !statistically_significant_primary_baseline_midline &
        statistically_significant_secondary_baseline_endline ~ "significant_in_secondary_only",
      TRUE ~ "not_significant_in_either"
    ),
    direction_comparison = case_when(
      is.na(estimate_primary_baseline_midline) | is.na(estimate_secondary_baseline_endline) ~ "direction_unavailable",
      sign(estimate_primary_baseline_midline) == sign(estimate_secondary_baseline_endline) ~ "same_direction",
      estimate_primary_baseline_midline == 0 | estimate_secondary_baseline_endline == 0 ~ "one_estimate_zero",
      TRUE ~ "opposite_direction"
    ),
    comparison_note = paste(
      "Estimate difference is descriptive only; the workflow does not estimate",
      "the covariance needed for a formal test comparing the primary and",
      "secondary rDiD estimates."
    )
  ) %>%
  arrange(domain, outcome_type, outcome_label)

safe_write_reviewed_csv(
  rdid_xgboost_primary_secondary_comparison,
  "table_rDiD_primary_secondary_comparison.csv"
)

rdid_sample_counts <- rdid_xgboost_results %>%
  select(
    contrast, population, followup_timepoint, outcome, outcome_label, domain,
    outcome_source, sample_size, n_households, n_intervention, n_comparison, note
  )
safe_write_reviewed_csv(rdid_sample_counts, "table_rDiD_outcome_sample_counts.csv", subfolder = "qa")

make_rdid_plot <- function(results, contrast_name, outcome_type_filter) {
  plot_data <- results %>%
    filter(
      contrast == contrast_name,
      outcome_type == outcome_type_filter,
      !is.na(estimate), !is.na(conf.low), !is.na(conf.high)
    ) %>%
    mutate(
      outcome_label = forcats::fct_reorder(outcome_label, estimate),
      significant_label = if_else(
        coalesce(statistically_significant, FALSE),
        "p < 0.05", "p >= 0.05 or unavailable"
      )
    )

  if (nrow(plot_data) == 0) {
    return(NULL)
  }

  ggplot(plot_data, aes(x = estimate, y = outcome_label, xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 0, color = "grey55", linewidth = 0.4) +
    geom_errorbar(aes(color = significant_label), orientation = "y", width = 0.18, linewidth = 0.55) +
    geom_point(aes(color = significant_label), size = 1.9) +
    facet_grid(domain ~ unit, scales = "free", space = "free_y") +
    scale_color_manual(values = c("p < 0.05" = "#D55E00", "p >= 0.05 or unavailable" = "#4E79A7")) +
    labs(x = "rDiD/XGBoost estimate with 95% CI", y = NULL, color = NULL) +
    theme_bw(base_size = 10) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text.y = element_text(angle = 0)
    )
}

plot_specs <- tribble(
  ~contrast, ~outcome_type, ~filename, ~width, ~height,
  "primary_baseline_midline", "binary", "fig_rDiD_primary_binary_estimates.png", 11, 22,
  "primary_baseline_midline", "continuous", "fig_rDiD_primary_continuous_estimates.png", 9, 6,
  "secondary_baseline_endline", "binary", "fig_rDiD_secondary_binary_estimates.png", 11, 22,
  "secondary_baseline_endline", "continuous", "fig_rDiD_secondary_continuous_estimates.png", 9, 6
)

pwalk(plot_specs, function(contrast, outcome_type, filename, width, height) {
  fig <- make_rdid_plot(rdid_xgboost_results, contrast, outcome_type)
  if (!is.null(fig)) {
    save_reviewed_plot(fig, filename, width = width, height = height)
  }
})

previous_results <- tibble(
  outcome = character(),
  contrast = character(),
  previous_estimator = character(),
  previous_population = character(),
  previous_estimate = numeric(),
  previous_conf.low = numeric(),
  previous_conf.high = numeric(),
  previous_p.value = numeric(),
  previous_significant = logical()
)
significance_change_audit <- rdid_xgboost_results %>%
  select(
    outcome, outcome_label, contrast, domain, unit,
    new_estimate = estimate, new_conf.low = conf.low, new_conf.high = conf.high,
    new_p.value = p.value, new_significant = statistically_significant
  ) %>%
  left_join(previous_results, by = c("outcome", "contrast")) %>%
  mutate(
    comparison_status = case_when(
      is.na(previous_estimator) ~ "no_matching_previous_result",
      is.na(previous_significant) | is.na(new_significant) ~ "significance_unknown",
      previous_significant == new_significant ~ "significance_unchanged",
      previous_significant & !new_significant ~ "no_longer_statistically_significant",
      !previous_significant & new_significant ~ "became_statistically_significant",
      TRUE ~ "significance_unknown"
    )
  ) %>%
  arrange(contrast, comparison_status, domain, outcome)

safe_write_reviewed_csv(significance_change_audit, "table_rDiD_significance_change_audit.csv")

changed_significance <- significance_change_audit %>%
  filter(comparison_status %in% c("no_longer_statistically_significant", "became_statistically_significant"))

summary_lines <- c(
  "# RF105 rDiD/XGBoost Previous-vs-Current Results Summary",
  "",
  paste0("Generated: ", Sys.time()),
  "",
  "## What Changed",
  "",
  "The reviewed workflow uses reverse difference-in-differences (rDiD) with cross-fit XGBoost nuisance models for all eligible impact outcomes.",
  "",
  "Primary analysis is baseline-to-midline among households with both baseline and midline outcome data. Secondary analysis is baseline-to-endline among households with both baseline and endline outcome data.",
  "",
  "## Comparison Source",
  "",
  if (nrow(previous_results) == 0) {
    "No previous result files are read by this self-contained workflow; automated significance comparison is therefore not performed."
  } else {
    paste0("Automated comparisons used previous result rows for ", n_distinct(previous_results$outcome), " matched outcomes where available.")
  },
  "",
  "## Statistical Significance",
  "",
  paste0("Matched rows with unchanged significance: ", sum(significance_change_audit$comparison_status == "significance_unchanged", na.rm = TRUE)),
  paste0("Rows that became statistically significant: ", sum(significance_change_audit$comparison_status == "became_statistically_significant", na.rm = TRUE)),
  paste0("Rows that were no longer statistically significant: ", sum(significance_change_audit$comparison_status == "no_longer_statistically_significant", na.rm = TRUE)),
  paste0("Rows without a matching previous result: ", sum(significance_change_audit$comparison_status == "no_matching_previous_result", na.rm = TRUE)),
  "",
  "See `table_rDiD_significance_change_audit.csv` for current estimates, confidence intervals, p-values, and comparison-status flags.",
  "",
  "## Rows With Changed Statistical Significance",
  ""
)

if (nrow(changed_significance) == 0) {
  summary_lines <- c(summary_lines, "No matched rows changed statistical significance.")
} else {
  changed_lines <- changed_significance %>%
    mutate(line = paste0("- ", contrast, ": ", outcome_label, " (", comparison_status, ")")) %>%
    pull(line)
  summary_lines <- c(summary_lines, changed_lines)
}

summary_file <- file.path(dir_tables_reviewed, "table_rDiD_results_comparison_summary.md")
writeLines(summary_lines, summary_file)
message("Wrote summary document: ", summary_file)

message("RF105 reviewed rDiD/XGBoost result post-processing complete.")
})




