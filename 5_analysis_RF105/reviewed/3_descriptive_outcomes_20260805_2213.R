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
#   4_data/clean_final/survey_refugee_hh_members.rds
#   4_data/clean_final/survey_refugee_symptoms.rds
#   4_data/clean_final/pm25_pats_refugee_indoor.rds
#   4_data/clean_final/pm25_pats_refugee_ambient.rds
#   7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_stove_daily_dataset_100_80_5_20.csv
#   PM2.5 is prepared directly from the two clean_final RDS inputs above.
#
# Outputs:
#   Tables:  7_tables/RF105_reviewed_YYYYMMDD/descriptive_*, tab1_*, pm25_*,
#            fuel_*, stove_*, health_*, supplemental_survey_*, manuscript_style_*,
#            and canonical PM2.5 CSV files.
#   Figures: 6_figures/RF105_reviewed_YYYYMMDD/descriptive_*, pm25_*, fuel_*,
#            stove_*, health_*, supplemental_survey_*, manuscript-style, and
#            PM2.5 descriptive figures.
#   QA:      7_tables/RF105_reviewed_YYYYMMDD/qa/* descriptive QA files.
#
# Notes:
#   - Uses one deduplicated household record per fcn_id-timepoint, matching the
#     reviewed RF105 helper function make_analysis_population().
#   - FCS follows the embedded calculation:
#     the embedded FCS/HDDS code in this script
#   - HDDS uses clean_final baseline-compatible weekly food-frequency variables,
#     with the embedded past-24-hour mapping as a fallback for earlier survey waves.
#   - Time-use outputs include self-reported categorical changes and standardized
#     fuel-time profiles combining survey and stove-use monitor data.
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

as_number <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

collapse_collection_years <- function(x) {
  years <- sort(unique(as.integer(x[!is.na(x)])))
  if (length(years) == 0) {
    return(NA_character_)
  }
  paste(years, collapse = ", ")
}

month_axis_label <- function(base_label, collection_years) {
  if (length(collection_years) == 0 || is.na(collection_years) || !nzchar(collection_years)) {
    return(base_label)
  }
  paste0(base_label, " (data collected ", collection_years, ")")
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

add_all_arms_rows <- function(df) {
  df_arm <- df %>%
    mutate(study_arm_overall = as.character(study_arm_overall))

  df_all_arms <- df_arm %>%
    filter(!is.na(study_arm_overall), study_arm_overall %in% arm_levels) %>%
    mutate(study_arm_overall = "all_arms")

  bind_rows(df_arm, df_all_arms)
}

arrange_timepoint_arm <- function(.data, ...) {
  .data %>%
    arrange(
      ...,
      factor(as.character(timepoint), levels = timepoint_levels),
      factor(as.character(study_arm_overall), levels = c(arm_levels, "all_arms"))
    )
}

prop_from_counts <- function(numerator, denominator) {
  if_else(
    !is.na(numerator) & !is.na(denominator) & denominator > 0,
    numerator / denominator,
    NA_real_
  )
}

prop_se_from_counts <- function(numerator, denominator) {
  proportion <- prop_from_counts(numerator, denominator)
  if_else(
    !is.na(proportion) & !is.na(denominator) & denominator > 0,
    sqrt(proportion * (1 - proportion) / denominator),
    NA_real_
  )
}

prop_ci_lower_from_counts <- function(numerator, denominator, scale = 100) {
  proportion <- prop_from_counts(numerator, denominator)
  se <- prop_se_from_counts(numerator, denominator)
  if_else(
    !is.na(proportion) & !is.na(se),
    pmax(0, scale * (proportion - qnorm(0.975) * se)),
    NA_real_
  )
}

prop_ci_upper_from_counts <- function(numerator, denominator, scale = 100) {
  proportion <- prop_from_counts(numerator, denominator)
  se <- prop_se_from_counts(numerator, denominator)
  if_else(
    !is.na(proportion) & !is.na(se),
    pmin(scale, scale * (proportion + qnorm(0.975) * se)),
    NA_real_
  )
}

add_binary_denominator_summaries <- function(.data,
                                             numerator_col = "n_yes",
                                             nonmissing_col = "n_nonmissing",
                                             total_col = "n_total") {
  numerator <- .data[[numerator_col]]
  n_nonmissing <- .data[[nonmissing_col]]
  n_total <- .data[[total_col]]

  .data %>%
    mutate(
      proportion_nonmissing = prop_from_counts(numerator, n_nonmissing),
      percent_nonmissing = 100 * proportion_nonmissing,
      se_nonmissing = prop_se_from_counts(numerator, n_nonmissing),
      ci_lower_nonmissing = prop_ci_lower_from_counts(numerator, n_nonmissing),
      ci_upper_nonmissing = prop_ci_upper_from_counts(numerator, n_nonmissing),
      proportion_total = prop_from_counts(numerator, n_total),
      percent_total = 100 * proportion_total,
      se_total = prop_se_from_counts(numerator, n_total),
      ci_lower_total = prop_ci_lower_from_counts(numerator, n_total),
      ci_upper_total = prop_ci_upper_from_counts(numerator, n_total),
      proportion = proportion_nonmissing,
      percent = percent_nonmissing,
      se = se_nonmissing,
      ci_lower = ci_lower_nonmissing,
      ci_upper = ci_upper_nonmissing
    )
}

calc_prop_summary <- function(df, value_var = "value") {
  add_all_arms_rows(df) %>%
    group_by(timepoint, study_arm_overall, outcome_group, outcome_name,
             outcome_label, source_variable, unit, population) %>%
    summarise(
      n_total = n(),
      n_nonmissing = sum(!is.na(.data[[value_var]])),
      n_yes = sum(.data[[value_var]] == 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    add_binary_denominator_summaries(numerator_col = "n_yes") %>%
    arrange_timepoint_arm(outcome_group, outcome_name)
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
  add_all_arms_rows(df) %>%
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
    arrange_timepoint_arm(outcome_group, outcome_name)
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
    add_all_arms_rows() %>%
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
    arrange_timepoint_arm(outcome_group, outcome_name, category_value)
}

write_plot_if_data <- function(plot_data, plot, filename, width = 10, height = 6) {
  if (nrow(plot_data) > 0) {
    save_reviewed_plot(plot, filename, width = width, height = height)
  }
}

arm_colors <- c(comparison = "#3B6EA8", intervention = "#C94C4C", all_arms = "#6F6F6F")
change_colors <- c(more = "#2F8F5B", same = "#9AA0A6", less = "#B6463A")

survey_raw <- readr::read_rds(file_survey_refugee_household) %>%
  add_rf105_aliases()

analysis_population <- make_analysis_population(survey_raw, id_var = "fcn_id")
survey <- analysis_population$all_deduplicated

write_reviewed_csv(
  analysis_population$sample_counts,
  "table_descriptive_population_counts.csv",
  subfolder = "qa"
)
################################################################################
# Generic descriptive LPG, fuel, stove, kitchen, and safety outcomes imported from
# 3.1_descriptive_generic_outcomes_20260812.R
#
# This imported framework produces broad generic outcome summaries. It
# writes generic companion outputs using table_descriptive_* and
# fig_descriptive_* filenames without the earlier generic-output prefix.
# These outputs remain distinct from the more tailored topic-specific descriptive tables already present below. Duplicate and
# near-duplicate analyses are documented in
# 7_tables/RF105_reviewed_YYYYMMDD/qa/table_descriptive_duplicate_notes.csv.
################################################################################

generic_descriptive_script_label <- "3_descriptive_outcomes_20260805_2213.R"
generic_missing_response_codes <- c("77", "88", "99")
generic_choice_form_files <- tibble(
  form_file = file.path(
    project_root,
    c(
      "2_data_raw/survey_baseline_survey and data review/rohingya_fuel_v64.xlsx",
      "2_data_raw/survey_midline_survey/rohingya_fuel_v94_endline_Rohingya.xlsx",
      "2_data_raw/survey_endline_survey and data review/rohingya_fuel_v116_no_hr.xlsx"
    )
  ),
  form_priority = c(1L, 2L, 3L)
)

generic_clean_choice_code <- function(x) {
  out <- str_squish(as.character(x))
  out[is.na(x) | out == ""] <- NA_character_
  out_num <- suppressWarnings(as.numeric(out))
  if_else(!is.na(out_num) & out_num == floor(out_num),
          as.character(as.integer(out_num)), out)
}

generic_read_choice_labels <- function(form_file, form_priority) {
  if (!file.exists(form_file)) {
    return(tibble())
  }

  form_sheets <- readxl::excel_sheets(form_file)
  if (!"choices" %in% form_sheets) {
    return(tibble())
  }

  survey_sheets <- intersect(
    c("survey", "survey_single_lang", "survey_combined"), form_sheets
  )
  if (length(survey_sheets) == 0) {
    return(tibble())
  }

  survey_choice_lists <- purrr::map_dfr(survey_sheets, function(sheet_name) {
    suppressMessages(
      readxl::read_excel(form_file, sheet = sheet_name, .name_repair = "unique")
    ) %>%
      janitor::clean_names() %>%
      transmute(
        source_variable = as.character(name),
        select_type = as.character(type),
        survey_sheet = sheet_name
      )
  }) %>%
    filter(str_detect(select_type, "^select_(one|multiple)\\s+")) %>%
    mutate(
      choice_list = str_match(select_type, "^select_(?:one|multiple)\\s+([^\\s]+)")[, 2],
      choice_list = str_remove_all(choice_list, "^\\[|\\]$")
    ) %>%
    filter(!is.na(source_variable), !is.na(choice_list))

  choice_sheet <- suppressMessages(
    readxl::read_excel(form_file, sheet = "choices", .name_repair = "unique")
  ) %>%
    janitor::clean_names()
  label_cols <- intersect(
    c("label_english_en", "label_english", "label"), names(choice_sheet)
  )
  if (length(label_cols) == 0) {
    return(tibble())
  }
  label_col <- label_cols[[1]]

  choices <- choice_sheet %>%
    transmute(
      choice_list = str_remove_all(as.character(list_name), "^\\[|\\]$"),
      response_value = generic_clean_choice_code(name),
      response_label = str_squish(as.character(.data[[label_col]]))
    ) %>%
    filter(!is.na(choice_list), !is.na(response_value), !is.na(response_label),
           response_label != "") %>%
    distinct(choice_list, response_value, response_label)

  survey_choice_lists %>%
    left_join(choices, by = "choice_list", relationship = "many-to-many") %>%
    filter(!is.na(response_value), !is.na(response_label)) %>%
    mutate(form_file = form_file, form_priority = form_priority) %>%
    distinct(source_variable, response_value, response_label, form_file,
             form_priority)
}

generic_choice_label_lookup <- purrr::map2_dfr(
  generic_choice_form_files$form_file,
  generic_choice_form_files$form_priority,
  generic_read_choice_labels
) %>%
  arrange(desc(form_priority)) %>%
  distinct(source_variable, response_value, .keep_all = TRUE)

generic_manual_choice_labels <- tribble(
  ~source_variable, ~response_value, ~response_label,
  "burn_plastic_frequency", "5", "5 times in prior week",
  "burn_plastic_frequency", "7", "7 times in prior week"
)

generic_choice_label_lookup <- bind_rows(
  generic_manual_choice_labels %>%
    mutate(form_file = NA_character_, form_priority = Inf),
  generic_choice_label_lookup
) %>%
  arrange(desc(form_priority)) %>%
  distinct(source_variable, response_value, .keep_all = TRUE)

generic_clean_numeric_value <- function(x, nonnegative = FALSE) {
  out <- as_number(x)
  out[out %in% as.numeric(generic_missing_response_codes)] <- NA_real_
  if (isTRUE(nonnegative)) {
    out[out < 0] <- NA_real_
  }
  out
}

generic_clean_category_value <- function(x, missing_codes = generic_missing_response_codes) {
  out <- str_squish(as.character(x))
  out[is.na(x)] <- NA_character_
  out <- na_if(out, "")
  out[str_to_lower(out) %in% c("na", "nan", "null")] <- NA_character_
  out[out %in% missing_codes] <- NA_character_
  out
}

generic_clean_text_value <- function(x) {
  out <- generic_clean_category_value(x)
  out[out == "0"] <- NA_character_
  out
}

generic_continuous_ci_lower <- function(mean_value, se_value, n_value) {
  if (is.na(n_value) || n_value <= 1 || is.na(se_value) || is.na(mean_value)) {
    return(NA_real_)
  }
  mean_value - qt(0.975, n_value - 1) * se_value
}

generic_continuous_ci_upper <- function(mean_value, se_value, n_value) {
  if (is.na(n_value) || n_value <= 1 || is.na(se_value) || is.na(mean_value)) {
    return(NA_real_)
  }
  mean_value + qt(0.975, n_value - 1) * se_value
}

generic_write_internal_text_csv <- function(x, filename, reason) {
  dir.create(dir_restricted_qa, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(dir_restricted_qa, filename)
  readr::write_csv(x, out_file, na = "")

  metadata <- tibble(
    output_file = normalizePath(out_file, winslash = "/", mustWork = FALSE),
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    reason = reason,
    release_status = "restricted_internal_only"
  )
  readr::write_csv(metadata, paste0(out_file, ".metadata.csv"), na = "")
  message("Wrote restricted free-text QA file: ", out_file)
  invisible(out_file)
}

analysis_label_response_value <- function(value, source_variable = NA_character_) {
  value <- generic_clean_category_value(value)
  source_variable <- rep_len(as.character(source_variable), length(value))

  purrr::map2_chr(value, source_variable, function(value_i, source_variable_i) {
    if (is.na(value_i)) {
      return(NA_character_)
    }

    if (!is.na(source_variable_i) && nrow(generic_choice_label_lookup) > 0) {
      matched_label <- generic_choice_label_lookup %>%
        filter(source_variable == source_variable_i, response_value == value_i) %>%
        pull(response_label) %>%
        unique()

      if (length(matched_label) > 0 && !is.na(matched_label[[1]]) &&
          nzchar(matched_label[[1]])) {
        return(matched_label[[1]])
      }
    }

    case_when(
      value_i == "0" ~ "No/none",
      value_i == "66" ~ "Other",
      value_i == "77" ~ "Refused",
      value_i == "88" ~ "Not applicable",
      value_i == "99" ~ "Do not know",
      TRUE ~ paste0("Unlabeled response ", value_i)
    )
  })
}

generic_clean_filename_token <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

generic_split_response_codes <- function(x) {
  x_clean <- generic_clean_category_value(x)
  lapply(x_clean, function(value) {
    if (is.na(value)) {
      return(NA_character_)
    }
    tokens <- unlist(strsplit(value, "\\s+"))
    tokens[tokens != ""]
  })
}

generic_multi_select_any_yn <- function(x) {
  tokens <- generic_split_response_codes(x)
  vapply(tokens, function(value) {
    if (length(value) == 1 && is.na(value)) {
      return(NA_integer_)
    }
    selected_codes <- setdiff(value, c("0", generic_missing_response_codes))
    as.integer(length(selected_codes) > 0)
  }, integer(1))
}

generic_multi_select_any_col <- function(df, var) {
  if (var %in% names(df)) {
    return(generic_multi_select_any_yn(df[[var]]))
  }
  rep(NA_integer_, nrow(df))
}

generic_positive_numeric_yn_col <- function(df, var) {
  if (!var %in% names(df)) {
    return(rep(NA_integer_, nrow(df)))
  }
  value <- generic_clean_numeric_value(df[[var]], nonnegative = TRUE)
  case_when(
    is.na(value) ~ NA_integer_,
    value > 0 ~ 1L,
    TRUE ~ 0L
  )
}

generic_option_codes_for_var <- function(df, base_var) {
  option_cols <- names(df)[startsWith(names(df), paste0(base_var, "/"))]
  option_codes_cols <- substring(option_cols, nchar(base_var) + 2L)

  option_codes_values <- character()
  if (base_var %in% names(df)) {
    option_codes_values <- unlist(generic_split_response_codes(df[[base_var]]))
  }

  codes <- generic_clean_category_value(unique(c(option_codes_cols, option_codes_values)))
  codes <- codes[!is.na(codes)]
  if (length(codes) == 0) {
    return(character())
  }

  tibble(
    option_code = unique(codes),
    option_sort_num = suppressWarnings(as.numeric(option_code))
  ) %>%
    arrange(is.na(option_sort_num), option_sort_num, option_code) %>%
    pull(option_code)
}

generic_select_multi_response <- function(df, base_var, option_code) {
  option_var <- paste0(base_var, "/", option_code)

  if (option_var %in% names(df)) {
    return(list(
      value = make_yn(df[[option_var]]),
      source_variable_used = option_var
    ))
  }

  if (base_var %in% names(df)) {
    tokens <- generic_split_response_codes(df[[base_var]])
    value <- vapply(tokens, function(response_codes) {
      if (length(response_codes) == 1 && is.na(response_codes)) {
        return(NA_integer_)
      }
      as.integer(option_code %in% response_codes)
    }, integer(1))

    return(list(
      value = value,
      source_variable_used = base_var
    ))
  }

  list(
    value = rep(NA_integer_, nrow(df)),
    source_variable_used = NA_character_
  )
}

generic_prop_ci_lower <- function(proportion, n_nonmissing) {
  if_else(
    n_nonmissing > 0 & !is.na(proportion),
    pmax(0, 100 * (proportion - qnorm(0.975) *
                     sqrt(proportion * (1 - proportion) / n_nonmissing))),
    NA_real_
  )
}

generic_prop_ci_upper <- function(proportion, n_nonmissing) {
  if_else(
    n_nonmissing > 0 & !is.na(proportion),
    pmin(100, 100 * (proportion + qnorm(0.975) *
                      sqrt(proportion * (1 - proportion) / n_nonmissing))),
    NA_real_
  )
}

generic_summarise_binary_vars <- function(df, var_table,
                                  population = "all_deduplicated_household_timepoint_records") {
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
        value = make_yn(.data[[var]])
      ) %>%
      add_all_arms_rows() %>%
      group_by(timepoint, study_arm_overall) %>%
      summarise(
        n_total = n(),
        n_nonmissing = sum(!is.na(value)),
        n_yes = sum(value == 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      add_binary_denominator_summaries(numerator_col = "n_yes") %>%
      mutate(
        outcome_group = var_table$outcome_group[[i]],
        outcome_name = var_table$outcome_name[[i]],
        outcome_label = var_table$outcome_label[[i]],
        source_variable = var,
        analysis_variable = var_table$analysis_variable[[i]],
        analysis_type = "binary_percent",
        unit = "percent",
        population = population,
        denominator_note = "percent/ci_lower/ci_upper and *_nonmissing columns use n_yes / n_nonmissing; *_total columns use n_yes / n_total. all_arms rows pool comparison and intervention households."
      )
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, analysis_variable, source_variable, analysis_type,
           unit, population, n_total, n_nonmissing, n_yes, percent, ci_lower,
           ci_upper, percent_nonmissing, ci_lower_nonmissing,
           ci_upper_nonmissing, percent_total, ci_lower_total, ci_upper_total,
           denominator_note) %>%
    arrange_timepoint_arm(outcome_group, outcome_name)
}

generic_summarise_continuous_vars <- function(df, var_table,
                                      population = "all_deduplicated_household_timepoint_records") {
  var_table <- var_table %>% filter(source_variable %in% names(df))
  if (nrow(var_table) == 0) {
    return(tibble())
  }

  default_denominator_note <- "Continuous summaries use nonmissing nonnegative numeric values; all_arms rows pool comparison and intervention households."

  map_dfr(seq_len(nrow(var_table)), function(i) {
    var <- var_table$source_variable[[i]]

    denominator_note_i <- default_denominator_note
    if ("denominator_note" %in% names(var_table)) {
      note_i <- var_table$denominator_note[[i]]
      if (!is.na(note_i) && nzchar(note_i)) {
        denominator_note_i <- note_i
      }
    }

    missing_response_codes_as_na_i <- TRUE
    if ("missing_response_codes_as_na" %in% names(var_table)) {
      missing_codes_i <- var_table$missing_response_codes_as_na[[i]]
      if (!is.na(missing_codes_i)) {
        missing_response_codes_as_na_i <- isTRUE(missing_codes_i)
      }
    }

    df %>%
      transmute(
        timepoint,
        study_arm_overall,
        value = {
          value <- as_number(.data[[var]])
          if (isTRUE(missing_response_codes_as_na_i)) {
            value <- generic_clean_numeric_value(.data[[var]], nonnegative = TRUE)
          } else {
            value[value < 0] <- NA_real_
          }
          value
        }
      ) %>%
      add_all_arms_rows() %>%
      group_by(timepoint, study_arm_overall) %>%
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
        se = if_else(n_nonmissing > 1 & !is.na(sd),
                     sd / sqrt(n_nonmissing),
                     NA_real_),
        ci_lower = generic_continuous_ci_lower(mean, se, n_nonmissing),
        ci_upper = generic_continuous_ci_upper(mean, se, n_nonmissing),
        .groups = "drop"
      ) %>%
      mutate(
        outcome_group = var_table$outcome_group[[i]],
        outcome_name = var_table$outcome_name[[i]],
        outcome_label = var_table$outcome_label[[i]],
        source_variable = var,
        analysis_variable = var_table$analysis_variable[[i]],
        analysis_type = "continuous_summary",
        unit = var_table$unit[[i]],
        population = population,
        denominator_note = denominator_note_i
      )
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, analysis_variable, source_variable, analysis_type,
           unit, population, n_total, n_nonmissing, mean, sd, median, p25,
           p75, min, max, ci_lower, ci_upper, denominator_note) %>%
    arrange_timepoint_arm(outcome_group, outcome_name)
}

generic_summarise_categorical_vars <- function(df, var_table,
                                       population = "all_deduplicated_household_timepoint_records") {
  var_table <- var_table %>% filter(source_variable %in% names(df))
  if (nrow(var_table) == 0) {
    return(tibble())
  }

  map_dfr(seq_len(nrow(var_table)), function(i) {
    var <- var_table$source_variable[[i]]
    base <- df %>%
      transmute(
        timepoint,
        study_arm_overall,
        category_value = generic_clean_category_value(.data[[var]])
      ) %>%
      add_all_arms_rows()

    denominators <- base %>%
      group_by(timepoint, study_arm_overall) %>%
      summarise(
        n_total = n(),
        n_nonmissing = sum(!is.na(category_value)),
        .groups = "drop"
      )

    base %>%
      filter(!is.na(category_value)) %>%
      count(timepoint, study_arm_overall, category_value,
            name = "n_category") %>%
      left_join(denominators, by = c("timepoint", "study_arm_overall")) %>%
      mutate(
        percent = if_else(n_nonmissing > 0,
                          100 * n_category / n_nonmissing,
                          NA_real_),
        category_label = analysis_label_response_value(category_value, var),
        outcome_group = var_table$outcome_group[[i]],
        outcome_name = var_table$outcome_name[[i]],
        outcome_label = var_table$outcome_label[[i]],
        source_variable = var,
        analysis_variable = var_table$analysis_variable[[i]],
        analysis_type = "categorical_distribution",
        unit = "percent",
        population = population,
        denominator_note = "Percent among nonmissing responses within each timepoint-arm group; all_arms rows pool comparison and intervention households."
      )
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, analysis_variable, source_variable, analysis_type,
           category_value, category_label, unit, population, n_total,
           n_nonmissing, n_category, percent, denominator_note) %>%
    arrange_timepoint_arm(outcome_group, outcome_name, category_value)
}

generic_summarise_multi_select_vars <- function(df, var_table,
                                        population = "all_deduplicated_household_timepoint_records") {
  var_table <- var_table %>% filter(source_variable %in% names(df) |
                                      map_lgl(source_variable, ~ any(startsWith(names(df), paste0(.x, "/")))))
  if (nrow(var_table) == 0) {
    return(tibble())
  }

  map_dfr(seq_len(nrow(var_table)), function(i) {
    base_var <- var_table$source_variable[[i]]
    option_codes <- generic_option_codes_for_var(df, base_var)
    if (length(option_codes) == 0) {
      return(tibble())
    }

    map_dfr(option_codes, function(option_code) {
      selected <- generic_select_multi_response(df, base_var, option_code)

      tibble(
        timepoint = df$timepoint,
        study_arm_overall = df$study_arm_overall,
        value = selected$value
      ) %>%
        add_all_arms_rows() %>%
        group_by(timepoint, study_arm_overall) %>%
        summarise(
          n_total = n(),
          n_nonmissing = sum(!is.na(value)),
          n_selected = sum(value == 1, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        add_binary_denominator_summaries(numerator_col = "n_selected") %>%
        mutate(
          outcome_group = var_table$outcome_group[[i]],
          outcome_name = var_table$outcome_name[[i]],
          outcome_label = var_table$outcome_label[[i]],
          analysis_variable = var_table$analysis_variable[[i]],
          source_variable = base_var,
          source_variable_used = selected$source_variable_used,
          option_code = option_code,
          option_label = analysis_label_response_value(option_code, base_var),
          analysis_type = "multiselect_option_percent",
          unit = "percent",
          population = population,
          denominator_note = "percent/ci_lower/ci_upper and *_nonmissing columns use n_selected / n_nonmissing; *_total columns use n_selected / n_total. all_arms rows pool comparison and intervention households."
        )
    })
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, analysis_variable, source_variable,
           source_variable_used, analysis_type, option_code, option_label,
           unit, population, n_total, n_nonmissing, n_selected, percent,
           ci_lower, ci_upper, percent_nonmissing, ci_lower_nonmissing,
           ci_upper_nonmissing, percent_total, ci_lower_total, ci_upper_total,
           denominator_note) %>%
    arrange_timepoint_arm(outcome_group, outcome_name, option_code)
}

generic_summarise_text_presence <- function(df, var_table,
                                    population = "all_deduplicated_household_timepoint_records") {
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
        text_value = generic_clean_text_value(.data[[var]])
      ) %>%
      add_all_arms_rows() %>%
      group_by(timepoint, study_arm_overall) %>%
      summarise(
        n_total = n(),
        n_nonmissing_text = sum(!is.na(text_value)),
        n_unique_text = n_distinct(text_value[!is.na(text_value)]),
        .groups = "drop"
      ) %>%
      mutate(
        percent_with_text = if_else(n_total > 0,
                                    100 * n_nonmissing_text / n_total,
                                    NA_real_),
        outcome_group = var_table$outcome_group[[i]],
        outcome_name = var_table$outcome_name[[i]],
        outcome_label = var_table$outcome_label[[i]],
        analysis_variable = var_table$analysis_variable[[i]],
        source_variable = var,
        analysis_type = "text_response_presence",
        unit = "percent",
        population = population,
        denominator_note = "Percent with a nonblank free-text response among all household-timepoint records; raw text response counts are restricted QA."
      )
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, analysis_variable, source_variable, analysis_type,
           unit, population, n_total, n_nonmissing_text, n_unique_text,
           percent_with_text, denominator_note) %>%
    arrange_timepoint_arm(outcome_group, outcome_name)
}

generic_summarise_text_response_counts <- function(df, var_table) {
  var_table <- var_table %>% filter(source_variable %in% names(df))
  if (nrow(var_table) == 0) {
    return(tibble())
  }

  map_dfr(seq_len(nrow(var_table)), function(i) {
    var <- var_table$source_variable[[i]]
    base <- df %>%
      transmute(
        timepoint,
        study_arm_overall,
        text_value = generic_clean_text_value(.data[[var]])
      ) %>%
      add_all_arms_rows()

    denominators <- base %>%
      group_by(timepoint, study_arm_overall) %>%
      summarise(n_nonmissing_text = sum(!is.na(text_value)), .groups = "drop")

    base %>%
      filter(!is.na(text_value)) %>%
      count(timepoint, study_arm_overall, text_value,
            name = "n_text_response") %>%
      left_join(denominators, by = c("timepoint", "study_arm_overall")) %>%
      mutate(
        percent_among_text_responses = if_else(
          n_nonmissing_text > 0,
          100 * n_text_response / n_nonmissing_text,
          NA_real_
        ),
        outcome_group = var_table$outcome_group[[i]],
        outcome_name = var_table$outcome_name[[i]],
        outcome_label = var_table$outcome_label[[i]],
        analysis_variable = var_table$analysis_variable[[i]],
        source_variable = var,
        analysis_type = "restricted_text_response_counts"
      )
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, analysis_variable, source_variable, analysis_type,
           text_value, n_nonmissing_text, n_text_response,
           percent_among_text_responses) %>%
    arrange_timepoint_arm(outcome_group, outcome_name, text_value)
}

generic_write_plot_if_data <- function(plot_data, plot, filename, width = 10, height = 6) {
  if (nrow(plot_data) == 0) {
    message("No rows available for figure: ", filename)
    return(invisible(NULL))
  }
  save_reviewed_plot(plot, filename, width = width, height = height)
}

arm_colors_requested <- c(comparison = "#3B6EA8", intervention = "#C94C4C")

generic_save_binary_figures <- function(binary_summary) {
  groups <- binary_summary %>%
    filter(study_arm_overall %in% arm_levels, n_nonmissing > 0, !is.na(percent)) %>%
    distinct(outcome_group) %>%
    pull(outcome_group)

  walk(groups, function(group_name) {
    plot_data <- binary_summary %>%
      filter(outcome_group == group_name,
             study_arm_overall %in% arm_levels,
             n_nonmissing > 0,
             !is.na(percent)) %>%
      mutate(outcome_label_plot = str_wrap(outcome_label, width = 34))

    fig <- ggplot(
      plot_data,
      aes(x = timepoint, y = percent, fill = study_arm_overall)
    ) +
      geom_col(position = position_dodge(width = 0.75), width = 0.65) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                    position = position_dodge(width = 0.75), width = 0.2) +
      facet_wrap(~ outcome_label_plot, ncol = 2) +
      scale_fill_manual(values = arm_colors_requested, drop = FALSE) +
      scale_y_continuous(labels = label_number(suffix = "%"),
                         limits = c(0, 100)) +
      theme_classic(base_size = 10) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 35, hjust = 1),
        strip.text = element_text(hjust = 0)
      ) +
      labs(x = "Timepoint", y = "Percent of households", fill = "Study arm")

    height <- max(4.5, 2.2 + 1.6 * ceiling(n_distinct(plot_data$outcome_label) / 2))
    generic_write_plot_if_data(
      plot_data,
      fig,
      paste0("fig_descriptive_binary_", generic_clean_filename_token(group_name), ".png"),
      width = 10,
      height = height
    )
  })
}

generic_save_multiselect_figures <- function(multiselect_summary) {
  groups <- multiselect_summary %>%
    filter(study_arm_overall %in% arm_levels, n_nonmissing > 0, !is.na(percent)) %>%
    distinct(outcome_group) %>%
    pull(outcome_group)

  walk(groups, function(group_name) {
    plot_data <- multiselect_summary %>%
      filter(outcome_group == group_name,
             study_arm_overall %in% arm_levels,
             n_nonmissing > 0,
             !is.na(percent)) %>%
      mutate(
        outcome_label_plot = str_wrap(outcome_label, width = 30),
        option_label_plot = str_wrap(option_label, width = 24)
      )

    fig <- ggplot(
      plot_data,
      aes(x = option_label_plot, y = percent, fill = study_arm_overall)
    ) +
      geom_col(position = position_dodge(width = 0.75), width = 0.65) +
      facet_grid(outcome_label_plot ~ timepoint, scales = "free_y", space = "free_y") +
      coord_flip() +
      scale_fill_manual(values = arm_colors_requested, drop = FALSE) +
      scale_y_continuous(labels = label_number(suffix = "%"),
                         limits = c(0, 100)) +
      theme_classic(base_size = 9) +
      theme(
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0, hjust = 0),
        panel.spacing.y = unit(0.7, "lines")
      ) +
      labs(x = NULL, y = "Percent selected", fill = "Study arm")

    height <- max(5, min(16, 2.5 + 0.32 * n_distinct(paste(plot_data$outcome_label,
                                                           plot_data$option_code))))
    generic_write_plot_if_data(
      plot_data,
      fig,
      paste0("fig_descriptive_multiselect_", generic_clean_filename_token(group_name), ".png"),
      width = 12,
      height = height
    )
  })
}

generic_save_categorical_figures <- function(categorical_summary) {
  groups <- categorical_summary %>%
    filter(study_arm_overall %in% arm_levels, n_nonmissing > 0, !is.na(percent)) %>%
    distinct(outcome_group) %>%
    pull(outcome_group)

  walk(groups, function(group_name) {
    plot_data <- categorical_summary %>%
      filter(outcome_group == group_name,
             study_arm_overall %in% arm_levels,
             n_nonmissing > 0,
             !is.na(percent)) %>%
      mutate(
        outcome_label_plot = str_wrap(outcome_label, width = 30),
        category_label_plot = str_wrap(category_label, width = 24)
      )

    fig <- ggplot(
      plot_data,
      aes(x = category_label_plot, y = percent, fill = study_arm_overall)
    ) +
      geom_col(position = position_dodge(width = 0.75), width = 0.65) +
      facet_grid(outcome_label_plot ~ timepoint, scales = "free_y", space = "free_y") +
      coord_flip() +
      scale_fill_manual(values = arm_colors_requested, drop = FALSE) +
      scale_y_continuous(labels = label_number(suffix = "%"),
                         limits = c(0, 100)) +
      theme_classic(base_size = 9) +
      theme(
        legend.position = "bottom",
        strip.text.y = element_text(angle = 0, hjust = 0),
        panel.spacing.y = unit(0.7, "lines")
      ) +
      labs(x = NULL, y = "Percent of nonmissing responses", fill = "Study arm")

    height <- max(5, min(16, 2.5 + 0.32 * n_distinct(paste(plot_data$outcome_label,
                                                           plot_data$category_label))))
    generic_write_plot_if_data(
      plot_data,
      fig,
      paste0("fig_descriptive_categorical_", generic_clean_filename_token(group_name), ".png"),
      width = 12,
      height = height
    )
  })
}

generic_save_continuous_figure <- function(continuous_summary) {
  plot_data <- continuous_summary %>%
    filter(study_arm_overall %in% arm_levels, n_nonmissing > 0, !is.na(mean)) %>%
    mutate(outcome_label_plot = str_wrap(paste0(outcome_label, " (", unit, ")"), width = 34))

  fig <- ggplot(
    plot_data,
    aes(x = timepoint, y = mean, color = study_arm_overall, group = study_arm_overall)
  ) +
    geom_hline(yintercept = 0, color = "grey85") +
    geom_point(position = position_dodge(width = 0.2), size = 1.8) +
    geom_errorbar(
      data = plot_data %>% filter(!is.na(ci_lower), !is.na(ci_upper)),
      aes(ymin = ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.2),
      width = 0.15
    ) +
    facet_wrap(~ outcome_label_plot, scales = "free_y", ncol = 2) +
    scale_color_manual(values = arm_colors_requested, drop = FALSE) +
    theme_classic(base_size = 10) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 35, hjust = 1),
      strip.text = element_text(hjust = 0)
    ) +
    labs(x = "Timepoint", y = "Mean with 95% CI", color = "Study arm")

  height <- max(5, 2.4 + 1.7 * ceiling(n_distinct(plot_data$outcome_label) / 2))
  generic_write_plot_if_data(
    plot_data,
    fig,
    "fig_descriptive_continuous_outcomes.png",
    width = 10,
    height = height
  )
}


################################################################################
# Analysis population and derived variables
################################################################################

survey$lpg_stove_repair_any_yn <- generic_multi_select_any_col(survey, "lpg_stove_repair")
survey$lpg_stove_repair_inspect_any_yn <- generic_multi_select_any_col(survey, "lpg_stove_repair_inspect")
survey$lpg_cylinder_repair_any_yn <- generic_multi_select_any_col(survey, "lpg_cylinder_repair")
survey$lpg_cylinder_repair_inspect_any_yn <- generic_multi_select_any_col(survey, "lpg_cylinder_repair_inspect")
survey$lpg_repair_details_any_yn <- generic_multi_select_any_col(survey, "lpg_repair_details")
survey$lpg_afraid_any_yn <- generic_multi_select_any_col(survey, "lpg_afraid")
survey$fire_any_yn <- generic_positive_numeric_yn_col(survey, "fire_number")
survey$burn_plastic_any_yn <- generic_positive_numeric_yn_col(survey, "burn_plastic_frequency")
survey$cook_sell_yesterday_any_yn <- generic_positive_numeric_yn_col(survey, "cook_sell_yesterday")
survey$cook_pressure_cooker_any_yn <- generic_positive_numeric_yn_col(survey, "cook_pressure_cooker")
forest_wood_fee_bdt <- generic_clean_numeric_value(num_col(survey, "forest_wood_fee"), nonnegative = TRUE)
buy_wood_cost_bdt <- generic_clean_numeric_value(num_col(survey, "buy_wood_cost"), nonnegative = TRUE)
spent_total_month_bdt <- generic_clean_numeric_value(num_col(survey, "spent_total_month"), nonnegative = TRUE)

survey$forest_wood_fee_paid_gt0_pct <- case_when(
  is.na(forest_wood_fee_bdt) ~ NA_real_,
  forest_wood_fee_bdt > 0 ~ 100,
  TRUE ~ 0
)
survey$forest_wood_fee_positive_bdt <- case_when(
  !is.na(forest_wood_fee_bdt) & forest_wood_fee_bdt > 0 ~ forest_wood_fee_bdt,
  TRUE ~ NA_real_
)
survey$forest_wood_fee_pct_monthly_expenditures <- case_when(
  !is.na(forest_wood_fee_bdt) & forest_wood_fee_bdt > 0 &
    !is.na(spent_total_month_bdt) & spent_total_month_bdt > 0 ~
    100 * forest_wood_fee_bdt / spent_total_month_bdt,
  TRUE ~ NA_real_
)
survey$buy_wood_cost_pct_monthly_expenditures <- case_when(
  !is.na(buy_wood_cost_bdt) & !is.na(spent_total_month_bdt) &
    spent_total_month_bdt > 0 ~ 100 * buy_wood_cost_bdt / spent_total_month_bdt,
  TRUE ~ NA_real_
)

write_reviewed_csv(
  analysis_population$sample_counts,
  "table_descriptive_generic_population_counts.csv",
  subfolder = "qa"
)

################################################################################
# Variable dictionaries
################################################################################

################################################################################
# Requested variable inventory
################################################################################

analysis_variables <- tribble(
  ~outcome_group, ~analysis_variable, ~analysis_label,
  "lpg_repair", "lpg_stove_repair", "LPG stove repair",
  "lpg_repair", "lpg_stove_repair_inspect", "LPG stove repair inspection",
  "lpg_repair", "lpg_stove_repair_image", "LPG stove repair image",
  "lpg_repair", "lpg_stove_repair_image_2", "LPG stove repair image 2",
  "lpg_repair", "lpg_cylinder_repair", "LPG cylinder repair",
  "lpg_repair", "lpg_cylinder_repair_inspect", "LPG cylinder repair inspection",
  "lpg_repair", "lpg_cylinder_repair_image", "LPG cylinder repair image",
  "lpg_repair", "lpg_cylinder_repair_image_2", "LPG cylinder repair image 2",
  "lpg_repair", "lpg_repair_details", "LPG repair details",
  "lpg_repair", "lpg_repair_costs", "LPG repair costs",
  "lpg_behaviors", "buy_lpg_place", "Place household buys LPG",
  "lpg_behaviors", "lpg_use", "LPG use",
  "lpg_behaviors", "lpg_extra_use", "Extra LPG use",
  "lpg_behaviors", "fuel_type_before_lpg", "Fuel type used before LPG",
  "lpg_behaviors", "lpg_changes_lifestyle", "Lifestyle changes after LPG",
  "lpg_distribution_women", "receive_lpg_women_rq", "Women required/requested for LPG receipt",
  "lpg_distribution_women", "receive_lpg_woman_who", "Woman involved in LPG receipt",
  "lpg_distribution_women", "receive_lpg_like", "Respondent liked LPG receipt process",
  "lpg_distribution_women", "receive_lpg_require_opinion", "Woman's opinion required for LPG receipt",
  "lpg_distribution_women", "receive_lpg_carry", "Who carried LPG",
  "lpg_distribution_women", "receive_lpg_volunteer_who", "Volunteer involved in LPG receipt",
  "lpg_distribution_women", "receive_lpg_volunteer_feel", "Feelings about LPG volunteer",
  "missed_refill", "refill_time_lpg_missed", "Reason household missed refill",
  "missed_refill", "refill_time_lpg_missed_other", "Other reason household missed refill",
  "safety", "lpg_safety_visit", "Received LPG safety visit",
  "safety", "lpg_afraid", "Afraid of LPG",
  "safety", "lpg_afraid_why", "Reason afraid of LPG",
  "safety", "lpg_gas_leak", "LPG gas leak",
  "safety", "lpg_child_burn", "Child burned by LPG",
  "safety", "fire_number", "Number of fires",
  "safety", "fire_why", "Reason for fire",
  "safety", "fire_consequence", "Consequence of fire",
  "forest_reasons", "reason_forest_food", "Visited forest for food",
  "forest_reasons", "reason_forest_med", "Visited forest for medicine",
  "forest_reasons", "reason_forest_shelter", "Visited forest for shelter materials",
  "forest_reasons", "reason_forest_privacy", "Visited forest for privacy",
  "forest_reasons", "reason_forest_defacation", "Visited forest for defecation",
  "forest_reasons", "reason_forest_leisure", "Visited forest for leisure",
  "forest_reasons", "reason_forest_other", "Visited forest for other reason",
  "forest_reasons", "reason_forest_other_specified", "Other forest reason specified",
  "forest_reasons", "cost_forest_not_wood", "Cost of non-wood forest product",
  "housing_materials", "roof_material", "Roof material",
  "housing_materials", "floor_material", "Floor material",
  "housing_materials", "wall_material", "Wall material",
  "housing_materials", "window_number", "Number of windows",
  "housing_materials", "window_door_wall", "Window or door in wall",
  "housing_materials", "garenga", "Garenga",
  "housing_materials", "heat_control", "Heat control strategy",
  "housing_materials", "cool_control", "Cooling control strategy",
  "plastic_burning", "burn_plastic_frequency", "Plastic burning frequency",
  "plastic_burning", "burn_plastic_types", "Types of plastic burned",
  "plastic_burning", "burn_plastic_reason", "Reason for burning plastic",
  "fuel_time_cost", "trad_stove_use_last", "Last traditional stove use",
  "fuel_time_cost", "trad_stove_use_fuel", "Fuel used in traditional stove",
  "fuel_time_cost", "collect_wood_when_last", "Last collected wood",
  "fuel_time_cost", "buy_wood_when_last", "Last bought wood",
  "fuel_time_cost", "traditional_use_yesterday", "Traditional stove uses yesterday",
  "fuel_time_cost", "LPG_use_yesterday", "LPG stove uses yesterday",
  "fuel_time_cost", "collect_wood_6mo", "Collected wood in past 6 months",
  "fuel_time_cost", "times_wood_week", "Firewood collection trips per week",
  "fuel_time_cost", "collect_wood_walk_hr", "Hours walking to collect wood",
  "fuel_time_cost", "forest_wood_fee", "Forest wood fee",
  "fuel_time_cost", "forest_wood_fee_paid_gt0_pct", "Paid any forest wood fee",
  "fuel_time_cost", "forest_wood_fee_positive_bdt", "Forest wood fee among households paying >0",
  "fuel_time_cost", "forest_wood_fee_pct_monthly_expenditures", "Forest wood fee as percent of monthly expenditures among households paying >0",
  "fuel_time_cost", "gather_scraps_dead", "Whether collected scraps/leaves/twigs were all dead",
  "fuel_time_cost", "gather_wood_reason", "Reason for gathering wood",
  "fuel_time_cost", "buy_wood_cost", "Wood purchase cost",
  "fuel_time_cost", "buy_wood_cost_pct_monthly_expenditures", "Wood purchase cost as percent of monthly expenditures",
  "fuel_time_cost", "buy_wood_reason", "Reason for buying wood",
  "fuel_time_cost", "buy_wood_cost_bundle", "Wood bundle cost",
  "fuel_time_cost", "buy_wood_bundle_last", "Wood bundles bought last time",
  "fuel_time_cost", "buy_wood_cost_month_estimate", "Estimated monthly wood cost",
  "fuel_time_cost", "lpg_days_possible", "Days LPG cylinder lasted or could last",
  "fuel_time_cost", "lpg_willingness_to_pay", "Willingness to pay for LPG",
  "fuel_time_cost", "buy_lpg_cost", "LPG purchase cost",
  "fuel_time_cost", "buy_lpg_walk", "Hours walking to buy LPG",
  "fuel_time_cost", "buy_lpg_wait", "Hours waiting to buy LPG",
  "fuel_time_cost", "receive_lpg_walk", "Hours walking to receive LPG",
  "fuel_time_cost", "receive_lpg_wait", "Hours waiting to receive LPG",
  "fuel_time_cost", "receive_crh_walk", "Hours walking to receive charcoal",
  "fuel_time_cost", "receive_crh_wait", "Hours waiting to receive charcoal",
  "fuel_consequences", "food_flavor", "Fuel effects on food flavor",
  "fuel_consequences", "food_burn_likelihood", "Likelihood of food burning",
  "fuel_consequences", "food_burn_consequence", "Consequences of food burning",
  "stove_use", "stove_reason_cook_together", "Used stove to cook together",
  "stove_use", "stove_boil_drink", "Used stove to boil drinking water",
  "stove_use", "stove_boil_bathe", "Used stove to boil bathing water",
  "stove_use", "stove_reason_stay_warm", "Used stove to stay warm",
  "stove_use", "stove_reason_sell_food", "Used stove to cook food to sell",
  "stove_use", "cook_to_sell", "Cooks food to sell",
  "stove_use", "cook_sell_yesterday", "Cooked food to sell yesterday",
  "who_cooks", "cook_who_w", "Women cook",
  "who_cooks", "cook_who_g", "Girls cook",
  "who_cooks", "cook_who_m", "Men cook",
  "who_cooks", "cook_who_b", "Boys cook",
  "cooking_practices", "cook_meal_yesterday_times", "Meals cooked yesterday",
  "cooking_practices", "cook_snack_yesterday_times", "Snacks cooked yesterday",
  "cooking_practices", "boil_yesterday_times", "Times boiled water yesterday",
  "cooking_practices", "soak_rice", "Soaked rice before cooking",
  "cooking_practices", "cook_method_rice", "Rice cooking method",
  "cooking_practices", "soak_lentils", "Soaked lentils before cooking",
  "cooking_practices", "cover_pot", "Covered pot while cooking",
  "cooking_practices", "cooking_area", "Cooking area",
  "cooking_practices", "kitchen_observe", "Kitchen observed",
  "kitchen_spotcheck", "screening_lpg_spotcheck", "Completed LPG spotcheck screening",
  "kitchen_spotcheck", "flooring_below_stove", "Flooring below stove",
  "kitchen_spotcheck", "space_between_wall_stove", "Space between wall and stove",
  "kitchen_spotcheck", "location_stove", "Stove location",
  "kitchen_spotcheck", "above_stove", "Items above stove",
  "kitchen_spotcheck", "garnja_kitchen", "Garnja in kitchen",
  "kitchen_spotcheck", "window_kitchen", "Kitchen has a window",
  "kitchen_spotcheck", "window_kitchen_stove_location", "Window near stove location",
  "kitchen_spotcheck", "window_kitchen_use", "Kitchen window use",
  "kitchen_spotcheck", "cook_saucepan_num", "Number of saucepans",
  "kitchen_spotcheck", "cook_pot_num", "Number of cooking pots",
  "kitchen_spotcheck", "cook_pot_with_handle_num", "Number of pots with handles",
  "kitchen_spotcheck", "cook_pressure_cooker_num", "Number of pressure cookers",
  "kitchen_spotcheck", "cook_pressure_cooker", "Pressure cooker present"
)

binary_vars <- tribble(
  ~outcome_group, ~analysis_variable, ~source_variable, ~outcome_name, ~outcome_label,
  "lpg_repair", "lpg_stove_repair", "lpg_stove_repair_any_yn", "lpg_stove_repair_any", "Any LPG stove repair item reported",
  "lpg_repair", "lpg_stove_repair_inspect", "lpg_stove_repair_inspect_any_yn", "lpg_stove_repair_inspect_any", "Any LPG stove repair inspection item reported",
  "lpg_repair", "lpg_cylinder_repair", "lpg_cylinder_repair_any_yn", "lpg_cylinder_repair_any", "Any LPG cylinder repair item reported",
  "lpg_repair", "lpg_cylinder_repair_inspect", "lpg_cylinder_repair_inspect_any_yn", "lpg_cylinder_repair_inspect_any", "Any LPG cylinder repair inspection item reported",
  "lpg_repair", "lpg_repair_details", "lpg_repair_details_any_yn", "lpg_repair_details_any", "Any LPG repair detail reported",
  "lpg_distribution_women", "receive_lpg_women_rq", "receive_lpg_women_rq", "receive_lpg_women_rq", "Women required/requested for LPG receipt",
  "lpg_distribution_women", "receive_lpg_require_opinion", "receive_lpg_require_opinion", "receive_lpg_require_opinion", "Woman's opinion required for LPG receipt",
  "lpg_distribution_women", "receive_lpg_volunteer_who", "receive_lpg_volunteer_who", "receive_lpg_volunteer_who", "Volunteer involved in LPG receipt",
  "safety", "lpg_safety_visit", "lpg_safety_visit", "lpg_safety_visit", "Received LPG safety visit",
  "safety", "lpg_afraid", "lpg_afraid_any_yn", "lpg_afraid_any", "Any fear of LPG reported",
  "safety", "lpg_gas_leak", "lpg_gas_leak", "lpg_gas_leak", "LPG gas leak reported",
  "safety", "lpg_child_burn", "lpg_child_burn", "lpg_child_burn", "Child burn reported",
  "safety", "fire_number", "fire_any_yn", "fire_any", "Any fire reported",
  "forest_reasons", "reason_forest_food", "reason_forest_food", "reason_forest_food", "Visited forest for food",
  "forest_reasons", "reason_forest_med", "reason_forest_med", "reason_forest_med", "Visited forest for medicine",
  "forest_reasons", "reason_forest_shelter", "reason_forest_shelter", "reason_forest_shelter", "Visited forest for shelter materials",
  "forest_reasons", "reason_forest_privacy", "reason_forest_privacy", "reason_forest_privacy", "Visited forest for privacy",
  "forest_reasons", "reason_forest_defacation", "reason_forest_defacation", "reason_forest_defacation", "Visited forest for defecation",
  "forest_reasons", "reason_forest_leisure", "reason_forest_leisure", "reason_forest_leisure", "Visited forest for leisure",
  "forest_reasons", "reason_forest_other", "reason_forest_other", "reason_forest_other", "Visited forest for other reason",
  "housing_materials", "window_door_wall", "window_door_wall", "window_door_wall", "Window or door in wall",
  "housing_materials", "garenga", "garenga", "garenga", "Garenga",
  "plastic_burning", "burn_plastic_frequency", "burn_plastic_any_yn", "burn_plastic_any", "Burned plastic at least once",
  "fuel_time_cost", "collect_wood_6mo", "collect_wood_6mo", "collect_wood_6mo", "Collected wood in past 6 months",
  "stove_use", "stove_reason_cook_together", "stove_reason_cook_together", "stove_reason_cook_together", "Used stove to cook together",
  "stove_use", "stove_boil_drink", "stove_boil_drink", "stove_boil_drink", "Used stove to boil drinking water",
  "stove_use", "stove_boil_bathe", "stove_boil_bathe", "stove_boil_bathe", "Used stove to boil bathing water",
  "stove_use", "stove_reason_stay_warm", "stove_reason_stay_warm", "stove_reason_stay_warm", "Used stove to stay warm",
  "stove_use", "stove_reason_sell_food", "stove_reason_sell_food", "stove_reason_sell_food", "Used stove to cook food to sell",
  "stove_use", "cook_sell_yesterday", "cook_sell_yesterday_any_yn", "cook_sell_yesterday_any", "Cooked food to sell yesterday",
  "who_cooks", "cook_who_w", "cook_who_w", "cook_who_w", "Women cook",
  "who_cooks", "cook_who_g", "cook_who_g", "cook_who_g", "Girls cook",
  "who_cooks", "cook_who_m", "cook_who_m", "cook_who_m", "Men cook",
  "who_cooks", "cook_who_b", "cook_who_b", "cook_who_b", "Boys cook",
  "cooking_practices", "soak_rice", "soak_rice", "soak_rice", "Soaked rice before cooking",
  "cooking_practices", "soak_lentils", "soak_lentils", "soak_lentils", "Soaked lentils before cooking",
  "cooking_practices", "cover_pot", "cover_pot", "cover_pot", "Covered pot while cooking",
  "cooking_practices", "kitchen_observe", "kitchen_observe", "kitchen_observe", "Kitchen observed",
  "kitchen_spotcheck", "screening_lpg_spotcheck", "screening_lpg_spotcheck", "screening_lpg_spotcheck", "Completed LPG spotcheck screening",
  "kitchen_spotcheck", "garnja_kitchen", "garnja_kitchen", "garnja_kitchen", "Garnja in kitchen",
  "kitchen_spotcheck", "window_kitchen", "window_kitchen", "window_kitchen", "Kitchen has a window",
  "kitchen_spotcheck", "window_kitchen_stove_location", "window_kitchen_stove_location", "window_kitchen_stove_location", "Window near stove location",
  "kitchen_spotcheck", "cook_pressure_cooker", "cook_pressure_cooker_any_yn", "cook_pressure_cooker_any", "Pressure cooker present"
) %>%
  mutate(analysis_output = "table_descriptive_binary_outcomes.csv")

multi_select_vars <- tribble(
  ~outcome_group, ~analysis_variable, ~source_variable, ~outcome_name, ~outcome_label,
  "lpg_repair", "lpg_stove_repair", "lpg_stove_repair", "lpg_stove_repair_options", "LPG stove repair",
  "lpg_repair", "lpg_stove_repair_inspect", "lpg_stove_repair_inspect", "lpg_stove_repair_inspect_options", "LPG stove repair inspection",
  "lpg_repair", "lpg_cylinder_repair", "lpg_cylinder_repair", "lpg_cylinder_repair_options", "LPG cylinder repair",
  "lpg_repair", "lpg_cylinder_repair_inspect", "lpg_cylinder_repair_inspect", "lpg_cylinder_repair_inspect_options", "LPG cylinder repair inspection",
  "lpg_repair", "lpg_repair_details", "lpg_repair_details", "lpg_repair_details_options", "LPG repair details",
  "lpg_behaviors", "fuel_type_before_lpg", "fuel_type_before_lpg", "fuel_type_before_lpg_options", "Fuel type before LPG",
  "lpg_behaviors", "lpg_changes_lifestyle", "lpg_changes_lifestyle", "lpg_changes_lifestyle_options", "Lifestyle changes after LPG",
  "housing_materials", "heat_control", "heat_control", "heat_control_options", "Heat control strategy",
  "housing_materials", "cool_control", "cool_control", "cool_control_options", "Cooling control strategy",
  "plastic_burning", "burn_plastic_types", "burn_plastic_types", "burn_plastic_types_options", "Types of plastic burned",
  "plastic_burning", "burn_plastic_reason", "burn_plastic_reason", "burn_plastic_reason_options", "Reason for burning plastic",
  "fuel_time_cost", "trad_stove_use_fuel", "trad_stove_use_fuel", "trad_stove_use_fuel_options", "Fuel used in traditional stove",
  "fuel_consequences", "food_flavor", "food_flavor", "food_flavor_options", "Fuel effects on food flavor",
  "fuel_consequences", "food_burn_consequence", "food_burn_consequence", "food_burn_consequence_options", "Consequences of food burning"
) %>%
  mutate(analysis_output = "table_descriptive_multiselect_outcomes.csv")

categorical_vars <- tribble(
  ~outcome_group, ~analysis_variable, ~source_variable, ~outcome_name, ~outcome_label,
  "lpg_behaviors", "buy_lpg_place", "buy_lpg_place", "buy_lpg_place", "Place household buys LPG",
  "lpg_behaviors", "lpg_use", "lpg_use", "lpg_use", "LPG use",
  "lpg_behaviors", "lpg_extra_use", "lpg_extra_use", "lpg_extra_use", "Extra LPG use",
  "lpg_distribution_women", "receive_lpg_woman_who", "receive_lpg_woman_who", "receive_lpg_woman_who", "Woman involved in LPG receipt",
  "lpg_distribution_women", "receive_lpg_like", "receive_lpg_like", "receive_lpg_like", "Respondent liked LPG receipt process",
  "lpg_distribution_women", "receive_lpg_carry", "receive_lpg_carry", "receive_lpg_carry", "Who carried LPG",
  "lpg_distribution_women", "receive_lpg_volunteer_who", "receive_lpg_volunteer_who", "receive_lpg_volunteer_who_categories", "Volunteer involved in LPG receipt",
  "lpg_distribution_women", "receive_lpg_volunteer_feel", "receive_lpg_volunteer_feel", "receive_lpg_volunteer_feel", "Feelings about LPG volunteer",
  "missed_refill", "refill_time_lpg_missed", "refill_time_lpg_missed", "refill_time_lpg_missed", "Reason household missed refill",
  "safety", "lpg_afraid", "lpg_afraid", "lpg_afraid", "Afraid of LPG",
  "safety", "lpg_afraid_why", "lpg_afraid_why", "lpg_afraid_why", "Reason afraid of LPG",
  "safety", "fire_why", "fire_why", "fire_why", "Reason for fire",
  "safety", "fire_consequence", "fire_consequence", "fire_consequence", "Consequence of fire",
  "housing_materials", "roof_material", "roof_material", "roof_material", "Roof material",
  "housing_materials", "floor_material", "floor_material", "floor_material", "Floor material",
  "housing_materials", "wall_material", "wall_material", "wall_material", "Wall material",
  "housing_materials", "window_door_wall", "window_door_wall", "window_door_wall_categories", "Window or door in wall",
  "housing_materials", "garenga", "garenga", "garenga_categories", "Garenga",
  "plastic_burning", "burn_plastic_frequency", "burn_plastic_frequency", "burn_plastic_frequency_categories", "Plastic burning frequency",
  "fuel_time_cost", "gather_scraps_dead", "gather_scraps_dead", "gather_scraps_dead", "Whether collected scraps/leaves/twigs were all dead",
  "fuel_time_cost", "gather_wood_reason", "gather_wood_reason", "gather_wood_reason", "Reason for gathering wood",
  "fuel_time_cost", "buy_wood_reason", "buy_wood_reason", "buy_wood_reason", "Reason for buying wood",
  "fuel_consequences", "food_burn_likelihood", "food_burn_likelihood", "food_burn_likelihood", "Likelihood of food burning",
  "stove_use", "cook_to_sell", "cook_to_sell", "cook_to_sell", "Cooks food to sell",
  "cooking_practices", "cook_method_rice", "cook_method_rice", "cook_method_rice", "Rice cooking method",
  "cooking_practices", "cooking_area", "cooking_area", "cooking_area", "Cooking area",
  "kitchen_spotcheck", "flooring_below_stove", "flooring_below_stove", "flooring_below_stove", "Flooring below stove",
  "kitchen_spotcheck", "space_between_wall_stove", "space_between_wall_stove", "space_between_wall_stove", "Space between wall and stove",
  "kitchen_spotcheck", "location_stove", "location_stove", "location_stove", "Stove location",
  "kitchen_spotcheck", "above_stove", "above_stove", "above_stove", "Items above stove",
  "kitchen_spotcheck", "window_kitchen_use", "window_kitchen_use", "window_kitchen_use", "Kitchen window use",
  "kitchen_spotcheck", "cook_pressure_cooker", "cook_pressure_cooker", "cook_pressure_cooker_categories", "Pressure cooker present"
) %>%
  mutate(analysis_output = "table_descriptive_categorical_outcomes.csv")

continuous_vars <- tribble(
  ~outcome_group, ~analysis_variable, ~source_variable, ~outcome_name, ~outcome_label, ~unit,
  "lpg_repair", "lpg_repair_costs", "lpg_repair_costs", "lpg_repair_costs", "LPG repair costs", "BDT",
  "safety", "fire_number", "fire_number", "fire_number", "Number of fires", "count",
  "forest_reasons", "cost_forest_not_wood", "cost_forest_not_wood", "cost_forest_not_wood", "Cost of non-wood forest product", "BDT",
  "housing_materials", "window_number", "window_number", "window_number", "Number of windows", "count",
  "plastic_burning", "burn_plastic_frequency", "burn_plastic_frequency", "burn_plastic_frequency", "Plastic burning frequency", "reported frequency",
  "fuel_time_cost", "trad_stove_use_last", "trad_stove_use_last", "trad_stove_use_last", "Last traditional stove use", "reported numeric value",
  "fuel_time_cost", "collect_wood_when_last", "collect_wood_when_last", "collect_wood_when_last", "Last collected wood", "reported numeric value",
  "fuel_time_cost", "buy_wood_when_last", "buy_wood_when_last", "buy_wood_when_last", "Last bought wood", "reported numeric value",
  "fuel_time_cost", "traditional_use_yesterday", "traditional_use_yesterday", "traditional_use_yesterday", "Traditional stove uses yesterday", "uses",
  "fuel_time_cost", "LPG_use_yesterday", "LPG_use_yesterday", "LPG_use_yesterday", "LPG stove uses yesterday", "uses",
  "fuel_time_cost", "times_wood_week", "times_wood_week", "times_wood_week", "Firewood collection trips per week", "trips/week",
  "fuel_time_cost", "collect_wood_walk_hr", "collect_wood_walk_hr", "collect_wood_walk_hr", "Hours walking to collect wood", "hours",
  "fuel_time_cost", "forest_wood_fee", "forest_wood_fee", "forest_wood_fee", "Forest wood fee", "BDT",
  "fuel_time_cost", "forest_wood_fee_paid_gt0_pct", "forest_wood_fee_paid_gt0_pct", "forest_wood_fee_paid_gt0_pct", "Households paying any forest wood fee", "percent",
  "fuel_time_cost", "forest_wood_fee_positive_bdt", "forest_wood_fee_positive_bdt", "forest_wood_fee_positive_bdt", "Forest wood fee among households paying >0", "BDT",
  "fuel_time_cost", "forest_wood_fee_pct_monthly_expenditures", "forest_wood_fee_pct_monthly_expenditures", "forest_wood_fee_pct_monthly_expenditures", "Forest wood fee as percent of monthly expenditures among households paying >0", "percent of monthly expenditures",
  "fuel_time_cost", "buy_wood_cost", "buy_wood_cost", "buy_wood_cost", "Wood purchase cost", "BDT",
  "fuel_time_cost", "buy_wood_cost_pct_monthly_expenditures", "buy_wood_cost_pct_monthly_expenditures", "buy_wood_cost_pct_monthly_expenditures", "Wood purchase cost as percent of monthly expenditures", "percent of monthly expenditures",
  "fuel_time_cost", "buy_wood_cost_bundle", "buy_wood_cost_bundle", "buy_wood_cost_bundle", "Wood bundle cost", "BDT",
  "fuel_time_cost", "buy_wood_bundle_last", "buy_wood_bundle_last", "buy_wood_bundle_last", "Wood bundles bought last time", "bundles",
  "fuel_time_cost", "buy_wood_cost_month_estimate", "buy_wood_cost_month_estimate", "buy_wood_cost_month_estimate", "Estimated monthly wood cost", "BDT",
  "fuel_time_cost", "lpg_days_possible", "lpg_days_possible", "lpg_days_possible", "Days LPG cylinder lasted or could last", "days",
  "fuel_time_cost", "lpg_willingness_to_pay", "lpg_willingness_to_pay", "lpg_willingness_to_pay", "Willingness to pay for LPG", "BDT",
  "fuel_time_cost", "buy_lpg_cost", "buy_lpg_cost", "buy_lpg_cost", "LPG purchase cost", "BDT",
  "fuel_time_cost", "buy_lpg_walk", "buy_lpg_walk", "buy_lpg_walk", "Hours walking to buy LPG", "hours",
  "fuel_time_cost", "buy_lpg_wait", "buy_lpg_wait", "buy_lpg_wait", "Hours waiting to buy LPG", "hours",
  "fuel_time_cost", "receive_lpg_walk", "receive_lpg_walk", "receive_lpg_walk", "Hours walking to receive LPG", "hours",
  "fuel_time_cost", "receive_lpg_wait", "receive_lpg_wait", "receive_lpg_wait", "Hours waiting to receive LPG", "hours",
  "fuel_time_cost", "receive_crh_walk", "receive_crh_walk", "receive_crh_walk", "Hours walking to receive charcoal", "hours",
  "fuel_time_cost", "receive_crh_wait", "receive_crh_wait", "receive_crh_wait", "Hours waiting to receive charcoal", "hours",
  "stove_use", "cook_sell_yesterday", "cook_sell_yesterday", "cook_sell_yesterday", "Cooked food to sell yesterday", "reported frequency",
  "cooking_practices", "cook_meal_yesterday_times", "cook_meal_yesterday_times", "cook_meal_yesterday_times", "Meals cooked yesterday", "times",
  "cooking_practices", "cook_snack_yesterday_times", "cook_snack_yesterday_times", "cook_snack_yesterday_times", "Snacks cooked yesterday", "times",
  "cooking_practices", "boil_yesterday_times", "boil_yesterday_times", "boil_yesterday_times", "Times boiled water yesterday", "times",
  "kitchen_spotcheck", "cook_saucepan_num", "cook_saucepan_num", "cook_saucepan_num", "Number of saucepans", "count",
  "kitchen_spotcheck", "cook_pot_num", "cook_pot_num", "cook_pot_num", "Number of cooking pots", "count",
  "kitchen_spotcheck", "cook_pot_with_handle_num", "cook_pot_with_handle_num", "cook_pot_with_handle_num", "Number of pots with handles", "count",
  "kitchen_spotcheck", "cook_pressure_cooker_num", "cook_pressure_cooker_num", "cook_pressure_cooker_num", "Number of pressure cookers", "count"
) %>%
  mutate(
    analysis_output = "table_descriptive_continuous_outcomes.csv",
    missing_response_codes_as_na = !source_variable %in% c(
      "forest_wood_fee_paid_gt0_pct",
      "forest_wood_fee_positive_bdt",
      "forest_wood_fee_pct_monthly_expenditures",
      "buy_wood_cost_pct_monthly_expenditures"
    ),
    denominator_note = case_when(
      source_variable == "forest_wood_fee_paid_gt0_pct" ~
        "Mean is the percent of households with nonmissing nonnegative forest_wood_fee who reported paying more than 0 BDT; all_arms rows pool comparison and intervention households.",
      source_variable == "forest_wood_fee_positive_bdt" ~
        "Amount summary is restricted to households with nonmissing nonnegative forest_wood_fee greater than 0 BDT; all_arms rows pool comparison and intervention households.",
      source_variable == "forest_wood_fee_pct_monthly_expenditures" ~
        "Summary is restricted to households with forest_wood_fee greater than 0 BDT and nonmissing positive spent_total_month; values are 100 * forest_wood_fee / spent_total_month.",
      source_variable == "buy_wood_cost_pct_monthly_expenditures" ~
        "Summary uses households with nonmissing nonnegative buy_wood_cost and nonmissing positive spent_total_month; values are 100 * buy_wood_cost / spent_total_month.",
      source_variable == "times_wood_week" ~
        "The clean-data source field is times_wood_day, but the questionnaire asks how many times in one week the household collects firewood; values are therefore interpreted as trips/week. All-arms rows pool comparison and intervention households.",
      TRUE ~ NA_character_
    )
  )

text_vars <- tribble(
  ~outcome_group, ~analysis_variable, ~source_variable, ~outcome_name, ~outcome_label,
  "missed_refill", "refill_time_lpg_missed_other", "refill_time_lpg_missed_other", "refill_time_lpg_missed_other", "Other reason household missed refill",
  "forest_reasons", "reason_forest_other_specified", "reason_forest_other_specified", "reason_forest_other_specified", "Other forest reason specified"
) %>%
  mutate(
    analysis_output = "table_descriptive_text_response_presence.csv",
    restricted_analysis_output = "table_descriptive_text_response_counts_internal.csv"
  )

################################################################################
# Tables
################################################################################

binary_summary <- generic_summarise_binary_vars(survey, binary_vars)
multiselect_summary <- generic_summarise_multi_select_vars(survey, multi_select_vars)
categorical_summary <- generic_summarise_categorical_vars(survey, categorical_vars)
continuous_summary <- generic_summarise_continuous_vars(survey, continuous_vars)
text_presence_summary <- generic_summarise_text_presence(survey, text_vars)
text_response_counts_internal <- generic_summarise_text_response_counts(survey, text_vars)

write_reviewed_csv(
  binary_summary,
  "table_descriptive_binary_outcomes.csv"
)
write_reviewed_csv(
  multiselect_summary,
  "table_descriptive_multiselect_outcomes.csv"
)
write_reviewed_csv(
  categorical_summary,
  "table_descriptive_categorical_outcomes.csv"
)
write_reviewed_csv(
  continuous_summary,
  "table_descriptive_continuous_outcomes.csv"
)
write_reviewed_csv(
  text_presence_summary,
  "table_descriptive_text_response_presence.csv"
)

if (nrow(text_response_counts_internal) > 0) {
  generic_write_internal_text_csv(
    text_response_counts_internal,
    "table_descriptive_text_response_counts_internal.csv",
    reason = "Free-text responses can contain sensitive or identifying details and should be reviewed internally before sharing."
  )
}

analysis_map <- bind_rows(
  binary_vars %>%
    transmute(analysis_variable, analysis_type = "binary_percent",
              reviewed_output = analysis_output),
  multi_select_vars %>%
    transmute(analysis_variable, analysis_type = "multiselect_option_percent",
              reviewed_output = analysis_output),
  categorical_vars %>%
    transmute(analysis_variable, analysis_type = "categorical_distribution",
              reviewed_output = analysis_output),
  continuous_vars %>%
    transmute(analysis_variable, analysis_type = "continuous_summary",
              reviewed_output = analysis_output),
  text_vars %>%
    transmute(analysis_variable, analysis_type = "text_response_presence",
              reviewed_output = analysis_output),
  text_vars %>%
    transmute(analysis_variable, analysis_type = "restricted_text_response_counts",
              reviewed_output = restricted_analysis_output)
) %>%
  distinct()

analysis_map_summary <- analysis_map %>%
  group_by(analysis_variable) %>%
  summarise(
    analysis_types = paste(sort(unique(analysis_type)), collapse = "; "),
    reviewed_outputs = paste(sort(unique(reviewed_output)), collapse = "; "),
    .groups = "drop"
  )

variable_coverage <- analysis_variables %>%
  mutate(
    direct_variable_available = analysis_variable %in% names(survey),
    slash_column_count = map_int(
      analysis_variable,
      ~ sum(startsWith(names(survey), paste0(.x, "/")))
    ),
    availability_status = case_when(
      direct_variable_available ~ "available",
      slash_column_count > 0 ~ "slash_columns_available_without_base_variable",
      TRUE ~ "missing_from_clean_final"
    )
  ) %>%
  left_join(analysis_map_summary, by = "analysis_variable") %>%
  mutate(
    analysis_types = replace_na(analysis_types, ""),
    reviewed_outputs = replace_na(reviewed_outputs, ""),
    note = case_when(
      str_detect(analysis_variable, "_image") &
        availability_status == "missing_from_clean_final" ~
        "Image field was not present in the clean_final household survey file inspected by this script.",
      analysis_types == "" & availability_status != "missing_from_clean_final" ~
        "Variable is available but not assigned to a summary table; review dictionary if this was unexpected.",
      TRUE ~ ""
    ),
    generated_by = generic_descriptive_script_label
  ) %>%
  arrange(outcome_group, analysis_variable)

write_reviewed_csv(
  variable_coverage,
  "table_descriptive_analysis_variable_coverage.csv",
  subfolder = "qa"
)

multiselect_option_coverage <- multiselect_summary %>%
  distinct(outcome_group, outcome_name, outcome_label, analysis_variable,
           source_variable, source_variable_used, option_code, option_label) %>%
  arrange(outcome_group, outcome_name, option_code)

write_reviewed_csv(
  multiselect_option_coverage,
  "table_descriptive_multiselect_option_coverage.csv",
  subfolder = "qa"
)

################################################################################
# Figures
################################################################################

generic_save_binary_figures(binary_summary)
generic_save_multiselect_figures(multiselect_summary)
generic_save_categorical_figures(categorical_summary)
generic_save_continuous_figure(continuous_summary)


generic_duplicate_notes <- tribble(
  ~analysis_variable, ~overlap_type, ~existing_output_or_section, ~note,
  "buy_lpg_cost", "duplicate", "table_descriptive_lpg_purchase_cost.csv", "The imported requested-outcome summary also reports buy_lpg_cost as a generic continuous requested outcome; the topic-specific LPG purchase-cost table remains the preferred manuscript table.",
  "lpg_willingness_to_pay", "duplicate", "table_descriptive_lpg_wtp_summary.csv", "The imported requested-outcome summary also reports willingness to pay as a generic continuous outcome; the topic-specific WTP table includes positive-only and threshold summaries requested later.",
  "lpg_days_possible", "near_duplicate", "table_descriptive_lpg_duration_household_size.csv", "The imported requested-outcome summary reports overall days LPG lasted/could last; the existing topic-specific figure/table stratifies by household size.",
  "buy_wood_cost", "near_duplicate", "table_descriptive_household_expenditures.csv; table_descriptive_fuel_collection_time.csv", "The imported requested-outcome summary reports a raw wood-cost field; existing tables summarize broader expenditure and fuel-collection constructs.",
  "collect_wood_walk_hr", "near_duplicate", "table_descriptive_fuel_collection_time.csv", "The imported requested-outcome summary reports one raw walking-time field; the existing fuel-collection-time table remains the tailored fuel-time output.",
  "burn_plastic_frequency", "duplicate", "table_descriptive_supplemental_binary_outcomes.csv; table_descriptive_supplemental_continuous_outcomes.csv; table_descriptive_nonlpg_plastic_sensitivity.csv", "Plastic-burning summaries already exist in the supplemental and sensitivity sections; the requested-output table gives a generic distribution and continuous summary.",
  "window_kitchen", "duplicate", "table_descriptive_supplemental_binary_outcomes.csv", "Kitchen-window summaries already exist in the supplemental ventilation section; the requested-output table keeps the companion-script generic version.",
  "cook_who_w/cook_who_g/cook_who_m/cook_who_b", "duplicate", "table_descriptive_supplemental_binary_outcomes.csv", "Who-cooks indicators are already summarized in the supplemental section; requested-output summaries duplicate those indicators in the generic requested table.",
  "stove_boil_drink/stove_boil_bathe/stove_reason_*", "near_duplicate", "table_descriptive_supplemental_binary_outcomes.csv; stove-monitor descriptive outputs", "Survey-reported stove-use purposes overlap with supplemental survey summaries and are distinct from Geocene monitor-measured stove-use outputs."
)
write_reviewed_csv(
  generic_duplicate_notes,
  "table_descriptive_duplicate_notes.csv",
  subfolder = "qa"
)

message("Generic descriptive outcome tables and figures complete.")

write_reviewed_csv(
  analysis_population$duplicate_records,
  "table_descriptive_duplicate_records.csv",
  subfolder = "qa"
)

################################################################################
# RF105B characteristics workbook for manuscript Table 1-style checks
################################################################################

# This workbook is intentionally written near the top of the descriptive workflow
# because it is a manuscript-facing descriptive output. It uses the same
# deduplicated clean_final household survey records as the rest of this script
# and adds pooled all-household columns for baseline, midline, and endline.
write_rf105b_characteristics_workbook <- function(survey_dedup,
                                                  output_file = file.path(
                                                    project_root,
                                                    "7_tables",
                                                    "table_descriptive_rf105b_characteristics_reordered.xlsx"
                                                  )) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Package 'openxlsx' is required to write ", basename(output_file),
      ". Run renv::restore() from the project root, then rerun this script.",
      call. = FALSE
    )
  }

  rf105b_income_fields <- c(
    "income_cash_ngo",
    "income_own_business",
    "income_wage_labor",
    "income_skill_labor",
    "income_selling_wood",
    "income_abroad",
    "income_humanitarian_asst",
    "income_handicrafts_tailoring",
    "income_farming"
  )

  ensure_workbook_cols <- function(df, vars) {
    missing_vars <- setdiff(vars, names(df))
    for (var in missing_vars) {
      df[[var]] <- NA
    }
    df
  }

  rf105b_row_sum_numeric <- function(df, vars) {
    vars <- vars[vars %in% names(df)]
    if (length(vars) == 0) {
      return(rep(NA_real_, nrow(df)))
    }

    mat <- do.call(cbind, lapply(vars, function(var) as_number(df[[var]])))
    out <- rowSums(mat, na.rm = TRUE)
    out[rowSums(!is.na(mat)) == 0] <- NA_real_
    out
  }

  rf105b_coalesce_num <- function(...) {
    vals <- list(...)
    out <- rep(NA_real_, length(vals[[1]]))
    for (value in vals) {
      value_num <- as_number(value)
      replace_idx <- is.na(out) & !is.na(value_num)
      out[replace_idx] <- value_num[replace_idx]
    }
    out
  }

  rf105b_make_binary <- function(x) {
    x_chr <- str_squish(str_to_lower(as.character(x)))
    x_num <- as_number(x_chr)
    case_when(
      is.na(x) | x_chr == "" ~ NA_integer_,
      !is.na(x_num) ~ as.integer(x_num > 0),
      x_chr %in% c("yes", "y", "true", "present") ~ 1L,
      x_chr %in% c("no", "n", "false", "absent") ~ 0L,
      TRUE ~ NA_integer_
    )
  }

  rf105b_first_nonmissing_chr <- function(x) {
    x_chr <- as.character(x)
    valid_idx <- which(!is.na(x_chr) & str_squish(x_chr) != "")
    if (length(valid_idx) == 0L) {
      return(NA_character_)
    }
    x_chr[valid_idx[[1]]]
  }

  rf105b_months_between <- function(start_date, end_date) {
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    out <- as.numeric(end_date - start_date) / (365.25 / 12)
    out[is.na(start_date) | is.na(end_date)] <- NA_real_
    out
  }

  format_workbook_p <- function(p) {
    case_when(
      is.na(p) ~ "",
      p < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p)
    )
  }

  calc_workbook_pairwise_p <- function(df, variable, type, group_a, group_b) {
    dat <- df %>%
      filter(group %in% c(group_a, group_b), !is.na(.data[[variable]])) %>%
      mutate(test_group = factor(group, levels = c(group_a, group_b)))

    if (nrow(dat) == 0 || n_distinct(dat$test_group) < 2) {
      return(NA_real_)
    }

    out <- tryCatch({
      if (identical(type, "continuous")) {
        stats::t.test(dat[[variable]] ~ dat$test_group)$p.value
      } else {
        tab <- table(dat[[variable]], dat$test_group)
        if (nrow(tab) < 2 || ncol(tab) < 2) {
          return(NA_real_)
        }
        chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
        if (any(chi$expected < 5)) {
          stats::fisher.test(tab)$p.value
        } else {
          chi$p.value
        }
      }
    }, error = function(e) NA_real_)

    as.numeric(out)
  }

  make_p_value_column <- function(dat, table_rows, group_a, group_b, column_name) {
    tibble(
      row = table_rows$row,
      value = pmap_chr(
        list(table_rows$variable, table_rows$type),
        ~ format_workbook_p(calc_workbook_pairwise_p(dat, ..1, ..2, group_a, group_b))
      )
    ) %>%
      rename(!!column_name := value)
  }

  format_workbook_mean_sd <- function(x, digits = 1) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return("")
    }
    sprintf(paste0("%.", digits, "f (%.", digits, "f)"), mean(x), sd(x))
  }

  format_workbook_n_pct <- function(x, digits = 1) {
    denom <- sum(!is.na(x))
    n_yes <- sum(x == 1, na.rm = TRUE)
    if (denom == 0) {
      return("")
    }
    sprintf(paste0("%d (%.", digits, "f%%)"), n_yes, 100 * n_yes / denom)
  }

  format_workbook_cell <- function(dat, variable, type) {
    x <- dat[[variable]]
    if (type == "binary") {
      format_workbook_n_pct(x)
    } else {
      format_workbook_mean_sd(x)
    }
  }

  make_summary_column <- function(dat, table_rows, column_name) {
    tibble(
      row = table_rows$row,
      value = map2_chr(
        table_rows$variable,
        table_rows$type,
        ~ format_workbook_cell(dat, .x, .y)
      )
    ) %>%
      rename(!!column_name := value)
  }

  baseline_phone_lookup <- tibble(
    fcn_id = character(),
    baseline_mobile_phone = character(),
    baseline_smartphone = character()
  )
  raw_baseline_file <- file.path(
    project_root,
    "2_data_raw",
    "survey_baseline",
    "RohingyaFuelMaster_Corrected_20200419_refugee.csv"
  )
  if (file.exists(raw_baseline_file)) {
    baseline_phone_lookup <- suppressWarnings(
      readr::read_csv(raw_baseline_file, show_col_types = FALSE)
    ) %>%
      ensure_workbook_cols(c("fcn_id", "mobile_phone", "smartphone")) %>%
      transmute(
        fcn_id = as.character(fcn_id),
        baseline_mobile_phone = as.character(mobile_phone),
        baseline_smartphone = as.character(smartphone),
        raw_row = row_number()
      ) %>%
      filter(!is.na(fcn_id), str_squish(fcn_id) != "") %>%
      arrange(fcn_id, raw_row) %>%
      group_by(fcn_id) %>%
      summarise(
        baseline_mobile_phone = rf105b_first_nonmissing_chr(baseline_mobile_phone),
        baseline_smartphone = rf105b_first_nonmissing_chr(baseline_smartphone),
        .groups = "drop"
      )
  }

  survey_for_workbook <- survey_dedup %>%
    ensure_workbook_cols(c(
      "fcn_id", "timepoint", "study_arm_overall", "collection_date",
      "target_child_sex", "target_child_months", "hh_size",
      "hh_size_2mo_u5", "hh_ppl_smoke", "income", "total_income_30",
      rf105b_income_fields, "spent_total_month", "total_expenditures_30",
      "total_expenditures_180", "clothing", "shelter", "celebrations",
      "medical", "education", "other_expenditures", "debt", "debt_total",
      "electricity", "mattress", "chair_bench", "portable_lamp",
      "solar_lamp", "mobile_phone", "smartphone", "electric_fan",
      "shovel", "sickle", "weaving_tool", "chicken_duck_pigeon"
    )) %>%
    mutate(fcn_id = as.character(fcn_id)) %>%
    left_join(baseline_phone_lookup, by = "fcn_id") %>%
    mutate(
      timepoint = as_ordered_timepoint(timepoint),
      study_arm_overall = as.character(study_arm_overall),
      mobile_phone = case_when(
        !is.na(mobile_phone) & str_squish(as.character(mobile_phone)) != "" ~
          as.character(mobile_phone),
        !is.na(baseline_mobile_phone) ~ baseline_mobile_phone,
        TRUE ~ NA_character_
      ),
      smartphone = case_when(
        !is.na(smartphone) & str_squish(as.character(smartphone)) != "" ~
          as.character(smartphone),
        !is.na(baseline_smartphone) ~ baseline_smartphone,
        TRUE ~ NA_character_
      ),
      exchange_rate = exchange_bdt_per_usd[as.character(timepoint)],
      electricity = case_when(
        as.character(electricity) == "2" ~ 1L,
        TRUE ~ rf105b_make_binary(electricity)
      ),
      target_child_female = case_when(
        str_squish(str_to_lower(as.character(target_child_sex))) %in%
          c("female", "f", "girl", "1") ~ 1L,
        str_squish(str_to_lower(as.character(target_child_sex))) %in%
          c("male", "m", "boy", "0") ~ 0L,
        TRUE ~ NA_integer_
      ),
      target_child_months_num = as_number(target_child_months),
      hh_size_num = as_number(hh_size),
      hh_size_2mo_u5_num = as_number(hh_size_2mo_u5),
      hh_ppl_smoke_num = as_number(hh_ppl_smoke),
      income_usd = as_number(total_income_30) / exchange_rate,
      total_expenditures_30_bdt = rf105b_coalesce_num(
        total_expenditures_30,
        spent_total_month
      ),
      total_expenditures_180_bdt = rf105b_coalesce_num(
        total_expenditures_180,
        rf105b_row_sum_numeric(
          .,
          c(
            "clothing", "shelter", "celebrations", "debt", "medical",
            "education", "other_expenditures"
          )
        )
      ),
      spent_total_month_usd = (
        total_expenditures_30_bdt + total_expenditures_180_bdt / 6
      ) / exchange_rate,
      debt_usd = rf105b_coalesce_num(debt, debt_total) / exchange_rate,
      mattress_yn = rf105b_make_binary(mattress),
      chair_bench_yn = rf105b_make_binary(chair_bench),
      portable_lamp_yn = rf105b_make_binary(portable_lamp),
      solar_lamp_yn = rf105b_make_binary(solar_lamp),
      mobile_phone_yn = rf105b_make_binary(mobile_phone),
      smartphone_yn = rf105b_make_binary(smartphone),
      electric_fan_yn = rf105b_make_binary(electric_fan),
      shovel_yn = rf105b_make_binary(shovel),
      sickle_yn = rf105b_make_binary(sickle),
      weaving_tool_yn = rf105b_make_binary(weaving_tool),
      chicken_duck_pigeon_yn = rf105b_make_binary(chicken_duck_pigeon)
    )

  presence_flags <- survey_for_workbook %>%
    distinct(fcn_id, timepoint) %>%
    mutate(present = TRUE) %>%
    pivot_wider(names_from = timepoint, values_from = present, values_fill = FALSE)

  for (timepoint in timepoint_levels) {
    if (!timepoint %in% names(presence_flags)) {
      presence_flags[[timepoint]] <- FALSE
    }
  }

  baseline_for_groups <- survey_for_workbook %>%
    filter(timepoint == "baseline") %>%
    left_join(presence_flags, by = "fcn_id") %>%
    mutate(
      baseline = coalesce(baseline, FALSE),
      midline = coalesce(midline, FALSE),
      endline = coalesce(endline, FALSE),
      group_g1 = baseline,
      group_g1a = baseline & !midline & !endline,
      group_g2 = baseline & midline,
      group_g2a = baseline & midline & !endline,
      group_g3a = baseline & !endline,
      group_g4 = baseline & midline & endline
    )

  table_rows <- tibble::tribble(
    ~row, ~characteristic, ~variable, ~type,
    5L, "Target child is female", "target_child_female", "binary",
    6L, "Age of target child (mo)", "target_child_months_num", "continuous",
    7L, "Number of household members", "hh_size_num", "continuous",
    8L, "Number of hh members 2 to <60 months", "hh_size_2mo_u5_num", "continuous",
    9L, "Number of household members who smoke", "hh_ppl_smoke_num", "continuous",
    10L, "Monthly income (USD)", "income_usd", "continuous",
    11L, "Monthly expenditure (USD)", "spent_total_month_usd", "continuous",
    12L, "Total debt (USD)", "debt_usd", "continuous",
    13L, "Has >=1 mattress", "mattress_yn", "binary",
    14L, "Has >=1 chair/bench", "chair_bench_yn", "binary",
    15L, "Has >=1 portable lamp", "portable_lamp_yn", "binary",
    16L, "Has >=1 weaving tool", "weaving_tool_yn", "binary",
    17L, "Has >=1 shovel", "shovel_yn", "binary",
    18L, "Has >=1 sickle", "sickle_yn", "binary",
    19L, "Has >=1 poultry", "chicken_duck_pigeon_yn", "binary",
    20L, "Has >=1 solar lamp", "solar_lamp_yn", "binary",
    21L, "Has >=1 mobile phone", "mobile_phone_yn", "binary",
    22L, "Has >=1 smartphone", "smartphone_yn", "binary",
    23L, "Has >=1 electric fan", "electric_fan_yn", "binary",
    24L, "House has solar electricity", "electricity", "binary"
  )

  group_defs <- tibble::tribble(
    ~arm, ~group, ~flag_var, ~column_label,
    "comparison", "comparison_g1", "group_g1", "Comparison Group 1: all baseline households",
    "comparison", "comparison_g1a", "group_g1a", "Comparison Group 1A: lost after baseline",
    "comparison", "comparison_g2", "group_g2", "Comparison Group 2: baseline and midline",
    "comparison", "comparison_g2a", "group_g2a", "Comparison Group 2A: lost after midline",
    "comparison", "comparison_g3a", "group_g3a", "Comparison Group 3A: lost before endline",
    "comparison", "comparison_g4", "group_g4", "Comparison Group 4: baseline, midline, and endline",
    "intervention", "intervention_g1", "group_g1", "Intervention Group 1: all baseline households",
    "intervention", "intervention_g1a", "group_g1a", "Intervention Group 1A: lost after baseline",
    "intervention", "intervention_g2", "group_g2", "Intervention Group 2: baseline and midline",
    "intervention", "intervention_g2a", "group_g2a", "Intervention Group 2A: lost after midline",
    "intervention", "intervention_g3a", "group_g3a", "Intervention Group 3A: lost before endline",
    "intervention", "intervention_g4", "group_g4", "Intervention Group 4: baseline, midline, and endline"
  )

  group_long <- pmap_dfr(group_defs, function(arm, group, flag_var, column_label) {
    baseline_for_groups %>%
      filter(study_arm_overall == arm, .data[[flag_var]]) %>%
      mutate(group = group)
  })

  p_value_defs <- tibble::tribble(
    ~group_a, ~group_b, ~column_label,
    "comparison_g4", "comparison_g3a", "p: comparison G4 vs G3A (overall attrition bias)",
    "comparison_g2", "comparison_g1a", "p: comparison G2 vs G1A (early-stage attrition)",
    "comparison_g4", "comparison_g2a", "p: comparison G4 vs G2A (late-stage attrition)",
    "intervention_g4", "intervention_g3a", "p: intervention G4 vs G3A (overall attrition bias)",
    "intervention_g2", "intervention_g1a", "p: intervention G2 vs G1A (early-stage attrition)",
    "intervention_g4", "intervention_g2a", "p: intervention G4 vs G2A (late-stage attrition)",
    "comparison_g1", "intervention_g1", "p: comparison G1 vs intervention G1"
  )

  all_timepoint_columns <- map_dfc(timepoint_levels, function(tp) {
    dat <- baseline_for_groups %>% filter(.data[[tp]])
    make_summary_column(dat, table_rows, paste0("All households ", tp)) %>%
      select(-row)
  })

  group_columns <- pmap_dfc(group_defs, function(arm, group, flag_var, column_label) {
    dat <- baseline_for_groups %>%
      filter(study_arm_overall == arm, .data[[flag_var]])
    make_summary_column(dat, table_rows, column_label) %>%
      select(-row)
  })

  p_value_columns <- pmap_dfc(
    p_value_defs,
    function(group_a, group_b, column_label) {
      make_p_value_column(group_long, table_rows, group_a, group_b, column_label) %>%
        select(-row)
    }
  )

  count_row <- tibble(
    characteristic = "n =",
    !!!setNames(
      map(timepoint_levels, function(tp) {
        baseline_for_groups %>%
          filter(.data[[tp]]) %>%
          distinct(fcn_id) %>%
          nrow()
      }),
      paste0("All households ", timepoint_levels)
    ),
    !!!setNames(
      pmap(group_defs, function(arm, group, flag_var, column_label) {
        baseline_for_groups %>%
          filter(study_arm_overall == arm, .data[[flag_var]]) %>%
          distinct(fcn_id) %>%
          nrow()
      }),
      group_defs$column_label
    ),
    !!!setNames(as.list(rep("", nrow(p_value_defs))), p_value_defs$column_label)
  ) %>%
    mutate(across(-characteristic, ~ paste0("n = ", .x)))

  output_table <- bind_cols(
    table_rows %>% select(characteristic),
    all_timepoint_columns,
    group_columns,
    p_value_columns
  ) %>%
    bind_rows(count_row, .) %>%
    mutate(
      characteristic = factor(
        characteristic,
        levels = c("n =", table_rows$characteristic),
        ordered = TRUE
      )
    ) %>%
    arrange(characteristic) %>%
    mutate(characteristic = as.character(characteristic))

  note <- paste(
    "Values are n (%) for binary variables and mean (SD) for continuous variables.",
    "All columns summarize baseline household characteristics. Midline and endline survey records are used only to identify, by fcn_id, which baseline households participated at each timepoint and therefore which baseline households belong in each column.",
    "Monthly income uses total_income_30; monthly expenditure uses total_expenditures_30 + total_expenditures_180 / 6, with spent_total_month and the six-month expenditure component sum used where the aggregate fields are absent in baseline clean_final.",
    "Mobile phone and smartphone ownership are filled from the raw baseline survey by fcn_id when unavailable in the clean baseline dataset.",
    "Participation-pattern columns use baseline household characteristics and the same group definitions as the prior RF105B Table 1 workbook.",
    "P-values compare the named column pairs using Welch t-tests for continuous variables and chi-square or Fisher exact tests for binary variables."
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "for paper")
  openxlsx::writeData(wb, "for paper", output_table, startRow = 1, startCol = 1)
  openxlsx::writeData(wb, "for paper", note, startRow = nrow(output_table) + 3, startCol = 1)
  openxlsx::setColWidths(wb, "for paper", cols = 1, widths = 38)
  openxlsx::setColWidths(wb, "for paper", cols = 2:ncol(output_table), widths = 24)
  openxlsx::freezePane(wb, "for paper", firstActiveRow = 2, firstActiveCol = 2)
  openxlsx::addStyle(
    wb,
    "for paper",
    openxlsx::createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE),
    rows = 1,
    cols = seq_len(ncol(output_table)),
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb,
    "for paper",
    openxlsx::createStyle(wrapText = TRUE, valign = "top"),
    rows = seq_len(nrow(output_table) + 3),
    cols = seq_len(ncol(output_table)),
    gridExpand = TRUE
  )

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  message("Wrote RF105B characteristics workbook: ", output_file)
  invisible(output_file)
}

write_rf105b_characteristics_workbook(survey)
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
) %>%
  mutate(
    collection_year = timepoint_collection_year(timepoint),
    timepoint_year_label = timepoint_label_with_year(timepoint)
  ) %>%
  relocate(collection_year, timepoint_year_label, .after = timepoint)
write_reviewed_csv(
  fuel_30_summary,
  "table_descriptive_fuel_use_past_month.csv"
)

# Integrated from 3.1_fuel_use_past_month_subset_workbook_20260921.R.
# Keep this block adjacent to the source CSV that defines its workbook contents.
local({
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop(
    "Package `openxlsx` is required to write the fuel-use subset workbook. ",
    "Run renv::restore() from the project root, then rerun this script.",
    call. = FALSE
  )
}
if (!requireNamespace("zip", quietly = TRUE)) {
  stop(
    "Package `zip` is required to repair the XLSX archive after openxlsx export. ",
    "Run renv::restore() from the project root, then rerun this script.",
    call. = FALSE
  )
}

source_csv <- file.path(
  dir_tables_reviewed,
  "table_descriptive_fuel_use_past_month.csv"
)
output_xlsx <- file.path(
  dir_tables_reviewed,
  "table_descriptive_fuel_use_past_month_subset.xlsx"
)

if (!file.exists(source_csv)) {
  stop("Missing source CSV for fuel-use subset workbook: ", source_csv, call. = FALSE)
}

format_percent_ci <- function(percent, ci_lower, ci_upper, digits = 1) {
  values <- c(percent, ci_lower, ci_upper)
  if (any(is.na(values))) {
    return("")
  }

  sprintf(
    paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
    percent, ci_lower, ci_upper
  )
}

fuel_use_subset_layout <- tibble::tribble(
  ~study_arm_overall, ~study_arm_label,      ~fuel_type,              ~fuel_method, ~source_variable,
  "comparison",       "Comparison group",    "LPG",                   "purchased",  "fuel_30_buy_lpg",
  "intervention",     "Intervention group",  "LPG",                   "purchased",  "fuel_30_buy_lpg",
  "comparison",       "Comparison group",    "LPG",                   "received",   "fuel_30_receive_lpg",
  "intervention",     "Intervention group",  "LPG",                   "received",   "fuel_30_receive_lpg",
  "comparison",       "Comparison group",    "Wood",                  "collected",  "fuel_30_collect_wood",
  "intervention",     "Intervention group",  "Wood",                  "collected",  "fuel_30_collect_wood",
  "comparison",       "Comparison group",    "Wood",                  "purchased",  "fuel_30_buy_wood",
  "intervention",     "Intervention group",  "Wood",                  "purchased",  "fuel_30_buy_wood",
  "comparison",       "Comparison group",    "Wood",                  "received",   "fuel_30_receive_wood",
  "intervention",     "Intervention group",  "Wood",                  "received",   "fuel_30_receive_wood",
  "comparison",       "Comparison group",    "Scraps/leaves/twigs",   "gathered",   "fuel_30_gather_scraps",
  "intervention",     "Intervention group",  "Scraps/leaves/twigs",   "gathered",   "fuel_30_gather_scraps",
  "comparison",       "Comparison group",    "Compressed rice husks", "purchased",  "fuel_30_buy_crh",
  "intervention",     "Intervention group",  "Compressed rice husks", "purchased",  "fuel_30_buy_crh",
  "comparison",       "Comparison group",    "Compressed rice husks", "received",   "fuel_30_receive_crh",
  "intervention",     "Intervention group",  "Compressed rice husks", "received",   "fuel_30_receive_crh",
  "comparison",       "Comparison group",    "Plastic",               "collected",  "fuel_30_other",
  "intervention",     "Intervention group",  "Plastic",               "collected",  "fuel_30_other"
) %>%
  dplyr::mutate(row_order = dplyr::row_number())

fuel_use_source <- readr::read_csv(source_csv, show_col_types = FALSE)

required_columns <- c(
  "timepoint", "study_arm_overall", "source_variable",
  "percent", "ci_lower", "ci_upper"
)
missing_columns <- setdiff(required_columns, names(fuel_use_source))
if (length(missing_columns) > 0) {
  stop(
    "Source CSV is missing required column(s): ",
    paste(missing_columns, collapse = ", "),
    call. = FALSE
  )
}

source_duplicates <- fuel_use_source %>%
  dplyr::filter(
    .data$timepoint %in% timepoint_levels,
    .data$study_arm_overall %in% arm_levels
  ) %>%
  dplyr::semi_join(
    fuel_use_subset_layout,
    by = c("study_arm_overall", "source_variable")
  ) %>%
  dplyr::count(.data$timepoint, .data$study_arm_overall, .data$source_variable) %>%
  dplyr::filter(.data$n > 1)

if (nrow(source_duplicates) > 0) {
  stop(
    "Source CSV has duplicate rows for the subset workbook keys.",
    call. = FALSE
  )
}

fuel_use_subset_values <- fuel_use_subset_layout %>%
  tidyr::crossing(timepoint = timepoint_levels) %>%
  dplyr::left_join(
    fuel_use_source %>%
      dplyr::filter(
        .data$timepoint %in% timepoint_levels,
        .data$study_arm_overall %in% arm_levels
      ) %>%
      dplyr::mutate(
        cell_value = purrr::pmap_chr(
          list(.data$percent, .data$ci_lower, .data$ci_upper),
          format_percent_ci
        )
      ) %>%
      dplyr::select(
        timepoint,
        study_arm_overall,
        source_variable,
        cell_value
      ),
    by = c("timepoint", "study_arm_overall", "source_variable")
  )

missing_cells <- fuel_use_subset_values %>%
  dplyr::filter(is.na(.data$cell_value)) %>%
  dplyr::select(
    study_arm_label,
    fuel_type,
    fuel_method,
    timepoint,
    source_variable
  )

if (nrow(missing_cells) > 0) {
  stop(
    "Could not fill all requested workbook cells from the source CSV. ",
    "First missing key: ",
    paste(unlist(missing_cells[1, ]), collapse = " | "),
    call. = FALSE
  )
}

fuel_use_subset_table <- fuel_use_subset_values %>%
  dplyr::mutate(timepoint = factor(.data$timepoint, levels = timepoint_levels)) %>%
  dplyr::arrange(.data$row_order, .data$timepoint) %>%
  dplyr::select(
    row_order,
    study_arm_label,
    fuel_type,
    fuel_method,
    timepoint,
    cell_value
  ) %>%
  tidyr::pivot_wider(names_from = timepoint, values_from = cell_value) %>%
  dplyr::arrange(.data$row_order) %>%
  dplyr::select(
    study_arm_label,
    fuel_type,
    fuel_method,
    dplyr::all_of(timepoint_levels)
  )

make_fuel_subset_workbook <- function() {
  wb <- openxlsx::createWorkbook()
  sheet_name <- "Sheet1"
  openxlsx::addWorksheet(wb, sheet_name)

  base_font <- "Times New Roman"
  header_style <- openxlsx::createStyle(
    fontName = base_font,
    fontSize = 10,
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    wrapText = TRUE,
    border = "TopBottomLeftRight",
    borderStyle = "medium"
  )
  timepoint_style <- openxlsx::createStyle(
    fontName = base_font,
    fontSize = 10,
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    wrapText = TRUE
  )
  row_label_style <- openxlsx::createStyle(
    fontName = base_font,
    fontSize = 10,
    halign = "left",
    valign = "center",
    wrapText = TRUE
  )
  value_style <- openxlsx::createStyle(
    fontName = base_font,
    fontSize = 10,
    halign = "right",
    valign = "center",
    wrapText = TRUE
  )

  openxlsx::writeData(wb, sheet_name, "Timepoint", startRow = 1, startCol = 4, colNames = FALSE)
  openxlsx::mergeCells(wb, sheet_name, cols = 4:6, rows = 1)
  openxlsx::writeData(
    wb,
    sheet_name,
    matrix(c("Study arm (overall)", "Fuel type", "Fuel method"), nrow = 1),
    startRow = 2,
    startCol = 1,
    colNames = FALSE
  )
  openxlsx::mergeCells(wb, sheet_name, cols = 1, rows = 2:4)
  openxlsx::mergeCells(wb, sheet_name, cols = 2, rows = 2:4)
  openxlsx::mergeCells(wb, sheet_name, cols = 3, rows = 2:4)
  openxlsx::writeData(
    wb,
    sheet_name,
    matrix(c("Baseline", "Midline", "Endline"), nrow = 1),
    startRow = 2,
    startCol = 4,
    colNames = FALSE
  )
  openxlsx::writeData(
    wb,
    sheet_name,
    matrix(rep("% (95% CI)", 3), nrow = 1),
    startRow = 3,
    startCol = 4,
    colNames = FALSE
  )

  openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = 4:6, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet_name, header_style, rows = 2:4, cols = 1:3, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet_name, timepoint_style, rows = 2:3, cols = 4:6, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet_name, row_label_style, rows = 5:22, cols = 1:3, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet_name, value_style, rows = 5:22, cols = 4:6, gridExpand = TRUE)

  openxlsx::setColWidths(wb, sheet_name, cols = 1:3, widths = 13)
  openxlsx::setColWidths(wb, sheet_name, cols = 4:5, widths = 10)
  openxlsx::setColWidths(wb, sheet_name, cols = 6, widths = 13)
  openxlsx::setRowHeights(wb, sheet_name, rows = 3, heights = 26.4)
  openxlsx::setRowHeights(wb, sheet_name, rows = 5:22, heights = 27)

  wb
}

regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x, perl = TRUE)
}

extract_xml_attr <- function(x, attr) {
  pattern <- paste0(".*\\b", attr, "=\"([^\"]*)\".*")
  sub(pattern, "\\1", x, perl = TRUE)
}

repair_missing_drawing_relationships <- function(path) {
  stage_dir <- tempfile("fuel_subset_xlsx_")
  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(stage_dir, recursive = TRUE, force = TRUE), add = TRUE)

  utils::unzip(path, exdir = stage_dir)

  rels_dir <- file.path(stage_dir, "xl", "worksheets", "_rels")
  rels_files <- list.files(rels_dir, pattern = "\\.rels$", full.names = TRUE)
  missing_part_names <- character()

  for (rels_file in rels_files) {
    rels_xml <- paste(readLines(rels_file, warn = FALSE), collapse = "")
    rel_matches <- regmatches(
      rels_xml,
      gregexpr(
        "<Relationship[^>]+Type=\"[^\"]+/(drawing|vmlDrawing)\"[^>]*/>",
        rels_xml,
        perl = TRUE
      )
    )[[1]]

    if (length(rel_matches) == 0 || identical(rel_matches, character(0))) {
      next
    }

    source_dir <- dirname(dirname(rels_file))

    for (rel in rel_matches) {
      target <- extract_xml_attr(rel, "Target")
      target_path <- normalizePath(
        file.path(source_dir, target),
        winslash = "/",
        mustWork = FALSE
      )

      if (file.exists(target_path)) {
        next
      }

      rels_xml <- gsub(rel, "", rels_xml, fixed = TRUE)
      rel_part <- substring(
        target_path,
        nchar(normalizePath(stage_dir, winslash = "/", mustWork = FALSE)) + 1L
      )
      missing_part_names <- c(missing_part_names, rel_part)
    }

    writeLines(rels_xml, rels_file, useBytes = TRUE)
  }

  missing_part_names <- unique(missing_part_names)
  content_types_file <- file.path(stage_dir, "[Content_Types].xml")
  if (length(missing_part_names) > 0 && file.exists(content_types_file)) {
    content_types <- paste(readLines(content_types_file, warn = FALSE), collapse = "")
    for (part_name in missing_part_names) {
      content_types <- gsub(
        paste0("<Override[^>]+PartName=\"", regex_escape(part_name), "\"[^>]*/>"),
        "",
        content_types,
        perl = TRUE
      )
    }
    writeLines(content_types, content_types_file, useBytes = TRUE)
  }

  archive_file <- tempfile(fileext = ".xlsx")
  files <- list.files(stage_dir, all.files = TRUE, no.. = TRUE, recursive = TRUE)
  zip::zipr(
    archive_file,
    files = files,
    root = stage_dir,
    mode = "mirror",
    include_directories = FALSE
  )
  file.copy(archive_file, path, overwrite = TRUE)

  invisible(path)
}

wb <- make_fuel_subset_workbook()
sheet_name <- "Sheet1"

openxlsx::writeData(
  wb,
  sheet_name,
  fuel_use_subset_table[, 1:3],
  startRow = 5,
  startCol = 1,
  colNames = FALSE
)
openxlsx::writeData(
  wb,
  sheet_name,
  fuel_use_subset_table[, 4:6],
  startRow = 5,
  startCol = 4,
  colNames = FALSE
)

dir.create(dirname(output_xlsx), recursive = TRUE, showWarnings = FALSE)
openxlsx::saveWorkbook(wb, output_xlsx, overwrite = TRUE)
repair_missing_drawing_relationships(output_xlsx)
message("Wrote fuel-use past-month subset workbook: ", output_xlsx)

})


# LPG and wood overlap among households reporting LPG use in the past 30 days.
# This table is calculated from all deduplicated household survey records and
# reports both the requested denominator (households reporting LPG use) and the
# full household denominator for transparency.
if (all(c("fuel_30_lpg", "fuel_30_wood") %in% names(survey))) {
  fuel_30_lpg_wood_base <- survey %>%
    mutate(
      lpg_30_yn = make_yn(fuel_30_lpg),
      wood_30_yn = make_yn(fuel_30_wood),
      lpg_and_wood_30_yn = case_when(
        lpg_30_yn == 1 & wood_30_yn == 1 ~ 1L,
        lpg_30_yn == 1 & wood_30_yn == 0 ~ 0L,
        TRUE ~ NA_integer_
      )
    )

  summarise_lpg_wood_overlap <- function(df, summary_level) {
    df %>%
      summarise(
        n_households = n(),
        n_lpg_nonmissing = sum(!is.na(lpg_30_yn)),
        n_lpg = sum(lpg_30_yn == 1, na.rm = TRUE),
        n_lpg_and_wood = sum(lpg_and_wood_30_yn == 1, na.rm = TRUE),
        n_lpg_with_wood_missing = sum(lpg_30_yn == 1 & is.na(wood_30_yn), na.rm = TRUE),
        pct_lpg_households_also_wood = if_else(
          n_lpg > 0,
          100 * n_lpg_and_wood / n_lpg,
          NA_real_
        ),
        pct_all_households_lpg_and_wood = if_else(
          n_households > 0,
          100 * n_lpg_and_wood / n_households,
          NA_real_
        ),
        .groups = "drop"
      ) %>%
      mutate(summary_level = summary_level, .before = 1)
  }

  lpg_wood_overlap_by_arm_timepoint <- fuel_30_lpg_wood_base %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise_lpg_wood_overlap("arm_timepoint")

  lpg_wood_overlap_by_timepoint <- fuel_30_lpg_wood_base %>%
    group_by(timepoint) %>%
    summarise_lpg_wood_overlap("all_arms_timepoint") %>%
    mutate(study_arm_overall = "all_arms", .after = timepoint)

  lpg_wood_overlap_overall <- fuel_30_lpg_wood_base %>%
    summarise_lpg_wood_overlap("all_records") %>%
    mutate(
      timepoint = "all_timepoints",
      study_arm_overall = "all_arms",
      .after = summary_level
    )

  fuel_30_lpg_wood_overlap <- bind_rows(
    lpg_wood_overlap_by_arm_timepoint,
    lpg_wood_overlap_by_timepoint,
    lpg_wood_overlap_overall
  ) %>%
    mutate(across(starts_with("pct_"), ~ round(.x, 1)))

  write_reviewed_csv(
    fuel_30_lpg_wood_overlap,
    "table_descriptive_lpg_users_also_wood.csv"
  )
}

# LPG shortage before scheduled refill. The current cleaned survey stores this
# timing in fuel_use_non_lpg_freq_cook, which is the number of days before the
# next refill when the household ran out of LPG for cooking. Values greater than
# 200 are excluded in the main summary to match the checked legacy threshold.
# The table includes arm-specific rows and all-arm pooled rows.
if ("fuel_use_non_lpg_freq_cook" %in% names(survey)) {
  lpg_refill_runout_base <- survey %>%
    mutate(
      days_before_refill_raw = suppressWarnings(as.numeric(fuel_use_non_lpg_freq_cook)),
      days_before_refill_in_range = if_else(
        !is.na(days_before_refill_raw) & days_before_refill_raw <= 60,
        days_before_refill_raw,
        NA_real_
      ),
      lpg_ran_out_before_refill = case_when(
        is.na(days_before_refill_raw) ~ NA_integer_,
        days_before_refill_raw > 60 ~ NA_integer_,
        days_before_refill_raw > 0 ~ 1L,
        days_before_refill_raw == 0 ~ 0L,
        TRUE ~ NA_integer_
      ),
      excluded_days_before_refill_gt_60 = !is.na(days_before_refill_raw) &
        days_before_refill_raw > 60,
      days_before_refill_all_households = case_when(
        excluded_days_before_refill_gt_60 ~ NA_real_,
        lpg_ran_out_before_refill == 1L ~ days_before_refill_in_range,
        TRUE ~ 0
      )
    )

  summarise_lpg_refill_runout <- function(df, summary_level, population_rule) {
    df %>%
      summarise(
        n_households = n(),
        n_runout_response_nonmissing = sum(!is.na(lpg_ran_out_before_refill)),
        n_ran_out_before_refill = sum(lpg_ran_out_before_refill == 1, na.rm = TRUE),
        n_did_not_run_out_before_refill = sum(lpg_ran_out_before_refill == 0, na.rm = TRUE),
        n_days_before_refill_gt_60_excluded = sum(excluded_days_before_refill_gt_60, na.rm = TRUE),
        n_all_household_days_summary_nonmissing =
          sum(!is.na(days_before_refill_all_households)),
        pct_ran_out_before_refill_all_households = if_else(
          n_households > 0,
          100 * n_ran_out_before_refill / n_households,
          NA_real_
        ),
        pct_ran_out_before_refill_among_nonmissing = if_else(
          n_runout_response_nonmissing > 0,
          100 * n_ran_out_before_refill / n_runout_response_nonmissing,
          NA_real_
        ),
        mean_days_before_refill_all_households = mean(
          days_before_refill_all_households,
          na.rm = TRUE
        ),
        sd_days_before_refill_all_households = sd(
          days_before_refill_all_households,
          na.rm = TRUE
        ),
        mean_days_before_refill_among_runout = mean(
          days_before_refill_in_range[lpg_ran_out_before_refill == 1],
          na.rm = TRUE
        ),
        sd_days_before_refill_among_runout = sd(
          days_before_refill_in_range[lpg_ran_out_before_refill == 1],
          na.rm = TRUE
        ),
        median_days_before_refill_among_runout = median(
          days_before_refill_in_range[lpg_ran_out_before_refill == 1],
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      mutate(
        summary_level = summary_level,
        population_rule = population_rule,
        .before = 1
      )
  }

  lpg_refill_runout_by_arm <- lpg_refill_runout_base %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise_lpg_refill_runout(
      summary_level = "arm_timepoint",
      population_rule = "arm_specific"
    )

  lpg_refill_runout_pooled <- lpg_refill_runout_base %>%
    group_by(timepoint) %>%
    summarise_lpg_refill_runout(
      summary_level = "all_arms_timepoint",
      population_rule = "both_arms_pooled"
    ) %>%
    mutate(study_arm_overall = "all_arms", .after = timepoint)
  lpg_refill_runout_summary <- bind_rows(
    lpg_refill_runout_by_arm,
    lpg_refill_runout_pooled
  ) %>%
    mutate(
      across(starts_with("pct_"), ~ round(.x, 1)),
      across(
        c(mean_days_before_refill_all_households,
          sd_days_before_refill_all_households,
          mean_days_before_refill_among_runout,
          sd_days_before_refill_among_runout,
          median_days_before_refill_among_runout),
        ~ if_else(is.nan(.x), NA_real_, round(.x, 1))
      ),
      denominator_note = case_when(
        summary_level == "arm_timepoint" ~
          "arm-specific households at this timepoint",
        summary_level == "all_arms_timepoint" ~
          "all households pooled across both arms at this timepoint",
        TRUE ~ NA_character_
      ),
      all_household_days_note = paste(
        "Households without a reported LPG runout are assigned zero days.",
        "Values greater than 60 days are excluded from the days calculation;",
        "n_all_household_days_summary_nonmissing reports the resulting denominator."
      )
    ) %>%
    arrange(summary_level, timepoint, study_arm_overall)

  write_reviewed_csv(
    lpg_refill_runout_summary,
    "table_descriptive_lpg_runout_before_refill.csv"
  )
}

# LPG purchase cost in the past 30 days. The cleaned survey stores this in
# buy_lpg_cost, denominated in BDT. The table keeps both all nonmissing responses
# and positive-cost-only summaries because a zero can indicate no paid LPG
# purchase in the recall window or a true zero-cost refill depending on context.
if ("buy_lpg_cost" %in% names(survey)) {
  lpg_purchase_cost_base <- survey %>%
    mutate(
      buy_lpg_cost_bdt = as_number(buy_lpg_cost),
      exchange_bdt_per_usd = exchange_bdt_per_usd[as.character(timepoint)],
      buy_lpg_cost_usd = buy_lpg_cost_bdt / exchange_bdt_per_usd,
      paid_lpg_gt0 = case_when(
        is.na(buy_lpg_cost_bdt) ~ NA_integer_,
        buy_lpg_cost_bdt > 0 ~ 1L,
        buy_lpg_cost_bdt == 0 ~ 0L,
        TRUE ~ NA_integer_
      )
    )

  summarise_lpg_purchase_cost <- function(df, summary_level) {
    df %>%
      summarise(
        n_households = n(),
        n_cost_nonmissing = sum(!is.na(buy_lpg_cost_bdt)),
        n_paid_gt0 = sum(paid_lpg_gt0 == 1, na.rm = TRUE),
        pct_paid_gt0_among_nonmissing = if_else(
          n_cost_nonmissing > 0,
          100 * n_paid_gt0 / n_cost_nonmissing,
          NA_real_
        ),
        mean_cost_bdt_all_nonmissing = mean(buy_lpg_cost_bdt, na.rm = TRUE),
        sd_cost_bdt_all_nonmissing = sd(buy_lpg_cost_bdt, na.rm = TRUE),
        median_cost_bdt_all_nonmissing = median(buy_lpg_cost_bdt, na.rm = TRUE),
        p25_cost_bdt_all_nonmissing = quantile(buy_lpg_cost_bdt, 0.25, na.rm = TRUE, names = FALSE),
        p75_cost_bdt_all_nonmissing = quantile(buy_lpg_cost_bdt, 0.75, na.rm = TRUE, names = FALSE),
        min_cost_bdt_all_nonmissing = min(buy_lpg_cost_bdt, na.rm = TRUE),
        max_cost_bdt_all_nonmissing = max(buy_lpg_cost_bdt, na.rm = TRUE),
        mean_cost_usd_all_nonmissing = mean(buy_lpg_cost_usd, na.rm = TRUE),
        sd_cost_usd_all_nonmissing = sd(buy_lpg_cost_usd, na.rm = TRUE),
        median_cost_usd_all_nonmissing = median(buy_lpg_cost_usd, na.rm = TRUE),
        p25_cost_usd_all_nonmissing = quantile(buy_lpg_cost_usd, 0.25, na.rm = TRUE, names = FALSE),
        p75_cost_usd_all_nonmissing = quantile(buy_lpg_cost_usd, 0.75, na.rm = TRUE, names = FALSE),
        min_cost_usd_all_nonmissing = min(buy_lpg_cost_usd, na.rm = TRUE),
        max_cost_usd_all_nonmissing = max(buy_lpg_cost_usd, na.rm = TRUE),
        mean_cost_bdt_positive = mean(buy_lpg_cost_bdt[buy_lpg_cost_bdt > 0], na.rm = TRUE),
        sd_cost_bdt_positive = sd(buy_lpg_cost_bdt[buy_lpg_cost_bdt > 0], na.rm = TRUE),
        median_cost_bdt_positive = median(buy_lpg_cost_bdt[buy_lpg_cost_bdt > 0], na.rm = TRUE),
        p25_cost_bdt_positive = quantile(buy_lpg_cost_bdt[buy_lpg_cost_bdt > 0], 0.25, na.rm = TRUE, names = FALSE),
        p75_cost_bdt_positive = quantile(buy_lpg_cost_bdt[buy_lpg_cost_bdt > 0], 0.75, na.rm = TRUE, names = FALSE),
        min_cost_bdt_positive = min(buy_lpg_cost_bdt[buy_lpg_cost_bdt > 0], na.rm = TRUE),
        max_cost_bdt_positive = max(buy_lpg_cost_bdt[buy_lpg_cost_bdt > 0], na.rm = TRUE),
        mean_cost_usd_positive = mean(buy_lpg_cost_usd[buy_lpg_cost_bdt > 0], na.rm = TRUE),
        sd_cost_usd_positive = sd(buy_lpg_cost_usd[buy_lpg_cost_bdt > 0], na.rm = TRUE),
        median_cost_usd_positive = median(buy_lpg_cost_usd[buy_lpg_cost_bdt > 0], na.rm = TRUE),
        p25_cost_usd_positive = quantile(buy_lpg_cost_usd[buy_lpg_cost_bdt > 0], 0.25, na.rm = TRUE, names = FALSE),
        p75_cost_usd_positive = quantile(buy_lpg_cost_usd[buy_lpg_cost_bdt > 0], 0.75, na.rm = TRUE, names = FALSE),
        min_cost_usd_positive = min(buy_lpg_cost_usd[buy_lpg_cost_bdt > 0], na.rm = TRUE),
        max_cost_usd_positive = max(buy_lpg_cost_usd[buy_lpg_cost_bdt > 0], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        summary_level = summary_level,
        denominator_note = "Percent paid >0 uses households with nonmissing buy_lpg_cost. Positive-cost summaries are restricted to households with buy_lpg_cost > 0.",
        across(starts_with("pct_"), ~ if_else(is.nan(.x) | is.infinite(.x), NA_real_, round(.x, 1))),
        across(ends_with("_bdt_all_nonmissing") | ends_with("_bdt_positive"),
               ~ if_else(is.nan(.x) | is.infinite(.x), NA_real_, round(.x, 0))),
        across(ends_with("_usd_all_nonmissing") | ends_with("_usd_positive"),
               ~ if_else(is.nan(.x) | is.infinite(.x), NA_real_, round(.x, 2)))
      )
  }

  lpg_purchase_cost_by_arm <- lpg_purchase_cost_base %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise_lpg_purchase_cost("arm_timepoint")

  lpg_purchase_cost_all_arms <- lpg_purchase_cost_base %>%
    group_by(timepoint) %>%
    summarise_lpg_purchase_cost("all_arms_timepoint") %>%
    mutate(study_arm_overall = "all_arms", .after = timepoint)

  lpg_purchase_cost_summary <- bind_rows(
    lpg_purchase_cost_by_arm,
    lpg_purchase_cost_all_arms
  ) %>%
    select(
      summary_level, timepoint, study_arm_overall,
      n_households, n_cost_nonmissing, n_paid_gt0, pct_paid_gt0_among_nonmissing,
      everything(), denominator_note
    ) %>%
    arrange(
      summary_level,
      factor(as.character(timepoint), levels = timepoint_levels),
      factor(as.character(study_arm_overall), levels = c(arm_levels, "all_arms"))
    )

  write_reviewed_csv(
    lpg_purchase_cost_summary,
    "table_descriptive_lpg_purchase_cost.csv"
  )
}
# Foods households would cook with each fuel. These follow-up questions ask which
# fuel a household would use for common foods. Codes are retained in the output;
# label mapping follows the reviewed fuel-choice coding used for these variables.
food_specific_fuel_vars <- tibble(
  source_variable = c(
    "fuel_rice",
    "fuel_kitchuri",
    "fuel_meat",
    "fuel_vegetables",
    "fuel_snacks",
    "fuel_water"
  ),
  food_item = c(
    "rice",
    "kitchuri",
    "meat",
    "vegetables",
    "snacks",
    "water"
  ),
  food_label = c(
    "Rice",
    "Kitchuri",
    "Meat",
    "Vegetables",
    "Snacks",
    "Water"
  )
) %>%
  filter(source_variable %in% names(survey))

food_specific_fuel_labels <- tibble(
  fuel_code = c("1", "2"),
  fuel_label = c("Non-LPG fuel", "LPG")
)

if (nrow(food_specific_fuel_vars) > 0) {
  food_specific_fuel_long <- survey %>%
    select(timepoint, study_arm_overall, all_of(food_specific_fuel_vars$source_variable)) %>%
    pivot_longer(
      cols = all_of(food_specific_fuel_vars$source_variable),
      names_to = "source_variable",
      values_to = "fuel_code"
    ) %>%
    mutate(
      fuel_code = str_squish(as.character(fuel_code)),
      fuel_code = na_if(fuel_code, "")
    ) %>%
    left_join(food_specific_fuel_vars, by = "source_variable")

  summarise_food_specific_fuel <- function(df, summary_level) {
    denominators <- df %>%
      group_by(timepoint, study_arm_overall, source_variable, food_item, food_label) %>%
      summarise(
        n_households = n(),
        n_nonmissing = sum(!is.na(fuel_code)),
        .groups = "drop"
      )

    counts <- df %>%
      filter(!is.na(fuel_code)) %>%
      count(
        timepoint, study_arm_overall, source_variable, food_item, food_label,
        fuel_code,
        name = "n_selected"
      )

    denominators %>%
      tidyr::crossing(food_specific_fuel_labels) %>%
      left_join(
        counts,
        by = c(
          "timepoint", "study_arm_overall", "source_variable", "food_item",
          "food_label", "fuel_code"
        )
      ) %>%
      mutate(
        n_selected = replace_na(n_selected, 0L),
        percent_among_nonmissing = if_else(
          n_nonmissing > 0,
          100 * n_selected / n_nonmissing,
          NA_real_
        ),
        pct_all_households = if_else(
          n_households > 0,
          100 * n_selected / n_households,
          NA_real_
        ),
        summary_level = summary_level,
        denominator_note = "Percent among nonmissing uses households with nonmissing food-specific fuel response; pct_all_households uses all deduplicated household-timepoint records.",
        coding_note = "fuel_code retained from clean_final; reviewed label mapping: 1 = Non-LPG fuel, 2 = LPG.",
        across(c(percent_among_nonmissing, pct_all_households), ~ round(.x, 1))
      )
  }

  food_specific_fuel_by_arm <- food_specific_fuel_long %>%
    summarise_food_specific_fuel("arm_timepoint")

  food_specific_fuel_all_arms <- food_specific_fuel_long %>%
    select(-study_arm_overall) %>%
    mutate(study_arm_overall = "all_arms") %>%
    summarise_food_specific_fuel("all_arms_timepoint")

  food_specific_fuel_summary <- bind_rows(
    food_specific_fuel_by_arm,
    food_specific_fuel_all_arms
  ) %>%
    select(
      summary_level, timepoint, study_arm_overall, source_variable,
      food_item, food_label, fuel_code, fuel_label,
      n_households, n_nonmissing, n_selected, percent_among_nonmissing,
      pct_all_households, denominator_note, coding_note
    ) %>%
    arrange(
      summary_level,
      factor(as.character(timepoint), levels = timepoint_levels),
      factor(as.character(study_arm_overall), levels = c(arm_levels, "all_arms")),
      food_item,
      factor(fuel_code, levels = food_specific_fuel_labels$fuel_code)
    )

  write_reviewed_csv(
    food_specific_fuel_summary,
    "table_descriptive_food_specific_fuel_choice.csv"
  )
}

# Reasons for wood/non-LPG use. The survey reason codes are:
# 1 = not enough other fuel, 2 = prefer food cooked with wood, 66 = other.
# Baseline summaries use the comparison arm only, because intervention households
# had not yet received LPG. Midline and endline summaries include both arms.
# Some households answered both gathered-wood and purchased-wood reason items;
# the primary categorized reason below gives priority to option 1, then 2, then 66,
# and a companion table records discordant paired responses.
if (all(c("gather_wood_reason", "buy_wood_reason") %in% names(survey))) {
  non_lpg_reason_levels <- c(
    "not_enough_other_fuel_or_lpg_ran_out",
    "preferred_food_cooked_on_biomass",
    "other_reason"
  )

  non_lpg_reason_base <- survey %>%
    mutate(
      fuel_30_wood_yn = if ("fuel_30_wood" %in% names(.)) make_yn(fuel_30_wood) else NA_integer_,
      gather_reason_code = suppressWarnings(as.numeric(gather_wood_reason)),
      buy_reason_code = suppressWarnings(as.numeric(buy_wood_reason)),
      has_reason_response = !is.na(gather_reason_code) | !is.na(buy_reason_code),
      reported_non_lpg_or_wood = fuel_30_wood_yn == 1 | has_reason_response,
      reason_not_enough_other_fuel = gather_reason_code == 1 | buy_reason_code == 1,
      reason_prefer_biomass_food = gather_reason_code == 2 | buy_reason_code == 2,
      reason_other = gather_reason_code == 66 | buy_reason_code == 66,
      primary_non_lpg_reason = case_when(
        reason_not_enough_other_fuel ~ "not_enough_other_fuel_or_lpg_ran_out",
        reason_prefer_biomass_food ~ "preferred_food_cooked_on_biomass",
        reason_other ~ "other_reason",
        TRUE ~ NA_character_
      ),
      primary_non_lpg_reason = factor(primary_non_lpg_reason, levels = non_lpg_reason_levels),
      reason_response_discordant = !is.na(gather_reason_code) &
        !is.na(buy_reason_code) & gather_reason_code != buy_reason_code
    )

  non_lpg_reason_counts <- non_lpg_reason_base %>%
    filter(reported_non_lpg_or_wood, has_reason_response) %>%
    add_all_arms_rows() %>%
    mutate(
      summary_level = if_else(
        study_arm_overall == "all_arms",
        "all_arms_timepoint",
        "arm_timepoint"
      ),
      population_rule = case_when(
        timepoint == "baseline" & study_arm_overall == "comparison" ~
          "baseline_comparison_arm_sentence_denominator",
        timepoint == "baseline" & study_arm_overall == "all_arms" ~
          "baseline_both_arms_descriptive",
        timepoint == "baseline" ~ "baseline_arm_specific_descriptive",
        timepoint %in% c("midline", "endline") & study_arm_overall == "all_arms" ~
          "followup_both_arms_sentence_denominator",
        timepoint %in% c("midline", "endline") ~
          "followup_arm_specific_descriptive",
        TRUE ~ NA_character_
      ),
      population_definition = case_when(
        timepoint == "baseline" & study_arm_overall == "comparison" ~
          "baseline comparison-arm households only",
        timepoint == "baseline" & study_arm_overall == "intervention" ~
          "baseline intervention-arm households only; descriptive context, not used for the manuscript sentence",
        timepoint == "baseline" & study_arm_overall == "all_arms" ~
          "baseline households pooled across both arms; descriptive context, not used for the manuscript sentence",
        timepoint %in% c("midline", "endline") & study_arm_overall == "all_arms" ~
          "all households pooled across both study arms",
        timepoint %in% c("midline", "endline") ~
          "households in this study arm",
        TRUE ~ NA_character_
      )
    ) %>%
    count(timepoint, summary_level, study_arm_overall, population_rule,
          population_definition, primary_non_lpg_reason, name = "n_households") %>%
    group_by(timepoint, summary_level, study_arm_overall, population_rule,
             population_definition) %>%
    tidyr::complete(
      primary_non_lpg_reason = factor(non_lpg_reason_levels, levels = non_lpg_reason_levels),
      fill = list(n_households = 0)
    ) %>%
    mutate(
      denominator = sum(n_households),
      percent = if_else(denominator > 0, round(100 * n_households / denominator, 1), NA_real_),
      reason_label = recode(
        as.character(primary_non_lpg_reason),
        not_enough_other_fuel_or_lpg_ran_out = "ran out of LPG or did not have enough other fuel",
        preferred_food_cooked_on_biomass = "preferred food cooked on biomass",
        other_reason = "used wood for other reasons",
        .default = as.character(primary_non_lpg_reason)
      ),
      denominator_definition = paste0(
        population_definition,
        "; households with wood/non-LPG use or a wood-use reason response; one primary reason per household"
      )
    ) %>%
    ungroup() %>%
    arrange(timepoint, summary_level, study_arm_overall, primary_non_lpg_reason)

  non_lpg_reason_sentence <- non_lpg_reason_counts %>%
    mutate(
      used_for_manuscript_sentence =
        (timepoint == "baseline" & study_arm_overall == "comparison") |
        (timepoint %in% c("midline", "endline") & study_arm_overall == "all_arms")
    )

  non_lpg_reason_lookup <- non_lpg_reason_sentence %>%
    select(timepoint, population_rule, population_definition, denominator,
           denominator_definition, primary_non_lpg_reason, n_households, percent) %>%
    tidyr::pivot_wider(
      names_from = primary_non_lpg_reason,
      values_from = c(n_households, percent),
      names_sep = "__"
    )

  non_lpg_reason_qa <- non_lpg_reason_base %>%
    filter(reported_non_lpg_or_wood) %>%
    add_all_arms_rows() %>%
    mutate(
      population_rule = case_when(
        timepoint == "baseline" & study_arm_overall == "comparison" ~
          "baseline_comparison_arm_sentence_denominator",
        timepoint == "baseline" & study_arm_overall == "all_arms" ~
          "baseline_both_arms_descriptive",
        timepoint == "baseline" ~ "baseline_arm_specific_descriptive",
        timepoint %in% c("midline", "endline") & study_arm_overall == "all_arms" ~
          "followup_both_arms_sentence_denominator",
        timepoint %in% c("midline", "endline") ~
          "followup_arm_specific_descriptive",
        TRUE ~ NA_character_
      )
    ) %>%
    group_by(timepoint, population_rule, study_arm_overall) %>%
    summarise(
      n_households = n(),
      n_with_any_reason_response = sum(has_reason_response),
      n_discordant_gather_buy_reason = sum(reason_response_discordant, na.rm = TRUE),
      n_not_enough_other_fuel_any_response = sum(reason_not_enough_other_fuel, na.rm = TRUE),
      n_prefer_biomass_food_any_response = sum(reason_prefer_biomass_food, na.rm = TRUE),
      n_other_any_response = sum(reason_other, na.rm = TRUE),
      .groups = "drop"
    )

  write_reviewed_csv(
    non_lpg_reason_counts,
    "table_descriptive_nonlpg_reason_counts.csv"
  )
  write_reviewed_csv(
    non_lpg_reason_sentence,
    "table_descriptive_nonlpg_reason_sentence.csv"
  )
  write_reviewed_csv(
    non_lpg_reason_qa,
    "table_descriptive_nonlpg_reason_qa.csv",
    subfolder = "qa"
  )

  # Backward-compatible baseline-only outputs now use the corrected baseline
  # denominator: comparison-arm households only.
  write_reviewed_csv(
    non_lpg_reason_counts,
    "table_descriptive_baseline_nonlpg_reason_counts.csv"
  )
  write_reviewed_csv(
    non_lpg_reason_qa %>% filter(timepoint == "baseline"),
    "table_descriptive_baseline_nonlpg_reason_qa.csv",
    subfolder = "qa"
  )
}
fuel_30_plot_data <- fuel_30_summary %>%
  filter(n_nonmissing > 0, !is.na(percent)) %>%
  filter(source_variable %in% c(
    "fuel_30_gather_scraps", "fuel_30_collect_wood", "fuel_30_receive_wood",
    "fuel_30_buy_wood", "fuel_30_receive_lpg", "fuel_30_buy_lpg",
    "fuel_30_receive_crh", "fuel_30_buy_crh", "fuel_30_other"
  )) %>%
  mutate(
    timepoint_year_label = factor(
      timepoint_year_label,
      levels = timepoint_label_with_year_levels,
      ordered = TRUE
    )
  )

fig_fuel_30 <- ggplot(
  fuel_30_plot_data,
  aes(x = timepoint_year_label, y = percent, fill = study_arm_overall)
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
    x = "Timepoint (data collection year)",
    y = "Households using fuel in past 30 days",
    fill = "Study arm"
  )
write_plot_if_data(
  fuel_30_plot_data,
  fig_fuel_30,
  "fig_descriptive_fuel_use_past_month.png",
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
  add_all_arms_rows() %>%
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
  "table_descriptive_lpg_duration_household_size.csv"
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
  "table_descriptive_lpg_duration_range_qa.csv",
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
  "fig_descriptive_lpg_duration_household_size.png",
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
  "table_descriptive_livelihood_training_skills.csv"
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
  "fig_descriptive_livelihood_training_skills.png",
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

# Food shortage coping outputs are separated by construct so the denominators
# are transparent:
#   1. Food insufficiency and coping actions selected.
#   2. Weekly frequency categories for the five food coping actions.
#   3. Most difficult and easiest coping action rankings.
#   4. A Coping Strategies Index (CSI) using available weekly frequency
#      fields and empirical severity weights derived from the survey ranking
#      questions about the most difficult and easiest coping actions.
food_shortage_status_labels <- tibble(
  source_variable = c(
    "food_insufficient_nutrition", "food_didnt_want", "food_cant_afford_2wk"
  ),
  outcome_name = source_variable,
  outcome_label = c(
    "Food insufficient for nutrition",
    "Ate food not wanted",
    "Could not afford enough food or did not have enough food/money in prior 2 weeks"
  )
) %>%
  filter(source_variable %in% names(survey))

food_frequency_labels <- tibble(
  source_variable = c("borrow_food", "reduce_food", "reduce_meals", "not_eat", "restrict_food"),
  outcome_name = source_variable,
  outcome_label = c(
    "Borrowed food",
    "Reduced food per meal",
    "Reduced meals per day",
    "Skipped all meals on some days",
    "Restricted adult food so children could eat"
  )
) %>%
  filter(source_variable %in% names(survey))

weekly_frequency_levels <- tibble(
  frequency_code = c("0", "1", "2", "3", "4"),
  frequency_label = c("Never", "One or two days", "Three to four days", "Five to six days", "Every day"),
  frequency_days_midpoint = c(0, 1.5, 3.5, 5.5, 7)
)

clean_coping_code <- function(x, missing_codes = c(77, 99)) {
  x_chr <- str_squish(as.character(x))
  suppressWarnings(x_num <- as.numeric(x_chr))
  out <- if_else(is.na(x) | x_chr == "", NA_character_, as.character(as.integer(x_num)))
  out[!is.na(x_num) & x_num %in% missing_codes] <- NA_character_
  out
}

food_multiselect_option <- function(df, prefix, option_code) {
  candidates <- c(
    paste0(prefix, option_code),
    paste0(prefix, ".", option_code),
    paste0(prefix, "/", option_code)
  )
  candidates <- candidates[candidates %in% names(df)]
  if (length(candidates) == 0) {
    return(rep(NA_integer_, nrow(df)))
  }
  option_values <- lapply(candidates, function(var) make_yn(df[[var]]))
  Reduce(dplyr::coalesce, option_values)
}

summarise_binary_percent <- function(df, labels, outcome_group) {
  available_vars <- labels$source_variable[labels$source_variable %in% names(df)]
  if (length(available_vars) == 0) {
    return(tibble())
  }

  df %>%
    select(any_of(c("timepoint", "study_arm_overall", available_vars))) %>%
    add_all_arms_rows() %>%
    pivot_longer(
      cols = all_of(available_vars),
      names_to = "source_variable",
      values_to = "raw_value",
      values_transform = list(raw_value = as.character)
    ) %>%
    left_join(labels, by = "source_variable") %>%
    mutate(value_yn = make_yn(raw_value)) %>%
    group_by(timepoint, study_arm_overall, outcome_name, outcome_label, source_variable) %>%
    summarise(
      n_total = n(),
      n_nonmissing = sum(!is.na(value_yn)),
      n_yes = sum(value_yn == 1, na.rm = TRUE),
      percent = if_else(n_nonmissing > 0, 100 * n_yes / n_nonmissing, NA_real_),
      pct_all_households = if_else(n_total > 0, 100 * n_yes / n_total, NA_real_),
      .groups = "drop"
    ) %>%
    mutate(
      outcome_group = outcome_group,
      unit = "percent",
      population = "all_deduplicated_household_timepoint_records",
      denominator_type = "nonmissing response for this item",
      percent = round(percent, 1),
      pct_all_households = round(pct_all_households, 1)
    ) %>%
    select(
      timepoint, study_arm_overall, outcome_group, outcome_name, outcome_label,
      source_variable, unit, population, denominator_type, n_total, n_nonmissing,
      n_yes, percent, pct_all_households
    )
}

summarise_food_action_option <- function(option_code, option_label) {
  if (!"food_cant_afford_2wk" %in% names(survey)) {
    stop(
      "food_cant_afford_2wk is required to denominator food coping actions.",
      call. = FALSE
    )
  }

  selected <- food_multiselect_option(survey, "food_cant_afford_action", option_code)
  survey %>%
    mutate(
      food_shortage_yn = make_yn(food_cant_afford_2wk),
      action_selected = selected
    ) %>%
    add_all_arms_rows() %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise(
      n_total = n(),
      n_nonmissing = sum(food_shortage_yn == 1, na.rm = TRUE),
      n_yes = sum(food_shortage_yn == 1 & action_selected == 1, na.rm = TRUE),
      percent = if_else(n_nonmissing > 0, 100 * n_yes / n_nonmissing, NA_real_),
      pct_all_households = if_else(n_total > 0, 100 * n_yes / n_total, NA_real_),
      .groups = "drop"
    ) %>%
    mutate(
      outcome_group = "food_shortage_coping_actions_selected",
      outcome_name = paste0("food_cant_afford_action_", option_code),
      outcome_label = option_label,
      source_variable = paste0("food_cant_afford_action/", option_code),
      unit = "percent",
      population = "all_deduplicated_household_timepoint_records",
      denominator_type = "households reporting food shortage in prior 2 weeks",
      percent = round(percent, 1),
      pct_all_households = round(pct_all_households, 1)
    ) %>%
    select(
      timepoint, study_arm_overall, outcome_group, outcome_name, outcome_label,
      source_variable, unit, population, denominator_type, n_total, n_nonmissing,
      n_yes, percent, pct_all_households
    )
}

food_shortage_status_summary <- summarise_binary_percent(
  survey,
  food_shortage_status_labels,
  "food_shortage_status"
)

food_action_summary <- purrr::map2_dfr(
  food_coping_labels$option_code,
  food_coping_labels$option_label,
  summarise_food_action_option
)

food_coping_summary <- bind_rows(
  food_shortage_status_summary,
  food_action_summary
) %>%
  arrange(outcome_group, outcome_name, timepoint, study_arm_overall)

write_reviewed_csv(
  food_coping_summary,
  "table_descriptive_food_shortage_coping.csv"
)

summarise_food_weekly_frequency <- function(var_name, label) {
  freq_data <- survey %>%
    transmute(
      timepoint,
      study_arm_overall,
      frequency_code = clean_coping_code(.data[[var_name]], missing_codes = c(77, 88, 99))
    ) %>%
    add_all_arms_rows()

  group_totals <- freq_data %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise(
      n_total = n(),
      n_frequency_nonmissing = sum(!is.na(frequency_code)),
      .groups = "drop"
    )

  freq_counts <- freq_data %>%
    filter(!is.na(frequency_code)) %>%
    count(timepoint, study_arm_overall, frequency_code, name = "n_households")

  group_totals %>%
    tidyr::crossing(weekly_frequency_levels) %>%
    left_join(freq_counts, by = c("timepoint", "study_arm_overall", "frequency_code")) %>%
    mutate(
      n_households = replace_na(n_households, 0L),
      outcome_name = var_name,
      outcome_label = label,
      source_variable = var_name,
      percent_among_nonmissing = if_else(
        n_frequency_nonmissing > 0,
        100 * n_households / n_frequency_nonmissing,
        NA_real_
      ),
      pct_all_households = if_else(n_total > 0, 100 * n_households / n_total, NA_real_),
      denominator_type = "nonmissing weekly-frequency response for this action",
      percent_among_nonmissing = round(percent_among_nonmissing, 1),
      pct_all_households = round(pct_all_households, 1)
    )
}

food_weekly_frequency_summary <- purrr::map2_dfr(
  food_frequency_labels$source_variable,
  food_frequency_labels$outcome_label,
  summarise_food_weekly_frequency
) %>%
  select(
    timepoint, study_arm_overall, outcome_name, outcome_label, source_variable,
    frequency_code, frequency_label, frequency_days_midpoint, denominator_type,
    n_total, n_frequency_nonmissing, n_households, percent_among_nonmissing,
    pct_all_households
  ) %>%
  arrange(outcome_name, timepoint, study_arm_overall, frequency_code)

write_reviewed_csv(
  food_weekly_frequency_summary,
  "table_descriptive_food_coping_weekly_frequency.csv"
)

summarise_food_rank <- function(rank_var, rank_type_label) {
  if (rank_var %notin% names(survey)) {
    return(tibble())
  }
  rank_code <- clean_coping_code(survey[[rank_var]], missing_codes = c(77, 99))

  purrr::map2_dfr(food_coping_labels$option_code, food_coping_labels$option_label,
                  function(option_code, option_label) {
    survey %>%
      mutate(rank_code_clean = rank_code) %>%
      add_all_arms_rows() %>%
      group_by(timepoint, study_arm_overall) %>%
      summarise(
        n_total = n(),
        n_rank_nonmissing = sum(!is.na(rank_code_clean)),
        n_selected = sum(rank_code_clean == option_code, na.rm = TRUE),
        percent = if_else(n_rank_nonmissing > 0, 100 * n_selected / n_rank_nonmissing, NA_real_),
        pct_all_households = if_else(n_total > 0, 100 * n_selected / n_total, NA_real_),
        .groups = "drop"
      ) %>%
      mutate(
        rank_type = rank_type_label,
        outcome_name = paste0(rank_var, "_", option_code),
        outcome_label = option_label,
        source_variable = rank_var,
        option_code = option_code,
        denominator_type = "nonmissing rank response",
        percent = round(percent, 1),
        pct_all_households = round(pct_all_households, 1)
      )
  })
}

food_coping_rank_summary <- bind_rows(
  summarise_food_rank("food_cant_afford_difficult", "most_difficult"),
  summarise_food_rank("food_cant_afford_easiest", "easiest")
) %>%
  select(
    timepoint, study_arm_overall, rank_type, outcome_name, outcome_label,
    source_variable, option_code, denominator_type, n_total, n_rank_nonmissing,
    n_selected, percent, pct_all_households
  ) %>%
  arrange(rank_type, outcome_name, timepoint, study_arm_overall)

write_reviewed_csv(
  food_coping_rank_summary,
  "table_descriptive_food_coping_difficulty_ease.csv"
)

weekly_code_to_days_midpoint <- function(x) {
  x_code <- clean_coping_code(x, missing_codes = c(77, 88, 99))
  dplyr::recode(
    x_code,
    `0` = 0,
    `1` = 1.5,
    `2` = 3.5,
    `3` = 5.5,
    `4` = 7,
    .default = NA_real_
  )
}

# Empirical coping-strategy weights. The project collected respondent rankings
# of the easiest and most difficult food-shortage coping actions. These weights
# are derived from those survey responses instead of external weights.
# The 1-4 scale is anchored at 1 = always ranked easiest and 4 = always ranked
# most difficult among households providing either ranking for that action.
derive_empirical_coping_weights <- function(label_data, component_map, difficult_var, easiest_var) {
  difficult_code <- clean_coping_code(survey[[difficult_var]], missing_codes = c(77, 99))
  easiest_code <- clean_coping_code(survey[[easiest_var]], missing_codes = c(77, 99))

  label_data %>%
    left_join(component_map, by = "option_code") %>%
    rowwise() %>%
    mutate(
      n_most_difficult = sum(difficult_code == option_code, na.rm = TRUE),
      n_easiest = sum(easiest_code == option_code, na.rm = TRUE),
      n_rank_mentions = n_most_difficult + n_easiest,
      pct_most_difficult_among_rank_mentions = if_else(
        n_rank_mentions > 0,
        100 * n_most_difficult / n_rank_mentions,
        NA_real_
      ),
      pct_easiest_among_rank_mentions = if_else(
        n_rank_mentions > 0,
        100 * n_easiest / n_rank_mentions,
        NA_real_
      ),
      empirical_difficulty_weight = if_else(
        n_rank_mentions > 0,
        1 + 3 * n_most_difficult / n_rank_mentions,
        NA_real_
      ),
      empirical_difficulty_weight_rounded = round(empirical_difficulty_weight, 2),
      included_in_coping_strategy_index = !is.na(source_variable) &
        source_variable %in% names(survey),
      weight_method = "Survey-derived from most difficult/easiest rankings: 1 + 3 * n_most_difficult / (n_most_difficult + n_easiest)",
      stability_note = if_else(
        n_rank_mentions < 10,
        "Fewer than 10 ranking mentions; interpret this empirical weight cautiously.",
        NA_character_
      )
    ) %>%
    ungroup()
}

food_csi_component_map <- tibble(
  option_code = c("1", "2", "3", "4", "5"),
  source_variable = c("borrow_food", "reduce_food", "reduce_meals", "not_eat", "restrict_food"),
  csi_component = c(
    "borrow_food_or_rely_on_help",
    "reduce_food_per_meal",
    "reduce_meals_per_day",
    "skip_all_meals_on_some_days",
    "restrict_adult_intake_so_children_can_eat"
  )
)

food_empirical_coping_weights <- derive_empirical_coping_weights(
  food_coping_labels,
  food_csi_component_map,
  "food_cant_afford_difficult",
  "food_cant_afford_easiest"
) %>%
  filter(option_code %in% food_csi_component_map$option_code) %>%
  select(
    option_code, source_variable, csi_component, outcome_label = option_label,
    included_in_coping_strategy_index, n_most_difficult, n_easiest,
    n_rank_mentions, pct_most_difficult_among_rank_mentions,
    pct_easiest_among_rank_mentions, empirical_difficulty_weight,
    empirical_difficulty_weight_rounded, weight_method, stability_note
  )

write_reviewed_csv(
  food_empirical_coping_weights,
  "table_descriptive_food_coping_strategy_index_components.csv",
  subfolder = "qa"
)

write_reviewed_csv(
  food_empirical_coping_weights,
  "table_descriptive_food_coping_weight_check.csv",
  subfolder = "qa"
)

food_csi_weights <- food_empirical_coping_weights %>%
  filter(included_in_coping_strategy_index) %>%
  select(source_variable, empirical_difficulty_weight) %>%
  tibble::deframe()

format_weighted_components <- function(weight_lookup, variables) {
  paste0(variables, "*", round(unname(weight_lookup[variables]), 2), collapse = " + ")
}

food_action_selected_for_csi <- tibble(
  borrow_food_selected = food_multiselect_option(survey, "food_cant_afford_action", "1"),
  reduce_food_selected = food_multiselect_option(survey, "food_cant_afford_action", "2"),
  reduce_meals_selected = food_multiselect_option(survey, "food_cant_afford_action", "3"),
  not_eat_selected = food_multiselect_option(survey, "food_cant_afford_action", "4"),
  restrict_food_selected = food_multiselect_option(survey, "food_cant_afford_action", "5")
)

food_csi_household <- bind_cols(survey, food_action_selected_for_csi) %>%
  mutate(
    food_cant_afford_2wk_yn = make_yn(food_cant_afford_2wk),
    borrow_food_days = case_when(
      !is.na(weekly_code_to_days_midpoint(borrow_food)) ~ weekly_code_to_days_midpoint(borrow_food),
      food_cant_afford_2wk_yn == 0 | borrow_food_selected == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    reduce_food_days = case_when(
      !is.na(weekly_code_to_days_midpoint(reduce_food)) ~ weekly_code_to_days_midpoint(reduce_food),
      food_cant_afford_2wk_yn == 0 | reduce_food_selected == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    reduce_meals_days = case_when(
      !is.na(weekly_code_to_days_midpoint(reduce_meals)) ~ weekly_code_to_days_midpoint(reduce_meals),
      food_cant_afford_2wk_yn == 0 | reduce_meals_selected == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    not_eat_days = case_when(
      !is.na(weekly_code_to_days_midpoint(not_eat)) ~ weekly_code_to_days_midpoint(not_eat),
      food_cant_afford_2wk_yn == 0 | not_eat_selected == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    restrict_food_days = case_when(
      !is.na(weekly_code_to_days_midpoint(restrict_food)) ~ weekly_code_to_days_midpoint(restrict_food),
      food_cant_afford_2wk_yn == 0 | restrict_food_selected == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    csi_survey_weighted =
      food_csi_weights[["borrow_food"]] * borrow_food_days +
      food_csi_weights[["reduce_food"]] * reduce_food_days +
      food_csi_weights[["reduce_meals"]] * reduce_meals_days +
      food_csi_weights[["not_eat"]] * not_eat_days +
      food_csi_weights[["restrict_food"]] * restrict_food_days,
    csi_survey_weighted_n_missing_components = rowSums(is.na(cbind(
      borrow_food_days, reduce_food_days, reduce_meals_days,
      not_eat_days, restrict_food_days
    )))
  )

food_csi_summary <- food_csi_household %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    csi_type = "survey_weighted_food_coping_strategy_index",
    n_total = n(),
    n_csi_nonmissing = sum(!is.na(csi_survey_weighted)),
    n_missing_any_included_component = sum(csi_survey_weighted_n_missing_components > 0, na.rm = TRUE),
    mean_csi = mean(csi_survey_weighted, na.rm = TRUE),
    sd_csi = sd(csi_survey_weighted, na.rm = TRUE),
    median_csi = median(csi_survey_weighted, na.rm = TRUE),
    p25_csi = quantile(csi_survey_weighted, 0.25, na.rm = TRUE, names = FALSE),
    p75_csi = quantile(csi_survey_weighted, 0.75, na.rm = TRUE, names = FALSE),
    min_csi = min(csi_survey_weighted, na.rm = TRUE),
    max_csi = max(csi_survey_weighted, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(c(mean_csi, sd_csi, median_csi, p25_csi, p75_csi, min_csi, max_csi),
           ~ if_else(is.nan(.x) | is.infinite(.x), NA_real_, round(.x, 2))),
    included_components = format_weighted_components(
      food_csi_weights,
      c("borrow_food", "reduce_food", "reduce_meals", "not_eat", "restrict_food")
    ),
    weight_method = "Survey-derived from food_cant_afford_difficult and food_cant_afford_easiest",
    frequency_conversion = "weekly_choices categories converted to midpoint days: 0, 1.5, 3.5, 5.5, 7",
    maximum_possible_score = round(7 * sum(food_csi_weights[c(
      "borrow_food", "reduce_food", "reduce_meals", "not_eat", "restrict_food"
    )], na.rm = TRUE), 2)
  )

write_reviewed_csv(
  food_csi_summary,
  "table_descriptive_food_coping_strategy_index.csv"
)

# Fuel shortage coping outputs parallel the repaired food-coping outputs, but use
# the fuel-specific variables. The weekly fuel coping frequency fields are
# borrow_fuel, reduce_fuel, reduce_meals1, and not_eat1. No fuel-specific
# difficulty/ease ranking variables are present in clean_final, so a QA table
# records that limitation rather than fabricating rankings.
fuel_shortage_status_labels <- tibble(
  source_variable = c("fuel_cant_afford_2wk"),
  outcome_name = source_variable,
  outcome_label = c("Could not afford enough fuel in prior 2 weeks")
) %>%
  filter(source_variable %in% names(survey))

fuel_frequency_labels <- tibble(
  source_variable = c("borrow_fuel", "reduce_fuel", "reduce_meals1", "not_eat1"),
  outcome_name = source_variable,
  outcome_label = c(
    "Borrowed fuel",
    "Reduced fuel use",
    "Reduced meals per day due to fuel shortage",
    "Skipped eating due to fuel shortage"
  )
) %>%
  filter(source_variable %in% names(survey))

summarise_fuel_action_option <- function(option_code, option_label) {
  if (!"fuel_cant_afford_2wk" %in% names(survey)) {
    stop(
      "fuel_cant_afford_2wk is required to denominator fuel coping actions.",
      call. = FALSE
    )
  }

  selected <- food_multiselect_option(survey, "fuel_cant_afford_action", option_code)
  survey %>%
    mutate(
      fuel_shortage_yn = make_yn(fuel_cant_afford_2wk),
      action_selected = selected
    ) %>%
    add_all_arms_rows() %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise(
      n_total = n(),
      n_nonmissing = sum(fuel_shortage_yn == 1, na.rm = TRUE),
      n_yes = sum(fuel_shortage_yn == 1 & action_selected == 1, na.rm = TRUE),
      percent = if_else(n_nonmissing > 0, 100 * n_yes / n_nonmissing, NA_real_),
      pct_all_households = if_else(n_total > 0, 100 * n_yes / n_total, NA_real_),
      .groups = "drop"
    ) %>%
    mutate(
      outcome_group = "fuel_shortage_coping_actions_selected",
      outcome_name = paste0("fuel_cant_afford_action_", option_code),
      outcome_label = option_label,
      source_variable = paste0("fuel_cant_afford_action/", option_code),
      unit = "percent",
      population = "all_deduplicated_household_timepoint_records",
      denominator_type = "households reporting fuel shortage in prior 2 weeks",
      percent = round(percent, 1),
      pct_all_households = round(pct_all_households, 1)
    ) %>%
    select(
      timepoint, study_arm_overall, outcome_group, outcome_name, outcome_label,
      source_variable, unit, population, denominator_type, n_total, n_nonmissing,
      n_yes, percent, pct_all_households
    )
}

fuel_shortage_status_summary <- summarise_binary_percent(
  survey,
  fuel_shortage_status_labels,
  "fuel_shortage_status"
)

fuel_action_summary <- purrr::map2_dfr(
  fuel_coping_labels$option_code,
  fuel_coping_labels$option_label,
  summarise_fuel_action_option
)

fuel_coping_summary <- bind_rows(
  fuel_shortage_status_summary,
  fuel_action_summary
) %>%
  arrange(outcome_group, outcome_name, timepoint, study_arm_overall)

write_reviewed_csv(
  fuel_coping_summary,
  "table_descriptive_fuel_shortage_coping.csv"
)

summarise_fuel_weekly_frequency <- function(var_name, label) {
  freq_data <- survey %>%
    transmute(
      timepoint,
      study_arm_overall,
      frequency_code = clean_coping_code(.data[[var_name]], missing_codes = c(77, 88, 99))
    ) %>%
    add_all_arms_rows()

  group_totals <- freq_data %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise(
      n_total = n(),
      n_frequency_nonmissing = sum(!is.na(frequency_code)),
      .groups = "drop"
    )

  freq_counts <- freq_data %>%
    filter(!is.na(frequency_code)) %>%
    count(timepoint, study_arm_overall, frequency_code, name = "n_households")

  group_totals %>%
    tidyr::crossing(weekly_frequency_levels) %>%
    left_join(freq_counts, by = c("timepoint", "study_arm_overall", "frequency_code")) %>%
    mutate(
      n_households = replace_na(n_households, 0L),
      outcome_name = var_name,
      outcome_label = label,
      source_variable = var_name,
      percent_among_nonmissing = if_else(
        n_frequency_nonmissing > 0,
        100 * n_households / n_frequency_nonmissing,
        NA_real_
      ),
      pct_all_households = if_else(n_total > 0, 100 * n_households / n_total, NA_real_),
      denominator_type = "nonmissing weekly-frequency response for this action",
      percent_among_nonmissing = round(percent_among_nonmissing, 1),
      pct_all_households = round(pct_all_households, 1)
    )
}

fuel_weekly_frequency_summary <- purrr::map2_dfr(
  fuel_frequency_labels$source_variable,
  fuel_frequency_labels$outcome_label,
  summarise_fuel_weekly_frequency
) %>%
  select(
    timepoint, study_arm_overall, outcome_name, outcome_label, source_variable,
    frequency_code, frequency_label, frequency_days_midpoint, denominator_type,
    n_total, n_frequency_nonmissing, n_households, percent_among_nonmissing,
    pct_all_households
  ) %>%
  arrange(outcome_name, timepoint, study_arm_overall, frequency_code)

write_reviewed_csv(
  fuel_weekly_frequency_summary,
  "table_descriptive_fuel_coping_weekly_frequency.csv"
)

summarise_fuel_rank <- function(rank_var, rank_type_label) {
  if (rank_var %notin% names(survey)) {
    return(tibble())
  }
  rank_code <- clean_coping_code(survey[[rank_var]], missing_codes = c(77, 99))

  purrr::map2_dfr(fuel_coping_labels$option_code, fuel_coping_labels$option_label,
                  function(option_code, option_label) {
    survey %>%
      mutate(rank_code_clean = rank_code) %>%
      add_all_arms_rows() %>%
      group_by(timepoint, study_arm_overall) %>%
      summarise(
        n_total = n(),
        n_rank_nonmissing = sum(!is.na(rank_code_clean)),
        n_selected = sum(rank_code_clean == option_code, na.rm = TRUE),
        percent = if_else(n_rank_nonmissing > 0, 100 * n_selected / n_rank_nonmissing, NA_real_),
        pct_all_households = if_else(n_total > 0, 100 * n_selected / n_total, NA_real_),
        .groups = "drop"
      ) %>%
      mutate(
        rank_type = rank_type_label,
        outcome_name = paste0("fuel_", rank_var, "_", option_code),
        outcome_label = option_label,
        source_variable = rank_var,
        source_construct = "Uses the same rank-response fields as the food coping ranking table",
        option_code = option_code,
        denominator_type = "nonmissing rank response",
        percent = round(percent, 1),
        pct_all_households = round(pct_all_households, 1)
      )
  })
}

fuel_coping_rank_summary <- bind_rows(
  summarise_fuel_rank("food_cant_afford_difficult", "most_difficult"),
  summarise_fuel_rank("food_cant_afford_easiest", "easiest")
) %>%
  select(
    timepoint, study_arm_overall, rank_type, outcome_name, outcome_label,
    source_variable, source_construct, option_code, denominator_type, n_total,
    n_rank_nonmissing, n_selected, percent, pct_all_households
  ) %>%
  arrange(rank_type, outcome_name, timepoint, study_arm_overall)

write_reviewed_csv(
  fuel_coping_rank_summary,
  "table_descriptive_fuel_coping_difficulty_ease.csv"
)

fuel_coping_index_component_map <- tibble(
  option_code = c("1", "2", "3", "4"),
  source_variable = c("borrow_fuel", "reduce_fuel", "reduce_meals1", "not_eat1"),
  index_component = c(
    "borrow_fuel",
    "reduce_fuel_use_or_portion",
    "reduce_meals_due_to_fuel_shortage",
    "skip_eating_due_to_fuel_shortage"
  ),
  food_coping_analog = c("borrow_food", "reduce_food", "reduce_meals", "not_eat")
) %>%
  left_join(
    food_empirical_coping_weights %>%
      select(
        option_code,
        food_coping_outcome_label = outcome_label,
        n_most_difficult,
        n_easiest,
        n_rank_mentions,
        pct_most_difficult_among_rank_mentions,
        pct_easiest_among_rank_mentions,
        empirical_difficulty_weight,
        empirical_difficulty_weight_rounded,
        weight_method,
        stability_note
      ),
    by = "option_code"
  ) %>%
  mutate(
    outcome_label = c(
      "Borrowed fuel",
      "Reduced fuel use",
      "Reduced meals per day due to fuel shortage",
      "Skipped eating due to fuel shortage"
    ),
    included_in_fuel_coping_index = source_variable %in% names(survey),
    reason = "Weight is derived from survey-ranked difficulty/ease for the analogous food coping strategy."
  ) %>%
  select(
    option_code, source_variable, index_component, outcome_label,
    food_coping_analog, food_coping_outcome_label,
    included_in_fuel_coping_index, n_most_difficult, n_easiest,
    n_rank_mentions, pct_most_difficult_among_rank_mentions,
    pct_easiest_among_rank_mentions, empirical_difficulty_weight,
    empirical_difficulty_weight_rounded, weight_method, reason, stability_note
  )
write_reviewed_csv(
  fuel_coping_index_component_map,
  "table_descriptive_fuel_coping_strategy_index_components.csv",
  subfolder = "qa"
)

fuel_csi_weights <- fuel_coping_index_component_map %>%
  filter(included_in_fuel_coping_index) %>%
  select(source_variable, empirical_difficulty_weight) %>%
  tibble::deframe()

fuel_action_selected_for_index <- tibble(
  borrow_fuel_selected = food_multiselect_option(survey, "fuel_cant_afford_action", "1"),
  reduce_fuel_selected = food_multiselect_option(survey, "fuel_cant_afford_action", "2"),
  reduce_meals_fuel_selected = food_multiselect_option(survey, "fuel_cant_afford_action", "3"),
  not_eat_fuel_selected = food_multiselect_option(survey, "fuel_cant_afford_action", "4")
)

fuel_coping_household <- bind_cols(survey, fuel_action_selected_for_index) %>%
  mutate(
    fuel_cant_afford_2wk_yn = make_yn(fuel_cant_afford_2wk),
    borrow_fuel_days = case_when(
      !is.na(weekly_code_to_days_midpoint(borrow_fuel)) ~ weekly_code_to_days_midpoint(borrow_fuel),
      fuel_cant_afford_2wk_yn == 0 | borrow_fuel_selected == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    reduce_fuel_days = case_when(
      !is.na(weekly_code_to_days_midpoint(reduce_fuel)) ~ weekly_code_to_days_midpoint(reduce_fuel),
      fuel_cant_afford_2wk_yn == 0 | reduce_fuel_selected == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    reduce_meals_fuel_days = case_when(
      !is.na(weekly_code_to_days_midpoint(reduce_meals1)) ~ weekly_code_to_days_midpoint(reduce_meals1),
      fuel_cant_afford_2wk_yn == 0 | reduce_meals_fuel_selected == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    not_eat_fuel_days = case_when(
      !is.na(weekly_code_to_days_midpoint(not_eat1)) ~ weekly_code_to_days_midpoint(not_eat1),
      fuel_cant_afford_2wk_yn == 0 | not_eat_fuel_selected == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    fuel_coping_strategy_index =
      fuel_csi_weights[["borrow_fuel"]] * borrow_fuel_days +
      fuel_csi_weights[["reduce_fuel"]] * reduce_fuel_days +
      fuel_csi_weights[["reduce_meals1"]] * reduce_meals_fuel_days +
      fuel_csi_weights[["not_eat1"]] * not_eat_fuel_days,
    fuel_coping_index_n_missing_components = rowSums(is.na(cbind(
      borrow_fuel_days, reduce_fuel_days, reduce_meals_fuel_days, not_eat_fuel_days
    )))
  )

fuel_coping_index_summary <- fuel_coping_household %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    index_type = "survey_weighted_fuel_coping_strategy_index",
    n_total = n(),
    n_index_nonmissing = sum(!is.na(fuel_coping_strategy_index)),
    n_missing_any_included_component = sum(fuel_coping_index_n_missing_components > 0, na.rm = TRUE),
    mean_index = mean(fuel_coping_strategy_index, na.rm = TRUE),
    sd_index = sd(fuel_coping_strategy_index, na.rm = TRUE),
    median_index = median(fuel_coping_strategy_index, na.rm = TRUE),
    p25_index = quantile(fuel_coping_strategy_index, 0.25, na.rm = TRUE, names = FALSE),
    p75_index = quantile(fuel_coping_strategy_index, 0.75, na.rm = TRUE, names = FALSE),
    min_index = min(fuel_coping_strategy_index, na.rm = TRUE),
    max_index = max(fuel_coping_strategy_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(c(mean_index, sd_index, median_index, p25_index, p75_index, min_index, max_index),
           ~ if_else(is.nan(.x) | is.infinite(.x), NA_real_, round(.x, 2))),
    included_components = format_weighted_components(
      fuel_csi_weights,
      c("borrow_fuel", "reduce_fuel", "reduce_meals1", "not_eat1")
    ),
    weight_method = "Survey-derived from food_cant_afford_difficult and food_cant_afford_easiest for analogous food coping strategies",
    frequency_conversion = "weekly_choices categories converted to midpoint days: 0, 1.5, 3.5, 5.5, 7",
    maximum_possible_score = round(7 * sum(fuel_csi_weights[c(
      "borrow_fuel", "reduce_fuel", "reduce_meals1", "not_eat1"
    )], na.rm = TRUE), 2)
  )

write_reviewed_csv(
  fuel_coping_index_summary,
  "table_descriptive_fuel_coping_strategy_index.csv"
)
food_coping_plot_data <- food_coping_summary %>%
  filter(n_nonmissing > 0, !is.na(percent)) %>%
  filter(outcome_group == "food_shortage_status" |
           source_variable %in% paste0("food_cant_afford_action/", 1:5))
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
  "fig_descriptive_food_shortage_coping.png",
  width = 10,
  height = 6
)
fuel_coping_plot_data <- fuel_coping_summary %>%
  filter(n_nonmissing > 0, !is.na(percent)) %>%
  filter(
    outcome_group == "fuel_shortage_status" |
      source_variable %in% paste0("fuel_cant_afford_action/", c(1:4, 7, 8, 14, 15))
  )
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
  "fig_descriptive_fuel_shortage_coping.png",
  width = 9,
  height = 6
)

# Integrated from 3.2_shortage_coping_figure_20260922.R.
# This figure reads the three reviewed tables written earlier in this script.
local({
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  stop(
    "Package `gridExtra` is required to assemble the figure. ",
    "Run renv::restore() from the project root, then rerun this script.",
    call. = FALSE
  )
}

fuel_csv <- file.path(
  dir_tables_reviewed,
  "table_descriptive_fuel_shortage_coping.csv"
)
food_csv <- file.path(
  dir_tables_reviewed,
  "table_descriptive_food_shortage_coping.csv"
)
plastic_csv <- file.path(
  dir_tables_reviewed,
  "table_descriptive_categorical_outcomes.csv"
)

input_paths <- c(fuel_csv, food_csv, plastic_csv)
missing_inputs <- input_paths[!file.exists(input_paths)]
if (length(missing_inputs) > 0) {
  stop(
    "Missing figure input(s): ",
    paste(missing_inputs, collapse = "; "),
    call. = FALSE
  )
}

figure_stem <- "fig_descriptive_shortage_coping_with_plastic_burning"
output_png <- file.path(dir_figures_reviewed, paste0(figure_stem, ".png"))
output_pdf <- file.path(dir_figures_reviewed, paste0(figure_stem, ".pdf"))

fuel_action_labels <- c(
  "Borrowed fuel or relied on relatives/friends",
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
  "Ate food that was not fully cooked"
)
food_action_labels <- c(
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
  "Exchanged food for other food"
)

fuel_strategy_map <- bind_rows(
  tibble::tibble(
    source_variable = "fuel_cant_afford_2wk",
    outcome_group = "fuel_shortage_status",
    strategy_label = "Did not have enough fuel to cook (past 2 weeks)",
    strategy_order = 1L
  ),
  tibble::tibble(
    source_variable = paste0("fuel_cant_afford_action/", seq_along(fuel_action_labels)),
    outcome_group = "fuel_shortage_coping_actions_selected",
    strategy_label = paste0(seq_along(fuel_action_labels), ". ", fuel_action_labels),
    strategy_order = seq_along(fuel_action_labels) + 1L
  )
)

food_strategy_map <- bind_rows(
  tibble::tribble(
    ~source_variable,              ~outcome_group,          ~strategy_label,                              ~strategy_order,
    "food_cant_afford_2wk",       "food_shortage_status", "Did not have enough food or money to buy food (past 2 weeks)", 1L,
    "food_insufficient_nutrition", "food_shortage_status", "Felt my food wasn't nutritious enough (past 2 weeks)",       2L,
    "food_didnt_want",            "food_shortage_status", "Relied on food that I didn't want to eat (past 2 weeks)",     3L
  ),
  tibble::tibble(
    source_variable = paste0("food_cant_afford_action/", seq_along(food_action_labels)),
    outcome_group = "food_shortage_coping_actions_selected",
    strategy_label = paste0(seq_along(food_action_labels), ". ", food_action_labels),
    strategy_order = seq_along(food_action_labels) + 3L
  )
)

required_columns <- c(
  "timepoint", "study_arm_overall", "outcome_group", "outcome_label",
  "source_variable", "denominator_type", "n_total", "n_nonmissing",
  "n_yes", "pct_all_households"
)

read_coping_percent_table <- function(path, coping_domain, strategy_map) {
  data <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  missing_columns <- setdiff(required_columns, names(data))

  if (length(names(data)) < 14 || names(data)[[14]] != "pct_all_households") {
    stop("Column N must be named `pct_all_households` in ", basename(path),
         call. = FALSE)
  }
  if (length(missing_columns) > 0) {
    stop(
      "Missing required column(s) in ", basename(path), ": ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  data <- data %>%
    mutate(
      across(c(n_total, n_nonmissing, n_yes, pct_all_households),
             ~ suppressWarnings(as.numeric(.x))),
      timepoint = stringr::str_to_lower(stringr::str_squish(timepoint)),
      study_arm_overall = stringr::str_to_lower(
        stringr::str_squish(study_arm_overall)
      )
    ) %>%
    filter(
      timepoint %in% timepoint_levels,
      study_arm_overall %in% arm_levels,
      source_variable %in% strategy_map$source_variable
    ) %>%
    inner_join(strategy_map, by = c("source_variable", "outcome_group"))

  duplicate_keys <- data %>%
    count(timepoint, study_arm_overall, source_variable) %>%
    filter(n != 1)
  if (nrow(duplicate_keys) > 0) {
    stop("Duplicate selected strategy rows in ", basename(path), call. = FALSE)
  }

  expected_rows <- tidyr::expand_grid(
    timepoint = timepoint_levels,
    study_arm_overall = arm_levels,
    source_variable = strategy_map$source_variable
  )
  missing_rows <- expected_rows %>%
    anti_join(
      data %>% distinct(timepoint, study_arm_overall, source_variable),
      by = c("timepoint", "study_arm_overall", "source_variable")
    )
  if (nrow(missing_rows) > 0 || nrow(data) != nrow(expected_rows)) {
    stop("Missing expected arm-timepoint-strategy rows in ", basename(path),
         call. = FALSE)
  }

  if (any(is.na(data$n_total)) || any(is.na(data$n_nonmissing)) ||
      any(is.na(data$n_yes)) || any(data$n_nonmissing < 0) ||
      any(data$n_yes < 0) || any(data$n_yes > data$n_nonmissing) ||
      any(data$n_nonmissing > data$n_total)) {
    stop("Invalid numerator or denominator values in ", basename(path),
         call. = FALSE)
  }

  expected_percent <- ifelse(
    data$n_total > 0,
    round(100 * data$n_yes / data$n_total, 1),
    NA_real_
  )
  percent_mismatch <-
    xor(is.na(data$pct_all_households), is.na(expected_percent)) |
    (!is.na(data$pct_all_households) & !is.na(expected_percent) &
       abs(data$pct_all_households - expected_percent) > 0.05)
  if (any(percent_mismatch)) {
    stop("Column N does not reconcile to n_yes / n_total in ",
         basename(path), call. = FALSE)
  }

  data %>%
    transmute(
      coping_domain = coping_domain,
      timepoint = factor(timepoint, levels = timepoint_levels, ordered = TRUE),
      study_arm_overall = factor(study_arm_overall, levels = arm_levels),
      study_arm_label = factor(
        study_arm_overall,
        levels = arm_levels,
        labels = c("Comparison group", "Intervention group")
      ),
      source_variable,
      source_outcome_label = outcome_label,
      strategy_order,
      strategy_label = factor(
        strategy_label,
        levels = strategy_map$strategy_label
      ),
      n_total,
      n_shortage_households = n_nonmissing,
      n_reporting_strategy = n_yes,
      percent_all_households = pct_all_households,
      denominator_type
    )
}

plot_data <- bind_rows(
  read_coping_percent_table(
    fuel_csv,
    coping_domain = "Fuel",
    strategy_map = fuel_strategy_map
  ),
  read_coping_percent_table(
    food_csv,
    coping_domain = "Food",
    strategy_map = food_strategy_map
  )
)

expected_plot_rows <-
  (nrow(fuel_strategy_map) + nrow(food_strategy_map)) *
  length(timepoint_levels) * length(arm_levels)
if (nrow(plot_data) != expected_plot_rows ||
    any(is.na(plot_data$percent_all_households)) ||
    any(plot_data$percent_all_households < 0 |
        plot_data$percent_all_households > 100)) {
  stop("Selected coping-strategy percentages failed validation.", call. = FALSE)
}

arm_colors <- c(
  "Comparison group" = "#3B6EA8",
  "Intervention group" = "#C94C4C"
)
arm_shapes <- c("Comparison group" = 16, "Intervention group" = 17)
arm_linetypes <- c("Comparison group" = "solid", "Intervention group" = "22")
position_by_arm <- position_dodge(width = 0.10)

make_axis_spec <- function(data) {
  max_percent <- max(data$percent_all_households, na.rm = TRUE)
  upper <- dplyr::case_when(
    max_percent > 80 ~ 100,
    max_percent > 60 ~ 80,
    max_percent > 40 ~ 60,
    max_percent > 20 ~ 40,
    TRUE ~ 20
  )
  step <- dplyr::case_when(
    upper >= 60 ~ 20,
    upper >= 40 ~ 10,
    TRUE ~ 5
  )

  list(upper = upper, breaks = seq(0, upper, by = step))
}

make_domain_plot <- function(data, title, show_legend, x_title, y_title,
                             y_upper, y_breaks) {
  strategy_levels <- data %>%
    distinct(strategy_order, strategy_label) %>%
    arrange(strategy_order) %>%
    pull(strategy_label) %>%
    as.character()
  data <- data %>%
    mutate(
      strategy_label = factor(as.character(strategy_label),
                              levels = strategy_levels)
    )

  ggplot(
    data,
    aes(
      x = timepoint,
      y = percent_all_households,
      color = study_arm_label,
      shape = study_arm_label,
      linetype = study_arm_label,
      group = study_arm_label
    )
  ) +
    geom_line(linewidth = 0.75, position = position_by_arm) +
    geom_point(size = 2.2, stroke = 0.6, position = position_by_arm) +
    facet_wrap(
      vars(strategy_label),
      ncol = 4,
      scales = "fixed",
      labeller = ggplot2::label_wrap_gen(width = 27)
    ) +
    scale_color_manual(values = arm_colors, drop = FALSE) +
    scale_shape_manual(values = arm_shapes, drop = FALSE) +
    scale_linetype_manual(values = arm_linetypes, drop = FALSE) +
    scale_x_discrete(
      drop = FALSE,
      labels = c(
        baseline = "Baseline",
        midline = "Midline",
        endline = "Endline"
      )
    ) +
    scale_y_continuous(
      limits = c(0, y_upper),
      breaks = y_breaks,
      labels = scales::label_number(suffix = "%", accuracy = 1),
      expand = expansion(mult = c(0, 0.03))
    ) +
    labs(
      title = title,
      x = x_title,
      y = y_title,
      color = "Study arm",
      shape = "Study arm",
      linetype = "Study arm"
    ) +
    theme_minimal(base_size = 10, base_family = "sans") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "#D9D9D9", linewidth = 0.35),
      panel.border = element_rect(color = "#777777", fill = NA, linewidth = 0.35),
      panel.spacing.x = grid::unit(0.7, "lines"),
      strip.background = element_rect(
        fill = "#F2F2F2", color = "#BDBDBD", linewidth = 0.35
      ),
      strip.text = element_text(face = "bold", size = 9, lineheight = 1.0),
      axis.text.x = element_text(size = 8.5),
      axis.text.y = element_text(size = 8.5),
      axis.title = element_text(size = 9.5),
      plot.title = element_text(face = "bold", size = 11, hjust = 0),
      plot.title.position = "plot",
      legend.position = if (show_legend) "top" else "none",
      legend.justification = "left",
      legend.box.just = "left",
      legend.margin = margin(t = 0, r = 0, b = 2, l = 0),
      legend.key.width = grid::unit(1.8, "lines"),
      plot.margin = margin(t = 3, r = 6, b = 3, l = 4)
    ) +
    guides(
      color = guide_legend(nrow = 1, byrow = TRUE),
      shape = guide_legend(nrow = 1, byrow = TRUE),
      linetype = guide_legend(nrow = 1, byrow = TRUE)
    )
}

fuel_plot_data <- plot_data %>% filter(coping_domain == "Fuel")
food_plot_data <- plot_data %>% filter(coping_domain == "Food")

read_burn_plastic_table <- function(path, fuel_data) {
  data <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  required <- c(
    "timepoint", "study_arm_overall", "outcome_group", "source_variable",
    "category_value", "n_total", "n_nonmissing", "n_category"
  )
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0) {
    stop("Missing burn-plastic column(s): ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }

  data <- data %>%
    mutate(
      across(c(category_value, n_total, n_nonmissing, n_category),
             ~ suppressWarnings(as.integer(.x))),
      timepoint = stringr::str_to_lower(stringr::str_squish(timepoint)),
      study_arm_overall = stringr::str_to_lower(
        stringr::str_squish(study_arm_overall)
      )
    ) %>%
    filter(
      outcome_group == "plastic_burning",
      source_variable == "burn_plastic_frequency",
      timepoint %in% timepoint_levels,
      study_arm_overall %in% arm_levels
    )

  if (nrow(data) == 0 ||
      any(is.na(data$category_value) | is.na(data$n_total) |
          is.na(data$n_nonmissing) | is.na(data$n_category)) ||
      any(!data$category_value %in% c(0:5, 7L)) ||
      any(data$n_total <= 0 | data$n_nonmissing < 0 |
          data$n_nonmissing > data$n_total | data$n_category < 0)) {
    stop("Invalid burn_plastic_frequency rows in ", basename(path),
         call. = FALSE)
  }

  duplicate_keys <- data %>%
    count(timepoint, study_arm_overall, category_value) %>%
    filter(n != 1)
  if (nrow(duplicate_keys) > 0) {
    stop("Duplicate burn_plastic_frequency categories in ", basename(path),
         call. = FALSE)
  }

  denominators <- data %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise(
      n_total = first(n_total),
      n_nonmissing = first(n_nonmissing),
      distinct_totals = n_distinct(n_total),
      distinct_nonmissing = n_distinct(n_nonmissing),
      sum_categories = sum(n_category),
      .groups = "drop"
    )
  fuel_denominators <- fuel_data %>%
    filter(source_variable == "fuel_cant_afford_2wk") %>%
    transmute(
      timepoint = as.character(timepoint),
      study_arm_overall = as.character(study_arm_overall),
      fuel_n_total = n_total
    )
  denominator_check <- denominators %>%
    left_join(fuel_denominators,
              by = c("timepoint", "study_arm_overall"))
  if (nrow(denominators) != length(timepoint_levels) * length(arm_levels) ||
      any(denominator_check$distinct_totals != 1 |
          denominator_check$distinct_nonmissing != 1 |
          denominator_check$sum_categories != denominator_check$n_nonmissing |
          is.na(denominator_check$fuel_n_total) |
          denominator_check$n_total != denominator_check$fuel_n_total)) {
    stop("Burn-plastic category counts or denominators failed validation.",
         call. = FALSE)
  }

  # Endline codes 5 and 7 are literal weekly counts alongside the coded bands.
  frequency_levels <- c("1-2 days", "3-4 days", "5-6 days", "Every day")
  counts <- data %>%
    mutate(frequency_band = case_when(
      category_value == 1L ~ "1-2 days",
      category_value == 2L ~ "3-4 days",
      category_value %in% c(3L, 5L) ~ "5-6 days",
      category_value %in% c(4L, 7L) ~ "Every day",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(frequency_band)) %>%
    group_by(timepoint, study_arm_overall, frequency_band) %>%
    summarise(n_category = sum(n_category), .groups = "drop")

  tidyr::expand_grid(
    timepoint = timepoint_levels,
    study_arm_overall = arm_levels,
    frequency_band = frequency_levels
  ) %>%
    left_join(counts,
              by = c("timepoint", "study_arm_overall", "frequency_band")) %>%
    left_join(
      denominators %>% select(timepoint, study_arm_overall, n_total, n_nonmissing),
      by = c("timepoint", "study_arm_overall")
    ) %>%
    mutate(
      n_category = coalesce(n_category, 0L),
      percent_all_households = 100 * n_category / n_total,
      timepoint = factor(timepoint, levels = timepoint_levels, ordered = TRUE),
      frequency_band = factor(frequency_band, levels = frequency_levels),
      study_arm_label = factor(
        study_arm_overall,
        levels = arm_levels,
        labels = c("Comparison group", "Intervention group")
      )
    )
}

plastic_plot_data <- read_burn_plastic_table(plastic_csv, fuel_plot_data)
fuel_axis <- make_axis_spec(fuel_plot_data)
food_axis <- make_axis_spec(food_plot_data)

fuel_plot <- make_domain_plot(
  fuel_plot_data,
  "Fuel shortage and coping responses",
  show_legend = TRUE,
  x_title = NULL,
  y_title = "All households (%)",
  y_upper = fuel_axis$upper,
  y_breaks = fuel_axis$breaks
)
food_plot <- make_domain_plot(
  food_plot_data,
  "Food shortage and coping responses",
  show_legend = FALSE,
  x_title = "Survey round",
  y_title = "All households (%)",
  y_upper = food_axis$upper,
  y_breaks = food_axis$breaks
)

plastic_axis_upper <- ceiling(
  (max(plastic_plot_data$percent_all_households) + 2) / 5
) * 5
plastic_plot <- ggplot(
  plastic_plot_data,
  aes(x = frequency_band, y = percent_all_households, fill = study_arm_label)
) +
  geom_col(
    position = position_dodge(width = 0.78),
    width = 0.7,
    color = "white",
    linewidth = 0.15
  ) +
  geom_text(
    aes(label = if_else(n_category > 0L,
                        sprintf("%.1f%%", percent_all_households), "")),
    position = position_dodge(width = 0.78),
    vjust = -0.25,
    size = 2.6,
    show.legend = FALSE
  ) +
  facet_wrap(
    vars(timepoint),
    nrow = 1,
    labeller = ggplot2::as_labeller(c(
      baseline = "Baseline", midline = "Midline", endline = "Endline"
    ))
  ) +
  scale_fill_manual(values = arm_colors, drop = FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    limits = c(0, plastic_axis_upper),
    breaks = seq(0, plastic_axis_upper, by = 5),
    labels = scales::label_number(suffix = "%", accuracy = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Plastic burning frequency in the prior week",
    x = "Days burning plastic in the prior week",
    y = "All households (%)"
  ) +
  theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#D9D9D9", linewidth = 0.35),
    panel.border = element_rect(color = "#777777", fill = NA, linewidth = 0.35),
    panel.spacing.x = grid::unit(0.7, "lines"),
    strip.background = element_rect(
      fill = "#F2F2F2", color = "#BDBDBD", linewidth = 0.35
    ),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(size = 8.5),
    axis.text.y = element_text(size = 8.5),
    axis.title = element_text(size = 9.5),
    plot.title = element_text(face = "bold", size = 11, hjust = 0),
    plot.title.position = "plot",
    plot.margin = margin(t = 3, r = 6, b = 3, l = 4),
    legend.position = "none"
  )

baseline_intervention_plastic <- plastic_plot_data %>%
  filter(timepoint == "baseline", study_arm_overall == "intervention") %>%
  slice(1)
figure_note <- paste(
  "Line panels use column N (`pct_all_households`) from the shortage-coping tables; plastic bars use n_category / n_total from the categorical-outcomes table.",
  paste0(
    "All percentages use all households in each study arm and survey round. ",
    "Plastic codes 5 and 7 are grouped with 5-6 days and every day, respectively. ",
    "Baseline intervention plastic-frequency responses: ",
    baseline_intervention_plastic$n_nonmissing, "/",
    baseline_intervention_plastic$n_total, "."
  ),
  sep = "\n"
)

figure_grob <- gridExtra::arrangeGrob(
  fuel_plot,
  plastic_plot,
  food_plot,
  ncol = 1,
  heights = c(4, 1.45, 4.08),
  top = grid::textGrob(
    "Fuel and food shortage indicators and coping responses",
    x = grid::unit(0.01, "npc"),
    hjust = 0,
    gp = grid::gpar(fontfamily = "sans", fontsize = 15, fontface = "bold")
  ),
  bottom = grid::textGrob(
    figure_note,
    x = grid::unit(0.01, "npc"),
    hjust = 0,
    gp = grid::gpar(fontfamily = "sans", fontsize = 8.5, col = "#444444")
  )
)

dir.create(dirname(output_png), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output_pdf), recursive = TRUE, showWarnings = FALSE)

ggplot2::ggsave(
  filename = output_png,
  plot = figure_grob,
  width = 16,
  height = 23.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
ggplot2::ggsave(
  filename = output_pdf,
  plot = figure_grob,
  width = 16,
  height = 23.5,
  units = "in",
  device = "pdf",
  bg = "white"
)

message("Wrote figure: ", output_png)
message("Wrote figure: ", output_pdf)

})


################################################################################
# 6 and 16. Food insecurity and dietary diversity
################################################################################

survey_food <- survey %>%
  mutate(
    hdds_no_misc_clean = num_col(., "hdds_no_misc"),
    hdds_assume_misc_1_clean = num_col(., "hdds_assume_misc_1"),
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
    fcs_category_standard = case_when(
      is.na(fcs) ~ NA_character_,
      fcs <= 21 ~ "poor",
      fcs <= 35 ~ "borderline",
      fcs > 35 ~ "acceptable"
    ),
    fcs_category_adjusted = case_when(
      is.na(fcs) ~ NA_character_,
      fcs <= 28 ~ "poor",
      fcs <= 42 ~ "borderline",
      fcs > 42 ~ "acceptable"
    ),
    # Retain the historical standard-threshold alias used by downstream code.
    fcs_category = fcs_category_standard,
    fcs_method = paste(
      "Reconstructed from item-level 7-day frequencies;",
      "items are summed within WFP food groups and capped at 7 days"
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
    hdds_no_misc_from_24h = hdds_no_misc,
    hdds_assume_misc_1_from_24h = if_else(
      !is.na(hdds_no_misc_from_24h),
      hdds_no_misc_from_24h + 1,
      NA_real_
    ),
    # HDDS is a 24-hour construct; the 7-day cleaner output is not coalesced.
    hdds_no_misc = hdds_no_misc_from_24h,
    hdds_assume_misc_1 = hdds_assume_misc_1_from_24h
  ) %>%
  select(
    -hdds_no_misc_clean,
    -hdds_assume_misc_1_clean,
    -hdds_no_misc_from_24h,
    -hdds_assume_misc_1_from_24h
  )

food_insecurity_summary <- bind_rows(
  summarise_continuous_vars(
    survey_food,
    tibble(
      source_variable = "fcs",
      outcome_name = "fcs",
      outcome_label = "Reconstructed Food Consumption Score"
    ),
    "food_insecurity_fcs",
    "score"
  ),
  summarise_binary_vars(
    survey_food,
    tibble(
      source_variable = "fcs_food_insecure",
      outcome_name = "fcs_food_insecure",
      outcome_label = "Poor or borderline reconstructed FCS"
    ),
    "food_insecurity_fcs",
    "percent"
  )
)

fcs_category_summary <- survey_food %>%
  filter(!is.na(fcs_category)) %>%
  add_all_arms_rows() %>%
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
    outcome_label = "Reconstructed FCS category (standard thresholds)",
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
  "table_descriptive_food_insecurity_scores.csv"
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
  labs(
    title = "Reconstructed Food Consumption Score categories",
    subtitle = "Standard 21/35 thresholds; see adjusted-threshold sensitivity output.",
    x = "Timepoint",
    y = "Households",
    fill = "FCS category"
  )
write_plot_if_data(
  fcs_category_summary,
  fig_fcs,
  "fig_descriptive_food_insecurity_scores.png",
  width = 8,
  height = 5
)

dietary_diversity_summary <- summarise_continuous_vars(
  survey_food,
  tibble(
    source_variable = c("hdds_no_misc", "hdds_assume_misc_1"),
    outcome_name = c("hdds_no_misc", "hdds_assume_misc_1"),
    outcome_label = c(
      "Observed 24-hour HDDS food groups (0-11; no miscellaneous group)",
      "24-hour HDDS sensitivity assuming miscellaneous group equals 1"
    )
  ),
  "dietary_diversity",
  "score"
)
write_reviewed_csv(
  dietary_diversity_summary,
  "table_descriptive_dietary_diversity_scores.csv"
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
  "fig_descriptive_dietary_diversity_scores.png",
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

  if ("target_child_wheezing_yn" %in% names(df)) {
    df <- df %>%
      mutate(
        target_child_asthma = case_when(
          target_child_wheezing_yn == 1 ~ 1L,
          target_child_wheezing_yn == 0 ~ 0L,
          TRUE ~ NA_integer_
        )
      )
  }

  df <- derive_child_severe_asthma_vars(df)

  df
}

survey_health <- add_health_descriptive_vars(survey)

severe_asthma_descriptive_coding_audit <- survey_health %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall, target_child_disturbed_speech_missing_type) %>%
  summarise(
    n_records = n(),
    n_child_wheeze_yes = sum(target_child_wheezing_yn == 1, na.rm = TRUE),
    n_disturbed_speech_nonmissing = sum(!is.na(target_child_distrubed_speech_yn_na_preserving)),
    n_severe_asthma_na_preserving_nonmissing = sum(!is.na(target_child_severe_asthma)),
    n_severe_asthma_skip_as_no_nonmissing = sum(!is.na(target_child_severe_asthma_skip_as_no)),
    n_severe_asthma_na_preserving_yes = sum(target_child_severe_asthma == 1, na.rm = TRUE),
    n_severe_asthma_skip_as_no_yes = sum(target_child_severe_asthma_skip_as_no == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall, target_child_disturbed_speech_missing_type)

write_reviewed_csv(
  severe_asthma_descriptive_coding_audit,
  "table_descriptive_child_severe_asthma_coding_audit.csv",
  subfolder = "qa"
)
asthma_labels <- tibble(
  source_variable = c(
    "target_child_asthma",
    "target_child_severe_asthma",
    "target_child_severe_asthma_skip_as_no"
  ),
  outcome_name = source_variable,
  outcome_label = c(
    "Child asthma proxy",
    "Child severe asthma proxy (NA-preserving)",
    "Child severe asthma proxy (skip-as-no sensitivity)"
  )
) %>%
  filter(source_variable %in% names(survey_health))

asthma_summary <- summarise_binary_vars(
  survey_health, asthma_labels, "asthma_and_severe_asthma"
)
write_reviewed_csv(
  asthma_summary,
  "table_descriptive_child_asthma_prevalence.csv"
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
  "fig_descriptive_child_asthma_prevalence.png",
  width = 8,
  height = 5
)

physical_health_labels <- tibble(
  source_variable = c(
    "target_child_cough_yn",
    "target_child_resp_rate_yn",
    "target_child_wheezing_yn",
    "target_child_disturbed_sleep_yn",
    "target_child_distrubed_speech_yn",
    "target_child_eye_red_yn",
    "target_child_eye_itch_yn",
    "target_child_lethargy_yn",
    "target_child_weight_loss_yn",
    "target_child_fever_yn",
    "target_child_clinic_resp_yn",
    "lpg_child_burn",
    "respondent_cough_yn",
    "resp_rate_reported_respondent_yn",
    "respondent_wheezing_yn",
    "respondent_disturbed_sleep_yn",
    "respondent_disturbed_speech_yn",
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
    "Disturbed sleep during wheeze",
    "Disturbed speech during wheeze",
    "Red eyes",
    "Itchy eyes",
    "Lethargy",
    "Unexplained weight loss in 3 mo.",
    "Fever",
    "Clinic visit for respiratory complaint",
    "Burned by LPG",
    "Cough",
    "Increased respiratory rate today",
    "Current wheeze",
    "Disturbed sleep during wheeze",
    "Disturbed speech during wheeze",
    "Red eyes",
    "Itchy eyes",
    "Sore eyes",
    "Unexplained weight loss in 3 mo.",
    "Headache",
    "Backache"
  ),
  respondent_group = c(rep("Child", 12), rep("Caregiver", 11)),
  display_order = seq_len(23)
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
  "table_descriptive_physical_health_symptoms.csv"
)

physical_health_figure_exclusions <- c(
  "lpg_child_burn",
  "resp_rate_reported_respondent_yn",
  "weight_loss_reported_respondent_yn"
)
physical_health_figure_labels <- physical_health_labels %>%
  filter(source_variable %notin% physical_health_figure_exclusions)

physical_health_plot_data <- physical_health_summary %>%
  filter(
    n_nonmissing > 0,
    !is.na(percent),
    study_arm_overall %in% arm_levels,
    source_variable %notin% physical_health_figure_exclusions
  ) %>%
  mutate(
    timepoint = factor(as.character(timepoint), levels = timepoint_levels),
    study_arm_overall = factor(
      as.character(study_arm_overall),
      levels = arm_levels
    ),
    facet_label = factor(
      str_wrap(paste(respondent_group, outcome_label, sep = ": "), width = 24),
      levels = str_wrap(
        paste(
          physical_health_figure_labels$respondent_group,
          physical_health_figure_labels$outcome_label,
          sep = ": "
        ),
        width = 24
      )
    )
  )

fig_physical_health <- ggplot(
  physical_health_plot_data,
  aes(
    x = timepoint,
    y = percent,
    color = study_arm_overall,
    group = study_arm_overall
  )
) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 1.8, na.rm = TRUE) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.08,
    linewidth = 0.4,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ facet_label,
    ncol = sum(physical_health_figure_labels$respondent_group == "Child")
  ) +
  scale_color_manual(values = arm_colors[arm_levels], drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    strip.text = element_text(size = 8)
  ) +
  labs(x = "Timepoint", y = "Percent reporting outcome", color = "Study arm")
write_plot_if_data(
  physical_health_plot_data,
  fig_physical_health,
  "fig_descriptive_physical_health_symptoms.png",
  width = 16,
  height = 8
)

################################################################################
# Mental health item scores and CES-D score
################################################################################

mental_health_requested_vars <- c(
  "bothered",
  "no_appetite",
  "feeling_down",
  "self_worth",
  "diff_concentrating",
  "depressed",
  "exert_effort",
  "hopeful",
  "life_failure",
  "fearful",
  "restless_sleep",
  "happy",
  "less_talkative",
  "lonely",
  "unfriendly_people",
  "enjoyed_life",
  "crying_spells",
  "sick",
  "feeling_disliked",
  "cant_get_going",
  "suicidal_thoughts_30"
)

mental_health_cesd_vars <- setdiff(
  mental_health_requested_vars,
  "suicidal_thoughts_30"
)

mental_health_positive_affect_vars <- c(
  "happy", "enjoyed_life", "self_worth", "hopeful"
)

clean_mental_health_source_value <- function(x) {
  out <- as_number(x)
  out[out %in% c(77, 88, 99)] <- NA_real_
  out[!is.na(out) & !out %in% 0:4] <- NA_real_
  out
}

mental_health_frequency_score <- function(x, var) {
  out <- clean_mental_health_source_value(x)
  if (var %in% mental_health_positive_affect_vars) {
    out <- 4 - out
  }
  pmin(out, 3)
}

mental_health_cesd_item_score <- function(x, var) {
  out <- clean_mental_health_source_value(x)
  if (var %in% mental_health_positive_affect_vars) {
    return(pmax(out - 1, 0))
  }
  pmin(out, 3)
}

add_mental_health_descriptive_vars <- function(df) {
  cesd_present_vars <- mental_health_cesd_vars[mental_health_cesd_vars %in% names(df)]
  if (length(cesd_present_vars) == length(mental_health_cesd_vars)) {
    cesd_mat <- as.data.frame(lapply(
      cesd_present_vars,
      function(var) mental_health_cesd_item_score(df[[var]], var)
    ))
    cesd_missing_items <- rowSums(is.na(cesd_mat))
    df$CES_D_score <- rowSums(cesd_mat)
    df$CES_D_score[cesd_missing_items > 0] <- NA_real_
    df$CES_D_go16_score <- case_when(
      is.na(df$CES_D_score) ~ NA_integer_,
      df$CES_D_score >= 16 ~ 1L,
      TRUE ~ 0L
    )
  } else {
    df$CES_D_score <- NA_real_
    df$CES_D_go16_score <- NA_integer_
  }

  for (var in intersect(mental_health_requested_vars, names(df))) {
    df[[var]] <- mental_health_frequency_score(df[[var]], var)
  }

  df
}

mental_health_labels <- tibble(
  source_variable = mental_health_requested_vars,
  outcome_name = source_variable,
  outcome_label = c(
    "Bothered by things",
    "No appetite",
    "Feeling down",
    "Low self-worth",
    "Difficulty concentrating",
    "Depressed",
    "Everything was an effort",
    "Hopeful",
    "Life felt like a failure",
    "Fearful",
    "Restless sleep",
    "Happy",
    "Less talkative",
    "Lonely",
    "People were unfriendly",
    "Enjoyed life",
    "Crying spells",
    "Felt sick",
    "Felt disliked",
    "Could not get going",
    "Suicidal thoughts in past 30 days"
  ),
  display_order = seq_along(mental_health_requested_vars),
  cesd_component = source_variable %in% mental_health_cesd_vars,
  scoring_note = if_else(
    cesd_component,
    "Item is summarized on the standard four-category response-frequency scale: 0 = never, 1 = rarely (1-2 days/week), 2 = sometimes (3-4 days/week), and 3 = most or all of the time (5-6 days/week or every day). Positive-affect items are returned to their response-frequency direction for item summaries and reverse-scored only for CES-D.",
    "Item is summarized on the standard four-category response-frequency scale: 0 = never, 1 = rarely (1-2 days/week), 2 = sometimes (3-4 days/week), and 3 = most or all of the time (5-6 days/week or every day); it is not included in the 20-item CES-D score."
  )
)

survey_mental_health <- add_mental_health_descriptive_vars(survey_health)

mental_health_variable_availability <- flag_missing_vars(
  survey_mental_health,
  c(mental_health_requested_vars, "CES_D_score", "CES_D_go16_score"),
  "Mental health descriptive outcomes"
)

write_reviewed_csv(
  mental_health_variable_availability,
  "table_descriptive_mental_health_variable_availability.csv",
  subfolder = "qa"
)

mental_health_labels_available <- mental_health_labels %>%
  filter(source_variable %in% names(survey_mental_health))

mental_health_item_summary <- summarise_continuous_vars(
  survey_mental_health,
  mental_health_labels_available %>%
    select(source_variable, outcome_name, outcome_label),
  "mental_health_item_scores",
  "item_score"
) %>%
  left_join(
    mental_health_labels_available %>%
      select(source_variable, display_order, cesd_component, scoring_note),
    by = "source_variable"
  )

mental_health_cesd_label <- tibble(
  source_variable = "CES_D_score",
  outcome_name = "CES_D_score",
  outcome_label = "CES-D score",
  display_order = length(mental_health_requested_vars) + 1L,
  cesd_component = NA,
  scoring_note = "Aggregate score calculated as the complete-case sum of 20 standard 0-3 CES-D item scores, with positive-affect items reverse-scored; suicidal_thoughts_30 is not included."
)

mental_health_cesd_summary <- summarise_continuous_vars(
  survey_mental_health,
  mental_health_cesd_label %>%
    select(source_variable, outcome_name, outcome_label),
  "mental_health_cesd_score",
  "score"
) %>%
  left_join(
    mental_health_cesd_label %>%
      select(source_variable, display_order, cesd_component, scoring_note),
    by = "source_variable"
  )

mental_health_cesd_at_risk_label <- tibble(
  source_variable = "CES_D_go16_score",
  outcome_name = "CES_D_go16_score",
  outcome_label = "At risk for depression (CES-D score >=16)",
  display_order = length(mental_health_requested_vars) + 2L,
  cesd_component = NA,
  scoring_note = "Binary indicator equal to 1 when the complete 20-item CES-D score is at least 16 and 0 otherwise. The table mean is the proportion at risk."
)

mental_health_cesd_at_risk_summary <- summarise_continuous_vars(
  survey_mental_health,
  mental_health_cesd_at_risk_label %>%
    select(source_variable, outcome_name, outcome_label),
  "mental_health_depression_risk",
  "proportion"
) %>%
  left_join(
    mental_health_cesd_at_risk_label %>%
      select(source_variable, display_order, cesd_component, scoring_note),
    by = "source_variable"
  ) %>%
  mutate(
    n_at_risk = as.integer(round(mean * n_nonmissing)),
    percent_at_risk = 100 * mean
  )

mental_health_summary <- bind_rows(
  mental_health_item_summary,
  mental_health_cesd_summary,
  mental_health_cesd_at_risk_summary
) %>%
  relocate(n_at_risk, percent_at_risk, .after = n_nonmissing) %>%
  arrange(display_order, timepoint, study_arm_overall)

write_reviewed_csv(
  mental_health_summary,
  "table_descriptive_mental_health_outcomes.csv"
)

mental_health_plot_data <- mental_health_summary %>%
  filter(
    n_nonmissing > 0,
    !is.na(mean),
    source_variable %in% mental_health_requested_vars,
    study_arm_overall %in% arm_levels
  ) %>%
  mutate(
    timepoint = factor(as.character(timepoint), levels = timepoint_levels),
    study_arm_overall = factor(
      as.character(study_arm_overall),
      levels = arm_levels
    ),
    facet_label = factor(
      str_wrap(outcome_label, width = 24),
      levels = str_wrap(mental_health_labels$outcome_label, width = 24)
    )
  )

write_reviewed_csv(
  mental_health_plot_data,
  "table_descriptive_mental_health_panel_plot_data.csv"
)

fig_mental_health <- ggplot(
  mental_health_plot_data,
  aes(
    x = timepoint,
    y = mean,
    color = study_arm_overall,
    group = study_arm_overall
  )
) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_point(aes(shape = timepoint), size = 1.9, na.rm = TRUE) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.08,
    linewidth = 0.4,
    na.rm = TRUE
  ) +
  facet_wrap(~ facet_label, nrow = 3) +
  scale_color_manual(values = arm_colors, drop = FALSE) +
  scale_shape_manual(
    values = c(baseline = 16, midline = 17, endline = 15),
    drop = FALSE
  ) +
  scale_y_continuous(
    breaks = 0:3,
    labels = c(
      "0\nNever",
      "1\nRarely\n(1-2 days/week)",
      "2\nSometimes\n(3-4 days/week)",
      "3\nMost or all of the time\n(5-6 days/week or every day)"
    ),
    limits = c(0, 3),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    axis.text.y = element_text(size = 7),
    strip.text = element_text(size = 8),
    panel.spacing.x = unit(0.8, "lines"),
    panel.spacing.y = unit(1.2, "lines")
  ) +
  labs(
    x = "Timepoint",
    y = "Mean response frequency",
    color = "Study arm",
    shape = "Timepoint"
  )

write_plot_if_data(
  mental_health_plot_data,
  fig_mental_health,
  "fig_descriptive_mental_health_outcomes.png",
  width = 18,
  height = 10
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
  "table_descriptive_fuel_collection_time.csv"
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
  "fig_descriptive_fuel_collection_time.png",
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
  add_all_arms_rows() %>%
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
  "table_descriptive_time_use_changes.csv"
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
  "fig_descriptive_time_use_changes.png",
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
  "table_descriptive_household_expenditures.csv"
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
  "fig_descriptive_household_expenditures.png",
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
# 4_rdid_xgboost_20260805_2213.R rather than modeled here.

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

add_harassment_ci_with_denominator <- function(df,
                                               numerator_col = "n_harassed",
                                               denominator_col = "num_people_collect_fuel") {
  if (nrow(df) == 0) {
    df$ci_lower <- numeric(0)
    df$ci_upper <- numeric(0)
    return(df)
  }

  ci <- t(mapply(function(x, n) {
    # Match the draft: report Wilson intervals only when at least one
    # person was harassed. Also leave the CI missing if the numerator is larger
    # than the reconstructed collector denominator, which indicates a data issue.
    if (is.na(n) || n <= 0 || is.na(x) || x <= 0 || x > n) {
      return(c(NA_real_, NA_real_))
    }
    harassment_prop_ci(x, n)
  }, df[[numerator_col]], df[[denominator_col]]))

  df$ci_lower <- ci[, 1]
  df$ci_upper <- ci[, 2]
  df
}

summarise_harassment_binary <- function(df, grouping_vars) {
  if (nrow(df) == 0) {
    return(tibble())
  }

  df %>%
    add_all_arms_rows() %>%
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
      "at midline only in clean_final. The denominator is the arm-stratified",
      "count of people in the same demographic reported to collect the same",
      "fuel at midline, reconstructed from clean_final fuel-procurement _ever",
      "variables rather than read from the unstratified aggregate CSV."
    ),
    available = source_variable %in% names(survey)
  )

fuel_procurement_who_midline_sources <- tidyr::expand_grid(
  harassment_fuel_lookup,
  harassment_demographic_lookup
) %>%
  mutate(
    source_variable = paste(fuel_type, demographic, "ever", sep = "_"),
    source_role = "midline_arm_stratified_fuel_procurement_person_denominator",
    intended_timepoint = "midline",
    available = source_variable %in% names(survey)
  )

write_reviewed_csv(
  fuel_procurement_who_midline_sources,
  "table_descriptive_fuel_procurement_availability.csv",
  subfolder = "qa"
)

fuel_procurement_who_midline_vars <- fuel_procurement_who_midline_sources %>%
  filter(available) %>%
  pull(source_variable)

fuel_procurement_who_midline_arm_stratified <- tibble()
if (length(fuel_procurement_who_midline_vars) > 0) {
  fuel_procurement_who_midline_arm_stratified <- survey %>%
    filter(timepoint == "midline") %>%
    select(fcn_id, timepoint, study_arm_overall,
           all_of(fuel_procurement_who_midline_vars)) %>%
    pivot_longer(
      cols = all_of(fuel_procurement_who_midline_vars),
      names_to = "source_variable",
      values_to = "collected_yn"
    ) %>%
    left_join(fuel_procurement_who_midline_sources, by = "source_variable") %>%
    mutate(
      collector_value = as_number(collected_yn),
      collector_yes = case_when(
        is.na(collector_value) ~ NA_integer_,
        collector_value > 0 ~ 1L,
        collector_value == 0 ~ 0L,
        TRUE ~ NA_integer_
      )
    ) %>%
    add_all_arms_rows() %>%
    group_by(timepoint, study_arm_overall, fuel_type, fuel_type_label,
             demographic, demographic_label, source_variable, source_role) %>%
    summarise(
      n_household_records = n_distinct(fcn_id),
      n_collector_item_nonmissing = sum(!is.na(collector_yes)),
      n_collector_item_yes = sum(collector_yes == 1L, na.rm = TRUE),
      n_collector_item_no = sum(collector_yes == 0L, na.rm = TRUE),
      num_people_collect_fuel = n_collector_item_yes,
      pc_collect_fuel = if_else(
        n_household_records > 0,
        100 * num_people_collect_fuel / n_household_records,
        NA_real_
      ),
      denominator_source = "clean_final_survey_refugee_household_midline_fuel_procurement_ever_variables",
      denominator_note = paste(
        "Arm-stratified denominator reconstructed from clean_final midline",
        "fuel-procurement person variables. This replaces the unstratified",
        "unstratified aggregate denominator."
      ),
      .groups = "drop"
    ) %>%
    arrange(study_arm_overall, fuel_type, demographic)
}

write_reviewed_csv(
  fuel_procurement_who_midline_arm_stratified,
  "table_descriptive_fuel_procurement_people.csv"
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
    add_all_arms_rows() %>%
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
  "table_descriptive_harassment_variable_audit.csv",
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
  "table_descriptive_harassment_household_types.csv"
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
  "table_descriptive_harassment_household_categories.csv"
)

fuel_person_harassment_vars <- harassment_fuel_person_sources %>%
  filter(available) %>%
  pull(source_variable)

harassment_fuel_person_long <- tibble()
if (length(fuel_person_harassment_vars) > 0) {
  harassment_fuel_person_long <- survey %>%
    filter(timepoint == "midline") %>%
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
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall, outcome_group, fuel_type, fuel_type_label,
           harassment_category, harassment_type, harassment_type_label, display_order,
           demographic, demographic_label, source_variable, source_form, recall_note,
           population) %>%
  summarise(
    n_total = n(),
    n_item_nonmissing = sum(!is.na(harassed)),
    n_harassed = sum(harassed == 1, na.rm = TRUE),
    mean_times_reported = mean_safe(times_reported),
    .groups = "drop"
  ) %>%
  left_join(
    fuel_procurement_who_midline_arm_stratified %>%
      transmute(
        timepoint,
        study_arm_overall,
        fuel_type,
        demographic,
        denominator_source_variable = source_variable,
        denominator_source_role = source_role,
        n_household_records_for_denominator = n_household_records,
        n_collector_item_nonmissing,
        n_collector_item_yes,
        n_collector_item_no,
        num_people_collect_fuel,
        pc_collect_fuel,
        denominator_source,
        denominator_note
      ),
    by = c("timepoint", "study_arm_overall", "fuel_type", "demographic")
  ) %>%
  mutate(
    # Backward-compatible column name: for this detailed fuel/person table,
    # n_nonmissing is now the collector denominator, not item nonmissingness.
    n_nonmissing = num_people_collect_fuel,
    n_harassed_gt_denominator = !is.na(n_harassed) &
      !is.na(num_people_collect_fuel) &
      n_harassed > num_people_collect_fuel,
    percent_harassed = if_else(
      !is.na(num_people_collect_fuel) & num_people_collect_fuel > 0,
      100 * n_harassed / num_people_collect_fuel,
      NA_real_
    )
  ) %>%
  add_harassment_ci_with_denominator() %>%
  arrange(fuel_type, display_order, demographic, timepoint, study_arm_overall) %>%
  filter(!is.na(num_people_collect_fuel), num_people_collect_fuel > 0)

harassment_fuel_person_denominator_qa <- harassment_fuel_person_summary %>%
  transmute(
    timepoint,
    study_arm_overall,
    fuel_type,
    demographic,
    harassment_type,
    n_harassed,
    num_people_collect_fuel,
    percent_harassed,
    n_item_nonmissing,
    n_harassed_gt_denominator,
    denominator_source_variable,
    denominator_note
  )

write_reviewed_csv(
  harassment_fuel_person_denominator_qa,
  "table_descriptive_harassment_denominator_qa.csv",
  subfolder = "qa"
)

write_reviewed_csv(
  harassment_fuel_person_summary,
  "table_descriptive_harassment_fuel_collectors.csv"
)

# Backward-compatible reviewed harassment table: detailed fuel/person harassment,
# matching the structure of the earlier reviewed output but with clearer notes.
write_reviewed_csv(
  harassment_fuel_person_summary,
  "table_descriptive_harassment_summary.csv"
)

harassment_category_plot_data <- harassment_household_category_summary %>%
  filter(n_nonmissing > 0) %>%
  mutate(
    outcome_label = factor(as.character(outcome_label), levels = harassment_category_levels),
    timepoint = as_ordered_timepoint(timepoint)
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
  "fig_descriptive_harassment_household_categories.png",
  width = 10,
  height = 5.5
)

harassment_plot_data <- harassment_fuel_person_summary %>%
  filter(
    timepoint == "midline",
    fuel_type %in% c("gather_scraps", "collect_wood", "receive_lpg"),
    harassment_category %in% c("Verbal/emotional", "Physical"),
    num_people_collect_fuel > 0
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
  "fig_descriptive_harassment_fuel_collectors.png",
  width = 12,
  height = 7
)

################################################################################# Coverage table and methods notes
################################################################################

coverage <- tribble(
  ~item, ~output_description, ~table_file, ~figure_file, ~status, ~note,
  1, "Types of cooking fuel used in past 30 days by arm/timepoint", "table_descriptive_fuel_use_past_month.csv; table_descriptive_fuel_use_past_month_subset.xlsx", "fig_descriptive_fuel_use_past_month.png", "complete", "Uses clean_final fuel_30 variables plus reviewed aliases for any LPG/wood/CRH.",
  2, "Usable duration of 12 kg LPG cylinder by household size", "table_descriptive_lpg_duration_household_size.csv", "fig_descriptive_lpg_duration_household_size.png", "complete", "Uses hh_size and lpg_days_possible; QA table flags zero and >120 day values.",
  3, "Livelihood training and use of skills", "table_descriptive_livelihood_training_skills.csv", "fig_descriptive_livelihood_training_skills.png", "complete_with_label_limitations", "Training/skill barrier option text was not preserved in clean_final; code outputs option codes and broad labels.",
  4, "Strategies used to cope with shortage of food", "table_descriptive_food_shortage_coping.csv", "fig_descriptive_food_shortage_coping.png; fig_descriptive_shortage_coping_with_plastic_burning.png", "complete", "The combined figure shows food_cant_afford_2wk, food_insufficient_nutrition, and food_didnt_want before all numbered food_cant_afford_action/1 through /13 panels. Their display labels use manuscript wording; the source-table labels remain unchanged. Line values use pct_all_households (column N) with all households in each arm and survey round as the denominator.",
  5, "Strategies used to cope with shortage of fuel", "table_descriptive_fuel_shortage_coping.csv; table_descriptive_categorical_outcomes.csv", "fig_descriptive_fuel_shortage_coping.png; fig_descriptive_shortage_coping_with_plastic_burning.png", "complete", "The combined figure shows fuel_cant_afford_2wk before numbered fuel_cant_afford_action/1 through /15 panels and a full-width plastic-burning frequency bar chart. The display label for fuel action 1 uses manuscript wording; the original source-table wording note remains in the table. Line panels use pct_all_households (column N); plastic bars use n_category / n_total. Endline plastic codes 5 and 7 are grouped with 5-6 days and every day. All percentages use households in each arm and survey round as the denominator.",
  6, "Food insecurity", "table_descriptive_food_insecurity_scores.csv", "fig_descriptive_food_insecurity_scores.png", "complete", "Recalculates FCS from weekly food-frequency variables and categorizes poor/borderline/acceptable.",
  7, "Asthma and severe asthma", "table_descriptive_child_asthma_prevalence.csv", "fig_descriptive_child_asthma_prevalence.png", "complete", "Uses reviewed child wheeze proxy and severe asthma proxy from wheeze plus disturbed speech, with NA-preserving and skip-as-no severe-asthma rows.",
  8, "Time collecting fuel", "table_descriptive_fuel_collection_time.csv", "fig_descriptive_fuel_collection_time.png", "complete", "Summarizes walking/waiting time variables in hours; values <0.01 treated as missing as in old script.",
  9, "Time cooking", "table_descriptive_time_use_changes.csv", "fig_descriptive_time_use_changes.png", "complete", "Categorical more/same/less change variable time_cooking.",
  10, "Time caring for self", "table_descriptive_time_use_changes.csv", "fig_descriptive_time_use_changes.png", "partial", "No exact time_caring_for_self variable found in clean_final; time_eating is shown as a proxy and flagged in the label.",
  11, "Time caring for children", "table_descriptive_time_use_changes.csv", "fig_descriptive_time_use_changes.png", "complete", "Categorical more/same/less change variable time_caring_for_children.",
  12, "Time for children to go to school", "table_descriptive_time_use_changes.csv", "fig_descriptive_time_use_changes.png", "complete", "Categorical more/same/less change variable time_child_school.",
  13, "Money spent on firewood", "table_descriptive_household_expenditures.csv", "fig_descriptive_household_expenditures.png", "complete", "Uses buy_wood_cost converted from BDT to USD using reviewed exchange-rate table.",
  14, "Money spent on food", "table_descriptive_household_expenditures.csv", "fig_descriptive_household_expenditures.png", "complete", "Uses spent_food converted from BDT to USD using reviewed exchange-rate table.",
  15, "Money spent on items other than firewood and food", "table_descriptive_household_expenditures.csv", "fig_descriptive_household_expenditures.png", "complete", "Uses sum of spent_hh_items, spent_hygiene, spent_tobacco_pan, spent_transport, and other_expenditures when available.",
  16, "Dietary diversity", "table_descriptive_dietary_diversity_scores.csv", "fig_descriptive_dietary_diversity_scores.png", "complete", "Uses clean_final baseline-compatible HDDS variables derived from weekly food-frequency items, with the draft past-24-hour mapping retained as a fallback.",
  17, "Harassment", "table_descriptive_harassment_household_categories.csv; table_descriptive_harassment_fuel_collectors.csv; table_descriptive_harassment_summary.csv", "fig_descriptive_harassment_household_categories.png; fig_descriptive_harassment_fuel_collectors.png", "complete_descriptive_only", "Household-level category summaries are shown by arm/timepoint. Detailed fuel/person harassment is midline-only in clean_final and uses an arm-stratified midline person-collector denominator reconstructed from clean_final fuel-procurement variables; rDiD estimability is audited separately.",
  18, "Physical health outcomes with child outcomes on top and caregiver outcomes on bottom", "table_descriptive_physical_health_symptoms.csv", "fig_descriptive_physical_health_symptoms.png", "complete", "The full table contains 12 child and 11 caregiver outcomes. The figure displays the 11 requested child outcomes in the first row and 9 requested caregiver outcomes in the second row; it excludes child burned by LPG, caregiver increased respiratory rate, caregiver unexplained weight loss, and the pooled all-arms series.",
  19, "Mental health item scores and CES-D", "table_descriptive_mental_health_outcomes.csv", "fig_descriptive_mental_health_outcomes.png", "complete", "Summarizes the requested 21 mental-health items on the standard 0-3 response-frequency scale plus aggregate CES-D by arm/timepoint. The two highest observed frequency categories (5-6 days/week and every day) are combined as 3 = most or all of the time. The figure has three rows with a common 0-3 y-axis and excludes the pooled all-arms series. CES-D is the complete-case sum of 20 standard 0-3 item scores, reverse-scores the four positive-affect items, and excludes suicidal_thoughts_30."
)
write_reviewed_csv(
  coverage,
  "table_descriptive_output_coverage.csv",
  subfolder = "qa"
)

methods_notes <- c(
  "# RF105 Reviewed Descriptive Outputs",
  "",
  paste0("Generated on ", Sys.Date(), " by 5_analysis_RF105/reviewed/3_descriptive_outcomes_20260805_2213.R."),
  "",
  "Input data: 4_data/clean_final/survey_refugee_household.rds.",
  "Population: one deduplicated household record per fcn_id-timepoint using make_analysis_population().",
  "",
  "Important notes:",
  "- These are descriptive summaries only; no DiD/rDiD/statistical modeling is performed in this script.",
  "- Binary percentage tables keep percent, ci_lower, and ci_upper as n_yes / n_nonmissing for backward compatibility and also report percent_total, ci_lower_total, and ci_upper_total as n_yes / n_total. The matching *_nonmissing columns make the nonmissing denominator explicit.",
  "- Mental-health outputs summarize requested items at baseline, midline, and endline on the standard 0-3 response-frequency scale (never, rarely, sometimes, most or all of the time); 5-6 days/week and every day are combined as 3. CES-D uses the same 0-3 coding and reverse-scores the four positive-affect items; suicidal_thoughts_30 is retained separately and is not included in CES-D.",
  "- FCS is recalculated using the embedded FCS calculation; HDDS uses the clean_final baseline-compatible variables derived in the cleaner, with the embedded past-24-hour mapping as a fallback.",
  "- Time-use outcomes are categorical more/same/less changes, not measured minutes.",
  "- No exact clean_final variable named time_caring_for_self was found. The script summarizes time_eating as a proxy and flags this in the coverage QA table.",
  "- Harassment household summaries use all nonmissing household item responses. Detailed fuel/person harassment variables are midline-only in clean_final; the reviewed denominator is the arm-stratified number of people in the same demographic reported to collect the same fuel at midline, reconstructed from clean_final fuel-procurement _ever variables."
)
readr::write_lines(methods_notes, file.path(dir_tables_qa, "table_descriptive_methods_notes.md"))
message("Wrote QA notes: ", file.path(dir_tables_qa, "table_descriptive_methods_notes.md"))

message("RF105 reviewed descriptive analyses complete.")

################################################################################
# Consolidated descriptive sections formerly stored as separate reviewed scripts
#
# These sections were folded into this main descriptive script so collaborators
# can regenerate all descriptive tables and figures from one file. Each section
# is kept close to the prior reviewed script and wrapped in local({ ... }) to
# avoid accidental object-name collisions between older modules. No rDiD models
# are run here; rDiD fitting and post-processing belong in 4_rdid_xgboost_20260805_2213.R.
#
# Upstream dependency: 1_geocene_stove_use_20260805_2213.R should be run
# before this script because stove-use descriptive plots use its reviewed daily
# analysis dataset. The PM2.5 explainer section uses ambient-adjusted
# household-timepoint PM2.5 products created before the main descriptive script.
################################################################################

################################################################################
# Consolidated household Table 1 descriptive section
################################################################################
local({
################################################################################
# RF105 reviewed Table 1
#
# Purpose:
#   Recreate the RF105 baseline household characteristic tables using the
#   cleaned final refugee household survey file.
#
# Input:
#   4_data/clean_final/survey_refugee_household.rds
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_baseline_by_arm.csv
#   7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_baseline_followup_groups.csv
#   7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_attrition_comparison_arm.csv
#   7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_attrition_intervention_arm.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/tab1_*.csv
#
# Main checks/fixes relative to the older script:
#   1. Reads from 4_data/clean_final, not older intermediate survey files.
#   2. Preserves old variable names by creating documented aliases where the
#      clean_final names changed (for example fuel_30_scraps).
#   3. Does not silently ignore duplicate fcn_id/timepoint records; writes a
#      QA file before deduplicating one record per fcn_id/timepoint.
#   4. Writes a count reconciliation table against the current manuscript
#      Appendix Table 1 sample sizes.
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

survey_data_raw <- readRDS(file_survey_refugee_household) %>%
  add_rf105_aliases()

analysis_population <- make_analysis_population(survey_data_raw, id_var = "fcn_id")

write_reviewed_csv(
  analysis_population$duplicate_records,
  "table_descriptive_tab1_duplicate_records.csv",
  subfolder = "qa"
)

write_reviewed_csv(
  analysis_population$sample_counts,
  "table_descriptive_tab1_sample_counts.csv",
  subfolder = "qa"
)

################################################################################
# Table 1 variable definitions
################################################################################

table1_vars_original <- c(
  "camp_id",
  "block_id",
  "subblock_id",
  "n_hh_members",
  "n_kids",
  "target_child_months",
  "target_child_sex",
  "electricity",
  "electric_fan_yn",
  "debt_yn",
  "income_usd",
  "spent_total_month_usd",
  "n_meals_yesterday",
  "smartphone_yn",
  "mobile_phone_yn",
  "mattress_yn",
  "blanket_yn",
  "mosquito_net_yn",
  "umbrella_yn",
  "chair_bench_yn",
  "table_yn",
  "shovel_yn",
  "sickle_yn",
  "weaving_tool_yn",
  "chicken_duck_pigeon_yn"
)

table1_labels <- c(
  camp_id = "Camp",
  block_id = "Block",
  subblock_id = "Sub-block",
  n_hh_members = "Number of household members",
  n_kids = "Number of children",
  target_child_months = "Target child age, months",
  target_child_sex = "Target child sex",
  electricity = "Household has electricity",
  electric_fan_yn = "Owns electric fan",
  debt_yn = "Household has debt",
  income_usd = "Monthly income, USD",
  spent_total_month_usd = "Monthly expenditures, USD",
  n_meals_yesterday = "Meals eaten yesterday",
  smartphone_yn = "Owns smartphone",
  mobile_phone_yn = "Owns mobile phone",
  mattress_yn = "Owns mattress",
  blanket_yn = "Owns blanket",
  mosquito_net_yn = "Owns mosquito net",
  umbrella_yn = "Owns umbrella",
  chair_bench_yn = "Owns chair or bench",
  table_yn = "Owns table",
  shovel_yn = "Owns shovel",
  sickle_yn = "Owns sickle",
  weaving_tool_yn = "Owns weaving tool",
  chicken_duck_pigeon_yn = "Owns poultry"
)

binary_table1_vars <- c(
  "electricity",
  "electric_fan_yn",
  "debt_yn",
  "smartphone_yn",
  "mobile_phone_yn",
  "mattress_yn",
  "blanket_yn",
  "mosquito_net_yn",
  "umbrella_yn",
  "chair_bench_yn",
  "table_yn",
  "shovel_yn",
  "sickle_yn",
  "weaving_tool_yn",
  "chicken_duck_pigeon_yn"
)

prepare_table1_data <- function(df) {
  df %>%
    mutate(
      exchange_rate = exchange_bdt_per_usd[as.character(timepoint)],
      income_usd = suppressWarnings(as.numeric(income)) / exchange_rate,
      spent_total_month_usd =
        suppressWarnings(as.numeric(spent_total_month)) / exchange_rate
    )
}

baseline_complete <- analysis_population$complete_3_survey %>%
  filter(timepoint == "baseline") %>%
  prepare_table1_data()

baseline_followup <- analysis_population$baseline_followup_status %>%
  prepare_table1_data()

missing_table1_vars <- flag_missing_vars(
  baseline_followup,
  table1_vars_original,
  "RF105 Table 1 variables after clean_final aliases"
)

write_reviewed_csv(
  missing_table1_vars,
  "table_descriptive_tab1_variable_availability.csv",
  subfolder = "qa"
)

table1_vars_available <- missing_table1_vars %>%
  filter(available) %>%
  pull(variable)

################################################################################
# Sample size reconciliation with manuscript Appendix Table 1
################################################################################

current_count_reconciliation <- baseline_followup %>%
  count(study_arm_overall, attrition_status, name = "current_clean_final_n") %>%
  mutate(
    study_arm_overall = as.character(study_arm_overall),
    attrition_status = as.character(attrition_status)
  )

manuscript_count_reconciliation <- tribble(
  ~study_arm_overall, ~attrition_status, ~manuscript_appendix_table1_n,
  "comparison", "participated_in_all_3_surveys", 437,
  "comparison", "lost_before_endline", 161,
  "intervention", "participated_in_all_3_surveys", 494,
  "intervention", "lost_before_endline", 100
) %>%
  left_join(
    current_count_reconciliation,
    by = c("study_arm_overall", "attrition_status")
  ) %>%
  mutate(
    difference_current_minus_manuscript =
      current_clean_final_n - manuscript_appendix_table1_n,
    note = "Review before changing manuscript counts; differences may reflect clean_final ID deduplication or manuscript sample restriction."
  )

write_reviewed_csv(
  manuscript_count_reconciliation,
  "table_descriptive_tab1_count_reconciliation.csv",
  subfolder = "qa"
)

################################################################################
# Baseline characteristics among participants with all three surveys
################################################################################

tab1_baseline_by_arm <- make_characteristics_table(
  df = baseline_complete,
  vars = table1_vars_available,
  labels = table1_labels,
  group_var = "study_arm_overall",
  binary_vars = binary_table1_vars
)

write_reviewed_csv(
  tab1_baseline_by_arm,
  "table_descriptive_baseline_by_arm.csv"
)

################################################################################
# Baseline characteristics by arm and follow-up completion status
################################################################################

baseline_followup_for_table <- baseline_followup %>%
  mutate(
    arm_followup_group = paste(study_arm_overall, attrition_status, sep = "_")
  )

tab1_baseline_by_arm_followup <- make_characteristics_table(
  df = baseline_followup_for_table,
  vars = table1_vars_available,
  labels = table1_labels,
  group_var = "arm_followup_group",
  binary_vars = binary_table1_vars
)

write_reviewed_csv(
  tab1_baseline_by_arm_followup,
  "table_descriptive_baseline_followup_groups.csv"
)

tab1_attrition_comparison <- baseline_followup %>%
  filter(study_arm_overall == "comparison") %>%
  make_characteristics_table(
    vars = table1_vars_available,
    labels = table1_labels,
    group_var = "attrition_status",
    binary_vars = binary_table1_vars
  )

write_reviewed_csv(
  tab1_attrition_comparison,
  "table_descriptive_attrition_comparison_arm.csv"
)

tab1_attrition_intervention <- baseline_followup %>%
  filter(study_arm_overall == "intervention") %>%
  make_characteristics_table(
    vars = table1_vars_available,
    labels = table1_labels,
    group_var = "attrition_status",
    binary_vars = binary_table1_vars
  )

write_reviewed_csv(
  tab1_attrition_intervention,
  "table_descriptive_attrition_intervention_arm.csv"
)
})

################################################################################
# Consolidated PM2.5 descriptive and canonical analysis section
################################################################################

local({
timepoint_levels <- c("baseline", "midline", "endline")
arm_levels <- c("comparison", "intervention")

as_ordered_timepoint <- function(x, extra_levels = character()) {
  x_clean <- trimws(tolower(as.character(x)))
  factor(x_clean, levels = c(timepoint_levels, extra_levels), ordered = TRUE)
}

primary_model_label <- "retired_model_not_run_in_descriptive_script"
default_material_infiltration_factor <- 0.75
sensitivity_infiltration_factors <- c(0, 0.25, 0.50, 1.00)
valid_monitoring_coverage_threshold <- 0.75
monitoring_period_hours <- 24
monitoring_period_count <- 2
monitoring_total_hours <- monitoring_period_hours * monitoring_period_count
expected_monitoring_interval_seconds <- 60
long_monitoring_interval_threshold_seconds <- expected_monitoring_interval_seconds
hapin_hour_time_zone <- "Asia/Dhaka"
hapin_pm25_reference_lines <- c(15, 25, 35, 37.5, 50, 75)
hapin_pm25_exceedance_thresholds <- c(0, 15, 25, 35, 37.5, 50, 75, 150, 400, 1000)

input_indoor_path <- file_pm25_indoor
input_indoor_anomaly_retained_path <- NA_character_
input_ambient_path <- file_pm25_ambient
input_survey_household_path <- file_survey_refugee_household
input_survey_location_path <- file.path(dir_clean_final, "survey_refugee_location.rds")
input_survey_hh_members_path <- file.path(dir_clean_final, "survey_refugee_hh_members.rds")
analysis_date <- date_stamp
script_path <- file.path(script_dir, "3_descriptive_outcomes_20260805_2213.R")
table_dir <- dir_tables_reviewed
restricted_table_dir <- file.path(dir_restricted_reviewed, "identified_tables")
figure_dir <- dir_figures_reviewed
dir.create(restricted_table_dir, recursive = TRUE, showWarnings = FALSE)

check_required_columns <- function(data, required_cols, data_name) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      data_name,
      " is missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}

geo_mean <- function(x) {
  x <- x[is.finite(x) & !is.na(x) & x > 0]
  if (length(x) == 0) return(NA_real_)
  exp(mean(log(x)))
}

first_nonmissing <- function(x) {
  if (is.character(x)) {
    x <- x[!is.na(x) & nzchar(x)]
  } else {
    x <- x[!is.na(x)]
  }
  if (length(x) == 0) return(NA)
  x[[1]]
}

as_number <- function(x) {
  suppressWarnings(as.numeric(x))
}

weighted_mean_pm <- function(x, w) {
  x <- as_number(x)
  w <- as_number(w)
  keep <- !is.na(x) & is.finite(x) & !is.na(w) & is.finite(w) & w > 0
  if (!any(keep)) return(NA_real_)
  sum(x[keep] * w[keep]) / sum(w[keep])
}

clean_hours_0_24 <- function(x) {
  x <- as_number(x)
  x[!is.finite(x) | x < 0 | x > 24] <- NA_real_
  x
}

clean_age_years <- function(x) {
  x <- as_number(x)
  x[!is.finite(x) | x < 0 | x > 120] <- NA_real_
  x
}

mean_or_na <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

safe_quantile <- function(x, prob) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = prob, na.rm = TRUE, names = FALSE, type = 7))
}

mean_datetime <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  as.POSIXct(mean(as.numeric(x)), origin = "1970-01-01", tz = "UTC")
}

as_clean_datetime <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  parsed <- suppressWarnings(lubridate::ymd_hms(x, quiet = TRUE, tz = "UTC"))
  if (all(is.na(parsed))) {
    parsed <- suppressWarnings(lubridate::ymd_hm(x, quiet = TRUE, tz = "UTC"))
  }
  parsed
}

median_or_na <- function(x) {
  x <- as_number(x)
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  median(x)
}

min_or_na <- function(x) {
  x <- as_number(x)
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

max_or_na <- function(x) {
  x <- as_number(x)
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

mode_number <- function(x) {
  x <- as_number(x)
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  x <- round(x, 3)
  tab <- table(x)
  as.numeric(names(tab)[which.max(tab)])
}

pct_at_mode <- function(x) {
  x <- as_number(x)
  x <- x[!is.na(x) & is.finite(x)]
  mode_x <- mode_number(x)
  if (length(x) == 0 || is.na(mode_x) || !is.finite(mode_x)) return(NA_real_)
  mean(round(x, 3) == mode_x) * 100
}

summarize_monitoring_period_coverage <- function(timestamp_rows, start_datetime, usual_interval_seconds, period_number) {
  period_start <- start_datetime + lubridate::hours(monitoring_period_hours * (period_number - 1L))
  period_end <- start_datetime + lubridate::hours(monitoring_period_hours * period_number)

  if (
    nrow(timestamp_rows) == 0 ||
      is.na(usual_interval_seconds) ||
      !is.finite(usual_interval_seconds) ||
      usual_interval_seconds <= 0
  ) {
    covered_seconds <- 0
    n_valid_timestamps <- 0L
  } else {
    represented_seconds <- ifelse(
      !is.na(timestamp_rows$positive_interval_seconds) &
        is.finite(timestamp_rows$positive_interval_seconds) &
        timestamp_rows$positive_interval_seconds > 0,
      pmin(timestamp_rows$positive_interval_seconds, usual_interval_seconds),
      usual_interval_seconds
    )
    observation_start <- as.numeric(timestamp_rows$dateTime)
    observation_end <- observation_start + represented_seconds
    period_start_num <- as.numeric(period_start)
    period_end_num <- as.numeric(period_end)
    overlap_seconds <- pmax(
      0,
      pmin(observation_end, period_end_num) - pmax(observation_start, period_start_num)
    )
    covered_seconds <- sum(overlap_seconds, na.rm = TRUE)
    n_valid_timestamps <- sum(timestamp_rows$dateTime >= period_start & timestamp_rows$dateTime < period_end, na.rm = TRUE)
  }

  coverage_prop <- min(1, covered_seconds / (monitoring_period_hours * 3600))
  data.frame(
    period_number = period_number,
    period_start_datetime = period_start,
    period_end_datetime = period_end,
    n_valid_timestamps = n_valid_timestamps,
    valid_monitoring_hours = covered_seconds / 3600,
    coverage_prop = coverage_prop,
    coverage_percent = coverage_prop * 100,
    has_valid_75pct_coverage = coverage_prop >= valid_monitoring_coverage_threshold,
    stringsAsFactors = FALSE
  )
}

make_scaffold <- function() {
  expand.grid(
    timepoint = timepoint_levels,
    study_arm_overall = arm_levels,
    stringsAsFactors = FALSE
  )
}

write_scaffolded_csv <- function(data, path) {
  scaffold <- make_scaffold()
  out <- merge(scaffold, data, by = c("timepoint", "study_arm_overall"), all.x = TRUE)
  out <- out[order(match(out$timepoint, timepoint_levels), match(out$study_arm_overall, arm_levels)), ]
  readr::write_csv(out, path, na = "")
  invisible(path)
}

message("Reading cleaned PM2.5 inputs")
if (!file.exists(input_indoor_path)) stop("Missing input file: ", input_indoor_path, call. = FALSE)
if (!file.exists(input_ambient_path)) stop("Missing input file: ", input_ambient_path, call. = FALSE)
if (!file.exists(input_survey_household_path)) stop("Missing input file: ", input_survey_household_path, call. = FALSE)
if (!file.exists(input_survey_location_path)) stop("Missing input file: ", input_survey_location_path, call. = FALSE)
if (!file.exists(input_survey_hh_members_path)) stop("Missing input file: ", input_survey_hh_members_path, call. = FALSE)

indoor <- readRDS(input_indoor_path)
indoor_anomaly_retained <- NULL
ambient <- readRDS(input_ambient_path)
survey_household <- readRDS(input_survey_household_path)
survey_location <- readRDS(input_survey_location_path)
survey_hh_members <- readRDS(input_survey_hh_members_path)
pm25_bound_qa <- bind_rows(
  tibble(source = "indoor", value = suppressWarnings(as.numeric(indoor$pm25_ug_m3))),
  tibble(source = "ambient", value = suppressWarnings(as.numeric(ambient$pm25_ug_m3)))
) %>%
  group_by(source) %>%
  summarise(
    n_rows = n(),
    n_nonfinite_or_missing = sum(!is.finite(value) | is.na(value)),
    n_below_lower_bound_10 = sum(is.finite(value) & !is.na(value) & value < 10),
    n_above_upper_bound_30000 = sum(is.finite(value) & !is.na(value) & value > 30000),
    lower_bound_ug_m3 = 10,
    upper_bound_ug_m3 = 30000,
    upper_tail_trim_applied = FALSE,
    .groups = "drop"
  )
readr::write_csv(pm25_bound_qa, file.path(dir_tables_qa, "table_qa_pm25_bounds.csv"), na = "")

check_required_columns(
  indoor,
  c(
    "timepoint", "study_arm_overall", "hh_id", "fcn_id", "hh_id_note",
    "raw_source_file", "PM_monitor", "dateTime", "pm25_ug_m3"
  ),
  "pm25_pats_refugee_indoor.rds"
)
if (!is.null(indoor_anomaly_retained)) {
  check_required_columns(
    indoor_anomaly_retained,
    c(
      "timepoint", "study_arm_overall", "hh_id", "fcn_id", "hh_id_note",
      "raw_source_file", "PM_monitor", "dateTime", "pm25_ug_m3"
    ),
    "pm25_pats_refugee_indoor_anomaly_retained_sensitivity.rds"
  )
}
check_required_columns(
  ambient,
  c(
    "timepoint", "study_arm_overall", "ambient_site_id", "note_clean",
    "raw_source_file", "dateTime", "pm25_ug_m3"
  ),
  "pm25_pats_refugee_ambient.rds"
)
check_required_columns(
  survey_household,
  c(
    "fcn_id", "hh_id", "KEY", "timepoint", "study_arm_overall",
    "hh_size", "hh_per_structure", "hours_inside", "hours_outside",
    "target_child_hours_inside", "target_child_hours_outside", "respondent_sl"
  ),
  "survey_refugee_household.rds"
)
check_required_columns(
  survey_location,
  c("PARENT_KEY", "location_number", "timepoint", "hours_inside", "hours_outside"),
  "survey_refugee_location.rds"
)
check_required_columns(
  survey_hh_members,
  c("PARENT_KEY", "mem_serial", "timepoint", "age_yrs", "hours_outside"),
  "survey_refugee_hh_members.rds"
)

prepare_indoor_pm <- function(data) {
  data %>%
    mutate(
      dateTime = as_clean_datetime(dateTime),
      timepoint = as_ordered_timepoint(timepoint),
      study_arm_overall = as.character(study_arm_overall),
      hh_id = as.character(hh_id),
      fcn_id = as.character(fcn_id),
      hh_id_note = as.character(hh_id_note),
      raw_source_file = as.character(raw_source_file),
      PM_monitor = as.character(PM_monitor),
      pm25_ug_m3 = pmin(pmax(as.numeric(pm25_ug_m3), 10), 30000)
    )
}

indoor <- prepare_indoor_pm(indoor)
if (!is.null(indoor_anomaly_retained)) {
  indoor_anomaly_retained <- prepare_indoor_pm(indoor_anomaly_retained)
}

ambient <- ambient %>%
  mutate(
    dateTime = as_clean_datetime(dateTime),
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    ambient_site_id = as.character(ambient_site_id),
    note_clean = as.character(note_clean),
    raw_source_file = as.character(raw_source_file),
    PM_monitor = as.character(PM_monitor),
    pm25_ug_m3 = pmin(pmax(as.numeric(pm25_ug_m3), 10), 30000)
  )

message("Auditing indoor PM2.5 data collection intervals and 24-hour monitoring coverage")
monitoring_window_keys <- c(
  "timepoint", "study_arm_overall", "hh_id", "fcn_id", "hh_id_note",
  "raw_source_file", "PM_monitor"
)

indoor_monitoring_valid <- indoor %>%
  filter(
    timepoint %in% timepoint_levels,
    study_arm_overall %in% arm_levels,
    !is.na(hh_id),
    !is.na(dateTime),
    !is.na(pm25_ug_m3),
    is.finite(pm25_ug_m3),
    pm25_ug_m3 > 0
  )

if (nrow(indoor_monitoring_valid) == 0) {
  stop("No valid indoor PM2.5 observations available for monitoring coverage audit.", call. = FALSE)
}

monitoring_window_index <- indoor_monitoring_valid %>%
  group_by(across(all_of(monitoring_window_keys))) %>%
  summarise(
    start_datetime = min(dateTime, na.rm = TRUE),
    end_datetime = max(dateTime, na.rm = TRUE),
    n_valid_observations = n(),
    n_unique_timestamps = n_distinct(dateTime),
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall, start_datetime, raw_source_file, PM_monitor) %>%
  mutate(
    monitoring_coverage_window_id = sprintf("coverage_window_%04d", row_number()),
    elapsed_hours_first_to_last = as.numeric(difftime(end_datetime, start_datetime, units = "hours"))
  )

monitoring_window_lookup <- monitoring_window_index %>%
  select(
    all_of(monitoring_window_keys),
    monitoring_coverage_window_id,
    deployment_start_datetime = start_datetime
  )

indoor_monitoring_timestamps <- indoor_monitoring_valid %>%
  inner_join(monitoring_window_lookup, by = monitoring_window_keys) %>%
  distinct(monitoring_coverage_window_id, dateTime, .keep_all = TRUE) %>%
  arrange(monitoring_coverage_window_id, dateTime) %>%
  group_by(monitoring_coverage_window_id) %>%
  mutate(
    next_datetime = lead(dateTime),
    next_pm25_ug_m3 = lead(pm25_ug_m3),
    interval_seconds = as.numeric(difftime(next_datetime, dateTime, units = "secs")),
    positive_interval_seconds = ifelse(
      !is.na(interval_seconds) & is.finite(interval_seconds) & interval_seconds > 0,
      round(interval_seconds, 3),
      NA_real_
    ),
    monitor_hour = lubridate::floor_date(dateTime, unit = "hour")
  ) %>%
  ungroup()

global_modal_interval_seconds <- mode_number(indoor_monitoring_timestamps$positive_interval_seconds)

monitoring_window_interval_diagnostics <- indoor_monitoring_timestamps %>%
  group_by(monitoring_coverage_window_id) %>%
  summarise(
    n_positive_intervals = sum(!is.na(positive_interval_seconds)),
    modal_interval_seconds = mode_number(positive_interval_seconds),
    median_interval_seconds = median_or_na(positive_interval_seconds),
    min_interval_seconds = min_or_na(positive_interval_seconds),
    max_interval_seconds = max_or_na(positive_interval_seconds),
    n_distinct_positive_intervals = n_distinct(positive_interval_seconds[!is.na(positive_interval_seconds)]),
    pct_positive_intervals_at_window_mode = pct_at_mode(positive_interval_seconds),
    .groups = "drop"
  ) %>%
  mutate(
    all_positive_intervals_same = ifelse(
      n_positive_intervals > 0,
      n_distinct_positive_intervals == 1L,
      NA
    ),
    usual_interval_seconds = ifelse(
      !is.na(modal_interval_seconds) & is.finite(modal_interval_seconds),
      modal_interval_seconds,
      global_modal_interval_seconds
    )
  )

monitoring_window_index <- monitoring_window_index %>%
  left_join(monitoring_window_interval_diagnostics, by = "monitoring_coverage_window_id")

long_interval_within_window_details <- indoor_monitoring_timestamps %>%
  filter(
    !is.na(positive_interval_seconds),
    positive_interval_seconds > long_monitoring_interval_threshold_seconds
  ) %>%
  mutate(
    gap_minutes = positive_interval_seconds / 60,
    gap_hours = positive_interval_seconds / 3600,
    expected_records_missed = pmax(
      0,
      floor(positive_interval_seconds / expected_monitoring_interval_seconds) - 1
    ),
    elapsed_hours_since_window_start = as.numeric(difftime(dateTime, deployment_start_datetime, units = "hours")),
    preceding_24h_period = pmax(
      1L,
      floor(elapsed_hours_since_window_start / monitoring_period_hours) + 1L
    ),
    gap_category = case_when(
      positive_interval_seconds <= 5 * 60 ~ "gt_60sec_to_5min",
      positive_interval_seconds <= 30 * 60 ~ "gt_5min_to_30min",
      positive_interval_seconds <= 2 * 3600 ~ "gt_30min_to_2h",
      TRUE ~ "gt_2h"
    ),
    crosses_study_timepoint = FALSE,
    crosses_raw_source_file = FALSE,
    crosses_pm_monitor = FALSE,
    gap_scope = "within_monitoring_window",
    gap_interpretation = "This interval is within one monitoring_coverage_window_id; it is not a gap between study timepoints, source files, or PM monitors in the coverage audit."
  ) %>%
  select(
    monitoring_coverage_window_id,
    all_of(monitoring_window_keys),
    dateTime,
    next_datetime,
    pm25_ug_m3,
    next_pm25_ug_m3,
    positive_interval_seconds,
    gap_minutes,
    gap_hours,
    expected_records_missed,
    preceding_24h_period,
    gap_category,
    gap_scope,
    gap_interpretation,
    crosses_study_timepoint,
    crosses_raw_source_file,
    crosses_pm_monitor
  ) %>%
  arrange(desc(positive_interval_seconds), timepoint, study_arm_overall, hh_id, dateTime)

within_window_interval_denominators <- indoor_monitoring_timestamps %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_household_windows = n_distinct(monitoring_coverage_window_id),
    n_positive_intervals = sum(!is.na(positive_interval_seconds)),
    .groups = "drop"
  )

within_window_long_interval_counts <- long_interval_within_window_details %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_long_intervals_gt_60sec = n(),
    n_household_windows_with_long_interval = n_distinct(monitoring_coverage_window_id),
    median_long_interval_seconds = median_or_na(positive_interval_seconds),
    p95_long_interval_seconds = safe_quantile(positive_interval_seconds, 0.95),
    max_long_interval_seconds = max_or_na(positive_interval_seconds),
    total_expected_records_missed = sum(expected_records_missed, na.rm = TRUE),
    n_long_intervals_gt_60sec_to_5min = sum(gap_category == "gt_60sec_to_5min", na.rm = TRUE),
    n_long_intervals_gt_5min_to_30min = sum(gap_category == "gt_5min_to_30min", na.rm = TRUE),
    n_long_intervals_gt_30min_to_2h = sum(gap_category == "gt_30min_to_2h", na.rm = TRUE),
    n_long_intervals_gt_2h = sum(gap_category == "gt_2h", na.rm = TRUE),
    .groups = "drop"
  )

long_interval_within_window_summary_by_arm <- within_window_interval_denominators %>%
  left_join(within_window_long_interval_counts, by = c("timepoint", "study_arm_overall")) %>%
  mutate(
    across(
      c(
        n_long_intervals_gt_60sec,
        n_household_windows_with_long_interval,
        total_expected_records_missed,
        n_long_intervals_gt_60sec_to_5min,
        n_long_intervals_gt_5min_to_30min,
        n_long_intervals_gt_30min_to_2h,
        n_long_intervals_gt_2h
      ),
      ~ dplyr::coalesce(.x, 0)
    ),
    pct_positive_intervals_gt_60sec = ifelse(
      n_positive_intervals > 0,
      n_long_intervals_gt_60sec / n_positive_intervals * 100,
      NA_real_
    ),
    interval_scope = "within_monitoring_window",
    interval_interpretation = "These long intervals occur inside the same timepoint/source-file/PM-monitor monitoring window."
  ) %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  )

long_interval_within_window_summary_overall <- data.frame(
  timepoint = "all",
  study_arm_overall = "all",
  n_household_windows = n_distinct(indoor_monitoring_timestamps$monitoring_coverage_window_id),
  n_positive_intervals = sum(!is.na(indoor_monitoring_timestamps$positive_interval_seconds)),
  n_long_intervals_gt_60sec = nrow(long_interval_within_window_details),
  n_household_windows_with_long_interval = n_distinct(long_interval_within_window_details$monitoring_coverage_window_id),
  median_long_interval_seconds = median_or_na(long_interval_within_window_details$positive_interval_seconds),
  p95_long_interval_seconds = safe_quantile(long_interval_within_window_details$positive_interval_seconds, 0.95),
  max_long_interval_seconds = max_or_na(long_interval_within_window_details$positive_interval_seconds),
  total_expected_records_missed = sum(long_interval_within_window_details$expected_records_missed, na.rm = TRUE),
  n_long_intervals_gt_60sec_to_5min = sum(long_interval_within_window_details$gap_category == "gt_60sec_to_5min", na.rm = TRUE),
  n_long_intervals_gt_5min_to_30min = sum(long_interval_within_window_details$gap_category == "gt_5min_to_30min", na.rm = TRUE),
  n_long_intervals_gt_30min_to_2h = sum(long_interval_within_window_details$gap_category == "gt_30min_to_2h", na.rm = TRUE),
  n_long_intervals_gt_2h = sum(long_interval_within_window_details$gap_category == "gt_2h", na.rm = TRUE),
  stringsAsFactors = FALSE
) %>%
  mutate(
    pct_positive_intervals_gt_60sec = ifelse(
      n_positive_intervals > 0,
      n_long_intervals_gt_60sec / n_positive_intervals * 100,
      NA_real_
    ),
    interval_scope = "within_monitoring_window",
    interval_interpretation = "These long intervals occur inside the same timepoint/source-file/PM-monitor monitoring window."
  )

long_interval_within_window_summary <- bind_rows(
  long_interval_within_window_summary_overall,
  long_interval_within_window_summary_by_arm
)

household_naive_intervals <- indoor_monitoring_valid %>%
  mutate(
    household_monitoring_identity = dplyr::coalesce(
      ifelse(!is.na(fcn_id) & nzchar(fcn_id), fcn_id, NA_character_),
      ifelse(!is.na(hh_id) & nzchar(hh_id), hh_id, NA_character_)
    ),
    timepoint_chr = as.character(timepoint),
    study_arm_overall_chr = as.character(study_arm_overall),
    raw_source_file_chr = as.character(raw_source_file),
    PM_monitor_chr = as.character(PM_monitor)
  ) %>%
  filter(!is.na(household_monitoring_identity)) %>%
  distinct(
    household_monitoring_identity,
    dateTime,
    timepoint_chr,
    study_arm_overall_chr,
    hh_id,
    fcn_id,
    raw_source_file_chr,
    PM_monitor_chr,
    .keep_all = TRUE
  ) %>%
  arrange(household_monitoring_identity, dateTime) %>%
  group_by(household_monitoring_identity) %>%
  mutate(
    next_datetime_household = lead(dateTime),
    next_timepoint = lead(timepoint_chr),
    next_study_arm_overall = lead(study_arm_overall_chr),
    next_hh_id = lead(hh_id),
    next_fcn_id = lead(fcn_id),
    next_raw_source_file = lead(raw_source_file_chr),
    next_PM_monitor = lead(PM_monitor_chr),
    naive_household_interval_seconds = as.numeric(difftime(next_datetime_household, dateTime, units = "secs"))
  ) %>%
  ungroup()

naive_household_long_interval_details <- household_naive_intervals %>%
  filter(
    !is.na(naive_household_interval_seconds),
    is.finite(naive_household_interval_seconds),
    naive_household_interval_seconds > long_monitoring_interval_threshold_seconds
  ) %>%
  mutate(
    gap_minutes = naive_household_interval_seconds / 60,
    gap_hours = naive_household_interval_seconds / 3600,
    gap_days = naive_household_interval_seconds / 86400,
    crosses_study_timepoint = !is.na(next_timepoint) & timepoint_chr != next_timepoint,
    crosses_study_arm = !is.na(next_study_arm_overall) & study_arm_overall_chr != next_study_arm_overall,
    crosses_hh_id = !is.na(next_hh_id) & hh_id != next_hh_id,
    crosses_fcn_id = !is.na(next_fcn_id) & fcn_id != next_fcn_id,
    crosses_raw_source_file = !is.na(next_raw_source_file) & raw_source_file_chr != next_raw_source_file,
    crosses_pm_monitor = !is.na(next_PM_monitor) & PM_monitor_chr != next_PM_monitor,
    gap_source_class = case_when(
      crosses_study_timepoint ~ "between_study_timepoints",
      crosses_raw_source_file | crosses_pm_monitor ~ "between_source_files_or_monitors_same_timepoint",
      TRUE ~ "within_same_household_sequence"
    ),
    answers_timepoint_gap_hypothesis = ifelse(
      crosses_study_timepoint,
      "yes_this_naive_household_gap_crosses_study_timepoints",
      "no_this_naive_household_gap_does_not_cross_study_timepoints"
    )
  ) %>%
  select(
    household_monitoring_identity,
    hh_id,
    fcn_id,
    timepoint = timepoint_chr,
    study_arm_overall = study_arm_overall_chr,
    raw_source_file = raw_source_file_chr,
    PM_monitor = PM_monitor_chr,
    dateTime,
    next_datetime_household,
    next_timepoint,
    next_study_arm_overall,
    next_hh_id,
    next_fcn_id,
    next_raw_source_file,
    next_PM_monitor,
    naive_household_interval_seconds,
    gap_minutes,
    gap_hours,
    gap_days,
    crosses_study_timepoint,
    crosses_study_arm,
    crosses_hh_id,
    crosses_fcn_id,
    crosses_raw_source_file,
    crosses_pm_monitor,
    gap_source_class,
    answers_timepoint_gap_hypothesis
  ) %>%
  arrange(desc(naive_household_interval_seconds), household_monitoring_identity, dateTime)

naive_household_interval_denominators <- household_naive_intervals %>%
  group_by(timepoint_chr, study_arm_overall_chr) %>%
  summarise(
    n_naive_household_positive_intervals = sum(
      !is.na(naive_household_interval_seconds) &
        is.finite(naive_household_interval_seconds) &
        naive_household_interval_seconds > 0
    ),
    .groups = "drop"
  )

naive_household_long_interval_counts <- naive_household_long_interval_details %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_naive_household_long_intervals_gt_60sec = n(),
    n_cross_study_timepoint = sum(crosses_study_timepoint, na.rm = TRUE),
    n_cross_raw_source_file = sum(crosses_raw_source_file, na.rm = TRUE),
    n_cross_pm_monitor = sum(crosses_pm_monitor, na.rm = TRUE),
    n_within_same_timepoint_source_file_monitor = sum(
      !crosses_study_timepoint & !crosses_raw_source_file & !crosses_pm_monitor,
      na.rm = TRUE
    ),
    median_naive_long_interval_seconds = median_or_na(naive_household_interval_seconds),
    max_naive_long_interval_seconds = max_or_na(naive_household_interval_seconds),
    .groups = "drop"
  )

naive_household_long_interval_summary_by_arm <- naive_household_interval_denominators %>%
  left_join(
    naive_household_long_interval_counts,
    by = c("timepoint_chr" = "timepoint", "study_arm_overall_chr" = "study_arm_overall")
  ) %>%
  mutate(
    across(
      c(
        n_naive_household_long_intervals_gt_60sec,
        n_cross_study_timepoint,
        n_cross_raw_source_file,
        n_cross_pm_monitor,
        n_within_same_timepoint_source_file_monitor
      ),
      ~ dplyr::coalesce(.x, 0)
    ),
    pct_naive_long_intervals_cross_study_timepoint = ifelse(
      n_naive_household_long_intervals_gt_60sec > 0,
      n_cross_study_timepoint / n_naive_household_long_intervals_gt_60sec * 100,
      NA_real_
    ),
    timepoint = timepoint_chr,
    study_arm_overall = study_arm_overall_chr,
    interval_scope = "naive_household_sequence",
    interval_interpretation = "This diagnostic intentionally sorts all valid rows by household identity only; it tests whether cross-timepoint deployment gaps would appear if deployment boundaries were ignored."
  ) %>%
  select(-timepoint_chr, -study_arm_overall_chr)

naive_household_long_interval_summary_overall <- data.frame(
  timepoint = "all",
  study_arm_overall = "all",
  n_naive_household_positive_intervals = sum(
    !is.na(household_naive_intervals$naive_household_interval_seconds) &
      is.finite(household_naive_intervals$naive_household_interval_seconds) &
      household_naive_intervals$naive_household_interval_seconds > 0
  ),
  n_naive_household_long_intervals_gt_60sec = nrow(naive_household_long_interval_details),
  n_cross_study_timepoint = sum(naive_household_long_interval_details$crosses_study_timepoint, na.rm = TRUE),
  n_cross_raw_source_file = sum(naive_household_long_interval_details$crosses_raw_source_file, na.rm = TRUE),
  n_cross_pm_monitor = sum(naive_household_long_interval_details$crosses_pm_monitor, na.rm = TRUE),
  n_within_same_timepoint_source_file_monitor = sum(
    !naive_household_long_interval_details$crosses_study_timepoint &
      !naive_household_long_interval_details$crosses_raw_source_file &
      !naive_household_long_interval_details$crosses_pm_monitor,
    na.rm = TRUE
  ),
  median_naive_long_interval_seconds = median_or_na(naive_household_long_interval_details$naive_household_interval_seconds),
  max_naive_long_interval_seconds = max_or_na(naive_household_long_interval_details$naive_household_interval_seconds),
  stringsAsFactors = FALSE
) %>%
  mutate(
    pct_naive_long_intervals_cross_study_timepoint = ifelse(
      n_naive_household_long_intervals_gt_60sec > 0,
      n_cross_study_timepoint / n_naive_household_long_intervals_gt_60sec * 100,
      NA_real_
    ),
    interval_scope = "naive_household_sequence",
    interval_interpretation = "This diagnostic intentionally sorts all valid rows by household identity only; it tests whether cross-timepoint deployment gaps would appear if deployment boundaries were ignored."
  )

naive_household_long_interval_summary <- bind_rows(
  naive_household_long_interval_summary_overall,
  naive_household_long_interval_summary_by_arm
)

long_interval_source_assessment <- data.frame(
  question = c(
    "Are intervals longer than 60 seconds present in the actual coverage audit?",
    "Can the actual coverage audit create long intervals by joining different study timepoints for the same household?",
    "Would a naive household-only interval calculation create cross-timepoint gaps?"
  ),
  answer = c(
    ifelse(nrow(long_interval_within_window_details) > 0, "yes", "no"),
    "no; monitoring_coverage_window_id includes timepoint, raw_source_file, and PM_monitor before intervals are calculated",
    ifelse(
      sum(naive_household_long_interval_details$crosses_study_timepoint, na.rm = TRUE) > 0,
      "yes; see naive household-only diagnostic tables",
      "no cross-timepoint gaps found even under household-only sorting"
    )
  ),
  value = c(
    as.character(nrow(long_interval_within_window_details)),
    "0 by construction",
    as.character(sum(naive_household_long_interval_details$crosses_study_timepoint, na.rm = TRUE))
  ),
  stringsAsFactors = FALSE
)

readr::write_csv(
  long_interval_within_window_summary,
  file.path(table_dir, "table_pm25_long_interval_within_window_summary.csv"),
  na = ""
)
readr::write_csv(
  naive_household_long_interval_summary,
  file.path(table_dir, "table_pm25_naive_household_long_interval_source_summary.csv"),
  na = ""
)
readr::write_csv(
  long_interval_source_assessment,
  file.path(table_dir, "table_pm25_long_interval_source_assessment.csv"),
  na = ""
)
readr::write_csv(
  long_interval_within_window_details,
  file.path(restricted_table_dir, "table_pm25_long_interval_within_window_details_internal.csv"),
  na = ""
)
readr::write_csv(
  naive_household_long_interval_details,
  file.path(restricted_table_dir, "table_pm25_naive_household_long_interval_details_internal.csv"),
  na = ""
)

monitoring_interval_by_household_hour <- indoor_monitoring_timestamps %>%
  group_by(monitoring_coverage_window_id, monitor_hour) %>%
  summarise(
    first_timestamp = min(dateTime, na.rm = TRUE),
    last_timestamp = max(dateTime, na.rm = TRUE),
    n_valid_timestamps = n(),
    n_positive_intervals = sum(!is.na(positive_interval_seconds)),
    modal_interval_seconds = mode_number(positive_interval_seconds),
    median_interval_seconds = median_or_na(positive_interval_seconds),
    min_interval_seconds = min_or_na(positive_interval_seconds),
    max_interval_seconds = max_or_na(positive_interval_seconds),
    n_distinct_positive_intervals = n_distinct(positive_interval_seconds[!is.na(positive_interval_seconds)]),
    pct_positive_intervals_at_hour_mode = pct_at_mode(positive_interval_seconds),
    .groups = "drop"
  ) %>%
  mutate(
    all_positive_intervals_same_within_hour = ifelse(
      n_positive_intervals > 0,
      n_distinct_positive_intervals == 1L,
      NA
    )
  ) %>%
  left_join(
    monitoring_window_index %>% select(monitoring_coverage_window_id, all_of(monitoring_window_keys)),
    by = "monitoring_coverage_window_id"
  ) %>%
  select(monitoring_coverage_window_id, all_of(monitoring_window_keys), monitor_hour, everything()) %>%
  arrange(timepoint, study_arm_overall, hh_id, monitor_hour)

interval_rows_for_summary <- indoor_monitoring_timestamps

interval_summary_by_arm <- interval_rows_for_summary %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_household_windows = n_distinct(monitoring_coverage_window_id),
    n_household_hour_groups = n_distinct(paste(monitoring_coverage_window_id, monitor_hour)),
    n_positive_intervals = sum(!is.na(positive_interval_seconds)),
    modal_interval_seconds = mode_number(positive_interval_seconds),
    median_interval_seconds = median_or_na(positive_interval_seconds),
    min_interval_seconds = min_or_na(positive_interval_seconds),
    max_interval_seconds = max_or_na(positive_interval_seconds),
    n_distinct_positive_intervals = n_distinct(positive_interval_seconds[!is.na(positive_interval_seconds)]),
    pct_positive_intervals_at_modal = pct_at_mode(positive_interval_seconds),
    all_positive_intervals_same = {
      x <- positive_interval_seconds[!is.na(positive_interval_seconds)]
      if (length(x) == 0) NA else length(unique(x)) == 1L
    },
    .groups = "drop"
  )

hour_interval_summary_by_arm <- monitoring_interval_by_household_hour %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_household_hour_groups_with_positive_intervals = sum(n_positive_intervals > 0, na.rm = TRUE),
    n_household_hour_groups_constant_interval = sum(all_positive_intervals_same_within_hour %in% TRUE, na.rm = TRUE),
    pct_household_hour_groups_constant_interval = ifelse(
      n_household_hour_groups_with_positive_intervals > 0,
      n_household_hour_groups_constant_interval / n_household_hour_groups_with_positive_intervals * 100,
      NA_real_
    ),
    .groups = "drop"
  )

interval_summary_by_arm <- interval_summary_by_arm %>%
  left_join(hour_interval_summary_by_arm, by = c("timepoint", "study_arm_overall"))

interval_summary_overall <- data.frame(
  timepoint = "all",
  study_arm_overall = "all",
  n_household_windows = n_distinct(interval_rows_for_summary$monitoring_coverage_window_id),
  n_household_hour_groups = n_distinct(paste(interval_rows_for_summary$monitoring_coverage_window_id, interval_rows_for_summary$monitor_hour)),
  n_positive_intervals = sum(!is.na(interval_rows_for_summary$positive_interval_seconds)),
  modal_interval_seconds = global_modal_interval_seconds,
  median_interval_seconds = median_or_na(interval_rows_for_summary$positive_interval_seconds),
  min_interval_seconds = min_or_na(interval_rows_for_summary$positive_interval_seconds),
  max_interval_seconds = max_or_na(interval_rows_for_summary$positive_interval_seconds),
  n_distinct_positive_intervals = n_distinct(interval_rows_for_summary$positive_interval_seconds[!is.na(interval_rows_for_summary$positive_interval_seconds)]),
  pct_positive_intervals_at_modal = pct_at_mode(interval_rows_for_summary$positive_interval_seconds),
  all_positive_intervals_same = {
    x <- interval_rows_for_summary$positive_interval_seconds[!is.na(interval_rows_for_summary$positive_interval_seconds)]
    if (length(x) == 0) NA else length(unique(x)) == 1L
  },
  n_household_hour_groups_with_positive_intervals = sum(monitoring_interval_by_household_hour$n_positive_intervals > 0, na.rm = TRUE),
  n_household_hour_groups_constant_interval = sum(monitoring_interval_by_household_hour$all_positive_intervals_same_within_hour %in% TRUE, na.rm = TRUE),
  pct_household_hour_groups_constant_interval = ifelse(
    sum(monitoring_interval_by_household_hour$n_positive_intervals > 0, na.rm = TRUE) > 0,
    sum(monitoring_interval_by_household_hour$all_positive_intervals_same_within_hour %in% TRUE, na.rm = TRUE) /
      sum(monitoring_interval_by_household_hour$n_positive_intervals > 0, na.rm = TRUE) * 100,
    NA_real_
  ),
  stringsAsFactors = FALSE
)

monitoring_interval_summary <- bind_rows(
  interval_summary_overall,
  interval_summary_by_arm %>% mutate(timepoint = as.character(timepoint), study_arm_overall = as.character(study_arm_overall))
) %>%
  mutate(
    interval_question = "Are positive inter-record data collection intervals the same for all hours in all households?",
    interval_answer = ifelse(
      all_positive_intervals_same,
      "yes_all_positive_intervals_identical",
      "no_positive_intervals_vary"
    )
  )

readr::write_csv(
  monitoring_interval_summary,
  file.path(table_dir, "table_pm25_monitoring_interval_summary.csv"),
  na = ""
)
readr::write_csv(
  monitoring_interval_by_household_hour,
  file.path(restricted_table_dir, "table_pm25_monitoring_interval_by_household_hour_internal.csv"),
  na = ""
)

monitoring_period_coverage <- bind_rows(lapply(seq_len(nrow(monitoring_window_index)), function(i) {
  window_id <- monitoring_window_index$monitoring_coverage_window_id[[i]]
  timestamp_rows <- indoor_monitoring_timestamps %>%
    filter(monitoring_coverage_window_id == window_id)
  out <- bind_rows(lapply(seq_len(monitoring_period_count), function(period_number) {
    summarize_monitoring_period_coverage(
      timestamp_rows = timestamp_rows,
      start_datetime = monitoring_window_index$start_datetime[[i]],
      usual_interval_seconds = monitoring_window_index$usual_interval_seconds[[i]],
      period_number = period_number
    )
  }))
  out$monitoring_coverage_window_id <- window_id
  out[, c("monitoring_coverage_window_id", setdiff(names(out), "monitoring_coverage_window_id"))]
}))

monitoring_period_1 <- monitoring_period_coverage %>%
  filter(period_number == 1L) %>%
  transmute(
    monitoring_coverage_window_id,
    period_1_start_datetime = period_start_datetime,
    period_1_end_datetime = period_end_datetime,
    n_valid_timestamps_24h_1 = n_valid_timestamps,
    valid_monitoring_hours_24h_1 = valid_monitoring_hours,
    coverage_prop_24h_1 = coverage_prop,
    coverage_percent_24h_1 = coverage_percent,
    has_valid_75pct_24h_1 = has_valid_75pct_coverage
  )

monitoring_period_2 <- monitoring_period_coverage %>%
  filter(period_number == 2L) %>%
  transmute(
    monitoring_coverage_window_id,
    period_2_start_datetime = period_start_datetime,
    period_2_end_datetime = period_end_datetime,
    n_valid_timestamps_24h_2 = n_valid_timestamps,
    valid_monitoring_hours_24h_2 = valid_monitoring_hours,
    coverage_prop_24h_2 = coverage_prop,
    coverage_percent_24h_2 = coverage_percent,
    has_valid_75pct_24h_2 = has_valid_75pct_coverage
  )

household_monitoring_coverage <- monitoring_window_index %>%
  left_join(monitoring_period_1, by = "monitoring_coverage_window_id") %>%
  left_join(monitoring_period_2, by = "monitoring_coverage_window_id") %>%
  mutate(
    valid_monitoring_hours_full_48h = pmin(
      monitoring_total_hours,
      dplyr::coalesce(valid_monitoring_hours_24h_1, 0) + dplyr::coalesce(valid_monitoring_hours_24h_2, 0)
    ),
    coverage_prop_full_48h = valid_monitoring_hours_full_48h / monitoring_total_hours,
    coverage_percent_full_48h = coverage_prop_full_48h * 100,
    average_coverage_percent_24h_periods = (
      dplyr::coalesce(coverage_percent_24h_1, 0) + dplyr::coalesce(coverage_percent_24h_2, 0)
    ) / monitoring_period_count,
    has_valid_75pct_24h_1 = dplyr::coalesce(has_valid_75pct_24h_1, FALSE),
    has_valid_75pct_24h_2 = dplyr::coalesce(has_valid_75pct_24h_2, FALSE),
    n_valid_24h_periods_75pct = as.integer(has_valid_75pct_24h_1) + as.integer(has_valid_75pct_24h_2),
    valid_monitoring_definition = paste0(
      "Valid monitoring uses positive finite indoor PM2.5 rows. Coverage is interval-overlap time using each deployment's modal positive interval, ",
      "with gaps capped at that usual interval. A 24-hour period is valid at >= ",
      valid_monitoring_coverage_threshold * 100,
      "% coverage."
    )
  ) %>%
  arrange(timepoint, study_arm_overall, start_datetime, hh_id)

monitoring_coverage_counts <- household_monitoring_coverage %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_households = n_distinct(hh_id),
    n_household_windows = n(),
    n_monitor_files = n_distinct(raw_source_file),
    n_households_valid_first_24h_75pct = n_distinct(hh_id[has_valid_75pct_24h_1]),
    n_households_valid_second_24h_75pct = n_distinct(hh_id[has_valid_75pct_24h_2]),
    n_household_windows_valid_first_24h_75pct = sum(has_valid_75pct_24h_1, na.rm = TRUE),
    n_household_windows_valid_second_24h_75pct = sum(has_valid_75pct_24h_2, na.rm = TRUE),
    n_households_with_0_valid_24h_periods_75pct = n_distinct(hh_id[n_valid_24h_periods_75pct == 0L]),
    n_households_with_1_valid_24h_period_75pct = n_distinct(hh_id[n_valid_24h_periods_75pct == 1L]),
    n_households_with_2_valid_24h_periods_75pct = n_distinct(hh_id[n_valid_24h_periods_75pct == 2L]),
    n_household_windows_with_0_valid_24h_periods_75pct = sum(n_valid_24h_periods_75pct == 0L, na.rm = TRUE),
    n_household_windows_with_1_valid_24h_period_75pct = sum(n_valid_24h_periods_75pct == 1L, na.rm = TRUE),
    n_household_windows_with_2_valid_24h_periods_75pct = sum(n_valid_24h_periods_75pct == 2L, na.rm = TRUE),
    median_coverage_percent_24h_1 = median_or_na(coverage_percent_24h_1),
    median_coverage_percent_24h_2 = median_or_na(coverage_percent_24h_2),
    mean_coverage_percent_24h_1 = mean_or_na(coverage_percent_24h_1),
    mean_coverage_percent_24h_2 = mean_or_na(coverage_percent_24h_2),
    median_average_coverage_percent_24h_periods = median_or_na(average_coverage_percent_24h_periods),
    median_coverage_percent_full_48h = median_or_na(coverage_percent_full_48h),
    valid_period_coverage_threshold_percent = valid_monitoring_coverage_threshold * 100,
    count_unit = "one row per household monitoring file/window; household counts use distinct hh_id within arm/timepoint",
    .groups = "drop"
  ) %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  )

readr::write_csv(
  household_monitoring_coverage,
  file.path(restricted_table_dir, "table_pm25_household_monitoring_coverage_internal.csv"),
  na = ""
)
write_scaffolded_csv(
  monitoring_coverage_counts,
  file.path(table_dir, "table_pm25_monitoring_coverage_counts.csv")
)

message("Summarizing indoor PM2.5 to household monitoring windows")
indoor_windows <- indoor %>%
  filter(
    timepoint %in% timepoint_levels,
    study_arm_overall %in% arm_levels,
    !is.na(hh_id),
    !is.na(dateTime),
    !is.na(pm25_ug_m3),
    is.finite(pm25_ug_m3),
    pm25_ug_m3 > 0
  ) %>%
  group_by(timepoint, study_arm_overall, hh_id, fcn_id, hh_id_note, raw_source_file, PM_monitor) %>%
  summarise(
    start_datetime = min(dateTime, na.rm = TRUE),
    end_datetime = max(dateTime, na.rm = TRUE),
    midpoint_datetime = mean_datetime(dateTime),
    n_obs_indoor = n(),
    n_hours_indoor = n_distinct(lubridate::floor_date(dateTime, unit = "hour")),
    duration_hours = as.numeric(difftime(max(dateTime, na.rm = TRUE), min(dateTime, na.rm = TRUE), units = "hours")),
    indoor_mean_pm = mean(pm25_ug_m3, na.rm = TRUE),
    indoor_gmean_pm = geo_mean(pm25_ug_m3),
    indoor_median_pm = median(pm25_ug_m3, na.rm = TRUE),
    indoor_p05_pm = safe_quantile(pm25_ug_m3, 0.05),
    indoor_p25_pm = safe_quantile(pm25_ug_m3, 0.25),
    indoor_p75_pm = safe_quantile(pm25_ug_m3, 0.75),
    indoor_p95_pm = safe_quantile(pm25_ug_m3, 0.95),
    indoor_p99_pm = safe_quantile(pm25_ug_m3, 0.99),
    indoor_max_pm = max(pm25_ug_m3, na.rm = TRUE),
    pct_obs_gt_35 = mean(pm25_ug_m3 > 35, na.rm = TRUE) * 100,
    pct_obs_gt_75 = mean(pm25_ug_m3 > 75, na.rm = TRUE) * 100,
    pct_obs_gt_150 = mean(pm25_ug_m3 > 150, na.rm = TRUE) * 100,
    pct_obs_gt_400 = mean(pm25_ug_m3 > 400, na.rm = TRUE) * 100,
    pct_obs_gt_1000 = mean(pm25_ug_m3 >= 1000, na.rm = TRUE) * 100,
    pct_obs_gt_5000 = mean(pm25_ug_m3 >= 5000, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    household_window_id = sprintf("pm_window_%04d", row_number()),
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = arm_levels)
  ) %>%
  arrange(timepoint, study_arm_overall, start_datetime, household_window_id)

ambient_clean <- ambient %>%
  filter(
    !is.na(dateTime),
    !is.na(pm25_ug_m3),
    is.finite(pm25_ug_m3),
    pm25_ug_m3 > 0
  ) %>%
  mutate(
    ambient_hour = lubridate::floor_date(dateTime, unit = "hour")
  )

message("Creating HAPIN-style PM2.5 exposure graphics and tables")

hapin_metric_metadata <- data.frame(
  metric_name = c("raw_indoor_pm25", "ambient_adjusted_indoor_excess_pm25"),
  metric_label = c(
    "Raw indoor PM2.5",
    sprintf(
      "Ambient-adjusted indoor-excess PM2.5: indoor minus %.2f x concurrent ambient",
      default_material_infiltration_factor
    )
  ),
  metric_scale = c("log_positive", "linear_zero"),
  stringsAsFactors = FALSE
)

hapin_identifier_cols <- c(
  "hh_id", "fcn_id", "hh_id_note", "raw_source_file", "PM_monitor",
  "monitoring_coverage_window_id", "start_datetime", "end_datetime",
  "period_start_datetime", "period_end_datetime"
)

hapin_deidentify <- function(data) {
  data %>% select(-any_of(hapin_identifier_cols))
}

hapin_weighted_mean <- function(value, weight) {
  value <- as_number(value)
  weight <- as_number(weight)
  ok <- !is.na(value) & is.finite(value) & !is.na(weight) & is.finite(weight) & weight > 0
  if (!any(ok)) return(NA_real_)
  sum(value[ok] * weight[ok]) / sum(weight[ok])
}

hapin_safe_metric_ratio <- function(numerator, denominator) {
  ifelse(
    !is.na(numerator) & is.finite(numerator) & numerator > 0 &
      !is.na(denominator) & is.finite(denominator) & denominator > 0,
    numerator / denominator,
    NA_real_
  )
}

hapin_window_id_lookup <- monitoring_window_index %>%
  transmute(
    monitoring_coverage_window_id,
    hapin_window_id = sprintf("hapin_window_%04d", row_number())
  )

hapin_period_index <- monitoring_period_coverage %>%
  left_join(
    monitoring_window_index %>%
      select(
        monitoring_coverage_window_id,
        all_of(monitoring_window_keys),
        start_datetime,
        end_datetime,
        usual_interval_seconds
      ),
    by = "monitoring_coverage_window_id"
  ) %>%
  left_join(hapin_window_id_lookup, by = "monitoring_coverage_window_id") %>%
  mutate(
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = arm_levels),
    period_label = factor(
      paste0("Day ", period_number),
      levels = paste0("Day ", seq_len(monitoring_period_count))
    )
  )

hapin_period_pm_raw <- indoor_monitoring_timestamps %>%
  select(monitoring_coverage_window_id, dateTime, pm25_ug_m3) %>%
  inner_join(
    hapin_period_index %>%
      select(
        monitoring_coverage_window_id,
        period_number,
        period_start_datetime,
        period_end_datetime
      ),
    by = "monitoring_coverage_window_id",
    relationship = "many-to-many"
  ) %>%
  filter(dateTime >= period_start_datetime, dateTime < period_end_datetime) %>%
  group_by(monitoring_coverage_window_id, period_number) %>%
  summarise(
    n_pm_rows_24h = n(),
    indoor_mean_pm25_24h = mean(pm25_ug_m3, na.rm = TRUE),
    indoor_gmean_pm25_24h = geo_mean(pm25_ug_m3),
    indoor_median_pm25_24h = median(pm25_ug_m3, na.rm = TRUE),
    indoor_p10_pm25_24h = safe_quantile(pm25_ug_m3, 0.10),
    indoor_p25_pm25_24h = safe_quantile(pm25_ug_m3, 0.25),
    indoor_p75_pm25_24h = safe_quantile(pm25_ug_m3, 0.75),
    indoor_p90_pm25_24h = safe_quantile(pm25_ug_m3, 0.90),
    indoor_p95_pm25_24h = safe_quantile(pm25_ug_m3, 0.95),
    indoor_max_pm25_24h = max(pm25_ug_m3, na.rm = TRUE),
    pct_minute_rows_gt_15 = mean(pm25_ug_m3 > 15, na.rm = TRUE) * 100,
    pct_minute_rows_gt_35 = mean(pm25_ug_m3 > 35, na.rm = TRUE) * 100,
    pct_minute_rows_gt_75 = mean(pm25_ug_m3 > 75, na.rm = TRUE) * 100,
    pct_minute_rows_gt_150 = mean(pm25_ug_m3 > 150, na.rm = TRUE) * 100,
    pct_minute_rows_gt_400 = mean(pm25_ug_m3 > 400, na.rm = TRUE) * 100,
    pct_minute_rows_gt_1000 = mean(pm25_ug_m3 > 1000, na.rm = TRUE) * 100,
    .groups = "drop"
  )

hapin_summarize_period_ambient <- function(i) {
  period_rows <- ambient_clean %>%
    filter(
      dateTime >= hapin_period_index$period_start_datetime[[i]],
      dateTime < hapin_period_index$period_end_datetime[[i]]
    )

  data.frame(
    monitoring_coverage_window_id = hapin_period_index$monitoring_coverage_window_id[[i]],
    period_number = hapin_period_index$period_number[[i]],
    n_ambient_rows_24h = nrow(period_rows),
    n_ambient_hours_24h = if (nrow(period_rows) == 0) 0L else n_distinct(period_rows$ambient_hour),
    ambient_mean_pm25_24h = if (nrow(period_rows) == 0) NA_real_ else mean(period_rows$pm25_ug_m3, na.rm = TRUE),
    ambient_gmean_pm25_24h = if (nrow(period_rows) == 0) NA_real_ else geo_mean(period_rows$pm25_ug_m3),
    ambient_median_pm25_24h = if (nrow(period_rows) == 0) NA_real_ else median(period_rows$pm25_ug_m3, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

hapin_period_ambient <- bind_rows(lapply(seq_len(nrow(hapin_period_index)), hapin_summarize_period_ambient))

hapin_24h_period_wide <- hapin_period_index %>%
  left_join(hapin_period_pm_raw, by = c("monitoring_coverage_window_id", "period_number")) %>%
  left_join(hapin_period_ambient, by = c("monitoring_coverage_window_id", "period_number")) %>%
  mutate(
    n_pm_rows_24h = dplyr::coalesce(n_pm_rows_24h, 0L),
    n_ambient_rows_24h = dplyr::coalesce(n_ambient_rows_24h, 0L),
    n_ambient_hours_24h = dplyr::coalesce(n_ambient_hours_24h, 0L),
    ambient_adjusted_mean_pm25_24h = indoor_mean_pm25_24h -
      default_material_infiltration_factor * ambient_mean_pm25_24h,
    has_valid_raw_24h = has_valid_75pct_coverage &
      !is.na(indoor_mean_pm25_24h) & is.finite(indoor_mean_pm25_24h),
    has_valid_ambient_adjusted_24h = has_valid_75pct_coverage &
      n_ambient_rows_24h > 0 &
      !is.na(ambient_adjusted_mean_pm25_24h) &
      is.finite(ambient_adjusted_mean_pm25_24h)
  )

hapin_24h_period_summary_internal <- bind_rows(
  hapin_24h_period_wide %>%
    transmute(
      timepoint,
      study_arm_overall,
      hh_id,
      fcn_id,
      hh_id_note,
      raw_source_file,
      PM_monitor,
      monitoring_coverage_window_id,
      hapin_window_id,
      start_datetime,
      end_datetime,
      period_number,
      period_label,
      period_start_datetime,
      period_end_datetime,
      valid_monitoring_hours_24h = valid_monitoring_hours,
      coverage_prop_24h = coverage_prop,
      coverage_percent_24h = coverage_percent,
      has_valid_75pct_coverage,
      metric_name = "raw_indoor_pm25",
      metric_label = hapin_metric_metadata$metric_label[hapin_metric_metadata$metric_name == "raw_indoor_pm25"],
      metric_scale = "log_positive",
      metric_valid_24h = has_valid_raw_24h,
      metric_value_24h = indoor_mean_pm25_24h,
      n_pm_rows_24h,
      n_ambient_rows_24h,
      n_ambient_hours_24h,
      indoor_mean_pm25_24h,
      indoor_gmean_pm25_24h,
      indoor_median_pm25_24h,
      indoor_p10_pm25_24h,
      indoor_p25_pm25_24h,
      indoor_p75_pm25_24h,
      indoor_p90_pm25_24h,
      indoor_p95_pm25_24h,
      indoor_max_pm25_24h,
      ambient_mean_pm25_24h,
      ambient_gmean_pm25_24h,
      ambient_median_pm25_24h,
      ambient_adjusted_mean_pm25_24h,
      infiltration_factor_for_adjustment = default_material_infiltration_factor,
      pct_minute_rows_gt_15,
      pct_minute_rows_gt_35,
      pct_minute_rows_gt_75,
      pct_minute_rows_gt_150,
      pct_minute_rows_gt_400,
      pct_minute_rows_gt_1000
    ),
  hapin_24h_period_wide %>%
    transmute(
      timepoint,
      study_arm_overall,
      hh_id,
      fcn_id,
      hh_id_note,
      raw_source_file,
      PM_monitor,
      monitoring_coverage_window_id,
      hapin_window_id,
      start_datetime,
      end_datetime,
      period_number,
      period_label,
      period_start_datetime,
      period_end_datetime,
      valid_monitoring_hours_24h = valid_monitoring_hours,
      coverage_prop_24h = coverage_prop,
      coverage_percent_24h = coverage_percent,
      has_valid_75pct_coverage,
      metric_name = "ambient_adjusted_indoor_excess_pm25",
      metric_label = hapin_metric_metadata$metric_label[hapin_metric_metadata$metric_name == "ambient_adjusted_indoor_excess_pm25"],
      metric_scale = "linear_zero",
      metric_valid_24h = has_valid_ambient_adjusted_24h,
      metric_value_24h = ambient_adjusted_mean_pm25_24h,
      n_pm_rows_24h,
      n_ambient_rows_24h,
      n_ambient_hours_24h,
      indoor_mean_pm25_24h,
      indoor_gmean_pm25_24h,
      indoor_median_pm25_24h,
      indoor_p10_pm25_24h,
      indoor_p25_pm25_24h,
      indoor_p75_pm25_24h,
      indoor_p90_pm25_24h,
      indoor_p95_pm25_24h,
      indoor_max_pm25_24h,
      ambient_mean_pm25_24h,
      ambient_gmean_pm25_24h,
      ambient_median_pm25_24h,
      ambient_adjusted_mean_pm25_24h,
      infiltration_factor_for_adjustment = default_material_infiltration_factor,
      pct_minute_rows_gt_15 = NA_real_,
      pct_minute_rows_gt_35 = NA_real_,
      pct_minute_rows_gt_75 = NA_real_,
      pct_minute_rows_gt_150 = NA_real_,
      pct_minute_rows_gt_400 = NA_real_,
      pct_minute_rows_gt_1000 = NA_real_
    )
) %>%
  arrange(metric_name, timepoint, study_arm_overall, hapin_window_id, period_number)

readr::write_csv(
  hapin_24h_period_summary_internal,
  file.path(restricted_table_dir, "table_pm25_hapin_24h_period_summary_internal.csv"),
  na = ""
)
readr::write_csv(
  hapin_deidentify(hapin_24h_period_summary_internal),
  file.path(table_dir, "table_pm25_hapin_24h_period_summary_deidentified.csv"),
  na = ""
)

hapin_48h_household_summary_internal <- hapin_24h_period_summary_internal %>%
  filter(metric_valid_24h) %>%
  group_by(
    metric_name,
    metric_label,
    metric_scale,
    timepoint,
    study_arm_overall,
    hh_id,
    fcn_id,
    hh_id_note,
    raw_source_file,
    PM_monitor,
    monitoring_coverage_window_id,
    hapin_window_id,
    start_datetime,
    end_datetime
  ) %>%
  summarise(
    n_valid_24h_periods = n_distinct(period_number),
    valid_24h_periods = paste(sort(unique(period_number)), collapse = ";"),
    valid_monitoring_hours_48h = sum(valid_monitoring_hours_24h, na.rm = TRUE),
    coverage_prop_valid_periods_48h = min(1, valid_monitoring_hours_48h / monitoring_total_hours),
    coverage_percent_valid_periods_48h = coverage_prop_valid_periods_48h * 100,
    metric_value_48h_time_weighted = hapin_weighted_mean(metric_value_24h, valid_monitoring_hours_24h),
    mean_valid_24h_metric_value = mean(metric_value_24h, na.rm = TRUE),
    geometric_mean_valid_24h_metric_value = if (dplyr::first(metric_name) == "raw_indoor_pm25") geo_mean(metric_value_24h) else NA_real_,
    median_valid_24h_metric_value = median(metric_value_24h, na.rm = TRUE),
    p25_valid_24h_metric_value = safe_quantile(metric_value_24h, 0.25),
    p75_valid_24h_metric_value = safe_quantile(metric_value_24h, 0.75),
    p90_valid_24h_metric_value = safe_quantile(metric_value_24h, 0.90),
    p95_valid_24h_metric_value = safe_quantile(metric_value_24h, 0.95),
    min_valid_24h_metric_value = min(metric_value_24h, na.rm = TRUE),
    max_valid_24h_metric_value = max(metric_value_24h, na.rm = TRUE),
    pct_valid_24h_periods_ge_0 = mean(metric_value_24h >= 0, na.rm = TRUE) * 100,
    pct_valid_24h_periods_ge_15 = mean(metric_value_24h >= 15, na.rm = TRUE) * 100,
    pct_valid_24h_periods_ge_35 = mean(metric_value_24h >= 35, na.rm = TRUE) * 100,
    pct_valid_24h_periods_ge_75 = mean(metric_value_24h >= 75, na.rm = TRUE) * 100,
    pct_valid_24h_periods_ge_150 = mean(metric_value_24h >= 150, na.rm = TRUE) * 100,
    pct_valid_24h_periods_ge_400 = mean(metric_value_24h >= 400, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(metric_name, timepoint, study_arm_overall, hapin_window_id)

readr::write_csv(
  hapin_48h_household_summary_internal,
  file.path(restricted_table_dir, "table_pm25_hapin_48h_household_summary_internal.csv"),
  na = ""
)
readr::write_csv(
  hapin_deidentify(hapin_48h_household_summary_internal),
  file.path(table_dir, "table_pm25_hapin_48h_household_summary_deidentified.csv"),
  na = ""
)

hapin_distribution_summary_24h <- hapin_24h_period_summary_internal %>%
  filter(metric_valid_24h) %>%
  group_by(metric_name, metric_label, metric_scale, timepoint, study_arm_overall) %>%
  summarise(
    summary_level = "valid_24h_period",
    n_observations = n(),
    n_households = n_distinct(hh_id),
    n_household_windows = n_distinct(hapin_window_id),
    mean_pm25 = mean(metric_value_24h, na.rm = TRUE),
    geometric_mean_pm25 = if (dplyr::first(metric_name) == "raw_indoor_pm25") geo_mean(metric_value_24h) else NA_real_,
    median_pm25 = median(metric_value_24h, na.rm = TRUE),
    p10_pm25 = safe_quantile(metric_value_24h, 0.10),
    p25_pm25 = safe_quantile(metric_value_24h, 0.25),
    p75_pm25 = safe_quantile(metric_value_24h, 0.75),
    p90_pm25 = safe_quantile(metric_value_24h, 0.90),
    p95_pm25 = safe_quantile(metric_value_24h, 0.95),
    min_pm25 = min(metric_value_24h, na.rm = TRUE),
    max_pm25 = max(metric_value_24h, na.rm = TRUE),
    .groups = "drop"
  )

hapin_distribution_summary_48h <- hapin_48h_household_summary_internal %>%
  filter(!is.na(metric_value_48h_time_weighted), is.finite(metric_value_48h_time_weighted)) %>%
  group_by(metric_name, metric_label, metric_scale, timepoint, study_arm_overall) %>%
  summarise(
    summary_level = "household_48h_time_weighted_mean",
    n_observations = n(),
    n_households = n_distinct(hh_id),
    n_household_windows = n_distinct(hapin_window_id),
    mean_pm25 = mean(metric_value_48h_time_weighted, na.rm = TRUE),
    geometric_mean_pm25 = if (dplyr::first(metric_name) == "raw_indoor_pm25") geo_mean(metric_value_48h_time_weighted) else NA_real_,
    median_pm25 = median(metric_value_48h_time_weighted, na.rm = TRUE),
    p10_pm25 = safe_quantile(metric_value_48h_time_weighted, 0.10),
    p25_pm25 = safe_quantile(metric_value_48h_time_weighted, 0.25),
    p75_pm25 = safe_quantile(metric_value_48h_time_weighted, 0.75),
    p90_pm25 = safe_quantile(metric_value_48h_time_weighted, 0.90),
    p95_pm25 = safe_quantile(metric_value_48h_time_weighted, 0.95),
    min_pm25 = min(metric_value_48h_time_weighted, na.rm = TRUE),
    max_pm25 = max(metric_value_48h_time_weighted, na.rm = TRUE),
    .groups = "drop"
  )

hapin_distribution_summary <- bind_rows(
  hapin_distribution_summary_24h,
  hapin_distribution_summary_48h
) %>%
  arrange(summary_level, metric_name, timepoint, study_arm_overall)

readr::write_csv(
  hapin_distribution_summary,
  file.path(table_dir, "table_pm25_hapin_distribution_summary.csv"),
  na = ""
)

hapin_day1 <- hapin_24h_period_summary_internal %>%
  filter(period_number == 1L, metric_valid_24h) %>%
  transmute(
    metric_name,
    metric_label,
    metric_scale,
    timepoint,
    study_arm_overall,
    hh_id,
    fcn_id,
    hh_id_note,
    raw_source_file,
    PM_monitor,
    monitoring_coverage_window_id,
    hapin_window_id,
    metric_value_24h_day1 = metric_value_24h,
    coverage_percent_24h_day1 = coverage_percent_24h,
    valid_monitoring_hours_24h_day1 = valid_monitoring_hours_24h
  )

hapin_day2 <- hapin_24h_period_summary_internal %>%
  filter(period_number == 2L, metric_valid_24h) %>%
  transmute(
    metric_name,
    metric_label,
    metric_scale,
    timepoint,
    study_arm_overall,
    hh_id,
    fcn_id,
    hh_id_note,
    raw_source_file,
    PM_monitor,
    monitoring_coverage_window_id,
    hapin_window_id,
    metric_value_24h_day2 = metric_value_24h,
    coverage_percent_24h_day2 = coverage_percent_24h,
    valid_monitoring_hours_24h_day2 = valid_monitoring_hours_24h
  )

hapin_day1_day2_agreement_internal <- full_join(
  hapin_day1,
  hapin_day2,
  by = c(
    "metric_name", "metric_label", "metric_scale", "timepoint", "study_arm_overall",
    "hh_id", "fcn_id", "hh_id_note", "raw_source_file", "PM_monitor",
    "monitoring_coverage_window_id", "hapin_window_id"
  )
) %>%
  mutate(
    has_both_valid_24h_periods = !is.na(metric_value_24h_day1) & !is.na(metric_value_24h_day2),
    day2_minus_day1 = metric_value_24h_day2 - metric_value_24h_day1,
    day2_divided_by_day1 = hapin_safe_metric_ratio(metric_value_24h_day2, metric_value_24h_day1),
    absolute_day_difference = abs(day2_minus_day1)
  ) %>%
  arrange(metric_name, timepoint, study_arm_overall, hapin_window_id)

readr::write_csv(
  hapin_deidentify(hapin_day1_day2_agreement_internal),
  file.path(table_dir, "table_pm25_hapin_day1_day2_agreement.csv"),
  na = ""
)

hapin_valid_period_index <- hapin_period_index %>%
  filter(has_valid_75pct_coverage) %>%
  select(
    monitoring_coverage_window_id,
    hapin_window_id,
    all_of(monitoring_window_keys),
    period_number,
    period_label,
    period_start_datetime,
    period_end_datetime
  )

hapin_ambient_hourly <- ambient_clean %>%
  group_by(ambient_hour) %>%
  summarise(
    n_ambient_rows_hour = n(),
    ambient_mean_pm25_hour = mean(pm25_ug_m3, na.rm = TRUE),
    .groups = "drop"
  )

hapin_household_period_hour_wide <- indoor_monitoring_timestamps %>%
  select(monitoring_coverage_window_id, dateTime, pm25_ug_m3) %>%
  inner_join(
    hapin_valid_period_index,
    by = "monitoring_coverage_window_id",
    relationship = "many-to-many"
  ) %>%
  filter(dateTime >= period_start_datetime, dateTime < period_end_datetime) %>%
  mutate(
    monitor_hour = lubridate::floor_date(dateTime, unit = "hour"),
    hour_of_day = lubridate::hour(lubridate::with_tz(monitor_hour, tzone = hapin_hour_time_zone))
  ) %>%
  group_by(
    timepoint,
    study_arm_overall,
    hh_id,
    fcn_id,
    raw_source_file,
    PM_monitor,
    monitoring_coverage_window_id,
    hapin_window_id,
    period_number,
    period_label,
    monitor_hour,
    hour_of_day
  ) %>%
  summarise(
    n_pm_rows_hour = n(),
    indoor_mean_pm25_hour = mean(pm25_ug_m3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(hapin_ambient_hourly, by = c("monitor_hour" = "ambient_hour")) %>%
  mutate(
    n_ambient_rows_hour = dplyr::coalesce(n_ambient_rows_hour, 0L),
    ambient_adjusted_mean_pm25_hour = indoor_mean_pm25_hour -
      default_material_infiltration_factor * ambient_mean_pm25_hour
  )

hapin_household_period_hour_long <- bind_rows(
  hapin_household_period_hour_wide %>%
    transmute(
      metric_name = "raw_indoor_pm25",
      metric_label = hapin_metric_metadata$metric_label[hapin_metric_metadata$metric_name == "raw_indoor_pm25"],
      metric_scale = "log_positive",
      timepoint,
      study_arm_overall,
      hh_id,
      fcn_id,
      hapin_window_id,
      period_number,
      period_label,
      monitor_hour,
      hour_of_day,
      metric_value_hour = indoor_mean_pm25_hour,
      n_pm_rows_hour,
      n_ambient_rows_hour
    ),
  hapin_household_period_hour_wide %>%
    transmute(
      metric_name = "ambient_adjusted_indoor_excess_pm25",
      metric_label = hapin_metric_metadata$metric_label[hapin_metric_metadata$metric_name == "ambient_adjusted_indoor_excess_pm25"],
      metric_scale = "linear_zero",
      timepoint,
      study_arm_overall,
      hh_id,
      fcn_id,
      hapin_window_id,
      period_number,
      period_label,
      monitor_hour,
      hour_of_day,
      metric_value_hour = ambient_adjusted_mean_pm25_hour,
      n_pm_rows_hour,
      n_ambient_rows_hour
    )
) %>%
  filter(!is.na(metric_value_hour), is.finite(metric_value_hour))

hapin_hour_of_day_summary <- hapin_household_period_hour_long %>%
  group_by(metric_name, metric_label, metric_scale, timepoint, study_arm_overall, hour_of_day) %>%
  summarise(
    n_household_period_hours = n(),
    n_households = n_distinct(hh_id),
    n_household_windows = n_distinct(hapin_window_id),
    n_valid_24h_periods = n_distinct(paste(hapin_window_id, period_number)),
    mean_pm25 = mean(metric_value_hour, na.rm = TRUE),
    median_pm25 = median(metric_value_hour, na.rm = TRUE),
    p10_pm25 = safe_quantile(metric_value_hour, 0.10),
    p25_pm25 = safe_quantile(metric_value_hour, 0.25),
    p75_pm25 = safe_quantile(metric_value_hour, 0.75),
    p90_pm25 = safe_quantile(metric_value_hour, 0.90),
    .groups = "drop"
  ) %>%
  filter(n_households >= 3) %>%
  arrange(metric_name, timepoint, study_arm_overall, hour_of_day)

readr::write_csv(
  hapin_hour_of_day_summary,
  file.path(table_dir, "table_pm25_hapin_hour_of_day_summary.csv"),
  na = ""
)

hapin_tail_exceedance_summary <- bind_rows(lapply(hapin_pm25_exceedance_thresholds, function(threshold_i) {
  hapin_48h_household_summary_internal %>%
    filter(!is.na(metric_value_48h_time_weighted), is.finite(metric_value_48h_time_weighted)) %>%
    group_by(metric_name, metric_label, metric_scale, timepoint, study_arm_overall) %>%
    summarise(
      threshold_pm25 = threshold_i,
      n_household_windows = n(),
      n_household_windows_at_or_above_threshold = sum(metric_value_48h_time_weighted >= threshold_i, na.rm = TRUE),
      percent_household_windows_at_or_above_threshold =
        100 * n_household_windows_at_or_above_threshold / n_household_windows,
      summary_unit = "household_48h_time_weighted_mean",
      .groups = "drop"
    )
})) %>%
  arrange(metric_name, timepoint, study_arm_overall, threshold_pm25)

readr::write_csv(
  hapin_tail_exceedance_summary,
  file.path(table_dir, "table_pm25_hapin_tail_exceedance_summary.csv"),
  na = ""
)

hapin_generated_figures <- character()

hapin_metric_file_token <- function(metric_name) {
  gsub("[^a-z0-9]+", "_", tolower(metric_name))
}

hapin_save_plot <- function(plot, filename, width = 10, height = 6) {
  ggplot2::ggsave(
    file.path(figure_dir, filename),
    plot,
    width = width,
    height = height,
    dpi = 300
  )
  hapin_generated_figures <<- c(hapin_generated_figures, filename)
  invisible(filename)
}

hapin_arm_colors <- c(comparison = "#4E79A7", intervention = "#D55E00")

hapin_add_y_scale <- function(plot, metric_name) {
  if (identical(metric_name, "raw_indoor_pm25")) {
    plot +
      geom_hline(
        yintercept = hapin_pm25_reference_lines,
        color = "grey55",
        linetype = "dashed",
        linewidth = 0.25
      ) +
      scale_y_log10()
  } else {
    plot + geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35)
  }
}

for (metric_i in hapin_metric_metadata$metric_name) {
  metric_label_i <- hapin_metric_metadata$metric_label[hapin_metric_metadata$metric_name == metric_i]
  metric_scale_i <- hapin_metric_metadata$metric_scale[hapin_metric_metadata$metric_name == metric_i]
  metric_token_i <- hapin_metric_file_token(metric_i)
  log_positive_i <- identical(metric_scale_i, "log_positive")

  plot_48h_data <- hapin_48h_household_summary_internal %>%
    filter(
      metric_name == metric_i,
      !is.na(metric_value_48h_time_weighted),
      is.finite(metric_value_48h_time_weighted)
    )
  if (log_positive_i) {
    plot_48h_data <- plot_48h_data %>% filter(metric_value_48h_time_weighted > 0)
  }

  if (nrow(plot_48h_data) > 0) {
    if (identical(metric_i, "raw_indoor_pm25")) {
      plot_48h_indoor_data <- plot_48h_data %>%
        transmute(
          timepoint,
          study_arm_overall,
          exposure_group = case_when(
            study_arm_overall == "comparison" ~ "comparison_indoor",
            study_arm_overall == "intervention" ~ "intervention_indoor",
            TRUE ~ NA_character_
          ),
          metric_value_48h_time_weighted,
          source_unit = "household_indoor_48h_time_weighted_mean"
        )

      plot_48h_outdoor_data <- hapin_24h_period_wide %>%
        filter(
          has_valid_75pct_coverage,
          n_ambient_rows_24h > 0,
          !is.na(ambient_mean_pm25_24h),
          is.finite(ambient_mean_pm25_24h),
          ambient_mean_pm25_24h > 0
        ) %>%
        group_by(timepoint, hapin_window_id) %>%
        summarise(
          metric_value_48h_time_weighted = hapin_weighted_mean(
            ambient_mean_pm25_24h,
            valid_monitoring_hours
          ),
          source_unit = "concurrent_outdoor_48h_time_weighted_mean",
          .groups = "drop"
        ) %>%
        filter(
          !is.na(metric_value_48h_time_weighted),
          is.finite(metric_value_48h_time_weighted),
          metric_value_48h_time_weighted > 0
        ) %>%
        mutate(
          study_arm_overall = "outdoor_pm25",
          exposure_group = "outdoor_pm25"
        ) %>%
        select(
          timepoint,
          study_arm_overall,
          exposure_group,
          metric_value_48h_time_weighted,
          source_unit
        )

      plot_48h_display_data <- bind_rows(plot_48h_indoor_data, plot_48h_outdoor_data) %>%
        filter(!is.na(exposure_group)) %>%
        mutate(
          exposure_group = factor(
            exposure_group,
            levels = c("comparison_indoor", "intervention_indoor", "outdoor_pm25"),
            labels = c("Comparison indoor", "Intervention indoor", "Outdoor PM2.5")
          )
        )

      fig_48h <- ggplot(
        plot_48h_display_data,
        aes(
          x = exposure_group,
          y = metric_value_48h_time_weighted,
          color = exposure_group
        )
      ) +
        geom_hline(
          yintercept = c(25, 50, 75),
          color = "grey45",
          linetype = "dotted",
          linewidth = 0.35
        ) +
        geom_boxplot(width = 0.46, outlier.shape = NA, alpha = 0.12) +
        geom_jitter(width = 0.08, height = 0, alpha = 0.45, size = 1.6) +
        stat_summary(
          fun = mean,
          geom = "point",
          shape = 23,
          fill = "white",
          color = "black",
          size = 2.8
        ) +
        facet_wrap(~ timepoint, nrow = 1) +
        scale_y_log10(
          breaks = c(10, 25, 50, 75, 100, 250, 500, 1000),
          labels = function(x) format(x, trim = TRUE, scientific = FALSE)
        ) +
        scale_color_manual(
          values = c(
            "Comparison indoor" = "#4E79A7",
            "Intervention indoor" = "#D55E00",
            "Outdoor PM2.5" = "#3A3A3A"
          ),
          drop = FALSE
        ) +
        labs(
          x = NULL,
          y = "48-hour time-weighted PM2.5 (ug/m3, log scale)",
          color = NULL,
          title = paste0(metric_label_i, " and concurrent outdoor PM2.5"),
          subtitle = paste(
            "Points are household monitoring windows; boxplots show median and IQR;",
            "diamonds show arithmetic means; dotted lines mark 25, 50, and 75 ug/m3"
          )
        ) +
        theme_bw(base_size = 11) +
        theme(
          legend.position = "bottom",
          axis.text.x = element_text(angle = 20, hjust = 1),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey92", color = "grey75")
        )
    } else {
      fig_48h <- ggplot(
        plot_48h_data,
        aes(
          x = study_arm_overall,
          y = metric_value_48h_time_weighted,
          color = study_arm_overall
        )
      ) +
        geom_boxplot(width = 0.46, outlier.shape = NA, alpha = 0.12) +
        geom_jitter(width = 0.08, height = 0, alpha = 0.45, size = 1.6) +
        stat_summary(
          fun = mean,
          geom = "point",
          shape = 23,
          fill = "white",
          color = "black",
          size = 2.8
        ) +
        facet_wrap(~ timepoint, nrow = 1) +
        scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
        labs(
          x = NULL,
          y = "48-hour time-weighted PM2.5 (ug/m3)",
          color = "Study arm",
          title = paste0(metric_label_i, " by study arm and timepoint"),
          subtitle = "Points are household monitoring windows; boxplots show median and IQR; diamonds show arithmetic means"
        ) +
        theme_bw(base_size = 11) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey92", color = "grey75")
        )
      fig_48h <- hapin_add_y_scale(fig_48h, metric_i)
    }

    hapin_save_plot(
      fig_48h,
      paste0("fig_pm25_hapin_48h_household_distribution_", metric_token_i, ".png"),
      width = 10,
      height = 5.6
    )
  }

  plot_period_data <- hapin_24h_period_summary_internal %>%
    filter(metric_name == metric_i, metric_valid_24h)
  if (log_positive_i) {
    plot_period_data <- plot_period_data %>% filter(metric_value_24h > 0)
  }

  if (nrow(plot_period_data) > 0) {
    fig_period <- ggplot(
      plot_period_data,
      aes(
        x = period_label,
        y = metric_value_24h,
        color = study_arm_overall
      )
    ) +
      geom_boxplot(
        aes(group = interaction(period_label, study_arm_overall)),
        position = position_dodge(width = 0.72),
        width = 0.52,
        outlier.shape = NA,
        alpha = 0.12
      ) +
      geom_jitter(
        position = position_jitterdodge(jitter.width = 0.10, dodge.width = 0.72),
        alpha = 0.35,
        size = 1.35
      ) +
      facet_wrap(~ timepoint, nrow = 1) +
      scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
      labs(
        x = NULL,
        y = "Valid 24-hour PM2.5 (ug/m3)",
        color = "Study arm",
        title = paste0(metric_label_i, " in each valid 24-hour monitoring period"),
        subtitle = "Each point is one valid household 24-hour period; boxplots show median and IQR"
      ) +
      theme_bw(base_size = 11) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", color = "grey75")
      )
    fig_period <- hapin_add_y_scale(fig_period, metric_i)
    hapin_save_plot(
      fig_period,
      paste0("fig_pm25_hapin_24h_period_distribution_", metric_token_i, ".png"),
      width = 10,
      height = 5.6
    )
  }

  plot_agreement_data <- hapin_day1_day2_agreement_internal %>%
    filter(
      metric_name == metric_i,
      has_both_valid_24h_periods,
      !is.na(metric_value_24h_day1),
      !is.na(metric_value_24h_day2),
      is.finite(metric_value_24h_day1),
      is.finite(metric_value_24h_day2)
    )
  if (log_positive_i) {
    plot_agreement_data <- plot_agreement_data %>%
      filter(metric_value_24h_day1 > 0, metric_value_24h_day2 > 0)
  }

  if (nrow(plot_agreement_data) > 0) {
    fig_agreement <- ggplot(
      plot_agreement_data,
      aes(
        x = metric_value_24h_day1,
        y = metric_value_24h_day2,
        color = study_arm_overall
      )
    ) +
      geom_abline(slope = 1, intercept = 0, color = "grey35", linetype = "dashed", linewidth = 0.45) +
      geom_point(alpha = 0.58, size = 1.8) +
      facet_wrap(~ timepoint, nrow = 1) +
      scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
      labs(
        x = "Day 1 valid 24-hour PM2.5 (ug/m3)",
        y = "Day 2 valid 24-hour PM2.5 (ug/m3)",
        color = "Study arm",
        title = paste0(metric_label_i, ": Day 1 versus Day 2 agreement"),
        subtitle = "Dashed line is equality between the two valid 24-hour periods"
      ) +
      theme_bw(base_size = 11) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", color = "grey75")
      )
    if (log_positive_i) {
      fig_agreement <- fig_agreement + scale_x_log10() + scale_y_log10()
    } else {
      fig_agreement <- fig_agreement +
        geom_hline(yintercept = 0, color = "grey60", linewidth = 0.30) +
        geom_vline(xintercept = 0, color = "grey60", linewidth = 0.30)
    }
    hapin_save_plot(
      fig_agreement,
      paste0("fig_pm25_hapin_day1_day2_agreement_", metric_token_i, ".png"),
      width = 10,
      height = 5.6
    )
  }

  plot_hour_data <- hapin_hour_of_day_summary %>% filter(metric_name == metric_i)
  if (log_positive_i) {
    plot_hour_data <- plot_hour_data %>% filter(p10_pm25 > 0, p90_pm25 > 0, median_pm25 > 0)
  }

  if (nrow(plot_hour_data) > 0) {
    fig_hour <- ggplot(
      plot_hour_data,
      aes(
        x = hour_of_day,
        y = median_pm25,
        color = study_arm_overall,
        fill = study_arm_overall
      )
    ) +
      geom_ribbon(aes(ymin = p10_pm25, ymax = p90_pm25), alpha = 0.10, color = NA) +
      geom_ribbon(aes(ymin = p25_pm25, ymax = p75_pm25), alpha = 0.22, color = NA) +
      geom_line(linewidth = 0.85) +
      facet_wrap(~ timepoint, nrow = 1) +
      scale_x_continuous(breaks = seq(0, 23, by = 3)) +
      scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
      scale_fill_manual(values = hapin_arm_colors, drop = FALSE) +
      labs(
        x = paste0("Hour of day (", hapin_hour_time_zone, ")"),
        y = "Household-period-hour PM2.5 (ug/m3)",
        color = "Study arm",
        fill = "Study arm",
        title = paste0(metric_label_i, " by hour of day"),
        subtitle = "Line is median; darker band is IQR; lighter band is 10th to 90th percentile"
      ) +
      theme_bw(base_size = 11) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", color = "grey75")
      )
    fig_hour <- hapin_add_y_scale(fig_hour, metric_i)
    hapin_save_plot(
      fig_hour,
      paste0("fig_pm25_hapin_hour_of_day_", metric_token_i, ".png"),
      width = 10,
      height = 5.6
    )
  }

  plot_tail_data <- hapin_48h_household_summary_internal %>%
    filter(
      metric_name == metric_i,
      !is.na(metric_value_48h_time_weighted),
      is.finite(metric_value_48h_time_weighted)
    )
  if (log_positive_i) {
    plot_tail_data <- plot_tail_data %>% filter(metric_value_48h_time_weighted > 0)
  }

  if (nrow(plot_tail_data) > 0) {
    fig_tail <- ggplot(
      plot_tail_data,
      aes(x = metric_value_48h_time_weighted, color = study_arm_overall)
    ) +
      stat_ecdf(aes(y = after_stat((1 - y) * 100)), linewidth = 0.85) +
      facet_wrap(~ timepoint, nrow = 1) +
      scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
      scale_y_continuous(limits = c(0, 100)) +
      labs(
        x = "48-hour time-weighted PM2.5 (ug/m3)",
        y = "Household windows at or above concentration (%)",
        color = "Study arm",
        title = paste0(metric_label_i, " upper-tail distribution"),
        subtitle = "Complementary empirical distribution of household 48-hour summaries"
      ) +
      theme_bw(base_size = 11) +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey92", color = "grey75")
      )
    if (log_positive_i) {
      fig_tail <- fig_tail +
        geom_vline(
          xintercept = hapin_pm25_reference_lines,
          color = "grey55",
          linetype = "dashed",
          linewidth = 0.25
        ) +
        scale_x_log10()
    } else {
      fig_tail <- fig_tail + geom_vline(xintercept = 0, color = "grey35", linewidth = 0.35)
    }
    hapin_save_plot(
      fig_tail,
      paste0("fig_pm25_hapin_tail_exceedance_", metric_token_i, ".png"),
      width = 10,
      height = 5.6
    )
  }
}

hapin_adjusted_metric_name <- "ambient_adjusted_indoor_excess_pm25"
hapin_adjusted_metric_label <- hapin_metric_metadata$metric_label[
  hapin_metric_metadata$metric_name == hapin_adjusted_metric_name
]
hapin_adjusted_metric_token <- hapin_metric_file_token(hapin_adjusted_metric_name)
hapin_adjusted_log_note <- paste(
  "Positive adjusted values only are shown because log scales cannot display",
  "zero or negative ambient-adjusted indoor-excess PM2.5 values."
)

plot_48h_adjusted_log <- hapin_48h_household_summary_internal %>%
  filter(
    metric_name == hapin_adjusted_metric_name,
    !is.na(metric_value_48h_time_weighted),
    is.finite(metric_value_48h_time_weighted),
    metric_value_48h_time_weighted > 0
  )

if (nrow(plot_48h_adjusted_log) > 0) {
  fig_48h_adjusted_log <- ggplot(
    plot_48h_adjusted_log,
    aes(
      x = study_arm_overall,
      y = metric_value_48h_time_weighted,
      color = study_arm_overall
    )
  ) +
    geom_boxplot(width = 0.46, outlier.shape = NA, alpha = 0.12) +
    geom_jitter(width = 0.08, height = 0, alpha = 0.45, size = 1.6) +
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 23,
      fill = "white",
      color = "black",
      size = 2.8
    ) +
    facet_wrap(~ timepoint, nrow = 1) +
    scale_y_log10() +
    scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
    labs(
      x = NULL,
      y = "Positive 48-hour time-weighted PM2.5 (ug/m3, log scale)",
      color = "Study arm",
      title = paste0(hapin_adjusted_metric_label, " by study arm and timepoint"),
      subtitle = paste(
        "Positive adjusted household windows only; boxplots show median and IQR;",
        "diamonds show arithmetic means"
      )
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = "grey75")
    )

  hapin_save_plot(
    fig_48h_adjusted_log,
    paste0(
      "fig_pm25_hapin_48h_household_distribution_",
      hapin_adjusted_metric_token,
      "_positive_log_y.png"
    ),
    width = 10,
    height = 5.6
  )
}

plot_period_adjusted_log <- hapin_24h_period_summary_internal %>%
  filter(
    metric_name == hapin_adjusted_metric_name,
    metric_valid_24h,
    !is.na(metric_value_24h),
    is.finite(metric_value_24h),
    metric_value_24h > 0
  )

if (nrow(plot_period_adjusted_log) > 0) {
  fig_period_adjusted_log <- ggplot(
    plot_period_adjusted_log,
    aes(
      x = period_label,
      y = metric_value_24h,
      color = study_arm_overall
    )
  ) +
    geom_boxplot(
      aes(group = interaction(period_label, study_arm_overall)),
      position = position_dodge(width = 0.72),
      width = 0.52,
      outlier.shape = NA,
      alpha = 0.12
    ) +
    geom_jitter(
      position = position_jitterdodge(jitter.width = 0.10, dodge.width = 0.72),
      alpha = 0.35,
      size = 1.35
    ) +
    facet_wrap(~ timepoint, nrow = 1) +
    scale_y_log10() +
    scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
    labs(
      x = NULL,
      y = "Positive valid 24-hour PM2.5 (ug/m3, log scale)",
      color = "Study arm",
      title = paste0(hapin_adjusted_metric_label, " in each valid 24-hour monitoring period"),
      subtitle = "Positive adjusted 24-hour periods only; boxplots show median and IQR"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = "grey75")
    )

  hapin_save_plot(
    fig_period_adjusted_log,
    paste0(
      "fig_pm25_hapin_24h_period_distribution_",
      hapin_adjusted_metric_token,
      "_positive_log_y.png"
    ),
    width = 10,
    height = 5.6
  )
}

plot_agreement_adjusted_log <- hapin_day1_day2_agreement_internal %>%
  filter(
    metric_name == hapin_adjusted_metric_name,
    has_both_valid_24h_periods,
    !is.na(metric_value_24h_day1),
    !is.na(metric_value_24h_day2),
    is.finite(metric_value_24h_day1),
    is.finite(metric_value_24h_day2),
    metric_value_24h_day1 > 0,
    metric_value_24h_day2 > 0
  )

if (nrow(plot_agreement_adjusted_log) > 0) {
  fig_agreement_adjusted_log <- ggplot(
    plot_agreement_adjusted_log,
    aes(
      x = metric_value_24h_day1,
      y = metric_value_24h_day2,
      color = study_arm_overall
    )
  ) +
    geom_abline(slope = 1, intercept = 0, color = "grey35", linetype = "dashed", linewidth = 0.45) +
    geom_point(alpha = 0.58, size = 1.8) +
    facet_wrap(~ timepoint, nrow = 1) +
    scale_x_log10() +
    scale_y_log10() +
    scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
    labs(
      x = "Positive Day 1 valid 24-hour PM2.5 (ug/m3, log scale)",
      y = "Positive Day 2 valid 24-hour PM2.5 (ug/m3, log scale)",
      color = "Study arm",
      title = paste0(hapin_adjusted_metric_label, ": Day 1 versus Day 2 agreement"),
      subtitle = "Positive adjusted paired 24-hour periods only; dashed line is equality"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = "grey75")
    )

  hapin_save_plot(
    fig_agreement_adjusted_log,
    paste0(
      "fig_pm25_hapin_day1_day2_agreement_",
      hapin_adjusted_metric_token,
      "_positive_log_xy.png"
    ),
    width = 10,
    height = 5.6
  )
}

plot_hour_adjusted_log <- hapin_hour_of_day_summary %>%
  filter(
    metric_name == hapin_adjusted_metric_name,
    p10_pm25 > 0,
    p25_pm25 > 0,
    median_pm25 > 0,
    p75_pm25 > 0,
    p90_pm25 > 0
  )

if (nrow(plot_hour_adjusted_log) > 0) {
  fig_hour_adjusted_log <- ggplot(
    plot_hour_adjusted_log,
    aes(
      x = hour_of_day,
      y = median_pm25,
      color = study_arm_overall,
      fill = study_arm_overall
    )
  ) +
    geom_ribbon(aes(ymin = p10_pm25, ymax = p90_pm25), alpha = 0.10, color = NA) +
    geom_ribbon(aes(ymin = p25_pm25, ymax = p75_pm25), alpha = 0.22, color = NA) +
    geom_line(linewidth = 0.85) +
    facet_wrap(~ timepoint, nrow = 1) +
    scale_x_continuous(breaks = seq(0, 23, by = 3)) +
    scale_y_log10() +
    scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
    scale_fill_manual(values = hapin_arm_colors, drop = FALSE) +
    labs(
      x = paste0("Hour of day (", hapin_hour_time_zone, ")"),
      y = "Positive household-period-hour PM2.5 (ug/m3, log scale)",
      color = "Study arm",
      fill = "Study arm",
      title = paste0(hapin_adjusted_metric_label, " by hour of day"),
      subtitle = paste(
        "Positive adjusted percentile bands only; line is median;",
        "darker band is IQR; lighter band is 10th to 90th percentile"
      )
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = "grey75")
    )

  hapin_save_plot(
    fig_hour_adjusted_log,
    paste0(
      "fig_pm25_hapin_hour_of_day_",
      hapin_adjusted_metric_token,
      "_positive_log_y.png"
    ),
    width = 10,
    height = 5.6
  )
}

plot_tail_adjusted_log <- hapin_48h_household_summary_internal %>%
  filter(
    metric_name == hapin_adjusted_metric_name,
    !is.na(metric_value_48h_time_weighted),
    is.finite(metric_value_48h_time_weighted),
    metric_value_48h_time_weighted > 0
  )

if (nrow(plot_tail_adjusted_log) > 0) {
  fig_tail_adjusted_log <- ggplot(
    plot_tail_adjusted_log,
    aes(x = metric_value_48h_time_weighted, color = study_arm_overall)
  ) +
    stat_ecdf(aes(y = after_stat((1 - y) * 100)), linewidth = 0.85) +
    facet_wrap(~ timepoint, nrow = 1) +
    scale_x_log10() +
    scale_color_manual(values = hapin_arm_colors, drop = FALSE) +
    scale_y_continuous(limits = c(0, 100)) +
    labs(
      x = "Positive 48-hour time-weighted PM2.5 (ug/m3, log scale)",
      y = "Household windows at or above concentration (%)",
      color = "Study arm",
      title = paste0(hapin_adjusted_metric_label, " upper-tail distribution"),
      subtitle = "Positive adjusted household windows only; PM2.5 axis is log scaled"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = "grey75")
    )

  hapin_save_plot(
    fig_tail_adjusted_log,
    paste0(
      "fig_pm25_hapin_tail_exceedance_",
      hapin_adjusted_metric_token,
      "_positive_log_x.png"
    ),
    width = 10,
    height = 5.6
  )
}

hapin_graphic_source_audit <- bind_rows(
  data.frame(
    audit_item = c(
      "source_script",
      "period_definition",
      "valid_24h_coverage_threshold_percent",
      "monitoring_interval_assumption_seconds",
      "raw_metric_definition",
      "ambient_adjusted_metric_definition",
      "hour_of_day_time_zone",
      "public_row_level_deidentification",
      "uncertainty_display",
      "ambient_adjusted_log_scale_display"
    ),
    audit_value = c(
      normalizePath(script_path, winslash = "/", mustWork = FALSE),
      "Day 1 and Day 2 are consecutive 24-hour periods beginning at each monitoring deployment start_datetime.",
      as.character(valid_monitoring_coverage_threshold * 100),
      as.character(expected_monitoring_interval_seconds),
      "Raw indoor PM2.5 uses the arithmetic mean of positive finite indoor rows in each valid period/window.",
      sprintf(
        "Ambient-adjusted indoor-excess PM2.5 is raw indoor arithmetic mean minus %.2f x concurrent ambient arithmetic mean.",
        default_material_infiltration_factor
      ),
      hapin_hour_time_zone,
      "Public HAPIN PM2.5 row-level outputs remove household IDs, FCN IDs, household notes, raw filenames, monitor IDs, internal coverage-window IDs, and exact timestamps.",
      "Shaded hour-of-day bands are descriptive percentiles across household-period-hour summaries, not 95% confidence intervals.",
      hapin_adjusted_log_note
    ),
    output_file = NA_character_,
    stringsAsFactors = FALSE
  ),
  data.frame(
    audit_item = "generated_figure",
    audit_value = hapin_generated_figures,
    output_file = file.path(figure_dir, hapin_generated_figures),
    stringsAsFactors = FALSE
  )
)

readr::write_csv(
  hapin_graphic_source_audit,
  file.path(table_dir, "table_pm25_hapin_graphic_source_audit.csv"),
  na = ""
)

# Canonical PM2.5 household-timepoint dataset ---------------------------------

pm25_weighted_mean <- function(x, w) {
  keep <- is.finite(x) & !is.na(x) & is.finite(w) & !is.na(w) & w > 0
  if (!any(keep)) return(NA_real_)
  stats::weighted.mean(x[keep], w[keep])
}

pm25_mean_ci <- function(x, cluster = NULL, conf_level = 0.95) {
  keep <- is.finite(x) & !is.na(x)
  x <- x[keep]
  if (!is.null(cluster)) cluster <- as.character(cluster[keep])
  if (length(x) == 0) return(tibble(mean = NA_real_, conf_low = NA_real_, conf_high = NA_real_))

  estimate <- mean(x)
  if (length(x) < 2) return(tibble(mean = estimate, conf_low = NA_real_, conf_high = NA_real_))

  if (is.null(cluster)) {
    se <- stats::sd(x) / sqrt(length(x))
    df <- length(x) - 1
  } else {
    cluster[is.na(cluster) | !nzchar(cluster)] <- "missing_cluster"
    scores <- tapply(x - estimate, cluster, sum)
    g <- length(scores)
    if (g < 2) return(tibble(mean = estimate, conf_low = NA_real_, conf_high = NA_real_))
    se <- sqrt((g / (g - 1)) * sum(scores^2) / length(x)^2)
    df <- g - 1
  }

  critical <- stats::qt(1 - (1 - conf_level) / 2, df = df)
  tibble(mean = estimate, conf_low = estimate - critical * se, conf_high = estimate + critical * se)
}

ambient_period_sources <- bind_rows(lapply(seq_len(nrow(hapin_period_index)), function(i) {
  rows <- ambient_clean %>%
    filter(
      dateTime >= hapin_period_index$period_start_datetime[[i]],
      dateTime < hapin_period_index$period_end_datetime[[i]]
    )
  tibble(
    monitoring_coverage_window_id = hapin_period_index$monitoring_coverage_window_id[[i]],
    period_number = hapin_period_index$period_number[[i]],
    ambient_monitor_files = if (nrow(rows) == 0) NA_character_ else
      paste(sort(unique(na.omit(rows$raw_source_file))), collapse = ";"),
    ambient_monitor_ids = if (nrow(rows) == 0) NA_character_ else
      paste(sort(unique(na.omit(rows$PM_monitor))), collapse = ";")
  )
}))

pm25_period_internal <- hapin_24h_period_wide %>%
  left_join(
    ambient_period_sources,
    by = c("monitoring_coverage_window_id", "period_number")
  ) %>%
  mutate(
    collection_date = as.Date(period_start_datetime),
    ambient_coverage_prop_24h = pmin(1, n_ambient_hours_24h / monitoring_period_hours),
    has_concurrent_ambient = n_ambient_rows_24h > 0 &
      is.finite(ambient_mean_pm25_24h) & !is.na(ambient_mean_pm25_24h),
    analytic_period_common_support = has_valid_raw_24h & has_concurrent_ambient,
    pm25_ambient_excess_f000 = if_else(analytic_period_common_support, indoor_mean_pm25_24h, NA_real_),
    pm25_ambient_excess_f025 = if_else(analytic_period_common_support, indoor_mean_pm25_24h - 0.25 * ambient_mean_pm25_24h, NA_real_),
    pm25_ambient_excess_f050 = if_else(analytic_period_common_support, indoor_mean_pm25_24h - 0.50 * ambient_mean_pm25_24h, NA_real_),
    pm25_ambient_excess_f075 = if_else(analytic_period_common_support, indoor_mean_pm25_24h - 0.75 * ambient_mean_pm25_24h, NA_real_),
    pm25_ambient_excess_f100 = if_else(analytic_period_common_support, indoor_mean_pm25_24h - 1.00 * ambient_mean_pm25_24h, NA_real_)
  )

pm25_adjusted_cols <- c(
  "pm25_ambient_excess_f000", "pm25_ambient_excess_f025",
  "pm25_ambient_excess_f050", "pm25_ambient_excess_f075",
  "pm25_ambient_excess_f100"
)

common_periods <- pm25_period_internal %>% filter(analytic_period_common_support)
stopifnot(
  all(abs(common_periods$pm25_ambient_excess_f000 - common_periods$indoor_mean_pm25_24h) < 1e-10),
  all(abs(common_periods$pm25_ambient_excess_f025 - (common_periods$indoor_mean_pm25_24h - 0.25 * common_periods$ambient_mean_pm25_24h)) < 1e-10),
  all(abs(common_periods$pm25_ambient_excess_f050 - (common_periods$indoor_mean_pm25_24h - 0.50 * common_periods$ambient_mean_pm25_24h)) < 1e-10),
  all(abs(common_periods$pm25_ambient_excess_f075 - (common_periods$indoor_mean_pm25_24h - 0.75 * common_periods$ambient_mean_pm25_24h)) < 1e-10),
  all(abs(common_periods$pm25_ambient_excess_f100 - (common_periods$indoor_mean_pm25_24h - 1.00 * common_periods$ambient_mean_pm25_24h)) < 1e-10)
)

pm25_window_internal <- common_periods %>%
  group_by(
    timepoint, study_arm_overall, hh_id, fcn_id, hh_id_note,
    raw_source_file, PM_monitor, monitoring_coverage_window_id, hapin_window_id
  ) %>%
  summarise(
    collection_date_min = min(collection_date, na.rm = TRUE),
    collection_date_max = max(collection_date, na.rm = TRUE),
    n_valid_24h_periods = n_distinct(period_number),
    valid_24h_periods = paste(sort(unique(period_number)), collapse = ";"),
    mean_ambient_coverage_prop = pm25_weighted_mean(ambient_coverage_prop_24h, valid_monitoring_hours),
    indoor_pm25_mean = pm25_weighted_mean(indoor_mean_pm25_24h, valid_monitoring_hours),
    ambient_pm25_mean = pm25_weighted_mean(ambient_mean_pm25_24h, valid_monitoring_hours),
    across(all_of(pm25_adjusted_cols), ~ pm25_weighted_mean(.x, valid_monitoring_hours)),
    valid_monitoring_hours = sum(valid_monitoring_hours, na.rm = TRUE),
    n_pm_observations = sum(n_pm_rows_24h, na.rm = TRUE),
    n_ambient_observations = sum(n_ambient_rows_24h, na.rm = TRUE),
    ambient_monitor_files = paste(sort(unique(na.omit(ambient_monitor_files))), collapse = ";"),
    ambient_monitor_ids = paste(sort(unique(na.omit(ambient_monitor_ids))), collapse = ";"),
    .groups = "drop"
  )

pm25_household_timepoint_internal <- pm25_window_internal %>%
  group_by(fcn_id, timepoint) %>%
  summarise(
    study_arm_overall = first_nonmissing(as.character(study_arm_overall)),
    hh_id = paste(sort(unique(na.omit(hh_id))), collapse = ";"),
    hh_id_note = paste(sort(unique(na.omit(hh_id_note))), collapse = ";"),
    collection_date_min = min(collection_date_min, na.rm = TRUE),
    collection_date_max = max(collection_date_max, na.rm = TRUE),
    n_monitoring_windows = n_distinct(monitoring_coverage_window_id),
    n_monitor_files = n_distinct(raw_source_file),
    n_valid_24h_periods = sum(n_valid_24h_periods, na.rm = TRUE),
    mean_ambient_coverage_prop = pm25_weighted_mean(mean_ambient_coverage_prop, valid_monitoring_hours),
    indoor_pm25_mean = pm25_weighted_mean(indoor_pm25_mean, valid_monitoring_hours),
    ambient_pm25_mean = pm25_weighted_mean(ambient_pm25_mean, valid_monitoring_hours),
    across(all_of(pm25_adjusted_cols), ~ pm25_weighted_mean(.x, valid_monitoring_hours)),
    valid_monitoring_hours = sum(valid_monitoring_hours, na.rm = TRUE),
    coverage_percent_of_48h = pmin(100, 100 * valid_monitoring_hours / monitoring_total_hours),
    n_pm_observations = sum(n_pm_observations, na.rm = TRUE),
    n_ambient_observations = sum(n_ambient_observations, na.rm = TRUE),
    raw_source_files = paste(sort(unique(na.omit(raw_source_file))), collapse = ";"),
    indoor_monitor_ids = paste(sort(unique(na.omit(PM_monitor))), collapse = ";"),
    ambient_monitor_files = paste(sort(unique(na.omit(ambient_monitor_files))), collapse = ";"),
    ambient_monitor_ids = paste(sort(unique(na.omit(ambient_monitor_ids))), collapse = ";"),
    source_script = "3_descriptive_outcomes_20260805_2213.R",
    ambient_fraction_default = default_material_infiltration_factor,
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall, fcn_id)

duplicate_pm25_keys <- pm25_household_timepoint_internal %>% count(fcn_id, timepoint) %>% filter(n != 1)
if (nrow(duplicate_pm25_keys) > 0) {
  stop("Canonical PM2.5 dataset does not have exactly one row per fcn_id-timepoint.", call. = FALSE)
}

arm_conflicts <- pm25_window_internal %>%
  group_by(fcn_id, timepoint) %>%
  summarise(n_arms = n_distinct(study_arm_overall), .groups = "drop") %>%
  filter(n_arms != 1)
if (nrow(arm_conflicts) > 0) stop("Conflicting study arms in canonical PM2.5 keys.", call. = FALSE)

adjusted_missing_patterns <- pm25_household_timepoint_internal %>%
  transmute(across(all_of(pm25_adjusted_cols), is.na)) %>%
  distinct()
if (nrow(adjusted_missing_patterns) != 1 || any(unlist(adjusted_missing_patterns[1, ]))) {
  stop("The five ambient-fraction outcomes do not share one complete analytic population.", call. = FALSE)
}

pm25_household_timepoint_public <- pm25_household_timepoint_internal %>%
  select(
    timepoint, study_arm_overall, n_monitoring_windows, n_valid_24h_periods,
    valid_monitoring_hours, coverage_percent_of_48h, n_pm_observations,
    n_ambient_observations, mean_ambient_coverage_prop, indoor_pm25_mean,
    ambient_pm25_mean, all_of(pm25_adjusted_cols), ambient_fraction_default
  )

readr::write_csv(
  pm25_period_internal,
  file.path(restricted_table_dir, "table_descriptive_pm25_24h_period_internal.csv"),
  na = ""
)
readr::write_csv(
  pm25_window_internal,
  file.path(restricted_table_dir, "table_descriptive_pm25_monitoring_window_internal.csv"),
  na = ""
)
readr::write_csv(
  pm25_household_timepoint_internal,
  file.path(restricted_table_dir, "table_descriptive_pm25_household_timepoint_internal.csv"),
  na = ""
)
readr::write_csv(
  pm25_period_internal %>% hapin_deidentify() %>%
    select(-any_of(c("collection_date", "period_start_datetime", "period_end_datetime", "ambient_monitor_files", "ambient_monitor_ids"))),
  file.path(table_dir, "table_descriptive_pm25_24h_period_deidentified.csv"),
  na = ""
)
readr::write_csv(
  pm25_household_timepoint_public,
  file.path(table_dir, "table_descriptive_pm25_household_timepoint_deidentified.csv"),
  na = ""
)

# Caregiver and target-child time-weighted exposure ---------------------------

reconcile_hours_inside <- function(hours_inside, hours_outside) {
  inside <- clean_hours_0_24(hours_inside)
  outside <- clean_hours_0_24(hours_outside)
  dplyr::case_when(
    !is.na(inside) & !is.na(outside) ~ (inside + 24 - outside) / 2,
    !is.na(inside) ~ inside,
    !is.na(outside) ~ 24 - outside,
    TRUE ~ NA_real_
  )
}

time_reporting_pattern <- function(hours_inside, hours_outside) {
  inside <- clean_hours_0_24(hours_inside)
  outside <- clean_hours_0_24(hours_outside)
  dplyr::case_when(
    !is.na(inside) & !is.na(outside) ~ "both_reported",
    !is.na(inside) ~ "inside_only",
    !is.na(outside) ~ "outside_only",
    TRUE ~ "neither_reported"
  )
}

household_time_key <- survey_household %>%
  transmute(
    household_key = as.character(KEY),
    fcn_id = as.character(fcn_id),
    hh_id = as.character(hh_id),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    respondent_serial = as.character(respondent_sl)
  ) %>%
  filter(
    !is.na(household_key),
    !is.na(fcn_id),
    timepoint %in% timepoint_levels,
    study_arm_overall %in% arm_levels
  )

member_age_key <- survey_hh_members %>%
  transmute(
    household_key = as.character(PARENT_KEY),
    person_serial = as.character(mem_serial),
    person_age_years = clean_age_years(age_yrs)
  ) %>%
  distinct(household_key, person_serial, .keep_all = TRUE)

baseline_time_records <- bind_rows(
  survey_household %>%
    transmute(
      fcn_id = as.character(fcn_id),
      hh_id = as.character(hh_id),
      timepoint = as.character(timepoint),
      study_arm_overall = as.character(study_arm_overall),
      population = "caregiver",
      population_definition = "survey respondent",
      reported_hours_inside = clean_hours_0_24(hours_inside),
      reported_hours_outside = clean_hours_0_24(hours_outside),
      time_use_source = "baseline household questionnaire"
    ),
  survey_household %>%
    transmute(
      fcn_id = as.character(fcn_id),
      hh_id = as.character(hh_id),
      timepoint = as.character(timepoint),
      study_arm_overall = as.character(study_arm_overall),
      population = "target_child",
      population_definition = "designated target child",
      reported_hours_inside = clean_hours_0_24(target_child_hours_inside),
      reported_hours_outside = clean_hours_0_24(target_child_hours_outside),
      time_use_source = "baseline household questionnaire"
    )
) %>%
  filter(
    timepoint == "baseline",
    study_arm_overall %in% arm_levels,
    !is.na(fcn_id)
  )

midline_location_records <- survey_location %>%
  transmute(
    household_key = as.character(PARENT_KEY),
    person_serial = as.character(location_number),
    reported_hours_inside = clean_hours_0_24(hours_inside),
    reported_hours_outside = clean_hours_0_24(hours_outside)
  ) %>%
  left_join(household_time_key, by = "household_key") %>%
  left_join(member_age_key, by = c("household_key", "person_serial")) %>%
  filter(
    timepoint == "midline",
    study_arm_overall %in% arm_levels,
    !is.na(fcn_id)
  )

midline_time_records <- bind_rows(
  midline_location_records %>%
    filter(!is.na(respondent_serial), person_serial == respondent_serial) %>%
    transmute(
      fcn_id, hh_id, timepoint, study_arm_overall,
      population = "caregiver",
      population_definition = "survey respondent",
      reported_hours_inside,
      reported_hours_outside,
      time_use_source = "midline household-location repeat"
    ),
  midline_location_records %>%
    filter(!is.na(person_age_years), person_age_years < 5) %>%
    transmute(
      fcn_id, hh_id, timepoint, study_arm_overall,
      population = "target_child",
      population_definition = "mean across household members younger than 5 years because a target-child serial was not available",
      reported_hours_inside,
      reported_hours_outside,
      time_use_source = "midline household-location repeat"
    )
)

endline_member_records <- survey_hh_members %>%
  transmute(
    household_key = as.character(PARENT_KEY),
    person_serial = as.character(mem_serial),
    person_age_years = clean_age_years(age_yrs),
    reported_hours_inside = NA_real_,
    reported_hours_outside = clean_hours_0_24(hours_outside)
  ) %>%
  left_join(household_time_key, by = "household_key") %>%
  filter(
    timepoint == "endline",
    study_arm_overall %in% arm_levels,
    !is.na(fcn_id)
  )

endline_time_records <- bind_rows(
  endline_member_records %>%
    filter(!is.na(respondent_serial), person_serial == respondent_serial) %>%
    transmute(
      fcn_id, hh_id, timepoint, study_arm_overall,
      population = "caregiver",
      population_definition = "survey respondent",
      reported_hours_inside,
      reported_hours_outside,
      time_use_source = "endline household-member repeat"
    ),
  endline_member_records %>%
    filter(!is.na(person_age_years), person_age_years < 5) %>%
    transmute(
      fcn_id, hh_id, timepoint, study_arm_overall,
      population = "target_child",
      population_definition = "mean across household members younger than 5 years because a target-child serial was not available",
      reported_hours_inside,
      reported_hours_outside,
      time_use_source = "endline household-member repeat"
    )
)

pm25_time_use_person_records <- bind_rows(
  baseline_time_records,
  midline_time_records,
  endline_time_records
) %>%
  mutate(
    person_time_reporting_pattern = time_reporting_pattern(
      reported_hours_inside,
      reported_hours_outside
    ),
    hours_inside_est = reconcile_hours_inside(
      reported_hours_inside,
      reported_hours_outside
    ),
    hours_outside_est = 24 - hours_inside_est
  ) %>%
  filter(!is.na(hours_inside_est), is.finite(hours_inside_est))

pm25_time_use_household <- pm25_time_use_person_records %>%
  group_by(
    fcn_id, hh_id, timepoint, study_arm_overall, population,
    population_definition, time_use_source
  ) %>%
  summarise(
    n_people_contributing_time_use = n(),
    reported_hours_inside = mean_or_na(reported_hours_inside),
    reported_hours_outside = mean_or_na(reported_hours_outside),
    hours_inside_est = mean_or_na(hours_inside_est),
    hours_outside_est = 24 - hours_inside_est,
    n_people_both_reported = sum(person_time_reporting_pattern == "both_reported"),
    n_people_inside_only = sum(person_time_reporting_pattern == "inside_only"),
    n_people_outside_only = sum(person_time_reporting_pattern == "outside_only"),
    .groups = "drop"
  ) %>%
  mutate(
    household_time_reporting_pattern = case_when(
      n_people_both_reported == n_people_contributing_time_use ~ "both_reported",
      n_people_inside_only == n_people_contributing_time_use ~ "inside_only",
      n_people_outside_only == n_people_contributing_time_use ~ "outside_only",
      TRUE ~ "mixed_patterns"
    )
  )

duplicate_time_use_keys <- pm25_time_use_household %>%
  count(fcn_id, timepoint, population) %>%
  filter(n != 1)
if (nrow(duplicate_time_use_keys) > 0) {
  stop("Time-use data do not have exactly one row per fcn_id-timepoint-population.", call. = FALSE)
}

pm25_exposure_household_internal <- pm25_time_use_household %>%
  left_join(
    pm25_household_timepoint_internal %>%
      transmute(
        fcn_id,
        timepoint = as.character(timepoint),
        pm_study_arm_overall = as.character(study_arm_overall),
        indoor_pm25_ug_m3 = indoor_pm25_mean,
        outdoor_pm25_ug_m3 = ambient_pm25_mean,
        n_valid_24h_periods,
        valid_monitoring_hours,
        n_monitoring_windows
      ),
    by = c("fcn_id", "timepoint")
  ) %>%
  mutate(
    pm_arm_matches_time_use = is.na(pm_study_arm_overall) |
      pm_study_arm_overall == study_arm_overall,
    time_weighted_average_pm25_ug_m3 = if_else(
      is.finite(hours_inside_est) &
        is.finite(hours_outside_est) &
        is.finite(indoor_pm25_ug_m3) &
        is.finite(outdoor_pm25_ug_m3),
      (hours_inside_est / 24) * indoor_pm25_ug_m3 +
        (hours_outside_est / 24) * outdoor_pm25_ug_m3,
      NA_real_
    ),
    exposure_formula = "(hours_inside_est/24)*indoor_pm25 + (hours_outside_est/24)*outdoor_pm25"
  ) %>%
  arrange(population, factor(timepoint, levels = timepoint_levels), study_arm_overall, fcn_id)

if (any(!pm25_exposure_household_internal$pm_arm_matches_time_use, na.rm = TRUE)) {
  stop("Study-arm mismatch between time-use and canonical PM2.5 data.", call. = FALSE)
}
if (any(
  abs(
    pm25_exposure_household_internal$hours_inside_est +
      pm25_exposure_household_internal$hours_outside_est - 24
  ) > 1e-10,
  na.rm = TRUE
)) {
  stop("Estimated inside and outside hours do not sum to 24.", call. = FALSE)
}

readr::write_csv(
  pm25_exposure_household_internal,
  file.path(restricted_table_dir, "table_descriptive_pm25_time_weighted_exposure_internal.csv"),
  na = ""
)

pm25_exposure_for_summary <- bind_rows(
  pm25_exposure_household_internal,
  pm25_exposure_household_internal %>%
    filter(study_arm_overall %in% arm_levels) %>%
    mutate(study_arm_overall = "all_arms")
)

pm25_exposure_summary <- pm25_exposure_for_summary %>%
  group_by(timepoint, study_arm_overall, population) %>%
  group_modify(function(.x, .y) {
    has_time <- is.finite(.x$hours_inside_est) & is.finite(.x$hours_outside_est)
    has_joint_data <- has_time &
      is.finite(.x$indoor_pm25_ug_m3) &
      is.finite(.x$outdoor_pm25_ug_m3)
    time_analytic <- .x[has_time, , drop = FALSE]
    analytic <- .x[has_joint_data, , drop = FALSE]
    inside_ci <- pm25_mean_ci(time_analytic$hours_inside_est)
    outside_ci <- pm25_mean_ci(time_analytic$hours_outside_est)
    indoor_ci <- pm25_mean_ci(analytic$indoor_pm25_ug_m3)
    outdoor_ci <- pm25_mean_ci(analytic$outdoor_pm25_ug_m3)
    group_hours_inside <- inside_ci$mean
    group_hours_outside <- outside_ci$mean
    group_weighted_exposure <- if (
      nrow(analytic) > 0 &&
        is.finite(group_hours_inside) &&
        is.finite(group_hours_outside)
    ) {
      (group_hours_inside / 24) * analytic$indoor_pm25_ug_m3 +
        (group_hours_outside / 24) * analytic$outdoor_pm25_ug_m3
    } else {
      numeric()
    }
    exposure_ci <- pm25_mean_ci(group_weighted_exposure)

    tibble(
      n_households_with_time_data = sum(has_time),
      n_households_with_time_and_pm = sum(has_joint_data),
      n_households_both_reported = sum(has_time & .x$household_time_reporting_pattern == "both_reported"),
      n_households_inside_only = sum(has_time & .x$household_time_reporting_pattern == "inside_only"),
      n_households_outside_only = sum(has_time & .x$household_time_reporting_pattern == "outside_only"),
      n_households_mixed_patterns = sum(has_time & .x$household_time_reporting_pattern == "mixed_patterns"),
      mean_hours_inside_est = inside_ci$mean,
      sd_hours_inside_est = if (nrow(time_analytic) > 1) stats::sd(time_analytic$hours_inside_est) else NA_real_,
      mean_hours_inside_est_ci_lower = inside_ci$conf_low,
      mean_hours_inside_est_ci_upper = inside_ci$conf_high,
      mean_hours_outside_est = outside_ci$mean,
      sd_hours_outside_est = if (nrow(time_analytic) > 1) stats::sd(time_analytic$hours_outside_est) else NA_real_,
      mean_hours_outside_est_ci_lower = outside_ci$conf_low,
      mean_hours_outside_est_ci_upper = outside_ci$conf_high,
      mean_indoor_pm25_ug_m3 = indoor_ci$mean,
      sd_indoor_pm25_ug_m3 = if (nrow(analytic) > 1) stats::sd(analytic$indoor_pm25_ug_m3) else NA_real_,
      mean_indoor_pm25_ci_lower = indoor_ci$conf_low,
      mean_indoor_pm25_ci_upper = indoor_ci$conf_high,
      mean_outdoor_pm25_ug_m3 = outdoor_ci$mean,
      sd_outdoor_pm25_ug_m3 = if (nrow(analytic) > 1) stats::sd(analytic$outdoor_pm25_ug_m3) else NA_real_,
      mean_outdoor_pm25_ci_lower = outdoor_ci$conf_low,
      mean_outdoor_pm25_ci_upper = outdoor_ci$conf_high,
      mean_time_weighted_average_pm25_ug_m3 = exposure_ci$mean,
      sd_time_weighted_average_pm25_ug_m3 =
        if (length(group_weighted_exposure) > 1) stats::sd(group_weighted_exposure) else NA_real_,
      mean_time_weighted_average_pm25_ci_lower = exposure_ci$conf_low,
      mean_time_weighted_average_pm25_ci_upper = exposure_ci$conf_high,
      median_time_weighted_average_pm25_ug_m3 =
        median(group_weighted_exposure, na.rm = TRUE),
      p25_time_weighted_average_pm25_ug_m3 =
        safe_quantile(group_weighted_exposure, 0.25),
      p75_time_weighted_average_pm25_ug_m3 =
        safe_quantile(group_weighted_exposure, 0.75),
      time_use_estimation_rule =
        "If both reports were valid: (inside + 24 - outside)/2; if only one was valid, use it or its 24-hour complement.",
      exposure_formula =
        "(arm-timepoint-population mean hours_inside_est/24)*household indoor_pm25 + (arm-timepoint-population mean hours_outside_est/24)*household outdoor_pm25",
      analysis_population =
        "Time weights use all households with valid time-use data in the study-arm/timepoint/population stratum; PM2.5 and exposure summaries use households in that stratum with canonical matched indoor and outdoor PM2.5. all_arms rows pool both study arms before calculating the time weights."
    )
  }) %>%
  ungroup()

pm25_exposure_summary_public <- tidyr::expand_grid(
  timepoint = timepoint_levels,
  study_arm_overall = c(arm_levels, "all_arms"),
  population = c("caregiver", "target_child")
) %>%
  left_join(
    pm25_exposure_summary %>%
      mutate(
        timepoint = as.character(timepoint),
        study_arm_overall = as.character(study_arm_overall),
        population = as.character(population)
      ),
    by = c("timepoint", "study_arm_overall", "population")
  ) %>%
  arrange(
    factor(population, levels = c("caregiver", "target_child")),
    factor(timepoint, levels = timepoint_levels),
    factor(study_arm_overall, levels = c(arm_levels, "all_arms"))
  )

readr::write_csv(
  pm25_exposure_summary_public,
  file.path(table_dir, "table_descriptive_pm25_time_weighted_exposure.csv"),
  na = ""
)

pm25_time_use_source_audit <- pm25_time_use_household %>%
  count(
    timepoint, study_arm_overall, population, population_definition,
    time_use_source, household_time_reporting_pattern,
    wt = n_people_contributing_time_use,
    name = "n_people_records"
  ) %>%
  left_join(
    pm25_time_use_household %>%
      count(
        timepoint, study_arm_overall, population, population_definition,
        time_use_source, household_time_reporting_pattern,
        name = "n_households"
      ),
    by = c(
      "timepoint", "study_arm_overall", "population",
      "population_definition", "time_use_source",
      "household_time_reporting_pattern"
    )
  ) %>%
  arrange(
    factor(population, levels = c("caregiver", "target_child")),
    factor(timepoint, levels = timepoint_levels),
    factor(study_arm_overall, levels = arm_levels),
    household_time_reporting_pattern
  )

readr::write_csv(
  pm25_time_use_source_audit,
  file.path(table_dir, "table_descriptive_pm25_time_use_source_audit.csv"),
  na = ""
)

pm25_measure_lookup <- tribble(
  ~source_column, ~measure, ~ambient_fraction, ~is_default,
  "indoor_pm25_mean", "raw_indoor_pm25", NA_real_, FALSE,
  "ambient_pm25_mean", "matched_outdoor_pm25", NA_real_, FALSE,
  "pm25_ambient_excess_f000", "ambient_adjusted_f000", 0.00, FALSE,
  "pm25_ambient_excess_f025", "ambient_adjusted_f025", 0.25, FALSE,
  "pm25_ambient_excess_f050", "ambient_adjusted_f050", 0.50, FALSE,
  "pm25_ambient_excess_f075", "ambient_adjusted_f075", 0.75, TRUE,
  "pm25_ambient_excess_f100", "ambient_adjusted_f100", 1.00, FALSE
)

pm25_household_long_arm <- pm25_household_timepoint_internal %>%
  select(fcn_id, timepoint, study_arm_overall, ambient_monitor_files, all_of(pm25_measure_lookup$source_column)) %>%
  pivot_longer(all_of(pm25_measure_lookup$source_column), names_to = "source_column", values_to = "pm25_ug_m3") %>%
  left_join(pm25_measure_lookup, by = "source_column")
pm25_household_long <- bind_rows(
  pm25_household_long_arm,
  pm25_household_long_arm %>% mutate(study_arm_overall = "all")
)

pm25_arm_timepoint_summary <- pm25_household_long %>%
  group_by(timepoint, study_arm_overall, source_column, measure, ambient_fraction, is_default) %>%
  group_modify(~ {
    cluster <- if (.y$measure[[1]] == "matched_outdoor_pm25") .x$ambient_monitor_files else NULL
    ci <- pm25_mean_ci(.x$pm25_ug_m3, cluster)
    tibble(
      n_household_timepoints = sum(is.finite(.x$pm25_ug_m3) & !is.na(.x$pm25_ug_m3)),
      n_households = n_distinct(.x$fcn_id[is.finite(.x$pm25_ug_m3) & !is.na(.x$pm25_ug_m3)]),
      mean_pm25 = ci$mean,
      sd_pm25 = if (sum(is.finite(.x$pm25_ug_m3) & !is.na(.x$pm25_ug_m3)) > 1)
        stats::sd(.x$pm25_ug_m3[is.finite(.x$pm25_ug_m3) & !is.na(.x$pm25_ug_m3)]) else NA_real_,
      mean_pm25_ci_lower = ci$conf_low,
      mean_pm25_ci_upper = ci$conf_high,
      median_pm25 = median(.x$pm25_ug_m3, na.rm = TRUE),
      p25_pm25 = safe_quantile(.x$pm25_ug_m3, 0.25),
      p75_pm25 = safe_quantile(.x$pm25_ug_m3, 0.75),
      p95_pm25 = safe_quantile(.x$pm25_ug_m3, 0.95),
      max_pm25 = max(.x$pm25_ug_m3, na.rm = TRUE),
      ci_method = if (.y$measure[[1]] == "matched_outdoor_pm25")
        "cluster-robust by concurrent ambient monitor-file set" else
        "t interval across household-timepoint observations"
    )
  }) %>%
  ungroup() %>%
  arrange(timepoint, factor(study_arm_overall, levels = c(arm_levels, "all")), match(source_column, pm25_measure_lookup$source_column))

readr::write_csv(
  pm25_arm_timepoint_summary,
  file.path(table_dir, "table_descriptive_pm25_arm_timepoint.csv"),
  na = ""
)
readr::write_csv(
  pm25_arm_timepoint_summary %>% filter(measure == "matched_outdoor_pm25"),
  file.path(table_dir, "table_descriptive_pm25_ambient_summary.csv"),
  na = ""
)

pm25_indoor_selected_arm_timepoint <- pm25_arm_timepoint_summary %>%
  filter(
    measure == "raw_indoor_pm25",
    study_arm_overall %in% arm_levels,
    (
      study_arm_overall == "comparison" &
        as.character(timepoint) %in% timepoint_levels
    ) |
      (
        study_arm_overall == "intervention" &
          as.character(timepoint) %in% c("midline", "endline")
      )
  ) %>%
  transmute(
    timepoint = as.character(timepoint),
    study_arm_overall,
    n_households,
    mean_indoor_pm25_ug_m3 = mean_pm25,
    sd_indoor_pm25_ug_m3 = sd_pm25,
    mean_indoor_pm25_95ci_lower = mean_pm25_ci_lower,
    mean_indoor_pm25_95ci_upper = mean_pm25_ci_upper,
    ci_method,
    analysis_population =
      "Canonical household-timepoint observations with valid 24-hour indoor monitoring and concurrent ambient PM2.5."
  ) %>%
  arrange(
    factor(study_arm_overall, levels = arm_levels),
    factor(timepoint, levels = timepoint_levels)
  )

readr::write_csv(
  pm25_indoor_selected_arm_timepoint,
  file.path(table_dir, "table_descriptive_pm25_indoor_selected_arm_timepoint.csv"),
  na = ""
)

# Add concurrent outdoor PM2.5 to the period-hour descriptive population and
# overwrite the hour-of-day table with all three primary descriptive metrics.
pm25_hour_primary <- bind_rows(
  hapin_household_period_hour_long,
  hapin_household_period_hour_wide %>%
    filter(n_ambient_rows_hour > 0, is.finite(ambient_mean_pm25_hour)) %>%
    transmute(
      metric_name = "concurrent_outdoor_pm25",
      metric_label = "Concurrent outdoor PM2.5",
      metric_scale = "log_positive",
      timepoint, study_arm_overall, hh_id, fcn_id, hapin_window_id,
      period_number, period_label, monitor_hour, hour_of_day,
      metric_value_hour = ambient_mean_pm25_hour,
      n_pm_rows_hour, n_ambient_rows_hour
    )
)

pm25_hour_primary_summary <- pm25_hour_primary %>%
  group_by(metric_name, metric_label, metric_scale, timepoint, study_arm_overall, hour_of_day) %>%
  summarise(
    n_household_period_hours = n(),
    n_households = n_distinct(hh_id),
    n_valid_24h_periods = n_distinct(paste(hapin_window_id, period_number)),
    mean_pm25 = mean(metric_value_hour, na.rm = TRUE),
    median_pm25 = median(metric_value_hour, na.rm = TRUE),
    p10_pm25 = safe_quantile(metric_value_hour, 0.10),
    p25_pm25 = safe_quantile(metric_value_hour, 0.25),
    p75_pm25 = safe_quantile(metric_value_hour, 0.75),
    p90_pm25 = safe_quantile(metric_value_hour, 0.90),
    .groups = "drop"
  ) %>%
  filter(n_households >= 3)

readr::write_csv(pm25_hour_primary_summary, file.path(table_dir, "table_descriptive_pm25_time_of_day.csv"), na = "")
readr::write_csv(pm25_hour_primary_summary, file.path(table_dir, "table_pm25_hapin_hour_of_day_summary.csv"), na = "")

p_hour_primary <- pm25_hour_primary_summary %>%
  mutate(
    study_arm_overall = factor(study_arm_overall, levels = arm_levels),
    metric_label = factor(
      metric_label,
      levels = c("Raw indoor PM2.5", "Concurrent outdoor PM2.5", "Ambient-adjusted indoor-excess PM2.5")
    )
  ) %>%
  ggplot(aes(x = hour_of_day, color = study_arm_overall, fill = study_arm_overall)) +
  geom_ribbon(aes(ymin = p10_pm25, ymax = p90_pm25), alpha = 0.10, color = NA) +
  geom_ribbon(aes(ymin = p25_pm25, ymax = p75_pm25), alpha = 0.20, color = NA) +
  geom_line(aes(y = median_pm25), linewidth = 0.65) +
  facet_grid(metric_label ~ timepoint, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, 23, 4)) +
  labs(
    x = "Hour of day", y = "PM2.5 (ug/m3)", color = "Study arm", fill = "Study arm",
    caption = "Lines are medians; dark and light bands are the 25th-75th and 10th-90th percentiles of household-period-hour means."
  ) +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave(file.path(figure_dir, "fig_descriptive_pm25_time_of_day_primary.png"), p_hour_primary, width = 12, height = 9, dpi = 300)
ggsave(file.path(figure_dir, "fig_descriptive_pm25_hourly_patterns.png"), p_hour_primary, width = 12, height = 9, dpi = 300)

pm25_paired_change <- pm25_household_timepoint_internal %>%
  select(fcn_id, study_arm_overall, timepoint, pm25_ambient_excess_f075) %>%
  pivot_wider(names_from = timepoint, values_from = pm25_ambient_excess_f075) %>%
  mutate(
    change_midline_minus_baseline = midline - baseline,
    change_endline_minus_baseline = endline - baseline
  )
readr::write_csv(
  pm25_paired_change %>% select(-fcn_id),
  file.path(table_dir, "table_descriptive_pm25_paired_change.csv"),
  na = ""
)

p_sampling <- pm25_household_timepoint_internal %>%
  ggplot(aes(x = collection_date_min, y = study_arm_overall, color = study_arm_overall)) +
  geom_point(position = position_jitter(height = 0.12, width = 0), alpha = 0.70, size = 1.6) +
  facet_wrap(~timepoint, scales = "free_x") +
  labs(x = "Monitoring start date", y = NULL, color = "Study arm") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(figure_dir, "fig_descriptive_pm25_sampling_timeline.png"), p_sampling, width = 10, height = 5, dpi = 300)

p_ambient_date <- common_periods %>%
  ggplot(aes(x = collection_date)) +
  geom_point(aes(y = indoor_mean_pm25_24h, color = study_arm_overall), alpha = 0.65, size = 1.4) +
  geom_point(aes(y = ambient_mean_pm25_24h), color = "black", alpha = 0.45, size = 1.2, shape = 17) +
  facet_wrap(~timepoint, scales = "free_x") +
  scale_y_log10() +
  labs(x = "Monitoring date", y = "24-hour mean PM2.5 (ug/m3, log scale)", color = "Indoor study arm") +
  theme_bw(base_size = 10) + theme(legend.position = "bottom")
ggsave(file.path(figure_dir, "fig_descriptive_pm25_ambient_by_date.png"), p_ambient_date, width = 10, height = 5, dpi = 300)

p_paired <- pm25_household_timepoint_internal %>%
  filter(timepoint %in% c("baseline", "midline", "endline")) %>%
  mutate(timepoint = factor(timepoint, levels = timepoint_levels)) %>%
  ggplot(aes(x = timepoint, y = pm25_ambient_excess_f075, group = fcn_id, color = study_arm_overall)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_line(alpha = 0.18, linewidth = 0.35) +
  geom_point(alpha = 0.45, size = 1.1) +
  facet_wrap(~study_arm_overall) +
  labs(x = NULL, y = "Indoor minus 0.75 x outdoor PM2.5 (ug/m3)", color = "Study arm") +
  theme_bw(base_size = 10) + theme(legend.position = "none")
ggsave(file.path(figure_dir, "fig_descriptive_pm25_paired_household_change.png"), p_paired, width = 9, height = 5, dpi = 300)

pm25_canonical_audit <- tibble(
  check = c(
    "canonical_rows", "unique_household_timepoint_keys", "valid_common_support_periods",
    "all_fractions_complete", "f000_equals_indoor", "default_fraction"
  ),
  value = c(
    nrow(pm25_household_timepoint_internal),
    n_distinct(paste(pm25_household_timepoint_internal$fcn_id, pm25_household_timepoint_internal$timepoint)),
    nrow(common_periods),
    sum(stats::complete.cases(pm25_household_timepoint_internal[, pm25_adjusted_cols])),
    sum(abs(pm25_household_timepoint_internal$pm25_ambient_excess_f000 - pm25_household_timepoint_internal$indoor_pm25_mean) < 1e-10),
    default_material_infiltration_factor
  ),
  source_script = "3_descriptive_outcomes_20260805_2213.R"
)
readr::write_csv(pm25_canonical_audit, file.path(dir_tables_qa, "table_qa_pm25_canonical_pipeline.csv"), na = "")

})
################################################################################
# Consolidated fuel-outage and plastic-burning descriptive section
################################################################################
local({
################################################################################
# RF105 reviewed fuel use, LPG outage, and plastic burning analyses
#
# Purpose:
#   Recreate the fuel-use summaries from the RF105A scripts using the final
#   cleaned household survey file, while documenting variable availability,
#   sample counts, and sensitivity analyses.
#
# Input:
#   4_data/clean_final/survey_refugee_household.rds
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/fuel_*.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/fuel_*.csv
#   6_figures/RF105_reviewed_YYYYMMDD/fuel_*.png
#
# Sensitivity analyses:
#   1. Complete three-survey household population versus all deduplicated
#      household-timepoint records.
#   2. Non-LPG cooking frequency exclusions at >60, >100, >200 days/month,
#      and no upper exclusion.
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

survey_data_raw <- readRDS(file_survey_refugee_household) %>%
  add_rf105_aliases()

analysis_population <- make_analysis_population(survey_data_raw, id_var = "fcn_id")

write_reviewed_csv(
  analysis_population$sample_counts,
  "table_descriptive_fuel_sample_counts.csv",
  subfolder = "qa"
)

################################################################################
# Fuel variable definitions
################################################################################

fuel_ever_vars <- c(
  "fuel_ever_lpg",
  "fuel_ever_wood",
  "fuel_ever_dung",
  "fuel_ever_sawdust",
  "fuel_ever_crop",
  "fuel_ever_gather_scraps",
  "fuel_ever_kero",
  "fuel_ever_charcoal",
  "fuel_ever_plastic",
  "fuel_ever_other"
)

fuel_30_vars <- c(
  "fuel_30_lpg",
  "fuel_30_wood",
  "fuel_30_dung",
  "fuel_30_sawdust",
  "fuel_30_crop",
  "fuel_30_gather_scraps",
  "fuel_30_kero",
  "fuel_30_charcoal",
  "fuel_30_plastic",
  "fuel_30_other"
)

fuel_30_line_vars <- c(
  "fuel_30_gather_scraps",
  "fuel_30_collect_wood",
  "fuel_30_buy_wood",
  "fuel_30_receive_wood",
  "fuel_30_buy_crh",
  "fuel_30_receive_crh",
  "fuel_30_buy_lpg",
  "fuel_30_receive_lpg",
  "fuel_30_other"
)

fuel_labels <- c(
  fuel_ever_lpg = "LPG",
  fuel_ever_wood = "Wood",
  fuel_ever_dung = "Animal dung",
  fuel_ever_sawdust = "Sawdust",
  fuel_ever_crop = "Crop residue",
  fuel_ever_gather_scraps = "Gathered scraps",
  fuel_ever_kero = "Kerosene",
  fuel_ever_charcoal = "Charcoal",
  fuel_ever_plastic = "Plastic",
  fuel_ever_other = "Other",
  fuel_30_lpg = "LPG",
  fuel_30_wood = "Wood",
  fuel_30_dung = "Animal dung",
  fuel_30_sawdust = "Sawdust",
  fuel_30_crop = "Crop residue",
  fuel_30_gather_scraps = "Gathered scraps",
  fuel_30_kero = "Kerosene",
  fuel_30_charcoal = "CRH/charcoal",
  fuel_30_plastic = "Plastic",
  fuel_30_other = "Other"
)

fuel_related_vars <- c(
  fuel_ever_vars,
  fuel_30_vars,
  fuel_30_line_vars,
  "lpg_runout",
  "lpg_runout_days",
  "fuel_use_non_lpg",
  "fuel_use_non_lpg_freq_cook",
  "plastic_cook",
  "fuel_cant_afford_2wk",
  "fuel_cant_afford"
)

write_reviewed_csv(
  flag_missing_vars(
    survey_data_raw,
    fuel_related_vars,
    "RF105 fuel-use variables after clean_final aliases"
  ),
  "table_descriptive_fuel_variable_availability.csv",
  subfolder = "qa"
)

summarise_fuel_use <- function(df, fuel_vars, recall_period, population_label) {
  fuel_vars_available <- fuel_vars[fuel_vars %in% names(df)]

  df %>%
    select(any_of(c("fcn_id", "timepoint", "study_arm_overall", fuel_vars_available))) %>%
    add_all_arms_rows() %>%
    pivot_longer(
      cols = all_of(fuel_vars_available),
      names_to = "fuel_variable",
      values_to = "used_raw",
      values_transform = list(used_raw = as.character)
    ) %>%
    mutate(
      used = make_yn(used_raw),
      fuel_type = recode(fuel_variable, !!!fuel_labels, .default = fuel_variable),
      recall_period = recall_period,
      population = population_label
    ) %>%
    group_by(population, recall_period, timepoint, study_arm_overall,
             fuel_variable, fuel_type) %>%
    summarise(
      n_nonmissing = sum(!is.na(used)),
      n_used = sum(used == 1, na.rm = TRUE),
      pct_used = 100 * n_used / n_nonmissing,
      .groups = "drop"
    ) %>%
    mutate(pct_used = if_else(is.nan(pct_used), NA_real_, pct_used)) %>%
    arrange(population, recall_period, fuel_type, timepoint, study_arm_overall)
}

survey_complete <- analysis_population$complete_3_survey
survey_all_dedup <- analysis_population$all_deduplicated

fuel_ever_complete <- summarise_fuel_use(
  survey_complete, fuel_ever_vars, "ever", "complete_3_survey"
)

fuel_30_complete <- summarise_fuel_use(
  survey_complete, fuel_30_vars, "past_30_days", "complete_3_survey"
)

fuel_30_all_dedup <- summarise_fuel_use(
  survey_all_dedup, fuel_30_vars, "past_30_days", "all_deduplicated_records"
)

fuel_summary <- bind_rows(
  fuel_ever_complete,
  fuel_30_complete,
  fuel_30_all_dedup
)

write_reviewed_csv(fuel_summary, "table_descriptive_fuel_use_summary.csv")

summarise_fuel_use_among_fuel_shortage <- function(df, fuel_vars, population_label) {
  if (!"fuel_cant_afford_2wk" %in% names(df)) {
    warning(
      "fuel_cant_afford_2wk is missing; skipping fuel-use table among fuel-shortage households.",
      call. = FALSE
    )
    return(tibble())
  }

  fuel_vars_available <- fuel_vars[fuel_vars %in% names(df)]
  if (length(fuel_vars_available) == 0) {
    return(tibble())
  }

  fuel_shortage_records <- df %>%
    mutate(fuel_shortage_2wk_yn = make_yn(fuel_cant_afford_2wk)) %>%
    filter(fuel_shortage_2wk_yn == 1)

  if (nrow(fuel_shortage_records) == 0) {
    return(tibble())
  }

  shortage_totals <- fuel_shortage_records %>%
    select(any_of(c("fcn_id", "timepoint", "study_arm_overall"))) %>%
    add_all_arms_rows() %>%
    count(timepoint, study_arm_overall, name = "n_fuel_shortage_households")

  fuel_shortage_records %>%
    select(any_of(c("fcn_id", "timepoint", "study_arm_overall", fuel_vars_available))) %>%
    add_all_arms_rows() %>%
    pivot_longer(
      cols = all_of(fuel_vars_available),
      names_to = "fuel_variable",
      values_to = "used_raw",
      values_transform = list(used_raw = as.character)
    ) %>%
    mutate(
      used = make_yn(used_raw),
      fuel_type = recode(fuel_variable, !!!fuel_labels, .default = fuel_variable),
      population = population_label,
      recall_period = "past_30_days",
      shortage_variable = "fuel_cant_afford_2wk",
      shortage_condition = "make_yn(fuel_cant_afford_2wk) == 1",
      denominator_type = "households reporting fuel shortage in prior 2 weeks with nonmissing fuel-use item"
    ) %>%
    group_by(population, recall_period, shortage_variable, shortage_condition,
             denominator_type, timepoint, study_arm_overall, fuel_variable,
             fuel_type) %>%
    summarise(
      n_nonmissing = sum(!is.na(used)),
      n_used = sum(used == 1, na.rm = TRUE),
      pct_used = if_else(n_nonmissing > 0, 100 * n_used / n_nonmissing, NA_real_),
      .groups = "drop"
    ) %>%
    left_join(shortage_totals, by = c("timepoint", "study_arm_overall")) %>%
    mutate(
      pct_used_all_fuel_shortage_households = if_else(
        n_fuel_shortage_households > 0,
        100 * n_used / n_fuel_shortage_households,
        NA_real_
      )
    ) %>%
    select(
      population, recall_period, shortage_variable, shortage_condition,
      denominator_type, timepoint, study_arm_overall, fuel_variable, fuel_type,
      n_fuel_shortage_households, n_nonmissing, n_used, pct_used,
      pct_used_all_fuel_shortage_households
    ) %>%
    arrange(population, recall_period, fuel_type, timepoint, study_arm_overall)
}

fuel_30_among_fuel_shortage <- summarise_fuel_use_among_fuel_shortage(
  survey_all_dedup,
  fuel_30_vars,
  "all_deduplicated_records_reporting_fuel_shortage_2wk"
)

write_reviewed_csv(
  fuel_30_among_fuel_shortage,
  "table_descriptive_fuel_use_past_30_days_among_fuel_shortage.csv"
)

################################################################################
# Fuel-use figure for complete three-survey households
################################################################################

fuel_line_var_table <- tibble(
  fuel_variable = fuel_30_line_vars,
  fuel = factor(
    c(
      "Scraps, gathered",
      "Wood, collected",
      "Wood, purchased",
      "Wood, received",
      "Compressed rice husks, purchased",
      "Compressed rice husks, received",
      "LPG, purchased",
      "LPG, received",
      "Plastic, collected"
    ),
    levels = c(
      "Scraps, gathered",
      "Wood, collected",
      "Wood, purchased",
      "Wood, received",
      "Compressed rice husks, purchased",
      "Compressed rice husks, received",
      "LPG, purchased",
      "LPG, received",
      "Plastic, collected"
    )
  ),
  fuel_method = factor(
    c("gathered", "collected", "purchased", "received", "purchased", "received", "purchased", "received", "collected"),
    levels = c("gathered", "collected", "purchased", "received")
  ),
  fuel_type = factor(
    c("Scraps", "Wood", "Wood", "Wood", "Compressed rice husks", "Compressed rice husks", "LPG", "LPG", "Plastic"),
    levels = c("Scraps", "Wood", "Compressed rice husks", "LPG", "Plastic")
  )
)

missing_fuel_line_vars <- setdiff(fuel_line_var_table$fuel_variable, names(survey_complete))
if (length(missing_fuel_line_vars) > 0) {
  stop(
    "Missing fuel variables needed for fig_descriptive_fuel_use_summary.png: ",
    paste(missing_fuel_line_vars, collapse = ", "),
    call. = FALSE
  )
}

fuel_plot_data <- survey_complete %>%
  select(fcn_id, study_arm_overall, timepoint, all_of(fuel_line_var_table$fuel_variable)) %>%
  pivot_longer(
    cols = all_of(fuel_line_var_table$fuel_variable),
    names_to = "fuel_variable",
    values_to = "used_raw",
    values_transform = list(used_raw = as.character)
  ) %>%
  mutate(
    used = make_yn(used_raw),
    used = if_else(is.na(used), 0L, used)
  ) %>%
  left_join(fuel_line_var_table, by = "fuel_variable") %>%
  filter(
    !is.na(fuel),
    study_arm_overall %in% arm_levels,
    timepoint %in% timepoint_levels
  ) %>%
  group_by(fuel_variable, fuel, fuel_method, fuel_type, study_arm_overall, timepoint) %>%
  summarise(
    n_households = n(),
    n_used = sum(used == 1, na.rm = TRUE),
    proportion = n_used / n_households,
    se = sqrt(proportion * (1 - proportion) / n_households),
    ci_lower = pmax(0, proportion - 1.96 * se),
    ci_upper = pmin(1, proportion + 1.96 * se),
    .groups = "drop"
  ) %>%
  mutate(
    study_arm_label = factor(
      as.character(study_arm_overall),
      levels = arm_levels,
      labels = c("Comparison group", "Intervention group")
    ),
    timepoint = factor(
      as.character(timepoint),
      levels = timepoint_levels,
      labels = timepoint_levels,
      ordered = TRUE
    ),
    denominator_note = paste(
      "Complete three-survey households; missing fuel indicators are treated",
      "as no-use to match the legacy RF105A Figure 2 calculation. The",
      "fuel_30_other variable is labelled as Plastic, collected following the",
      "legacy manuscript code."
    )
  ) %>%
  arrange(study_arm_label, fuel, timepoint)

write_reviewed_csv(
  fuel_plot_data,
  "table_descriptive_fuel_use_line_plot_data.csv"
)

fig_fuel_30 <- ggplot(
  fuel_plot_data,
  aes(
    x = timepoint,
    y = proportion,
    color = fuel,
    group = fuel
  )
) +
  geom_point(size = 1.8) +
  geom_line(linewidth = 0.7) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.15,
    linewidth = 0.4
  ) +
  facet_wrap(~ study_arm_label) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.1),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Types of Cooking Fuel Used in the Past 30 Days,\n By Study Arm and Timepoint",
    x = "Timepoint",
    y = "Percent of households using specified fuel type (%)",
    color = "Type of fuel used in past 30 days"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(ncol = 1))

save_reviewed_plot(fig_fuel_30, "fig_descriptive_fuel_use_summary.png",
                   width = 10, height = 6)

################################################################################
# LPG outage summaries
################################################################################

runout_vars <- c("lpg_runout", "lpg_runout_days")
runout_vars_available <- runout_vars[runout_vars %in% names(survey_complete)]

if (length(runout_vars_available) > 0) {
  lpg_runout_summary <- survey_complete %>%
    add_all_arms_rows() %>%
    mutate(
      lpg_runout_yn = if ("lpg_runout" %in% names(.)) make_yn(lpg_runout) else NA_integer_,
      lpg_runout_days_num = if ("lpg_runout_days" %in% names(.)) {
        suppressWarnings(as.numeric(lpg_runout_days))
      } else {
        NA_real_
      }
    ) %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise(
      n_households = n(),
      n_lpg_runout_nonmissing = sum(!is.na(lpg_runout_yn)),
      n_lpg_runout = sum(lpg_runout_yn == 1, na.rm = TRUE),
      pct_lpg_runout = 100 * n_lpg_runout / n_lpg_runout_nonmissing,
      mean_lpg_runout_days = mean(lpg_runout_days_num, na.rm = TRUE),
      median_lpg_runout_days = median(lpg_runout_days_num, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pct_lpg_runout = if_else(is.nan(pct_lpg_runout), NA_real_, pct_lpg_runout),
      mean_lpg_runout_days = if_else(
        is.nan(mean_lpg_runout_days), NA_real_, mean_lpg_runout_days
      )
    )

  write_reviewed_csv(lpg_runout_summary, "table_descriptive_lpg_runout_summary.csv")
}

################################################################################
# Plastic and non-LPG cooking sensitivity analyses
################################################################################

non_lpg_vars <- c(
  "fuel_use_non_lpg",
  "fuel_use_non_lpg_freq_cook",
  "plastic_cook",
  "fuel_cant_afford"
)

non_lpg_vars_available <- non_lpg_vars[non_lpg_vars %in% names(survey_complete)]

if (length(non_lpg_vars_available) > 0) {
  non_lpg_base <- survey_complete %>%
    mutate(
      non_lpg_yn = if ("fuel_use_non_lpg" %in% names(.)) {
        make_yn(fuel_use_non_lpg)
      } else {
        NA_integer_
      },
      plastic_cook_yn = if ("plastic_cook" %in% names(.)) {
        make_yn(plastic_cook)
      } else {
        NA_integer_
      },
      fuel_cant_afford_yn = if ("fuel_cant_afford" %in% names(.)) {
        make_yn(fuel_cant_afford)
      } else {
        NA_integer_
      },
      non_lpg_cook_days = if ("fuel_use_non_lpg_freq_cook" %in% names(.)) {
        suppressWarnings(as.numeric(fuel_use_non_lpg_freq_cook))
      } else {
        NA_real_
      }
    )

  threshold_grid <- tibble(
    exclusion_rule = c(
      "exclude_non_lpg_days_gt_60",
      "exclude_non_lpg_days_gt_100",
      "exclude_non_lpg_days_gt_200_main",
      "no_upper_exclusion"
    ),
    max_non_lpg_days = c(60, 100, 200, Inf)
  )

  non_lpg_sensitivity <- threshold_grid %>%
    mutate(data = map(max_non_lpg_days, function(max_days) {
      if (is.infinite(max_days)) {
        non_lpg_base
      } else {
        non_lpg_base %>%
          filter(is.na(non_lpg_cook_days) | non_lpg_cook_days <= max_days)
      }
    })) %>%
    select(exclusion_rule, max_non_lpg_days, data) %>%
    unnest(data) %>%
    add_all_arms_rows() %>%
    group_by(exclusion_rule, max_non_lpg_days, timepoint, study_arm_overall) %>%
    summarise(
      n_households = n(),
      n_non_lpg_nonmissing = sum(!is.na(non_lpg_yn)),
      n_non_lpg = sum(non_lpg_yn == 1, na.rm = TRUE),
      pct_non_lpg = 100 * n_non_lpg / n_non_lpg_nonmissing,
      n_plastic_nonmissing = sum(!is.na(plastic_cook_yn)),
      n_plastic = sum(plastic_cook_yn == 1, na.rm = TRUE),
      pct_plastic = 100 * n_plastic / n_plastic_nonmissing,
      n_cannot_afford_nonmissing = sum(!is.na(fuel_cant_afford_yn)),
      n_cannot_afford = sum(fuel_cant_afford_yn == 1, na.rm = TRUE),
      pct_cannot_afford = 100 * n_cannot_afford / n_cannot_afford_nonmissing,
      mean_non_lpg_cook_days = mean(non_lpg_cook_days, na.rm = TRUE),
      median_non_lpg_cook_days = median(non_lpg_cook_days, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(across(
      starts_with("pct_"),
      ~ if_else(is.nan(.x), NA_real_, .x)
    ))

  write_reviewed_csv(
    non_lpg_sensitivity,
    "table_descriptive_nonlpg_plastic_sensitivity.csv"
  )
}
})

################################################################################
# Shared Geocene outputs use the cleaned all-event household-date datasets.
source(file.path(project_root, "5_analysis_RF105", "reviewed", "geocene_analysis_helpers.R"))
geocene_run_analysis("100_80_5_20")
# Consolidated respiratory-health descriptive section
################################################################################
local({
################################################################################
# RF105 reviewed respiratory, physical health, mental health, and expenditure
# descriptive analyses
#
# Purpose:
#   Recreate the RF105B health outcome checks using the final cleaned household
#   survey file, without writing derived health data back into 4_data.
#
# Input:
#   4_data/clean_final/survey_refugee_household.rds
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/health_*.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/health_*.csv
#   6_figures/RF105_reviewed_YYYYMMDD/fig_descriptive_child_health_symptoms.png
#
# rDiD/XGBoost models:
#   Primary and secondary modeled results are produced by
#   4_rdid_xgboost_20260805_2213.R. This file is kept for descriptive QA summaries
#   and the child symptom figure.
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

survey_data_raw <- readRDS(file_survey_refugee_household) %>%
  add_rf105_aliases()

analysis_population <- make_analysis_population(survey_data_raw, id_var = "fcn_id")

write_reviewed_csv(
  analysis_population$sample_counts,
  "table_descriptive_health_sample_counts.csv",
  subfolder = "qa"
)

################################################################################
# Outcome derivations
################################################################################

derive_health_outcomes <- function(df) {
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
    "depressed",
    "sick",
    "mental_health_difficulty"
  )

  for (root in health_roots) {
    yn_name <- paste0(root, "_yn")
    if (root %in% names(df) && yn_name %notin% names(df)) {
      df[[yn_name]] <- make_yn(df[[root]])
    }
  }

  # Preserve the misspelled historical variable name used in the old RF105B
  # script, so downstream comparisons still line up with prior code.
  if ("target_child_distrubed_speech_yn" %in% names(df) &&
      "target_child_disturbed_speech_yn" %notin% names(df)) {
    df$target_child_disturbed_speech_yn <- df$target_child_distrubed_speech_yn
  }

  # Preserve true disturbed-speech missingness. Severe asthma is derived below
  # with an NA-preserving primary definition and a skip-as-no sensitivity.
  if ("target_child_wheezing_yn" %in% names(df)) {
    df <- df %>%
      mutate(
        target_child_asthma = case_when(
          target_child_wheezing_yn == 1 ~ 1L,
          target_child_wheezing_yn == 0 ~ 0L,
          TRUE ~ NA_integer_
        )
      )
  }

  df <- derive_child_severe_asthma_vars(df)
  if ("healthcare_visits_6mo" %in% names(df)) {
    df <- df %>%
      mutate(
        healthcare_visit_6mo_yn =
          as.integer(suppressWarnings(as.numeric(healthcare_visits_6mo)) > 0)
      )
  }

  df %>%
    mutate(
      exchange_rate = exchange_bdt_per_usd[as.character(timepoint)],
      medical_usd = if ("medical" %in% names(.)) {
        suppressWarnings(as.numeric(medical)) / exchange_rate
      } else {
        NA_real_
      },
      total_expenditures_30_usd = if ("total_expenditures_30" %in% names(.)) {
        suppressWarnings(as.numeric(total_expenditures_30)) / exchange_rate
      } else {
        NA_real_
      },
      spent_total_month_usd = if ("spent_total_month" %in% names(.)) {
        suppressWarnings(as.numeric(spent_total_month)) / exchange_rate
      } else {
        NA_real_
      }
    )
}

health_complete <- analysis_population$complete_3_survey %>%
  derive_health_outcomes()

health_all_dedup <- analysis_population$all_deduplicated %>%
  derive_health_outcomes()

################################################################################
# Outcome list aligned with the existing RF105B health script
################################################################################

resp_vars_impacted <- c(
  "target_child_clinic_resp_yn",
  "target_child_wheezing_yn",
  "target_child_distrubed_speech_yn",
  "respondent_disturbed_speech_yn",
  "target_child_eye_red_yn",
  "target_child_eye_itch_yn",
  "respondent_eye_red_yn",
  "respondent_eye_itch_yn",
  "respondent_eye_sore_yn",
  "target_child_asthma",
  "target_child_severe_asthma",
  "target_child_severe_asthma_skip_as_no",
  "target_child_cough_yn",
  "target_child_fever_yn",
  "target_child_resp_rate_yn"
)

prior_descriptive_health_outcomes <- c(
  "target_child_lethargy_yn",
  "target_child_weight_loss_yn",
  "respondent_cough_yn",
  "respondent_disturbed_sleep_yn",
  "respondent_headache_yn",
  "respondent_backache_yn"
)

additional_rf105b_outcomes <- c(
  "healthcare_visit_6mo_yn",
  "depressed_yn",
  "sick_yn",
  "mental_health_difficulty_yn",
  "medical_usd",
  "total_expenditures_30_usd",
  "spent_total_month_usd"
)

health_outcomes <- c(
  resp_vars_impacted,
  prior_descriptive_health_outcomes,
  additional_rf105b_outcomes
)

health_labels <- c(
  target_child_clinic_resp_yn = "Child clinic visit for respiratory complaint",
  target_child_wheezing_yn = "Child wheeze",
  target_child_distrubed_speech_yn = "Child disturbed speech",
  respondent_disturbed_speech_yn = "Respondent disturbed speech",
  target_child_eye_red_yn = "Child red eyes",
  target_child_eye_itch_yn = "Child itchy eyes",
  respondent_eye_red_yn = "Respondent red eyes",
  respondent_eye_itch_yn = "Respondent itchy eyes",
  respondent_eye_sore_yn = "Respondent sore eyes",
  target_child_asthma = "Child asthma proxy",
  target_child_severe_asthma = "Child severe asthma proxy (NA-preserving)",
  target_child_severe_asthma_skip_as_no = "Child severe asthma proxy (skip-as-no sensitivity)",
  target_child_cough_yn = "Child persistent cough",
  target_child_fever_yn = "Child fever",
  target_child_resp_rate_yn = "Child increased respiratory rate",
  target_child_lethargy_yn = "Child lethargy",
  target_child_weight_loss_yn = "Child unexplained weight loss",
  respondent_cough_yn = "Respondent cough",
  respondent_disturbed_sleep_yn = "Respondent disturbed sleep",
  respondent_headache_yn = "Respondent headache",
  respondent_backache_yn = "Respondent backache",
  healthcare_visit_6mo_yn = "Any healthcare visit in past 6 months",
  depressed_yn = "Respondent felt depressed",
  sick_yn = "Respondent felt sick",
  mental_health_difficulty_yn = "Mental health difficulty",
  medical_usd = "Medical expenditures, USD",
  total_expenditures_30_usd = "Total expenditures in past 30 days, USD",
  spent_total_month_usd = "Monthly expenditures, USD"
)

binary_health_outcomes <- setdiff(health_outcomes, c(
  "medical_usd",
  "total_expenditures_30_usd",
  "spent_total_month_usd"
))

health_variable_availability <- flag_missing_vars(
  health_complete,
  health_outcomes,
  "RF105B reviewed health and expenditure outcomes"
)

write_reviewed_csv(
  health_variable_availability,
  "table_descriptive_health_variable_availability.csv",
  subfolder = "qa"
)

health_outcomes_available <- health_variable_availability %>%
  filter(available) %>%
  pull(variable)

################################################################################
# Prevalence/mean summaries by timepoint and arm
################################################################################

summarise_health_outcomes <- function(df, population_label) {
  df %>%
    select(fcn_id, timepoint, study_arm_overall, all_of(health_outcomes_available)) %>%
    pivot_longer(
      cols = all_of(health_outcomes_available),
      names_to = "outcome",
      values_to = "value"
    ) %>%
    mutate(
      value = suppressWarnings(as.numeric(value)),
      outcome_label = recode(outcome, !!!health_labels, .default = outcome),
      outcome_type = if_else(
        outcome %in% binary_health_outcomes,
        "binary_percent",
        "continuous_mean"
      ),
      population = population_label
    ) %>%
    add_all_arms_rows() %>%
    group_by(population, outcome, outcome_label, outcome_type,
             timepoint, study_arm_overall) %>%
    summarise(
      n_nonmissing = sum(!is.na(value)),
      mean_or_prevalence = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      mean_or_prevalence = if_else(
        outcome_type == "binary_percent",
        100 * mean_or_prevalence,
        mean_or_prevalence
      ),
      sd = if_else(is.nan(sd), NA_real_, sd)
    )
}

health_summary <- bind_rows(
  summarise_health_outcomes(health_complete, "complete_3_survey"),
  summarise_health_outcomes(health_all_dedup, "all_deduplicated_records")
)

write_reviewed_csv(
  health_summary,
  "table_descriptive_health_outcome_summary.csv"
)

################################################################################
# Child physical-health rDiD sample-size diagnostic embedded from
# 6.1_child_physical_health_sample_size_diagnostic_20260807_0828.R
################################################################################

# This diagnostic explains why target-child physical-health rDiD panels can have
# different complete-case sample sizes by outcome. It uses the same deduplicated
# household-timepoint data and health derivations as the descriptive health
# summaries above, then writes aggregate QA and restricted fcn_id-level QA.
as_number_diag <- function(x) {
  suppressWarnings(as.numeric(x))
}

first_nonmissing_diag <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  x[[1]]
}

make_yn_diag <- function(x) {
  x_chr <- str_squish(str_to_lower(as.character(x)))
  x_num <- as_number_diag(x_chr)

  case_when(
    is.na(x) ~ NA_integer_,
    !is.na(x_num) & x_num %in% c(77, 88, 99) ~ NA_integer_,
    !is.na(x_num) ~ as.integer(x_num > 0),
    x_chr %in% c("yes", "y", "true", "present") ~ 1L,
    x_chr %in% c("no", "n", "false", "absent") ~ 0L,
    TRUE ~ NA_integer_
  )
}

coalesce_character_columns_diag <- function(df, vars) {
  vars <- vars[vars %in% names(df)]
  if (length(vars) == 0) return(rep(NA_character_, nrow(df)))
  values <- lapply(vars, function(var) as.character(df[[var]]))
  out <- values[[1]]
  if (length(values) > 1) {
    for (i in seq_along(values)[-1]) {
      out <- dplyr::coalesce(out, values[[i]])
    }
  }
  out
}

child_physical_diag_outcomes <- tribble(
  ~raw_variable, ~outcome, ~outcome_label,
  "target_child_cough", "target_child_cough_yn", "Child cough",
  "target_child_resp_rate", "target_child_resp_rate_yn", "Child increased respiratory rate today",
  "target_child_wheezing", "target_child_wheezing_yn", "Child current wheeze",
  "target_child_eye_red", "target_child_eye_red_yn", "Child red eyes",
  "target_child_eye_itch", "target_child_eye_itch_yn", "Child itchy eyes",
  "target_child_lethargy", "target_child_lethargy_yn", "Child lethargy",
  "target_child_weight_loss", "target_child_weight_loss_yn", "Child unexplained weight loss in 3 months",
  "target_child_fever", "target_child_fever_yn", "Child fever",
  "target_child_clinic_resp", "target_child_clinic_resp_yn", "Child clinic visit for respiratory complaint"
)

respondent_physical_diag_outcomes <- tribble(
  ~raw_variable, ~outcome, ~outcome_label,
  "respondent_cough", "respondent_cough_yn", "Caregiver cough",
  "respondent_resp_rate_reported_combined", "respondent_resp_rate_yn", "Caregiver increased respiratory rate today",
  "respondent_wheezing", "respondent_wheezing_yn", "Caregiver current wheeze",
  "respondent_eye_red", "respondent_eye_red_yn", "Caregiver red eyes",
  "respondent_eye_itch", "respondent_eye_itch_yn", "Caregiver itchy eyes",
  "respondent_eye_sore", "respondent_eye_sore_yn", "Caregiver sore eyes",
  "respondent_weight_loss_reported_combined", "respondent_weight_loss_yn", "Caregiver unexplained weight loss in 3 months",
  "respondent_headache", "respondent_headache_yn", "Caregiver headache",
  "respondent_backache", "respondent_backache_yn", "Caregiver backache"
)

expected_child_health_contrast_counts <- tribble(
  ~contrast, ~followup_timepoint, ~study_arm_overall, ~expected_n_households,
  "primary_baseline_midline", "midline", "intervention", 558L,
  "primary_baseline_midline", "midline", "comparison", 575L,
  "secondary_baseline_endline", "endline", "intervention", 496L,
  "secondary_baseline_endline", "endline", "comparison", 446L
) %>%
  mutate(
    followup_timepoint = as_ordered_timepoint(followup_timepoint),
    study_arm_overall = factor(study_arm_overall, levels = arm_levels)
  )

survey_model_data_diag <- health_all_dedup %>%
  mutate(
    respondent_resp_rate_reported_combined = coalesce_character_columns_diag(
      ., c("resp_rate_reported_respondent", "resp_rate_reported_respondant")
    ),
    respondent_weight_loss_reported_combined = coalesce_character_columns_diag(
      ., c("weight_loss_reported_respondent", "weight_loss_reported_respondant")
    )
  )

all_physical_diag_outcomes <- bind_rows(
  child_physical_diag_outcomes %>% mutate(respondent_group = "target_child"),
  respondent_physical_diag_outcomes %>% mutate(respondent_group = "respondent")
)

for (i in seq_len(nrow(all_physical_diag_outcomes))) {
  raw_var <- all_physical_diag_outcomes$raw_variable[[i]]
  outcome_var <- all_physical_diag_outcomes$outcome[[i]]
  if (raw_var %in% names(survey_model_data_diag)) {
    survey_model_data_diag[[outcome_var]] <- make_yn_diag(survey_model_data_diag[[raw_var]])
  } else {
    survey_model_data_diag[[outcome_var]] <- NA_integer_
  }
}

baseline_covars_diag <- survey_model_data_diag %>%
  filter(timepoint == "baseline") %>%
  transmute(
    fcn_id = as.character(fcn_id),
    study_arm_overall,
    A = if_else(study_arm_overall == "intervention", 1, 0, missing = NA_real_),
    hh_size = as_number_diag(hh_size),
    hh_per_structure = as_number_diag(hh_per_structure)
  ) %>%
  group_by(fcn_id) %>%
  summarise(
    study_arm_overall = first_nonmissing_diag(study_arm_overall),
    A = as_number_diag(first_nonmissing_diag(A)),
    hh_size = as_number_diag(first_nonmissing_diag(hh_size)),
    hh_per_structure = as_number_diag(first_nonmissing_diag(hh_per_structure)),
    .groups = "drop"
  ) %>%
  mutate(study_arm_overall = factor(as.character(study_arm_overall), levels = arm_levels))

make_contrast_households_diag <- function(followup_timepoint, contrast) {
  followup_ids <- survey_model_data_diag %>%
    filter(timepoint == followup_timepoint) %>%
    distinct(fcn_id = as.character(fcn_id))

  baseline_covars_diag %>%
    inner_join(followup_ids, by = "fcn_id") %>%
    filter(!is.na(A), !is.na(study_arm_overall)) %>%
    mutate(
      contrast = contrast,
      followup_timepoint = as.character(followup_timepoint)
    )
}

contrast_households_diag <- bind_rows(
  make_contrast_households_diag("midline", "primary_baseline_midline"),
  make_contrast_households_diag("endline", "secondary_baseline_endline")
)

expected_panel_counts_diag <- contrast_households_diag %>%
  count(contrast, followup_timepoint, study_arm_overall, name = "observed_paired_households") %>%
  full_join(expected_child_health_contrast_counts,
            by = c("contrast", "followup_timepoint", "study_arm_overall")) %>%
  mutate(
    observed_paired_households = replace_na(observed_paired_households, 0L),
    expected_n_households = replace_na(expected_n_households, NA_integer_),
    difference_observed_minus_expected = observed_paired_households - expected_n_households,
    matches_expected = observed_paired_households == expected_n_households
  ) %>%
  arrange(contrast, study_arm_overall)

make_outcome_missingness_diag <- function(outcome_row, followup_timepoint, contrast) {
  baseline_y <- survey_model_data_diag %>%
    filter(timepoint == "baseline") %>%
    transmute(
      fcn_id = as.character(fcn_id),
      baseline_value = as_number_diag(.data[[outcome_row$outcome]]),
      baseline_raw_value = if (outcome_row$raw_variable %in% names(.)) as.character(.data[[outcome_row$raw_variable]]) else NA_character_
    )

  followup_y <- survey_model_data_diag %>%
    filter(timepoint == followup_timepoint) %>%
    transmute(
      fcn_id = as.character(fcn_id),
      followup_value = as_number_diag(.data[[outcome_row$outcome]]),
      followup_raw_value = if (outcome_row$raw_variable %in% names(.)) as.character(.data[[outcome_row$raw_variable]]) else NA_character_
    )

  contrast_households_diag %>%
    filter(.data$contrast == contrast) %>%
    left_join(baseline_y, by = "fcn_id") %>%
    left_join(followup_y, by = "fcn_id") %>%
    mutate(
      outcome = outcome_row$outcome,
      raw_variable = outcome_row$raw_variable,
      outcome_label = outcome_row$outcome_label,
      baseline_missing = is.na(baseline_value),
      followup_missing = is.na(followup_value),
      complete_panel = !baseline_missing & !followup_missing,
      missing_pattern = case_when(
        !baseline_missing & !followup_missing ~ "complete_baseline_and_followup",
        baseline_missing & !followup_missing ~ "missing_baseline_only",
        !baseline_missing & followup_missing ~ "missing_followup_only",
        baseline_missing & followup_missing ~ "missing_both_timepoints",
        TRUE ~ "unclassified"
      )
    )
}

outcome_household_diagnostics_diag <- bind_rows(
  map_dfr(seq_len(nrow(child_physical_diag_outcomes)), function(i) {
    make_outcome_missingness_diag(child_physical_diag_outcomes[i, ], "midline", "primary_baseline_midline")
  }),
  map_dfr(seq_len(nrow(child_physical_diag_outcomes)), function(i) {
    make_outcome_missingness_diag(child_physical_diag_outcomes[i, ], "endline", "secondary_baseline_endline")
  })
) %>%
  mutate(respondent_group = "target_child")

respondent_household_diagnostics_diag <- bind_rows(
  map_dfr(seq_len(nrow(respondent_physical_diag_outcomes)), function(i) {
    make_outcome_missingness_diag(respondent_physical_diag_outcomes[i, ], "midline", "primary_baseline_midline")
  }),
  map_dfr(seq_len(nrow(respondent_physical_diag_outcomes)), function(i) {
    make_outcome_missingness_diag(respondent_physical_diag_outcomes[i, ], "endline", "secondary_baseline_endline")
  })
) %>%
  mutate(respondent_group = "respondent")

outcome_missingness_summary_diag <- outcome_household_diagnostics_diag %>%
  group_by(contrast, followup_timepoint, study_arm_overall, outcome, raw_variable, outcome_label) %>%
  summarise(
    expected_or_paired_households = n_distinct(fcn_id),
    n_complete_panel = sum(complete_panel, na.rm = TRUE),
    n_missing_baseline_only = sum(missing_pattern == "missing_baseline_only", na.rm = TRUE),
    n_missing_followup_only = sum(missing_pattern == "missing_followup_only", na.rm = TRUE),
    n_missing_both_timepoints = sum(missing_pattern == "missing_both_timepoints", na.rm = TRUE),
    n_missing_baseline_any = sum(baseline_missing, na.rm = TRUE),
    n_missing_followup_any = sum(followup_missing, na.rm = TRUE),
    n_dropped_from_expected = expected_or_paired_households - n_complete_panel,
    pct_complete_panel = 100 * n_complete_panel / expected_or_paired_households,
    .groups = "drop"
  ) %>%
  arrange(contrast, study_arm_overall, outcome)

inconsistency_summary_diag <- outcome_missingness_summary_diag %>%
  group_by(contrast, study_arm_overall) %>%
  summarise(
    n_outcomes = n_distinct(outcome),
    min_complete_panel = min(n_complete_panel, na.rm = TRUE),
    max_complete_panel = max(n_complete_panel, na.rm = TRUE),
    range_complete_panel = max_complete_panel - min_complete_panel,
    max_missing_baseline_any = max(n_missing_baseline_any, na.rm = TRUE),
    max_missing_followup_any = max(n_missing_followup_any, na.rm = TRUE),
    outcomes_with_min_complete_panel = paste(outcome_label[n_complete_panel == min_complete_panel], collapse = "; "),
    outcomes_with_max_complete_panel = paste(outcome_label[n_complete_panel == max_complete_panel], collapse = "; "),
    likely_explanation = case_when(
      range_complete_panel == 0 ~ "All target-child physical-health outcomes have the same complete-case panel size.",
      max_missing_baseline_any > 0 & max_missing_followup_any > 0 ~ "Outcome-specific missing/refused/don't-know values occur at both baseline and follow-up.",
      max_missing_baseline_any > 0 ~ "Outcome-specific missing/refused/don't-know values occur at baseline.",
      max_missing_followup_any > 0 ~ "Outcome-specific missing/refused/don't-know values occur at follow-up.",
      TRUE ~ "Complete-case panel sizes differ for a reason not captured by baseline/follow-up outcome missingness."
    ),
    .groups = "drop"
  ) %>%
  left_join(expected_panel_counts_diag, by = c("contrast", "study_arm_overall")) %>%
  arrange(contrast, study_arm_overall)

raw_value_codes_diag <- bind_rows(lapply(child_physical_diag_outcomes$raw_variable, function(raw_var) {
  if (!raw_var %in% names(survey_model_data_diag)) {
    return(tibble(
      raw_variable = raw_var,
      timepoint = factor(character(), levels = timepoint_levels, ordered = TRUE),
      study_arm_overall = factor(character(), levels = arm_levels),
      raw_value = character(),
      n = integer()
    ))
  }
  survey_model_data_diag %>%
    count(timepoint, study_arm_overall, raw_value = as.character(.data[[raw_var]]), name = "n") %>%
    mutate(raw_variable = raw_var, .before = timepoint)
})) %>%
  left_join(child_physical_diag_outcomes, by = "raw_variable") %>%
  arrange(raw_variable, timepoint, study_arm_overall, desc(n), raw_value)

respondent_missingness_summary_diag <- respondent_household_diagnostics_diag %>%
  group_by(contrast, followup_timepoint, study_arm_overall, outcome, raw_variable, outcome_label) %>%
  summarise(
    expected_or_paired_households = n_distinct(fcn_id),
    n_complete_panel = sum(complete_panel, na.rm = TRUE),
    n_missing_baseline_any = sum(baseline_missing, na.rm = TRUE),
    n_missing_followup_any = sum(followup_missing, na.rm = TRUE),
    n_dropped_from_expected = expected_or_paired_households - n_complete_panel,
    .groups = "drop"
  )

child_vs_respondent_health_counts_diag <- bind_rows(
  outcome_missingness_summary_diag %>%
    mutate(respondent_group = "target_child") %>%
    select(respondent_group, contrast, followup_timepoint, study_arm_overall,
           outcome, raw_variable, outcome_label, expected_or_paired_households,
           n_complete_panel, n_missing_baseline_any, n_missing_followup_any,
           n_dropped_from_expected),
  respondent_missingness_summary_diag %>%
    mutate(respondent_group = "respondent") %>%
    select(respondent_group, contrast, followup_timepoint, study_arm_overall,
           outcome, raw_variable, outcome_label, expected_or_paired_households,
           n_complete_panel, n_missing_baseline_any, n_missing_followup_any,
           n_dropped_from_expected)
) %>%
  arrange(contrast, study_arm_overall, respondent_group, outcome)

household_any_child_missing_diag <- outcome_household_diagnostics_diag %>%
  group_by(contrast, followup_timepoint, study_arm_overall, fcn_id) %>%
  summarise(
    n_child_outcomes_missing = sum(!complete_panel, na.rm = TRUE),
    child_outcomes_missing = paste(outcome_label[!complete_panel], collapse = "; "),
    .groups = "drop"
  )

household_any_respondent_missing_diag <- respondent_household_diagnostics_diag %>%
  group_by(contrast, followup_timepoint, study_arm_overall, fcn_id) %>%
  summarise(
    n_respondent_outcomes_missing = sum(!complete_panel, na.rm = TRUE),
    respondent_outcomes_missing = paste(outcome_label[!complete_panel], collapse = "; "),
    .groups = "drop"
  )

restricted_child_vs_respondent_discrepancies_diag <- household_any_child_missing_diag %>%
  left_join(
    household_any_respondent_missing_diag,
    by = c("contrast", "followup_timepoint", "study_arm_overall", "fcn_id")
  ) %>%
  filter(n_child_outcomes_missing > 0 | n_respondent_outcomes_missing > 0) %>%
  mutate(
    discrepancy_type = case_when(
      n_child_outcomes_missing > 0 & replace_na(n_respondent_outcomes_missing, 0L) == 0L ~
        "child_missing_respondent_complete",
      n_child_outcomes_missing == 0 & n_respondent_outcomes_missing > 0 ~
        "respondent_missing_child_complete",
      n_child_outcomes_missing > 0 & n_respondent_outcomes_missing > 0 ~
        "both_child_and_respondent_missing",
      TRUE ~ "none"
    )
  ) %>%
  arrange(contrast, study_arm_overall, discrepancy_type, fcn_id)

restricted_missing_households_diag <- outcome_household_diagnostics_diag %>%
  filter(!complete_panel) %>%
  select(
    contrast, followup_timepoint, study_arm_overall, fcn_id,
    outcome, raw_variable, outcome_label,
    missing_pattern, baseline_missing, followup_missing,
    baseline_value, followup_value, baseline_raw_value, followup_raw_value,
    hh_size, hh_per_structure
  ) %>%
  arrange(contrast, study_arm_overall, outcome, missing_pattern, fcn_id)

write_reviewed_csv(
  expected_panel_counts_diag,
  "table_qa_child_health_expected_panel_counts.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  outcome_missingness_summary_diag,
  "table_qa_child_health_outcome_missingness.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  raw_value_codes_diag,
  "table_qa_child_health_raw_value_codes.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  inconsistency_summary_diag,
  "table_qa_child_health_inconsistency_summary.csv",
  subfolder = "qa"
)
write_reviewed_csv(
  child_vs_respondent_health_counts_diag,
  "table_qa_child_vs_respondent_health_counts.csv",
  subfolder = "qa"
)
write_restricted_qa_csv(
  restricted_child_vs_respondent_discrepancies_diag,
  "restricted_qa_child_vs_respondent_health_discrepancies.csv",
  reason = paste(
    "Contains fcn_id-level comparison of target-child and respondent",
    "physical-health complete-case status by contrast."
  )
)
write_restricted_qa_csv(
  restricted_missing_households_diag,
  "restricted_qa_child_health_missing_households.csv",
  reason = paste(
    "Contains fcn_id-level records showing which households are dropped from",
    "target-child physical-health rDiD panels because baseline and/or follow-up",
    "outcome values are missing."
  )
)

message("Child physical-health sample-size diagnostic complete.")
################################################################################
# Modeled rDiD/XGBoost health results are run in 4_rdid_xgboost_20260805_2213.R.

################################################################################
# Child symptom figure for manuscript QA
################################################################################

child_figure_outcomes <- intersect(
  c(
    "target_child_cough_yn",
    "target_child_resp_rate_yn",
    "target_child_wheezing_yn",
    "target_child_eye_red_yn",
    "target_child_eye_itch_yn",
    "target_child_lethargy_yn",
    "target_child_weight_loss_yn",
    "target_child_fever_yn",
    "target_child_clinic_resp_yn"
  ),
  health_outcomes_available
)

if (length(child_figure_outcomes) > 0) {
  child_plot_data <- health_summary %>%
    filter(
      population == "complete_3_survey",
      outcome %in% child_figure_outcomes,
      outcome_type == "binary_percent"
    ) %>%
    mutate(
      outcome_label = factor(
        outcome_label,
        levels = health_labels[child_figure_outcomes]
      )
    )

  fig_child_health <- ggplot(
    child_plot_data,
    aes(x = timepoint, y = mean_or_prevalence, fill = study_arm_overall)
  ) +
    geom_col(position = position_dodge(width = 0.75), width = 0.68) +
    facet_wrap(~ outcome_label, ncol = 4) +
    scale_y_continuous(labels = label_number(suffix = "%")) +
    scale_fill_manual(
      values = c(comparison = "#4E79A7", intervention = "#F28E2B"),
      na.translate = FALSE
    ) +
    labs(
      x = NULL,
      y = "Households reporting symptom",
      fill = "Study arm"
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )

  save_reviewed_plot(
    fig_child_health,
    "fig_descriptive_child_health_symptoms.png",
    width = 10,
    height = 5.5
  )
}
})

################################################################################
# Consolidated from the supplemental descriptive section in this script
################################################################################
local({
################################################################################
# RF105 reviewed supplemental supplemental survey outcomes
#
# Purpose:
#   Add aggregate descriptive outputs for outcome families that appeared in the
#   requested RF105 survey outcome inventory but were not
#   already represented in the reviewed RF105 scripts.
#
# Inputs:
#   4_data/clean_final/survey_refugee_household.rds
#   4_data/clean_final/survey_refugee_hh_members.rds
#   4_data/clean_final/survey_refugee_symptoms.rds
#
# Outputs:
#   Tables:  7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_supplemental_*.csv
#   Figures: 6_figures/RF105_reviewed_YYYYMMDD/fig_descriptive_supplemental_*.png
#   QA:      7_tables/RF105_reviewed_YYYYMMDD/qa/table_descriptive_supplemental_*.csv
#
# Notes:
#   - This file is descriptive only. Old DiD-style outcomes that are comparable
#     over time are estimated in 4_rdid_xgboost_20260805_2213.R using rDiD/XGBoost.
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
config_file <- file.path(script_dir, "0_RF105_config_20260805_2213.R")
if (!file.exists(config_file)) {
  config_file <- file.path("5_analysis_RF105", "reviewed", "0_RF105_config_20260805_2213.R")
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
      add_all_arms_rows() %>%
      group_by(timepoint, study_arm_overall) %>%
      summarise(
        n_total = n(),
        n_nonmissing = sum(!is.na(value)),
        n_yes = sum(value == 1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      add_binary_denominator_summaries(numerator_col = "n_yes") %>%
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
           n_total, n_nonmissing, n_yes, percent, ci_lower, ci_upper,
           percent_nonmissing, ci_lower_nonmissing, ci_upper_nonmissing,
           percent_total, ci_lower_total, ci_upper_total) %>%
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
      add_all_arms_rows() %>%
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
      add_all_arms_rows() %>%
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

arm_colors <- c(comparison = "#3B6EA8", intervention = "#C94C4C", all_arms = "#6F6F6F")

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
  "implemented_in_current_descriptive_script", "sleep", "table_descriptive_supplemental_binary_outcomes.csv",
  labels = c("Bad dreams")
)

muac_binary <- make_var_table(
  c("target_child_arm_measurements_yn", "target_child_muac_lt145",
    "target_child_muac_below_fiorentino", "target_child_muac_who_mam_or_sam",
    "target_child_muac_who_sam"),
  "implemented_in_current_descriptive_script", "child_muac", "table_descriptive_supplemental_binary_outcomes.csv",
  labels = c("Child MUAC measurements available", "Average MUAC <145 mm",
             "MUAC below Fiorentino sex/age cutoff", "MUAC <125 mm", "MUAC <115 mm")
)

ventilation_binary <- make_var_table(
  c("window_number_zero", "window_kitchen", "window_kitchen_use"),
  "implemented_in_current_descriptive_script", "ventilation", "table_descriptive_supplemental_binary_outcomes.csv",
  labels = c("No windows reported", "Kitchen has a window", "Kitchen window is used")
)

who_cooks <- make_var_table(
  c("cook_who_w", "cook_who_g", "cook_who_m", "cook_who_b"),
  "implemented_in_current_descriptive_script", "who_cooks", "table_descriptive_supplemental_binary_outcomes.csv",
  labels = c("Women cook", "Girls cook", "Men cook", "Boys cook")
)

stove_use <- make_var_table(
  c("stove_boil_drink", "stove_boil_bathe", "stove_reason_stay_warm",
    "stove_reason_cook_together", "stove_reason_sell_food"),
  "implemented_in_current_descriptive_script", "stove_use", "table_descriptive_supplemental_binary_outcomes.csv",
  labels = c("Boils drinking water", "Boils bathing water", "Uses stove to stay warm",
             "Uses stove to cook for/with others", "Uses stove to cook food to sell")
)

forest_use <- make_var_table(
  c("gather_wood", "forest_collect_not_wood", "reason_forest_food",
    "reason_forest_med", "reason_forest_shelter", "reason_forest_privacy",
    "reason_forest_defacation", "reason_forest_leisure", "reason_forest_other"),
  "implemented_in_current_descriptive_script", "forest_use", "table_descriptive_supplemental_binary_outcomes.csv",
  labels = c("Collected firewood for non-household cooking", "Collected non-wood forest products",
             "Forest reason: food", "Forest reason: medicine", "Forest reason: shelter material",
             "Forest reason: privacy", "Forest reason: defecation", "Forest reason: leisure",
             "Forest reason: other")
)

plastic_use <- make_var_table(
  c("plastic_burn_any_yn", "plastic_burn_gt1_yn", "burn_plastic_types/1",
    "burn_plastic_types/2", "burn_plastic_types/3", "burn_plastic_types/66",
    "burn_plastic_reason/1", "burn_plastic_reason/2", "burn_plastic_reason/3"),
  "implemented_in_current_descriptive_script", "plastic_burning", "table_descriptive_supplemental_binary_outcomes.csv",
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
  "implemented_in_current_descriptive_script; implemented_in_current_descriptive_script",
  "programmatic_credit_debt", "table_descriptive_supplemental_binary_outcomes.csv"
)

repair <- make_var_table(
  c(paste0("lpg_stove_repair/", c(0:5, 66)),
    paste0("lpg_cylinder_repair/", c(0:6, 66)),
    paste0("lpg_repair_details/", 1:5)),
  "implemented_in_current_descriptive_script", "lpg_repairs", "table_descriptive_supplemental_binary_outcomes.csv"
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
  source_script = "implemented_in_current_descriptive_script",
  outcome_family = "fuel_procurement_who",
  output_file = "table_descriptive_supplemental_fuel_procurement.csv"
)

household_binary_vars <- bind_rows(
  sleep_binary, muac_binary, ventilation_binary, who_cooks, stove_use,
  forest_use, plastic_use, programmatic, repair, fuel_person
)

household_continuous_vars <- tribble(
  ~source_variable, ~unit, ~source_script,
  "sleep_hours", "hours", "implemented_in_current_descriptive_script",
  "sleep_fall_asleep_min", "minutes", "implemented_in_current_descriptive_script",
  "target_child_mid_arm_circ_av_clean", "mm", "implemented_in_current_descriptive_script",
  "window_number", "count", "implemented_in_current_descriptive_script",
  "traditional_use_yesterday", "uses", "implemented_in_current_descriptive_script",
  "LPG_use_yesterday", "uses", "implemented_in_current_descriptive_script",
  "boil_yesterday_times", "times", "implemented_in_current_descriptive_script",
  "cook_sell_days_week", "days per week", "implemented_in_current_descriptive_script",
  "cook_to_sell_percent", "percent category", "implemented_in_current_descriptive_script",
  "forest_wood_fee", "BDT", "implemented_in_current_descriptive_script; implemented_in_current_descriptive_script",
  "cost_forest_not_wood", "BDT", "implemented_in_current_descriptive_script",
  "buy_wood_cost_bundle", "BDT", "implemented_in_current_descriptive_script",
  "burn_plastic_frequency", "times per week", "implemented_in_current_descriptive_script",
  "receive_lpg_walk", "hours", "implemented_in_current_descriptive_script",
  "receive_lpg_wait", "hours", "implemented_in_current_descriptive_script",
  "buy_lpg_walk", "hours", "implemented_in_current_descriptive_script",
  "buy_lpg_wait", "hours", "implemented_in_current_descriptive_script",
  "receive_crh_walk", "hours", "implemented_in_current_descriptive_script",
  "receive_crh_wait", "hours", "implemented_in_current_descriptive_script",
  "receive_wood_wait", "hours", "implemented_in_current_descriptive_script",
  "lpg_repair_costs", "BDT", "implemented_in_current_descriptive_script",
  "lpg_willingness_to_pay", "BDT", "implemented_in_current_descriptive_script",
  "total_income_30_usd", "USD", "implemented_in_current_descriptive_script; implemented_in_current_descriptive_script",
  "spent_total_month_usd", "USD", "implemented_in_current_descriptive_script; implemented_in_current_descriptive_script",
  "spent_food_pct", "proportion", "implemented_in_current_descriptive_script",
  "debt_total_usd", "USD", "implemented_in_current_descriptive_script"
) %>%
  mutate(
    outcome_name = source_variable,
    outcome_label = pretty_label(source_variable),
    outcome_family = "supplemental_survey_household_continuous",
    reviewed_output = "table_descriptive_supplemental_continuous_outcomes.csv",
    note = ""
  )

household_categorical_vars <- make_var_table(
  c("sleep_quality", "sleep_bad_dreams", "window_door_wall", "gather_wood_dead",
    "gather_wood_reason", "buy_wood_reason", "refill_time_lpg_missed",
    "lpg_extra_use", "lpg_afraid_why", "fire_why", "fire_consequence",
    "lpg_repair_details", "reason_forest_other_specified"),
  "implemented_in_current_descriptive_script",
  "supplemental_survey_household_categorical",
  "table_descriptive_supplemental_categorical_outcomes.csv"
)

household_binary_summary <- summarise_binary_vars(survey, household_binary_vars)
write_reviewed_csv(
  household_binary_summary,
  "table_descriptive_supplemental_binary_outcomes.csv"
)

household_continuous_summary <- summarise_continuous_vars(survey, household_continuous_vars)
write_reviewed_csv(
  household_continuous_summary,
  "table_descriptive_supplemental_continuous_outcomes.csv"
)

household_categorical_summary <- summarise_categorical_vars(survey, household_categorical_vars)
write_reviewed_csv(
  household_categorical_summary,
  "table_descriptive_supplemental_categorical_outcomes.csv"
)

fuel_procurement_who_summary <- household_binary_summary %>%
  filter(outcome_group == "fuel_procurement_who")
write_reviewed_csv(
  fuel_procurement_who_summary,
  "table_descriptive_supplemental_fuel_procurement.csv"
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
                   "fig_descriptive_supplemental_binary_outcomes.png",
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
                   "fig_descriptive_supplemental_continuous_outcomes.png",
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
  mutate(
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = c(arm_levels, "all_arms", "missing_study_arm"))
  ) %>%
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
  add_all_arms_rows() %>%
  count(timepoint, study_arm_overall, name = "n_member_rows") %>%
  arrange(timepoint, study_arm_overall)
write_reviewed_csv(
  member_timepoint_availability,
  "table_descriptive_supplemental_timepoint_availability.csv",
  subfolder = "qa"
)

member_demographics_summary <- members %>%
  add_all_arms_rows() %>%
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
  "table_descriptive_supplemental_hours_outside.csv"
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
      add_all_arms_rows() %>%
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
        source_script = "2_Rohingya_individual_survey_analysis/implemented_in_current_descriptive_script"
      )
  }) %>%
  arrange(outcome_name, timepoint, study_arm_overall, age_group, sex_label)
write_reviewed_csv(
  location_summary,
  "table_descriptive_supplemental_locations_visited.csv"
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
                   "fig_descriptive_member_locations_visited.png",
                   width = 14, height = 10)

symptoms <- readr::read_rds(file_survey_refugee_symptoms) %>%
  mutate(
    timepoint = as_ordered_timepoint(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = c(arm_levels, "all_arms", "missing_study_arm"))
  ) %>%
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
      add_all_arms_rows() %>%
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
        source_script = "2_Rohingya_individual_survey_analysis/implemented_in_current_descriptive_script"
      )
  }) %>%
  arrange(outcome_name, timepoint, study_arm_overall, age_group, sex_label)
write_reviewed_csv(
  symptom_summary,
  "table_descriptive_supplemental_covid_symptoms.csv"
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
                   "fig_descriptive_member_covid_symptoms.png",
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
  "implemented_in_current_descriptive_script", "household_binary", "reason_forest_defecation", "table_descriptive_supplemental_binary_outcomes.csv", "represented_by_clean_final_spelling", "clean_final uses reason_forest_defacation.",
  "implemented_in_current_descriptive_script", "rdid", "income", "4_rdid_xgboost_20260805_2213.R", "included_as_total_income_30_usd", "Baseline total_income_30 is calculated from income components in add_rf105_aliases(); descriptive summaries are included here and rDiD is estimated in 4_rdid_xgboost_20260805_2213.R.",
  "implemented_in_current_descriptive_script", "rdid", "spent_total_month_with_6mo_monthly", "4_rdid_xgboost_20260805_2213.R", "represented_by_spent_total_month", "The exact old variable is absent from clean_final; rDiD uses spent_total_month."
)

member_coverage <- bind_rows(
  location_labels %>%
    transmute(source_script = "2_Rohingya_individual_survey_analysis/implemented_in_current_descriptive_script",
              outcome_family = "individual_locations", source_variable,
              reviewed_output = "table_descriptive_supplemental_locations_visited.csv",
              status = if_else(source_variable %in% names(members), "summarized", "not_available_in_clean_final"),
              note = ""),
  symptom_labels %>%
    transmute(source_script = "2_Rohingya_individual_survey_analysis/implemented_in_current_descriptive_script",
              outcome_family = "individual_covid_like_symptoms", source_variable,
              reviewed_output = "table_descriptive_supplemental_covid_symptoms.csv",
              status = if_else(source_variable %in% names(symptoms), "summarized", "not_available_in_clean_final"),
              note = "")
)

supplemental_survey_coverage <- bind_rows(
  household_coverage,
  known_unavailable_or_represented,
  member_coverage
) %>%
  arrange(source_script, outcome_family, source_variable)
write_reviewed_csv(
  supplemental_survey_coverage,
  "table_descriptive_supplemental_outcome_coverage.csv",
  subfolder = "qa"
)

source_script_audit <- tribble(
  ~source_script, ~reviewed_addition, ~notes,
  "implemented_in_current_descriptive_script", "Additional rDiD outcomes added to 4_rdid_xgboost_20260805_2213.R, including total_income_30_usd.", "Baseline total_income_30 is calculated from the specified income component variables when the aggregate field is missing.",
  "implemented_in_current_descriptive_script", "Fuel-procurement person, walk/wait, forest fee, and bundle-cost summaries.", "The old output used household counts by demographic; reviewed output keeps one row per household-timepoint and aggregate denominators.",
  "implemented_in_current_descriptive_script", "Stove purpose/use descriptive outcomes and fuel-use rDiD additions.", "cook_sell_days_week is aliased from cook_sell_yesterday; cook_to_sell_percent is aliased from cook_to_sell.",
  "implemented_in_current_descriptive_script", "Plastic-burning frequency and reasons/types; plastic_burn_gt1_yn added to rDiD.", "The old script filtered one LPG-runout variable without clear justification; reviewed descriptive outputs do not apply that filter.",
  "implemented_in_current_descriptive_script", "Forest-use descriptive outcomes.", "reason_forest_defacation spelling follows clean_final; gather_wood_dead is aliased from gather_scraps_dead.",
  "implemented_in_current_descriptive_script", "Who-cooks descriptive outcomes.", "Outputs are by arm and timepoint instead of a TableOne object only.",
  "implemented_in_current_descriptive_script", "Economic descriptive outcomes plus spent_food_pct and total_income_30_usd rDiD additions.", "Reviewed spent_food_pct corrects the old logical if_else expression; baseline total_income_30 is calculated from component income variables.",
  "implemented_in_current_descriptive_script", "Credit/debt, income/expenditure, repair, and willingness-to-pay descriptive outputs; additional economic rDiD outcomes.", "Outputs use clean_final and reviewed output folders.",
  "implemented_in_current_descriptive_script", "Window and ventilation descriptive outcomes.", "No temperature or radiation variables with clear names were found in survey_refugee_household.rds.",
  "implemented_in_current_descriptive_script", "LPG training, safety, refill, repair, and willingness-to-pay summaries.", "Date-difference calculations from the exploratory script are not repeated because date formats/denominators need a separate programmatic QA pass.",
  "implemented_in_current_descriptive_script", "Sleep questionnaire descriptive outcomes.", "The embedded calculation summarized sleep_hours, sleep_bad_dreams, and sleep_quality.",
  "implemented_in_current_descriptive_script", "Child MUAC measurement availability and derived MUAC flags.", "Includes Fiorentino cutoffs from the old comments plus WHO <125 and <115 mm flags.",
  "2_Rohingya_individual_survey_analysis/implemented_in_current_descriptive_script", "Member demographics, hours outside, locations visited, and COVID-like symptoms.", "Member/symptom denominators are individual rows, not household rows. Symptom records cannot be validly linked to member age/sex from clean_final, so symptom outputs are arm/timepoint summaries only."
)
write_reviewed_csv(
  source_script_audit,
  "table_descriptive_supplemental_source_audit.csv",
  subfolder = "qa"
)

methods_notes <- c(
  "# Supplemental Survey Survey Outcomes",
  "",
  paste0("Generated on ", Sys.Date(), " by 5_analysis_RF105/reviewed/the supplemental descriptive section in this script."),
  "",
  "This script adds aggregate descriptive outputs for older household and individual survey outcome families not already present in the main reviewed RF105 scripts.",
  "Old simple DiD/model outcomes that are comparable over time were added to 4_rdid_xgboost_20260805_2213.R rather than modeled here.",
  "No fcn_id-level table is exported by this script.",
  "",
  "Important limitations:",
  "- Baseline total_income_30 is calculated as the sum of income_cash_ngo, income_own_business, income_wage_labor, income_skill_labor, income_selling_wood, income_abroad, income_humanitarian_asst, income_handicrafts_tailoring, and income_farming when the aggregate field is missing.",
  "- Supplemental household binary tables keep percent, ci_lower, and ci_upper as n_yes / n_nonmissing for backward compatibility and also report percent_total, ci_lower_total, and ci_upper_total as n_yes / n_total. The matching *_nonmissing columns make the nonmissing denominator explicit.",
  "- cook_sell_days_week is aliased from cook_sell_yesterday, cook_to_sell_percent is aliased from cook_to_sell, and gather_wood_dead is aliased from gather_scraps_dead. reason_forest_defecation is represented by the clean_final spelling reason_forest_defacation.",
  "- Member/location/symptom outputs use individual-level denominators and should not be compared directly with household percentages.",
  "- Symptom records cannot be validly linked to member age/sex from clean_final, so symptom summaries are by study arm and timepoint only."
)
readr::write_lines(
  methods_notes,
  file.path(dir_tables_qa, "table_descriptive_supplemental_methods_notes.md")
)
message("Wrote QA notes: ", file.path(dir_tables_qa, "table_descriptive_supplemental_methods_notes.md"))
})

################################################################################
# Requested cooking-practice prevalence
# Integrated from 3.3_requested_practice_prevalence_20260924.R.
# Preserves the requested prevalence table and revised two-panel figure.
################################################################################
local({
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  stop(
    "Package `gridExtra` is required to assemble this figure. ",
    "Run renv::restore() from the project root, then rerun this script.",
    call. = FALSE
  )
}

practice_vars <- tribble(
  ~practice_group, ~source_variable, ~practice_label, ~value_type, ~derivation_note,
  "Common stove and cooking practices", "stove_reason_cook_together",
  "Cooked food for friends/relatives", "binary_yes_no",
  "Yes/no survey field summarized as n_yes / n_total.",
  "Common stove and cooking practices", "stove_boil_drink",
  "Boiled drinking water", "binary_yes_no",
  "Yes/no survey field summarized as n_yes / n_total.",
  "Common stove and cooking practices", "stove_boil_bathe",
  "Boiled bathing water", "binary_yes_no",
  "Yes/no survey field summarized as n_yes / n_total.",
  "Rare or food-selling practices", "stove_reason_stay_warm",
  "Used stove to stay warm", "binary_yes_no",
  "Yes/no survey field summarized as n_yes / n_total.",
  "Rare or food-selling practices", "stove_reason_sell_food",
  "Used stove to cook food to sell", "binary_yes_no",
  "Yes/no survey field summarized as n_yes / n_total.",
  "Common stove and cooking practices", "soak_rice",
  "Soaked rice before cooking", "binary_yes_no",
  "Yes/no survey field summarized as n_yes / n_total.",
  "Common stove and cooking practices", "soak_lentils",
  "Soaked lentils before cooking", "binary_yes_no",
  "Yes/no survey field summarized as n_yes / n_total.",
  "Common stove and cooking practices", "cover_pot",
  "Covered pot while cooking", "binary_yes_no",
  "Yes/no survey field summarized as n_yes / n_total."
) %>%
  mutate(practice_order = row_number())

clean_numeric_practice <- function(x) {
  x_chr <- stringr::str_squish(as.character(x))
  x_chr[x_chr == ""] <- NA_character_
  suppressWarnings(out <- as.numeric(x_chr))
  out[out %in% c(77, 88, 99)] <- NA_real_
  out
}

positive_numeric_any <- function(x) {
  value <- clean_numeric_practice(x)
  case_when(
    is.na(value) ~ NA_integer_,
    value > 0 ~ 1L,
    TRUE ~ 0L
  )
}

positive_response_present <- function(x) {
  value <- clean_numeric_practice(x)
  case_when(
    is.na(value) ~ NA_integer_,
    value > 0 ~ 1L,
    TRUE ~ 0L
  )
}

derive_practice_value <- function(df, var, value_type) {
  if (!var %in% names(df)) {
    stop("Missing requested variable in clean household survey: ", var,
         call. = FALSE)
  }

  if (identical(value_type, "binary_yes_no")) {
    return(make_yn(df[[var]]))
  }
  if (identical(value_type, "positive_numeric_any")) {
    return(positive_numeric_any(df[[var]]))
  }
  if (identical(value_type, "positive_response_present")) {
    return(positive_response_present(df[[var]]))
  }

  stop("Unknown value_type for ", var, ": ", value_type, call. = FALSE)
}

calc_prop_ci <- function(numerator, denominator) {
  proportion <- ifelse(denominator > 0, numerator / denominator, NA_real_)
  se <- sqrt(proportion * (1 - proportion) / denominator)
  tibble(
    percent = 100 * proportion,
    ci_lower = pmax(0, 100 * (proportion - qnorm(0.975) * se)),
    ci_upper = pmin(100, 100 * (proportion + qnorm(0.975) * se))
  )
}

survey_raw <- readr::read_rds(file_survey_refugee_household) %>%
  add_rf105_aliases()
analysis_population <- make_analysis_population(survey_raw, id_var = "fcn_id")
survey <- analysis_population$all_deduplicated %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  ) %>%
  filter(timepoint %in% timepoint_levels, study_arm_overall %in% arm_levels)

survey_with_all_arms <- bind_rows(
  survey,
  survey %>% mutate(study_arm_overall = "all_arms")
)

practice_summary <- purrr::map_dfr(seq_len(nrow(practice_vars)), function(i) {
  var_info <- practice_vars[i, ]
  value <- derive_practice_value(
    survey_with_all_arms,
    var_info$source_variable,
    var_info$value_type
  )

  survey_with_all_arms %>%
    transmute(
      timepoint,
      study_arm_overall,
      value = value
    ) %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise(
      n_total = n(),
      n_nonmissing = sum(!is.na(value)),
      n_practice = sum(value == 1L, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    bind_cols(calc_prop_ci(.$n_practice, .$n_total)) %>%
    rename(
      percent_all_households = percent,
      ci_lower_all_households = ci_lower,
      ci_upper_all_households = ci_upper
    ) %>%
    bind_cols(calc_prop_ci(.$n_practice, .$n_nonmissing)) %>%
    rename(
      percent_nonmissing = percent,
      ci_lower_nonmissing = ci_lower,
      ci_upper_nonmissing = ci_upper
    ) %>%
    mutate(
      across(
        c(percent_all_households, ci_lower_all_households,
          ci_upper_all_households),
        ~ if_else(n_nonmissing > 0, .x, NA_real_)
      ),
      practice_group = var_info$practice_group,
      source_variable = var_info$source_variable,
      practice_label = var_info$practice_label,
      value_type = var_info$value_type,
      derivation_note = var_info$derivation_note,
      practice_order = var_info$practice_order,
      denominator_note = paste(
        "Primary figure percentage is n_practice / n_total among all deduplicated household-timepoint records in the arm and survey round.",
        "Rows with n_nonmissing = 0 are kept in the table but not plotted."
      )
    )
})

expected_rows <- tidyr::expand_grid(
  timepoint = timepoint_levels,
  study_arm_overall = c(arm_levels, "all_arms"),
  source_variable = practice_vars$source_variable
)
missing_rows <- expected_rows %>%
  anti_join(
    practice_summary %>% distinct(timepoint, study_arm_overall, source_variable),
    by = c("timepoint", "study_arm_overall", "source_variable")
  )
if (nrow(missing_rows) > 0) {
  stop("Missing expected practice summary rows.", call. = FALSE)
}
if (any(practice_summary$n_practice > practice_summary$n_nonmissing) ||
    any(practice_summary$n_nonmissing > practice_summary$n_total) ||
    any(practice_summary$n_total <= 0)) {
  stop("Practice prevalence numerator/denominator validation failed.",
       call. = FALSE)
}

practice_summary <- practice_summary %>%
  mutate(
    timepoint = factor(timepoint, levels = timepoint_levels, ordered = TRUE),
    timepoint_label = factor(
      timepoint_label_with_year(timepoint),
      levels = timepoint_label_with_year_levels
    ),
    study_arm_label = factor(
      study_arm_overall,
      levels = c(arm_levels, "all_arms"),
      labels = c("Comparison", "Intervention", "All households")
    )
  ) %>%
  arrange(practice_order, timepoint, study_arm_overall) %>%
  select(
    timepoint, timepoint_label, study_arm_overall, study_arm_label,
    practice_group, practice_order, practice_label, source_variable,
    value_type, n_total, n_nonmissing, n_practice,
    percent_all_households, ci_lower_all_households,
    ci_upper_all_households, percent_nonmissing, ci_lower_nonmissing,
    ci_upper_nonmissing, derivation_note, denominator_note
  )

write_reviewed_csv(
  practice_summary,
  "table_descriptive_requested_practice_prevalence.csv"
)

plot_data <- practice_summary %>%
  filter(
    study_arm_overall %in% arm_levels,
    !is.na(percent_all_households)
  ) %>%
  mutate(
    percent_label = if_else(
      percent_all_households < 0.05 & percent_all_households > 0,
      "<0.1%",
      sprintf("%.1f%%", percent_all_households)
    )
  )

if (nrow(plot_data) == 0) {
  stop("No requested practice prevalence rows are available for plotting.",
       call. = FALSE)
}

arm_colors_requested <- c(
  "Comparison" = "#3B6EA8",
  "Intervention" = "#C94C4C"
)
position_arm <- position_dodge(width = 0.72)

make_practice_panel <- function(data, title, y_upper, y_breaks,
                                show_legend = FALSE) {
  data <- data %>%
    mutate(
      practice_label = factor(
        practice_label,
        levels = rev(unique(practice_label[order(practice_order)]))
      ),
      label_hjust = if_else(percent_all_households > 0.86 * y_upper, 1.05, -0.08)
    )

  ggplot(
    data,
    aes(x = practice_label, y = percent_all_households, fill = study_arm_label)
  ) +
    geom_col(position = position_arm, width = 0.64) +
    geom_text(
      aes(label = percent_label, hjust = label_hjust),
      position = position_arm,
      size = 2.8,
      show.legend = FALSE
    ) +
    facet_wrap(vars(timepoint_label), nrow = 1) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = arm_colors_requested, drop = FALSE) +
    scale_y_continuous(
      limits = c(0, y_upper),
      breaks = y_breaks,
      labels = scales::label_number(suffix = "%", accuracy = 1),
      expand = expansion(mult = c(0, 0.03))
    ) +
    labs(
      title = title,
      x = NULL,
      y = "Households (%)",
      fill = "Study arm"
    ) +
    theme_minimal(base_size = 10, base_family = "sans") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#D9D9D9", linewidth = 0.35),
      panel.border = element_rect(color = "#777777", fill = NA, linewidth = 0.35),
      panel.spacing.x = grid::unit(0.8, "lines"),
      strip.background = element_rect(
        fill = "#F2F2F2", color = "#BDBDBD", linewidth = 0.35
      ),
      strip.text = element_text(face = "bold", size = 9),
      axis.text.y = element_text(size = 8.5),
      axis.text.x = element_text(size = 8.5),
      axis.title = element_text(size = 9.5),
      plot.title = element_text(face = "bold", size = 11, hjust = 0),
      plot.title.position = "plot",
      legend.position = if (show_legend) "top" else "none",
      legend.justification = "left",
      legend.box.just = "left",
      legend.margin = margin(t = 0, r = 0, b = 2, l = 0),
      plot.margin = margin(t = 3, r = 18, b = 3, l = 4)
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))
}

common_plot_data <- plot_data %>%
  filter(practice_group == "Common stove and cooking practices")
rare_plot_data <- plot_data %>%
  filter(practice_group == "Rare or food-selling practices")

rare_upper <- max(5, ceiling((max(rare_plot_data$percent_all_households,
                                  na.rm = TRUE) + 0.5) / 2) * 2)
rare_breaks <- scales::breaks_pretty(n = 5)(c(0, rare_upper))
rare_breaks <- rare_breaks[rare_breaks >= 0 & rare_breaks <= rare_upper]

common_plot <- make_practice_panel(
  common_plot_data,
  "Common stove and cooking practices",
  y_upper = 100,
  y_breaks = seq(0, 100, by = 25),
  show_legend = TRUE
)
rare_plot <- make_practice_panel(
  rare_plot_data,
  "Rare or food-selling practices",
  y_upper = rare_upper,
  y_breaks = rare_breaks,
  show_legend = FALSE
)

figure_grob <- gridExtra::arrangeGrob(
  common_plot,
  rare_plot,
  ncol = 1,
  heights = c(1.35, 1),
  top = grid::textGrob(
    "Prevalence of selected stove and cooking practices",
    x = grid::unit(0.01, "npc"),
    hjust = 0,
    gp = grid::gpar(fontfamily = "sans", fontsize = 15, fontface = "bold")
  )
)

figure_stem <- "fig_descriptive_requested_practice_prevalence_revised"
output_png <- file.path(dir_figures_reviewed, paste0(figure_stem, ".png"))
output_pdf <- file.path(dir_figures_reviewed, paste0(figure_stem, ".pdf"))

dir.create(dirname(output_png), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(output_pdf), recursive = TRUE, showWarnings = FALSE)

ggplot2::ggsave(
  filename = output_png,
  plot = figure_grob,
  width = 12,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)
ggplot2::ggsave(
  filename = output_pdf,
  plot = figure_grob,
  width = 12,
  height = 9,
  units = "in",
  device = "pdf",
  bg = "white"
)

message("Wrote figure: ", output_png)
message("Wrote figure: ", output_pdf)

})

################################################################################
# Income amounts and credit access
# Integrated from 3.3_income_credit_figures_20260924.R.
# Preserves source-specific income and credit tables, QA coverage, and figures.
################################################################################
local({
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  stop(
    "Package `gridExtra` is required to assemble the credit figure. ",
    "Run renv::restore() from the project root, then rerun this script.",
    call. = FALSE
  )
}

mean_safe_local <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

sd_safe_local <- function(x) {
  if (sum(!is.na(x)) < 2) NA_real_ else sd(x, na.rm = TRUE)
}

median_safe_local <- function(x) {
  if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
}

quantile_safe_local <- function(x, prob) {
  if (all(is.na(x))) NA_real_ else quantile(x, prob, na.rm = TRUE, names = FALSE)
}

add_prop_ci <- function(df, numerator_col = "n_yes",
                        denominator_col = "n_denominator") {
  numerator <- df[[numerator_col]]
  denominator <- df[[denominator_col]]
  proportion <- if_else(
    !is.na(numerator) & !is.na(denominator) & denominator > 0,
    numerator / denominator,
    NA_real_
  )
  se <- if_else(
    !is.na(proportion) & !is.na(denominator) & denominator > 0,
    sqrt(proportion * (1 - proportion) / denominator),
    NA_real_
  )

  df %>%
    mutate(
      proportion = proportion,
      percent = 100 * proportion,
      se = se,
      ci_lower = pmax(0, 100 * (proportion - qnorm(0.975) * se)),
      ci_upper = pmin(100, 100 * (proportion + qnorm(0.975) * se))
    )
}

label_dollar_dynamic <- function(x) {
  max_abs <- suppressWarnings(max(abs(x), na.rm = TRUE))
  if (!is.finite(max_abs)) max_abs <- 0
  accuracy <- case_when(
    max_abs < 1 ~ 0.01,
    max_abs < 10 ~ 0.1,
    TRUE ~ 1
  )
  scales::label_dollar(accuracy = accuracy)(x)
}

label_percent_dynamic <- function(x) {
  max_abs <- suppressWarnings(max(abs(x), na.rm = TRUE))
  if (!is.finite(max_abs)) max_abs <- 0
  accuracy <- if_else(max_abs < 10, 0.1, 1)
  scales::label_number(suffix = "%", accuracy = accuracy)(x)
}

income_source_labels <- tribble(
  ~source_variable,                 ~income_source_label,       ~display_order,
  "income_cash_ngo",                "Cash/NGO support",         1L,
  "income_own_business",            "Own business",             2L,
  "income_wage_labor",              "Wage labor",               3L,
  "income_skill_labor",             "Skilled labor",            4L,
  "income_selling_wood",            "Selling wood",             5L,
  "income_abroad",                  "Remittances",              6L,
  "income_humanitarian_asst",       "Humanitarian assistance",  7L,
  "income_handicrafts_tailoring",   "Handicrafts/tailoring",    8L,
  "income_farming",                 "Farming",                  9L
)

credit_source_labels <- tribble(
  ~source_variable,          ~credit_source_label, ~display_order,
  "credit_relatives",       "Relatives",          1L,
  "credit_charities",       "Charities",          2L,
  "credit_village_head",    "Village head",       3L,
  "credit_lender",          "Lender",             4L,
  "credit_bank",            "Bank",               5L,
  "credit_cooperative",     "Cooperative",        6L
)

survey_raw <- readr::read_rds(file_survey_refugee_household) %>%
  add_rf105_aliases()
analysis_population <- make_analysis_population(survey_raw, id_var = "fcn_id")
survey <- analysis_population$all_deduplicated %>%
  filter(study_arm_overall %in% arm_levels)

variable_coverage <- bind_rows(
  income_source_labels %>%
    transmute(
      analysis_domain = "income_source_amounts",
      source_variable,
      requested_label = income_source_label
    ),
  tibble(
    analysis_domain = "credit_access",
    source_variable = "credit_access",
    requested_label = "Any credit access/use"
  ),
  credit_source_labels %>%
    transmute(
      analysis_domain = "credit_sources_among_credit_users",
      source_variable,
      requested_label = credit_source_label
    )
) %>%
  mutate(
    available = source_variable %in% names(survey),
    status = if_else(available, "available", "missing_from_clean_final")
  )

write_reviewed_csv(
  variable_coverage,
  "table_descriptive_income_credit_variable_coverage.csv",
  subfolder = "qa"
)

missing_requested_vars <- variable_coverage %>%
  filter(!available) %>%
  pull(source_variable)
if (length(missing_requested_vars) > 0) {
  stop(
    "Requested variable(s) are missing from survey_refugee_household.rds: ",
    paste(missing_requested_vars, collapse = ", "),
    call. = FALSE
  )
}

study_arm_labels <- c(
  comparison = "Comparison group",
  intervention = "Intervention group"
)
study_arm_colors <- c(
  "Comparison group" = "#3B6EA8",
  "Intervention group" = "#C94C4C"
)

income_long <- purrr::pmap_dfr(income_source_labels, function(source_variable,
                                                              income_source_label,
                                                              display_order) {
  var <- source_variable
  tibble(
    timepoint = survey$timepoint,
    study_arm_overall = survey$study_arm_overall,
    source_variable = var,
    income_source_label = income_source_label,
    display_order = display_order,
    income_bdt = make_nonnegative_amount(survey[[var]])
  )
})

income_summary <- income_long %>%
  group_by(timepoint, study_arm_overall, source_variable, income_source_label,
           display_order) %>%
  summarise(
    n_total = n(),
    n_nonmissing = sum(!is.na(income_bdt)),
    n_positive = sum(income_bdt > 0, na.rm = TRUE),
    pct_positive = if_else(
      n_nonmissing > 0,
      100 * n_positive / n_nonmissing,
      NA_real_
    ),
    mean_bdt = mean_safe_local(income_bdt),
    sd_bdt = sd_safe_local(income_bdt),
    median_bdt = median_safe_local(income_bdt),
    p25_bdt = quantile_safe_local(income_bdt, 0.25),
    p75_bdt = quantile_safe_local(income_bdt, 0.75),
    se_bdt = if_else(n_nonmissing > 1, sd_bdt / sqrt(n_nonmissing), NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    ci_lower_bdt = if_else(
      n_nonmissing > 1 & !is.na(se_bdt),
      pmax(0, mean_bdt - qt(0.975, n_nonmissing - 1) * se_bdt),
      NA_real_
    ),
    ci_upper_bdt = if_else(
      n_nonmissing > 1 & !is.na(se_bdt),
      mean_bdt + qt(0.975, n_nonmissing - 1) * se_bdt,
      NA_real_
    ),
    exchange_bdt_per_usd = unname(exchange_bdt_per_usd[as.character(timepoint)]),
    mean_usd = mean_bdt / exchange_bdt_per_usd,
    sd_usd = sd_bdt / exchange_bdt_per_usd,
    median_usd = median_bdt / exchange_bdt_per_usd,
    p25_usd = p25_bdt / exchange_bdt_per_usd,
    p75_usd = p75_bdt / exchange_bdt_per_usd,
    ci_lower_usd = ci_lower_bdt / exchange_bdt_per_usd,
    ci_upper_usd = ci_upper_bdt / exchange_bdt_per_usd,
    timepoint_label = timepoint_label_with_year(timepoint),
    study_arm_label = unname(study_arm_labels[as.character(study_arm_overall)]),
    denominator_note = "Mean and distribution statistics use nonmissing, nonnegative monthly BDT amount values after applying RF105 missing-code cleaning; USD columns use the timepoint-specific exchange rate in the reviewed config."
  )

income_source_order <- income_long %>%
  filter(as.character(timepoint) == "baseline") %>%
  group_by(source_variable, income_source_label) %>%
  summarise(
    baseline_mean_bdt = mean_safe_local(income_bdt),
    .groups = "drop"
  ) %>%
  arrange(desc(baseline_mean_bdt), source_variable) %>%
  pull(income_source_label)

income_summary <- income_summary %>%
  mutate(display_order = match(income_source_label, income_source_order)) %>%
  arrange(display_order, timepoint, study_arm_overall)

write_reviewed_csv(
  income_summary,
  "table_descriptive_income_source_amounts.csv"
)

income_plot_data <- income_summary %>%
  filter(n_nonmissing > 0, !is.na(mean_usd)) %>%
  mutate(
    timepoint_label = factor(timepoint_label, levels = timepoint_label_with_year_levels),
    study_arm_label = factor(study_arm_label, levels = study_arm_labels),
    income_source_label = factor(
      income_source_label,
      levels = income_source_order
    )
  )

income_plot_top <- ggplot(
  filter(income_plot_data, display_order <= 3),
  aes(
    x = timepoint_label,
    y = mean_usd,
    color = study_arm_label,
    group = study_arm_label
  )
) +
  geom_line(linewidth = 0.7, position = position_dodge(width = 0.12)) +
  geom_point(size = 2.1, position = position_dodge(width = 0.12)) +
  geom_errorbar(
    aes(ymin = ci_lower_usd, ymax = ci_upper_usd),
    width = 0.08,
    linewidth = 0.35,
    position = position_dodge(width = 0.12),
    na.rm = TRUE
  ) +
  facet_wrap(~ income_source_label, scales = "fixed", nrow = 1) +
  scale_color_manual(values = study_arm_colors, drop = FALSE) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 5),
    labels = scales::label_dollar(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(ylim = c(0, 30)) +
  labs(
    x = NULL,
    y = NULL,
    color = "Study arm"
  ) +
  theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8.5),
    legend.position = "top",
    legend.justification = "left"
  )

income_plot_bottom <- ggplot(
  filter(income_plot_data, display_order > 3),
  aes(
    x = timepoint_label,
    y = mean_usd,
    color = study_arm_label,
    group = study_arm_label
  )
) +
  geom_line(linewidth = 0.7, position = position_dodge(width = 0.12)) +
  geom_point(size = 2.1, position = position_dodge(width = 0.12)) +
  geom_errorbar(
    aes(ymin = ci_lower_usd, ymax = ci_upper_usd),
    width = 0.08,
    linewidth = 0.35,
    position = position_dodge(width = 0.12),
    na.rm = TRUE
  ) +
  facet_wrap(~ income_source_label, scales = "fixed", ncol = 3) +
  scale_color_manual(values = study_arm_colors, drop = FALSE) +
  scale_y_continuous(
    breaks = seq(0, 6, by = 2),
    labels = scales::label_dollar(accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(ylim = c(0, 6)) +
  labs(x = NULL, y = NULL, color = "Study arm") +
  theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8.5),
    legend.position = "none"
  )

income_panels <- gridExtra::arrangeGrob(
  income_plot_top,
  income_plot_bottom,
  ncol = 1,
  heights = c(1.15, 2)
)

fig_income_sources <- gridExtra::arrangeGrob(
  income_panels,
  top = grid::textGrob(
    "Household income by source, study arm, and survey round",
    x = grid::unit(0.01, "npc"),
    hjust = 0,
    gp = grid::gpar(fontfamily = "sans", fontsize = 13, fontface = "bold")
  ),
  left = grid::textGrob(
    "Mean monthly amount (USD)",
    rot = 90,
    gp = grid::gpar(fontfamily = "sans", fontsize = 9.5)
  )
)

save_reviewed_plot(
  fig_income_sources,
  "fig_descriptive_income_source_amounts_by_arm_timepoint.png",
  width = 12,
  height = 8
)
save_reviewed_plot(
  fig_income_sources,
  "fig_descriptive_income_source_amounts_by_arm_timepoint.pdf",
  width = 12,
  height = 8
)

credit_base <- survey %>%
  mutate(credit_access_yn = make_yn(credit_access)) %>%
  select(timepoint, study_arm_overall, credit_access_yn,
         all_of(credit_source_labels$source_variable))

credit_access_summary <- credit_base %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_household_records = n(),
    n_denominator = sum(!is.na(credit_access_yn)),
    n_nonmissing = n_denominator,
    n_yes = sum(credit_access_yn == 1, na.rm = TRUE),
    n_missing_source_response = NA_integer_,
    percent_nonmissing = NA_real_,
    .groups = "drop"
  ) %>%
  add_prop_ci() %>%
  mutate(
    outcome_group = "credit_access",
    source_variable = "credit_access",
    outcome_label = "Any credit access/use",
    display_order = 0L,
    denominator_type = "households_with_nonmissing_credit_access",
    timepoint_label = timepoint_label_with_year(timepoint),
    study_arm_label = unname(study_arm_labels[as.character(study_arm_overall)])
  )

credit_source_long <- purrr::pmap_dfr(credit_source_labels, function(source_variable,
                                                                     credit_source_label,
                                                                     display_order) {
  var <- source_variable
  tibble(
    timepoint = credit_base$timepoint,
    study_arm_overall = credit_base$study_arm_overall,
    credit_access_yn = credit_base$credit_access_yn,
    source_variable = var,
    outcome_label = credit_source_label,
    display_order = display_order,
    source_selected = make_yn(credit_base[[var]])
  )
})

credit_source_summary <- credit_source_long %>%
  filter(credit_access_yn == 1) %>%
  group_by(timepoint, study_arm_overall, source_variable, outcome_label,
           display_order) %>%
  summarise(
    n_household_records = n(),
    n_denominator = n(),
    n_nonmissing = sum(!is.na(source_selected)),
    n_yes = sum(source_selected == 1, na.rm = TRUE),
    n_missing_source_response = n_denominator - n_nonmissing,
    percent_nonmissing = if_else(
      n_nonmissing > 0,
      100 * n_yes / n_nonmissing,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  add_prop_ci() %>%
  mutate(
    outcome_group = "credit_source_among_credit_users",
    denominator_type = "households_with_credit_access_yes",
    timepoint_label = timepoint_label_with_year(timepoint),
    study_arm_label = unname(study_arm_labels[as.character(study_arm_overall)])
  )

credit_summary <- bind_rows(
  credit_access_summary,
  credit_source_summary
) %>%
  select(
    outcome_group, source_variable, outcome_label, display_order,
    timepoint, timepoint_label, study_arm_overall, study_arm_label,
    denominator_type, n_household_records, n_denominator, n_nonmissing,
    n_yes, n_missing_source_response, proportion, percent, se, ci_lower,
    ci_upper, percent_nonmissing
  ) %>%
  arrange(outcome_group, display_order, timepoint, study_arm_overall) %>%
  mutate(
    denominator_note = case_when(
      outcome_group == "credit_access" ~
        "Credit-access prevalence uses households with nonmissing credit_access as the denominator.",
      TRUE ~
        "Credit-source percentages use households with credit_access == 1 as the denominator; 99/missing source responses are not counted as selected and are reported in n_missing_source_response."
    )
  )

write_reviewed_csv(
  credit_summary,
  "table_descriptive_credit_access_sources.csv"
)

credit_access_plot_data <- credit_summary %>%
  filter(outcome_group == "credit_access") %>%
  mutate(
    timepoint_label = factor(timepoint_label, levels = timepoint_label_with_year_levels),
    study_arm_label = factor(study_arm_label, levels = study_arm_labels)
  )

credit_source_plot_data <- credit_summary %>%
  filter(outcome_group == "credit_source_among_credit_users") %>%
  mutate(
    timepoint_label = factor(timepoint_label, levels = timepoint_label_with_year_levels),
    study_arm_label = factor(study_arm_label, levels = study_arm_labels),
    outcome_label = factor(
      outcome_label,
      levels = credit_source_labels$credit_source_label
    )
  )

credit_top_plot_data <- bind_rows(
  credit_access_plot_data %>%
    mutate(panel_label = "Accessing credit"),
  credit_source_plot_data %>%
    filter(outcome_label == "Relatives") %>%
    mutate(panel_label = "Credit from relatives")
) %>%
  mutate(
    panel_label = factor(
      panel_label,
      levels = c("Accessing credit", "Credit from relatives")
    ),
    timepoint_index = as.numeric(timepoint_label),
    bar_x = timepoint_index + if_else(
      as.character(study_arm_label) == "Comparison group",
      -0.21,
      0.21
    ),
    label_y = ci_upper + 0.4
  )

credit_other_source_plot_data <- credit_source_plot_data %>%
  filter(outcome_label != "Relatives") %>%
  mutate(
    outcome_label = factor(
      as.character(outcome_label),
      levels = c("Village head", "Charities", "Lender", "Bank", "Cooperative")
    ),
    timepoint_index = as.numeric(timepoint_label),
    bar_x = timepoint_index + if_else(
      as.character(study_arm_label) == "Comparison group",
      -0.21,
      0.21
    ),
    label_y = ci_upper + 0.05
  )

credit_top_plot <- ggplot(
  credit_top_plot_data,
  aes(x = bar_x, y = percent, fill = study_arm_label)
) +
  geom_col(width = 0.32) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.12,
    linewidth = 0.35
  ) +
  geom_text(
    aes(
      y = label_y,
      label = scales::label_number(accuracy = 0.1, suffix = "%")(percent)
    ),
    size = 2.7,
    fontface = "bold",
    hjust = 0.5,
    vjust = 0
  ) +
  facet_wrap(~ panel_label, nrow = 1) +
  scale_fill_manual(values = study_arm_colors, drop = FALSE) +
  scale_x_continuous(
    breaks = seq_along(timepoint_label_with_year_levels),
    labels = timepoint_label_with_year_levels,
    limits = c(0.5, length(timepoint_label_with_year_levels) + 0.5),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(0, 106),
    breaks = seq(0, 100, by = 25),
    labels = scales::label_number(suffix = "%", accuracy = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = NULL,
    y = "Prevalence (%)",
    fill = "Study arm"
  ) +
  theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.x = element_text(angle = 25, hjust = 1),
    legend.position = "top",
    legend.justification = "left"
  )

credit_source_plot <- ggplot(
  credit_other_source_plot_data,
  aes(x = bar_x, y = percent, fill = study_arm_label)
) +
  geom_col(width = 0.32) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.12,
    linewidth = 0.35
  ) +
  geom_text(
    aes(
      y = label_y,
      label = scales::label_number(accuracy = 0.1, suffix = "%")(percent)
    ),
    size = 2.4,
    fontface = "bold",
    hjust = 0.5,
    vjust = 0
  ) +
  facet_wrap(~ outcome_label, nrow = 1, scales = "fixed") +
  scale_fill_manual(values = study_arm_colors, drop = FALSE) +
  scale_x_continuous(
    breaks = seq_along(timepoint_label_with_year_levels),
    labels = timepoint_label_with_year_levels,
    limits = c(0.5, length(timepoint_label_with_year_levels) + 0.5),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    labels = label_percent_dynamic,
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Sources of credit other than relatives",
    x = NULL,
    y = "Credit users (%)",
    fill = "Study arm"
  ) +
  theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold", size = 9),
    plot.title = element_text(face = "bold", size = 11, hjust = 0),
    plot.title.position = "plot",
    axis.text.x = element_text(angle = 25, hjust = 1, size = 8.5),
    legend.position = "none"
  )

fig_credit_sources <- gridExtra::arrangeGrob(
  credit_top_plot,
  credit_source_plot,
  ncol = 1,
  heights = c(1.05, 1),
  top = grid::textGrob(
    "Credit access and sources by study arm and survey round",
    x = grid::unit(0.01, "npc"),
    hjust = 0,
    gp = grid::gpar(fontfamily = "sans", fontsize = 14, fontface = "bold")
  )
)

ggplot2::ggsave(
  filename = file.path(dir_figures_reviewed,
                       "fig_descriptive_credit_access_sources_by_arm_timepoint.png"),
  plot = fig_credit_sources,
  width = 12,
  height = 7.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
ggplot2::ggsave(
  filename = file.path(dir_figures_reviewed,
                       "fig_descriptive_credit_access_sources_by_arm_timepoint.pdf"),
  plot = fig_credit_sources,
  width = 12,
  height = 7.5,
  units = "in",
  device = "pdf",
  bg = "white"
)

message(
  "Wrote figure: ",
  file.path(dir_figures_reviewed,
            "fig_descriptive_credit_access_sources_by_arm_timepoint.png")
)
message(
  "Wrote figure: ",
  file.path(dir_figures_reviewed,
            "fig_descriptive_credit_access_sources_by_arm_timepoint.pdf")
)

message("Income-source and credit-source figure generation complete.")

})

################################################################################
# Forest-use reasons and entry cost
# Integrated from 3.4_forest_use_reasons_cost_20260924.R.
# Preserves all-household reason denominators and reason-specific cost summaries.
################################################################################
local({
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  stop(
    "Package `gridExtra` is required to assemble this figure. ",
    "Run renv::restore() from the project root, then rerun this script.",
    call. = FALSE
  )
}

mean_safe_local <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

sd_safe_local <- function(x) {
  if (sum(!is.na(x)) < 2) NA_real_ else sd(x, na.rm = TRUE)
}

format_mean_sd_local <- function(mean_value, sd_value, digits = 1) {
  if (is.na(mean_value)) {
    return("")
  }
  if (is.na(sd_value)) {
    return(sprintf(paste0("%.", digits, "f (NA)"), mean_value))
  }
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"), mean_value, sd_value)
}

forest_reason_specs <- tribble(
  ~requested_variable,          ~alternate_variable,          ~reason_label,        ~reason_order,
  "reason_forest_food",        NA_character_,                "Food",              1L,
  "reason_forest_med",         NA_character_,                "Medicine",          2L,
  "reason_forest_shelter",     NA_character_,                "Shelter materials", 3L,
  "reason_forest_privacy",     NA_character_,                "Privacy",           4L,
  "reason_forest_defacation",  "reason_forest_defecation",   "Defecation",        5L,
  "reason_forest_leisure",     NA_character_,                "Leisure",           6L,
  "reason_forest_other",       NA_character_,                "Other",             7L
)

survey_raw <- readr::read_rds(file_survey_refugee_household) %>%
  add_rf105_aliases()
analysis_population <- make_analysis_population(survey_raw, id_var = "fcn_id")
survey <- analysis_population$all_deduplicated %>%
  filter(study_arm_overall %in% arm_levels) %>%
  mutate(
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  )

resolve_available_variable <- function(primary, alternate, data_names) {
  if (primary %in% data_names) {
    return(primary)
  }
  if (!is.na(alternate) && alternate %in% data_names) {
    return(alternate)
  }
  NA_character_
}

variable_coverage <- forest_reason_specs %>%
  mutate(
    source_variable = purrr::map2_chr(
      requested_variable,
      alternate_variable,
      resolve_available_variable,
      data_names = names(survey)
    ),
    available = !is.na(source_variable),
    status = if_else(available, "available", "missing_from_clean_final"),
    note = case_when(
      requested_variable == "reason_forest_defacation" &
        source_variable == "reason_forest_defacation" ~
        "Uses clean_final spelling; old scripts sometimes used reason_forest_defecation.",
      requested_variable == "reason_forest_defacation" &
        source_variable == "reason_forest_defecation" ~
        "Fallback spelling used because reason_forest_defacation was unavailable.",
      TRUE ~ ""
    )
  )

required_context_vars <- c("forest_collect_not_wood", "cost_forest_not_wood")
context_coverage <- tibble(
  requested_variable = required_context_vars,
  alternate_variable = NA_character_,
  reason_label = c("Any non-wood forest use", "Cost of entering forest for non-wood reasons"),
  reason_order = NA_integer_,
  source_variable = required_context_vars,
  available = required_context_vars %in% names(survey),
  status = if_else(available, "available", "missing_from_clean_final"),
  note = ""
)

write_reviewed_csv(
  bind_rows(variable_coverage, context_coverage),
  "table_descriptive_forest_use_reasons_cost_variable_coverage.csv",
  subfolder = "qa"
)

missing_vars <- bind_rows(variable_coverage, context_coverage) %>%
  filter(!available) %>%
  pull(requested_variable)
if (length(missing_vars) > 0) {
  stop(
    "Forest-use variable(s) are missing from survey_refugee_household.rds: ",
    paste(missing_vars, collapse = ", "),
    call. = FALSE
  )
}

forest_reason_specs <- variable_coverage %>%
  select(source_variable, requested_variable, reason_label, reason_order, note)

survey_with_all_arms <- bind_rows(
  survey,
  survey %>% mutate(study_arm_overall = "all_arms")
) %>%
  mutate(
    forest_collect_not_wood_yn = make_yn(forest_collect_not_wood),
    cost_forest_not_wood_bdt = make_nonnegative_amount(cost_forest_not_wood)
  )

study_arm_labels <- c(
  comparison = "Comparison group",
  intervention = "Intervention group",
  all_arms = "All households"
)
study_arm_colors <- c(
  "Comparison group" = "#3B6EA8",
  "Intervention group" = "#C94C4C"
)

forest_reason_summary <- purrr::pmap_dfr(
  forest_reason_specs,
  function(source_variable, requested_variable, reason_label,
           reason_order, note) {
    reason_selected <- make_yn(survey_with_all_arms[[source_variable]])
    forest_use <- survey_with_all_arms$forest_collect_not_wood_yn
    cost <- survey_with_all_arms$cost_forest_not_wood_bdt

    survey_with_all_arms %>%
      transmute(
        timepoint,
        study_arm_overall,
        reason_selected = reason_selected,
        forest_collect_not_wood_yn = forest_use,
        cost_forest_not_wood_bdt = cost,
        cost_for_selected_reason_bdt = if_else(
          reason_selected == 1L,
          cost_forest_not_wood_bdt,
          NA_real_
        )
      ) %>%
      group_by(timepoint, study_arm_overall) %>%
      summarise(
        n_total = n(),
        n_forest_use_nonwood_nonmissing = sum(!is.na(forest_collect_not_wood_yn)),
        n_forest_use_nonwood = sum(forest_collect_not_wood_yn == 1L, na.rm = TRUE),
        n_reason_nonmissing = sum(!is.na(reason_selected)),
        n_reason = sum(reason_selected == 1L, na.rm = TRUE),
        n_reason_among_forest_users = sum(
          reason_selected == 1L & forest_collect_not_wood_yn == 1L,
          na.rm = TRUE
        ),
        n_reason_without_forest_use_yes = sum(
          reason_selected == 1L &
            (is.na(forest_collect_not_wood_yn) | forest_collect_not_wood_yn != 1L),
          na.rm = TRUE
        ),
        n_cost_nonmissing_among_reason = sum(!is.na(cost_for_selected_reason_bdt)),
        mean_cost_bdt_among_reason = mean_safe_local(cost_for_selected_reason_bdt),
        sd_cost_bdt_among_reason = sd_safe_local(cost_for_selected_reason_bdt),
        .groups = "drop"
      ) %>%
      mutate(
        requested_variable = requested_variable,
        source_variable = source_variable,
        reason_label = reason_label,
        reason_order = reason_order,
        pct_all_households = if_else(
          n_total > 0,
          100 * n_reason / n_total,
          NA_real_
        ),
        pct_nonwood_forest_users = if_else(
          n_forest_use_nonwood > 0,
          100 * n_reason_among_forest_users / n_forest_use_nonwood,
          NA_real_
        ),
        mean_sd_cost_bdt_among_reason = purrr::map2_chr(
          mean_cost_bdt_among_reason,
          sd_cost_bdt_among_reason,
          format_mean_sd_local
        ),
        timepoint_label = timepoint_label_with_year(timepoint),
        study_arm_label = unname(study_arm_labels[study_arm_overall]),
        denominator_note = paste(
          "pct_nonwood_forest_users is n_reason_among_forest_users / n_forest_use_nonwood.",
          "pct_all_households is n_reason / n_total.",
          "Cost mean/SD use nonmissing, nonnegative cost_forest_not_wood among households selecting the reason."
        ),
        data_note = note
      )
  }
) %>%
  mutate(
    timepoint = factor(timepoint, levels = timepoint_levels, ordered = TRUE),
    timepoint_label = factor(timepoint_label, levels = timepoint_label_with_year_levels),
    study_arm_overall = factor(study_arm_overall, levels = c(arm_levels, "all_arms")),
    study_arm_label = factor(
      study_arm_label,
      levels = unname(study_arm_labels[c(arm_levels, "all_arms")])
    )
  ) %>%
  arrange(reason_order, timepoint, study_arm_overall) %>%
  select(
    reason_order, reason_label, requested_variable, source_variable,
    timepoint, timepoint_label, study_arm_overall, study_arm_label,
    n_total, n_forest_use_nonwood_nonmissing, n_forest_use_nonwood,
    n_reason_nonmissing, n_reason, n_reason_among_forest_users,
    n_reason_without_forest_use_yes, pct_all_households,
    pct_nonwood_forest_users, n_cost_nonmissing_among_reason,
    mean_cost_bdt_among_reason, sd_cost_bdt_among_reason,
    mean_sd_cost_bdt_among_reason, denominator_note, data_note
  )

if (any(forest_reason_summary$n_reason_among_forest_users >
        forest_reason_summary$n_forest_use_nonwood)) {
  stop("Forest reason numerator exceeds non-wood forest-use denominator.",
       call. = FALSE)
}

write_reviewed_csv(
  forest_reason_summary,
  "table_descriptive_forest_use_reasons_cost.csv"
)

plot_data <- forest_reason_summary %>%
  filter(study_arm_overall %in% arm_levels) %>%
  mutate(
    reason_label = factor(
      reason_label,
      levels = rev(forest_reason_specs$reason_label)
    ),
    timepoint_label = factor(timepoint_label, levels = timepoint_label_with_year_levels),
    study_arm_label = factor(study_arm_label, levels = unname(study_arm_labels[arm_levels])),
    prevalence_label = if_else(
      !is.na(pct_all_households),
      sprintf("%.1f%%", pct_all_households),
      ""
    )
  )

if (nrow(plot_data) == 0) {
  stop("No forest-use reason rows are available for plotting.", call. = FALSE)
}

position_arm <- position_dodge(width = 0.72)

prevalence_axis_upper <- max(plot_data$pct_all_households, na.rm = TRUE)
if (!is.finite(prevalence_axis_upper) || prevalence_axis_upper <= 0) {
  prevalence_axis_upper <- 1
}
prevalence_axis_upper <- max(1, ceiling(prevalence_axis_upper * 1.25))

reason_prevalence_plot <- ggplot(
  plot_data,
  aes(x = reason_label, y = pct_all_households, fill = study_arm_label)
) +
  geom_col(position = position_arm, width = 0.64) +
  geom_text(
    aes(label = prevalence_label),
    position = position_arm,
    hjust = -0.08,
    size = 2.8,
    show.legend = FALSE
  ) +
  facet_wrap(vars(timepoint_label), nrow = 1) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = study_arm_colors, drop = FALSE) +
  scale_y_continuous(
    limits = c(0, prevalence_axis_upper),
    breaks = scales::breaks_pretty(n = 5),
    labels = scales::label_number(suffix = "%", accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Reasons for non-wood forest use",
    x = NULL,
    y = "All households (%)",
    fill = "Study arm"
  ) +
  theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D9D9D9", linewidth = 0.35),
    panel.border = element_rect(color = "#777777", fill = NA, linewidth = 0.35),
    panel.spacing.x = grid::unit(0.8, "lines"),
    strip.background = element_rect(
      fill = "#F2F2F2", color = "#BDBDBD", linewidth = 0.35
    ),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.y = element_text(size = 8.5),
    axis.text.x = element_text(size = 8.5),
    axis.title = element_text(size = 9.5),
    plot.title = element_text(face = "bold", size = 11, hjust = 0),
    plot.title.position = "plot",
    legend.position = "top",
    legend.justification = "left",
    legend.box.just = "left",
    legend.margin = margin(t = 0, r = 0, b = 2, l = 0),
    plot.margin = margin(t = 3, r = 18, b = 3, l = 4)
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

cost_plot_data <- plot_data %>%
  filter(n_cost_nonmissing_among_reason > 0) %>%
  mutate(
    sd_lower = pmax(
      0,
      mean_cost_bdt_among_reason - coalesce(sd_cost_bdt_among_reason, 0)
    ),
    sd_upper = mean_cost_bdt_among_reason + coalesce(sd_cost_bdt_among_reason, 0)
  )

cost_axis_upper <- max(cost_plot_data$sd_upper, na.rm = TRUE)
if (!is.finite(cost_axis_upper) || cost_axis_upper <= 0) {
  cost_axis_upper <- 1
}
cost_axis_upper <- ceiling((cost_axis_upper * 1.08) / 50) * 50

reason_cost_plot <- ggplot(
  cost_plot_data,
  aes(x = reason_label, y = mean_cost_bdt_among_reason, color = study_arm_label)
) +
  geom_errorbar(
    aes(ymin = sd_lower, ymax = sd_upper),
    position = position_arm,
    width = 0.28,
    linewidth = 0.45,
    na.rm = TRUE
  ) +
  geom_point(position = position_arm, size = 2.2) +
  facet_wrap(vars(timepoint_label), nrow = 1) +
  coord_flip() +
  scale_color_manual(values = study_arm_colors, drop = FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    limits = c(0, cost_axis_upper),
    breaks = scales::breaks_pretty(n = 5),
    labels = scales::label_number(accuracy = 1),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    title = "Cost of entering the forest among households selecting each reason",
    x = NULL,
    y = "Mean cost (BDT), with SD",
    color = "Study arm"
  ) +
  theme_minimal(base_size = 10, base_family = "sans") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#D9D9D9", linewidth = 0.35),
    panel.border = element_rect(color = "#777777", fill = NA, linewidth = 0.35),
    panel.spacing.x = grid::unit(0.8, "lines"),
    strip.background = element_rect(
      fill = "#F2F2F2", color = "#BDBDBD", linewidth = 0.35
    ),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.y = element_text(size = 8.5),
    axis.text.x = element_text(size = 8.5),
    axis.title = element_text(size = 9.5),
    plot.title = element_text(face = "bold", size = 11, hjust = 0),
    plot.title.position = "plot",
    legend.position = "none",
    plot.margin = margin(t = 3, r = 18, b = 3, l = 4)
  )

figure_note <- paste(
  "Reason percentages use all households as the denominator, including households that did not report non-wood forest use.",
  "The cost panel shows mean cost_forest_not_wood among households selecting each reason; error bars are +/- 1 SD and are truncated at zero.",
  "Households selecting multiple reasons contribute to each applicable reason-specific cost summary.",
  sep = "\n"
)

figure_grob <- gridExtra::arrangeGrob(
  reason_prevalence_plot,
  reason_cost_plot,
  ncol = 1,
  heights = c(1.05, 1),
  top = grid::textGrob(
    "Forest use for non-wood reasons and reported entry cost",
    x = grid::unit(0.01, "npc"),
    hjust = 0,
    gp = grid::gpar(fontfamily = "sans", fontsize = 15, fontface = "bold")
  ),
  bottom = grid::textGrob(
    figure_note,
    x = grid::unit(0.01, "npc"),
    hjust = 0,
    gp = grid::gpar(fontfamily = "sans", fontsize = 8.5, col = "#444444")
  )
)

ggplot2::ggsave(
  filename = file.path(dir_figures_reviewed,
                       "fig_descriptive_forest_use_reasons_cost.png"),
  plot = figure_grob,
  width = 12,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)
ggplot2::ggsave(
  filename = file.path(dir_figures_reviewed,
                       "fig_descriptive_forest_use_reasons_cost.pdf"),
  plot = figure_grob,
  width = 12,
  height = 9,
  units = "in",
  device = "pdf",
  bg = "white"
)

message(
  "Wrote figure: ",
  file.path(dir_figures_reviewed,
            "fig_descriptive_forest_use_reasons_cost.png")
)
message(
  "Wrote figure: ",
  file.path(dir_figures_reviewed,
            "fig_descriptive_forest_use_reasons_cost.pdf")
)

message("Forest-use reason and cost table/figure generation complete.")

})


################################################################################
# Consolidated from the manuscript-style figure section in this script
################################################################################
local({
old_tz_for_manuscript_style_figures <- Sys.getenv("TZ", unset = NA_character_)
on.exit({
  if (is.na(old_tz_for_manuscript_style_figures) || old_tz_for_manuscript_style_figures == "") {
    Sys.unsetenv("TZ")
  } else {
    Sys.setenv(TZ = old_tz_for_manuscript_style_figures)
  }
}, add = TRUE)
################################################################################
# RF105 reviewed manuscript-style figure generation
#
# Purpose:
#   Recreate reviewed analogues of selected draft/manuscript figures using the
#   checked RF105 data products and clean_final inputs. These figures preserve
#   the visual intent of the draft figures while making inputs,
#   denominators, and output locations explicit.
#
# Inputs:
#   4_data/clean_final/survey_refugee_household.rds
#   4_data/clean_final/pm25_pats_refugee_indoor.rds
#   7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_stove_daily_dataset_100_80_5_20.csv
#   7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_harassment_summary.csv
#   7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_physical_health_symptoms.csv
#
# Outputs:
#   Figures in 6_figures/RF105_reviewed_YYYYMMDD/:
#     fig_descriptive_stove_use_composite_panel.png
#     fig_descriptive_stove_exclusive_midline.tiff
#     fig_descriptive_stove_minutes_midline.tiff
#     fig_descriptive_respondent_time_changes.png
#     fig_descriptive_child_time_changes.png
#     fig_descriptive_harassment_summary.tiff
#     fig_descriptive_pm25_hourly_patterns.png
#     fig_descriptive_health_symptom_panel.png
#     fig_descriptive_food_dietary_scores.tiff
#     fig_descriptive_food_group_diversity_7d.png
#     fig_descriptive_hdds_24h_food_groups_score.png
#     fig_descriptive_food_sources_overall.png
#     fig_descriptive_food_sources_by_item.png
#     fig_descriptive_lpg_willingness_pay.png
#
#   Tables in 7_tables/RF105_reviewed_YYYYMMDD/:
#     table_descriptive_manuscript_*.csv
#     qa/table_descriptive_manuscript_figure_*.csv
#
# Notes:
#   This script intentionally does not run any impact models. Intervention impact
#   estimates are produced in 4_rdid_xgboost_20260805_2213.R. This file is for the
#   descriptive/manuscript-style manuscript figures only.
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

Sys.setenv(TZ = "Asia/Dhaka")

################################################################################
# Local helpers
################################################################################

as_number <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

collapse_collection_years <- function(x) {
  years <- sort(unique(as.integer(x[!is.na(x)])))
  if (length(years) == 0) {
    return(NA_character_)
  }
  paste(years, collapse = ", ")
}

month_axis_label <- function(base_label, collection_years) {
  if (length(collection_years) == 0 || is.na(collection_years) || !nzchar(collection_years)) {
    return(base_label)
  }
  paste0(base_label, " (data collected ", collection_years, ")")
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
      "\nRun 00_run_RF105_20260805_2213.R or the upstream reviewed script first."
    )
  }
  readr::read_csv(file, show_col_types = FALSE)
}

arm_colors <- c(
  comparison = "#430154",
  intervention = "#138B87",
  all_arms = "#6F6F6F"
)
stove_colors <- c(lpg = "#0072B2", biomass = "#D55E00")
change_colors <- c(more = "#2F8F5B", less = "#B6463A")

geocene_recoded_midline_timepoint <- "midline"
geocene_previous_recoded_midline_label <- "recode as midline"
geocene_stove_timepoint_levels <- c("baseline", "midline", "endline")

as_geocene_stove_timepoint <- function(x) {
  x_clean <- str_squish(str_to_lower(as.character(x)))
  x_clean <- if_else(
    x_clean == geocene_previous_recoded_midline_label,
    geocene_recoded_midline_timepoint,
    x_clean
  )
  factor(
    x_clean,
    levels = geocene_stove_timepoint_levels,
    ordered = TRUE
  )
}

manuscript_figure_targets <- tibble(
  figure_description = c(
    "Composite stove-use and energy-consumption panel",
    "Exclusive LPG stove use by month since receipt (2020 data collection)",
    "Daily stove-use minutes by fuel and month since receipt (2020 data collection)",
    "Child time-use changes",
    "Respondent time-use changes",
    "Harassment while collecting fuel",
    "Indoor PM2.5 by hour of day",
    "Child and caregiver physical health outcomes",
    "Reconstructed food consumption score and dietary diversity",
    "Seven-day food-group diversity (exploratory, not standard HDDS)",
    "Twenty-four-hour dietary diversity (11 observed HDDS groups)",
    "Household food acquisition source shares",
    "Food acquisition sources by food item",
    "LPG willingness to pay"
  ),
  reviewed_figure = c(
    "fig_descriptive_stove_use_composite_panel.png",
    "fig_descriptive_stove_exclusive_midline.tiff",
    "fig_descriptive_stove_minutes_midline.tiff",
    "fig_descriptive_child_time_changes.png",
    "fig_descriptive_respondent_time_changes.png",
    "fig_descriptive_harassment_summary.tiff",
    "fig_descriptive_pm25_hourly_patterns.png",
    "fig_descriptive_health_symptom_panel.png",
    "fig_descriptive_food_dietary_scores.tiff",
    "fig_descriptive_food_group_diversity_7d.png",
    "fig_descriptive_hdds_24h_food_groups_score.png",
    "fig_descriptive_food_sources_overall.png",
    "fig_descriptive_food_sources_by_item.png",
    "fig_descriptive_lpg_willingness_pay.png"
  ),
  output_folder = dir_figures_reviewed
)
write_reviewed_csv(
  manuscript_figure_targets,
  "table_descriptive_manuscript_figure_targets.csv",
  subfolder = "qa"
)

################################################################################
# Load reviewed inputs
################################################################################

survey_raw <- readRDS(file_survey_refugee_household) %>%
  add_rf105_aliases()

survey_population <- make_analysis_population(survey_raw)
survey <- survey_population$all_deduplicated

stove_daily_file <- resolve_reviewed_or_restricted_csv(
  "table_descriptive_stove_daily_dataset_100_80_5_20.csv"
)
stove_daily <- readr::read_csv(stove_daily_file, show_col_types = FALSE)

################################################################################
# Standardized fuel procurement, cooking, and cleaning time
################################################################################

fuel_time_variant <- "100_80_5_20"
fuel_time_source_url <- paste0(
  "https://mptf.undp.org/sites/default/files/documents/35000/",
  "20200512_annual_report_bangladesh_safeplus.pdf"
)

clean_nonnegative_fuel_time <- function(x) {
  value <- as_number(x)
  value[!is.finite(value) | value < 0] <- NA_real_
  value
}

lpg_refill_interval_days <- function(household_size) {
  household_size <- as_number(household_size)
  case_when(
    !is.finite(household_size) | household_size < 1 ~ NA_real_,
    household_size <= 3 ~ 47,
    household_size <= 5 ~ 38,
    household_size <= 7 ~ 32,
    household_size <= 9 ~ 29,
    household_size <= 11 ~ 24,
    household_size >= 12 ~ 21,
    TRUE ~ NA_real_
  )
}

fuel_time_mean <- function(x) {
  if (sum(!is.na(x)) == 0) NA_real_ else mean(x, na.rm = TRUE)
}

fuel_time_sd <- function(x) {
  if (sum(!is.na(x)) < 2) NA_real_ else stats::sd(x, na.rm = TRUE)
}

summarise_fuel_time_groups <- function(df) {
  df %>%
    summarise(
      n_households = sum(!is.na(value)),
      mean = fuel_time_mean(value),
      sd = fuel_time_sd(value),
      .groups = "drop"
    ) %>%
    mutate(
      se = if_else(n_households > 1, sd / sqrt(n_households), NA_real_),
      t_critical = vapply(
        n_households,
        function(n) {
          if (is.na(n) || n <= 1) NA_real_ else stats::qt(0.975, df = n - 1)
        },
        numeric(1)
      ),
      ci_lower = mean - t_critical * se,
      ci_upper = mean + t_critical * se
    ) %>%
    select(-t_critical)
}

refill_boundary_check <- lpg_refill_interval_days(
  c(NA, -1, 0, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 20)
)
refill_boundary_expected <- c(
  NA, NA, NA, 47, 47, 38, 38, 32, 32, 29, 29, 24, 24, 21, 21
)
stopifnot(isTRUE(all.equal(refill_boundary_check, refill_boundary_expected)))

survey_fuel_time_households <- survey %>%
  transmute(
    fcn_id,
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    hh_size = clean_nonnegative_fuel_time(num_col(., "hh_size")),
    times_wood_week = clean_nonnegative_fuel_time(num_col(., "times_wood_week")),
    biomass_walk_min = 60 * clean_nonnegative_fuel_time(
      num_col(., "collect_wood_walk_hr")
    ),
    receive_lpg_walk_min = 60 * clean_nonnegative_fuel_time(
      num_col(., "receive_lpg_walk")
    ),
    receive_lpg_wait_min = 60 * clean_nonnegative_fuel_time(
      num_col(., "receive_lpg_wait")
    ),
    buy_lpg_walk_min = 60 * clean_nonnegative_fuel_time(
      num_col(., "buy_lpg_walk")
    ),
    buy_lpg_wait_min = 60 * clean_nonnegative_fuel_time(
      num_col(., "buy_lpg_wait")
    )
  ) %>%
  mutate(
    hh_size = if_else(hh_size >= 1, hh_size, NA_real_),
    lpg_refill_interval_days = lpg_refill_interval_days(hh_size),
    lpg_refills_week = 7 / lpg_refill_interval_days,
    lpg_walk_min = coalesce(receive_lpg_walk_min, buy_lpg_walk_min),
    lpg_wait_min = coalesce(receive_lpg_wait_min, buy_lpg_wait_min)
  )

if (anyDuplicated(
  survey_fuel_time_households %>% transmute(key = paste(fcn_id, timepoint)) %>% pull(key)
) > 0) {
  stop("Fuel-time survey input has duplicate fcn_id-timepoint records.", call. = FALSE)
}

survey_fuel_time_long <- bind_rows(
  survey_fuel_time_households %>%
    transmute(
      fcn_id, study_arm_overall, timepoint,
      data_source = "survey", panel_order = 1L,
      panel = "A. Fuel procurement frequency (times/week)",
      stove_use_category = NA_character_, component = "Biomass",
      estimate_basis = "Survey reported", unit = "times/week",
      value = times_wood_week, display_in_figure = TRUE
    ),
  survey_fuel_time_households %>%
    transmute(
      fcn_id, study_arm_overall, timepoint,
      data_source = "survey", panel_order = 1L,
      panel = "A. Fuel procurement frequency (times/week)",
      stove_use_category = NA_character_, component = "LPG",
      estimate_basis = "SAFE Plus schedule", unit = "times/week",
      value = lpg_refills_week, display_in_figure = TRUE
    ),
  survey_fuel_time_households %>%
    transmute(
      fcn_id, study_arm_overall, timepoint,
      data_source = "survey", panel_order = 2L,
      panel = "B. Two-way walking time (minutes/procurement event)",
      stove_use_category = NA_character_, component = "Biomass",
      estimate_basis = "Survey reported", unit = "minutes/procurement event",
      value = biomass_walk_min, display_in_figure = TRUE
    ),
  survey_fuel_time_households %>%
    transmute(
      fcn_id, study_arm_overall, timepoint,
      data_source = "survey", panel_order = 2L,
      panel = "B. Two-way walking time (minutes/procurement event)",
      stove_use_category = NA_character_, component = "LPG",
      estimate_basis = "Survey reported", unit = "minutes/procurement event",
      value = lpg_walk_min, display_in_figure = TRUE
    ),
  survey_fuel_time_households %>%
    transmute(
      fcn_id, study_arm_overall, timepoint,
      data_source = "survey", panel_order = 3L,
      panel = "C. Additional procurement time (minutes/procurement event)",
      stove_use_category = NA_character_, component = "Waiting in line for LPG",
      estimate_basis = "Survey reported", unit = "minutes/procurement event",
      value = lpg_wait_min, display_in_figure = TRUE
    )
)

survey_fuel_time_summary <- survey_fuel_time_long %>%
  group_by(
    data_source, panel_order, panel, study_arm_overall, timepoint,
    stove_use_category, component, estimate_basis, unit, display_in_figure
  ) %>%
  summarise_fuel_time_groups() %>%
  mutate(
    denominator_note = paste0(
      "Household-level survey values; n_households is the nonmissing denominator. ",
      "LPG receipt walking/waiting fields are preferred, with purchase fields used ",
      "component-wise when receipt fields are missing."
    )
  )

survey_fuel_time_strata <- survey_fuel_time_households %>%
  filter(
    study_arm_overall %in% arm_levels,
    timepoint %in% timepoint_levels
  ) %>%
  distinct(study_arm_overall, timepoint)

survey_fuel_time_assumptions <- bind_rows(
  survey_fuel_time_strata %>%
    mutate(component = "Wood collection", mean = 30),
  survey_fuel_time_strata %>%
    mutate(component = "Wood preparation", mean = 12)
) %>%
  transmute(
    data_source = "assumption", panel_order = 3L,
    panel = "C. Additional procurement time (minutes/procurement event)",
    study_arm_overall, timepoint, stove_use_category = NA_character_, component,
    estimate_basis = "Prespecified assumption",
    unit = "minutes/procurement event",
    n_households = NA_integer_, mean, sd = NA_real_, se = NA_real_,
    ci_lower = mean, ci_upper = mean, display_in_figure = TRUE,
    denominator_note = paste0(
      "Fixed per-trip assumption; 30 minutes to collect biomass and 12 minutes ",
      "to prepare biomass for cooking."
    )
  )

fuel_time_flag <- function(x) {
  str_to_lower(str_squish(as.character(x))) %in% c("1", "true", "yes")
}

monitor_fuel_time_daily <- stove_daily %>%
  mutate(
    fcn_id = as.character(fcn_id),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    exclusive_lpg_flag = fuel_time_flag(exclusive_lpg_recalc),
    exclusive_biomass_flag = fuel_time_flag(exclusive_biomass_recalc),
    mixed_use_flag = fuel_time_flag(mixed_use_recalc)
  )

if (any(
  rowSums(as.matrix(monitor_fuel_time_daily %>% select(
    exclusive_lpg_flag, exclusive_biomass_flag, mixed_use_flag
  ))) > 1
)) {
  stop("A monitored household-day has more than one stove-use category.", call. = FALSE)
}

monitor_fuel_time_daily <- monitor_fuel_time_daily %>%
  mutate(
    stove_use_category = case_when(
      exclusive_lpg_flag ~ "exclusive_lpg",
      exclusive_biomass_flag ~ "exclusive_biomass",
      mixed_use_flag ~ "mixed_use",
      TRUE ~ NA_character_
    ),
    lpg_events = clean_nonnegative_fuel_time(cooking_events_with_lpg_zero),
    biomass_events = clean_nonnegative_fuel_time(cooking_events_with_biomass_zero),
    lpg_stove_on_min = clean_nonnegative_fuel_time(stove_on_min_sum_lpg_zero),
    biomass_stove_on_min = clean_nonnegative_fuel_time(stove_on_min_sum_biomass_zero),
    total_stove_on_min = if_else(
      !is.na(lpg_stove_on_min) & !is.na(biomass_stove_on_min),
      lpg_stove_on_min + biomass_stove_on_min,
      NA_real_
    ),
    lpg_cleaning_min = 5 * lpg_events,
    biomass_cleaning_min = 10 * biomass_events,
    total_cleaning_min = if_else(
      !is.na(lpg_cleaning_min) & !is.na(biomass_cleaning_min),
      lpg_cleaning_min + biomass_cleaning_min,
      NA_real_
    )
  ) %>%
  filter(
    !is.na(stove_use_category),
    study_arm_overall %in% arm_levels,
    timepoint %in% timepoint_levels
  )

mixed_monitor_daily <- monitor_fuel_time_daily %>%
  filter(stove_use_category == "mixed_use")
if (nrow(mixed_monitor_daily) > 0) {
  stopifnot(
    max(abs(
      mixed_monitor_daily$total_stove_on_min -
        mixed_monitor_daily$lpg_stove_on_min -
        mixed_monitor_daily$biomass_stove_on_min
    ), na.rm = TRUE) < 1e-8,
    max(abs(
      mixed_monitor_daily$total_cleaning_min -
        mixed_monitor_daily$lpg_cleaning_min -
        mixed_monitor_daily$biomass_cleaning_min
    ), na.rm = TRUE) < 1e-8
  )
}

monitor_fuel_time_households <- monitor_fuel_time_daily %>%
  group_by(fcn_id, study_arm_overall, timepoint, stove_use_category) %>%
  summarise(
    n_monitor_days = n(),
    lpg_stove_on_min = fuel_time_mean(lpg_stove_on_min),
    biomass_stove_on_min = fuel_time_mean(biomass_stove_on_min),
    total_stove_on_min = fuel_time_mean(total_stove_on_min),
    lpg_cleaning_min = fuel_time_mean(lpg_cleaning_min),
    biomass_cleaning_min = fuel_time_mean(biomass_cleaning_min),
    total_cleaning_min = fuel_time_mean(total_cleaning_min),
    .groups = "drop"
  )

monitor_fuel_time_long <- bind_rows(
  monitor_fuel_time_households %>%
    transmute(
      fcn_id, study_arm_overall, timepoint, stove_use_category,
      data_source = "stove_use_monitor", panel_order = 4L,
      panel = "D. Stove-on time (minutes/day)",
      component = case_when(
        stove_use_category == "exclusive_lpg" ~ "LPG",
        stove_use_category == "exclusive_biomass" ~ "Biomass",
        TRUE ~ "All fuels (total)"
      ),
      estimate_basis = "Stove-use monitor", unit = "minutes/day",
      value = total_stove_on_min,
      display_in_figure = stove_use_category != "mixed_use"
    ),
  monitor_fuel_time_households %>%
    filter(stove_use_category == "mixed_use") %>%
    transmute(
      fcn_id, study_arm_overall, timepoint, stove_use_category,
      data_source = "stove_use_monitor", panel_order = 4L,
      panel = "D. Stove-on time (minutes/day)", component = "LPG component",
      estimate_basis = "Stove-use monitor", unit = "minutes/day",
      value = lpg_stove_on_min, display_in_figure = TRUE
    ),
  monitor_fuel_time_households %>%
    filter(stove_use_category == "mixed_use") %>%
    transmute(
      fcn_id, study_arm_overall, timepoint, stove_use_category,
      data_source = "stove_use_monitor", panel_order = 4L,
      panel = "D. Stove-on time (minutes/day)", component = "Biomass component",
      estimate_basis = "Stove-use monitor", unit = "minutes/day",
      value = biomass_stove_on_min, display_in_figure = TRUE
    ),
  monitor_fuel_time_households %>%
    transmute(
      fcn_id, study_arm_overall, timepoint, stove_use_category,
      data_source = "stove_use_monitor", panel_order = 5L,
      panel = "E. Pot-cleaning time (minutes/day)",
      component = case_when(
        stove_use_category == "exclusive_lpg" ~ "LPG",
        stove_use_category == "exclusive_biomass" ~ "Biomass",
        TRUE ~ "All fuels (total)"
      ),
      estimate_basis = "Stove-use monitor + cleaning assumption",
      unit = "minutes/day", value = total_cleaning_min,
      display_in_figure = stove_use_category != "mixed_use"
    ),
  monitor_fuel_time_households %>%
    filter(stove_use_category == "mixed_use") %>%
    transmute(
      fcn_id, study_arm_overall, timepoint, stove_use_category,
      data_source = "stove_use_monitor", panel_order = 5L,
      panel = "E. Pot-cleaning time (minutes/day)", component = "LPG component",
      estimate_basis = "Stove-use monitor + cleaning assumption",
      unit = "minutes/day", value = lpg_cleaning_min,
      display_in_figure = TRUE
    ),
  monitor_fuel_time_households %>%
    filter(stove_use_category == "mixed_use") %>%
    transmute(
      fcn_id, study_arm_overall, timepoint, stove_use_category,
      data_source = "stove_use_monitor", panel_order = 5L,
      panel = "E. Pot-cleaning time (minutes/day)", component = "Biomass component",
      estimate_basis = "Stove-use monitor + cleaning assumption",
      unit = "minutes/day", value = biomass_cleaning_min,
      display_in_figure = TRUE
    )
)

monitor_fuel_time_summary <- monitor_fuel_time_long %>%
  group_by(
    data_source, panel_order, panel, study_arm_overall, timepoint,
    stove_use_category, component, estimate_basis, unit, display_in_figure
  ) %>%
  summarise_fuel_time_groups() %>%
  mutate(
    denominator_note = paste0(
      "Equal household weights after averaging monitored household-days within ",
      "household, arm, timepoint, and stove-use category. Mixed-use LPG and biomass ",
      "components are retained separately."
    )
  )

fuel_time_plot_data <- bind_rows(
  survey_fuel_time_summary,
  survey_fuel_time_assumptions,
  monitor_fuel_time_summary
) %>%
  mutate(
    stove_use_category_label = recode(
      stove_use_category,
      exclusive_lpg = "Excl. LPG",
      exclusive_biomass = "Excl. biomass",
      mixed_use = "Mixed use",
      .default = NA_character_
    ),
    group_label = if_else(
      data_source == "stove_use_monitor",
      paste(
        str_to_title(timepoint), stove_use_category_label,
        str_to_title(study_arm_overall), sep = "\n"
      ),
      paste(str_to_title(timepoint), str_to_title(study_arm_overall), sep = "\n")
    )
  ) %>%
  arrange(
    panel_order,
    factor(timepoint, levels = timepoint_levels),
    factor(study_arm_overall, levels = arm_levels),
    factor(stove_use_category, levels = c(
      "exclusive_lpg", "exclusive_biomass", "mixed_use"
    )),
    component
  )

write_reviewed_csv(
  fuel_time_plot_data,
  paste0("table_descriptive_fuel_time_burden_plot_data_", fuel_time_variant, ".csv")
)

fuel_time_plot_for_figure <- fuel_time_plot_data %>%
  filter(display_in_figure, !is.na(mean)) %>%
  mutate(
    panel = factor(panel, levels = unique(fuel_time_plot_data$panel)),
    group_label = factor(group_label, levels = unique(fuel_time_plot_data$group_label)),
    estimate_basis_figure = case_when(
      estimate_basis == "SAFE Plus schedule" ~ "Published refill schedule",
      estimate_basis == "Prespecified assumption" ~ "Prespecified duration",
      str_detect(estimate_basis, "Stove-use monitor") ~ "Stove-use monitor",
      TRUE ~ "Survey reported"
    ),
    plot_ci_lower = pmax(0, ci_lower)
  )

fuel_time_component_colors <- c(
  "Biomass" = "#D55E00",
  "LPG" = "#0072B2",
  "Biomass component" = "#A65628",
  "LPG component" = "#56B4E9",
  "Wood collection" = "#CC79A7",
  "Wood preparation" = "#E69F00",
  "Waiting in line for LPG" = "#009E73"
)

fuel_time_component_legend_order <- c(
  "Biomass", "LPG", "Biomass component", "LPG component",
  "Wood collection", "Wood preparation", "Waiting in line for LPG"
)

fig_fuel_time_burden <- ggplot(
  fuel_time_plot_for_figure,
  aes(x = group_label, y = mean, color = component, shape = estimate_basis_figure)
) +
  geom_errorbar(
    aes(ymin = plot_ci_lower, ymax = ci_upper),
    position = position_dodge(width = 0.65), width = 0.18, linewidth = 0.4
  ) +
  geom_point(position = position_dodge(width = 0.65), size = 2.1) +
  facet_wrap(~ panel, scales = "free", ncol = 1) +
  scale_color_manual(
    values = fuel_time_component_colors,
    breaks = fuel_time_component_legend_order,
    drop = FALSE
  ) +
  scale_shape_manual(values = c(
    "Survey reported" = 16,
    "Published refill schedule" = 17,
    "Prespecified duration" = 15,
    "Stove-use monitor" = 18
  )) +
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE, order = 1),
    shape = guide_legend(order = 2)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.12))) +
  theme_classic(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    strip.text = element_text(face = "bold", size = 9),
    panel.spacing = unit(1.1, "lines"),
    legend.position = "bottom",
    legend.box = "vertical"
  ) +
  labs(
    title = "Household time associated with fuel procurement, cooking, and cleaning",
    x = NULL,
    y = NULL,
    color = "Fuel or component",
    shape = "Source of data"
  )

save_plot_if_data(
  fuel_time_plot_for_figure,
  fig_fuel_time_burden,
  paste0("fig_descriptive_fuel_time_burden_components_", fuel_time_variant, ".png"),
  width = 11,
  height = 16
)

lpg_baseline_procurement <- survey_fuel_time_households %>%
  filter(timepoint == "baseline", study_arm_overall == "comparison") %>%
  mutate(
    walk_min_week = lpg_refills_week * lpg_walk_min,
    wait_min_week = lpg_refills_week * lpg_wait_min,
    complete_procurement = !is.na(lpg_refills_week) &
      !is.na(lpg_walk_min) & !is.na(lpg_wait_min)
  )

biomass_baseline_procurement <- survey_fuel_time_households %>%
  filter(timepoint == "baseline", study_arm_overall == "intervention") %>%
  mutate(
    walk_min_week = times_wood_week * biomass_walk_min,
    collection_min_week = times_wood_week * 30,
    preparation_min_week = times_wood_week * 12,
    complete_procurement = !is.na(times_wood_week) & !is.na(biomass_walk_min)
  )

lpg_complete_procurement <- lpg_baseline_procurement %>%
  filter(complete_procurement)
biomass_complete_procurement <- biomass_baseline_procurement %>%
  filter(complete_procurement)

lpg_procurement_week <- c(
  walking = fuel_time_mean(lpg_complete_procurement$walk_min_week),
  waiting = fuel_time_mean(lpg_complete_procurement$wait_min_week)
)
lpg_procurement_week <- c(
  lpg_procurement_week,
  fuel_gathering_total = sum(lpg_procurement_week)
)

biomass_procurement_week <- c(
  walking = fuel_time_mean(biomass_complete_procurement$walk_min_week),
  collection = fuel_time_mean(biomass_complete_procurement$collection_min_week),
  preparation = fuel_time_mean(biomass_complete_procurement$preparation_min_week)
)
biomass_procurement_week <- c(
  biomass_procurement_week,
  fuel_gathering_total = sum(biomass_procurement_week)
)

monitor_profile_households <- monitor_fuel_time_daily %>%
  filter(stove_use_category %in% c("exclusive_lpg", "exclusive_biomass")) %>%
  group_by(fcn_id, stove_use_category) %>%
  summarise(
    n_monitor_days = n(),
    cooking_min_day = fuel_time_mean(total_stove_on_min),
    cleaning_min_day = fuel_time_mean(total_cleaning_min),
    .groups = "drop"
  )

monitor_profile_summary <- monitor_profile_households %>%
  group_by(stove_use_category) %>%
  summarise(
    n_monitor_households = n(),
    cooking_min_day = fuel_time_mean(cooking_min_day),
    cleaning_min_day = fuel_time_mean(cleaning_min_day),
    .groups = "drop"
  )

monitor_profile_value <- function(category, variable) {
  value <- monitor_profile_summary %>%
    filter(stove_use_category == category) %>%
    pull(all_of(variable))
  if (length(value) == 0) NA_real_ else value[[1]]
}

lpg_cooking_day <- monitor_profile_value("exclusive_lpg", "cooking_min_day")
lpg_cleaning_day <- monitor_profile_value("exclusive_lpg", "cleaning_min_day")
lpg_monitor_n <- monitor_profile_value("exclusive_lpg", "n_monitor_households")
biomass_cooking_day <- monitor_profile_value("exclusive_biomass", "cooking_min_day")
biomass_cleaning_day <- monitor_profile_value("exclusive_biomass", "cleaning_min_day")
biomass_monitor_n <- monitor_profile_value("exclusive_biomass", "n_monitor_households")

lpg_profile_week <- c(
  lpg_procurement_week,
  cooking = 7 * lpg_cooking_day,
  cleaning = 7 * lpg_cleaning_day
)
lpg_profile_week <- c(
  lpg_profile_week,
  total = lpg_profile_week[["fuel_gathering_total"]] +
    lpg_profile_week[["cooking"]] + lpg_profile_week[["cleaning"]]
)

biomass_profile_week <- c(
  biomass_procurement_week,
  cooking = 7 * biomass_cooking_day,
  cleaning = 7 * biomass_cleaning_day
)
biomass_profile_week <- c(
  biomass_profile_week,
  total = biomass_profile_week[["fuel_gathering_total"]] +
    biomass_profile_week[["cooking"]] + biomass_profile_week[["cleaning"]]
)

fuel_time_profile_rows <- bind_rows(
  tibble(
    profile = "exclusive_lpg",
    component = names(lpg_profile_week),
    minutes_per_week = as.numeric(lpg_profile_week),
    survey_source_arm = "comparison",
    survey_source_timepoint = "baseline",
    monitor_category = "exclusive_lpg",
    n_survey_component_nonmissing = c(
      sum(!is.na(lpg_baseline_procurement$walk_min_week)),
      sum(!is.na(lpg_baseline_procurement$wait_min_week)),
      nrow(lpg_complete_procurement), NA, NA, NA
    ),
    n_survey_complete_procurement = nrow(lpg_complete_procurement),
    n_monitor_households = as.integer(lpg_monitor_n),
    source_population = c(
      "Baseline comparison survey households",
      "Baseline comparison survey households",
      "Baseline comparison survey households with complete LPG procurement inputs",
      "Households with monitored exclusive-LPG days, pooled across arm/timepoint",
      "Households with monitored exclusive-LPG days, pooled across arm/timepoint",
      "Standardized profile combining survey and monitor summaries"
    ),
    calculation = c(
      "mean[(7 / refill interval days) * two-way LPG walk minutes]",
      "mean[(7 / refill interval days) * LPG wait minutes]",
      "walking + waiting",
      "7 * mean household daily monitored stove-on minutes",
      "7 * mean household daily LPG events * 5 minutes/event",
      "fuel gathering + cooking + cleaning"
    )
  ),
  tibble(
    profile = "exclusive_biomass",
    component = names(biomass_profile_week),
    minutes_per_week = as.numeric(biomass_profile_week),
    survey_source_arm = "intervention",
    survey_source_timepoint = "baseline",
    monitor_category = "exclusive_biomass",
    n_survey_component_nonmissing = c(
      sum(!is.na(biomass_baseline_procurement$walk_min_week)),
      sum(!is.na(biomass_baseline_procurement$collection_min_week)),
      sum(!is.na(biomass_baseline_procurement$preparation_min_week)),
      nrow(biomass_complete_procurement), NA, NA, NA
    ),
    n_survey_complete_procurement = nrow(biomass_complete_procurement),
    n_monitor_households = as.integer(biomass_monitor_n),
    source_population = c(
      "Baseline intervention survey households",
      "Baseline intervention survey households",
      "Baseline intervention survey households",
      "Baseline intervention survey households with complete biomass procurement inputs",
      "Households with monitored exclusive-biomass days, pooled across arm/timepoint",
      "Households with monitored exclusive-biomass days, pooled across arm/timepoint",
      "Standardized profile combining survey and monitor summaries"
    ),
    calculation = c(
      "mean[firewood trips/week * two-way walk minutes/trip]",
      "mean[firewood trips/week * 30 collection minutes/trip]",
      "mean[firewood trips/week * 12 preparation minutes/trip]",
      "walking + collection + preparation",
      "7 * mean household daily monitored stove-on minutes",
      "7 * mean household daily biomass events * 10 minutes/event",
      "fuel gathering + cooking + cleaning"
    )
  )
) %>%
  mutate(
    minutes_per_day = minutes_per_week / 7,
    estimate_type = "standardized_profile"
  ) %>%
  select(
    profile, component, estimate_type, minutes_per_day, minutes_per_week,
    survey_source_arm, survey_source_timepoint, monitor_category,
    n_survey_component_nonmissing, n_survey_complete_procurement,
    n_monitor_households, source_population, calculation
  )

fuel_time_profile_differences <- fuel_time_profile_rows %>%
  filter(component %in% c("fuel_gathering_total", "cooking", "cleaning", "total")) %>%
  select(profile, component, minutes_per_day, minutes_per_week) %>%
  pivot_wider(
    names_from = profile,
    values_from = c(minutes_per_day, minutes_per_week)
  ) %>%
  transmute(
    profile = "biomass_minus_lpg",
    component,
    estimate_type = "standardized_profile_difference",
    minutes_per_day = minutes_per_day_exclusive_biomass -
      minutes_per_day_exclusive_lpg,
    minutes_per_week = minutes_per_week_exclusive_biomass -
      minutes_per_week_exclusive_lpg,
    survey_source_arm = "intervention minus comparison",
    survey_source_timepoint = "baseline",
    monitor_category = "exclusive_biomass minus exclusive_lpg",
    n_survey_component_nonmissing = NA_integer_,
    n_survey_complete_procurement = NA_integer_,
    n_monitor_households = NA_integer_,
    source_population = "Difference between standardized exclusive-use profiles",
    calculation = "exclusive biomass minus exclusive LPG; see profile rows for denominators"
  )

fuel_time_profiles <- bind_rows(
  fuel_time_profile_rows,
  fuel_time_profile_differences
)

profile_value <- function(profile_name, component_name, unit_name) {
  fuel_time_profiles %>%
    filter(profile == profile_name, component == component_name) %>%
    pull(all_of(unit_name)) %>%
    first(default = NA_real_)
}

for (profile_name in c("exclusive_lpg", "exclusive_biomass")) {
  expected_total <- sum(c(
    profile_value(profile_name, "fuel_gathering_total", "minutes_per_week"),
    profile_value(profile_name, "cooking", "minutes_per_week"),
    profile_value(profile_name, "cleaning", "minutes_per_week")
  ))
  observed_total <- profile_value(profile_name, "total", "minutes_per_week")
  stopifnot(isTRUE(all.equal(observed_total, expected_total, tolerance = 1e-8)))
}

write_reviewed_csv(
  fuel_time_profiles,
  paste0(
    "table_descriptive_fuel_time_budget_baseline_profiles_",
    fuel_time_variant, ".csv"
  )
)

# Standardized time saved when an intervention household moves from the
# baseline exclusive-biomass profile to the endline exclusive-LPG profile.
# Procurement inputs use survey households in the corresponding arm and round;
# cooking and cleaning inputs use household-weighted exclusive-use monitor days.
switch_lpg_endline_procurement <- survey_fuel_time_households %>%
  filter(timepoint == "endline", study_arm_overall == "intervention") %>%
  mutate(
    walk_min_week = lpg_refills_week * lpg_walk_min,
    wait_min_week = lpg_refills_week * lpg_wait_min,
    complete_procurement = !is.na(lpg_refills_week) &
      !is.na(lpg_walk_min) & !is.na(lpg_wait_min)
  )

switch_lpg_complete_procurement <- switch_lpg_endline_procurement %>%
  filter(complete_procurement)

switch_lpg_procurement_week <- c(
  walking = fuel_time_mean(switch_lpg_complete_procurement$walk_min_week),
  waiting = fuel_time_mean(switch_lpg_complete_procurement$wait_min_week)
)
switch_lpg_procurement_week <- c(
  switch_lpg_procurement_week,
  fuel_procurement = sum(switch_lpg_procurement_week)
)

switch_monitor_profiles <- monitor_fuel_time_households %>%
  filter(
    study_arm_overall == "intervention",
    (timepoint == "baseline" & stove_use_category == "exclusive_biomass") |
      (timepoint == "endline" & stove_use_category == "exclusive_lpg")
  ) %>%
  group_by(timepoint, stove_use_category) %>%
  summarise(
    n_monitor_households = n(),
    cooking_min_day = fuel_time_mean(total_stove_on_min),
    cleaning_min_day = fuel_time_mean(total_cleaning_min),
    .groups = "drop"
  )

switch_monitor_value <- function(timepoint_name, category_name, variable) {
  value <- switch_monitor_profiles %>%
    filter(
      timepoint == timepoint_name,
      stove_use_category == category_name
    ) %>%
    pull(all_of(variable))
  if (length(value) == 0) NA_real_ else value[[1]]
}

switch_baseline_biomass_week <- c(
  fuel_procurement = biomass_procurement_week[["fuel_gathering_total"]],
  cooking = 7 * switch_monitor_value(
    "baseline", "exclusive_biomass", "cooking_min_day"
  ),
  cleaning = 7 * switch_monitor_value(
    "baseline", "exclusive_biomass", "cleaning_min_day"
  )
)
switch_baseline_biomass_week <- c(
  switch_baseline_biomass_week,
  total = sum(switch_baseline_biomass_week)
)

switch_endline_lpg_week <- c(
  fuel_procurement = switch_lpg_procurement_week[["fuel_procurement"]],
  cooking = 7 * switch_monitor_value(
    "endline", "exclusive_lpg", "cooking_min_day"
  ),
  cleaning = 7 * switch_monitor_value(
    "endline", "exclusive_lpg", "cleaning_min_day"
  )
)
switch_endline_lpg_week <- c(
  switch_endline_lpg_week,
  total = sum(switch_endline_lpg_week)
)

switch_time_savings <- tibble(
  contrast = "baseline_exclusive_biomass_to_endline_exclusive_lpg",
  component = names(switch_baseline_biomass_week),
  baseline_biomass_minutes_per_week = as.numeric(switch_baseline_biomass_week),
  endline_lpg_minutes_per_week = as.numeric(switch_endline_lpg_week)
) %>%
  mutate(
    baseline_biomass_minutes_per_day = baseline_biomass_minutes_per_week / 7,
    endline_lpg_minutes_per_day = endline_lpg_minutes_per_week / 7,
    minutes_saved_per_week = baseline_biomass_minutes_per_week -
      endline_lpg_minutes_per_week,
    minutes_saved_per_day = minutes_saved_per_week / 7,
    hours_saved_per_week = minutes_saved_per_week / 60,
    hours_saved_per_day = minutes_saved_per_day / 60,
    baseline_survey_arm = "intervention",
    baseline_survey_timepoint = "baseline",
    endline_survey_arm = "intervention",
    endline_survey_timepoint = "endline",
    n_baseline_survey_complete_procurement = nrow(biomass_complete_procurement),
    n_endline_survey_complete_procurement = nrow(switch_lpg_complete_procurement),
    n_baseline_exclusive_biomass_monitor_households = as.integer(
      switch_monitor_value(
        "baseline", "exclusive_biomass", "n_monitor_households"
      )
    ),
    n_endline_exclusive_lpg_monitor_households = as.integer(
      switch_monitor_value("endline", "exclusive_lpg", "n_monitor_households")
    ),
    estimate_type = "standardized_counterfactual_profile_difference",
    interpretation = paste0(
      "Positive savings indicate less household time under the standardized ",
      "endline exclusive-LPG profile. This is not a paired within-household estimate."
    ),
    calculation = case_when(
      component == "fuel_procurement" ~
        "baseline biomass walking + collection + preparation minus endline LPG walking + waiting",
      component == "cooking" ~
        "baseline exclusive-biomass monitored stove-on time minus endline exclusive-LPG monitored stove-on time",
      component == "cleaning" ~
        "baseline biomass events * 10 minutes minus endline LPG events * 5 minutes",
      component == "total" ~
        "fuel procurement + cooking + cleaning",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    contrast, component,
    baseline_biomass_minutes_per_day, endline_lpg_minutes_per_day,
    minutes_saved_per_day, hours_saved_per_day,
    baseline_biomass_minutes_per_week, endline_lpg_minutes_per_week,
    minutes_saved_per_week, hours_saved_per_week,
    baseline_survey_arm, baseline_survey_timepoint,
    endline_survey_arm, endline_survey_timepoint,
    n_baseline_survey_complete_procurement,
    n_endline_survey_complete_procurement,
    n_baseline_exclusive_biomass_monitor_households,
    n_endline_exclusive_lpg_monitor_households,
    estimate_type, interpretation, calculation
  )

write_reviewed_csv(
  switch_time_savings,
  paste0(
    "table_descriptive_fuel_time_saved_biomass_baseline_to_lpg_endline_",
    fuel_time_variant, ".csv"
  )
)

switch_time_plot_data <- bind_rows(
  tibble(
    profile = "Baseline exclusive biomass",
    component = c("Fuel procurement", "Cooking", "Cleaning"),
    hours_per_week = as.numeric(
      switch_baseline_biomass_week[c("fuel_procurement", "cooking", "cleaning")]
    ) / 60
  ),
  tibble(
    profile = "Endline exclusive LPG",
    component = c("Fuel procurement", "Cooking", "Cleaning"),
    hours_per_week = as.numeric(
      switch_endline_lpg_week[c("fuel_procurement", "cooking", "cleaning")]
    ) / 60
  )
) %>%
  mutate(
    profile = factor(
      profile,
      levels = c("Baseline exclusive biomass", "Endline exclusive LPG")
    ),
    component = factor(
      component,
      levels = c("Fuel procurement", "Cooking", "Cleaning")
    )
  )

switch_total_savings <- switch_time_savings %>%
  filter(component == "total")
switch_profile_totals <- switch_time_plot_data %>%
  group_by(profile) %>%
  summarise(hours_per_week = sum(hours_per_week), .groups = "drop")

fig_switch_time_savings <- ggplot(
  switch_time_plot_data,
  aes(x = profile, y = hours_per_week, fill = component)
) +
  geom_col(width = 0.68) +
  geom_text(
    data = switch_profile_totals,
    aes(
      x = profile,
      y = hours_per_week,
      label = sprintf("%.1f h/week", hours_per_week)
    ),
    inherit.aes = FALSE,
    vjust = -0.45,
    fontface = "bold",
    size = 3.8
  ) +
  scale_fill_manual(
    values = c(
      "Fuel procurement" = "#0072B2",
      "Cooking" = "#D55E00",
      "Cleaning" = "#009E73"
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10),
    plot.caption = element_text(hjust = 0, size = 8),
    plot.caption.position = "plot"
  ) +
  labs(
    title = "Estimated household time saved by switching from biomass to LPG",
    subtitle = sprintf(
      paste0(
        "Standardized intervention-arm profiles: %.1f hours saved per week ",
        "(%.1f hours per day)"
      ),
      switch_total_savings$hours_saved_per_week,
      switch_total_savings$hours_saved_per_day
    ),
    x = NULL,
    y = "Household time (hours per week)",
    fill = "Time component",
    caption = stringr::str_wrap(
      paste0(
        "Procurement inputs use intervention-arm survey data. Cooking and cleaning ",
        "inputs use household-weighted monitored exclusive-use days. This is a ",
        "standardized contrast, not a paired within-household estimate."
      ),
      width = 105
    )
  )

save_plot_if_data(
  switch_time_plot_data,
  fig_switch_time_savings,
  paste0(
    "fig_descriptive_fuel_time_saved_biomass_baseline_to_lpg_endline_",
    fuel_time_variant, ".png"
  ),
  width = 8,
  height = 6
)

fuel_time_assumptions <- bind_rows(
  tibble(
    assumption_id = "firewood_frequency_alias",
    value = "times_wood_week = times_wood_day",
    unit = "trips/week",
    formula = "Direct alias; no numerical transformation",
    source_variables = "times_wood_day",
    analytic_variable = "times_wood_week",
    source = "Questionnaire wording",
    note = paste0(
      "The historical field name implies daily frequency, but the questionnaire ",
      "asks how many times in one week the household collects firewood."
    )
  ),
  tibble(
    assumption_id = paste0(
      "lpg_refill_interval_household_size_",
      c("1_3", "4_5", "6_7", "8_9", "10_11", "12_plus")
    ),
    value = as.character(c(47, 38, 32, 29, 24, 21)),
    unit = "days/refill",
    formula = "Expected refills/week = 7 / days/refill",
    source_variables = "hh_size",
    analytic_variable = "lpg_refills_week",
    source = fuel_time_source_url,
    note = "SAFE Plus household-size-specific LPG refill schedule."
  ),
  tribble(
    ~assumption_id, ~value, ~unit, ~formula, ~source_variables, ~analytic_variable, ~source, ~note,
    "biomass_collection_time", "30", "minutes/trip", "times_wood_week * 30", "times_wood_day", "biomass_collection_min_week", "Prespecified by investigators", "Applied once per reported weekly firewood collection trip.",
    "biomass_preparation_time", "12", "minutes/trip", "times_wood_week * 12", "times_wood_day", "biomass_preparation_min_week", "Prespecified by investigators", "Applied once per reported weekly firewood collection trip.",
    "lpg_cleaning_time", "5", "minutes/cooking event", "LPG monitored events/day * 5", "cooking_events_with_lpg_zero", "lpg_cleaning_min", "Prespecified by investigators", "Applied to monitored LPG cooking events.",
    "biomass_cleaning_time", "10", "minutes/cooking event", "Biomass monitored events/day * 10", "cooking_events_with_biomass_zero", "biomass_cleaning_min", "Prespecified by investigators", "Applied to monitored biomass cooking events.",
    "lpg_walk_wait_precedence", "receive then buy", "not applicable", "coalesce(receive_lpg_*, buy_lpg_*)", "receive_lpg_walk; receive_lpg_wait; buy_lpg_walk; buy_lpg_wait", "lpg_walk_min; lpg_wait_min", "RF105 survey", "Receipt fields are preferred separately for walking and waiting; purchase fields are fallbacks.",
    "monitor_weighting", "equal household weights", "not applicable", "Mean household-day value within household, then mean across households", "stove-use daily dataset", "monitor summaries", "RF105 Geocene variant 100_80_5_20", "Prevents households with more monitored days from receiving greater weight.",
    "mixed_use_components", "component-wise", "not applicable", "LPG and biomass monitored minutes/events retained separately and summed for totals", "fuel-specific monitored events and minutes", "mixed-use time components", "RF105 Geocene variant 100_80_5_20", "Cleaning uses 5 minutes per LPG event plus 10 minutes per biomass event.",
    "baseline_biomass_to_endline_lpg_contrast", "intervention arm", "not applicable", "baseline exclusive-biomass standardized profile minus endline exclusive-LPG standardized profile", "survey procurement inputs; exclusive-use monitored cooking events and stove-on minutes", "hours_saved_per_day; hours_saved_per_week", "RF105 survey and Geocene variant 100_80_5_20", "Positive values indicate less household time under the endline LPG profile; this is not a paired within-household estimate."
  )
)

write_reviewed_csv(
  fuel_time_assumptions,
  "table_descriptive_fuel_time_burden_assumptions.csv",
  subfolder = "qa"
)

fuel_time_methods_paragraph <- paste0(
  "We estimated household time spent obtaining fuel, cooking, and cleaning by ",
  "combining survey responses, published LPG refill schedules, stove-use monitor ",
  "data, and prespecified task-duration assumptions. We interpreted the questionnaire ",
  "item on firewood collection frequency as trips per week, consistent with the ",
  "questionnaire wording. Expected LPG procurement frequency was calculated as seven ",
  "divided by the household-size-specific refill interval of 47, 38, 32, 29, 24, or ",
  "21 days for households with 1-3, 4-5, 6-7, 8-9, 10-11, or 12 or more members, ",
  "respectively, based on the SAFE Plus refill schedule (United Nations Joint ",
  "Programme SAFE Plus, 2020). Biomass procurement time included reported two-way ",
  "walking time plus 30 minutes for collection and 12 minutes for preparation per ",
  "trip, whereas LPG procurement time included reported two-way walking and waiting ",
  "time. Cooking time was estimated from monitored stove-on minutes, and cleaning ",
  "time was estimated by multiplying monitored cooking events by five minutes for LPG ",
  "and 10 minutes for biomass. For the exclusive-use profiles, LPG procurement inputs ",
  "came from comparison households at baseline and biomass procurement inputs came ",
  "from intervention households at baseline; monitored exclusive-LPG and exclusive-",
  "biomass cooking data were pooled across study arms and timepoints. We averaged ",
  "household-level values with equal household weighting and converted components to ",
  "common daily and weekly units before summing them. Because procurement and monitored ",
  "cooking components came from different analytic subsets, the totals represent ",
  "standardized time-use profiles rather than directly observed totals for individual ",
  "households. We additionally estimated time saved under a switch from exclusive ",
  "biomass use at baseline to exclusive LPG use at endline among intervention households. ",
  "The baseline profile combined baseline intervention-arm biomass procurement inputs ",
  "with monitored baseline intervention household-days classified as exclusive biomass; ",
  "the endline profile combined endline intervention-arm LPG procurement inputs with ",
  "monitored endline intervention household-days classified as exclusive LPG. We ",
  "subtracted the endline LPG profile from the baseline biomass profile and converted ",
  "the difference to hours per household per day and week. This contrast is a standardized ",
  "counterfactual profile comparison, not a paired within-household change estimate."
)

fuel_time_methods_file <- file.path(
  dir_tables_qa,
  "methods_time_use_calculations.md"
)
readr::write_lines(
  c(
    "# Fuel-time calculation methods",
    "",
    fuel_time_methods_paragraph,
    "",
    paste0("Refill-schedule source: ", fuel_time_source_url)
  ),
  fuel_time_methods_file
)
message("Wrote QA methods note: ", fuel_time_methods_file)

negative_survey_time_values <- survey %>%
  transmute(
    across(
      any_of(c(
        "times_wood_week", "collect_wood_walk_hr", "receive_lpg_walk",
        "receive_lpg_wait", "buy_lpg_walk", "buy_lpg_wait"
      )),
      as_number
    )
  ) %>%
  unlist(use.names = FALSE) %>%
  { sum(. < 0, na.rm = TRUE) }

switch_total_row <- switch_time_savings %>%
  filter(component == "total")
switch_component_rows <- switch_time_savings %>%
  filter(component != "total")

stopifnot(
  isTRUE(all.equal(
    switch_total_row$baseline_biomass_minutes_per_week,
    sum(switch_component_rows$baseline_biomass_minutes_per_week),
    tolerance = 1e-8
  )),
  isTRUE(all.equal(
    switch_total_row$endline_lpg_minutes_per_week,
    sum(switch_component_rows$endline_lpg_minutes_per_week),
    tolerance = 1e-8
  )),
  isTRUE(all.equal(
    switch_total_row$hours_saved_per_week,
    switch_total_row$minutes_saved_per_week / 60,
    tolerance = 1e-8
  )),
  isTRUE(all.equal(
    switch_total_row$hours_saved_per_day,
    switch_total_row$hours_saved_per_week / 7,
    tolerance = 1e-8
  ))
)

fuel_time_qa <- tibble(
  check = c(
    "lpg_refill_household_size_boundaries",
    "survey_household_timepoint_uniqueness",
    "negative_survey_time_values_cleaned",
    "mixed_stove_on_total_equals_components",
    "mixed_cleaning_total_equals_components",
    "exclusive_profile_totals_equal_components",
    "baseline_biomass_endline_lpg_totals_equal_components",
    "baseline_biomass_endline_lpg_hours_conversion",
    "public_plot_data_has_no_restricted_fields",
    "public_profile_data_has_no_restricted_fields",
    "public_assumptions_have_no_restricted_fields",
    "public_switch_savings_data_has_no_restricted_fields"
  ),
  status = "pass",
  detail = c(
    "Tested missing, invalid, and every household-size schedule boundary.",
    "One deduplicated survey record per fcn_id-timepoint.",
    paste0(
      negative_survey_time_values,
      " negative source values were converted to missing before calculation."
    ),
    "Mixed total stove-on minutes equal LPG plus biomass minutes.",
    "Mixed cleaning minutes equal 5*LPG events plus 10*biomass events.",
    "Fuel gathering, cooking, and cleaning sum to each exclusive-profile total.",
    paste0(
      "Baseline biomass and endline LPG procurement, cooking, and cleaning ",
      "components sum to their respective profile totals."
    ),
    "Hours saved equal minutes saved / 60; daily hours equal weekly hours / 7.",
    paste(restricted_fields_present(fuel_time_plot_data), collapse = "; "),
    paste(restricted_fields_present(fuel_time_profiles), collapse = "; "),
    paste(restricted_fields_present(fuel_time_assumptions), collapse = "; "),
    paste(restricted_fields_present(switch_time_savings), collapse = "; ")
  )
) %>%
  mutate(
    detail = if_else(
      check %in% c(
        "public_plot_data_has_no_restricted_fields",
        "public_profile_data_has_no_restricted_fields",
        "public_assumptions_have_no_restricted_fields",
        "public_switch_savings_data_has_no_restricted_fields"
      ) & detail == "",
      "No restricted fields detected.",
      detail
    )
  )

stopifnot(
  length(restricted_fields_present(fuel_time_plot_data)) == 0,
  length(restricted_fields_present(fuel_time_profiles)) == 0,
  length(restricted_fields_present(fuel_time_assumptions)) == 0,
  length(restricted_fields_present(switch_time_savings)) == 0
)

write_reviewed_csv(
  fuel_time_qa,
  "table_qa_fuel_time_calculation_checks.csv",
  subfolder = "qa"
)

pm_indoor <- readRDS(file_pm25_indoor)

################################################################################
# Composite stove-use and energy-consumption panel like RF105A fig3/fig4
################################################################################

if (!requireNamespace("gridExtra", quietly = TRUE)) {
  stop(
    "Package gridExtra is required for the stove-use composite panel. Run renv::restore() from the project root, then rerun this script.",
    call. = FALSE
  )
}

# Geocene figures are generated by the shared primary/sensitivity pipeline.
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
  "table_descriptive_respondent_time_changes.csv"
)
write_reviewed_csv(
  child_time_plot_data,
  "table_descriptive_child_time_changes.csv"
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
  "fig_descriptive_respondent_time_changes.png",
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
  "fig_descriptive_child_time_changes.png",
  width = 11,
  height = 5.5
)

################################################################################
# Caregiver and child time-use change figures
# Integrated from 3.4_caregiver_child_time_figures_20260924.R.
# Uses the reviewed output directories established by the main configuration.
################################################################################
local({
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  stop(
    "Package `gridExtra` is required to assemble the combined figures. ",
    "Run renv::restore() from the project root, then rerun this script.",
    call. = FALSE
  )
}

change_colors <- c(more = "#2F8F5B", less = "#B6463A")

as_number_local <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

coalesce_vars_local <- function(df, vars) {
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

prop_ci_percent_local <- function(x, n) {
  if (is.na(n) || n <= 0 || is.na(x)) {
    return(c(NA_real_, NA_real_))
  }

  ci <- tryCatch(
    suppressWarnings(stats::prop.test(x = x, n = n, correct = FALSE)$conf.int),
    error = function(e) c(NA_real_, NA_real_)
  )
  100 * ci
}

add_prop_ci_local <- function(df, x_var, n_var,
                              lower_name = "ci_lower", upper_name = "ci_upper") {
  if (nrow(df) == 0) {
    df[[lower_name]] <- numeric(0)
    df[[upper_name]] <- numeric(0)
    return(df)
  }

  ci <- t(mapply(prop_ci_percent_local, df[[x_var]], df[[n_var]]))
  df[[lower_name]] <- ci[, 1]
  df[[upper_name]] <- ci[, 2]
  df
}

make_time_more_less_summary_local <- function(df, time_vars) {
  plot_data_long <- purrr::pmap_dfr(
    time_vars,
    function(source_variable, source_variables, outcome_label, display_order) {
      tibble(
        source_variable = source_variable,
        source_variables_used = paste(source_variables, collapse = "; "),
        outcome_label = outcome_label,
        display_order = display_order,
        response_code = as_number_local(coalesce_vars_local(df, source_variables))
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
    add_prop_ci_local("n_category", "n_more_less") %>%
    mutate(
      signed_ci_lower = if_else(change == "less", -ci_upper, ci_lower),
      signed_ci_upper = if_else(change == "less", -ci_lower, ci_upper),
      change = factor(change, levels = c("less", "more")),
      outcome_label = factor(outcome_label, levels = time_vars$outcome_label)
    ) %>%
    arrange(display_order, change)
}

make_time_change_plot <- function(plot_data, plot_title, show_legend = TRUE) {
  legend_position <- if (isTRUE(show_legend)) "bottom" else "none"

  ggplot(
    plot_data,
    aes(x = outcome_label, y = signed_percent / 100, fill = change)
  ) +
    geom_col(position = position_dodge(width = 0.75), width = 0.65) +
    geom_errorbar(
      aes(ymin = signed_ci_lower / 100, ymax = signed_ci_upper / 100),
      position = position_dodge(width = 0.75), width = 0.2
    ) +
    geom_hline(yintercept = 0, color = "grey35") +
    scale_fill_manual(values = change_colors, labels = c(less = "Less", more = "More")) +
    scale_y_continuous(
      limits = c(-1, 1),
      breaks = seq(-1, 1, by = 0.5),
      labels = scales::percent_format(accuracy = 1)
    ) +
    theme_classic(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = legend_position,
      plot.title = element_text(face = "bold")
    ) +
    labs(
      title = plot_title,
      x = "Activity",
      y = "Percent of more/less responses",
      fill = "Time spent"
    )
}

make_drudgery_summary <- function(df) {
  drudgery_labels <- tibble(
    task_code = c(1, 2, 3, 4, 5, 6, 7, 88),
    task_label = c(
      "Cooking",
      "Washing dishes",
      "Washing clothes",
      "Collecting water",
      "Harvesting wood",
      "Caring for children",
      "Caring for others",
      "Nothing is difficult"
    ),
    display_order = c(1, 2, 3, 4, 5, 6, 7, 99)
  )

  if (!all(c("drudgery_most_diff", "drudgery_second_most_diff") %in% names(df))) {
    return(tibble(
      task_code = numeric(0), task_label = character(0),
      display_order = numeric(0), n_most_difficult = integer(0),
      n_second_most_difficult = integer(0), n_either_most_or_second = integer(0),
      n_any_valid_ranking = integer(0), percent_either_most_or_second = numeric(0),
      ci_lower = numeric(0), ci_upper = numeric(0),
      n_same_task_recorded_as_most_and_second = integer(0)
    ))
  }

  most_code <- as_number_local(df$drudgery_most_diff)
  second_code <- as_number_local(df$drudgery_second_most_diff)
  valid_codes <- drudgery_labels$task_code
  has_any_valid <- most_code %in% valid_codes | second_code %in% valid_codes
  n_any_valid <- sum(has_any_valid, na.rm = TRUE)
  n_same <- sum(
    !is.na(most_code) & !is.na(second_code) &
      most_code == second_code & most_code %in% valid_codes,
    na.rm = TRUE
  )

  out <- drudgery_labels %>%
    rowwise() %>%
    mutate(
      n_most_difficult = sum(most_code == task_code, na.rm = TRUE),
      n_second_most_difficult = sum(second_code == task_code, na.rm = TRUE),
      n_either_most_or_second = sum(
        (most_code == task_code | second_code == task_code) %in% TRUE,
        na.rm = TRUE
      ),
      n_any_valid_ranking = n_any_valid,
      percent_either_most_or_second = if_else(
        n_any_valid_ranking > 0,
        100 * n_either_most_or_second / n_any_valid_ranking,
        NA_real_
      ),
      n_same_task_recorded_as_most_and_second = n_same
    ) %>%
    ungroup() %>%
    add_prop_ci_local("n_either_most_or_second", "n_any_valid_ranking") %>%
    arrange(display_order)

  out
}

make_drudgery_inset_plot <- function(drudgery_data) {
  plot_data <- drudgery_data %>%
    filter(task_code != 88, n_either_most_or_second > 0) %>%
    mutate(
      task_label = forcats::fct_reorder(task_label, percent_either_most_or_second)
    )

  ggplot(plot_data, aes(x = task_label, y = percent_either_most_or_second / 100)) +
    geom_col(fill = "#5B6C8F", width = 0.7) +
    coord_flip() +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.08))
    ) +
    theme_classic(base_size = 8) +
    theme(
      plot.background = element_rect(fill = "white", color = "grey55", linewidth = 0.25),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(face = "bold", size = 8, margin = margin(b = 2)),
      axis.title = element_blank(),
      axis.text = element_text(size = 6),
      axis.ticks.y = element_blank(),
      plot.margin = margin(4, 5, 4, 5)
    ) +
    labs(title = "Most or second-most difficult")
}

survey_raw <- readRDS(file_survey_refugee_household) %>%
  add_rf105_aliases()

survey_population <- make_analysis_population(survey_raw)
survey <- survey_population$all_deduplicated

survey_midline_intervention <- survey %>%
  filter(timepoint == "midline", study_arm_overall == "intervention")

caregiver_time_vars <- tibble::tibble(
  source_variable = c(
    "time_cooking",
    "time_washing_dishes",
    "time_washing_clothes",
    "time_collecting_water",
    "time_harvesting_wood",
    "time_gathering_nonwood_items",
    "time_caring_for_children",
    "time_caring_for_others",
    "time_accompanying_children",
    "time_employment_ngo",
    "time_unskilled_labor",
    "time_selling_food",
    "time_learning",
    "time_socializing",
    "time_sleeping",
    "time_eating",
    "time_nothing"
  ),
  source_variables = list(
    "time_cooking",
    "time_washing_dishes",
    "time_washing_clothes",
    "time_collecting_water",
    "time_harvesting_wood",
    c("time_gathering_nonwood_items", "time_gathering_non.wood_items", "time_gathering_non-wood_items"),
    "time_caring_for_children",
    "time_caring_for_others",
    "time_accompanying_children",
    "time_employment_ngo",
    "time_unskilled_labor",
    "time_selling_food",
    "time_learning",
    "time_socializing",
    "time_sleeping",
    "time_eating",
    "time_nothing"
  ),
  outcome_label = c(
    "Cooking",
    "Washing dishes",
    "Washing clothes",
    "Collecting water",
    "Harvesting wood",
    "Gathering non-wood items",
    "Caring for children",
    "Caring for others",
    "Accompanying children",
    "NGO employment",
    "Unskilled labor",
    "Selling food",
    "Learning",
    "Socializing",
    "Sleeping",
    "Eating",
    "Doing nothing"
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

coverage_table <- bind_rows(
  caregiver_time_vars %>%
    select(source_variable, source_variables, outcome_label, display_order) %>%
    mutate(panel = "caregiver"),
  child_time_vars %>%
    select(source_variable, source_variables, outcome_label, display_order) %>%
    mutate(panel = "child")
) %>%
  mutate(
    source_variables_used = vapply(source_variables, paste, character(1), collapse = "; "),
    source_variables_available = vapply(source_variables, function(vars) {
      paste(vars[vars %in% names(survey_midline_intervention)], collapse = "; ")
    }, character(1)),
    n_source_variables_available = vapply(source_variables, function(vars) {
      sum(vars %in% names(survey_midline_intervention))
    }, integer(1)),
    status = if_else(n_source_variables_available > 0, "available", "missing_from_clean_final")
  ) %>%
  select(panel, source_variable, source_variables_used, source_variables_available,
         n_source_variables_available, outcome_label, display_order, status)

write_reviewed_csv(
  coverage_table,
  "table_descriptive_caregiver_child_time_variable_coverage.csv",
  subfolder = "qa"
)

caregiver_time_plot_data <- make_time_more_less_summary_local(
  survey_midline_intervention,
  caregiver_time_vars
)
child_time_plot_data <- make_time_more_less_summary_local(
  survey_midline_intervention,
  child_time_vars
)
drudgery_plot_data <- make_drudgery_summary(survey_midline_intervention)

write_reviewed_csv(
  caregiver_time_plot_data,
  "table_descriptive_caregiver_time_changes.csv"
)
write_reviewed_csv(
  drudgery_plot_data,
  "table_descriptive_caregiver_drudgery_most_or_second_most_difficult.csv"
)

fig_time_caregiver <- make_time_change_plot(
  caregiver_time_plot_data,
  "Caregiver-reported change in time use after households started receiving LPG"
)

fig_time_child <- make_time_change_plot(
  child_time_plot_data,
  "Caregiver-reported change in child time use after households started receiving LPG"
)

save_reviewed_plot(
  fig_time_caregiver,
  "fig_descriptive_caregiver_time_changes.png",
  width = 14,
  height = 6
)

fig_time_caregiver_combined <- make_time_change_plot(
  caregiver_time_plot_data,
  "A. Caregivers",
  show_legend = TRUE
)
fig_time_child_combined <- make_time_change_plot(
  child_time_plot_data,
  "B. Children",
  show_legend = FALSE
)

fig_caregiver_child_combined <- gridExtra::arrangeGrob(
  fig_time_caregiver_combined,
  fig_time_child_combined,
  ncol = 1,
  heights = c(1.1, 0.9),
  top = grid::textGrob(
    "Reported change in time use after households started receiving LPG",
    gp = grid::gpar(fontface = "bold", fontsize = 14)
  )
)

save_reviewed_plot(
  fig_caregiver_child_combined,
  "fig_descriptive_caregiver_child_time_changes.png",
  width = 14,
  height = 10.5
)

drudgery_inset <- make_drudgery_inset_plot(drudgery_plot_data)

fig_time_caregiver_with_inset <- fig_time_caregiver_combined +
  annotation_custom(
    grob = ggplotGrob(drudgery_inset),
    xmin = 0.35, xmax = 6.35,
    ymin = 0.35, ymax = 0.98
  )

fig_caregiver_child_with_drudgery <- gridExtra::arrangeGrob(
  fig_time_caregiver_with_inset,
  fig_time_child_combined,
  ncol = 1,
  heights = c(1.1, 0.9),
  top = grid::textGrob(
    "Reported change in time use after households started receiving LPG",
    gp = grid::gpar(fontface = "bold", fontsize = 14)
  )
)

save_reviewed_plot(
  fig_caregiver_child_with_drudgery,
  "fig_descriptive_caregiver_child_time_changes_with_drudgery.png",
  width = 14,
  height = 10.5
)

message("Caregiver and child time-use figures complete.")

})


################################################################################
# Harassment figure for the manuscript harassment draft plot
################################################################################

harassment_detail <- read_reviewed_csv_required("table_descriptive_harassment_summary.csv")

harassment_plot_data <- harassment_detail %>%
  filter(
    timepoint == "midline",
    fuel_type %in% c("gather_scraps", "collect_wood", "receive_lpg"),
    harassment_type %in% c("insult", "belittle", "scare", "push", "hit", "kick", "choke", "weapon"),
    num_people_collect_fuel > 0,
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
  "table_descriptive_harassment_plot_data.csv"
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
    y = "Percentage of fuel collectors reporting harassment while collecting fuel"
  )

save_plot_if_data(
  harassment_plot_data,
  fig_harassment,
  "fig_descriptive_harassment_summary.tiff",
  width = 12,
  height = 7
)

################################################################################
# PM2.5 hour-of-day outputs
#
# The consolidated PM2.5 module above writes both
# fig_descriptive_pm25_time_of_day_primary.png and the legacy-compatible
# fig_descriptive_pm25_hourly_patterns.png. Both include raw indoor, concurrent
# outdoor, and default ambient-adjusted PM2.5 based on household-period-hour
# summaries and descriptive percentile bands.
################################################################################

################################################################################
# Physical-health figure for the manuscript child/caregiver figure
################################################################################

physical_health_plot_data <- read_reviewed_csv_required(
  "table_descriptive_physical_health_symptoms.csv"
) %>%
  filter(
    n_nonmissing > 0,
    !is.na(percent),
    study_arm_overall %in% arm_levels,
    source_variable %notin% physical_health_figure_exclusions
  ) %>%
  mutate(
    timepoint = factor(as.character(timepoint), levels = timepoint_levels),
    study_arm_overall = factor(
      as.character(study_arm_overall),
      levels = arm_levels
    ),
    respondent_group = factor(respondent_group, levels = c("Child", "Caregiver")),
    facet_label = factor(
      str_wrap(paste(respondent_group, outcome_label, sep = ": "), width = 24),
      levels = str_wrap(
        paste(
          physical_health_figure_labels$respondent_group,
          physical_health_figure_labels$outcome_label,
          sep = ": "
        ),
        width = 24
      )
    )
  )

write_reviewed_csv(
  physical_health_plot_data,
  "table_descriptive_health_panel_plot_data.csv"
)

fig_physical_health <- ggplot(
  physical_health_plot_data,
  aes(
    x = timepoint,
    y = percent,
    color = study_arm_overall,
    group = study_arm_overall
  )
) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 1.8, na.rm = TRUE) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.08,
    linewidth = 0.4,
    na.rm = TRUE
  ) +
  facet_wrap(
    ~ facet_label,
    ncol = sum(physical_health_figure_labels$respondent_group == "Child")
  ) +
  scale_color_manual(values = arm_colors[arm_levels], drop = FALSE) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    strip.text = element_text(size = 8)
  ) +
  labs(x = "Timepoint", y = "Percent reporting outcome", color = "Study arm")

save_plot_if_data(
  physical_health_plot_data,
  fig_physical_health,
  "fig_descriptive_health_symptom_panel.png",
  width = 16,
  height = 8
)

################################################################################
# Food consumption score and dietary diversity figure for manuscript FCS/HDDS plot
################################################################################

hdds_weekly_group_metadata <- tribble(
  ~hdds_group, ~hdds_group_label, ~source_variables, ~display_order,
  "hdds_weekly_cereals", "Cereals", "rice_adults_week; bread_adults_week; corn_adults_week", 1L,
  "hdds_weekly_tubers", "Roots/tubers", "potatoes_adults_week", 2L,
  "hdds_weekly_pulses", "Pulses", "lentils_adults_week", 3L,
  "hdds_weekly_eggs", "Eggs", "eggs_adults_week", 4L,
  "hdds_weekly_milk", "Dairy", "dairy_adults_week", 5L,
  "hdds_weekly_veggies", "Vegetables", "veggies_adults_week", 6L,
  "hdds_weekly_fruit", "Fruit", "fruit_adults_week", 7L,
  "hdds_weekly_fish", "Fish", "fish_adults_week", 8L,
  "hdds_weekly_meat", "Meat/poultry", "poultry_adults_week; goat_sheep_adults_week; beef_adults_week", 9L,
  "hdds_weekly_oil", "Oils/fats", "oil_adults_week", 10L,
  "hdds_weekly_sugar", "Sugar", "sugar_adults_week", 11L
)

hdds_weekly_group_colors <- c(
  "Cereals" = "#8C6D31",
  "Roots/tubers" = "#D8A03D",
  "Pulses" = "#7A6BB7",
  "Eggs" = "#F2C94C",
  "Dairy" = "#6BAED6",
  "Vegetables" = "#2CA25F",
  "Fruit" = "#E6550D",
  "Fish" = "#3182BD",
  "Meat/poultry" = "#C43C39",
  "Oils/fats" = "#756BB1",
  "Sugar" = "#969696"
)

hdds_24h_group_metadata <- tribble(
  ~hdds_group, ~hdds_group_label, ~source_variables, ~display_order,
  "hdds_24h_cereals", "Cereals", "food_consump_adult_day/1; /2; /3", 1L,
  "hdds_24h_tubers", "Roots/tubers", "food_consump_adult_day/4", 2L,
  "hdds_24h_pulses", "Pulses", "food_consump_adult_day/5", 3L,
  "hdds_24h_eggs", "Eggs", "food_consump_adult_day/6", 4L,
  "hdds_24h_milk", "Dairy", "food_consump_adult_day/7", 5L,
  "hdds_24h_veggies", "Vegetables", "food_consump_adult_day/8", 6L,
  "hdds_24h_fruit", "Fruit", "food_consump_adult_day/9", 7L,
  "hdds_24h_fish", "Fish", "food_consump_adult_day/10", 8L,
  "hdds_24h_meat", "Meat/poultry", "food_consump_adult_day/11; /12; /13", 9L,
  "hdds_24h_oil", "Oils/fats", "food_consump_adult_day/14", 10L,
  "hdds_24h_sugar", "Sugar", "food_consump_adult_day/15", 11L
)

survey_food <- survey %>%
  mutate(
    hdds_no_misc_clean = num_col(., "hdds_no_misc"),
    hdds_assume_misc_1_clean = num_col(., "hdds_assume_misc_1"),
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
    fcs_category_standard = case_when(
      is.na(fcs) ~ NA_character_,
      fcs <= 21 ~ "poor",
      fcs <= 35 ~ "borderline",
      fcs > 35 ~ "acceptable"
    ),
    fcs_category_adjusted = case_when(
      is.na(fcs) ~ NA_character_,
      fcs <= 28 ~ "poor",
      fcs <= 42 ~ "borderline",
      fcs > 42 ~ "acceptable"
    ),
    # Retain the historical standard-threshold alias used by downstream code.
    fcs_category = fcs_category_standard,
    fcs_method = paste(
      "Reconstructed from item-level 7-day frequencies;",
      "items are summed within WFP food groups and capped at 7 days"
    ),
    hdds_weekly_cereals = hdds_food_group(., c(
      "rice_adults_week", "bread_adults_week", "corn_adults_week"
    )),
    hdds_weekly_tubers = hdds_food_group(., "potatoes_adults_week"),
    hdds_weekly_pulses = hdds_food_group(., "lentils_adults_week"),
    hdds_weekly_eggs = hdds_food_group(., "eggs_adults_week"),
    hdds_weekly_milk = hdds_food_group(., "dairy_adults_week"),
    hdds_weekly_veggies = hdds_food_group(., "veggies_adults_week"),
    hdds_weekly_fruit = hdds_food_group(., "fruit_adults_week"),
    hdds_weekly_fish = hdds_food_group(., "fish_adults_week"),
    hdds_weekly_meat = hdds_food_group(., c(
      "poultry_adults_week", "goat_sheep_adults_week", "beef_adults_week"
    )),
    hdds_weekly_oil = hdds_food_group(., "oil_adults_week"),
    hdds_weekly_sugar = hdds_food_group(., "sugar_adults_week"),
    hdds_no_misc_weekly = row_sum_vars(
      tibble(
        hdds_weekly_cereals, hdds_weekly_tubers, hdds_weekly_pulses,
        hdds_weekly_eggs, hdds_weekly_milk, hdds_weekly_veggies,
        hdds_weekly_fruit, hdds_weekly_fish, hdds_weekly_meat,
        hdds_weekly_oil, hdds_weekly_sugar
      ),
      hdds_weekly_group_metadata$hdds_group
    ),
    hdds_assume_misc_1_weekly = if_else(
      !is.na(hdds_no_misc_weekly),
      hdds_no_misc_weekly + 1,
      NA_real_
    ),
    hdds_24h_cereals = hdds_food_group(., c(
      "food_consump_adult_day/1", "food_consump_adult_day/2",
      "food_consump_adult_day/3"
    )),
    hdds_24h_tubers = hdds_food_group(., "food_consump_adult_day/4"),
    hdds_24h_pulses = hdds_food_group(., "food_consump_adult_day/5"),
    hdds_24h_eggs = hdds_food_group(., "food_consump_adult_day/6"),
    hdds_24h_milk = hdds_food_group(., "food_consump_adult_day/7"),
    hdds_24h_veggies = hdds_food_group(., "food_consump_adult_day/8"),
    hdds_24h_fruit = hdds_food_group(., "food_consump_adult_day/9"),
    hdds_24h_fish = hdds_food_group(., "food_consump_adult_day/10"),
    hdds_24h_meat = hdds_food_group(., c(
      "food_consump_adult_day/11", "food_consump_adult_day/12",
      "food_consump_adult_day/13"
    )),
    hdds_24h_oil = hdds_food_group(., "food_consump_adult_day/14"),
    hdds_24h_sugar = hdds_food_group(., "food_consump_adult_day/15"),
    hdds_no_misc_from_24h = row_sum_vars(
      tibble(
        hdds_24h_cereals, hdds_24h_tubers, hdds_24h_pulses,
        hdds_24h_eggs, hdds_24h_milk, hdds_24h_veggies,
        hdds_24h_fruit, hdds_24h_fish, hdds_24h_meat,
        hdds_24h_oil, hdds_24h_sugar
      ),
      c(
        "hdds_24h_cereals", "hdds_24h_tubers", "hdds_24h_pulses",
        "hdds_24h_eggs", "hdds_24h_milk", "hdds_24h_veggies",
        "hdds_24h_fruit", "hdds_24h_fish", "hdds_24h_meat",
        "hdds_24h_oil", "hdds_24h_sugar"
      )
    ),
    hdds_assume_misc_1_from_24h = if_else(
      !is.na(hdds_no_misc_from_24h),
      hdds_no_misc_from_24h + 1,
      NA_real_
    ),
    # HDDS is a 24-hour construct. Do not coalesce it with the 7-day measure.
    hdds_no_misc = hdds_no_misc_from_24h,
    hdds_assume_misc_1 = hdds_assume_misc_1_from_24h
  ) %>%
  select(
    -hdds_no_misc_clean,
    -hdds_assume_misc_1_clean,
    -hdds_no_misc_from_24h,
    -hdds_assume_misc_1_from_24h
  )

fcs_hdds_plot_data <- survey_food %>%
  select(
    fcn_id,
    timepoint,
    study_arm_overall,
    hdds_no_misc,
    hdds_assume_misc_1,
    fcs,
    fcs_category,
    fcs_category_standard,
    fcs_category_adjusted,
    fcs_method,
    hdds_no_misc_weekly
  ) %>%
  filter(!is.na(fcs), !is.na(timepoint), !is.na(study_arm_overall))

fcs_hdds_summary <- fcs_hdds_plot_data %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall, fcs_category) %>%
  summarise(
    n = n(),
    fcs_mean = mean(fcs, na.rm = TRUE),
    fcs_median = median(fcs, na.rm = TRUE),
    hdds_24h_observed_mean = mean(hdds_no_misc, na.rm = TRUE),
    food_group_diversity_7d_mean = mean(hdds_no_misc_weekly, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(hdds_24h_observed_mean, food_group_diversity_7d_mean),
      ~ if_else(is.nan(.x) | is.infinite(.x), NA_real_, .x)
    )
  )

hdds_food_group_plot_data <- survey_food %>%
  filter(!is.na(timepoint), study_arm_overall %in% arm_levels) %>%
  select(
    timepoint,
    study_arm_overall,
    all_of(hdds_weekly_group_metadata$hdds_group)
  ) %>%
  pivot_longer(
    cols = all_of(hdds_weekly_group_metadata$hdds_group),
    names_to = "hdds_group",
    values_to = "food_group_consumed"
  ) %>%
  left_join(hdds_weekly_group_metadata, by = "hdds_group") %>%
  group_by(
    timepoint, study_arm_overall, hdds_group, hdds_group_label,
    source_variables, display_order
  ) %>%
  summarise(
    n_records = n(),
    n_nonmissing = sum(!is.na(food_group_consumed)),
    n_consumed = sum(food_group_consumed == 1, na.rm = TRUE),
    mean_food_group_score = if_else(
      n_nonmissing > 0,
      n_consumed / n_nonmissing,
      NA_real_
    ),
    percent_consumed = 100 * mean_food_group_score,
    .groups = "drop"
  ) %>%
  arrange_timepoint_arm(display_order)

food_group_diversity_7d_score_summary <- survey_food %>%
  filter(!is.na(timepoint), study_arm_overall %in% arm_levels) %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_records = n(),
    n_food_group_diversity_7d_nonmissing = sum(!is.na(hdds_no_misc_weekly)),
    mean_food_group_diversity_7d = mean(hdds_no_misc_weekly, na.rm = TRUE),
    sd_food_group_diversity_7d = sd(hdds_no_misc_weekly, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(
        mean_food_group_diversity_7d,
        sd_food_group_diversity_7d
      ),
      ~ if_else(is.nan(.x) | is.infinite(.x), NA_real_, .x)
    )
  ) %>%
  arrange_timepoint_arm()

hdds_24h_food_group_plot_data <- survey_food %>%
  filter(!is.na(timepoint), study_arm_overall %in% arm_levels) %>%
  select(
    timepoint,
    study_arm_overall,
    all_of(hdds_24h_group_metadata$hdds_group)
  ) %>%
  pivot_longer(
    cols = all_of(hdds_24h_group_metadata$hdds_group),
    names_to = "hdds_group",
    values_to = "food_group_consumed"
  ) %>%
  left_join(hdds_24h_group_metadata, by = "hdds_group") %>%
  group_by(
    timepoint, study_arm_overall, hdds_group, hdds_group_label,
    source_variables, display_order
  ) %>%
  summarise(
    n_records = n(),
    n_nonmissing = sum(!is.na(food_group_consumed)),
    n_consumed = sum(food_group_consumed == 1, na.rm = TRUE),
    mean_food_group_score = if_else(
      n_nonmissing > 0,
      n_consumed / n_nonmissing,
      NA_real_
    ),
    percent_consumed = 100 * mean_food_group_score,
    .groups = "drop"
  ) %>%
  arrange_timepoint_arm(display_order)

hdds_24h_score_summary <- survey_food %>%
  filter(!is.na(timepoint), study_arm_overall %in% arm_levels) %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_records = n(),
    n_hdds_24h_observed = sum(!is.na(hdds_no_misc)),
    mean_hdds_24h_observed_0_11 = mean(hdds_no_misc, na.rm = TRUE),
    sd_hdds_24h_observed_0_11 = sd(hdds_no_misc, na.rm = TRUE),
    mean_hdds_24h_assume_misc_1_0_12 = mean(
      hdds_assume_misc_1,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(
        mean_hdds_24h_observed_0_11,
        sd_hdds_24h_observed_0_11,
        mean_hdds_24h_assume_misc_1_0_12
      ),
      ~ if_else(is.nan(.x) | is.infinite(.x), NA_real_, .x)
    )
  ) %>%
  arrange_timepoint_arm()

fcs_threshold_sensitivity_summary <- survey_food %>%
  select(
    timepoint,
    study_arm_overall,
    fcs_category_standard,
    fcs_category_adjusted
  ) %>%
  pivot_longer(
    cols = c(fcs_category_standard, fcs_category_adjusted),
    names_to = "threshold_scheme",
    values_to = "fcs_category"
  ) %>%
  mutate(
    threshold_scheme = recode(
      threshold_scheme,
      fcs_category_standard = "standard_21_35",
      fcs_category_adjusted = "adjusted_28_42"
    )
  ) %>%
  filter(!is.na(fcs_category)) %>%
  add_all_arms_rows() %>%
  count(
    timepoint,
    study_arm_overall,
    threshold_scheme,
    fcs_category,
    name = "n_households"
  ) %>%
  group_by(timepoint, study_arm_overall, threshold_scheme) %>%
  mutate(
    n_nonmissing = sum(n_households),
    percent = 100 * n_households / n_nonmissing
  ) %>%
  ungroup() %>%
  arrange_timepoint_arm(threshold_scheme, fcs_category)

fcs_threshold_context <- survey_food %>%
  filter(!is.na(timepoint), study_arm_overall %in% arm_levels) %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_households = n(),
    mean_oil_days = mean(fcs_oil, na.rm = TRUE),
    mean_sugar_days = mean(fcs_sugar, na.rm = TRUE),
    pct_oil_at_least_6_days = 100 * mean(
      fcs_oil >= 6,
      na.rm = TRUE
    ),
    pct_sugar_at_least_6_days = 100 * mean(
      fcs_sugar >= 6,
      na.rm = TRUE
    ),
    pct_oil_and_sugar_7_days = 100 * mean(
      fcs_oil == 7 & fcs_sugar == 7,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange_timepoint_arm()

food_indicator_method_notes <- tribble(
  ~indicator, ~recall_period, ~implementation, ~interpretation_limit,
  "FCS", "7 days",
  paste(
    "Reconstructed by summing item frequencies within eight WFP groups,",
    "capping each group at 7 days, and applying standard weights."
  ),
  paste(
    "Current WFP guidance requires direct aggregate-group responses;",
    "the reconstructed score may overestimate consumption."
  ),
  "Seven-day food-group diversity", "7 days",
  "Presence in 11 observed food groups from the listed weekly variables.",
  "Exploratory diversity measure; it is not standard 24-hour HDDS.",
  "HDDS-compatible observed groups", "24 hours",
  "Presence in 11 observed HDDS groups at midline and endline.",
  paste(
    "Condiments/miscellaneous were not collected, so an exact 12-group",
    "HDDS cannot be calculated without an assumption."
  ),
  "Food acquisition source shares", "7 days",
  paste(
    "Food-item days are split equally across selected sources, then scaled",
    "so each WFP FCS group contributes at most 7 days."
  ),
  paste(
    "The questionnaire allowed multiple item-level sources, whereas WFP",
    "asks for one primary source per aggregate food group."
  )
) %>%
  mutate(
    guidance_reference = "https://docs.wfp.org/api/documents/WFP-0000158062/download/",
    source_method_reference = paste0(
      "https://github.com/WFP-VAM/RAMResourcesScripts/tree/main/Indicators/",
      "Food-consumption-sources"
    )
  )

write_reviewed_csv(
  fcs_hdds_plot_data,
  "table_descriptive_food_dietary_plot_data.csv"
)
write_reviewed_csv(
  fcs_hdds_summary,
  "table_descriptive_food_dietary_summary.csv"
)
write_reviewed_csv(
  hdds_food_group_plot_data,
  "table_descriptive_food_group_diversity_7d_plot_data.csv"
)
write_reviewed_csv(
  food_group_diversity_7d_score_summary,
  "table_descriptive_food_group_diversity_7d_score_summary.csv"
)
write_reviewed_csv(
  hdds_24h_food_group_plot_data,
  "table_descriptive_hdds_24h_food_group_plot_data.csv"
)
write_reviewed_csv(
  hdds_24h_score_summary,
  "table_descriptive_hdds_24h_score_summary.csv"
)
write_reviewed_csv(
  fcs_threshold_sensitivity_summary,
  "table_descriptive_fcs_threshold_sensitivity.csv"
)
write_reviewed_csv(
  fcs_threshold_context,
  "table_descriptive_fcs_threshold_context.csv"
)
write_reviewed_csv(
  food_indicator_method_notes,
  "table_descriptive_food_indicator_method_notes.csv",
  subfolder = "qa"
)

fig_fcs_hdds <- ggplot(
  fcs_hdds_plot_data,
  aes(x = fcs, fill = study_arm_overall)
) +
  geom_bar(aes(y = after_stat(count / sum(count)))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_vline(xintercept = 35.5, linetype = "dashed") +
  geom_vline(xintercept = 21.5, linetype = "dashed") +
  scale_fill_manual(
    name = "Study arm",
    breaks = c("intervention", "comparison"),
    labels = c("Intervention", "Comparison"),
    values = c(intervention = "#138B87", comparison = "#430154")
  ) +
  theme_bw() +
  labs(
    title = "Reconstructed food consumption score",
    subtitle = "Item-level frequencies summed within WFP groups and capped at 7 days; standard cutoffs shown.",
    x = "Reconstructed FCS",
    y = "Percentage of households",
    caption = "The questionnaire did not collect direct aggregate-group FCS responses required by current WFP guidance."
  ) +
  facet_grid(timepoint ~ ., scales = "fixed")

save_plot_if_data(
  fcs_hdds_plot_data,
  fig_fcs_hdds,
  "fig_descriptive_food_dietary_scores.tiff",
  width = 10,
  height = 6
)

hdds_food_group_plot_ready <- hdds_food_group_plot_data %>%
  filter(n_nonmissing > 0) %>%
  mutate(
    timepoint_label = factor(
      timepoint_label_with_year(timepoint),
      levels = timepoint_label_with_year_levels
    ),
    study_arm_label = factor(
      as.character(study_arm_overall),
      levels = arm_levels,
      labels = str_to_title(arm_levels)
    ),
    hdds_group_label = factor(
      hdds_group_label,
      levels = hdds_weekly_group_metadata$hdds_group_label
    )
  )

food_group_diversity_7d_score_labels <- food_group_diversity_7d_score_summary %>%
  filter(!is.na(mean_food_group_diversity_7d)) %>%
  mutate(
    timepoint_label = factor(
      timepoint_label_with_year(timepoint),
      levels = timepoint_label_with_year_levels
    ),
    study_arm_label = factor(
      as.character(study_arm_overall),
      levels = arm_levels,
      labels = str_to_title(arm_levels)
    ),
    score_label = sprintf("%.1f", mean_food_group_diversity_7d)
  )

fig_hdds_food_groups <- ggplot(
  hdds_food_group_plot_ready,
  aes(x = study_arm_label, y = mean_food_group_score, fill = hdds_group_label)
) +
  geom_col(width = 0.72, color = "white", linewidth = 0.15) +
  geom_text(
    data = food_group_diversity_7d_score_labels,
    aes(
      x = study_arm_label,
      y = mean_food_group_diversity_7d + 0.25,
      label = score_label
    ),
    inherit.aes = FALSE,
    size = 3.2
  ) +
  facet_wrap(~ timepoint_label, nrow = 1) +
  scale_fill_manual(values = hdds_weekly_group_colors, drop = FALSE) +
  scale_y_continuous(
    breaks = 0:11,
    limits = c(0, 11.5),
    expand = expansion(mult = c(0, 0.02))
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Seven-day household food-group diversity",
    subtitle = "Exploratory measure from the listed weekly food-frequency variables; this is not standard 24-hour HDDS.",
    x = NULL,
    y = "Mean 7-day diversity score (0-11)",
    fill = "Food group"
  )

save_plot_if_data(
  hdds_food_group_plot_ready,
  fig_hdds_food_groups,
  "fig_descriptive_food_group_diversity_7d.png",
  width = 12,
  height = 7
)

hdds_24h_food_group_plot_ready <- hdds_24h_food_group_plot_data %>%
  filter(n_nonmissing > 0) %>%
  mutate(
    timepoint_label = factor(
      timepoint_label_with_year(timepoint),
      levels = timepoint_label_with_year_levels
    ),
    study_arm_label = factor(
      as.character(study_arm_overall),
      levels = arm_levels,
      labels = str_to_title(arm_levels)
    ),
    hdds_group_label = factor(
      hdds_group_label,
      levels = hdds_24h_group_metadata$hdds_group_label
    )
  )

hdds_24h_score_labels <- hdds_24h_score_summary %>%
  filter(!is.na(mean_hdds_24h_observed_0_11)) %>%
  mutate(
    timepoint_label = factor(
      timepoint_label_with_year(timepoint),
      levels = timepoint_label_with_year_levels
    ),
    study_arm_label = factor(
      as.character(study_arm_overall),
      levels = arm_levels,
      labels = str_to_title(arm_levels)
    ),
    score_label = sprintf("%.1f", mean_hdds_24h_observed_0_11)
  )

fig_hdds_24h_food_groups <- ggplot(
  hdds_24h_food_group_plot_ready,
  aes(x = study_arm_label, y = mean_food_group_score, fill = hdds_group_label)
) +
  geom_col(width = 0.72, color = "white", linewidth = 0.15) +
  geom_text(
    data = hdds_24h_score_labels,
    aes(
      x = study_arm_label,
      y = mean_hdds_24h_observed_0_11 + 0.25,
      label = score_label
    ),
    inherit.aes = FALSE,
    size = 3.2
  ) +
  facet_wrap(~ timepoint_label, nrow = 1, drop = TRUE) +
  scale_fill_manual(values = hdds_weekly_group_colors, drop = FALSE) +
  scale_y_continuous(
    breaks = 0:11,
    limits = c(0, 11.5),
    expand = expansion(mult = c(0, 0.02))
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Twenty-four-hour household dietary diversity",
    subtitle = paste(
      "Eleven observed HDDS food groups; condiments/miscellaneous were not",
      "collected, so the displayed score ranges from 0 to 11."
    ),
    x = NULL,
    y = "Mean observed 24-hour score (0-11)",
    fill = "Food group"
  )

save_plot_if_data(
  hdds_24h_food_group_plot_ready,
  fig_hdds_24h_food_groups,
  "fig_descriptive_hdds_24h_food_groups_score.png",
  width = 10,
  height = 7
)

################################################################################
# Food acquisition sources
################################################################################

food_source_item_metadata <- tribble(
  ~food_item, ~food_item_label, ~frequency_variable, ~source_variable, ~fcs_group, ~display_order,
  "rice", "Rice", "rice_adults_week", "rice_source", "staples", 1L,
  "bread", "Bread", "bread_adults_week", "bread_source", "staples", 2L,
  "corn", "Corn", "corn_adults_week", "corn_source", "staples", 3L,
  "potatoes", "Potatoes", "potatoes_adults_week", "potatoes_source", "staples", 4L,
  "lentils", "Lentils", "lentils_adults_week", "lentils_source", "pulses", 5L,
  "eggs", "Eggs", "eggs_adults_week", "eggs_source", "protein", 6L,
  "dairy", "Dairy", "dairy_adults_week", "dairy_source", "dairy", 7L,
  "veggies", "Vegetables", "veggies_adults_week", "veggies_source", "vegetables", 8L,
  "fruit", "Fruit", "fruit_adults_week", "fruit_source", "fruit", 9L,
  "fish", "Fish", "fish_adults_week", "fish_source", "protein", 10L,
  "poultry", "Poultry", "poultry_adults_week", "poultry_source", "protein", 11L,
  "goat_sheep", "Goat/sheep", "goat_sheep_adults_week", "goat_sheep_source", "protein", 12L,
  "beef", "Beef", "beef_adults_week", "beef_source", "protein", 13L,
  "oil", "Oil/fat", "oil_adults_week", "oil_source", "oil", 14L,
  "sugar", "Sugar/sweets", "sugar_adults_week", "sugar_source", "sugar", 15L
)

food_source_code_metadata <- tribble(
  ~source_code, ~source_label, ~source_order,
  1L, "Food aid", 2L,
  2L, "Purchased", 1L,
  3L, "Purchased", 1L,
  4L, "Gift", 3L,
  5L, "Borrowed", 4L,
  6L, "Own production", 5L,
  7L, "Exchanged", 6L,
  8L, "Exchanged", 6L,
  9L, "Gathered/hunted/fished", 7L,
  66L, "Other", 8L
)

food_source_category_metadata <- food_source_code_metadata %>%
  distinct(source_label, source_order) %>%
  arrange(source_order)

food_source_category_levels <- food_source_category_metadata$source_label

food_source_colors <- c(
  "Purchased" = "#E69F00",
  "Food aid" = "#0072B2",
  "Gift" = "#009E73",
  "Borrowed" = "#CC79A7",
  "Own production" = "#56B4E9",
  "Exchanged" = "#D55E00",
  "Gathered/hunted/fished" = "#5E5E5E",
  "Other" = "#1A1A1A"
)

food_source_input <- survey_food %>%
  mutate(food_source_record_id = row_number())

food_frequency_long <- food_source_input %>%
  select(
    food_source_record_id,
    fcn_id,
    timepoint,
    study_arm_overall,
    all_of(food_source_item_metadata$frequency_variable)
  ) %>%
  pivot_longer(
    cols = all_of(food_source_item_metadata$frequency_variable),
    names_to = "frequency_variable",
    values_to = "days_consumed_raw"
  ) %>%
  left_join(
    food_source_item_metadata %>%
      select(food_item, frequency_variable),
    by = "frequency_variable"
  ) %>%
  mutate(
    days_consumed_raw = suppressWarnings(as.numeric(days_consumed_raw)),
    days_consumed = if_else(
      !is.na(days_consumed_raw) & between(days_consumed_raw, 0, 7),
      days_consumed_raw,
      NA_real_
    )
  )

food_source_tokens <- food_source_input %>%
  select(
    food_source_record_id,
    fcn_id,
    timepoint,
    study_arm_overall,
    all_of(food_source_item_metadata$source_variable)
  ) %>%
  pivot_longer(
    cols = all_of(food_source_item_metadata$source_variable),
    names_to = "source_variable",
    values_to = "source_codes_raw"
  ) %>%
  left_join(
    food_source_item_metadata %>%
      select(
        food_item,
        food_item_label,
        source_variable,
        fcs_group,
        display_order
      ),
    by = "source_variable"
  ) %>%
  mutate(
    source_codes_raw = na_if(str_squish(as.character(source_codes_raw)), "")
  ) %>%
  separate_rows(source_codes_raw, sep = "\\s+") %>%
  mutate(
    source_code = suppressWarnings(as.integer(as.numeric(source_codes_raw)))
  ) %>%
  distinct(food_source_record_id, food_item, source_code, .keep_all = TRUE) %>%
  left_join(
    food_frequency_long %>%
      select(
        food_source_record_id,
        food_item,
        days_consumed_raw,
        days_consumed
      ),
    by = c("food_source_record_id", "food_item")
  )

food_source_item_qa <- food_source_tokens %>%
  group_by(
    food_source_record_id,
    fcn_id,
    timepoint,
    study_arm_overall,
    food_item,
    food_item_label,
    fcs_group,
    display_order
  ) %>%
  summarise(
    days_consumed_raw = first(days_consumed_raw),
    days_consumed = first(days_consumed),
    n_valid_sources = n_distinct(
      source_code[source_code %in% food_source_code_metadata$source_code]
    ),
    has_dont_eat_code = any(source_code == 0L, na.rm = TRUE),
    has_invalid_source_code = any(
      !is.na(source_code) &
        !(source_code %in% c(0L, food_source_code_metadata$source_code))
    ),
    .groups = "drop"
  ) %>%
  mutate(
    has_invalid_frequency = !is.na(days_consumed_raw) &
      !between(days_consumed_raw, 0, 7),
    consumed_item = !is.na(days_consumed) & days_consumed > 0,
    missing_source_for_consumed_item = consumed_item & n_valid_sources == 0,
    source_reported_for_nonconsumed_item = !is.na(days_consumed) &
      days_consumed == 0 & n_valid_sources > 0,
    dont_eat_code_with_valid_source = has_dont_eat_code & n_valid_sources > 0
  )

food_source_qa_summary <- food_source_item_qa %>%
  filter(!is.na(timepoint), study_arm_overall %in% arm_levels) %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_households = n_distinct(food_source_record_id),
    n_food_item_records = n(),
    n_consumed_item_records = sum(consumed_item, na.rm = TRUE),
    n_consumed_items_missing_source = sum(
      missing_source_for_consumed_item,
      na.rm = TRUE
    ),
    n_nonconsumed_items_with_source = sum(
      source_reported_for_nonconsumed_item,
      na.rm = TRUE
    ),
    n_items_with_dont_eat_and_valid_source = sum(
      dont_eat_code_with_valid_source,
      na.rm = TRUE
    ),
    n_items_with_invalid_source_code = sum(has_invalid_source_code, na.rm = TRUE),
    n_items_with_invalid_frequency = sum(has_invalid_frequency, na.rm = TRUE),
    pct_consumed_items_with_analyzable_source = 100 * (
      n_consumed_item_records - n_consumed_items_missing_source
    ) / n_consumed_item_records,
    .groups = "drop"
  ) %>%
  arrange_timepoint_arm()

# WFP expects one primary source per aggregate food group. Because this survey
# recorded multiple sources per item, split item-days equally across valid
# selections, then cap the combined contribution of each FCS group at 7 days.
food_source_weighted <- food_source_tokens %>%
  inner_join(
    food_source_item_qa %>%
      select(
        food_source_record_id,
        food_item,
        days_consumed,
        n_valid_sources,
        consumed_item
      ),
    by = c(
      "food_source_record_id",
      "food_item",
      "days_consumed"
    )
  ) %>%
  filter(
    consumed_item,
    n_valid_sources > 0,
    source_code %in% food_source_code_metadata$source_code
  ) %>%
  left_join(food_source_code_metadata, by = "source_code") %>%
  mutate(item_source_days = days_consumed / n_valid_sources)

food_source_group_scalars <- food_source_weighted %>%
  group_by(food_source_record_id, fcs_group) %>%
  summarise(
    group_item_days = sum(item_source_days, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group_days_capped = pmin(group_item_days, 7),
    group_scale = if_else(
      group_item_days > 0,
      group_days_capped / group_item_days,
      NA_real_
    )
  )

food_source_days_by_household <- food_source_weighted %>%
  left_join(
    food_source_group_scalars,
    by = c("food_source_record_id", "fcs_group")
  ) %>%
  mutate(source_days = item_source_days * group_scale) %>%
  group_by(
    food_source_record_id,
    fcn_id,
    timepoint,
    study_arm_overall,
    source_label
  ) %>%
  summarise(source_days = sum(source_days, na.rm = TRUE), .groups = "drop")

food_source_household_denominators <- food_source_days_by_household %>%
  group_by(food_source_record_id, fcn_id, timepoint, study_arm_overall) %>%
  summarise(total_source_days = sum(source_days), .groups = "drop") %>%
  filter(total_source_days > 0)

food_source_household_shares <- food_source_household_denominators %>%
  mutate(.food_source_join = 1L) %>%
  left_join(
    food_source_category_metadata %>% mutate(.food_source_join = 1L),
    by = ".food_source_join"
  ) %>%
  select(-.food_source_join) %>%
  left_join(
    food_source_days_by_household,
    by = c(
      "food_source_record_id",
      "fcn_id",
      "timepoint",
      "study_arm_overall",
      "source_label"
    )
  ) %>%
  mutate(
    source_days = replace_na(source_days, 0),
    source_share = source_days / total_source_days
  )

food_source_share_qa <- food_source_household_shares %>%
  group_by(
    food_source_record_id,
    fcn_id,
    timepoint,
    study_arm_overall
  ) %>%
  summarise(household_share_sum = sum(source_share), .groups = "drop") %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_households_with_analyzable_sources = n_distinct(food_source_record_id),
    min_household_share_sum = min(household_share_sum),
    max_household_share_sum = max(household_share_sum),
    max_absolute_share_sum_error = max(abs(household_share_sum - 1)),
    .groups = "drop"
  )

food_source_qa_summary <- food_source_qa_summary %>%
  left_join(
    food_source_share_qa,
    by = c("timepoint", "study_arm_overall")
  ) %>%
  arrange_timepoint_arm()

food_source_summary <- food_source_household_shares %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall, source_label, source_order) %>%
  summarise(
    n_households = n_distinct(food_source_record_id),
    mean_household_share = mean(source_share),
    sd_household_share = sd(source_share),
    se_household_share = sd_household_share / sqrt(n_households),
    ci_lower = pmax(0, mean_household_share - 1.96 * se_household_share),
    ci_upper = pmin(1, mean_household_share + 1.96 * se_household_share),
    percent = 100 * mean_household_share,
    .groups = "drop"
  ) %>%
  arrange_timepoint_arm(source_order)

food_source_item_reported <- food_source_tokens %>%
  inner_join(
    food_source_item_qa %>%
      select(
        food_source_record_id,
        food_item,
        days_consumed,
        n_valid_sources,
        consumed_item
      ),
    by = c(
      "food_source_record_id",
      "food_item",
      "days_consumed"
    )
  ) %>%
  filter(
    consumed_item,
    n_valid_sources > 0,
    source_code %in% food_source_code_metadata$source_code
  ) %>%
  left_join(food_source_code_metadata, by = "source_code") %>%
  distinct(food_source_record_id, food_item, source_label) %>%
  mutate(source_reported = 1L)

food_source_item_denominators <- food_source_item_qa %>%
  filter(consumed_item, n_valid_sources > 0) %>%
  select(
    food_source_record_id,
    fcn_id,
    timepoint,
    study_arm_overall,
    food_item,
    food_item_label,
    display_order
  )

food_source_item_household <- food_source_item_denominators %>%
  mutate(.food_source_join = 1L) %>%
  left_join(
    food_source_category_metadata %>% mutate(.food_source_join = 1L),
    by = ".food_source_join"
  ) %>%
  select(-.food_source_join) %>%
  left_join(
    food_source_item_reported,
    by = c("food_source_record_id", "food_item", "source_label")
  ) %>%
  mutate(source_reported = replace_na(source_reported, 0L))

food_source_item_summary <- food_source_item_household %>%
  add_all_arms_rows() %>%
  group_by(
    timepoint,
    study_arm_overall,
    food_item,
    food_item_label,
    display_order,
    source_label,
    source_order
  ) %>%
  summarise(
    n_consuming_households_with_source = n_distinct(food_source_record_id),
    n_households_reporting_source = sum(source_reported),
    percent_households_reporting_source = 100 * mean(source_reported),
    .groups = "drop"
  ) %>%
  arrange_timepoint_arm(display_order, source_order)

write_reviewed_csv(
  food_source_summary,
  "table_descriptive_food_source_summary.csv"
)
write_reviewed_csv(
  food_source_item_summary,
  "table_descriptive_food_sources_by_item.csv"
)
write_reviewed_csv(
  food_source_qa_summary,
  "table_descriptive_food_source_qa.csv",
  subfolder = "qa"
)

food_source_plot_data <- food_source_summary %>%
  filter(study_arm_overall %in% arm_levels) %>%
  mutate(
    timepoint_label = factor(
      timepoint_label_with_year(timepoint),
      levels = timepoint_label_with_year_levels
    ),
    study_arm_label = factor(
      as.character(study_arm_overall),
      levels = arm_levels,
      labels = str_to_title(arm_levels)
    ),
    source_label = factor(source_label, levels = food_source_category_levels)
  )

fig_food_sources_overall <- ggplot(
  food_source_plot_data,
  aes(x = study_arm_label, y = mean_household_share, fill = source_label)
) +
  geom_col(width = 0.72, color = "white", linewidth = 0.15) +
  facet_wrap(~ timepoint_label, nrow = 1) +
  scale_fill_manual(values = food_source_colors, drop = FALSE) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0))
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "Sources of foods consumed in the previous 7 days",
    subtitle = paste(
      "Mean household share of reported food-group days; multiple sources",
      "for an item are allocated equally."
    ),
    x = NULL,
    y = "Mean share of food-group days",
    fill = "Food source",
    caption = paste(
      "Item days are capped at 7 within each FCS food group.",
      "'Do not eat/have' responses are excluded from source shares."
    )
  )

save_plot_if_data(
  food_source_plot_data,
  fig_food_sources_overall,
  "fig_descriptive_food_sources_overall.png",
  width = 12,
  height = 7
)

food_source_item_plot_data <- food_source_item_summary %>%
  filter(study_arm_overall %in% arm_levels) %>%
  mutate(
    timepoint_label = factor(
      timepoint_label_with_year(timepoint),
      levels = timepoint_label_with_year_levels
    ),
    study_arm_label = factor(
      as.character(study_arm_overall),
      levels = arm_levels,
      labels = str_to_title(arm_levels)
    ),
    food_item_label = factor(
      food_item_label,
      levels = rev(food_source_item_metadata$food_item_label)
    ),
    source_label = factor(source_label, levels = food_source_category_levels)
  )

fig_food_sources_by_item <- ggplot(
  food_source_item_plot_data,
  aes(
    x = source_label,
    y = food_item_label,
    fill = percent_households_reporting_source
  )
) +
  geom_tile(color = "white", linewidth = 0.2) +
  facet_grid(timepoint_label ~ study_arm_label, drop = TRUE) +
  scale_fill_gradientn(
    colors = c("#F7FBFF", "#6BAED6", "#08306B"),
    limits = c(0, 100),
    labels = function(x) paste0(round(x), "%")
  ) +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Reported sources for each food item consumed",
    subtitle = paste(
      "Percentage among households consuming the item and reporting at least",
      "one valid source; percentages may exceed 100 across sources."
    ),
    x = NULL,
    y = NULL,
    fill = "Households"
  )

save_plot_if_data(
  food_source_item_plot_data,
  fig_food_sources_by_item,
  "fig_descriptive_food_sources_by_item.png",
  width = 14,
  height = 12
)

################################################################################
# LPG willingness-to-pay figure for manuscript programmatic evaluation plot
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

summarise_lpg_wtp <- function(df, summary_level) {
  df %>%
    summarise(
      n_nonmissing = sum(!is.na(lpg_willingness_to_pay_usd)),
      n_willing_to_pay_gt0 = sum(lpg_willingness_to_pay_usd > 0, na.rm = TRUE),
      pct_willing_to_pay_gt0 = if_else(
        n_nonmissing > 0,
        100 * n_willing_to_pay_gt0 / n_nonmissing,
        NA_real_
      ),
      n_willing_to_pay_gt_6_12_usd = sum(lpg_willingness_to_pay_usd > 6.12, na.rm = TRUE),
      pct_willing_to_pay_gt_6_12_usd = if_else(
        n_nonmissing > 0,
        100 * n_willing_to_pay_gt_6_12_usd / n_nonmissing,
        NA_real_
      ),
      n_positive_wtp = sum(lpg_willingness_to_pay_usd > 0, na.rm = TRUE),
      mean_positive_usd = mean(lpg_willingness_to_pay_usd[lpg_willingness_to_pay_usd > 0], na.rm = TRUE),
      sd_positive_usd = sd(lpg_willingness_to_pay_usd[lpg_willingness_to_pay_usd > 0], na.rm = TRUE),
      median_positive_usd = median(lpg_willingness_to_pay_usd[lpg_willingness_to_pay_usd > 0], na.rm = TRUE),
      p25_positive_usd = quantile(lpg_willingness_to_pay_usd[lpg_willingness_to_pay_usd > 0], 0.25, na.rm = TRUE, names = FALSE),
      p75_positive_usd = quantile(lpg_willingness_to_pay_usd[lpg_willingness_to_pay_usd > 0], 0.75, na.rm = TRUE, names = FALSE),
      min_positive_usd = min(lpg_willingness_to_pay_usd[lpg_willingness_to_pay_usd > 0], na.rm = TRUE),
      max_positive_usd = max(lpg_willingness_to_pay_usd[lpg_willingness_to_pay_usd > 0], na.rm = TRUE),
      mean_positive_bdt = mean(lpg_willingness_to_pay_bdt[lpg_willingness_to_pay_usd > 0], na.rm = TRUE),
      sd_positive_bdt = sd(lpg_willingness_to_pay_bdt[lpg_willingness_to_pay_usd > 0], na.rm = TRUE),
      median_positive_bdt = median(lpg_willingness_to_pay_bdt[lpg_willingness_to_pay_usd > 0], na.rm = TRUE),
      p25_positive_bdt = quantile(lpg_willingness_to_pay_bdt[lpg_willingness_to_pay_usd > 0], 0.25, na.rm = TRUE, names = FALSE),
      p75_positive_bdt = quantile(lpg_willingness_to_pay_bdt[lpg_willingness_to_pay_usd > 0], 0.75, na.rm = TRUE, names = FALSE),
      min_positive_bdt = min(lpg_willingness_to_pay_bdt[lpg_willingness_to_pay_usd > 0], na.rm = TRUE),
      max_positive_bdt = max(lpg_willingness_to_pay_bdt[lpg_willingness_to_pay_usd > 0], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      summary_level = summary_level,
      denominator_note = "Percentages use households with nonmissing lpg_willingness_to_pay; distribution statistics are restricted to households willing to pay > 0.",
      across(
        c(
          pct_willing_to_pay_gt0, pct_willing_to_pay_gt_6_12_usd,
          mean_positive_usd, sd_positive_usd, median_positive_usd,
          p25_positive_usd, p75_positive_usd, min_positive_usd, max_positive_usd
        ),
        ~ if_else(is.nan(.x) | is.infinite(.x), NA_real_, round(.x, 2))
      ),
      across(
        c(
          mean_positive_bdt, sd_positive_bdt, median_positive_bdt,
          p25_positive_bdt, p75_positive_bdt, min_positive_bdt, max_positive_bdt
        ),
        ~ if_else(is.nan(.x) | is.infinite(.x), NA_real_, round(.x, 0))
      )
    )
}

lpg_wtp_by_arm <- lpg_wtp_plot_data %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise_lpg_wtp("arm_timepoint")

lpg_wtp_all_arms <- lpg_wtp_plot_data %>%
  group_by(timepoint) %>%
  summarise_lpg_wtp("all_arms_timepoint") %>%
  mutate(study_arm_overall = "all_arms", .after = timepoint)

lpg_wtp_summary <- bind_rows(lpg_wtp_by_arm, lpg_wtp_all_arms) %>%
  select(
    summary_level, timepoint, study_arm_overall, n_nonmissing,
    n_willing_to_pay_gt0, pct_willing_to_pay_gt0,
    n_willing_to_pay_gt_6_12_usd, pct_willing_to_pay_gt_6_12_usd,
    n_positive_wtp,
    mean_positive_usd, sd_positive_usd, median_positive_usd,
    p25_positive_usd, p75_positive_usd, min_positive_usd, max_positive_usd,
    mean_positive_bdt, sd_positive_bdt, median_positive_bdt,
    p25_positive_bdt, p75_positive_bdt, min_positive_bdt, max_positive_bdt,
    denominator_note
  ) %>%
  arrange(summary_level, timepoint, study_arm_overall)

write_reviewed_csv(
  lpg_wtp_plot_data,
  "table_descriptive_lpg_wtp_plot_data.csv"
)
write_reviewed_csv(
  lpg_wtp_summary,
  "table_descriptive_lpg_wtp_summary.csv"
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
  "fig_descriptive_lpg_willingness_pay.png",
  width = 6,
  height = 5
)

################################################################################
# Output coverage audit
################################################################################

figure_coverage <- manuscript_figure_targets %>%
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
  "table_descriptive_manuscript_figure_coverage.csv",
  subfolder = "qa"
)

message("Manuscript-style reviewed figure generation complete.")
})
