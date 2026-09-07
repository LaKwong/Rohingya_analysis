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
#   7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_stove_daily_dataset.csv
#   7_tables/pm25_ambient_adjusted_YYYYMMDD public outputs and 8_restricted/pm25_ambient_adjusted_YYYYMMDD internal PM panel, when present
#
# Outputs:
#   Tables:  7_tables/RF105_reviewed_YYYYMMDD/descriptive_*, tab1_*, pm25_*,
#            fuel_*, stove_*, health_*, supplemental_survey_*, manuscript_style_*,
#            and pm25_explainer_* CSV files.
#   Figures: 6_figures/RF105_reviewed_YYYYMMDD/descriptive_*, pm25_*, fuel_*,
#            stove_*, health_*, supplemental_survey_*, manuscript-style, and
#            PM2.5 explainer figures.
#   QA:      7_tables/RF105_reviewed_YYYYMMDD/qa/* descriptive QA files.
#
# Notes:
#   - Uses one deduplicated household record per fcn_id-timepoint, matching the
#     reviewed RF105 helper function make_analysis_population().
#   - FCS follows the embedded calculation:
#     the embedded FCS/HDDS code in this script
#   - HDDS uses clean_final baseline-compatible weekly food-frequency variables,
#     with the embedded past-24-hour mapping as a fallback for earlier survey waves.
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

calc_prop_summary <- function(df, value_var = "value") {
  add_all_arms_rows(df) %>%
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
        proportion = if_else(n_nonmissing > 0, n_yes / n_nonmissing, NA_real_),
        percent = 100 * proportion,
        ci_lower = generic_prop_ci_lower(proportion, n_nonmissing),
        ci_upper = generic_prop_ci_upper(proportion, n_nonmissing),
        .groups = "drop"
      ) %>%
      mutate(
        outcome_group = var_table$outcome_group[[i]],
        outcome_name = var_table$outcome_name[[i]],
        outcome_label = var_table$outcome_label[[i]],
        source_variable = var,
        analysis_variable = var_table$analysis_variable[[i]],
        analysis_type = "binary_percent",
        unit = "percent",
        population = population,
        denominator_note = "Percent among nonmissing responses within each timepoint-arm group; all_arms rows pool comparison and intervention households."
      )
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, analysis_variable, source_variable, analysis_type,
           unit, population, n_total, n_nonmissing, n_yes, percent, ci_lower,
           ci_upper, denominator_note) %>%
    arrange_timepoint_arm(outcome_group, outcome_name)
}

generic_summarise_continuous_vars <- function(df, var_table,
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
        value = generic_clean_numeric_value(.data[[var]], nonnegative = TRUE)
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
        denominator_note = "Continuous summaries use nonmissing nonnegative numeric values; all_arms rows pool comparison and intervention households."
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
          proportion = if_else(n_nonmissing > 0,
                               n_selected / n_nonmissing,
                               NA_real_),
          percent = 100 * proportion,
          ci_lower = generic_prop_ci_lower(proportion, n_nonmissing),
          ci_upper = generic_prop_ci_upper(proportion, n_nonmissing),
          .groups = "drop"
        ) %>%
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
          denominator_note = "Percent selected among nonmissing responses for this multi-select item; all_arms rows pool comparison and intervention households."
        )
    })
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, analysis_variable, source_variable,
           source_variable_used, analysis_type, option_code, option_label,
           unit, population, n_total, n_nonmissing, n_selected, percent,
           ci_lower, ci_upper, denominator_note) %>%
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
  "fuel_time_cost", "times_wood_day", "Wood collection trips per day",
  "fuel_time_cost", "collect_wood_walk_hr", "Hours walking to collect wood",
  "fuel_time_cost", "forest_wood_fee", "Forest wood fee",
  "fuel_time_cost", "gather_wood_reason", "Reason for gathering wood",
  "fuel_time_cost", "buy_wood_cost", "Wood purchase cost",
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
  "fuel_time_cost", "times_wood_day", "times_wood_day", "times_wood_day", "Wood collection trips per day", "trips",
  "fuel_time_cost", "collect_wood_walk_hr", "collect_wood_walk_hr", "collect_wood_walk_hr", "Hours walking to collect wood", "hours",
  "fuel_time_cost", "forest_wood_fee", "forest_wood_fee", "forest_wood_fee", "Forest wood fee", "BDT",
  "fuel_time_cost", "buy_wood_cost", "buy_wood_cost", "buy_wood_cost", "Wood purchase cost", "BDT",
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
  mutate(analysis_output = "table_descriptive_continuous_outcomes.csv")

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
      ". Install it with install.packages('openxlsx') and rerun this script.",
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

  survey_for_workbook <- survey_dedup %>%
    ensure_workbook_cols(c(
      "fcn_id", "timepoint", "study_arm_overall",
      "target_child_sex", "target_child_months", "hh_size",
      "hh_size_2mo_u5", "hh_ppl_smoke", "income", "total_income_30",
      rf105b_income_fields, "spent_total_month", "debt", "debt_total",
      "electricity", "mattress", "chair_bench", "portable_lamp",
      "solar_lamp", "mobile_phone", "smartphone", "electric_fan",
      "shovel", "sickle", "weaving_tool", "chicken_duck_pigeon"
    )) %>%
    mutate(
      timepoint = as_ordered_timepoint(timepoint),
      study_arm_overall = as.character(study_arm_overall),
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
      income_component_total_bdt = rf105b_row_sum_numeric(., rf105b_income_fields),
      income_usd = rf105b_coalesce_num(
        income_component_total_bdt,
        income,
        total_income_30
      ) / exchange_rate,
      spent_total_month_usd = as_number(spent_total_month) / exchange_rate,
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

  all_timepoint_columns <- map_dfc(timepoint_levels, function(tp) {
    dat <- survey_for_workbook %>% filter(timepoint == tp)
    make_summary_column(dat, table_rows, paste0("All households ", tp)) %>%
      select(-row)
  })

  group_columns <- pmap_dfc(group_defs, function(arm, group, flag_var, column_label) {
    dat <- baseline_for_groups %>%
      filter(study_arm_overall == arm, .data[[flag_var]])
    make_summary_column(dat, table_rows, column_label) %>%
      select(-row)
  })

  count_row <- tibble(
    characteristic = "n =",
    !!!setNames(
      map(timepoint_levels, function(tp) {
        survey_for_workbook %>%
          filter(timepoint == tp) %>%
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
    )
  ) %>%
    mutate(across(-characteristic, ~ paste0("n = ", .x)))

  output_table <- bind_cols(
    table_rows %>% select(characteristic),
    all_timepoint_columns,
    group_columns
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
    "The first three columns summarize all deduplicated households at each survey timepoint, pooled across study arms.",
    "Participation-pattern columns use baseline household characteristics and the same group definitions as the prior RF105B Table 1 workbook."
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
        days_before_refill_raw > 60
    )

  summarise_lpg_refill_runout <- function(df, summary_level, population_rule) {
    df %>%
      summarise(
        n_households = n(),
        n_runout_response_nonmissing = sum(!is.na(lpg_ran_out_before_refill)),
        n_ran_out_before_refill = sum(lpg_ran_out_before_refill == 1, na.rm = TRUE),
        n_did_not_run_out_before_refill = sum(lpg_ran_out_before_refill == 0, na.rm = TRUE),
        n_days_before_refill_gt_60_excluded = sum(excluded_days_before_refill_gt_60, na.rm = TRUE),
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
        c(mean_days_before_refill_among_runout,
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
    hdds_no_misc_from_24h = hdds_no_misc,
    hdds_assume_misc_1_from_24h = if_else(
      !is.na(hdds_no_misc_from_24h),
      hdds_no_misc_from_24h + 1,
      NA_real_
    ),
    hdds_no_misc = dplyr::coalesce(hdds_no_misc_clean, hdds_no_misc_from_24h),
    hdds_assume_misc_1 = dplyr::coalesce(
      hdds_assume_misc_1_clean,
      hdds_assume_misc_1_from_24h
    )
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
  labs(x = "Timepoint", y = "Households", fill = "FCS category")
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
      "HDDS without miscellaneous group",
      "HDDS assuming miscellaneous group equals 1"
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
  "table_descriptive_physical_health_symptoms.csv"
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
  "fig_descriptive_physical_health_symptoms.png",
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
  1, "Types of cooking fuel used in past 30 days by arm/timepoint", "table_descriptive_fuel_use_past_month.csv", "fig_descriptive_fuel_use_past_month.png", "complete", "Uses clean_final fuel_30 variables plus reviewed aliases for any LPG/wood/CRH.",
  2, "Usable duration of 12 kg LPG cylinder by household size", "table_descriptive_lpg_duration_household_size.csv", "fig_descriptive_lpg_duration_household_size.png", "complete", "Uses hh_size and lpg_days_possible; QA table flags zero and >120 day values.",
  3, "Livelihood training and use of skills", "table_descriptive_livelihood_training_skills.csv", "fig_descriptive_livelihood_training_skills.png", "complete_with_label_limitations", "Training/skill barrier option text was not preserved in clean_final; code outputs option codes and broad labels.",
  4, "Strategies used to cope with shortage of food", "table_descriptive_food_shortage_coping.csv", "fig_descriptive_food_shortage_coping.png", "complete", "Coping labels copied from 3_data_cleaning/1.5_define_vector_columns.R.",
  5, "Strategies used to cope with shortage of fuel", "table_descriptive_fuel_shortage_coping.csv", "fig_descriptive_fuel_shortage_coping.png", "complete", "Coping labels copied from 3_data_cleaning/1.5_define_vector_columns.R; option-1 wording note retained.",
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
  18, "Physical health outcomes with child outcomes on top and caregiver outcomes on bottom", "table_descriptive_physical_health_symptoms.csv", "fig_descriptive_physical_health_symptoms.png", "complete", "Figure facet order is the 9 requested child outcomes followed by 9 requested caregiver outcomes."
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
# Consolidated PM2.5 descriptive section
################################################################################
local({
################################################################################
# RF105 reviewed PM2.5 analysis and time-of-day figure
#
# Purpose:
#   Recreate the indoor PM2.5 summaries and time-of-day figure using the final
#   cleaned PATS PM2.5 files.
#
# Inputs:
#   4_data/clean_final/pm25_pats_refugee_indoor.rds
#   4_data/clean_final/pm25_pats_refugee_ambient.rds
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/pm25_*.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/pm25_*.csv
#   6_figures/RF105_reviewed_YYYYMMDD/fig_descriptive_pm25_time_of_day.png
#
# Sensitivity analyses:
#   1. Main RF105B-style LOD handling: values below 10 ug/m3 set to 10.
#   2. No LOD substitution.
#   3. Main LOD handling plus exclusion of the highest 2.5% of values.
#   4. Summary stratified by season when a season variable is available.
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

pm_indoor_raw <- readRDS(file_pm25_indoor) %>%
  clean_timepoint_arm()

pm_ambient_raw <- readRDS(file_pm25_ambient) %>%
  clean_timepoint_arm()

detect_first_existing <- function(df, candidates, label) {
  found <- candidates[candidates %in% names(df)]
  if (length(found) == 0) {
    stop("Could not find ", label, ". Checked: ", paste(candidates, collapse = ", "))
  }
  found[[1]]
}

pm_value_var <- detect_first_existing(
  pm_indoor_raw,
  c("PM_Estimate", "pm25_ug_m3", "pm25", "pm2_5", "pm2.5"),
  "PM2.5 concentration column"
)

datetime_var <- detect_first_existing(
  pm_indoor_raw,
  c("dateTime", "datetime", "date_time", "sample_datetime", "DateTime"),
  "date-time column"
)

household_id_var <- detect_first_existing(
  pm_indoor_raw,
  c("fcn_id", "hh_id", "household_id", "pats_household_id"),
  "household ID column"
)

season_var <- intersect(
  c("season_monitored", "season", "monitoring_season"),
  names(pm_indoor_raw)
)
season_var <- if (length(season_var) == 0) NA_character_ else season_var[[1]]

pm_required_vars <- c(
  pm_value_var, datetime_var, household_id_var,
  "timepoint", "study_arm_overall", season_var
)

write_reviewed_csv(
  flag_missing_vars(
    pm_indoor_raw,
    pm_required_vars[!is.na(pm_required_vars)],
    "RF105 PM2.5 analysis variables"
  ),
  "table_descriptive_pm25_variable_availability.csv",
  subfolder = "qa"
)

################################################################################
# Prepare PM2.5 data
################################################################################

pm_indoor <- pm_indoor_raw %>%
  mutate(
    pm25_raw = suppressWarnings(as.numeric(.data[[pm_value_var]])),
    monitor_datetime = as.POSIXct(.data[[datetime_var]],
                                  tz = "Asia/Dhaka"),
    household_id = as.character(.data[[household_id_var]])
  ) %>%
  filter(
    !is.na(pm25_raw),
    !is.na(monitor_datetime),
    !is.na(timepoint),
    !is.na(study_arm_overall),
    !is.na(household_id)
  )

if ("include_in_indoor_final" %in% names(pm_indoor)) {
  pm_indoor <- pm_indoor %>%
    filter(is.na(include_in_indoor_final) | include_in_indoor_final == TRUE)
}

pm_ambient <- pm_ambient_raw %>%
  mutate(
    pm25_raw = suppressWarnings(as.numeric(.data[[pm_value_var]])),
    monitor_datetime = as.POSIXct(.data[[datetime_var]],
                                  tz = "Asia/Dhaka")
  ) %>%
  filter(!is.na(pm25_raw), !is.na(monitor_datetime))

pm_qc_counts <- bind_rows(
  pm_indoor_raw %>%
    summarise(
      file = "pm25_pats_refugee_indoor.rds",
      n_rows_raw = n(),
      n_pm_nonmissing = sum(!is.na(.data[[pm_value_var]])),
      n_datetime_nonmissing = sum(!is.na(.data[[datetime_var]])),
      n_households = n_distinct(.data[[household_id_var]], na.rm = TRUE)
    ),
  pm_indoor %>%
    summarise(
      file = "pm25_pats_refugee_indoor.rds_after_review_filters",
      n_rows_raw = n(),
      n_pm_nonmissing = sum(!is.na(pm25_raw)),
      n_datetime_nonmissing = sum(!is.na(monitor_datetime)),
      n_households = n_distinct(household_id, na.rm = TRUE)
    )
)

write_reviewed_csv(pm_qc_counts, "table_descriptive_pm25_qc_counts.csv", subfolder = "qa")

make_pm_scenario <- function(df, scenario) {
  out <- df

  if (scenario %in% c("main_lod10", "trim_top_2_5_percent_lod10")) {
    out <- out %>%
      mutate(pm25_reviewed = pmax(pm25_raw, 10))
  } else if (scenario == "no_lod_substitution") {
    out <- out %>%
      mutate(pm25_reviewed = pm25_raw)
  } else {
    stop("Unknown PM2.5 scenario: ", scenario)
  }

  if (scenario == "trim_top_2_5_percent_lod10") {
    trim_value <- quantile(out$pm25_reviewed, probs = 0.975, na.rm = TRUE)
    out <- out %>%
      filter(pm25_reviewed <= trim_value)
  }

  out %>%
    mutate(scenario = scenario)
}

pm_scenarios <- bind_rows(
  make_pm_scenario(pm_indoor, "main_lod10"),
  make_pm_scenario(pm_indoor, "no_lod_substitution"),
  make_pm_scenario(pm_indoor, "trim_top_2_5_percent_lod10")
)

################################################################################
# Summary table by timepoint and study arm
################################################################################

summarise_pm <- function(df, grouping_vars) {
  df %>%
    add_all_arms_rows() %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      n_observations = n(),
      n_households = n_distinct(household_id),
      n_monitor_files = n_distinct(raw_source_file, na.rm = TRUE),
      mean_pm25 = mean(pm25_reviewed, na.rm = TRUE),
      median_pm25 = median(pm25_reviewed, na.rm = TRUE),
      p25_pm25 = quantile(pm25_reviewed, probs = 0.25, na.rm = TRUE),
      p75_pm25 = quantile(pm25_reviewed, probs = 0.75, na.rm = TRUE),
      p95_pm25 = quantile(pm25_reviewed, probs = 0.95, na.rm = TRUE),
      max_pm25 = max(pm25_reviewed, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(across(all_of(grouping_vars)))
}

pm_summary <- pm_scenarios %>%
  summarise_pm(c("scenario", "timepoint", "study_arm_overall"))

write_reviewed_csv(pm_summary, "table_descriptive_pm25_arm_timepoint.csv")

if (!is.na(season_var)) {
  pm_season_summary <- pm_scenarios %>%
    mutate(season_reviewed = as.character(.data[[season_var]])) %>%
    summarise_pm(c("scenario", "season_reviewed", "timepoint", "study_arm_overall"))

  write_reviewed_csv(pm_season_summary, "table_descriptive_pm25_season_summary.csv")
}

################################################################################
# Household-level rDiD/XGBoost PM2.5 models are run in 4_rdid_xgboost_20260805_2213.R.

################################################################################
# Time-of-day figure
################################################################################

pm_time_of_day <- pm_scenarios %>%
  filter(scenario == "main_lod10") %>%
  mutate(hour = hour(monitor_datetime)) %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall, hour) %>%
  summarise(
    mean_pm25 = mean(pm25_reviewed, na.rm = TRUE),
    p25_pm25 = quantile(pm25_reviewed, probs = 0.25, na.rm = TRUE),
    p75_pm25 = quantile(pm25_reviewed, probs = 0.75, na.rm = TRUE),
    n_observations = n(),
    n_households = n_distinct(household_id),
    .groups = "drop"
  ) %>%
  filter(n_households >= 3)

write_reviewed_csv(pm_time_of_day, "table_descriptive_pm25_time_of_day.csv")

fig_pm_time_of_day <- ggplot(
  pm_time_of_day,
  aes(x = hour, y = mean_pm25, color = study_arm_overall, fill = study_arm_overall)
) +
  geom_ribbon(
    aes(ymin = p25_pm25, ymax = p75_pm25),
    alpha = 0.16,
    color = NA
  ) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ timepoint, nrow = 1) +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  scale_y_log10(labels = label_number()) +
  scale_color_manual(
    values = c(comparison = "#4E79A7", intervention = "#F28E2B"),
    na.translate = FALSE
  ) +
  scale_fill_manual(
    values = c(comparison = "#4E79A7", intervention = "#F28E2B"),
    na.translate = FALSE
  ) +
  labs(
    x = "Hour of day",
    y = expression(PM[2.5]~(mu*g/m^3)),
    color = "Study arm",
    fill = "Study arm"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

save_reviewed_plot(
  fig_pm_time_of_day,
  "fig_descriptive_pm25_time_of_day.png",
  width = 9,
  height = 4
)

################################################################################
# Ambient PM2.5 summary, for comparison and manuscript QA
################################################################################

if (nrow(pm_ambient) > 0) {
  pm_ambient_summary <- pm_ambient %>%
    mutate(pm25_reviewed = pmax(pm25_raw, 10)) %>%
    add_all_arms_rows() %>%
    group_by(timepoint, study_arm_overall) %>%
    summarise(
      n_observations = n(),
      mean_pm25 = mean(pm25_reviewed, na.rm = TRUE),
      median_pm25 = median(pm25_reviewed, na.rm = TRUE),
      p25_pm25 = quantile(pm25_reviewed, probs = 0.25, na.rm = TRUE),
      p75_pm25 = quantile(pm25_reviewed, probs = 0.75, na.rm = TRUE),
      p95_pm25 = quantile(pm25_reviewed, probs = 0.95, na.rm = TRUE),
      max_pm25 = max(pm25_reviewed, na.rm = TRUE),
      .groups = "drop"
    )

  write_reviewed_csv(pm_ambient_summary, "table_descriptive_pm25_ambient_summary.csv")
}
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
  "fuel_30_receive_lpg",
  "fuel_30_gather_scraps",
  "fuel_30_buy_wood",
  "fuel_30_buy_lpg",
  "fuel_30_buy_crh",
  "fuel_30_receive_crh",
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
      "LPG, received",
      "Scraps, gathered",
      "Wood, purchased",
      "LPG, purchased",
      "Compressed rice husks, purchased",
      "Compressed rice husks, received",
      "Plastic, collected"
    ),
    levels = c(
      "LPG, received",
      "Scraps, gathered",
      "Wood, purchased",
      "LPG, purchased",
      "Compressed rice husks, purchased",
      "Compressed rice husks, received",
      "Plastic, collected"
    )
  ),
  fuel_method = factor(
    c("received", "collected", "purchased", "purchased", "purchased", "received", "collected"),
    levels = c("purchased", "received", "collected")
  ),
  fuel_type = factor(
    c("LPG", "Wood", "Wood", "LPG", "Compressed rice husks", "Compressed rice husks", "Plastic"),
    levels = c("LPG", "Wood", "Compressed rice husks", "Plastic")
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
  arrange(study_arm_label, fuel_type, fuel_method, fuel, timepoint)

write_reviewed_csv(
  fuel_plot_data,
  "table_descriptive_fuel_use_line_plot_data.csv"
)

fig_fuel_30 <- ggplot(
  fuel_plot_data,
  aes(
    x = timepoint,
    y = proportion,
    color = fuel_type,
    linetype = fuel_method,
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
    y = "Proportion of Households (%)",
    color = NULL,
    linetype = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  )

save_reviewed_plot(fig_fuel_30, "fig_descriptive_fuel_use_summary.png",
                   width = 6, height = 6)

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
# Consolidated stove-monitor uptake descriptive section
################################################################################
local({
################################################################################
# RF105 reviewed stove-monitor uptake and cooking-time analyses
#
# Purpose:
#   Create stove-use duration summaries and figures from the checked Geocene
#   results produced by 1_geocene_stove_use_20260805_2213.R. This script is
#   intentionally downstream of the combined Geocene review script and does not
#   re-read or independently reprocess clean_final/stove_use_geocene_refugee_daily.rds.
#
# Inputs from 1_geocene_stove_use_20260805_2213.R:
#   7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_stove_daily_dataset.csv
#   7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_geocene_daily_summary.csv
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/stove_*.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/stove_*.csv
#   6_figures/RF105_reviewed_YYYYMMDD/stove_*.png
#
# Sensitivity analyses:
#   1. Main analysis uses all reviewed daily records exported by script 8.
#   2. Sensitivity excludes the first 7, 14, 30, and 60 days after first
#      receiving LPG, because the earliest monitored days may reflect uptake
#      transition behavior rather than stable use.
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

resolve_reviewed_or_restricted_csv <- function(filename, restricted_subfolder = "identified_tables",
                                               upstream_script = NULL) {
  candidate_paths <- c(
    file.path(dir_tables_reviewed, filename),
    file.path(dir_restricted_reviewed, restricted_subfolder, filename),
    file.path(dir_restricted_qa, filename)
  )
  candidate_paths <- candidate_paths[file.exists(candidate_paths)]

  if (length(candidate_paths) == 0 && !is.null(upstream_script)) {
    upstream_path <- file.path(script_dir, upstream_script)
    stop(
      "Required reviewed result is missing from public and restricted output folders: ", filename, "\n",
      "Run the upstream reviewed script first: ", upstream_path, "\n",
      "Recommended: run 5_analysis_RF105/reviewed/00_run_RF105_20260805_2213.R so dependencies are regenerated in order.",
      call. = FALSE
    )
  }

  if (length(candidate_paths) == 0) {
    stop(
      "Required reviewed result is missing from public and restricted output folders: ", filename, "\n",
      "Run the upstream reviewed script first.",
      call. = FALSE
    )
  }
  candidate_paths[[1]]
}

read_geocene_reviewed_csv <- function(filename) {
  file_path <- resolve_reviewed_or_restricted_csv(
    filename,
    upstream_script = "1_geocene_stove_use_20260805_2213.R"
  )
  readr::read_csv(file_path, show_col_types = FALSE)
}

as_logical_clean <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  x_chr <- str_squish(str_to_lower(as.character(x)))
  suppressWarnings(x_num <- as.numeric(x_chr))
  case_when(
    is.na(x) | x_chr == "" ~ NA,
    x_chr %in% c("true", "t", "yes", "y") ~ TRUE,
    x_chr %in% c("false", "f", "no", "n") ~ FALSE,
    !is.na(x_num) ~ x_num != 0,
    TRUE ~ NA
  )
}

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

geocene_daily_file <- "table_descriptive_stove_daily_dataset.csv"
geocene_daily_summary_file <- "table_descriptive_geocene_daily_summary.csv"

stove_data_raw <- read_geocene_reviewed_csv(geocene_daily_file) %>%
  mutate(
    timepoint = as_geocene_stove_timepoint(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = c(arm_levels, "all_arms", "missing_study_arm"))
  )
geocene_daily_summary <- read_geocene_reviewed_csv(geocene_daily_summary_file) %>%
  mutate(
    timepoint = as_geocene_stove_timepoint(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = c(arm_levels, "all_arms", "missing_study_arm"))
  )

stove_result_source_dependency <- tibble(
  downstream_section = "stove_monitor_uptake_descriptive_section",
  source_script = "1_geocene_stove_use_20260805_2213.R",
  source_file = file.path(dir_tables_reviewed, geocene_daily_file),
  source_role = "reviewed household-day stove-use analysis dataset",
  note = paste(
    "All stove-use duration calculations in this script use the reviewed",
    "Geocene result file rather than re-reading clean_final stove-use data."
  )
)

write_reviewed_csv(
  stove_result_source_dependency,
  "table_descriptive_stove_source_dependency.csv",
  subfolder = "qa"
)

stove_required_vars <- c(
  "fcn_id",
  "hh_id",
  "timepoint",
  "study_arm_overall",
  "date",
  "days_after_first_receiving",
  "months_after_first_receiving_numeric",
  "cooking_events_with_lpg_zero",
  "cooking_events_with_biomass_zero",
  "stove_on_min_sum_lpg_zero",
  "stove_on_min_sum_biomass_zero",
  "stove_on_min_sum_total_zero",
  "observed_stove_use_day",
  "n_stoves_with_recorded_use",
  "valid_exclusive_use_denominator",
  "lpg_available_for_analysis",
  "exclusive_biomass_recalc",
  "exclusive_lpg_recalc",
  "mixed_use_recalc"
)

write_reviewed_csv(
  flag_missing_vars(
    stove_data_raw,
    stove_required_vars,
    "reviewed Geocene daily analysis dataset from 1_geocene_stove_use_20260805_2213.R"
  ),
  "table_descriptive_stove_variable_availability.csv",
  subfolder = "qa"
)

missing_required <- setdiff(stove_required_vars, names(stove_data_raw))
if (length(missing_required) > 0) {
  stop(
    "Required variables are missing from the reviewed Geocene daily analysis dataset: ",
    paste(missing_required, collapse = ", "),
    call. = FALSE
  )
}

stove_data <- stove_data_raw %>%
  mutate(
    fcn_id = str_squish(as.character(fcn_id)),
    hh_id = str_squish(as.character(hh_id)),
    timepoint = as_geocene_stove_timepoint(timepoint),
    study_arm_overall = str_squish(str_to_lower(as.character(study_arm_overall))),
    date = as.Date(date),
    collection_year = lubridate::year(date),
    days_after_first_receiving =
      suppressWarnings(as.numeric(days_after_first_receiving)),
    months_after_first_receiving_numeric =
      suppressWarnings(as.numeric(months_after_first_receiving_numeric)),
    cooking_events_with_lpg_zero =
      suppressWarnings(as.numeric(cooking_events_with_lpg_zero)),
    cooking_events_with_biomass_zero =
      suppressWarnings(as.numeric(cooking_events_with_biomass_zero)),
    stove_on_min_sum_lpg_zero =
      suppressWarnings(as.numeric(stove_on_min_sum_lpg_zero)),
    stove_on_min_sum_biomass_zero =
      suppressWarnings(as.numeric(stove_on_min_sum_biomass_zero)),
    stove_on_min_sum_total_zero =
      suppressWarnings(as.numeric(stove_on_min_sum_total_zero)),
    observed_stove_use_day = as_logical_clean(observed_stove_use_day),
    n_stoves_with_recorded_use =
      suppressWarnings(as.integer(n_stoves_with_recorded_use)),
    valid_exclusive_use_denominator = as_logical_clean(valid_exclusive_use_denominator),
    lpg_available_for_analysis = as_logical_clean(lpg_available_for_analysis),
    exclusive_biomass_recalc = as_logical_clean(exclusive_biomass_recalc),
    exclusive_lpg_recalc = as_logical_clean(exclusive_lpg_recalc),
    mixed_use_recalc = as_logical_clean(mixed_use_recalc)
  ) %>%
  filter(
    !is.na(date),
    !is.na(fcn_id), fcn_id != "",
    timepoint %in% geocene_stove_timepoint_levels,
    study_arm_overall %in% c(arm_levels, "all_arms"),
    observed_stove_use_day
  )

stove_duplicate_days <- stove_data %>%
  add_count(fcn_id, date, name = "n_records_for_fcn_date") %>%
  filter(n_records_for_fcn_date > 1) %>%
  select(
    any_of(c(
      "fcn_id", "hh_id", "date", "timepoint", "study_arm_overall",
      "n_records_for_fcn_date", "raw_source_file"
    ))
  ) %>%
  arrange(fcn_id, date)

write_reviewed_csv(
  stove_duplicate_days,
  "table_descriptive_stove_duplicate_dates.csv",
  subfolder = "qa"
)

stove_sample_counts <- stove_data %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    collection_years = collapse_collection_years(collection_year),
    n_daily_records = n(),
    n_households = n_distinct(fcn_id),
    first_monitoring_date = min(date, na.rm = TRUE),
    last_monitoring_date = max(date, na.rm = TRUE),
    .groups = "drop"
  )

write_reviewed_csv(stove_sample_counts, "table_descriptive_stove_sample_counts.csv")

################################################################################
# Daily stove-use summaries
################################################################################

stove_daily_summary <- geocene_daily_summary %>%
  filter(
    timepoint %in% geocene_stove_timepoint_levels,
    study_arm_overall %in% c(arm_levels, "all_arms")
  ) %>%
  arrange(timepoint, study_arm_overall)

write_reviewed_csv(stove_daily_summary, "table_descriptive_stove_daily_summary.csv")

stove_daily_summary_check <- stove_data %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    n_daily_records = n(),
    n_households = n_distinct(fcn_id),
    n_days_exclusive_denominator = sum(valid_exclusive_use_denominator, na.rm = TRUE),
    n_exclusive_lpg_days = sum(exclusive_lpg_recalc, na.rm = TRUE),
    .groups = "drop"
  )

stove_daily_summary_compare <- stove_daily_summary %>%
  select(
    timepoint,
    study_arm_overall,
    n_daily_records,
    n_households,
    n_days_exclusive_denominator,
    n_exclusive_lpg_days
  ) %>%
  left_join(
    stove_daily_summary_check,
    by = c("timepoint", "study_arm_overall"),
    suffix = c("_geocene_summary", "_daily_dataset")
  ) %>%
  mutate(
    summaries_match = coalesce(
      n_daily_records_geocene_summary == n_daily_records_daily_dataset &
        n_households_geocene_summary == n_households_daily_dataset &
        n_days_exclusive_denominator_geocene_summary == n_days_exclusive_denominator_daily_dataset &
        n_exclusive_lpg_days_geocene_summary == n_exclusive_lpg_days_daily_dataset,
      FALSE
    )
  )

write_reviewed_csv(
  stove_daily_summary_compare,
  "table_descriptive_stove_daily_summary_consistency_check.csv",
  subfolder = "qa"
)

if (any(!stove_daily_summary_compare$summaries_match, na.rm = TRUE)) {
  stop("Downstream stove daily summary does not match the reviewed Geocene daily dataset.", call. = FALSE)
}

if (any(stove_data$valid_exclusive_use_denominator & !stove_data$observed_stove_use_day, na.rm = TRUE)) {
  stop("Downstream exclusive-use denominator includes household-days without recorded stove use.", call. = FALSE)
}

if (any(
  stove_daily_summary$timepoint == "baseline" &
    stove_daily_summary$study_arm_overall == "intervention" &
    stove_daily_summary$n_exclusive_lpg_days != 0,
  na.rm = TRUE
)) {
  stop("Downstream stove summary must carry forward zero baseline intervention exclusive-LPG days.", call. = FALSE)
}

################################################################################
# Sensitivity: exclude early days after first LPG receipt
################################################################################

early_day_thresholds <- tibble(
  sensitivity = c(
    "main_all_days",
    "exclude_first_7_days",
    "exclude_first_14_days",
    "exclude_first_30_days",
    "exclude_first_60_days"
  ),
  min_days_after_first_receiving = c(-Inf, 7, 14, 30, 60)
)

stove_early_day_sensitivity <- early_day_thresholds %>%
  mutate(data = map(min_days_after_first_receiving, function(min_days) {
    if (is.infinite(min_days)) {
      stove_data
    } else {
      stove_data %>%
        filter(
          is.na(days_after_first_receiving) |
            days_after_first_receiving >= min_days
        )
    }
  })) %>%
  unnest(data) %>%
  add_all_arms_rows() %>%
  group_by(sensitivity, min_days_after_first_receiving,
           timepoint, study_arm_overall) %>%
  summarise(
    collection_years = collapse_collection_years(collection_year),
    n_daily_records = n(),
    n_households = n_distinct(fcn_id),
    n_days_exclusive_denominator = sum(valid_exclusive_use_denominator, na.rm = TRUE),
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
    mean_lpg_minutes_per_day = mean(stove_on_min_sum_lpg_zero, na.rm = TRUE),
    mean_biomass_minutes_per_day =
      mean(stove_on_min_sum_biomass_zero, na.rm = TRUE),
    mean_total_stove_minutes_per_day =
      mean(stove_on_min_sum_total_zero, na.rm = TRUE),
    .groups = "drop"
  )

write_reviewed_csv(
  stove_early_day_sensitivity,
  "table_descriptive_stove_early_sensitivity.csv"
)

################################################################################
# Month-after-receipt summaries and figures
################################################################################

stove_month_collection_years <- stove_data %>%
  filter(lpg_available_for_analysis, !is.na(months_after_first_receiving_numeric)) %>%
  pull(collection_year) %>%
  collapse_collection_years()

stove_month_summary <- stove_data %>%
  filter(lpg_available_for_analysis, !is.na(months_after_first_receiving_numeric)) %>%
  add_all_arms_rows() %>%
  group_by(months_after_first_receiving_numeric, study_arm_overall) %>%
  summarise(
    collection_years = collapse_collection_years(collection_year),
    n_daily_records = n(),
    n_households = n_distinct(fcn_id),
    n_days_exclusive_denominator = sum(valid_exclusive_use_denominator, na.rm = TRUE),
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
    mean_lpg_minutes_per_day = mean(stove_on_min_sum_lpg_zero, na.rm = TRUE),
    mean_biomass_minutes_per_day =
      mean(stove_on_min_sum_biomass_zero, na.rm = TRUE),
    mean_total_stove_minutes_per_day =
      mean(stove_on_min_sum_total_zero, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_households >= 3)

write_reviewed_csv(stove_month_summary, "table_descriptive_stove_month_summary.csv")

fig_exclusive_lpg <- ggplot(
  stove_month_summary,
  aes(
    x = months_after_first_receiving_numeric,
    y = pct_exclusive_lpg_days,
    color = study_arm_overall
  )
) +
  geom_line(linewidth = 0.85) +
  geom_point(size = 1.7) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = label_number(suffix = "%")
  ) +
  scale_color_manual(
    values = c(comparison = "#4E79A7", intervention = "#F28E2B"),
    na.translate = FALSE
  ) +
  labs(
    x = month_axis_label("Months after first receiving LPG", stove_month_collection_years),
    y = "Daily records with exclusive LPG use",
    color = "Study arm"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

save_reviewed_plot(
  fig_exclusive_lpg,
  "fig_descriptive_stove_exclusive_lpg_month.png",
  width = 7,
  height = 4.5
)

stove_minutes_long <- stove_month_summary %>%
  select(
    months_after_first_receiving_numeric,
    study_arm_overall,
    mean_lpg_minutes_per_day,
    mean_biomass_minutes_per_day
  ) %>%
  pivot_longer(
    cols = c(mean_lpg_minutes_per_day, mean_biomass_minutes_per_day),
    names_to = "fuel_type",
    values_to = "mean_minutes_per_day"
  ) %>%
  mutate(
    fuel_type = recode(
      fuel_type,
      mean_lpg_minutes_per_day = "LPG",
      mean_biomass_minutes_per_day = "Biomass"
    )
  )

fig_stove_minutes <- ggplot(
  stove_minutes_long,
  aes(
    x = months_after_first_receiving_numeric,
    y = mean_minutes_per_day,
    color = fuel_type
  )
) +
  geom_line(linewidth = 0.85) +
  facet_wrap(~ study_arm_overall) +
  scale_color_manual(values = c(LPG = "#F28E2B", Biomass = "#4E79A7")) +
  labs(
    x = month_axis_label("Months after first receiving LPG", stove_month_collection_years),
    y = "Mean stove-on minutes per day",
    color = "Fuel type"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

save_reviewed_plot(
  fig_stove_minutes,
  "fig_descriptive_stove_minutes_fuel_month.png",
  width = 7,
  height = 4.5
)

stove_events_long <- stove_data %>%
  select(
    fcn_id, timepoint, study_arm_overall, date,
    cooking_events_with_lpg_zero, cooking_events_with_biomass_zero
  ) %>%
  pivot_longer(
    cols = c(cooking_events_with_lpg_zero, cooking_events_with_biomass_zero),
    names_to = "fuel_type",
    values_to = "cooking_events"
  ) %>%
  mutate(
    fuel_type = recode(
      fuel_type,
      cooking_events_with_lpg_zero = "LPG",
      cooking_events_with_biomass_zero = "Biomass"
    )
  )

stove_events_summary <- stove_events_long %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall, fuel_type) %>%
  summarise(
    n_daily_records = sum(!is.na(cooking_events)),
    n_households = n_distinct(fcn_id[!is.na(cooking_events)]),
    mean_events_per_day = mean(cooking_events, na.rm = TRUE),
    median_events_per_day = median(cooking_events, na.rm = TRUE),
    p25_events_per_day = quantile(cooking_events, 0.25, na.rm = TRUE),
    p75_events_per_day = quantile(cooking_events, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

write_reviewed_csv(stove_events_summary, "table_descriptive_stove_events_summary.csv")

message("RF105 reviewed downstream stove-monitor analysis complete.")
})

################################################################################
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
#   7_tables/RF105_reviewed_YYYYMMDD/table_descriptive_stove_daily_dataset.csv
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

arm_colors <- c(comparison = "#430154", intervention = "#138B87")
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
    "Food consumption score and dietary diversity",
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
  "table_descriptive_stove_daily_dataset.csv"
)
stove_daily <- readr::read_csv(stove_daily_file, show_col_types = FALSE)

pm_indoor <- readRDS(file_pm25_indoor)

################################################################################
# Composite stove-use and energy-consumption panel like RF105A fig3/fig4
################################################################################

if (!requireNamespace("gridExtra", quietly = TRUE)) {
  stop(
    "Install gridExtra before generating the stove-use composite panel.",
    call. = FALSE
  )
}

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
    timepoint = as_geocene_stove_timepoint(timepoint),
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
    lpg_available_for_analysis = as.logical(lpg_available_for_analysis),
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
    lpg_available_for_analysis,
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
################################################################################

stove_midline_intervention <- stove_daily %>%
  mutate(
    timepoint = as_geocene_stove_timepoint(timepoint),
    study_arm_overall = factor(study_arm_overall, levels = c(arm_levels, "all_arms", "missing_study_arm"))
  ) %>%
  filter(timepoint == "midline", study_arm_overall == "intervention") %>%
  mutate(
    days_after_first_receiving = as_number(days_after_first_receiving),
    months_after_first_receiving = as_number(months_after_first_receiving_numeric),
    collection_year = lubridate::year(as.Date(date)),
    stove_on_min_sum_lpg_zero = as_number(stove_on_min_sum_lpg_zero),
    stove_on_min_sum_biomass_zero = as_number(stove_on_min_sum_biomass_zero),
    lpg_available_for_analysis = as.logical(lpg_available_for_analysis),
    exclusive_lpg_recalc = as.integer(as.logical(exclusive_lpg_recalc))
  ) %>%
  filter(lpg_available_for_analysis)

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
# PM2.5 by hour of day, using reviewed clean indoor PM data
################################################################################

pm_household_hourly_input <- pm_indoor %>%
  clean_timepoint_arm() %>%
  filter(
    !is.na(timepoint),
    !is.na(study_arm_overall),
    !is.na(PM_Estimate),
    PM_Estimate > 0,
    !is.na(nearest_min),
    !is.na(fcn_id)
  ) %>%
  mutate(
    nearest_min_60 = lubridate::floor_date(nearest_min, "60 minutes"),
    PM_Estimate = as_number(PM_Estimate),
    fcn_id = as.character(fcn_id),
    hh_id = as.character(hh_id),
    raw_source_file = as.character(raw_source_file),
    PM_monitor = as.character(PM_monitor)
  ) %>%
  group_by(timepoint, study_arm_overall, fcn_id, hh_id, raw_source_file, PM_monitor, nearest_min_60) %>%
  summarise(
    n_minute_records = n(),
    PM_Estimate_household_hour = mean(PM_Estimate, na.rm = TRUE),
    PM_Estimate_household_hour_sd_minute = sd(PM_Estimate, na.rm = TRUE),
    .groups = "drop"
  )

write_reviewed_csv(
  pm_household_hourly_input,
  "table_descriptive_pm25_household_hour_inputs_internal.csv"
)

pm_hourly_plot_data <- pm_household_hourly_input %>%
  add_all_arms_rows() %>%
  group_by(timepoint, study_arm_overall, nearest_min_60) %>%
  summarise(
    n_household_hours = n(),
    n_records = sum(n_minute_records, na.rm = TRUE),
    n_households = n_distinct(fcn_id),
    n_source_files = n_distinct(raw_source_file),
    PM_Estimate_av = mean(PM_Estimate_household_hour, na.rm = TRUE),
    PM_Estimate_sd = sd(PM_Estimate_household_hour, na.rm = TRUE),
    se_pm_estimate = if_else(n_household_hours > 1, PM_Estimate_sd / sqrt(n_household_hours), NA_real_),
    lower_ci = if_else(!is.na(se_pm_estimate), pmax(0.1, PM_Estimate_av - 1.96 * se_pm_estimate), NA_real_),
    upper_ci = if_else(!is.na(se_pm_estimate), pmax(0.1, PM_Estimate_av + 1.96 * se_pm_estimate), NA_real_),
    uncertainty_unit = "household_hour",
    .groups = "drop"
  ) %>%
  arrange(timepoint, study_arm_overall, nearest_min_60)

write_reviewed_csv(
  pm_hourly_plot_data,
  "table_descriptive_pm25_hourly_plot_data.csv"
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
  "fig_descriptive_pm25_hourly_patterns.png",
  width = 10,
  height = 6
)

################################################################################
# Physical-health figure for the manuscript child/caregiver figure
################################################################################

physical_health_plot_data <- read_reviewed_csv_required(
  "table_descriptive_physical_health_symptoms.csv"
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
  "table_descriptive_health_panel_plot_data.csv"
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
  "fig_descriptive_health_symptom_panel.png",
  width = 16,
  height = 8
)

################################################################################
# Food consumption score and dietary diversity figure for manuscript FCS/HDDS plot
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
    hdds_no_misc_from_24h = hdds_no_misc,
    hdds_assume_misc_1_from_24h = if_else(
      !is.na(hdds_no_misc_from_24h),
      hdds_no_misc_from_24h + 1,
      NA_real_
    ),
    hdds_no_misc = dplyr::coalesce(hdds_no_misc_clean, hdds_no_misc_from_24h),
    hdds_assume_misc_1 = dplyr::coalesce(
      hdds_assume_misc_1_clean,
      hdds_assume_misc_1_from_24h
    )
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
    fcs_category
  ) %>%
  filter(!is.na(fcs), !is.na(timepoint), !is.na(study_arm_overall))

fcs_hdds_summary <- fcs_hdds_plot_data %>%
  add_all_arms_rows() %>%
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
  "table_descriptive_food_dietary_plot_data.csv"
)
write_reviewed_csv(
  fcs_hdds_summary,
  "table_descriptive_food_dietary_summary.csv"
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
  "fig_descriptive_food_dietary_scores.tiff",
  width = 10,
  height = 6
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

################################################################################
# Consolidated PM2.5 midline/endline explanation section
################################################################################
local({
################################################################################
# RF105 reviewed PM2.5 midline/endline explainer figures
#
# Purpose:
#   Create diagnostic figures that help explain why the reviewed PM2.5 rDiD
#   analysis shows a clearer intervention-associated difference at midline than
#   at endline.
#
# Inputs:
#   4_data/clean_final/pm25_pats_refugee_indoor.rds
#   4_data/clean_final/survey_refugee_household.rds
#   7_tables/pm25_ambient_adjusted_YYYYMMDD/
#     table_pm25_window_dataset_deidentified.csv, if available
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/pm25_explainer_*.csv
#   6_figures/RF105_reviewed_YYYYMMDD/pm25_explainer_*.png
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

first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  x[[1]]
}

as_number <- function(x) {
  suppressWarnings(as.numeric(x))
}

mean_ci <- function(x) {
  x <- x[is.finite(x) & !is.na(x)]
  if (length(x) == 0) {
    return(tibble(mean = NA_real_, conf.low = NA_real_, conf.high = NA_real_))
  }
  se <- stats::sd(x) / sqrt(length(x))
  tibble(
    mean = mean(x),
    conf.low = mean(x) - stats::qt(0.975, df = length(x) - 1) * se,
    conf.high = mean(x) + stats::qt(0.975, df = length(x) - 1) * se
  )
}

find_latest_ambient_dir <- function() {
  candidate_dirs <- list.dirs(file.path(project_root, "7_tables"),
                              full.names = TRUE, recursive = FALSE)
  candidate_dirs <- candidate_dirs[
    str_detect(basename(candidate_dirs), "^pm25_ambient_adjusted_")
  ]
  if (length(candidate_dirs) == 0) {
    return(NA_character_)
  }
  candidate_dirs[[order(basename(candidate_dirs), decreasing = TRUE)[[1]]]]
}

################################################################################
# Data-source audit
################################################################################

ambient_script_file <- file.path(
  project_root,
  "5_analysis_RF105",
  "reviewed",
  "2_pm25_ambient_adjusted_analysis_20260805_2213.R"
)

ambient_table_dir <- file.path(
  project_root,
  "7_tables",
  paste0("pm25_ambient_adjusted_", date_stamp)
)
if (!dir.exists(ambient_table_dir)) {
  ambient_table_dir <- find_latest_ambient_dir()
}

ambient_household_file <- file.path(
  ambient_table_dir,
  "table_pm25_window_dataset_deidentified.csv"
)
restricted_ambient_table_dir <- if (!is.na(ambient_table_dir)) {
  file.path(
    project_root,
    "8_restricted",
    basename(ambient_table_dir)
  )
} else {
  NA_character_
}
ambient_household_timepoint_file <- file.path(
  restricted_ambient_table_dir,
  "table_rDiD_pm25_panel_internal.csv"
)
if (is.na(ambient_household_timepoint_file) || !file.exists(ambient_household_timepoint_file)) {
  ambient_household_timepoint_file <- file.path(
    ambient_table_dir,
    "table_rDiD_pm25_panel_internal.csv"
  )
}

pm25_source_audit <- tibble(
  ambient_adjusted_script = ambient_script_file,
  ambient_adjusted_household_window_output = ambient_household_file,
  ambient_adjusted_household_timepoint_output = ambient_household_timepoint_file,
  descriptive_explainer_uses_ambient_adjusted_output = TRUE,
  note = paste(
    "This descriptive PM2.5 explainer uses the ambient-adjusted",
    "household-timepoint file generated by 2_pm25_ambient_adjusted_analysis_20260805_2213.R.",
    "The default descriptive PM metric is indoor PM2.5 minus 0.75 times",
    "concurrent ambient PM2.5."
  )
)

write_reviewed_csv(
  pm25_source_audit,
  "table_descriptive_pm25_data_source.csv",
  subfolder = "qa"
)

################################################################################
# Paired household PM2.5 panels from the ambient-adjusted household-timepoint source
################################################################################

survey_raw <- readRDS(file_survey_refugee_household) %>%
  add_rf105_aliases() %>%
  clean_timepoint_arm()

analysis_population <- make_analysis_population(survey_raw, id_var = "fcn_id")

baseline_covars <- analysis_population$all_deduplicated %>%
  filter(timepoint == "baseline") %>%
  transmute(
    fcn_id = as.character(fcn_id),
    baseline_survey_arm = case_when(
      study_arm_overall == "intervention" ~ "intervention",
      study_arm_overall == "comparison" ~ "comparison",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(fcn_id) %>%
  summarise(
    baseline_survey_arm = as.character(first_nonmissing(baseline_survey_arm)),
    .groups = "drop"
  ) %>%
  filter(!is.na(baseline_survey_arm))

if (!file.exists(ambient_household_timepoint_file)) {
  stop(
    "Missing ambient-adjusted PM household-timepoint file: ",
    ambient_household_timepoint_file,
    ". Run 5_analysis_RF105/reviewed/2_pm25_ambient_adjusted_analysis_20260805_2213.R first.",
    call. = FALSE
  )
}

pm_household <- readr::read_csv(
  ambient_household_timepoint_file,
  show_col_types = FALSE
) %>%
  clean_timepoint_arm() %>%
  transmute(
    fcn_id = as.character(fcn_id),
    timepoint,
    collection_date_min = as.Date(collection_date_min),
    collection_date_max = as.Date(collection_date_max),
    n_monitor_files = as_number(n_monitor_files),
    pm25_adjusted_default = as_number(indoor_minus_ambient_material_default)
  ) %>%
  filter(
    !is.na(fcn_id),
    fcn_id != "",
    !is.na(timepoint),
    !is.na(pm25_adjusted_default)
  )
make_pm_panel <- function(followup_timepoint, contrast_label) {
  baseline_pm <- pm_household %>%
    filter(timepoint == "baseline") %>%
    transmute(
      fcn_id,
      pm25_baseline = pm25_adjusted_default,
      baseline_collection_date_min = collection_date_min,
      baseline_collection_date_max = collection_date_max,
      baseline_monitor_files = n_monitor_files
    )

  followup_pm <- pm_household %>%
    filter(timepoint == followup_timepoint) %>%
    transmute(
      fcn_id,
      pm25_followup = pm25_adjusted_default,
      followup_collection_date_min = collection_date_min,
      followup_collection_date_max = collection_date_max,
      followup_monitor_files = n_monitor_files
    )

  baseline_covars %>%
    inner_join(baseline_pm, by = "fcn_id") %>%
    inner_join(followup_pm, by = "fcn_id") %>%
    mutate(
      contrast = contrast_label,
      followup_timepoint = followup_timepoint,
      pm25_change_followup_minus_baseline = pm25_followup - pm25_baseline,
      pm25_reduction_baseline_minus_followup = pm25_baseline - pm25_followup
    )
}

pm_paired_panels <- bind_rows(
  make_pm_panel("midline", "primary_baseline_midline"),
  make_pm_panel("endline", "secondary_baseline_endline")
) %>%
  mutate(
    baseline_survey_arm = factor(baseline_survey_arm, levels = arm_levels),
    contrast_label = recode(
      contrast,
      primary_baseline_midline = "Primary: baseline to midline",
      secondary_baseline_endline = "Secondary: baseline to endline"
    )
  )

paired_change_summary <- pm_paired_panels %>%
  group_by(contrast, contrast_label, baseline_survey_arm) %>%
  summarise(
    n_households = n_distinct(fcn_id),
    mean_baseline_pm25 = mean(pm25_baseline, na.rm = TRUE),
    mean_followup_pm25 = mean(pm25_followup, na.rm = TRUE),
    median_baseline_pm25 = median(pm25_baseline, na.rm = TRUE),
    median_followup_pm25 = median(pm25_followup, na.rm = TRUE),
    mean_change_followup_minus_baseline =
      mean(pm25_change_followup_minus_baseline, na.rm = TRUE),
    median_change_followup_minus_baseline =
      median(pm25_change_followup_minus_baseline, na.rm = TRUE),
    mean_reduction_baseline_minus_followup =
      mean(pm25_reduction_baseline_minus_followup, na.rm = TRUE),
    median_reduction_baseline_minus_followup =
      median(pm25_reduction_baseline_minus_followup, na.rm = TRUE),
    .groups = "drop"
  )

write_reviewed_csv(
  paired_change_summary,
  "table_descriptive_pm25_paired_change.csv"
)

# This descriptive PM explainer intentionally does not read rDiD result files.
# rDiD PM2.5 estimates and model figures are produced in 4_rdid_xgboost_20260805_2213.R.

paired_change_figure <- ggplot(
  pm_paired_panels,
  aes(
    x = baseline_survey_arm,
    y = pm25_change_followup_minus_baseline,
    color = baseline_survey_arm
  )
) +
  geom_hline(yintercept = 0, color = "grey35", linewidth = 0.45) +
  geom_boxplot(width = 0.45, outlier.shape = NA, alpha = 0.12) +
  geom_jitter(width = 0.08, height = 0, alpha = 0.45, size = 1.7) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    fill = "white",
    color = "black",
    size = 2.8
  ) +facet_wrap(~ contrast_label, scales = "free_y") +
  scale_color_manual(
    values = c(comparison = "#4E79A7", intervention = "#D55E00")
  ) +
  labs(
    x = NULL,
    y = "Follow-up minus baseline ambient-adjusted indoor PM2.5 (ug/m3)",
    color = NULL,
    title = "Paired household ambient-adjusted PM2.5 changes by analysis contrast",
    subtitle = "Negative values indicate lower PM2.5 at follow-up than baseline",
    caption = paste(
      "Household means use the default adjusted PM source: indoor PM2.5 minus 0.75 x concurrent ambient PM2.5",
      "as the reviewed descriptive PM2.5 explainer source."
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey92", color = "grey75")
  )

save_reviewed_plot(
  paired_change_figure,
  "fig_descriptive_pm25_paired_household_change.png",
  width = 10,
  height = 5.6
)

################################################################################
# Ambient context figure from the ambient-adjusted household-window dataset
################################################################################

if (file.exists(ambient_household_file)) {
  ambient_household <- readr::read_csv(
    ambient_household_file,
    show_col_types = FALSE
  ) %>%
    mutate(
      timepoint = as_ordered_timepoint(timepoint),
      study_arm_overall = factor(study_arm_overall, levels = arm_levels),
      has_concurrent_ambient = as.logical(has_concurrent_ambient)
    ) %>%
    filter(has_concurrent_ambient)

  ambient_context_long <- ambient_household %>%
    select(
      timepoint, study_arm_overall,
      indoor_mean_pm,
      ambient_all_mean_pm,
      indoor_minus_ambient_material_default
    ) %>%
    pivot_longer(
      cols = c(
        indoor_mean_pm,
        ambient_all_mean_pm,
        indoor_minus_ambient_material_default
      ),
      names_to = "measure",
      values_to = "pm25"
    ) %>%
    mutate(
      measure = recode(
        measure,
        indoor_mean_pm = "Indoor household mean",
        ambient_all_mean_pm = "Concurrent ambient mean",
        indoor_minus_ambient_material_default =
          "Indoor minus 0.75 x ambient"
      ),
      measure = factor(
        measure,
        levels = c(
          "Indoor household mean",
          "Concurrent ambient mean",
          "Indoor minus 0.75 x ambient"
        )
      )
    )

  ambient_context_summary <- ambient_context_long %>%
    add_all_arms_rows() %>%
    group_by(timepoint, study_arm_overall, measure) %>%
    summarise(
      n_windows = n(),
      median_pm25 = median(pm25, na.rm = TRUE),
      p25_pm25 = quantile(pm25, 0.25, na.rm = TRUE, names = FALSE),
      p75_pm25 = quantile(pm25, 0.75, na.rm = TRUE, names = FALSE),
      mean_pm25 = mean(pm25, na.rm = TRUE),
      .groups = "drop"
    )

  write_reviewed_csv(
    ambient_context_summary,
    "table_descriptive_pm25_ambient_context.csv"
  )

  ambient_context_figure <- ggplot(
    ambient_context_summary,
    aes(
      x = timepoint,
      y = median_pm25,
      ymin = p25_pm25,
      ymax = p75_pm25,
      color = study_arm_overall,
      group = study_arm_overall
    )
  ) +
    geom_hline(yintercept = 0, color = "grey70", linewidth = 0.35) +
    geom_pointrange(
      position = position_dodge(width = 0.45),
      linewidth = 0.55
    ) +
    geom_line(position = position_dodge(width = 0.45), linewidth = 0.45) +
    facet_wrap(~ measure, scales = "free_y") +
    scale_color_manual(
      values = c(comparison = "#4E79A7", intervention = "#D55E00")
    ) +
    labs(
      x = NULL,
      y = "Median PM2.5 with IQR (ug/m3)",
      color = "Study arm",
      title = "Indoor PM2.5, concurrent ambient PM2.5, and ambient-excess PM2.5",
      subtitle = "Ambient-adjusted household-window dataset; windows without concurrent ambient data are excluded",
      caption = paste(
        "Ambient-excess measure uses the default 0.75 ambient infiltration/material assumption",
        "from 2_pm25_ambient_adjusted_analysis_20260805_2213.R."
      )
    ) +
    theme_bw(base_size = 11) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "grey92", color = "grey75")
    )

  save_reviewed_plot(
    ambient_context_figure,
    "fig_descriptive_pm25_ambient_excess.png",
    width = 11,
    height = 5.5
  )
} else {
  write_reviewed_csv(
    tibble(
      expected_file = ambient_household_file,
      status = "not_found",
      note = paste(
        "Run 5_analysis_RF105/reviewed/2_pm25_ambient_adjusted_analysis_20260805_2213.R",
        "to generate the ambient-adjusted household-window dataset."
      )
    ),
    "table_descriptive_pm25_ambient_missing.csv",
    subfolder = "qa"
  )
}
})
