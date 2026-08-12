################################################################################
# RF105 requested descriptive LPG, fuel, stove, kitchen, and safety outcomes
#
# Purpose:
#   Create descriptive tables and figures for the additional requested household
#   survey outcomes, including LPG repair/distribution/safety, forest use,
#   housing, plastic burning, fuel time/cost, stove use, who cooks, cooking
#   practices, and kitchen spotcheck outcomes. This script is designed to run as
#   a standalone reviewed companion script and can later be inserted into
#   3_descriptive_outcomes_20260805_2213.R.
#
# Inputs:
#   4_data/clean_final/survey_refugee_household.rds
#
# Outputs:
#   Tables in 7_tables/RF105_reviewed_YYYYMMDD/:
#     table_descriptive_requested_binary_outcomes.csv
#     table_descriptive_requested_multiselect_outcomes.csv
#     table_descriptive_requested_categorical_outcomes.csv
#     table_descriptive_requested_continuous_outcomes.csv
#     table_descriptive_requested_text_response_presence.csv
#   QA tables in 7_tables/RF105_reviewed_YYYYMMDD/qa/:
#     table_descriptive_requested_population_counts.csv
#     table_descriptive_requested_variable_coverage.csv
#     table_descriptive_requested_multiselect_option_coverage.csv
#   Restricted QA in 8_restricted/RF105_reviewed_YYYYMMDD/qa/:
#     table_descriptive_requested_text_response_counts_internal.csv
#   Figures in 6_figures/RF105_reviewed_YYYYMMDD/:
#     fig_descriptive_requested_binary_*.png
#     fig_descriptive_requested_multiselect_*.png
#     fig_descriptive_requested_categorical_*.png
#     fig_descriptive_requested_continuous_outcomes.png
#
# Notes:
#   - Uses one deduplicated household record per fcn_id-timepoint, matching
#     make_analysis_population().
#   - Tables include arm-specific rows and all-arm pooled rows at each timepoint.
#   - Figures show comparison and intervention arms separately by timepoint.
#   - Free-text response values are written only to restricted QA; the public
#     table reports response presence and unique-response counts.
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

script_label <- "3.1_descriptive_requested_outcomes_20260812.R"
missing_response_codes <- c("77", "88", "99")

as_number <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

clean_numeric_value <- function(x, nonnegative = FALSE) {
  out <- as_number(x)
  out[out %in% as.numeric(missing_response_codes)] <- NA_real_
  if (isTRUE(nonnegative)) {
    out[out < 0] <- NA_real_
  }
  out
}

clean_category_value <- function(x, missing_codes = missing_response_codes) {
  out <- str_squish(as.character(x))
  out[is.na(x)] <- NA_character_
  out <- na_if(out, "")
  out[str_to_lower(out) %in% c("na", "nan", "null")] <- NA_character_
  out[out %in% missing_codes] <- NA_character_
  out
}

clean_text_value <- function(x) {
  out <- clean_category_value(x)
  out[out == "0"] <- NA_character_
  out
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

write_internal_text_csv <- function(x, filename, reason) {
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

add_all_arms_rows <- function(df) {
  df_arm <- df %>%
    mutate(study_arm_overall = as.character(study_arm_overall))

  df_all_arms <- df_arm %>%
    filter(!is.na(study_arm_overall), study_arm_overall %in% arm_levels) %>%
    mutate(study_arm_overall = "all_arms")

  bind_rows(df_arm, df_all_arms) %>%
    mutate(study_arm_overall = factor(study_arm_overall,
                                      levels = c(arm_levels, "all_arms")))
}

arrange_timepoint_arm <- function(.data, ...) {
  .data %>%
    arrange(
      ...,
      factor(as.character(timepoint), levels = timepoint_levels),
      factor(as.character(study_arm_overall), levels = c(arm_levels, "all_arms"))
    )
}

label_response_value <- function(value) {
  value <- as.character(value)
  case_when(
    is.na(value) ~ NA_character_,
    value == "0" ~ "0: No/none",
    value == "66" ~ "66: Other",
    str_detect(value, "^[0-9]+$") ~ paste0("Code ", value),
    TRUE ~ value
  )
}

clean_filename_token <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

split_response_codes <- function(x) {
  x_clean <- clean_category_value(x)
  lapply(x_clean, function(value) {
    if (is.na(value)) {
      return(NA_character_)
    }
    tokens <- unlist(strsplit(value, "\\s+"))
    tokens[tokens != ""]
  })
}

multi_select_any_yn <- function(x) {
  tokens <- split_response_codes(x)
  vapply(tokens, function(value) {
    if (length(value) == 1 && is.na(value)) {
      return(NA_integer_)
    }
    selected_codes <- setdiff(value, c("0", missing_response_codes))
    as.integer(length(selected_codes) > 0)
  }, integer(1))
}

multi_select_any_col <- function(df, var) {
  if (var %in% names(df)) {
    return(multi_select_any_yn(df[[var]]))
  }
  rep(NA_integer_, nrow(df))
}

positive_numeric_yn_col <- function(df, var) {
  if (!var %in% names(df)) {
    return(rep(NA_integer_, nrow(df)))
  }
  value <- clean_numeric_value(df[[var]], nonnegative = TRUE)
  case_when(
    is.na(value) ~ NA_integer_,
    value > 0 ~ 1L,
    TRUE ~ 0L
  )
}

option_codes_for_var <- function(df, base_var) {
  option_cols <- names(df)[startsWith(names(df), paste0(base_var, "/"))]
  option_codes_cols <- substring(option_cols, nchar(base_var) + 2L)

  option_codes_values <- character()
  if (base_var %in% names(df)) {
    option_codes_values <- unlist(split_response_codes(df[[base_var]]))
  }

  codes <- clean_category_value(unique(c(option_codes_cols, option_codes_values)))
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

select_multi_response <- function(df, base_var, option_code) {
  option_var <- paste0(base_var, "/", option_code)

  if (option_var %in% names(df)) {
    return(list(
      value = make_yn(df[[option_var]]),
      source_variable_used = option_var
    ))
  }

  if (base_var %in% names(df)) {
    tokens <- split_response_codes(df[[base_var]])
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

prop_ci_lower <- function(proportion, n_nonmissing) {
  if_else(
    n_nonmissing > 0 & !is.na(proportion),
    pmax(0, 100 * (proportion - qnorm(0.975) *
                     sqrt(proportion * (1 - proportion) / n_nonmissing))),
    NA_real_
  )
}

prop_ci_upper <- function(proportion, n_nonmissing) {
  if_else(
    n_nonmissing > 0 & !is.na(proportion),
    pmin(100, 100 * (proportion + qnorm(0.975) *
                      sqrt(proportion * (1 - proportion) / n_nonmissing))),
    NA_real_
  )
}

summarise_binary_vars <- function(df, var_table,
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
        ci_lower = prop_ci_lower(proportion, n_nonmissing),
        ci_upper = prop_ci_upper(proportion, n_nonmissing),
        .groups = "drop"
      ) %>%
      mutate(
        outcome_group = var_table$outcome_group[[i]],
        outcome_name = var_table$outcome_name[[i]],
        outcome_label = var_table$outcome_label[[i]],
        source_variable = var,
        requested_variable = var_table$requested_variable[[i]],
        analysis_type = "binary_percent",
        unit = "percent",
        population = population,
        denominator_note = "Percent among nonmissing responses within each timepoint-arm group; all_arms rows pool comparison and intervention households."
      )
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, requested_variable, source_variable, analysis_type,
           unit, population, n_total, n_nonmissing, n_yes, percent, ci_lower,
           ci_upper, denominator_note) %>%
    arrange_timepoint_arm(outcome_group, outcome_name)
}

summarise_continuous_vars <- function(df, var_table,
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
        value = clean_numeric_value(.data[[var]], nonnegative = TRUE)
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
        ci_lower = continuous_ci_lower(mean, se, n_nonmissing),
        ci_upper = continuous_ci_upper(mean, se, n_nonmissing),
        .groups = "drop"
      ) %>%
      mutate(
        outcome_group = var_table$outcome_group[[i]],
        outcome_name = var_table$outcome_name[[i]],
        outcome_label = var_table$outcome_label[[i]],
        source_variable = var,
        requested_variable = var_table$requested_variable[[i]],
        analysis_type = "continuous_summary",
        unit = var_table$unit[[i]],
        population = population,
        denominator_note = "Continuous summaries use nonmissing nonnegative numeric values; all_arms rows pool comparison and intervention households."
      )
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, requested_variable, source_variable, analysis_type,
           unit, population, n_total, n_nonmissing, mean, sd, median, p25,
           p75, min, max, ci_lower, ci_upper, denominator_note) %>%
    arrange_timepoint_arm(outcome_group, outcome_name)
}

summarise_categorical_vars <- function(df, var_table,
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
        category_value = clean_category_value(.data[[var]])
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
        category_label = label_response_value(category_value),
        outcome_group = var_table$outcome_group[[i]],
        outcome_name = var_table$outcome_name[[i]],
        outcome_label = var_table$outcome_label[[i]],
        source_variable = var,
        requested_variable = var_table$requested_variable[[i]],
        analysis_type = "categorical_distribution",
        unit = "percent",
        population = population,
        denominator_note = "Percent among nonmissing responses within each timepoint-arm group; all_arms rows pool comparison and intervention households."
      )
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, requested_variable, source_variable, analysis_type,
           category_value, category_label, unit, population, n_total,
           n_nonmissing, n_category, percent, denominator_note) %>%
    arrange_timepoint_arm(outcome_group, outcome_name, category_value)
}

summarise_multi_select_vars <- function(df, var_table,
                                        population = "all_deduplicated_household_timepoint_records") {
  var_table <- var_table %>% filter(source_variable %in% names(df) |
                                      map_lgl(source_variable, ~ any(startsWith(names(df), paste0(.x, "/")))))
  if (nrow(var_table) == 0) {
    return(tibble())
  }

  map_dfr(seq_len(nrow(var_table)), function(i) {
    base_var <- var_table$source_variable[[i]]
    option_codes <- option_codes_for_var(df, base_var)
    if (length(option_codes) == 0) {
      return(tibble())
    }

    map_dfr(option_codes, function(option_code) {
      selected <- select_multi_response(df, base_var, option_code)

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
          ci_lower = prop_ci_lower(proportion, n_nonmissing),
          ci_upper = prop_ci_upper(proportion, n_nonmissing),
          .groups = "drop"
        ) %>%
        mutate(
          outcome_group = var_table$outcome_group[[i]],
          outcome_name = var_table$outcome_name[[i]],
          outcome_label = var_table$outcome_label[[i]],
          requested_variable = var_table$requested_variable[[i]],
          source_variable = base_var,
          source_variable_used = selected$source_variable_used,
          option_code = option_code,
          option_label = label_response_value(option_code),
          analysis_type = "multiselect_option_percent",
          unit = "percent",
          population = population,
          denominator_note = "Percent selected among nonmissing responses for this multi-select item; all_arms rows pool comparison and intervention households."
        )
    })
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, requested_variable, source_variable,
           source_variable_used, analysis_type, option_code, option_label,
           unit, population, n_total, n_nonmissing, n_selected, percent,
           ci_lower, ci_upper, denominator_note) %>%
    arrange_timepoint_arm(outcome_group, outcome_name, option_code)
}

summarise_text_presence <- function(df, var_table,
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
        text_value = clean_text_value(.data[[var]])
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
        requested_variable = var_table$requested_variable[[i]],
        source_variable = var,
        analysis_type = "text_response_presence",
        unit = "percent",
        population = population,
        denominator_note = "Percent with a nonblank free-text response among all household-timepoint records; raw text response counts are restricted QA."
      )
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, requested_variable, source_variable, analysis_type,
           unit, population, n_total, n_nonmissing_text, n_unique_text,
           percent_with_text, denominator_note) %>%
    arrange_timepoint_arm(outcome_group, outcome_name)
}

summarise_text_response_counts <- function(df, var_table) {
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
        text_value = clean_text_value(.data[[var]])
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
        requested_variable = var_table$requested_variable[[i]],
        source_variable = var,
        analysis_type = "restricted_text_response_counts"
      )
  }) %>%
    select(timepoint, study_arm_overall, outcome_group, outcome_name,
           outcome_label, requested_variable, source_variable, analysis_type,
           text_value, n_nonmissing_text, n_text_response,
           percent_among_text_responses) %>%
    arrange_timepoint_arm(outcome_group, outcome_name, text_value)
}

write_plot_if_data <- function(plot_data, plot, filename, width = 10, height = 6) {
  if (nrow(plot_data) == 0) {
    message("No rows available for figure: ", filename)
    return(invisible(NULL))
  }
  save_reviewed_plot(plot, filename, width = width, height = height)
}

arm_colors_requested <- c(comparison = "#3B6EA8", intervention = "#C94C4C")

save_binary_figures <- function(binary_summary) {
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
    write_plot_if_data(
      plot_data,
      fig,
      paste0("fig_descriptive_requested_binary_", clean_filename_token(group_name), ".png"),
      width = 10,
      height = height
    )
  })
}

save_multiselect_figures <- function(multiselect_summary) {
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
    write_plot_if_data(
      plot_data,
      fig,
      paste0("fig_descriptive_requested_multiselect_", clean_filename_token(group_name), ".png"),
      width = 12,
      height = height
    )
  })
}

save_categorical_figures <- function(categorical_summary) {
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
    write_plot_if_data(
      plot_data,
      fig,
      paste0("fig_descriptive_requested_categorical_", clean_filename_token(group_name), ".png"),
      width = 12,
      height = height
    )
  })
}

save_continuous_figure <- function(continuous_summary) {
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
  write_plot_if_data(
    plot_data,
    fig,
    "fig_descriptive_requested_continuous_outcomes.png",
    width = 10,
    height = height
  )
}

################################################################################
# Requested variable inventory
################################################################################

requested_variables <- tribble(
  ~outcome_group, ~requested_variable, ~requested_label,
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

################################################################################
# Analysis population and derived variables
################################################################################

survey_raw <- readr::read_rds(file_survey_refugee_household) %>%
  add_rf105_aliases()

analysis_population <- make_analysis_population(survey_raw, id_var = "fcn_id")
survey <- analysis_population$all_deduplicated

survey$lpg_stove_repair_any_yn <- multi_select_any_col(survey, "lpg_stove_repair")
survey$lpg_stove_repair_inspect_any_yn <- multi_select_any_col(survey, "lpg_stove_repair_inspect")
survey$lpg_cylinder_repair_any_yn <- multi_select_any_col(survey, "lpg_cylinder_repair")
survey$lpg_cylinder_repair_inspect_any_yn <- multi_select_any_col(survey, "lpg_cylinder_repair_inspect")
survey$lpg_repair_details_any_yn <- multi_select_any_col(survey, "lpg_repair_details")
survey$lpg_afraid_any_yn <- multi_select_any_col(survey, "lpg_afraid")
survey$fire_any_yn <- positive_numeric_yn_col(survey, "fire_number")
survey$burn_plastic_any_yn <- positive_numeric_yn_col(survey, "burn_plastic_frequency")
survey$cook_sell_yesterday_any_yn <- positive_numeric_yn_col(survey, "cook_sell_yesterday")
survey$cook_pressure_cooker_any_yn <- positive_numeric_yn_col(survey, "cook_pressure_cooker")

write_reviewed_csv(
  analysis_population$sample_counts,
  "table_descriptive_requested_population_counts.csv",
  subfolder = "qa"
)

################################################################################
# Variable dictionaries
################################################################################

binary_vars <- tribble(
  ~outcome_group, ~requested_variable, ~source_variable, ~outcome_name, ~outcome_label,
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
  mutate(analysis_output = "table_descriptive_requested_binary_outcomes.csv")

multi_select_vars <- tribble(
  ~outcome_group, ~requested_variable, ~source_variable, ~outcome_name, ~outcome_label,
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
  mutate(analysis_output = "table_descriptive_requested_multiselect_outcomes.csv")

categorical_vars <- tribble(
  ~outcome_group, ~requested_variable, ~source_variable, ~outcome_name, ~outcome_label,
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
  mutate(analysis_output = "table_descriptive_requested_categorical_outcomes.csv")

continuous_vars <- tribble(
  ~outcome_group, ~requested_variable, ~source_variable, ~outcome_name, ~outcome_label, ~unit,
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
  mutate(analysis_output = "table_descriptive_requested_continuous_outcomes.csv")

text_vars <- tribble(
  ~outcome_group, ~requested_variable, ~source_variable, ~outcome_name, ~outcome_label,
  "missed_refill", "refill_time_lpg_missed_other", "refill_time_lpg_missed_other", "refill_time_lpg_missed_other", "Other reason household missed refill",
  "forest_reasons", "reason_forest_other_specified", "reason_forest_other_specified", "reason_forest_other_specified", "Other forest reason specified"
) %>%
  mutate(
    analysis_output = "table_descriptive_requested_text_response_presence.csv",
    restricted_analysis_output = "table_descriptive_requested_text_response_counts_internal.csv"
  )

################################################################################
# Tables
################################################################################

binary_summary <- summarise_binary_vars(survey, binary_vars)
multiselect_summary <- summarise_multi_select_vars(survey, multi_select_vars)
categorical_summary <- summarise_categorical_vars(survey, categorical_vars)
continuous_summary <- summarise_continuous_vars(survey, continuous_vars)
text_presence_summary <- summarise_text_presence(survey, text_vars)
text_response_counts_internal <- summarise_text_response_counts(survey, text_vars)

write_reviewed_csv(
  binary_summary,
  "table_descriptive_requested_binary_outcomes.csv"
)
write_reviewed_csv(
  multiselect_summary,
  "table_descriptive_requested_multiselect_outcomes.csv"
)
write_reviewed_csv(
  categorical_summary,
  "table_descriptive_requested_categorical_outcomes.csv"
)
write_reviewed_csv(
  continuous_summary,
  "table_descriptive_requested_continuous_outcomes.csv"
)
write_reviewed_csv(
  text_presence_summary,
  "table_descriptive_requested_text_response_presence.csv"
)

if (nrow(text_response_counts_internal) > 0) {
  write_internal_text_csv(
    text_response_counts_internal,
    "table_descriptive_requested_text_response_counts_internal.csv",
    reason = "Free-text responses can contain sensitive or identifying details and should be reviewed internally before sharing."
  )
}

analysis_map <- bind_rows(
  binary_vars %>%
    transmute(requested_variable, analysis_type = "binary_percent",
              reviewed_output = analysis_output),
  multi_select_vars %>%
    transmute(requested_variable, analysis_type = "multiselect_option_percent",
              reviewed_output = analysis_output),
  categorical_vars %>%
    transmute(requested_variable, analysis_type = "categorical_distribution",
              reviewed_output = analysis_output),
  continuous_vars %>%
    transmute(requested_variable, analysis_type = "continuous_summary",
              reviewed_output = analysis_output),
  text_vars %>%
    transmute(requested_variable, analysis_type = "text_response_presence",
              reviewed_output = analysis_output),
  text_vars %>%
    transmute(requested_variable, analysis_type = "restricted_text_response_counts",
              reviewed_output = restricted_analysis_output)
) %>%
  distinct()

analysis_map_summary <- analysis_map %>%
  group_by(requested_variable) %>%
  summarise(
    analysis_types = paste(sort(unique(analysis_type)), collapse = "; "),
    reviewed_outputs = paste(sort(unique(reviewed_output)), collapse = "; "),
    .groups = "drop"
  )

variable_coverage <- requested_variables %>%
  mutate(
    direct_variable_available = requested_variable %in% names(survey),
    slash_column_count = map_int(
      requested_variable,
      ~ sum(startsWith(names(survey), paste0(.x, "/")))
    ),
    availability_status = case_when(
      direct_variable_available ~ "available",
      slash_column_count > 0 ~ "slash_columns_available_without_base_variable",
      TRUE ~ "missing_from_clean_final"
    )
  ) %>%
  left_join(analysis_map_summary, by = "requested_variable") %>%
  mutate(
    analysis_types = replace_na(analysis_types, ""),
    reviewed_outputs = replace_na(reviewed_outputs, ""),
    note = case_when(
      str_detect(requested_variable, "_image") &
        availability_status == "missing_from_clean_final" ~
        "Requested image field was not present in the clean_final household survey file inspected by this script.",
      analysis_types == "" & availability_status != "missing_from_clean_final" ~
        "Variable is available but not assigned to a summary table; review dictionary if this was unexpected.",
      TRUE ~ ""
    ),
    generated_by = script_label
  ) %>%
  arrange(outcome_group, requested_variable)

write_reviewed_csv(
  variable_coverage,
  "table_descriptive_requested_variable_coverage.csv",
  subfolder = "qa"
)

multiselect_option_coverage <- multiselect_summary %>%
  distinct(outcome_group, outcome_name, outcome_label, requested_variable,
           source_variable, source_variable_used, option_code, option_label) %>%
  arrange(outcome_group, outcome_name, option_code)

write_reviewed_csv(
  multiselect_option_coverage,
  "table_descriptive_requested_multiselect_option_coverage.csv",
  subfolder = "qa"
)

################################################################################
# Figures
################################################################################

save_binary_figures(binary_summary)
save_multiselect_figures(multiselect_summary)
save_categorical_figures(categorical_summary)
save_continuous_figure(continuous_summary)

message("Requested descriptive outcome tables and figures complete.")
