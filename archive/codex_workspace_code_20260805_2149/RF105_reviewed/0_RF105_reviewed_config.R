################################################################################
# RF105 reviewed analysis configuration
#
# Purpose:
#   Shared paths and helper functions for reviewed RF105 companion analyses.
#
# Expected use:
#   Copy this folder into 5_analysis_RF105/ or run with:
#     Sys.setenv(ROHINGYA_ANALYSIS_ROOT = "G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis")
#
# Inputs:
#   4_data/clean_final/*.rds
#
# Outputs:
#   6_figures/RF105_reviewed_YYYYMMDD/
#   7_tables/RF105_reviewed_YYYYMMDD/
#
# Notes:
#   These helpers keep the existing tidyverse/script workflow while making
#   inputs, outputs, QA checks, and sensitivity analyses explicit.
################################################################################

required_packages <- c(
  "here", "tidyverse", "janitor", "lubridate", "broom", "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install required packages before running reviewed RF105 scripts: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(lubridate)
  library(broom)
  library(scales)
  library(here)
})

`%notin%` <- Negate(`%in%`)

date_stamp <- format(Sys.Date(), "%Y%m%d")

project_root <- Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = here::here())

dir_clean_final <- file.path(project_root, "4_data", "clean_final")
dir_figures_reviewed <- file.path(
  project_root, "6_figures", paste0("RF105_reviewed_", date_stamp)
)
dir_tables_reviewed <- file.path(
  project_root, "7_tables", paste0("RF105_reviewed_", date_stamp)
)
dir_tables_qa <- file.path(dir_tables_reviewed, "qa")

dir.create(dir_figures_reviewed, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tables_reviewed, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_tables_qa, recursive = TRUE, showWarnings = FALSE)

file_survey_refugee_household <- file.path(
  dir_clean_final, "survey_refugee_household.rds"
)
file_pm25_indoor <- file.path(
  dir_clean_final, "pm25_pats_refugee_indoor.rds"
)
file_pm25_ambient <- file.path(
  dir_clean_final, "pm25_pats_refugee_ambient.rds"
)
file_stove_daily <- file.path(
  dir_clean_final, "stove_use_geocene_refugee_daily.rds"
)

timepoint_levels <- c("baseline", "midline", "endline")
arm_levels <- c("comparison", "intervention")

exchange_bdt_per_usd <- c(
  baseline = 84.91,
  midline = 84.76,
  endline = 93.99
)

write_reviewed_csv <- function(x, filename, subfolder = NULL) {
  out_dir <- if (is.null(subfolder)) {
    dir_tables_reviewed
  } else {
    file.path(dir_tables_reviewed, subfolder)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, filename)
  readr::write_csv(x, out_file, na = "")
  message("Wrote table: ", out_file)
  invisible(out_file)
}

save_reviewed_plot <- function(plot, filename, width = 8, height = 5,
                               units = "in", dpi = 300, bg = "white") {
  out_file <- file.path(dir_figures_reviewed, filename)
  ggplot2::ggsave(
    filename = out_file,
    plot = plot,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    bg = bg
  )
  message("Wrote figure: ", out_file)
  invisible(out_file)
}

clean_timepoint_arm <- function(df) {
  df %>%
    mutate(
      timepoint = str_squish(str_to_lower(as.character(timepoint))),
      study_arm_overall = str_squish(str_to_lower(as.character(study_arm_overall))),
      timepoint = factor(timepoint, levels = timepoint_levels),
      study_arm_overall = factor(study_arm_overall, levels = arm_levels)
    )
}

make_yn <- function(x) {
  x_chr <- str_squish(str_to_lower(as.character(x)))
  suppressWarnings(x_num <- as.numeric(x_chr))

  case_when(
    is.na(x) ~ NA_integer_,
    !is.na(x_num) ~ as.integer(x_num > 0),
    x_chr %in% c("yes", "y", "true", "present") ~ 1L,
    x_chr %in% c("no", "n", "false", "absent") ~ 0L,
    TRUE ~ NA_integer_
  )
}

add_rf105_aliases <- function(df) {
  # Existing RF105 scripts used these older variable names. The clean_final
  # survey file uses shorter names, so create aliases rather than changing
  # every downstream analysis line.
  if ("fuel_30_scraps" %in% names(df) &&
      "fuel_30_gather_scraps" %notin% names(df)) {
    df$fuel_30_gather_scraps <- df$fuel_30_scraps
  }

  if ("fuel_ever_scraps" %in% names(df) &&
      "fuel_ever_gather_scraps" %notin% names(df)) {
    df$fuel_ever_gather_scraps <- df$fuel_ever_scraps
  }
  any_yn_from <- function(vars) {
    vars <- vars[vars %in% names(df)]
    if (length(vars) == 0) {
      return(rep(NA_integer_, nrow(df)))
    }
    mat <- do.call(cbind, lapply(vars, function(var) make_yn(df[[var]])))
    out <- as.integer(rowSums(mat == 1, na.rm = TRUE) > 0)
    out[rowSums(!is.na(mat)) == 0] <- NA_integer_
    out
  }

  if ("fuel_30_lpg" %notin% names(df)) {
    df$fuel_30_lpg <- any_yn_from(c("fuel_30_receive_lpg", "fuel_30_buy_lpg"))
  }

  if ("fuel_30_wood" %notin% names(df)) {
    df$fuel_30_wood <- any_yn_from(c(
      "fuel_30_collect_wood", "fuel_30_buy_wood", "fuel_30_receive_wood"
    ))
  }

  if ("fuel_30_charcoal" %notin% names(df)) {
    df$fuel_30_charcoal <- any_yn_from(c(
      "fuel_30_receive_crh", "fuel_30_buy_crh"
    ))
  }

  if ("fuel_use_non_lpg_ever" %in% names(df) &&
      "fuel_use_non_lpg" %notin% names(df)) {
    df$fuel_use_non_lpg <- df$fuel_use_non_lpg_ever
  }

  if ("fuel_cant_afford_2wk" %in% names(df) &&
      "fuel_cant_afford" %notin% names(df)) {
    df$fuel_cant_afford <- df$fuel_cant_afford_2wk
  }

  if ("plastic_cook" %notin% names(df) &&
      any(c("burn_plastic_types", "burn_plastic_reason") %in% names(df))) {
    plastic_fields <- c("burn_plastic_types", "burn_plastic_reason")
    plastic_fields <- plastic_fields[plastic_fields %in% names(df)]
    plastic_mat <- do.call(cbind, lapply(plastic_fields, function(var) {
      value <- str_squish(as.character(df[[var]]))
      !is.na(df[[var]]) & value != ""
    }))
    df$plastic_cook <- as.integer(rowSums(plastic_mat, na.rm = TRUE) > 0)
  }

  if ("total_income_30" %in% names(df) && "income" %notin% names(df)) {
    df$income <- df$total_income_30
  }

  if ("debt_total" %in% names(df) && "debt_yn" %notin% names(df)) {
    df$debt_yn <- make_yn(df$debt_total)
  } else if ("debt" %in% names(df) && "debt_yn" %notin% names(df)) {
    df$debt_yn <- make_yn(df$debt)
  }

  asset_roots <- c(
    "electric_fan", "smartphone", "mobile_phone", "mattress", "blanket",
    "mosquito_net", "umbrella", "chair_bench", "table", "shovel",
    "sickle", "weaving_tool", "chicken_duck_pigeon"
  )

  for (root in asset_roots) {
    yn_name <- paste0(root, "_yn")
    if (root %in% names(df) && yn_name %notin% names(df)) {
      df[[yn_name]] <- make_yn(df[[root]])
    }
  }

  df
}

flag_missing_vars <- function(df, vars, context) {
  tibble(
    context = context,
    variable = vars,
    available = vars %in% names(df)
  ) %>%
    mutate(
      status = if_else(available, "available", "missing_from_clean_final")
    )
}

make_analysis_population <- function(df, id_var = "fcn_id") {
  stopifnot(id_var %in% names(df))

  df_clean <- df %>%
    clean_timepoint_arm() %>%
    filter(
      !is.na(.data[[id_var]]),
      .data[[id_var]] != "",
      !is.na(timepoint),
      timepoint %in% timepoint_levels
    )

  duplicate_records <- df_clean %>%
    add_count(.data[[id_var]], timepoint, name = "n_records_for_id_timepoint") %>%
    filter(n_records_for_id_timepoint > 1) %>%
    select(any_of(c(
      id_var, "hh_id", "timepoint", "study_arm_overall", "camp_id",
      "block_id", "subblock_id", "start_date", "end_date",
      "submission_time", "raw_source_file", "n_records_for_id_timepoint"
    ))) %>%
    arrange(.data[[id_var]], timepoint)

  arrange_cols <- intersect(
    c(id_var, "timepoint", "start_date", "end_date", "submission_time"),
    names(df_clean)
  )

  # Keep the first record after a deterministic sort. This mirrors the old
  # "one household per timepoint" analysis expectation while preserving a QA
  # file with all records that were collapsed.
  df_dedup <- df_clean %>%
    arrange(across(all_of(arrange_cols))) %>%
    group_by(.data[[id_var]], timepoint) %>%
    slice(1) %>%
    ungroup()

  complete_ids <- df_dedup %>%
    distinct(.data[[id_var]], timepoint) %>%
    count(.data[[id_var]], name = "n_timepoints") %>%
    filter(n_timepoints == length(timepoint_levels)) %>%
    pull(.data[[id_var]])

  df_complete <- df_dedup %>%
    filter(.data[[id_var]] %in% complete_ids)

  baseline_followup_status <- df_dedup %>%
    filter(timepoint == "baseline") %>%
    mutate(
      three_survey_participant = .data[[id_var]] %in% complete_ids,
      attrition_status = if_else(
        three_survey_participant,
        "participated_in_all_3_surveys",
        "lost_before_endline"
      )
    )

  sample_counts <- df_dedup %>%
    mutate(three_survey_participant = .data[[id_var]] %in% complete_ids) %>%
    count(timepoint, study_arm_overall, three_survey_participant, name = "n") %>%
    arrange(timepoint, study_arm_overall, desc(three_survey_participant))

  list(
    all_deduplicated = df_dedup,
    complete_3_survey = df_complete,
    baseline_followup_status = baseline_followup_status,
    duplicate_records = duplicate_records,
    sample_counts = sample_counts
  )
}

format_mean_sd <- function(x, digits = 1) {
  if (all(is.na(x))) {
    return("")
  }
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f)"),
    mean(x, na.rm = TRUE),
    sd(x, na.rm = TRUE)
  )
}

format_n_pct <- function(x, digits = 1) {
  denom <- sum(!is.na(x))
  n_yes <- sum(x == 1, na.rm = TRUE)
  if (denom == 0) {
    return("")
  }
  sprintf(paste0("%d (%.", digits, "f%%)"), n_yes, 100 * n_yes / denom)
}

format_categories <- function(x, digits = 1) {
  x <- as.character(x)
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    return("")
  }
  tab <- sort(table(x), decreasing = TRUE)
  paste(
    sprintf(
      paste0("%s: %d (%.", digits, "f%%)"),
      names(tab), as.integer(tab), 100 * as.integer(tab) / sum(tab)
    ),
    collapse = "; "
  )
}

is_binary_var <- function(x) {
  x_nonmissing <- x[!is.na(x)]
  if (length(x_nonmissing) == 0) {
    return(FALSE)
  }
  all(unique(x_nonmissing) %in% c(0, 1, FALSE, TRUE))
}

format_p <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

calc_group_p <- function(df, var, group_var) {
  dat <- df %>%
    filter(!is.na(.data[[var]]), !is.na(.data[[group_var]]))

  if (n_distinct(dat[[group_var]]) < 2 || nrow(dat) == 0) {
    return(NA_real_)
  }

  x <- dat[[var]]
  g <- as.factor(dat[[group_var]])

  out <- tryCatch({
    if (is.numeric(x) && !is_binary_var(x)) {
      if (nlevels(g) == 2) {
        t.test(x ~ g)$p.value
      } else {
        summary(aov(x ~ g))[[1]][["Pr(>F)"]][1]
      }
    } else {
      suppressWarnings(chisq.test(table(x, g))$p.value)
    }
  }, error = function(e) NA_real_)

  as.numeric(out)
}

make_characteristics_table <- function(df, vars, labels = NULL, group_var = NULL,
                                       binary_vars = character()) {
  vars <- vars[vars %in% names(df)]
  labels <- labels[vars]
  labels[is.na(labels)] <- vars[is.na(labels)]

  summarise_one_group <- function(dat, group_name) {
    map_dfr(vars, function(var) {
      x <- dat[[var]]
      is_binary <- var %in% binary_vars || is_binary_var(x)

      value <- if (is.numeric(x) && !is_binary) {
        format_mean_sd(x)
      } else if (is_binary) {
        format_n_pct(as.integer(x))
      } else {
        format_categories(x)
      }

      tibble(
        variable = var,
        characteristic = labels[[var]],
        group = group_name,
        value = value
      )
    })
  }

  if (is.null(group_var)) {
    summarise_one_group(df, "overall") %>%
      select(variable, characteristic, overall = value)
  } else {
    groups <- split(df, df[[group_var]], drop = TRUE)
    group_table <- imap_dfr(groups, summarise_one_group) %>%
      pivot_wider(names_from = group, values_from = value)

    p_table <- tibble(
      variable = vars,
      p_value = map_dbl(vars, ~ calc_group_p(df, .x, group_var))
    ) %>%
      mutate(p_value = format_p(p_value))

    group_table %>%
      left_join(p_table, by = "variable") %>%
      relocate(p_value, .after = last_col())
  }
}

did_lm_sensitivity <- function(df, outcome, end_timepoint = "endline",
                               id_var = "fcn_id") {
  dat <- df %>%
    filter(
      timepoint %in% c("baseline", end_timepoint),
      study_arm_overall %in% arm_levels
    ) %>%
    mutate(
      y = suppressWarnings(as.numeric(.data[[outcome]])),
      time_after_baseline = as.integer(timepoint == end_timepoint),
      intervention_arm = as.integer(study_arm_overall == "intervention")
    ) %>%
    filter(!is.na(y))

  if (nrow(dat) == 0 || n_distinct(dat$time_after_baseline) < 2 ||
      n_distinct(dat$intervention_arm) < 2) {
    return(tibble(
      outcome = outcome,
      comparison_timepoint = end_timepoint,
      estimate = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_,
      p.value = NA_real_,
      n = nrow(dat),
      note = "insufficient nonmissing data"
    ))
  }

  fit <- lm(y ~ time_after_baseline * intervention_arm, data = dat)

  broom::tidy(fit, conf.int = TRUE) %>%
    filter(term == "time_after_baseline:intervention_arm") %>%
    transmute(
      outcome = outcome,
      comparison_timepoint = end_timepoint,
      estimate = estimate,
      conf.low = conf.low,
      conf.high = conf.high,
      p.value = p.value,
      n = nrow(dat),
      note = "unadjusted DiD sensitivity; positive estimate means larger intervention-arm increase"
    )
}
