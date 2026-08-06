################################################################################
# RF105B Table 1 workbook filler
#
# Purpose:
#   Fill the RF105B Table 1 Excel template with baseline descriptive
#   characteristics and tests for differences across participation-pattern
#   groups, while preserving the original workbook as a template.
#
# Inputs:
#   4_data/clean_final/survey_refugee_household.rds
#   7_tables/RF105B Table 1.xlsx
#
# Outputs:
#   7_tables/RF105B Table 1_filled_YYYYMMDD.xlsx
#   7_tables/RF105B_table1_YYYYMMDD/qa/*.csv
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
config_file_candidates <- c(
  file.path(script_dir, "0_RF105_reviewed_config.R"),
  file.path(getwd(), "code", "RF105_reviewed", "0_RF105_reviewed_config.R"),
  file.path(getwd(), "5_analysis_RF105", "0_RF105_reviewed_config.R"),
  file.path(Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = ""),
            "5_analysis_RF105", "0_RF105_reviewed_config.R"),
  "G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/5_analysis_RF105/0_RF105_reviewed_config.R"
)
config_file <- config_file_candidates[file.exists(config_file_candidates)][1]

if (is.na(config_file)) {
  stop("Could not find 0_RF105_reviewed_config.R. Set ROHINGYA_ANALYSIS_ROOT.")
}

source(config_file)

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop(
    "Package 'openxlsx' is required to preserve and fill the Excel template. ",
    "Install it with install.packages(\"openxlsx\") and rerun this script."
  )
}

date_stamp <- format(Sys.Date(), "%Y%m%d")
analysis_root_default <- "G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis"
analysis_root <- Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = analysis_root_default)

input_survey_file <- file.path(
  analysis_root, "4_data", "clean_final", "survey_refugee_household.rds"
)
baseline_raw_file <- Sys.getenv(
  "RF105B_TABLE1_BASELINE_RAW",
  unset = file.path(
    analysis_root, "2_data_raw", "survey_baseline",
    "RohingyaFuelMaster_Corrected_20200419_refugee.csv"
  )
)
template_file <- Sys.getenv(
  "RF105B_TABLE1_TEMPLATE",
  unset = file.path(analysis_root, "7_tables", "RF105B Table 1.xlsx")
)
output_file <- Sys.getenv(
  "RF105B_TABLE1_OUTPUT",
  unset = file.path(
    analysis_root, "7_tables",
    paste0("RF105B Table 1_filled_", date_stamp, ".xlsx")
  )
)
qa_dir <- file.path(
  analysis_root, "7_tables", paste0("RF105B_table1_", date_stamp), "qa"
)

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_survey_file)) {
  stop("Input survey file does not exist: ", input_survey_file)
}

if (!file.exists(template_file)) {
  stop("Table 1 template does not exist: ", template_file)
}

write_qa_csv <- function(x, filename) {
  out_file <- file.path(qa_dir, filename)
  readr::write_csv(x, out_file, na = "")
  message("Wrote QA file: ", out_file)
  invisible(out_file)
}

as_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

make_binary <- function(x) {
  x_chr <- stringr::str_squish(stringr::str_to_lower(as.character(x)))
  x_num <- as_num(x_chr)

  dplyr::case_when(
    is.na(x) ~ NA_integer_,
    !is.na(x_num) ~ as.integer(x_num > 0),
    x_chr %in% c("yes", "y", "true", "present") ~ 1L,
    x_chr %in% c("no", "n", "false", "absent") ~ 0L,
    TRUE ~ NA_integer_
  )
}

coalesce_num <- function(...) {
  vals <- list(...)
  out <- rep(NA_real_, length(vals[[1]]))

  for (value in vals) {
    value_num <- as_num(value)
    replace_idx <- is.na(out) & !is.na(value_num)
    out[replace_idx] <- value_num[replace_idx]
  }

  out
}

field_or_na <- function(df, var) {
  if (var %in% names(df)) df[[var]] else rep(NA, nrow(df))
}

ensure_cols <- function(df, vars) {
  missing_vars <- setdiff(vars, names(df))

  for (var in missing_vars) {
    df[[var]] <- NA
  }

  df
}

coalesce_character <- function(primary, fallback) {
  primary_chr <- as.character(primary)
  fallback_chr <- as.character(fallback)
  missing_primary <- is.na(primary_chr) | stringr::str_squish(primary_chr) == ""
  primary_chr[missing_primary] <- fallback_chr[missing_primary]
  primary_chr
}

first_nonmissing <- function(x) {
  x_chr <- as.character(x)
  x_chr <- x_chr[!is.na(x_chr) & stringr::str_squish(x_chr) != ""]

  if (length(x_chr) == 0) {
    return(NA_character_)
  }

  x_chr[[1]]
}

row_sum_numeric <- function(df) {
  if (ncol(df) == 0) {
    return(rep(NA_real_, nrow(df)))
  }

  num_df <- as.data.frame(lapply(df, as_num))
  observed <- rowSums(!is.na(num_df)) > 0
  out <- rowSums(num_df, na.rm = TRUE)
  out[!observed] <- NA_real_
  out
}

format_mean_sd <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return("")
  }

  sprintf(
    paste0("%.", digits, "f (%.", digits, "f)"),
    mean(x),
    stats::sd(x)
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

format_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    p < 0.001 ~ "<0.001",
    TRUE ~ sprintf("%.3f", p)
  )
}

calc_p <- function(df, value_var, group_var, var_type) {
  dat <- df %>%
    dplyr::filter(!is.na(.data[[value_var]]), !is.na(.data[[group_var]]))

  if (nrow(dat) == 0 || dplyr::n_distinct(dat[[group_var]]) < 2) {
    return(NA_real_)
  }

  groups <- table(dat[[group_var]])
  if (length(groups) < 2 || any(groups == 0)) {
    return(NA_real_)
  }

  out <- tryCatch({
    if (identical(var_type, "continuous")) {
      stats::t.test(dat[[value_var]] ~ as.factor(dat[[group_var]]))$p.value
    } else {
      tab <- table(dat[[value_var]], dat[[group_var]])
      if (nrow(tab) < 2 || ncol(tab) < 2) {
        return(NA_real_)
      }

      chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
      if (any(chi$expected < 5)) {
        stats::fisher.test(tab, simulate.p.value = any(dim(tab) > 2))$p.value
      } else {
        chi$p.value
      }
    }
  }, error = function(e) NA_real_)

  as.numeric(out)
}

summarise_for_group <- function(df, var, var_type) {
  x <- df[[var]]

  if (identical(var_type, "continuous")) {
    format_mean_sd(x)
  } else {
    format_n_pct(as.integer(x))
  }
}

make_wide_summary <- function(df, rows, group_levels) {
  purrr::map_dfr(seq_len(nrow(rows)), function(i) {
    var <- rows$variable[[i]]
    var_type <- rows$type[[i]]

    group_values <- purrr::map_chr(group_levels, function(group_level) {
      summarise_for_group(
        df %>% dplyr::filter(.data$group == group_level),
        var,
        var_type
      )
    })

    tibble::tibble(
      row = rows$row[[i]],
      variable = var,
      label = rows$label[[i]]
    ) %>%
      dplyr::bind_cols(tibble::as_tibble_row(stats::setNames(group_values, group_levels)))
  })
}

make_pairwise_p <- function(df, rows, group_a, group_b) {
  purrr::map_dfr(seq_len(nrow(rows)), function(i) {
    dat <- df %>%
      dplyr::filter(.data$group %in% c(group_a, group_b)) %>%
      dplyr::mutate(test_group = factor(.data$group, levels = c(group_a, group_b)))

    tibble::tibble(
      row = rows$row[[i]],
      variable = rows$variable[[i]],
      p_value = format_p(calc_p(dat, rows$variable[[i]], "test_group", rows$type[[i]]))
    )
  })
}

describe_test <- function(df, value_var, group_var, var_type) {
  dat <- df %>%
    dplyr::filter(!is.na(.data[[value_var]]), !is.na(.data[[group_var]]))

  if (nrow(dat) == 0 || dplyr::n_distinct(dat[[group_var]]) < 2) {
    return("not enough data")
  }

  if (identical(var_type, "continuous")) {
    return("Welch t-test")
  }

  tab <- table(dat[[value_var]], dat[[group_var]])
  if (nrow(tab) < 2 || ncol(tab) < 2) {
    return("not enough data")
  }

  chi <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
  if (any(chi$expected < 5)) {
    return("Fisher exact")
  }

  "Chi-square"
}

make_pairwise_p_detail <- function(df, rows, group_a, group_b, comparison_name) {
  purrr::map_dfr(seq_len(nrow(rows)), function(i) {
    row_var <- rows$variable[[i]]
    row_type <- rows$type[[i]]
    dat <- df %>%
      dplyr::filter(.data$group %in% c(group_a, group_b)) %>%
      dplyr::mutate(test_group = factor(.data$group, levels = c(group_a, group_b)))
    values_a <- dat[[row_var]][dat$group == group_a]
    values_b <- dat[[row_var]][dat$group == group_b]
    p_raw <- calc_p(dat, row_var, "test_group", row_type)

    tibble::tibble(
      comparison = comparison_name,
      row = rows$row[[i]],
      label = rows$label[[i]],
      variable = row_var,
      type = row_type,
      group_a = group_a,
      group_b = group_b,
      group_a_n_nonmissing = sum(!is.na(values_a)),
      group_b_n_nonmissing = sum(!is.na(values_b)),
      group_a_mean = if (identical(row_type, "continuous")) mean(values_a, na.rm = TRUE) else NA_real_,
      group_b_mean = if (identical(row_type, "continuous")) mean(values_b, na.rm = TRUE) else NA_real_,
      group_a_sd = if (identical(row_type, "continuous")) stats::sd(values_a, na.rm = TRUE) else NA_real_,
      group_b_sd = if (identical(row_type, "continuous")) stats::sd(values_b, na.rm = TRUE) else NA_real_,
      group_a_yes = if (!identical(row_type, "continuous")) sum(values_a == 1, na.rm = TRUE) else NA_integer_,
      group_b_yes = if (!identical(row_type, "continuous")) sum(values_b == 1, na.rm = TRUE) else NA_integer_,
      group_a_pct = if (!identical(row_type, "continuous")) 100 * mean(values_a == 1, na.rm = TRUE) else NA_real_,
      group_b_pct = if (!identical(row_type, "continuous")) 100 * mean(values_b == 1, na.rm = TRUE) else NA_real_,
      test = describe_test(dat, row_var, "test_group", row_type),
      p_value_raw = p_raw,
      p_value = format_p(p_raw)
    )
  })
}

income_component_fields <- c(
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

baseline_fallback_fields <- c(
  "mobile_phone",
  "smartphone",
  income_component_fields
)

read_baseline_fallback <- function(file, fields) {
  if (!file.exists(file)) {
    warning("Baseline raw fallback file does not exist: ", file)
    return(tibble::tibble())
  }

  raw <- readr::read_csv(
    file,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )

  missing_fields <- setdiff(c("fcn_id", fields), names(raw))
  if (length(missing_fields) > 0) {
    warning(
      "Baseline raw fallback file is missing expected fields: ",
      paste(missing_fields, collapse = ", ")
    )
  }

  keep_fields <- intersect(c("fcn_id", fields), names(raw))
  raw <- raw %>%
    dplyr::select(dplyr::all_of(keep_fields)) %>%
    dplyr::mutate(
      raw_baseline_row = dplyr::row_number(),
      fcn_id = stringr::str_squish(as.character(.data$fcn_id))
    ) %>%
    dplyr::filter(!is.na(.data$fcn_id), .data$fcn_id != "")

  duplicate_rows <- raw %>%
    dplyr::group_by(.data$fcn_id) %>%
    dplyr::filter(dplyr::n() > 1L) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$fcn_id, .data$raw_baseline_row)

  write_qa_csv(
    duplicate_rows,
    "rf105b_table1_raw_baseline_duplicate_fcn_id_records.csv"
  )

  raw %>%
    dplyr::arrange(.data$fcn_id, .data$raw_baseline_row) %>%
    dplyr::group_by(.data$fcn_id) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(setdiff(keep_fields, "fcn_id")), first_nonmissing),
      .groups = "drop"
    )
}

survey_data_raw <- readRDS(input_survey_file) %>%
  add_rf105_aliases()

baseline_fallback <- read_baseline_fallback(
  baseline_raw_file,
  baseline_fallback_fields
)

if (nrow(baseline_fallback) > 0) {
  baseline_fallback_renamed <- baseline_fallback %>%
    dplyr::rename_with(
      ~paste0(.x, "_raw_baseline"),
      dplyr::all_of(setdiff(names(baseline_fallback), "fcn_id"))
    )

  survey_data_raw <- survey_data_raw %>%
    dplyr::mutate(fcn_id = as.character(.data$fcn_id)) %>%
    dplyr::left_join(baseline_fallback_renamed, by = "fcn_id")

  for (field in baseline_fallback_fields) {
    fallback_field <- paste0(field, "_raw_baseline")
    if (fallback_field %in% names(survey_data_raw)) {
      primary <- field_or_na(survey_data_raw, field)
      survey_data_raw[[field]] <- coalesce_character(
        primary,
        survey_data_raw[[fallback_field]]
      )
    }
  }
}

analysis_population <- make_analysis_population(survey_data_raw, id_var = "fcn_id")
survey_dedup <- analysis_population$all_deduplicated %>%
  dplyr::mutate(
    timepoint = as.character(.data$timepoint),
    study_arm_overall = as.character(.data$study_arm_overall)
  )

write_qa_csv(
  analysis_population$duplicate_records,
  "rf105b_table1_duplicate_fcn_id_timepoint_records.csv"
)

presence_flags <- survey_dedup %>%
  dplyr::distinct(.data$fcn_id, .data$timepoint) %>%
  dplyr::mutate(present = TRUE) %>%
  tidyr::pivot_wider(
    names_from = "timepoint",
    values_from = "present",
    values_fill = FALSE
  )

for (timepoint in timepoint_levels) {
  if (timepoint %notin% names(presence_flags)) {
    presence_flags[[timepoint]] <- FALSE
  }
}

baseline_data <- survey_dedup %>%
  dplyr::filter(.data$timepoint == "baseline") %>%
  dplyr::left_join(presence_flags, by = "fcn_id") %>%
  ensure_cols(c(
    "target_child_sex", "target_child_months", "hh_size", "hh_size_u2mo",
    "hh_size_2mo_u5", "hh_ppl_smoke", "income", "total_income_30",
    income_component_fields,
    "spent_total_month", "debt", "debt_total", "electricity", "mattress",
    "chair_bench", "portable_lamp", "solar_lamp", "mobile_phone",
    "smartphone", "electric_fan", "shovel", "sickle", "weaving_tool",
    "chicken_duck_pigeon"
  )) %>%
  dplyr::mutate(
    baseline = dplyr::coalesce(.data$baseline, FALSE),
    midline = dplyr::coalesce(.data$midline, FALSE),
    endline = dplyr::coalesce(.data$endline, FALSE),
    group_g1 = .data$baseline,
    group_g1a = .data$baseline & !.data$midline & !.data$endline,
    group_g2 = .data$baseline & .data$midline,
    group_g2a = .data$baseline & .data$midline & !.data$endline,
    group_g3a = .data$baseline & !.data$endline,
    group_g4 = .data$baseline & .data$midline & .data$endline,
    participation_pattern = paste0(
      dplyr::if_else(.data$baseline, "B", ""),
      dplyr::if_else(.data$midline, "M", ""),
      dplyr::if_else(.data$endline, "E", "")
    ),
    electricity = dplyr::case_when(
      as.character(.data$electricity) == "2" ~ 1L,
      TRUE ~ make_binary(.data$electricity)
    ),
    target_child_female = dplyr::case_when(
      stringr::str_squish(stringr::str_to_lower(as.character(.data$target_child_sex))) %in%
        c("female", "f", "girl", "1") ~ 1L,
      stringr::str_squish(stringr::str_to_lower(as.character(.data$target_child_sex))) %in%
        c("male", "m", "boy", "0") ~ 0L,
      TRUE ~ NA_integer_
    ),
    target_child_months_num = as_num(.data$target_child_months),
    hh_size_num = as_num(.data$hh_size),
    hh_size_u2mo_num = as_num(.data$hh_size_u2mo),
    hh_size_2mo_u5_num = as_num(.data$hh_size_2mo_u5),
    hh_ppl_smoke_num = as_num(.data$hh_ppl_smoke),
    income_component_total_bdt = row_sum_numeric(
      dplyr::across(dplyr::all_of(income_component_fields))
    ),
    income_usd = coalesce_num(
      .data$income_component_total_bdt,
      .data$income,
      .data$total_income_30
    ) / exchange_bdt_per_usd[["baseline"]],
    spent_total_month_usd =
      as_num(.data$spent_total_month) / exchange_bdt_per_usd[["baseline"]],
    debt_usd = coalesce_num(
      .data$debt,
      .data$debt_total
    ) / exchange_bdt_per_usd[["baseline"]],
    mattress_yn = make_binary(.data$mattress),
    chair_bench_yn = make_binary(.data$chair_bench),
    portable_lamp_yn = make_binary(.data$portable_lamp),
    solar_lamp_yn = make_binary(.data$solar_lamp),
    mobile_phone_yn = make_binary(.data$mobile_phone),
    smartphone_yn = make_binary(.data$smartphone),
    electric_fan_yn = make_binary(.data$electric_fan),
    shovel_yn = make_binary(.data$shovel),
    sickle_yn = make_binary(.data$sickle),
    weaving_tool_yn = make_binary(.data$weaving_tool),
    chicken_duck_pigeon_yn = make_binary(.data$chicken_duck_pigeon)
  )

baseline_endline_without_midline <- baseline_data %>%
  dplyr::filter(.data$baseline, !.data$midline, .data$endline) %>%
  dplyr::select(dplyr::any_of(c(
    "fcn_id", "hh_id", "study_arm_overall", "camp_id", "block_id",
    "subblock_id", "participation_pattern", "raw_source_file"
  ))) %>%
  dplyr::arrange(.data$study_arm_overall, .data$fcn_id)

write_qa_csv(
  baseline_endline_without_midline,
  "rf105b_table1_baseline_endline_without_midline_households.csv"
)

table_rows <- tibble::tribble(
  ~row, ~label, ~variable, ~type,
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

source_field_map <- tibble::tribble(
  ~variable, ~source_fields, ~source_rule,
  "target_child_female", "target_child_sex", "all",
  "target_child_months_num", "target_child_months", "all",
  "hh_size_num", "hh_size", "all",
  "hh_size_2mo_u5_num", "hh_size_2mo_u5", "all",
  "hh_ppl_smoke_num", "hh_ppl_smoke", "all",
  "income_usd", paste(income_component_fields, collapse = "; "), "all",
  "spent_total_month_usd", "spent_total_month", "all",
  "debt_usd", "debt; debt_total", "any",
  "mattress_yn", "mattress", "all",
  "chair_bench_yn", "chair_bench", "all",
  "portable_lamp_yn", "portable_lamp", "all",
  "solar_lamp_yn", "solar_lamp", "all",
  "mobile_phone_yn", "mobile_phone", "all",
  "smartphone_yn", "smartphone", "all",
  "electric_fan_yn", "electric_fan", "all",
  "shovel_yn", "shovel", "all",
  "sickle_yn", "sickle", "all",
  "weaving_tool_yn", "weaving_tool", "all",
  "chicken_duck_pigeon_yn", "chicken_duck_pigeon", "all",
  "electricity", "electricity", "all"
)

source_field_availability <- source_field_map %>%
  dplyr::mutate(
    source_field_list = stringr::str_split(.data$source_fields, ";\\s*"),
    n_source_required = lengths(.data$source_field_list),
    n_source_present = purrr::map_int(
      .data$source_field_list,
      ~sum(.x %in% names(survey_data_raw))
    ),
    missing_source_fields = purrr::map_chr(
      .data$source_field_list,
      ~paste(setdiff(.x, names(survey_data_raw)), collapse = "; ")
    ),
    source_fields_present = dplyr::case_when(
      .data$source_rule == "any" ~ .data$n_source_present >= 1L,
      TRUE ~ .data$n_source_present == .data$n_source_required
    )
  ) %>%
  dplyr::select(-"source_field_list")

missing_table_vars <- table_rows %>%
  dplyr::left_join(source_field_availability, by = "variable") %>%
  dplyr::mutate(
    derived_variable_available = .data$variable %in% names(baseline_data),
    status = dplyr::case_when(
      !.data$derived_variable_available ~ "derived_variable_missing",
      !.data$source_fields_present ~ "source_field_missing_in_clean_final",
      TRUE ~ "available"
    )
  )

write_qa_csv(missing_table_vars, "rf105b_table1_variable_availability.csv")

group_defs <- tibble::tribble(
  ~arm, ~group, ~flag_var, ~excel_col,
  "comparison", "comparison_g1", "group_g1", "B",
  "comparison", "comparison_g1a", "group_g1a", "C",
  "comparison", "comparison_g2", "group_g2", "D",
  "comparison", "comparison_g2a", "group_g2a", "E",
  "comparison", "comparison_g3a", "group_g3a", "F",
  "comparison", "comparison_g4", "group_g4", "G",
  "intervention", "intervention_g1", "group_g1", "K",
  "intervention", "intervention_g1a", "group_g1a", "L",
  "intervention", "intervention_g2", "group_g2", "M",
  "intervention", "intervention_g2a", "group_g2a", "N",
  "intervention", "intervention_g3a", "group_g3a", "O",
  "intervention", "intervention_g4", "group_g4", "P"
)

table_long <- purrr::pmap_dfr(group_defs, function(arm, group, flag_var, excel_col) {
  baseline_data %>%
    dplyr::filter(.data$study_arm_overall == arm, .data[[flag_var]]) %>%
    dplyr::mutate(group = group)
})

group_counts <- table_long %>%
  dplyr::distinct(.data$group, .data$fcn_id) %>%
  dplyr::count(.data$group, name = "n") %>%
  dplyr::right_join(group_defs %>% dplyr::select("arm", "group", "excel_col"),
                    by = "group") %>%
  dplyr::mutate(n = dplyr::coalesce(.data$n, 0L)) %>%
  dplyr::arrange(factor(.data$excel_col, levels = c("B", "C", "D", "E", "F", "G",
                                                    "K", "L", "M", "N", "O", "P")))

expected_counts <- tibble::tribble(
  ~group, ~expected_n,
  "comparison_g1", 598L,
  "comparison_g1a", 22L,
  "comparison_g2", 575L,
  "comparison_g2a", 130L,
  "comparison_g3a", 152L,
  "comparison_g4", 445L,
  "intervention_g1", 594L,
  "intervention_g1a", 34L,
  "intervention_g2", 558L,
  "intervention_g2a", 64L,
  "intervention_g3a", 98L,
  "intervention_g4", 494L
)

group_count_check <- group_counts %>%
  dplyr::left_join(expected_counts, by = "group") %>%
  dplyr::mutate(
    matches_expected = .data$n == .data$expected_n,
    note = dplyr::if_else(
      .data$matches_expected,
      "matches expected count from planning check",
      "review count difference before using table"
    )
  )

write_qa_csv(group_count_check, "rf105b_table1_group_counts.csv")

if (any(!group_count_check$matches_expected)) {
  warning("One or more RF105B Table 1 group counts do not match expected counts.")
}

participation_patterns <- baseline_data %>%
  dplyr::count(.data$study_arm_overall, .data$participation_pattern, name = "n") %>%
  dplyr::arrange(.data$study_arm_overall, .data$participation_pattern)

write_qa_csv(participation_patterns, "rf105b_table1_participation_patterns.csv")

summary_wide <- make_wide_summary(
  table_long,
  table_rows,
  group_defs$group
)

p_comparison_g1a_g2 <- make_pairwise_p(
  table_long %>% dplyr::filter(stringr::str_starts(.data$group, "comparison_")),
  table_rows,
  "comparison_g1a",
  "comparison_g2"
) %>%
  dplyr::rename(p_comparison_g1a_vs_g2 = p_value)

p_comparison_g3a_g4 <- make_pairwise_p(
  table_long %>% dplyr::filter(stringr::str_starts(.data$group, "comparison_")),
  table_rows,
  "comparison_g3a",
  "comparison_g4"
) %>%
  dplyr::rename(p_comparison_g3a_vs_g4 = p_value)

p_intervention_g1a_g2 <- make_pairwise_p(
  table_long %>% dplyr::filter(stringr::str_starts(.data$group, "intervention_")),
  table_rows,
  "intervention_g1a",
  "intervention_g2"
) %>%
  dplyr::rename(p_intervention_g1a_vs_g2 = p_value)

p_intervention_g3a_g4 <- make_pairwise_p(
  table_long %>% dplyr::filter(stringr::str_starts(.data$group, "intervention_")),
  table_rows,
  "intervention_g3a",
  "intervention_g4"
) %>%
  dplyr::rename(p_intervention_g3a_vs_g4 = p_value)

p_g2_comparison_intervention <- make_pairwise_p(
  table_long %>% dplyr::filter(.data$group %in% c("comparison_g2", "intervention_g2")),
  table_rows,
  "comparison_g2",
  "intervention_g2"
) %>%
  dplyr::rename(p_g2_comparison_vs_intervention = p_value)

p_g4_comparison_intervention <- make_pairwise_p(
  table_long %>% dplyr::filter(.data$group %in% c("comparison_g4", "intervention_g4")),
  table_rows,
  "comparison_g4",
  "intervention_g4"
) %>%
  dplyr::rename(p_g4_comparison_vs_intervention = p_value)

p_value_details <- dplyr::bind_rows(
  make_pairwise_p_detail(
    table_long %>% dplyr::filter(stringr::str_starts(.data$group, "comparison_")),
    table_rows,
    "comparison_g1a",
    "comparison_g2",
    "comparison_g1a_vs_g2"
  ),
  make_pairwise_p_detail(
    table_long %>% dplyr::filter(stringr::str_starts(.data$group, "comparison_")),
    table_rows,
    "comparison_g3a",
    "comparison_g4",
    "comparison_g3a_vs_g4"
  ),
  make_pairwise_p_detail(
    table_long %>% dplyr::filter(stringr::str_starts(.data$group, "intervention_")),
    table_rows,
    "intervention_g1a",
    "intervention_g2",
    "intervention_g1a_vs_g2"
  ),
  make_pairwise_p_detail(
    table_long %>% dplyr::filter(stringr::str_starts(.data$group, "intervention_")),
    table_rows,
    "intervention_g3a",
    "intervention_g4",
    "intervention_g3a_vs_g4"
  ),
  make_pairwise_p_detail(
    table_long %>% dplyr::filter(.data$group %in% c("comparison_g2", "intervention_g2")),
    table_rows,
    "comparison_g2",
    "intervention_g2",
    "g2_comparison_vs_intervention"
  ),
  make_pairwise_p_detail(
    table_long %>% dplyr::filter(.data$group %in% c("comparison_g4", "intervention_g4")),
    table_rows,
    "comparison_g4",
    "intervention_g4",
    "g4_comparison_vs_intervention"
  )
)

write_qa_csv(p_value_details, "rf105b_table1_exact_test_statistics.csv")

results_for_qa <- summary_wide %>%
  dplyr::left_join(p_comparison_g1a_g2, by = c("row", "variable")) %>%
  dplyr::left_join(p_comparison_g3a_g4, by = c("row", "variable")) %>%
  dplyr::left_join(p_intervention_g1a_g2, by = c("row", "variable")) %>%
  dplyr::left_join(p_intervention_g3a_g4, by = c("row", "variable")) %>%
  dplyr::left_join(p_g2_comparison_intervention, by = c("row", "variable")) %>%
  dplyr::left_join(p_g4_comparison_intervention, by = c("row", "variable"))

write_qa_csv(results_for_qa, "rf105b_table1_values_and_p_values.csv")

wb <- openxlsx::loadWorkbook(template_file)
sheet_name <- "for paper"

if (sheet_name %notin% names(wb)) {
  stop("Expected sheet not found in workbook: ", sheet_name)
}

write_cell <- function(col, row, value) {
  openxlsx::writeData(
    wb,
    sheet = sheet_name,
    x = value,
    startCol = match(col, LETTERS),
    startRow = row,
    colNames = FALSE,
    rowNames = FALSE
  )
}

# Clear the previous table body and note area before writing the shortened table.
openxlsx::writeData(
  wb,
  sheet = sheet_name,
  x = matrix("", nrow = 23L, ncol = 21L),
  startCol = 1L,
  startRow = 5L,
  colNames = FALSE,
  rowNames = FALSE
)

for (i in seq_len(nrow(group_count_check))) {
  write_cell(group_count_check$excel_col[[i]], 4L, paste0("n = ", group_count_check$n[[i]]))
}

value_cols <- c(
  comparison_g1 = "B",
  comparison_g1a = "C",
  comparison_g2 = "D",
  comparison_g2a = "E",
  comparison_g3a = "F",
  comparison_g4 = "G",
  intervention_g1 = "K",
  intervention_g1a = "L",
  intervention_g2 = "M",
  intervention_g2a = "N",
  intervention_g3a = "O",
  intervention_g4 = "P"
)

p_cols <- c(
  p_comparison_g1a_vs_g2 = "H",
  p_comparison_g3a_vs_g4 = "I",
  p_intervention_g1a_vs_g2 = "Q",
  p_intervention_g3a_vs_g4 = "R",
  p_g2_comparison_vs_intervention = "T",
  p_g4_comparison_vs_intervention = "U"
)

for (i in seq_len(nrow(results_for_qa))) {
  row_num <- results_for_qa$row[[i]]
  write_cell("A", row_num, results_for_qa$label[[i]])

  for (col_name in names(value_cols)) {
    write_cell(value_cols[[col_name]], row_num, results_for_qa[[col_name]][[i]])
  }

  for (col_name in names(p_cols)) {
    write_cell(p_cols[[col_name]], row_num, results_for_qa[[col_name]][[i]])
  }
}

note_text <- paste(
  "Note: Midline data do not include three baseline households that were included at endline",
  "(1 comparison, 2 intervention); these households are counted in Group 1 but not Group 2 or Group 4."
)
note_row <- max(table_rows$row) + 2L

openxlsx::writeData(
  wb,
  sheet = sheet_name,
  x = note_text,
  startCol = 1L,
  startRow = note_row,
  colNames = FALSE,
  rowNames = FALSE
)
openxlsx::mergeCells(wb, sheet = sheet_name, cols = 1:21, rows = note_row)
openxlsx::addStyle(
  wb,
  sheet = sheet_name,
  style = openxlsx::createStyle(
    textDecoration = "italic",
    wrapText = TRUE,
    valign = "top"
  ),
  rows = note_row,
  cols = 1:21,
  gridExpand = TRUE,
  stack = TRUE
)
openxlsx::setRowHeights(wb, sheet = sheet_name, rows = note_row, heights = 36)

openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)

message("Wrote filled workbook: ", output_file)
message("RF105B Table 1 workbook fill complete.")
