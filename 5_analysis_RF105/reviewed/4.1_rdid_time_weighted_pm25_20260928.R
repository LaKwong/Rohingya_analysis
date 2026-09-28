################################################################################
# RF105 baseline-to-midline rDiD for time-weighted PM2.5 exposure
#
# Purpose:
#   Estimate baseline-to-midline rDiD effects on household-level time-weighted
#   PM2.5 exposure for women caregivers and target children. Exposure combines
#   household-, timepoint-, arm-, and population-specific time use with matched
#   indoor and outdoor PM2.5 concentrations.
#
# Inputs:
#   4_data/clean_final/survey_refugee_household.rds
#   8_restricted/RF105_reviewed_YYYYMMDD/identified_tables/
#     table_descriptive_pm25_time_weighted_exposure_internal.csv
#
# Outputs:
#   7_tables/RF105_reviewed_YYYYMMDD/
#     table_rDiD_pm25_time_weighted_exposure_baseline_midline.csv
#   7_tables/RF105_reviewed_YYYYMMDD/qa/
#     table_rDiD_pm25_time_weighted_exposure_panel_counts.csv
################################################################################

get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 1L) {
    return(dirname(normalizePath(
      sub("^--file=", "", file_arg), winslash = "/", mustWork = FALSE
    )))
  }
  getwd()
}

script_dir <- get_script_dir()
config_file <- file.path(script_dir, "0_RF105_config_20260805_2213.R")
if (!file.exists(config_file)) {
  config_file <- file.path(
    "5_analysis_RF105", "reviewed", "0_RF105_config_20260805_2213.R"
  )
}
source(config_file)

if (!requireNamespace("xgboost", quietly = TRUE)) {
  stop(
    "Package xgboost is required. Run renv::restore() and rerun this script.",
    call. = FALSE
  )
}

first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) NA else x[[1L]]
}

as_number <- function(x) suppressWarnings(as.numeric(as.character(x)))

xvars <- c("hh_size", "hh_per_structure")
min_rdid_arm_households <- as.integer(Sys.getenv(
  "RF105_RDID_MIN_ARM_HOUSEHOLDS", unset = "25"
))

household_file <- file.path(dir_clean_final, "survey_refugee_household.rds")
exposure_file <- file.path(
  dir_restricted_reviewed,
  "identified_tables",
  "table_descriptive_pm25_time_weighted_exposure_internal.csv"
)
pm_household_file <- file.path(
  dir_restricted_reviewed,
  "identified_tables",
  "table_descriptive_pm25_household_timepoint_internal.csv"
)

if (!file.exists(household_file)) {
  stop("Missing household input: ", household_file, call. = FALSE)
}
if (!file.exists(exposure_file)) {
  stop(
    "Missing time-weighted exposure input: ", exposure_file,
    ". Run 3_descriptive_outcomes_20260805_2213.R first.",
    call. = FALSE
  )
}
if (!file.exists(pm_household_file)) {
  stop(
    "Missing household PM2.5 input: ", pm_household_file,
    ". Run 3_descriptive_outcomes_20260805_2213.R first.",
    call. = FALSE
  )
}

survey_household <- readRDS(household_file)
exposure_internal <- readr::read_csv(exposure_file, show_col_types = FALSE)
pm_household_internal <- readr::read_csv(
  pm_household_file,
  show_col_types = FALSE
)

required_household_columns <- c(
  "fcn_id", "timepoint", "study_arm_overall", "hh_size", "hh_per_structure"
)
required_exposure_columns <- c(
  "fcn_id", "timepoint", "study_arm_overall", "population",
  "hours_inside_est", "time_weighted_average_pm25_ug_m3"
)
required_pm_columns <- c(
  "fcn_id", "timepoint", "study_arm_overall", "indoor_pm25_mean",
  "ambient_pm25_mean"
)

if (length(setdiff(required_household_columns, names(survey_household))) > 0L) {
  stop("Household input is missing required rDiD columns.", call. = FALSE)
}
if (length(setdiff(required_exposure_columns, names(exposure_internal))) > 0L) {
  stop("Exposure input is missing required rDiD columns.", call. = FALSE)
}
if (length(setdiff(required_pm_columns, names(pm_household_internal))) > 0L) {
  stop("Household PM2.5 input is missing required rDiD columns.", call. = FALSE)
}

baseline_covars <- survey_household %>%
  filter(as.character(timepoint) == "baseline") %>%
  transmute(
    fcn_id = as.character(fcn_id),
    A = if_else(
      as.character(study_arm_overall) == "intervention", 1,
      if_else(as.character(study_arm_overall) == "comparison", 0, NA_real_)
    ),
    hh_size = as_number(hh_size),
    hh_per_structure = as_number(hh_per_structure)
  ) %>%
  group_by(fcn_id) %>%
  summarise(
    A = as_number(first_nonmissing(A)),
    across(all_of(xvars), ~ as_number(first_nonmissing(.x))),
    .groups = "drop"
  )

caregiver_exposure <- exposure_internal %>%
  transmute(
    fcn_id = as.character(fcn_id),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    population = as.character(population),
    exposure = as_number(time_weighted_average_pm25_ug_m3),
    time_use_imputed = FALSE
  ) %>%
  filter(
    timepoint %in% c("baseline", "midline"),
    study_arm_overall %in% arm_levels,
    population == "caregiver",
    !is.na(fcn_id), fcn_id != "",
    is.finite(exposure)
  )

target_child_time_cell_means <- exposure_internal %>%
  transmute(
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    population = as.character(population),
    hours_inside_observed = as_number(hours_inside_est)
  ) %>%
  filter(
    timepoint %in% c("baseline", "midline"),
    study_arm_overall %in% arm_levels,
    population == "target_child",
    is.finite(hours_inside_observed)
  ) %>%
  group_by(timepoint, study_arm_overall) %>%
  summarise(
    mean_hours_inside_cell = mean(hours_inside_observed),
    n_children_with_observed_time = n(),
    .groups = "drop"
  )

if (nrow(target_child_time_cell_means) != 4L) {
  stop(
    "Target-child time-use means are not available for all baseline/midline arm cells.",
    call. = FALSE
  )
}

target_child_observed_time <- exposure_internal %>%
  transmute(
    fcn_id = as.character(fcn_id),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    population = as.character(population),
    hours_inside_observed = as_number(hours_inside_est)
  ) %>%
  filter(
    timepoint %in% c("baseline", "midline"),
    study_arm_overall %in% arm_levels,
    population == "target_child",
    !is.na(fcn_id), fcn_id != "",
    is.finite(hours_inside_observed)
  ) %>%
  select(-population)

target_child_exposure <- pm_household_internal %>%
  transmute(
    fcn_id = as.character(fcn_id),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall),
    indoor_pm25 = as_number(indoor_pm25_mean),
    outdoor_pm25 = as_number(ambient_pm25_mean)
  ) %>%
  filter(
    timepoint %in% c("baseline", "midline"),
    study_arm_overall %in% arm_levels,
    !is.na(fcn_id), fcn_id != "",
    is.finite(indoor_pm25), is.finite(outdoor_pm25)
  ) %>%
  left_join(
    target_child_observed_time,
    by = c("fcn_id", "timepoint", "study_arm_overall")
  ) %>%
  left_join(
    target_child_time_cell_means,
    by = c("timepoint", "study_arm_overall")
  ) %>%
  mutate(
    time_use_imputed = !is.finite(hours_inside_observed),
    hours_inside_used = if_else(
      time_use_imputed,
      mean_hours_inside_cell,
      hours_inside_observed
    ),
    hours_outside_used = 24 - hours_inside_used,
    exposure =
      (hours_inside_used / 24) * indoor_pm25 +
      (hours_outside_used / 24) * outdoor_pm25,
    population = "target_child"
  ) %>%
  select(
    fcn_id, timepoint, study_arm_overall, population, exposure,
    time_use_imputed
  )

exposure_analysis <- bind_rows(caregiver_exposure, target_child_exposure)

duplicate_exposure_keys <- exposure_analysis %>%
  count(fcn_id, timepoint, population, name = "n_rows") %>%
  filter(n_rows != 1L)

if (nrow(duplicate_exposure_keys) > 0L) {
  stop(
    "Time-weighted exposure must contain one row per household, timepoint, and population.",
    call. = FALSE
  )
}

make_panel <- function(population_name) {
  exposure_analysis %>%
    filter(population == population_name) %>%
    select(fcn_id, timepoint, exposure, time_use_imputed) %>%
    tidyr::pivot_wider(
      names_from = timepoint,
      values_from = c(exposure, time_use_imputed),
      names_sep = "_"
    ) %>%
    transmute(
      fcn_id,
      Z = exposure_baseline,
      Y = exposure_midline,
      baseline_time_imputed = time_use_imputed_baseline,
      midline_time_imputed = time_use_imputed_midline
    ) %>%
    inner_join(baseline_covars, by = "fcn_id") %>%
    filter(!is.na(A), is.finite(Z), is.finite(Y))
}

xgb_xfit <- function(X_tr, y_tr, X_te, objective,
                     depths = as.numeric(strsplit(
                       Sys.getenv("RF105_XGB_DEPTHS", unset = "2"), ","
                     )[[1]]),
                     etas = as.numeric(strsplit(
                       Sys.getenv("RF105_XGB_ETAS", unset = "0.05"), ","
                     )[[1]]),
                     max_nrounds = as.integer(Sys.getenv(
                       "RF105_XGB_MAX_NROUNDS", unset = "100"
                     )),
                     early_stopping_rounds = as.integer(Sys.getenv(
                       "RF105_XGB_EARLY_STOP", unset = "10"
                     )),
                     seed = 1) {
  keep <- !is.na(y_tr)
  X_tr <- X_tr[keep, , drop = FALSE]
  y_tr <- y_tr[keep]

  if (nrow(X_tr) < 5L) {
    return(rep(mean(y_tr, na.rm = TRUE), nrow(X_te)))
  }
  if (objective == "binary:logistic" && length(unique(y_tr)) < 2L) {
    return(rep(mean(y_tr, na.rm = TRUE), nrow(X_te)))
  }

  dtrain <- xgboost::xgb.DMatrix(X_tr, label = y_tr, missing = NA)
  metric <- if (objective == "reg:squarederror") "rmse" else "logloss"
  log_col <- paste0("test_", metric, "_mean")
  cv_nfold <- min(3L, nrow(X_tr))
  best <- list(score = Inf, pars = NULL, nrounds = NULL)

  for (max_depth in depths) {
    for (eta in etas) {
      pars <- list(
        objective = objective,
        max_depth = max_depth,
        eta = eta,
        nthread = as.integer(Sys.getenv("RF105_XGB_NTHREAD", unset = "1")),
        verbosity = 0
      )

      set.seed(seed)
      cv <- tryCatch(
        xgboost::xgb.cv(
          params = pars,
          data = dtrain,
          nrounds = max_nrounds,
          nfold = cv_nfold,
          early_stopping_rounds = early_stopping_rounds,
          verbose = 0,
          metrics = metric
        ),
        error = function(e) NULL
      )

      if (is.null(cv) || !(log_col %in% names(cv$evaluation_log))) next
      best_iteration <- which.min(cv$evaluation_log[[log_col]])
      score <- cv$evaluation_log[[log_col]][best_iteration]
      if (!is.na(score) && score < best$score) {
        best <- list(score = score, pars = pars, nrounds = best_iteration)
      }
    }
  }

  if (is.null(best$pars) || is.null(best$nrounds)) {
    return(rep(mean(y_tr, na.rm = TRUE), nrow(X_te)))
  }

  model <- xgboost::xgb.train(
    params = best$pars,
    data = dtrain,
    nrounds = best$nrounds,
    verbose = 0
  )
  predict(model, xgboost::xgb.DMatrix(X_te, missing = NA))
}

dml_drdid_reverse_xgb <- function(dat, K = 5L, seed = 1L) {
  dat <- dat %>% filter(!is.na(Z), !is.na(Y), !is.na(A))
  n <- nrow(dat)
  n_intervention <- sum(dat$A == 1, na.rm = TRUE)
  n_comparison <- sum(dat$A == 0, na.rm = TRUE)

  if (n_intervention < min_rdid_arm_households ||
      n_comparison < min_rdid_arm_households) {
    return(list(
      estimate = NA_real_, se = NA_real_, conf_low = NA_real_,
      conf_high = NA_real_, p_value = NA_real_, n = n,
      n_intervention = n_intervention, n_comparison = n_comparison,
      status = "not_estimable_below_prespecified_minimum",
      note = paste0(
        "Not estimated because the paired panel was below the prespecified ",
        "minimum of ", min_rdid_arm_households, " households per arm."
      )
    ))
  }

  X <- data.matrix(dat[, xvars, drop = FALSE])
  A <- as_number(dat$A)
  D <- as_number(dat$Y - dat$Z)
  K_eff <- min(K, n_intervention, n_comparison)
  folds <- integer(n)

  set.seed(seed)
  for (arm_value in c(0, 1)) {
    arm_index <- which(A == arm_value)
    folds[arm_index] <- sample(rep(seq_len(K_eff), length.out = length(arm_index)))
  }

  m1_hat <- numeric(n)
  p_hat <- numeric(n)
  for (fold in seq_len(K_eff)) {
    test_index <- which(folds == fold)
    train_index <- which(folds != fold)
    intervention_train <- train_index[A[train_index] == 1]

    m1_hat[test_index] <- xgb_xfit(
      X[intervention_train, , drop = FALSE],
      D[intervention_train],
      X[test_index, , drop = FALSE],
      "reg:squarederror",
      seed = seed + fold
    )
    p_hat[test_index] <- xgb_xfit(
      X[train_index, , drop = FALSE],
      A[train_index],
      X[test_index, , drop = FALSE],
      "binary:logistic",
      seed = seed + fold
    )
  }

  p_hat <- pmin(pmax(p_hat, 0.01), 0.99)
  pi0 <- mean(1 - A)
  psi_vector <- (A - p_hat) / p_hat * (D - m1_hat) / pi0
  estimate <- mean(psi_vector)
  influence_function <- psi_vector - ((1 - A) / pi0) * estimate
  se <- stats::sd(influence_function) / sqrt(n)
  confidence_interval <- estimate + c(-1, 1) * stats::qnorm(0.975) * se
  p_value <- ifelse(se == 0, NA_real_, 2 * stats::pnorm(-abs(estimate / se)))

  list(
    estimate = estimate,
    se = se,
    conf_low = confidence_interval[[1L]],
    conf_high = confidence_interval[[2L]],
    p_value = p_value,
    n = n,
    n_intervention = n_intervention,
    n_comparison = n_comparison,
    status = "estimated",
    note = paste(
      "rDiD DML-DR estimator with five-fold cross-fit XGBoost nuisance models;",
      "baseline covariates were household size and households per structure."
    )
  )
}

population_labels <- c(
  caregiver = "Women caregivers",
  target_child = "Target children"
)

panels <- lapply(names(population_labels), make_panel)
names(panels) <- names(population_labels)

panel_counts <- purrr::imap_dfr(panels, function(panel, population_name) {
  purrr::map_dfr(c(0, 1), function(arm_value) {
    arm_panel <- panel %>% filter(A == arm_value)
    arm_name <- if_else(arm_value == 1, "intervention", "comparison")
    baseline_imputation_value <- target_child_time_cell_means %>%
      filter(
        timepoint == "baseline",
        study_arm_overall == arm_name
      ) %>%
      pull(mean_hours_inside_cell)
    midline_imputation_value <- target_child_time_cell_means %>%
      filter(
        timepoint == "midline",
        study_arm_overall == arm_name
      ) %>%
      pull(mean_hours_inside_cell)

    tibble(
      population = population_name,
      population_label = unname(population_labels[[population_name]]),
      study_arm = arm_name,
      n_paired_households = nrow(arm_panel),
      n_baseline_time_imputed =
        sum(arm_panel$baseline_time_imputed %in% TRUE),
      n_midline_time_imputed =
        sum(arm_panel$midline_time_imputed %in% TRUE),
      baseline_imputation_value_hours_inside = if_else(
        population_name == "target_child",
        baseline_imputation_value,
        NA_real_
      ),
      midline_imputation_value_hours_inside = if_else(
        population_name == "target_child",
        midline_imputation_value,
        NA_real_
      ),
      prespecified_minimum_per_arm = min_rdid_arm_households,
      passes_minimum = n_paired_households >= prespecified_minimum_per_arm
    )
  })
})

results <- purrr::imap_dfr(panels, function(panel, population_name) {
  result <- dml_drdid_reverse_xgb(panel)
  tibble(
    contrast = "primary_baseline_midline",
    population = population_name,
    population_label = unname(population_labels[[population_name]]),
    estimator = "rDID_XGBoost",
    outcome = "time_weighted_average_pm25_ug_m3",
    unit = "ug/m3",
    estimate = result$estimate,
    se = result$se,
    conf_low = result$conf_low,
    conf_high = result$conf_high,
    p_value = result$p_value,
    sample_size = result$n,
    n_intervention = result$n_intervention,
    n_comparison = result$n_comparison,
    prespecified_minimum_per_arm = min_rdid_arm_households,
    estimability_status = result$status,
    missing_child_time_handling = if_else(
      population_name == "target_child",
      paste(
        "Observed target-child time was used when available; missing time was",
        "replaced by the corresponding timepoint- and arm-specific mean."
      ),
      "No caregiver time-use imputation was applied."
    ),
    estimate_direction = paste(
      "Negative values indicate a greater baseline-to-midline reduction",
      "in the intervention arm than in the comparison arm."
    ),
    note = result$note
  )
})

readr::write_csv(
  results,
  file.path(
    dir_tables_reviewed,
    "table_rDiD_pm25_time_weighted_exposure_baseline_midline.csv"
  ),
  na = ""
)
readr::write_csv(
  panel_counts,
  file.path(
    dir_tables_qa,
    "table_rDiD_pm25_time_weighted_exposure_panel_counts.csv"
  ),
  na = ""
)

message("Completed time-weighted PM2.5 rDiD analysis.")
