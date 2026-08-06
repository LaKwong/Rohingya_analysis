## XGBOOST FUNCTION 
library(xgboost)
library(tidyverse)

### Purpose 
# This file is the analysis code for RF105B revisions created after consultation with alejandro. It is based on code he assembled 

#1) In paper we report the kim and lee analysis (Part 4 of newest write-up: did_drdid_pre) from baseline to  endline  using data of people that responded to all three timepoints 

#In the supplement, we report 

#2) Kim / Lee from baseline to midline (using all three timepoint dataset)  (OR DO WE USE ENDLINE AS PRIMARY VAR)? (Part 4 of Code)
#2a Optional if Layla requires we also include one using those that responsed to baseline and midline survey data (but were dropped by endline?)

#3) Kim and Lee using GLM 


#4) The orig xgboost results baseline - mid and endline (Part 3 of code) 

#5) The pre-period ATE (Part 5 of code-making sure to change dataset structure as there are no more Yvars and xvar now called wvar) 



### SECTION 0: Data set up 


#Input Data 
file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds") ## Hh that responded to all three timepoints
file_survey_base_mid <- file_out_base_mid <- here::here("4_data/RohingyaFuel_survey_data_baseline_midline.rds") ## Hh that responded to baseline and midline timepoints
fcs_data <- here::here("4_data/RohingyaFuel_survey_data_fcs.rds")
CES_D_data <- here::here("4_data/RohingyaFuel_survey_data_CES_D.rds")

#Output data 
fcs_output <- read_rds(fcs_data) 
CES_D_output <- read_rds(CES_D_data)

BDT_USD_exchange_rate_baseline <- 84.88 # as of 2 Nov 2020
BDT_USD_exchange_rate_midline <- 84.74 #as of 21 August 2021
BDT_USD_exchange_rate_endline <- 93.45 #as of 4 July 2022



#Load Survey 
survey_data <- read_rds(file_survey_data_base) %>% 
  left_join(CES_D_output, by = c("fcn_id", "timepoint", "study_arm_overall")) %>%
  left_join(fcs_output, by = c("fcn_id", "timepoint", "study_arm_overall")) %>% 
  mutate(
    respondent_disturbed_speech_yn = if_else(is.na(respondent_disturbed_speech), 0, respondent_disturbed_speech),
    target_child_distrubed_speech_yn = if_else(is.na(target_child_distrubed_speech), 0, target_child_distrubed_speech), 
    spent_food = case_when(
      timepoint == "baseline" ~ spent_food/ BDT_USD_exchange_rate_baseline,
      timepoint == "midline" ~ spent_food/ BDT_USD_exchange_rate_midline,
      timepoint == "endline" ~ spent_food/ BDT_USD_exchange_rate_endline
    ),
    buy_wood_cost = case_when(
      timepoint == "baseline" ~ buy_wood_cost/ BDT_USD_exchange_rate_baseline,
      timepoint == "midline" ~ buy_wood_cost/ BDT_USD_exchange_rate_midline,
      timepoint == "endline" ~ buy_wood_cost/ BDT_USD_exchange_rate_endline
    ),
    target_child_alri = if_else(target_child_clinic_resp_yn == 1 | target_child_resp_rate_yn == 1, 1, 0), 
    target_child_asthma = if_else(target_child_wheezing_yn == 1, 1, 0), 
    target_child_severe_asthma = if_else(target_child_wheezing_yn == 1 & target_child_distrubed_speech_yn == 1, 1, 0)
  )

survey_data_base_mid <- read_rds(file_survey_base_mid) %>% 
  left_join(CES_D_output, by = c("fcn_id", "timepoint", "study_arm_overall")) %>%
  left_join(fcs_output, by = c("fcn_id", "timepoint", "study_arm_overall")) %>% 
  mutate(
    respondent_disturbed_speech_yn = if_else(is.na(respondent_disturbed_speech), 0, respondent_disturbed_speech),
    target_child_distrubed_speech_yn = if_else(is.na(target_child_distrubed_speech), 0, target_child_distrubed_speech), 
    spent_food = case_when(
      timepoint == "baseline" ~ spent_food/ BDT_USD_exchange_rate_baseline,
      timepoint == "midline" ~ spent_food/ BDT_USD_exchange_rate_midline,
      timepoint == "endline" ~ spent_food/ BDT_USD_exchange_rate_endline
    ),
    buy_wood_cost = case_when(
      timepoint == "baseline" ~ buy_wood_cost/ BDT_USD_exchange_rate_baseline,
      timepoint == "midline" ~ buy_wood_cost/ BDT_USD_exchange_rate_midline,
      timepoint == "endline" ~ buy_wood_cost/ BDT_USD_exchange_rate_endline
    ),
    target_child_alri = if_else(target_child_clinic_resp_yn == 1 | target_child_resp_rate_yn == 1, 1, 0), 
    target_child_asthma = if_else(target_child_wheezing_yn == 1, 1, 0), 
    target_child_severe_asthma = if_else(target_child_wheezing_yn == 1 & target_child_distrubed_speech_yn == 1, 1, 0)
  )


# Household member individual data (The ed and age vars were collected in separate data from bc of way df was set up)
data_hh_member_filename <- here::here("4_data/RohingyaFuel_data_hh_member.rds")
data_hh_member_in <- read_rds(data_hh_member_filename)


## outcome variables to analyze
outcomes <- c(
  "target_child_eye_itch_yn",
  "target_child_eye_red_yn",
  "fcs",
  "fcs_binary", 
  "target_child_clinic_resp_yn", 
  "target_child_wheezing_yn",
  "target_child_distrubed_speech_yn",
  "respondent_disturbed_speech_yn", 
  "target_child_eye_red_yn",
  "target_child_eye_itch_yn", 
  "respondent_eye_red_yn",
  "respondent_eye_itch_yn",
  "respondent_wheezing_yn", 
  "respondent_eye_sore_yn",
  "target_child_resp_rate", 
  "respondent_eye_sore_yn",
  "target_child_cough",
  "target_child_fever",
  "target_child_resp_rate",
  "spent_food",
  "buy_wood_cost",
  "CES_D_o16_score", 
  "suicidal_thoughts_30_yn",
  "target_child_fever", 
  "target_child_alri",
  "target_child_asthma",
  "target_child_severe_asthma"
)

xvars <- c(
  "hh_size",
  "hh_per_structure",
  "age_yrs",
  "edu_yrs"
)



vars_to_keep <- c(
  "fcn_id", "hh_per_structure", "hh_size",
  "timepoint", "study_arm_overall_num",
  "age_yrs", "edu_yrs",
  outcomes
)

#Merge hh member data with survey data to get the age and education variables for the target child
data_hh_member <- data_hh_member_in %>%
  filter(hhh_relation == 1) %>%
  select(PARENT_KEY, disability, age_yrs, edu_yrs) 

survey_data_merge <- survey_data %>%
  left_join(data_hh_member, by = c("KEY" = "PARENT_KEY"))


hh_data <-  survey_data_merge %>%
  select(all_of(vars_to_keep)) %>%
  mutate(
    A = if_else(study_arm_overall_num == 1, 1, 0),
    #target_child_clinic_resp = if_else(target_child_clinic_resp > 0, 1, 0), 
    #  target_child_eye_itch = if_else(target_child_eye_itch > 0, 1, 0)
  )

### merge base_mid files 
survey_data_merge_base_mid <- survey_data_base_mid %>%
  left_join(data_hh_member, by = c("KEY" = "PARENT_KEY"))

hh_data_base_mid <-  survey_data_merge_base_mid %>%
  select(all_of(vars_to_keep)) %>%
  mutate(
    A = if_else(study_arm_overall_num == 1, 1, 0),
    #target_child_clinic_resp = if_else(target_child_clinic_resp > 0, 1, 0), 
    #  target_child_eye_itch = if_else(target_child_eye_itch > 0, 1, 0)
  )


make_dml_panel <- function(data, outcome_var) {
  
  outcome_sym <- rlang::sym(outcome_var)
  
  # 1. Household-level covariates
  hh_covars <- data %>%
    group_by(fcn_id) %>%
    summarise(
      A                = first(na.omit(A)),
      hh_size          = first(na.omit(hh_size)),
      hh_per_structure = first(na.omit(hh_per_structure)),
      age_yrs          = first(na.omit(age_yrs)),
      edu_yrs          = first(na.omit(edu_yrs)),
      .groups = "drop"
    )
  
  # 2. Collapse duplicate household-wave rows
  outcomes_long <- data %>%
    group_by(fcn_id, timepoint) %>%
    summarise(
      outcome =
        if(all(is.na(!!outcome_sym))) NA_real_
      else mean(!!outcome_sym, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(timepoint = tolower(timepoint)) %>%
    complete(
      fcn_id,
      timepoint = c("baseline", "midline", "endline")
    )
  
  # 3. Convert to wide format
  outcomes_wide <- outcomes_long %>%
    pivot_wider(
      id_cols = fcn_id,
      names_from = timepoint,
      values_from = outcome
    ) %>%
    rename(
      Z = baseline,
      Y1 = midline,
      Y2 = endline
    )
  
  # 4. Merge with covariates
  hh_covars %>%
    left_join(outcomes_wide, by = "fcn_id")
  
}





### Sections 1- 3: Kim and Lee based DID-PRE


# DML-DR pre-period ATT (difference-in-differences in reverse): the
# comparator-group effect on Z. Same data format as dml_drdid; returns
# psi_pre = E[Z(-1) - Z(inf) | T = -1].


dml_drdid_pre_mid <- dml_drdid_pre_mid <- function(dat, x_vars, outcome_name = NULL,
                                                   K = 5, seed = 1) {
  
  dat <- dat %>%
    filter(!is.na(Z), !is.na(Y1))
  
  set.seed(seed)
  X  <- data.matrix(dat[, x_vars, drop = FALSE])
  A  <- as.numeric(dat$A)
  D  <- as.numeric(dat$Y1 - dat$Z)            # within-household change Δ = Y - Z
  n  <- nrow(dat)
  folds <- sample(rep(seq_len(K), length.out = n))
  
  dat <- dat %>%
    filter(!is.na(Z), !is.na(Y1))
  
  m1_hat <- numeric(n)   # cross-fit estimate of E[Δ | X, A = 1] (intervention)
  p_hat  <- numeric(n)   # cross-fit estimate of P(A = 1 | X)
  for (k in seq_len(K)) {
    te <- which(folds == k); tr <- which(folds != k); i_tr <- tr[A[tr] == 1]
    cat("Outcome:", outcome_name, "\n")
    cat("NA in D:", sum(is.na(D)), "\n")
    cat("NaN in D:", sum(is.nan(D)), "\n")
    cat("Inf in D:", sum(is.infinite(D)), "\n")
    m1_hat[te] <- xgb_xfit(X[i_tr, , drop = FALSE], D[i_tr],
                           X[te,   , drop = FALSE], "reg:squarederror")
    p_hat[te]  <- xgb_xfit(X[tr,   , drop = FALSE], A[tr],
                           X[te,   , drop = FALSE], "binary:logistic")
  }
  p_hat <- pmin(pmax(p_hat, 1e-3), 1 - 1e-3)    # numerical safety only
  
  pi0     <- mean(1 - A)
  psi_vec <- (A - p_hat) / p_hat * (D - m1_hat) / pi0
  psi     <- mean(psi_vec)
  phi     <- psi_vec - ((1 - A) / pi0) * psi    # recentered influence function
  se      <- sd(phi) / sqrt(n)
  
  list(psi = psi, se = se,
       ci  = psi + c(-1, 1) * qnorm(0.975) * se,
       m1_hat = m1_hat, p_hat = p_hat, phi = phi)
}

dml_drdid_pre_end <- function(dat, x_vars, K = 5, seed = 1) {
  set.seed(seed)
  
  dat <- dat %>%
    filter(!is.na(Z), !is.na(Y2))
  
  X  <- data.matrix(dat[, x_vars, drop = FALSE])
  A  <- as.numeric(dat$A)
  D  <- as.numeric(dat$Y2 - dat$Z)            # within-household change Δ = Y - Z
  n  <- nrow(dat)
  folds <- sample(rep(seq_len(K), length.out = n))
  
  
  
  m1_hat <- numeric(n)   # cross-fit estimate of E[Δ | X, A = 1] (intervention)
  p_hat  <- numeric(n)   # cross-fit estimate of P(A = 1 | X)
  for (k in seq_len(K)) {
    te <- which(folds == k); tr <- which(folds != k); i_tr <- tr[A[tr] == 1]
    m1_hat[te] <- xgb_xfit(X[i_tr, , drop = FALSE], D[i_tr],
                           X[te,   , drop = FALSE], "reg:squarederror")
    p_hat[te]  <- xgb_xfit(X[tr,   , drop = FALSE], A[tr],
                           X[te,   , drop = FALSE], "binary:logistic")
  }
  p_hat <- pmin(pmax(p_hat, 1e-3), 1 - 1e-3)    # numerical safety only
  
  pi0     <- mean(1 - A)
  psi_vec <- (A - p_hat) / p_hat * (D - m1_hat) / pi0
  psi     <- mean(psi_vec)
  phi     <- psi_vec - ((1 - A) / pi0) * psi    # recentered influence function
  se      <- sd(phi) / sqrt(n)
  
  list(psi = psi, se = se,
       ci  = psi + c(-1, 1) * qnorm(0.975) * se,
       m1_hat = m1_hat, p_hat = p_hat, phi = phi)
}

# Section 3: Simplified pre-period ATT with parametric nuisances on the full sample.


# Outcome regression of `y` on X, fit on rows `idx`, predicted for every row.
# family = "gaussian" (linear regression) or "binomial" (logistic regression).
or_fit <- function(X, y, idx, family = "gaussian") {
  fam <- if (family == "binomial") binomial() else gaussian()
  df  <- data.frame(y = y, X)
  fit <- suppressWarnings(
    glm(y ~ ., data = df[idx, , drop = FALSE], family = fam))
  predict(fit, newdata = data.frame(X), type = "response")
}

# Propensity model P(A = 1 | X), logistic, fit and predicted on the full sample.
ps_fit <- function(X, A) {
  fit <- glm(A ~ ., data = data.frame(A = A, X), family = binomial())
  predict(fit, newdata = data.frame(X), type = "response")
}

# Train xgboost on `X_tr/y_tr` with a small grid over (max_depth, eta) and
# early stopping for nrounds; return predictions on `X_te`.
xgb_xfit <- function(X_tr, y_tr, X_te, objective,
                     depths = c(3, 5, 7), etas = c(0.03, 0.1)) {
  d       <- xgb.DMatrix(X_tr, label = y_tr)
  metric  <- if (objective == "reg:squarederror") "rmse" else "logloss"
  log_col <- paste0("test_", metric, "_mean")
  best    <- list(score = Inf, pars = NULL, nrounds = NULL)
  for (mxd in depths) for (eta in etas) {
    pars <- list(objective = objective, max_depth = mxd, eta = eta, verbosity = 0)
    cv   <- xgb.cv(params = pars, data = d, nrounds = 2000, nfold = 3,
                   early_stopping_rounds = 50, verbose = 0, metrics = metric)
    i    <- which.min(cv$evaluation_log[[log_col]])
    if (cv$evaluation_log[[log_col]][i] < best$score)
      best <- list(score = cv$evaluation_log[[log_col]][i], pars = pars, nrounds = i)
  }
  mod <- xgb.train(params = best$pars, data = d, nrounds = best$nrounds, verbose = 0)
  predict(mod, X_te)
}


drdid_pre_glm_mid <- function(dat, x_vars) {
  
  dat <- dat %>%
    filter(!is.na(Z), !is.na(Y1))
  
  dat <- dat %>% #Replace missing vars with the mean of dataset
    mutate(
      hh_per_structure_missing = as.integer(is.na(hh_per_structure)),
      edu_yrs_missing          = as.integer(is.na(edu_yrs)),
      
      hh_per_structure = replace_na(
        hh_per_structure,
        mean(hh_per_structure, na.rm = TRUE)
      ),
      
      edu_yrs = replace_na(
        edu_yrs,
        mean(edu_yrs, na.rm = TRUE)
      )
    )
  
  X <- data.matrix(dat[, x_vars, drop = FALSE])
  A <- as.numeric(dat$A)
  D <- as.numeric(dat$Y1 - dat$Z)
  n <- nrow(dat)
  
  
  
  m1 <- or_fit(X, D, which(A == 1), "gaussian")   # E[Δ | X, A = 1], linear
  p  <- pmin(pmax(ps_fit(X, A), 1e-3), 1 - 1e-3)
  
  pi0     <- mean(1 - A)
  psi_vec <- (A - p) / p * (D - m1) / pi0
  psi     <- mean(psi_vec)
  phi     <- psi_vec - ((1 - A) / pi0) * psi
  se      <- sd(phi) / sqrt(n)
  
  list(psi = psi, se = se, ci = psi + c(-1, 1) * qnorm(0.975) * se)
}

drdid_pre_glm_end <- function(dat, x_vars) {
  
  dat <- dat %>%
    filter(!is.na(Z), !is.na(Y2))
  
  dat <- dat %>% ### Replace missing vars with the mean of dataset 
    mutate(
      hh_per_structure_missing = as.integer(is.na(hh_per_structure)),
      edu_yrs_missing          = as.integer(is.na(edu_yrs)),
      
      hh_per_structure = replace_na(
        hh_per_structure,
        mean(hh_per_structure, na.rm = TRUE)
      ),
      
      edu_yrs = replace_na(
        edu_yrs,
        mean(edu_yrs, na.rm = TRUE)
      )
    )
  
  X <- data.matrix(dat[, x_vars, drop = FALSE])
  A <- as.numeric(dat$A)
  D <- as.numeric(dat$Y2 - dat$Z)
  n <- nrow(dat)
  
  
  
  m1 <- or_fit(X, D, which(A == 1), "gaussian")   # E[Δ | X, A = 1], linear
  p  <- pmin(pmax(ps_fit(X, A), 1e-3), 1 - 1e-3)
  
  
  pi0     <- mean(1 - A)
  psi_vec <- (A - p) / p * (D - m1) / pi0
  psi     <- mean(psi_vec)
  phi     <- psi_vec - ((1 - A) / pi0) * psi
  se      <- sd(phi) / sqrt(n)
  
  list(psi = psi, se = se, ci = psi + c(-1, 1) * qnorm(0.975) * se)
}

##################################################################
# Print results for section 1 and Section 2
#These are the kim and Lee Results for a dataset constructed with all three timepoints of data 
# Outcomes that should be reported as percentage-point effects
binary_outcomes <- c(
  "target_child_eye_itch_yn",
  "target_child_eye_red_yn",
  "fcs_binary",
  "target_child_clinic_resp_yn",
  "target_child_wheezing_yn",
  "target_child_distrubed_speech_yn",
  "respondent_disturbed_speech_yn",
  "respondent_eye_red_yn",
  "respondent_eye_itch_yn",
  "respondent_wheezing_yn",
  "respondent_eye_sore_yn",
  "target_child_cough",
  "target_child_fever",
  "suicidal_thoughts_30_yn",
  "CES_D_o16_score", 
  "target_child_fever", 
  "target_child_alri",
  "target_child_asthma",
  "target_child_severe_asthma"
)

results_table_pre <- map_dfr(outcomes, function(outcome_name) {
  
  # Build DML-ready panel
  df_panel <- make_dml_panel(hh_data, outcome_name)
  
  # DML-XGBoost estimates
  res_mid <- dml_drdid_pre_mid(
    dat = df_panel,
    x_vars = xvars
  )
  
  res_end <- dml_drdid_pre_end(
    dat = df_panel,
    x_vars = xvars
  )
  
  # GLM estimates
  res_mid_glm <- drdid_pre_glm_mid(
    dat = df_panel,
    x_vars = xvars
  )
  
  res_end_glm <- drdid_pre_glm_end(
    dat = df_panel,
    x_vars = xvars
  )
  
  # Determine reporting scale
  is_binary <- outcome_name %in% binary_outcomes
  
  scale_factor <- if (is_binary) 100 else 1
  unit_label   <- if (is_binary) " pp" else ""
  
  # Format results
  mid_result <- sprintf(
    "%.2f%s [%.2f, %.2f]",
    scale_factor * res_mid$psi,
    unit_label,
    scale_factor * res_mid$ci[1],
    scale_factor * res_mid$ci[2]
  )
  
  end_result <- sprintf(
    "%.2f%s [%.2f, %.2f]",
    scale_factor * res_end$psi,
    unit_label,
    scale_factor * res_end$ci[1],
    scale_factor * res_end$ci[2]
  )
  
  mid_result_glm <- sprintf(
    "%.2f%s [%.2f, %.2f]",
    scale_factor * res_mid_glm$psi,
    unit_label,
    scale_factor * res_mid_glm$ci[1],
    scale_factor * res_mid_glm$ci[2]
  )
  
  end_result_glm <- sprintf(
    "%.2f%s [%.2f, %.2f]",
    scale_factor * res_end_glm$psi,
    unit_label,
    scale_factor * res_end_glm$ci[1],
    scale_factor * res_end_glm$ci[2]
  )
  
  tibble(
    outcome = outcome_name,
    midline = mid_result,
    endline = end_result,
    midline_glm = mid_result_glm,
    endline_glm = end_result_glm
  )
})

View(results_table_pre)
write_csv(results_table_pre, here::here("4_data/KimLee_Section1_Results_Pre.csv"))


###########################################################
## SECTION 4: XGBOOST DML-DR-DiD ATT (Secondary analysis )


# Train xgboost on `X_tr/y_tr` with a small grid over (max_depth, eta) and
# early stopping for nrounds; return predictions on `X_te`.
xgb_xfit <- function(X_tr, y_tr, X_te, objective,
                     depths = c(3, 5, 7), etas = c(0.03, 0.1)) {
  d       <- xgb.DMatrix(X_tr, label = y_tr)
  metric  <- if (objective == "reg:squarederror") "rmse" else "logloss"
  log_col <- paste0("test_", metric, "_mean")
  best    <- list(score = Inf, pars = NULL, nrounds = NULL)
  for (mxd in depths) for (eta in etas) {
    pars <- list(objective = objective, max_depth = mxd, eta = eta, verbosity = 0)
    cv   <- xgb.cv(params = pars, data = d, nrounds = 2000, nfold = 3,
                   early_stopping_rounds = 50, verbose = 0, metrics = metric)
    i    <- which.min(cv$evaluation_log[[log_col]])
    if (cv$evaluation_log[[log_col]][i] < best$score)
      best <- list(score = cv$evaluation_log[[log_col]][i], pars = pars, nrounds = i)
  }
  mod <- xgb.train(params = best$pars, data = d, nrounds = best$nrounds, verbose = 0)
  predict(mod, X_te)
}

# DML-DR-DiD ATT.
#   dat:    data.frame with columns Z (baseline outcome), Y1 (outcome midline), A (1 = intervention, 0 = comparator),
#           plus the baseline covariates named in `x_vars`. At endline the outcomes are encoded as Y2 instead of Y1 both of which are included in dat. 
#   x_vars: character vector of covariate column names.
#   K:      number of cross-fitting folds.
dml_drdid_mid <- function(dat, x_vars, K = 5, seed = 1) {
  
  dat <- dat %>%
    filter(
      !is.na(Z),
      !is.na(Y1),
      !is.na(A)
    ) %>%
    filter(if_all(all_of(x_vars), ~ !is.na(.)))
  
  set.seed(seed)
  X  <- data.matrix(dat[, x_vars, drop = FALSE])
  A  <- as.numeric(dat$A)
  D  <- as.numeric(dat$Y1 - dat$Z)   # within-household change Δ = Y - Z
  n  <- nrow(dat)
  folds <- sample(rep(seq_len(K), length.out = n))
  
  m_hat <- numeric(n)   # cross-fit estimate of E[Δ | X, A = 0]
  p_hat <- numeric(n)   # cross-fit estimate of P(A = 1 | X)
  for (k in seq_len(K)) {
    te <- which(folds == k); tr <- which(folds != k); c_tr <- tr[A[tr] == 0]
    m_hat[te] <- xgb_xfit(X[c_tr, , drop = FALSE], D[c_tr],
                          X[te,   , drop = FALSE], "reg:squarederror")
    p_hat[te] <- xgb_xfit(X[tr,   , drop = FALSE], A[tr],
                          X[te,   , drop = FALSE], "binary:logistic")
  }
  p_hat  <- pmin(pmax(p_hat, 1e-3), 1 - 1e-3)    # numerical safety only
  
  # The estimator: DR moment + influence-function SE.
  pi_hat  <- mean(A)
  psi_vec <- (A - p_hat) / (1 - p_hat) * (D - m_hat) / pi_hat
  psi     <- mean(psi_vec)
  phi     <- psi_vec - (A / pi_hat) * psi        # recentered influence function
  se      <- sd(phi) / sqrt(n)
  
  list(psi = psi, se = se,
       ci  = psi + c(-1, 1) * qnorm(0.975) * se,
       m_hat = m_hat, p_hat = p_hat, phi = phi)
}
dml_drdid_end <- function(dat, x_vars, K = 5, seed = 1) {
  
  dat <- dat %>%
    filter(
      !is.na(Z),
      !is.na(Y1),
      !is.na(A)
    ) %>%
    filter(if_all(all_of(x_vars), ~ !is.na(.)))
  
  set.seed(seed)
  X  <- data.matrix(dat[, x_vars, drop = FALSE])
  A  <- as.numeric(dat$A)
  D  <- as.numeric(dat$Y2 - dat$Z)   # within-household change Δ = Y - Z
  n  <- nrow(dat)
  folds <- sample(rep(seq_len(K), length.out = n))
  
  m_hat <- numeric(n)   # cross-fit estimate of E[Δ | X, A = 0]
  p_hat <- numeric(n)   # cross-fit estimate of P(A = 1 | X)
  for (k in seq_len(K)) {
    te <- which(folds == k); tr <- which(folds != k); c_tr <- tr[A[tr] == 0]
    m_hat[te] <- xgb_xfit(X[c_tr, , drop = FALSE], D[c_tr],
                          X[te,   , drop = FALSE], "reg:squarederror")
    p_hat[te] <- xgb_xfit(X[tr,   , drop = FALSE], A[tr],
                          X[te,   , drop = FALSE], "binary:logistic")
  }
  p_hat  <- pmin(pmax(p_hat, 1e-3), 1 - 1e-3)    # numerical safety only
  
  # The estimator: DR moment + influence-function SE.
  pi_hat  <- mean(A)
  psi_vec <- (A - p_hat) / (1 - p_hat) * (D - m_hat) / pi_hat
  psi     <- mean(psi_vec)
  phi     <- psi_vec - (A / pi_hat) * psi        # recentered influence function
  se      <- sd(phi) / sqrt(n)
  
  list(psi = psi, se = se,
       ci  = psi + c(-1, 1) * qnorm(0.975) * se,
       m_hat = m_hat, p_hat = p_hat, phi = phi)
}

# Parametric nuisance helpers (full sample, no cross-fitting).

# Outcome regression of `y` on X, fit on rows `idx`, predicted for every row.
# family = "gaussian" (linear regression) or "binomial" (logistic regression).
or_fit <- function(X, y, idx, family = "gaussian") {
  fam <- if (family == "binomial") binomial() else gaussian()
  df  <- data.frame(y = y, X)
  fit <- suppressWarnings(
    glm(y ~ ., data = df[idx, , drop = FALSE], family = fam))
  predict(fit, newdata = data.frame(X), type = "response")
}

# Propensity model P(A = 1 | X), logistic, fit and predicted on the full sample.
ps_fit <- function(X, A) {
  fit <- glm(A ~ ., data = data.frame(A = A, X), family = binomial())
  predict(fit, newdata = data.frame(X), type = "response")
}



# Simplified DML-DR-DiD ATT with parametric nuisances on the full sample.
# The change Δ = Y - Z is modeled by linear regression for both continuous and
# binary outcomes (a linear-probability model of the change in the binary case).Again Y1 is midline and Y2 is endline. 

drdid_glm_mid <- function(dat, x_vars) {
  
  dat <- dat %>%
    filter(
      !is.na(Z),
      !is.na(Y1),
      !is.na(A)
    ) %>%
    filter(if_all(all_of(x_vars), ~ !is.na(.)))
  
  
  X <- data.matrix(dat[, x_vars, drop = FALSE])
  A <- as.numeric(dat$A)
  D <- as.numeric(dat$Y1 - dat$Z)
  n <- nrow(dat)
  
  m0 <- or_fit(X, D, which(A == 0), "gaussian")   # E[Δ | X, A = 0], linear
  p  <- pmin(pmax(ps_fit(X, A), 1e-3), 1 - 1e-3)
  
  pi_hat  <- mean(A)
  psi_vec <- (A - p) / (1 - p) * (D - m0) / pi_hat
  psi     <- mean(psi_vec)
  phi     <- psi_vec - (A / pi_hat) * psi
  se      <- sd(phi) / sqrt(n)
  
  list(psi = psi, se = se, ci = psi + c(-1, 1) * qnorm(0.975) * se)
}

drdid_glm_end <- function(dat, x_vars) {
  
  dat <- dat %>%
    filter(
      !is.na(Z),
      !is.na(Y2),
      !is.na(A)
    ) %>%
    filter(if_all(all_of(x_vars), ~ !is.na(.)))
  
  
  X <- data.matrix(dat[, x_vars, drop = FALSE])
  A <- as.numeric(dat$A)
  D <- as.numeric(dat$Y2 - dat$Z)
  n <- nrow(dat)
  
  m0 <- or_fit(X, D, which(A == 0), "gaussian")   # E[Δ | X, A = 0], linear
  p  <- pmin(pmax(ps_fit(X, A), 1e-3), 1 - 1e-3)
  
  pi_hat  <- mean(A)
  psi_vec <- (A - p) / (1 - p) * (D - m0) / pi_hat
  psi     <- mean(psi_vec)
  phi     <- psi_vec - (A / pi_hat) * psi
  se      <- sd(phi) / sqrt(n)
  
  list(psi = psi, se = se, ci = psi + c(-1, 1) * qnorm(0.975) * se)
}

## Run DML for every outcome

binary_outcomes <- c(
  "target_child_eye_itch_yn",
  "target_child_eye_red_yn",
  "fcs_binary",
  "target_child_clinic_resp_yn",
  "target_child_wheezing_yn",
  "target_child_distrubed_speech_yn",
  "respondent_disturbed_speech_yn",
  "respondent_eye_red_yn",
  "respondent_eye_itch_yn",
  "respondent_wheezing_yn",
  "respondent_eye_sore_yn",
  "target_child_cough",
  "target_child_fever",
  "suicidal_thoughts_30_yn", 
  "CES_D_o16_score",
  "target_child_alri", 
  "target_child_asthma",
  "target_child_severe_asthma"
)

results_table <- map_dfr(outcomes, function(outcome_name) {
  
  # Build DML-ready panel
  df_panel <- make_dml_panel(hh_data, outcome_name)
  
  # DML-XGBoost estimates
  res_mid <- dml_drdid_mid(
    dat = df_panel,
    x_vars = xvars
  )
  
  res_end <- dml_drdid_end(
    dat = df_panel,
    x_vars = xvars
  )
  
  # GLM estimates
  res_mid_glm <- drdid_glm_mid(
    dat = df_panel,
    x_vars = xvars
  )
  
  res_end_glm <- drdid_glm_end(
    dat = df_panel,
    x_vars = xvars
  )
  
  # Determine reporting scale
  is_binary <- outcome_name %in% binary_outcomes
  
  scale_factor <- if (is_binary) 100 else 1
  unit_label   <- if (is_binary) " pp" else ""
  
  # Format results
  mid_result <- sprintf(
    "%.1f%s [%.1f, %.1f]",
    scale_factor * res_mid$psi,
    unit_label,
    scale_factor * res_mid$ci[1],
    scale_factor * res_mid$ci[2]
  )
  
  end_result <- sprintf(
    "%.1f%s [%.1f, %.1f]",
    scale_factor * res_end$psi,
    unit_label,
    scale_factor * res_end$ci[1],
    scale_factor * res_end$ci[2]
  )
  
  mid_result_glm <- sprintf(
    "%.1f%s [%.1f, %.1f]",
    scale_factor * res_mid_glm$psi,
    unit_label,
    scale_factor * res_mid_glm$ci[1],
    scale_factor * res_mid_glm$ci[2]
  )
  
  end_result_glm <- sprintf(
    "%.1f%s [%.1f, %.1f]",
    scale_factor * res_end_glm$psi,
    unit_label,
    scale_factor * res_end_glm$ci[1],
    scale_factor * res_end_glm$ci[2]
  )
  
  tibble(
    outcome = outcome_name,
    midline = mid_result,
    endline = end_result,
    midline_glm = mid_result_glm,
    endline_glm = end_result_glm
  )
})


view(results_table)
write_csv(results_table, here::here("4_data/XGBOOST_Section4_Results.csv"))

############################################################################
#### Section #5 

# DML AIPW for the Z-ATE.
#   dat:    data.frame with columns Z, A (1 = intervention, 0 = comparator),
#           plus the t = -1 covariate columns named in `w_vars`.
#   w_vars: character vector of covariate column names.
#   K:      number of cross-fitting folds.

dat <- dat %>% ### Replace missing vars with the mean of dataset 
  filter(!is.na(Z) & !is.na(A)) %>%
  mutate(
    hh_per_structure_missing = as.integer(is.na(hh_per_structure)),
    edu_yrs_missing          = as.integer(is.na(edu_yrs)),
    
    hh_per_structure = replace_na(
      hh_per_structure,
      mean(hh_per_structure, na.rm = TRUE)
    ),
    
    edu_yrs = replace_na(
      edu_yrs,
      mean(edu_yrs, na.rm = TRUE)
    )
  )
w_vars <- xvars #Check covariate names

#install.packages("caret")
xgb_xfit <- function(X_tr, y_tr, X_te, objective,
                     depths = c(3, 5, 7), etas = c(0.03, 0.1)) {
  
  d <- xgb.DMatrix(X_tr, label = y_tr)
  
  metric  <- if (objective == "reg:squarederror") "rmse" else "logloss"
  log_col <- paste0("test_", metric, "_mean")
  
  best <- list(score = Inf, pars = NULL, nrounds = NULL)
  
  for (mxd in depths) {
    for (eta in etas) {
      
      pars <- list(
        objective = objective,
        max_depth = mxd,
        eta = eta,
        verbosity = 0
      )
      
      cv <- xgb.cv(
        params = pars,
        data = d,
        nrounds = 2000,
        nfold = 3,
        early_stopping_rounds = 50,
        verbose = 0,
        metrics = metric
      )
      
      i <- which.min(cv$evaluation_log[[log_col]])
      
      if (cv$evaluation_log[[log_col]][i] < best$score) {
        best <- list(score = cv$evaluation_log[[log_col]][i],
                     pars = pars,
                     nrounds = i)
      }
    }
  }
  
  mod <- xgb.train(
    params = best$pars,
    data = d,
    nrounds = best$nrounds,
    verbose = 0
  )
  
  dte <- xgb.DMatrix(X_te)
  predict(mod, dte)
}

dml_aipw_z <- function(dat, w_vars, K = 5, seed = 1) {
  set.seed(seed)
  W  <- data.matrix(dat[, w_vars, drop = FALSE])
  B  <- 1 - as.numeric(dat$A)        # B = 1 if comparator (treated by Z time)
  Z  <- as.numeric(dat$Z)
  n  <- nrow(dat)
  folds <- caret::createFolds(B, k = K, list = FALSE)
  
  m1_hat <- numeric(n)   # cross-fit estimate of E[Z | W, B = 1]
  m0_hat <- numeric(n)   # cross-fit estimate of E[Z | W, B = 0]
  e_hat  <- numeric(n)   # cross-fit estimate of P(B = 1 | W)
  for (k in seq_len(K)) {
    te  <- which(folds == k); tr <- which(folds != k)
    tr1 <- tr[B[tr] == 1];    tr0 <- tr[B[tr] == 0]
    if (length(tr1) == 0 || length(tr0) == 0) next
    cat("nrow W[tr1,]:", nrow(W[tr1, , drop = FALSE]), "\n")
    cat("length Z[tr1]:", length(Z[tr1]), "\n")
    
    cat("nrow W[tr0,]:", nrow(W[tr0, , drop = FALSE]), "\n")
    cat("length Z[tr0]:", length(Z[tr0]), "\n")
    
    cat("nrow W[te,]:", nrow(W[te, , drop = FALSE]), "\n")
    m1_hat[te] <- xgb_xfit(W[tr1, , drop = FALSE], Z[tr1],
                           W[te,  , drop = FALSE], "reg:squarederror")
    m0_hat[te] <- xgb_xfit(W[tr0, , drop = FALSE], Z[tr0],
                           W[te,  , drop = FALSE], "reg:squarederror")
    e_hat[te]  <- xgb_xfit(W[tr,  , drop = FALSE], B[tr],
                           W[te,  , drop = FALSE], "binary:logistic")
  }
  e_hat <- pmin(pmax(e_hat, 1e-3), 1 - 1e-3)
  
  phi <- (m1_hat - m0_hat) +
    B       * (Z - m1_hat) / e_hat -
    (1 - B) * (Z - m0_hat) / (1 - e_hat)
  psi <- mean(phi)
  se  <- sd(phi) / sqrt(n)
  
  list(
    estimate = list(
      psi = psi,
      se = se,
      ci_low = psi - qnorm(0.975) * se,
      ci_high = psi + qnorm(0.975) * se
    ),
    nuisance = list(
      m1_hat = m1_hat,
      m0_hat = m0_hat,
      e_hat = e_hat
    ),
    phi = phi
  )
}

res_z <- dml_aipw_z(
  dat    = dat,
  w_vars = w_vars)

#view(res_z)


res_z$estimate$psi
res_z$estimate$se
res_z$estimate$ci_low
res_z$estimate$ci_high


## Unadjusted DiD for midline and endline using the same data as the DML analyses (i.e. only households with both baseline midline and endline data respectively)
#also include the dif in dif function used currently in the main paper 

hh_db_mid <-
  hh_data %>% 
  mutate(
    timepoint_num = case_when(
      timepoint == "baseline" ~ 0,
      timepoint == "midline" ~ 1
    )
  )

hh_db_end <-
  hh_data %>% 
  mutate(
    timepoint_num = case_when(
      timepoint == "baseline" ~ 0,
      timepoint == "endline" ~ 1
    )
  )


dind_fxn <- function(var_interest, data){
  
  lm_caller <- function(LHS, RHS, data){
    formula <- as.formula(paste0(LHS, " ~ ", RHS))
    lm(formula, data = data)
  }
  
  model_list <- lapply(
    var_interest,
    lm_caller,
    RHS = "timepoint_num + study_arm_overall_num + timepoint_num * study_arm_overall_num",
    data = data
  )
  
  model_list_conf <- map(model_list, ~broom::tidy(.x, conf.int = TRUE))
  
  out <- map2_dfr(model_list_conf, var_interest, function(df, name){
    
    df %>%
      filter(term == "timepoint_num:study_arm_overall_num") %>%
      transmute(
        outcome = name,
        estimate = estimate,
        conf.low = conf.low,
        conf.high = conf.high
      )
  })
  
  return(out)
}

results_table_base_mid <- map_dfr(outcomes, function(outcome_name) {
  
  # Build DML-ready panel
  df_panel <- make_dml_panel(hh_data_base_mid, outcome_name)
  
  # Midline DiD xgboost
  res_mid <- dml_drdid_mid(
    dat = df_panel,
    x_vars = xvars
  )
  
  # Midline glm 
  res_mid_glm <- drdid_glm_mid(
    dat = df_panel,
    x_vars = xvars
  ) 
  
  # DiD 
  res_did_mid <- dind_fxn(
    var_interest = outcome_name,
    data = hh_db_mid
  )
  
  #DiD End 
  res_did_end <- dind_fxn(
    var_interest = outcome_name,
    data = hh_db_end
  )
  
  
  # Format estimates + CI
  mid_result <- sprintf(
    "%.3f [%.3f, %.3f]",
    res_mid$psi,
    res_mid$ci[1],
    res_mid$ci[2]
  )
  
  mid_result_glm <- sprintf(
    "%.3f [%.3f, %.3f]",
    res_mid_glm$psi,
    res_mid_glm$ci[1],
    res_mid_glm$ci[2]
  )
  
  did_result_mid <- sprintf(
    "%.1f [%.1f, %.1f]",
    res_did_mid$estimate,
    res_did_mid$conf.low,
    res_did_mid$conf.high
  )
  
  did_result_end <- sprintf(
    "%.1f [%.1f, %.1f]",
    res_did_end$estimate,
    res_did_end$conf.low,
    res_did_end$conf.high
  )
  
  # Return tidy row
  tibble(
    outcome = outcome_name,
    midline = mid_result,
    did_mid = did_result_mid, 
    did_end = did_result_end
  )
})

view(results_table_base_mid)
