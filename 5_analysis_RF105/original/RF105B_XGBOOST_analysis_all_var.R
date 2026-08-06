## XGBOOST FUNCTION 
library(xgboost)
library(tidyverse)


### Whole survey files 

file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")
file_survey_base_mid <- file_out_base_mid <- here::here("4_data/RohingyaFuel_survey_data_baseline_midline.rds")
fcs_data <- here::here("4_data/RohingyaFuel_survey_data_fcs.rds")
CES_D_data <- here::here("4_data/RohingyaFuel_survey_data_CES_D.rds")




fcs_output <- read_rds(fcs_data) 
CES_D_output <- read_rds(CES_D_data)

survey_data <- read_rds(file_survey_data_base) %>% 
  left_join(CES_D_output, by = c("fcn_id", "timepoint", "study_arm_overall")) %>%
  left_join(fcs_output, by = c("fcn_id", "timepoint", "study_arm_overall")) %>% 
  mutate(
    respondent_disturbed_speech_yn = if_else(is.na(respondent_disturbed_speech), 0, respondent_disturbed_speech),
    target_child_distrubed_speech_yn = if_else(is.na(target_child_distrubed_speech), 0, target_child_distrubed_speech)
  )

survey_data_base_mid <- read_rds(file_survey_base_mid) %>% 
  left_join(CES_D_output, by = c("fcn_id", "timepoint", "study_arm_overall")) %>%
  left_join(fcs_output, by = c("fcn_id", "timepoint", "study_arm_overall")) %>% 
  mutate(
    respondent_disturbed_speech_yn = if_else(is.na(respondent_disturbed_speech), 0, respondent_disturbed_speech),
    target_child_distrubed_speech_yn = if_else(is.na(target_child_distrubed_speech), 0, target_child_distrubed_speech)
  )

# Household member individual data 
data_hh_member_filename <- here::here("4_data/RohingyaFuel_data_hh_member.rds")
data_hh_member <- read_rds(data_hh_member_filename)


## outcome variables to analyze
outcomes <- c(
  "target_child_eye_itch_yn",
  "target_child_eye_red_yn",
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
  "target_child_resp_rate", 
  "respondent_eye_sore_yn",
  "target_child_cough",
  "target_child_fever",
  "target_child_resp_rate",
  "spent_food",
  "buy_wood_cost",
  "CES_D_o16_score", 
  "suicidal_thoughts_30_yn"
)

vars_to_keep <- c(
  "fcn_id", "hh_per_structure", "hh_size",
  "timepoint", "study_arm_overall_num",
  "age_yrs", "edu_yrs",
  outcomes
)

#Merge hh member data with survey data to get the age and education variables for the target child
data_hh_member <- data_hh_member %>%
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
      Y0 = baseline,
      Y1 = midline,
      Y2 = endline
    )
  
  # 4. Merge with covariates
  hh_covars %>%
    left_join(outcomes_wide, by = "fcn_id")
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


#### XGBOOST ANALYSIS 
# DML-DR-DiD ATT.
#   dat:    data.frame with columns Y0, Y1, A (1 = intervention, 0 = comparison),
#           plus the baseline covariates named in `x_vars`.
#   x_vars: character vector of covariate column names.
#   K:      number of cross-fitting folds.

dml_drdid_midline <- function(dat, x_vars, K = 5, seed = 1) {
  set.seed(seed)
  X  <- data.matrix(dat[, x_vars, drop = FALSE])
  A  <- as.numeric(dat$A)
  dY <- as.numeric(dat$Y1 - dat$Y0)
  n  <- nrow(dat)
  folds <- sample(rep(seq_len(K), length.out = n))
  
  m_hat <- numeric(n)   # cross-fit estimate of E[ΔY | X, A = 0]
  p_hat <- numeric(n)   # cross-fit estimate of P(A = 1 | X)
  for (k in seq_len(K)) {
    te <- which(folds == k); tr <- which(folds != k); c_tr <- tr[A[tr] == 0]
    m_hat[te] <- xgb_xfit(X[c_tr, , drop = FALSE], dY[c_tr],
                          X[te,   , drop = FALSE], "reg:squarederror")
    p_hat[te] <- xgb_xfit(X[tr,   , drop = FALSE], A[tr],
                          X[te,   , drop = FALSE], "binary:logistic")
  }
  p_hat  <- pmin(pmax(p_hat, 1e-3), 1 - 1e-3)    # numerical safety only
  
  # The estimator: DR moment + influence-function SE.
  pi_hat <- mean(A)
  psi    <- (A - p_hat) / (1 - p_hat) * (dY - m_hat) / pi_hat
  tau    <- mean(psi)
  phi    <- psi - (A / pi_hat) * tau             # recentered influence function
  se     <- sd(phi) / sqrt(n)
  
  list(tau = tau, se = se,
       ci  = tau + c(-1, 1) * qnorm(0.975) * se,
       m_hat = m_hat, p_hat = p_hat, phi = phi)
}

dml_drdid_midline <- function(dat, x_vars, K = 5, seed = 1) {
  # Keep only complete finite observations
  needed_vars <- c("Y0", "Y1", "A", x_vars)
  dat <- dat %>%
    filter(
      across(all_of(needed_vars), ~ is.finite(.x))
    )
  set.seed(seed)
  
  X  <- data.matrix(dat[, x_vars, drop = FALSE])
  A  <- as.numeric(dat$A)
  dY <- as.numeric(dat$Y1 - dat$Y0)
  
  n <- nrow(dat)
  
  # Safety check
  if(n < K) {
    stop("Not enough complete observations for chosen number of folds.")
  }
  folds <- sample(rep(seq_len(K), length.out = n))
  m_hat <- numeric(n)   # E[ΔY | X, A = 0]
  p_hat <- numeric(n)   # P(A = 1 | X)
  
  for (k in seq_len(K)) {
    
    te <- which(folds == k)
    tr <- which(folds != k)
    c_tr <- tr[A[tr] == 0]
    
    # Skip fold if no controls available
    if(length(c_tr) == 0) next
    m_hat[te] <- xgb_xfit(
      X[c_tr, , drop = FALSE],
      dY[c_tr],
      X[te, , drop = FALSE],
      "reg:squarederror"
    )
    p_hat[te] <- xgb_xfit(
      X[tr, , drop = FALSE],
      A[tr],
      X[te, , drop = FALSE],
      "binary:logistic"
    )
  }
  # Numerical stability
  p_hat <- pmin(pmax(p_hat, 1e-3), 1 - 1e-3)
  
  # DR-DiD estimator
  pi_hat <- mean(A)
  psi <- (A - p_hat) / (1 - p_hat) *
    (dY - m_hat) / pi_hat
  tau <- mean(psi)
  phi <- psi - (A / pi_hat) * tau
  se <- sd(phi) / sqrt(n)
  list(
    tau   = tau,
    se    = se,
    ci    = tau + c(-1, 1) * qnorm(0.975) * se,
    n     = n,
    m_hat = m_hat,
    p_hat = p_hat,
    phi   = phi
  )
}


dml_drdid_endline <- function(dat, x_vars, K = 5, seed = 1) {
  set.seed(seed)
  X  <- data.matrix(dat[, x_vars, drop = FALSE])
  A  <- as.numeric(dat$A)
  dY <- as.numeric(dat$Y2 - dat$Y0)
  n  <- nrow(dat)
  folds <- sample(rep(seq_len(K), length.out = n))
  
  m_hat <- numeric(n)   # cross-fit estimate of E[ΔY | X, A = 0]
  p_hat <- numeric(n)   # cross-fit estimate of P(A = 1 | X)
  for (k in seq_len(K)) {
    te <- which(folds == k); tr <- which(folds != k); c_tr <- tr[A[tr] == 0]
    m_hat[te] <- xgb_xfit(X[c_tr, , drop = FALSE], dY[c_tr],
                          X[te,   , drop = FALSE], "reg:squarederror")
    p_hat[te] <- xgb_xfit(X[tr,   , drop = FALSE], A[tr],
                          X[te,   , drop = FALSE], "binary:logistic")
  }
  p_hat  <- pmin(pmax(p_hat, 1e-3), 1 - 1e-3)    # numerical safety only
  
  # The estimator: DR moment + influence-function SE.
  pi_hat <- mean(A)
  psi    <- (A - p_hat) / (1 - p_hat) * (dY - m_hat) / pi_hat
  tau    <- mean(psi)
  phi    <- psi - (A / pi_hat) * tau             # recentered influence function
  se     <- sd(phi) / sqrt(n)
  
  list(tau = tau, se = se,
       ci  = tau + c(-1, 1) * qnorm(0.975) * se,
       m_hat = m_hat, p_hat = p_hat, phi = phi)
}

dml_drdid_endline <- function(dat, x_vars, K = 5, seed = 1) {
  # Keep only complete finite observations
  needed_vars <- c("Y0", "Y2", "A", x_vars)
  dat <- dat %>%
    filter(
      across(all_of(needed_vars), ~ is.finite(.x))
    )
  set.seed(seed)
  
  X  <- data.matrix(dat[, x_vars, drop = FALSE])
  A  <- as.numeric(dat$A)
  dY <- as.numeric(dat$Y2 - dat$Y0)
  
  n <- nrow(dat)
  
  # Safety check
  if(n < K) {
    stop("Not enough complete observations for chosen number of folds.")
  }
  folds <- sample(rep(seq_len(K), length.out = n))
  m_hat <- numeric(n)   # E[ΔY | X, A = 0]
  p_hat <- numeric(n)   # P(A = 1 | X)
  
  for (k in seq_len(K)) {
    
    te <- which(folds == k)
    tr <- which(folds != k)
    c_tr <- tr[A[tr] == 0]
    
    # Skip fold if no controls available
    if(length(c_tr) == 0) next
    m_hat[te] <- xgb_xfit(
      X[c_tr, , drop = FALSE],
      dY[c_tr],
      X[te, , drop = FALSE],
      "reg:squarederror"
    )
    p_hat[te] <- xgb_xfit(
      X[tr, , drop = FALSE],
      A[tr],
      X[te, , drop = FALSE],
      "binary:logistic"
    )
  }
  # Numerical stability
  p_hat <- pmin(pmax(p_hat, 1e-3), 1 - 1e-3)
  
  # DR-DiD estimator
  pi_hat <- mean(A)
  psi <- (A - p_hat) / (1 - p_hat) *
    (dY - m_hat) / pi_hat
  tau <- mean(psi)
  phi <- psi - (A / pi_hat) * tau
  se <- sd(phi) / sqrt(n)
  list(
    tau   = tau,
    se    = se,
    ci    = tau + c(-1, 1) * qnorm(0.975) * se,
    n     = n,
    m_hat = m_hat,
    p_hat = p_hat,
    phi   = phi
  )
}

# Get the table of each var in hh data 
summary_table <- data.frame(
  Variable = names(df_eye_itch),
  N = sapply(df_eye_itch, function(x) sum(!is.na(x))),
  Mean = sapply(df_eye_itch, function(x)
    if(is.numeric(x)) mean(x, na.rm = TRUE) else NA),
  Missing = sapply(df_eye_itch, function(x) sum(is.na(x)))
)

print(summary_table)


# 
# #Run functions to get at each of these 
# 
# 
# df_eye_itch <- make_dml_panel(hh_data, "spent_food")
# #df_eye_itch <- make_wide_outcome(hh_data, "target_child_eye_itch_yn")
# #df_eye_red <- make_wide_outcome(hh_data, "target_child_eye_red_yn")
# 
# res <- dml_drdid_midline(
#   dat    = df_eye_itch,
#   x_vars = c("hh_size",  "hh_per_structure", "age_yrs", "edu_yrs")
# )
# 
# res_end <- dml_drdid_endline(
#   dat    = df_eye_itch,
#   x_vars = c("hh_size",  "hh_per_structure", "age_yrs", "edu_yrs")
# )

# ## Print for each variable (Doing this one at a time and putting in the supplement table)
# res$tau    # ATT point estimate
# res$se     # standard error
# res$ci     # 95% Wald CI
# 
# res_end$tau    # ATT point estimate
# res_end$se     # standard error
# res_end$ci     # 95% Wald CI


### # Parametric nuisance helpers (full sample, no cross-fitting).

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
# binary outcomes (a linear-probability model of the change in the binary case).
drdid_glm <- function(dat, x_vars) {
  X <- data.matrix(dat[, x_vars, drop = FALSE])
  A <- as.numeric(dat$A)
  D <- as.numeric(dat$Y - dat$Z)
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



## covariates for DML
xvars <- c(
  "hh_size",
  "hh_per_structure",
  "age_yrs",
  "edu_yrs"
)

## Run DML for every outcome
results_table <- map_dfr(outcomes, function(outcome_name) {
  
  # Build DML-ready panel
  df_panel <- make_dml_panel(hh_data, outcome_name)
  
  # Midline DiD
  res_mid <- dml_drdid_midline(
    dat = df_panel,
    x_vars = xvars
  )
  
  # Endline DiD
  res_end <- dml_drdid_endline(
    dat = df_panel,
    x_vars = xvars
  )
  
  # Format estimates + CI
  mid_result <- sprintf(
    "%.3f [%.3f, %.3f]",
    res_mid$tau,
    res_mid$ci[1],
    res_mid$ci[2]
  )
  
  end_result <- sprintf(
    "%.3f [%.3f, %.3f]",
    res_end$tau,
    res_end$ci[1],
    res_end$ci[2]
  )
  
  # Return tidy row
  tibble(
    outcome = outcome_name,
    midline = mid_result,
    endline = end_result
  )
})

## Print final table
print(results_table)


## Run DML for every outcome for data with hh who answered baseline and midline 
results_table_base_mid <- map_dfr(outcomes, function(outcome_name) {
  
  # Build DML-ready panel
  df_panel <- make_dml_panel(hh_data_base_mid, outcome_name)
  
  # Midline DiD
  res_mid <- dml_drdid_midline(
    dat = df_panel,
    x_vars = xvars
  )
  
  # Format estimates + CI
  mid_result <- sprintf(
    "%.3f [%.3f, %.3f]",
    res_mid$tau,
    res_mid$ci[1],
    res_mid$ci[2]
  )
  
  # Return tidy row
  tibble(
    outcome = outcome_name,
    midline = mid_result
  )
})

## Print final table
print(results_table_base_mid)
view(results_table_base_mid)



######## Redoing analysis using the DML AIPW estimator 
# DML AIPW for the Z-ATE.
#   dat:    data.frame with columns Z, A (1 = intervention, 0 = comparator),
#           plus the t = -1 covariate columns named in `w_vars`.
#   w_vars: character vector of covariate column names.
#   K:      number of cross-fitting folds.

dml_aipw_z <- function(dat, w_vars, K = 5, seed = 1) {
  set.seed(seed)
  W  <- data.matrix(dat[, w_vars, drop = FALSE])
  B  <- 1 - as.numeric(dat$A)        # B = 1 if comparator (treated by Z time)
  Z  <- as.numeric(dat$Z)
  n  <- nrow(dat)
  folds <- sample(rep(seq_len(K), length.out = n))
  
  m1_hat <- numeric(n)   # cross-fit estimate of E[Z | W, B = 1]
  m0_hat <- numeric(n)   # cross-fit estimate of E[Z | W, B = 0]
  e_hat  <- numeric(n)   # cross-fit estimate of P(B = 1 | W)
  for (k in seq_len(K)) {
    te  <- which(folds == k); tr <- which(folds != k)
    tr1 <- tr[B[tr] == 1];    tr0 <- tr[B[tr] == 0]
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
  
  list(psi = psi, se = se,
       ci  = psi + c(-1, 1) * qnorm(0.975) * se,
       m1_hat = m1_hat, m0_hat = m0_hat, e_hat = e_hat, phi = phi)
}

## Run DML AIPW for every outcome
results_table_aipw <- purrr::map_dfr(outcomes, function(outcome_name) {
  
  cat("\nRunning:", outcome_name, "\n")
  
  df_panel <- make_dml_panel(hh_data, outcome_name)
  
  # Skip if all missing
  if(all(is.na(df_panel$Y1)) | all(is.na(df_panel$Y2))) {
    
    warning(paste("Skipping", outcome_name, "- all outcomes missing"))
    
    return(tibble(
      outcome = outcome_name,
      midline = NA_character_,
      endline = NA_character_
    ))
  }
  
  # -----------------------
  # MIDLINE
  # -----------------------
  
  df_mid <- df_panel %>%
    mutate(Z = Y1)
  
  res_mid <- tryCatch(
    
    dml_aipw_z(
      dat = df_mid,
      w_vars = xvars
    ),
    
    error = function(e) {
      
      warning(paste("Midline failed for", outcome_name))
      
      return(NULL)
    }
  )
  
  # -----------------------
  # ENDLINE
  # -----------------------
  
  df_end <- df_panel %>%
    mutate(Z = Y2)
  
  res_end <- tryCatch(
    
    dml_aipw_z(
      dat = df_end,
      w_vars = xvars
    ),
    
    error = function(e) {
      
      warning(paste("Endline failed for", outcome_name))
      
      return(NULL)
    }
  )
  
  # Format safely
  mid_result <- if(is.null(res_mid)) {
    NA_character_
  } else {
    sprintf(
      "%.3f [%.3f, %.3f]",
      res_mid$psi,
      res_mid$ci[1],
      res_mid$ci[2]
    )
  }
  
  end_result <- if(is.null(res_end)) {
    NA_character_
  } else {
    sprintf(
      "%.3f [%.3f, %.3f]",
      res_end$psi,
      res_end$ci[1],
      res_end$ci[2]
    )
  }
  
  tibble(
    outcome = outcome_name,
    midline = mid_result,
    endline = end_result
  )
})

## Print final table

#combine results table and results aipw table
final_results <- results_table %>%
  left_join(results_table_aipw, by = "outcome", suffix = c("_drdid", "_aipw")) %>%
  select(outcome, midline_drdid = midline_drdid, endline_drdid = endline_drdid,
         midline_aipw = midline_aipw, endline_aipw = endline_aipw)

view(final_results)

