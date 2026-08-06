# Rohingya 105B with XG Boost 
#install.packages("xgboost")
library(xgboost)

file_survey_data_base <- here::here("4_data/RohingyaFuel_survey_data_triple.rds")
survey_data <- read_rds(file_survey_data_base)

# DML-DR-DiD ATT.
#   dat:    data.frame with columns Y0, Y1, Y2,  A (1 = intervention, 0 = comparison),
#           plus the baseline covariates named in `x_vars`.
#   x_vars: character vector of covariate column names.
#   K:      number of cross-fitting folds.

#Add Y0, Y1 and and A to the data frame
#This means trimmng the datato only the relevant columns for the DML-DR-DiD estimation and moving it to a wide format (1 row per hh)

#We also need to add the age yrs and education yrs to the dataset these are not saved in the survey data but in a separate dataset. We will merge them in using the fcn_id and timepoint variables.
data_hh_member_filename <- here::here("4_data/RohingyaFuel_data_hh_member.rds")
data_hh_member <- read_rds(data_hh_member_filename)

#Merge hh member data with survey data to get the age and education variables for the target child
data_hh_member <- data_hh_member %>%
  filter(hhh_relation == 1) %>%
  select(PARENT_KEY, disability, age_yrs, edu_yrs) 


survey_data_merge <- survey_data %>%
  left_join(data_hh_member, by = c("KEY" = "PARENT_KEY"))


household_data <- survey_data_merge %>%
  select(fcn_id, hh_per_structure, hh_size, timepoint, target_child_clinic_resp,study_arm_overall_num, age_yrs, edu_yrs) %>%
  mutate(
    A = if_else(study_arm_overall_num == 1, 1, 0),
    target_child_clinic_resp = if_else(target_child_clinic_resp > 0, 1, 0), 
  #  target_child_eye_itch = if_else(target_child_eye_itch > 0, 1, 0)
  )

hh_data <-  survey_data_merge %>%
  select(
    fcn_id,
    hh_per_structure,
    hh_size,
    timepoint, 
    target_child_eye_itch_yn,
    target_child_eye_red_yn, 
    study_arm_overall_num,
    age_yrs,
    edu_yrs) %>%
  mutate(
    A = if_else(study_arm_overall_num == 1, 1, 0),
    #target_child_clinic_resp = if_else(target_child_clinic_resp > 0, 1, 0), 
    #  target_child_eye_itch = if_else(target_child_eye_itch > 0, 1, 0)
  )


household_data_filled <- household_data %>%
  group_by(fcn_id) %>%
  fill(
    hh_per_structure,
    hh_size,
    age_yrs,
    edu_yrs,
    A,
    .direction = "downup"
  ) %>%
  ungroup()

household_data_wide <- household_data_filled %>%
  pivot_wider(
    id_cols = c(
      fcn_id,
      hh_per_structure,
      hh_size,
      age_yrs,
      edu_yrs,
      A
    ),
    names_from = timepoint,
    values_from = target_child_clinic_resp
    ) %>%
  rename(
    Y0 = baseline,
    Y1 = midline,
    Y2 = endline
  )


household_data_wide_final <- household_data_wide %>%
  group_by(fcn_id) %>%
  reframe(
    hh_per_structure = first(na.omit(hh_per_structure)),
    hh_size          = first(na.omit(hh_size)),
    age_yrs          = first(na.omit(age_yrs)),
    edu_yrs          = first(na.omit(edu_yrs)),
    A                = first(na.omit(A)),
    Y0               = first(na.omit(Y0)),
    Y1               = first(na.omit(Y1)),
    Y2               = first(na.omit(Y2))
  )

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
#   dat:    data.frame with columns Y0, Y1, A (1 = intervention, 0 = comparison),
#           plus the baseline covariates named in `x_vars`.
#   x_vars: character vector of covariate column names.
#   K:      number of cross-fitting folds.

dml_drdid <- function(dat, x_vars, K = 5, seed = 1) {
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


res <- dml_drdid(
  dat    = household_data_wide_final,
  x_vars = c("hh_size",  "hh_per_structure", "age_yrs", "edu_yrs")
)
res$tau    # ATT point estimate
res$se     # standard error
res$ci     # 95% Wald CI


##### Trying to make it give the same for any outcome 

library(dplyr)
library(tidyr)
library(rlang)

make_dml_panel <- function(data, outcome_var) {
  
  outcome_sym <- rlang::sym(outcome_var)
  
  # 1. Household-level covariates (one row per hh)
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
  
  # 2. Outcome in long format (collapsed duplicates)
  outcomes_long <- data %>%
    group_by(fcn_id, timepoint) %>%
    summarise(
      outcome = max(!!outcome_sym, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(timepoint = tolower(timepoint)) %>%
    complete(
      fcn_id,
      timepoint = c("baseline", "midline", "endline"),
      fill = list(outcome = 0)
    )
  
  # 3. Wide outcomes
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
  
  # 4. Final merge
  final_data <- hh_covars %>%
    left_join(outcomes_wide, by = "fcn_id")
  
  return(final_data)
}




df_eye_itch <- make_dml_panel(hh_data, "target_child_eye_itch_yn")
#df_eye_itch <- make_wide_outcome(hh_data, "target_child_eye_itch_yn")
#df_eye_red <- make_wide_outcome(hh_data, "target_child_eye_red_yn")

res <- dml_drdid(
  dat    = df_eye_itch,
  x_vars = c("hh_size",  "hh_per_structure", "age_yrs", "edu_yrs")
)
res$tau    # ATT point estimate
res$se     # standard error
res$ci     # 95% Wald CI


