# Shared two-stage Geocene statistics. Source via geocene_analysis_helpers.R.
geocene_weighting_note <- paste(
  "Equal household weights within each analysis subset. Conditional LPG/biomass means use only that fuel's use days;",
  "total-stove means use all monitored days. SD/median are across household means, not household-days.",
  "Percentages average household proportions; counts remain raw. Count/total SDs are not applicable.")

geocene_stats <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  tibble(n_valid_households = n, mean = if (n) mean(x) else NA_real_,
    sd = if (n > 1) sd(x) else NA_real_, median = if (n) median(x) else NA_real_,
    sd_reason = if (n > 1) NA_character_ else if (n) "fewer_than_two_households" else "no_eligible_households")
}

geocene_wide_stats <- function(x, suffix) {
  s <- geocene_stats(x)
  names(s) <- c(paste0("n_households_", suffix), paste0("mean_", suffix),
    paste0("sd_", suffix), paste0("median_", suffix), paste0("sd_reason_", suffix))
  s
}

geocene_average <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_

geocene_summary_one <- function(d) {
  h <- d %>% group_by(fcn_id) %>% summarise(
    lpg_events_per_day = geocene_average(cooking_events_with_lpg_zero[lpg_recorded]),
    biomass_events_per_day = geocene_average(cooking_events_with_biomass_zero[biomass_recorded]),
    total_stove_events_per_day = geocene_average(cooking_events_with_lpg_zero + cooking_events_with_biomass_zero),
    lpg_minutes_per_day = geocene_average(stove_on_min_sum_lpg_zero[lpg_recorded]),
    biomass_minutes_per_day = geocene_average(stove_on_min_sum_biomass_zero[biomass_recorded]),
    total_stove_minutes_per_day = geocene_average(stove_on_min_sum_total_zero),
    household_pct_exclusive_lpg_days = 100 * mean(exclusive_lpg_recalc),
    household_pct_exclusive_biomass_days = 100 * mean(exclusive_biomass_recalc),
    household_pct_mixed_use_days = 100 * mean(mixed_use_recalc),
    pct_lpg_events = 100 * sum(cooking_events_with_lpg_zero) / sum(cooking_events_with_lpg_zero + cooking_events_with_biomass_zero),
    pct_lpg_minutes = 100 * sum(stove_on_min_sum_lpg_zero) / sum(stove_on_min_sum_total_zero),
    .groups = "drop")
  out <- tibble(n_daily_records = nrow(d), n_household_days_monitored = nrow(d),
    n_households = n_distinct(d$fcn_id), n_days_exclusive_denominator = nrow(d),
    n_stoves_monitored = sum(d$n_stoves_with_recorded_use),
    n_exclusive_lpg_days = sum(d$exclusive_lpg_recalc), n_exclusive_biomass_days = sum(d$exclusive_biomass_recalc),
    n_mixed_use_days = sum(d$mixed_use_recalc),
    n_lpg_use_household_days = sum(d$lpg_recorded), n_biomass_use_household_days = sum(d$biomass_recorded))
  for (metric in setdiff(names(h), "fcn_id")) out <- bind_cols(out, geocene_wide_stats(h[[metric]], metric))
  # Compatibility aliases; explicit household-weighted fields above are authoritative.
  out %>% mutate(pct_exclusive_lpg_days = mean_household_pct_exclusive_lpg_days,
    pct_exclusive_biomass_days = mean_household_pct_exclusive_biomass_days,
    pct_mixed_use_days = mean_household_pct_mixed_use_days, denominator_note = geocene_weighting_note)
}

geocene_use_summary <- function(d, groups = character()) {
  d %>% group_by(across(all_of(groups))) %>% group_modify(~geocene_summary_one(.x)) %>% ungroup()
}

geocene_conditional_cooking <- function(d, events, minutes) {
  h <- tibble(fcn_id = d$fcn_id, events = events, minutes = minutes) %>%
    group_by(fcn_id) %>% summarise(events = geocene_average(events), minutes = geocene_average(minutes), .groups = "drop")
  es <- geocene_stats(h$events); ms <- geocene_stats(h$minutes)
  tibble(n_household_days = nrow(d), n_households = nrow(h),
    total_cooking_events = sum(events), total_cooking_minutes = sum(minutes),
    n_days_with_event_count = sum(is.finite(events)), n_days_with_cooking_minutes = sum(is.finite(minutes)),
    n_households_with_event_count = es$n_valid_households, n_households_with_cooking_minutes = ms$n_valid_households,
    mean_cooking_events_per_day = es$mean, sd_cooking_events_per_day = es$sd,
    median_cooking_events_per_day = es$median, sd_reason_cooking_events = es$sd_reason,
    mean_daily_cooking_minutes = ms$mean, sd_daily_cooking_minutes = ms$sd,
    median_daily_cooking_minutes = ms$median, sd_reason_daily_cooking_minutes = ms$sd_reason,
    note = geocene_weighting_note)
}

geocene_categories <- c("exclusive_lpg", "exclusive_biomass", "both_stoves")
geocene_category <- function(d) case_when(d$exclusive_lpg_recalc ~ "exclusive_lpg",
  d$exclusive_biomass_recalc ~ "exclusive_biomass", d$mixed_use_recalc ~ "both_stoves")

geocene_cooking_by_use <- function(daily) {
  bind_rows(lapply(geocene_categories, function(category) {
    d <- daily[geocene_category(daily) == category, ]
    geocene_conditional_cooking(d, d$cooking_events_with_lpg_zero + d$cooking_events_with_biomass_zero,
      d$stove_on_min_sum_total_zero) %>% mutate(stove_use_category = category, .before = 1)
  }))
}

geocene_mixed_use_by_fuel <- function(daily) {
  d <- filter(daily, mixed_use_recalc)
  bind_rows(lapply(c("both_fuels_combined", "lpg", "biomass"), function(fuel) {
    events <- if (fuel == "both_fuels_combined") d$cooking_events_with_lpg_zero + d$cooking_events_with_biomass_zero else d[[paste0("cooking_events_with_", fuel, "_zero")]]
    minutes <- if (fuel == "both_fuels_combined") d$stove_on_min_sum_total_zero else d[[paste0("stove_on_min_sum_", fuel, "_zero")]]
    geocene_conditional_cooking(d, events, minutes) %>% mutate(fuel_contribution = fuel, .before = 1)
  }))
}

# Preserve the existing conversion formulas, including their historical units.
geocene_metric_days <- function(d, groups = character()) {
  base <- d %>% select(all_of(c(groups, "fcn_id"))) %>% mutate(stove_use_category = geocene_category(d))
  bind_rows(lapply(c("lpg", "biomass", "total"), function(fuel) {
    lp <- d$stove_on_min_sum_lpg_zero / 60 * (3.4 / 3.6)
    bp <- d$stove_on_min_sum_biomass_zero / 60 * (6.824 / 3.6)
    values <- switch(fuel,
      lpg = list(minutes = d$stove_on_min_sum_lpg_zero, events = d$cooking_events_with_lpg_zero, energy_consumed_mj = lp, energy_pot_mj = lp * .67),
      biomass = list(minutes = d$stove_on_min_sum_biomass_zero, events = d$cooking_events_with_biomass_zero, energy_consumed_mj = bp, energy_pot_mj = bp * .128),
      total = list(minutes = d$stove_on_min_sum_total_zero, events = d$cooking_events_with_lpg_zero + d$cooking_events_with_biomass_zero, energy_consumed_mj = lp + bp, energy_pot_mj = lp * .67 + bp * .128))
    bind_cols(base, as_tibble(values)) %>% mutate(fuel = fuel) %>%
      pivot_longer(all_of(names(values)), names_to = "metric", values_to = "value")
  }))
}

geocene_reconcilable <- function(d, groups = character()) {
  stopifnot(all(d$observed_stove_use_day), !anyNA(d$fcn_id),
    all(d$exclusive_lpg_recalc + d$exclusive_biomass_recalc + d$mixed_use_recalc == 1))
  if ("date" %in% names(d)) stopifnot(!anyDuplicated(d[c(groups, "fcn_id", "date")]))
  base <- d %>% group_by(across(all_of(c(groups, "fcn_id")))) %>% summarise(n_monitored_days = n(), .groups = "drop")
  counts <- d %>% mutate(stove_use_category = geocene_category(d)) %>%
    count(across(all_of(c(groups, "fcn_id", "stove_use_category"))), name = "n_category_days")
  prevalence_h <- tidyr::crossing(base, stove_use_category = geocene_categories) %>%
    left_join(counts, by = c(groups, "fcn_id", "stove_use_category")) %>%
    mutate(n_category_days = replace_na(n_category_days, 0L), value = n_category_days / n_monitored_days)
  describe <- function(x, keys) x %>% group_by(across(all_of(keys))) %>%
    summarise(n_households = n(), n_household_days = sum(n_monitored_days),
      n_valid_households = sum(is.finite(value)), mean = geocene_average(value),
      sd = if (n_valid_households > 1) sd(value[is.finite(value)]) else NA_real_,
      median = if (n_valid_households) median(value[is.finite(value)]) else NA_real_,
      sd_reason = if (n_valid_households > 1) NA_character_ else if (n_valid_households) "fewer_than_two_households" else "no_eligible_households", .groups = "drop")
  prevalence <- describe(prevalence_h, c(groups, "stove_use_category")) %>%
    rename(mean_prevalence_fraction = mean, sd_prevalence_fraction = sd, median_prevalence_fraction = median) %>%
    mutate(mean_household_percentage = 100 * mean_prevalence_fraction,
      sd_household_percentage = 100 * sd_prevalence_fraction, median_household_percentage = 100 * median_prevalence_fraction)
  metrics <- geocene_metric_days(d, groups)
  stopifnot(all(is.finite(metrics$value)))
  combos <- tibble(stove_use_category = c("exclusive_lpg", "exclusive_biomass", rep("both_stoves", 3)),
    fuel = c("lpg", "biomass", "total", "lpg", "biomass"))
  sums <- metrics %>% semi_join(combos, by = c("stove_use_category", "fuel")) %>%
    group_by(across(all_of(c(groups, "fcn_id", "stove_use_category", "fuel", "metric")))) %>%
    summarise(category_sum = sum(value), .groups = "drop")
  h <- prevalence_h %>% inner_join(combos, by = "stove_use_category", relationship = "many-to-many") %>%
    tidyr::crossing(metric = c("minutes", "events", "energy_consumed_mj", "energy_pot_mj")) %>%
    left_join(sums, by = c(groups, "fcn_id", "stove_use_category", "fuel", "metric")) %>%
    mutate(category_sum = replace_na(category_sum, 0),
      contribution = category_sum / n_monitored_days,
      conditional = if_else(n_category_days > 0, category_sum / n_category_days, NA_real_))
  keys <- c(groups, "stove_use_category", "fuel", "metric")
  conditional <- h %>% mutate(value = conditional, n_monitored_days = n_category_days) %>%
    filter(n_category_days > 0) %>% describe(keys)
  # Include absent categories with explicit missing-statistic reasons.
  skeleton <- h %>% distinct(across(all_of(keys)))
  conditional <- skeleton %>% left_join(conditional, by = keys) %>%
    mutate(across(c(n_households, n_household_days, n_valid_households), ~replace_na(.x, 0L)),
      sd_reason = if_else(n_valid_households == 0, "no_eligible_households", sd_reason))
  contributions <- h %>% mutate(value = contribution) %>% describe(keys)
  overall_h <- metrics %>% group_by(across(all_of(c(groups, "fcn_id", "fuel", "metric")))) %>%
    summarise(value = mean(value), n_monitored_days = n(), .groups = "drop")
  overall <- describe(overall_h, c(groups, "fuel", "metric"))
  reconstruction <- contributions %>% select(all_of(keys), mean_contribution = mean) %>%
    left_join(select(prevalence, all_of(c(groups, "stove_use_category")), mean_prevalence_fraction), by = c(groups, "stove_use_category")) %>%
    mutate(reconstruction_conditional_mean = if_else(mean_prevalence_fraction > 0, mean_contribution / mean_prevalence_fraction, NA_real_),
      sd = NA_real_, sd_reason = "SD not calculated for derived reconstruction ratio; SDs are reported for conditional household means and zero-inclusive contributions.",
      mean_reason = if_else(mean_prevalence_fraction == 0, "no_category_days", NA_character_))
  # Numeric identities are checked before any rounding or publication.
  pcheck <- prevalence %>% group_by(across(all_of(groups))) %>% summarise(p = sum(mean_prevalence_fraction), .groups = "drop")
  if (nrow(d)) stopifnot(all(abs(pcheck$p - 1) < 1e-10))
  ccheck <- contributions %>% filter(stove_use_category != "both_stoves" | fuel == "total") %>%
    group_by(across(all_of(c(groups, "metric")))) %>% summarise(contribution_sum = sum(mean), .groups = "drop") %>%
    left_join(filter(overall, fuel == "total"), by = c(groups, "metric"))
  stopifnot(all(abs(ccheck$contribution_sum - ccheck$mean) < 1e-8))
  reconstructed <- with(reconstruction, mean_prevalence_fraction * reconstruction_conditional_mean)
  available <- reconstruction$mean_prevalence_fraction > 0
  stopifnot(all(abs(reconstructed[available] - reconstruction$mean_contribution[available]) < 1e-8))
  mixed_check <- contributions %>% filter(stove_use_category == "both_stoves") %>%
    select(all_of(c(groups, "metric")), fuel, mean) %>% pivot_wider(names_from = fuel, values_from = mean)
  if (nrow(mixed_check)) stopifnot(all(abs(mixed_check$total - mixed_check$lpg - mixed_check$biomass) < 1e-8))
  list(prevalence = prevalence, conditional_use = conditional, contributions = contributions,
    overall_use = overall, reconstruction = reconstruction, household_values = h,
    overall_household_values = overall_h)
}

geocene_window_tables <- function(daily) {
  post <- filter(daily, !is.na(days_after_first_receiving), days_after_first_receiving >= 0)
  pieces <- list(overall = mutate(daily, window = "all"),
    timepoint = mutate(daily, window = as.character(timepoint)),
    exact_day = mutate(post, window = as.character(days_after_first_receiving)),
    week = mutate(post, window = as.character(floor(days_after_first_receiving / 7))),
    period30 = mutate(post, window = as.character(floor(days_after_first_receiving / 30))),
    month = mutate(post, window = as.character(months_after_first_receiving_numeric)),
    receipt_intervals = mutate(filter(daily, !is.na(days_after_first_receiving)),
      window = case_when(days_after_first_receiving < 0 ~ "not_yet_received", days_after_first_receiving > 210 ~ "211+",
        TRUE ~ as.character(pmax(1, ceiling(days_after_first_receiving / 30))))),
    midline_intervention_month = mutate(filter(post, timepoint == "midline", study_arm_overall == "intervention"), window = as.character(months_after_first_receiving_numeric)),
    post_receipt = mutate(post, window = "all"), post_receipt_30 = mutate(filter(post, days_after_first_receiving >= 30), window = "all"))
  results <- lapply(names(pieces), function(scope) {
    d <- geocene_all_arms(pieces[[scope]]) %>% mutate(summary_scope = scope)
    r <- geocene_reconcilable(d, c("summary_scope", "window", "study_arm_overall"))
    definitions <- c(overall = "All observed dates", timepoint = "Existing recoded study timepoint",
      exact_day = "Exact elapsed day, >=0", week = "floor(elapsed days / 7), >=0",
      period30 = "floor(elapsed days / 30), >=0", month = "Existing months_after_first_receiving_numeric, elapsed days >=0",
      receipt_intervals = "not_yet_received; 1=0-30; 2=31-60; ...; 7=181-210; 211+",
      midline_intervention_month = "Existing receipt month; intervention midline only, elapsed days >=0",
      post_receipt = "Elapsed days >=0", post_receipt_30 = "Elapsed days >=30")
    lapply(r, function(x) mutate(x, window_definition = definitions[[scope]],
      weighting_note = "Equal households within window; unmonitored dates are not zeros. Counts/totals: SD not applicable."))
  })
  setNames(lapply(names(results[[1]]), function(name) bind_rows(lapply(results, `[[`, name))), names(results[[1]]))
}

geocene_percentage_summary <- function(d, groups) {
  s <- geocene_use_summary(d, groups) %>% mutate(
    percent_exclusive_lpg_days = pct_exclusive_lpg_days,
    proportion_exclusive_lpg_days = pct_exclusive_lpg_days / 100,
    ci_lower = NA_real_, ci_upper = NA_real_)
  n <- s$n_households_household_pct_exclusive_lpg_days
  eligible <- n > 1
  margin <- rep(NA_real_, nrow(s))
  margin[eligible] <- qt(.975, n[eligible] - 1) * s$sd_household_pct_exclusive_lpg_days[eligible] / sqrt(n[eligible])
  s %>% mutate(ci_lower = pmax(0, percent_exclusive_lpg_days - margin),
    ci_upper = pmin(100, percent_exclusive_lpg_days + margin),
    ci_note = "t interval across household percentages, clipped to 0-100; NA with fewer than two households")
}
