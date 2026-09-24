# Focused unequal-duration tests; no study outputs are written.
source(file.path(Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = "."), "5_analysis_RF105", "reviewed", "geocene_analysis_helpers.R"))
d <- tibble(fcn_id = c("A", "A", "A", "B", "C"),
  date = as.Date("2020-01-01") + c(0, 1, 2, 0, 0),
  study_arm_overall = c("intervention", "intervention", "intervention", "comparison", "comparison"),
  timepoint = "midline", days_after_first_receiving = c(0, 1, 2, 0, NA_real_),
  months_after_first_receiving_numeric = 0,
  lpg_recorded = c(TRUE, TRUE, FALSE, TRUE, TRUE),
  biomass_recorded = c(FALSE, FALSE, TRUE, TRUE, FALSE),
  stove_on_min_sum_lpg_zero = c(10, 30, 0, 100, 80),
  stove_on_min_sum_biomass_zero = c(0, 0, 20, 50, 0),
  cooking_events_with_lpg_zero = c(1, 3, 0, 10, 8),
  cooking_events_with_biomass_zero = c(0, 0, 2, 5, 0)) %>%
  mutate(stove_on_min_sum_total_zero = stove_on_min_sum_lpg_zero + stove_on_min_sum_biomass_zero,
    observed_stove_use_day = TRUE, n_stoves_with_recorded_use = as.integer(lpg_recorded) + as.integer(biomass_recorded),
    exclusive_lpg_recalc = lpg_recorded & !biomass_recorded,
    exclusive_biomass_recalc = biomass_recorded & !lpg_recorded, mixed_use_recalc = lpg_recorded & biomass_recorded)
s <- geocene_use_summary(d)
stopifnot(abs(s$mean_total_stove_minutes_per_day - mean(c(20, 150, 80))) < 1e-10,
  abs(s$sd_total_stove_minutes_per_day - sd(c(20, 150, 80))) < 1e-10,
  s$median_total_stove_minutes_per_day == 80,
  abs(s$mean_lpg_minutes_per_day - mean(c(20, 100, 80))) < 1e-10,
  s$n_households_biomass_minutes_per_day == 2, s$n_biomass_use_household_days == 2)
r <- geocene_reconcilable(d)
p <- filter(r$prevalence, stove_use_category == "exclusive_lpg")$mean_prevalence_fraction
conditional <- filter(r$conditional_use, stove_use_category == "exclusive_lpg", metric == "minutes")$mean
contribution <- filter(r$contributions, stove_use_category == "exclusive_lpg", metric == "minutes")$mean
reconstruction <- filter(r$reconstruction, stove_use_category == "exclusive_lpg", metric == "minutes")$reconstruction_conditional_mean
stopifnot(abs(p - 5/9) < 1e-10, conditional == 50,
  abs(contribution - mean(c(40/3, 0, 80))) < 1e-10,
  abs(p * reconstruction - contribution) < 1e-10,
  abs(p * conditional - contribution) > 1)
zero <- filter(r$overall_use, fuel == "lpg", metric == "minutes")
stopifnot(abs(zero$sd - sd(c(40/3, 100, 80))) < 1e-10,
  all(is.na(r$reconstruction$sd)), all(nzchar(r$reconstruction$sd_reason)))
# Replicate A's complete pattern on new dates: its statistical weight is unchanged.
replicated <- bind_rows(d, mutate(filter(d, fcn_id == "A"), date = date + 10))
s2 <- geocene_use_summary(replicated)
stat_cols <- grep("^(mean_|sd_|median_|pct_)", names(s), value = TRUE)
stopifnot(isTRUE(all.equal(s[stat_cols], s2[stat_cols])))
r2 <- geocene_reconcilable(replicated)
for (name in c("prevalence", "conditional_use", "contributions", "overall_use", "reconstruction")) {
  cols <- setdiff(names(r[[name]]), c("n_household_days"))
  stopifnot(isTRUE(all.equal(r[[name]][cols], r2[[name]][cols])))
}
exact <- geocene_reconcilable(filter(d, date == min(date)))
stopifnot(all(exact$prevalence$sd_prevalence_fraction ==
  vapply(exact$prevalence$stove_use_category, function(cat) sd(as.numeric(geocene_category(filter(d, date == min(date))) == cat)), numeric(1))))
single <- geocene_reconcilable(d[1, ])
stopifnot(all(is.na(single$conditional_use$sd)),
  all(nzchar(single$conditional_use$sd_reason)),
  all(single$contributions$mean[single$contributions$stove_use_category != "exclusive_lpg"] == 0))
stopifnot(inherits(try(geocene_reconcilable(bind_rows(d, d[1, ])), silent = TRUE), "try-error"))
empty <- geocene_use_summary(d[0, ])
stopifnot(empty$n_households == 0, is.na(empty$mean_lpg_minutes_per_day),
  empty$sd_reason_lpg_minutes_per_day == "no_eligible_households")
# Same windows and numerical results after pseudonymization; no ID/date is public.
public <- mutate(d, fcn_id = paste0("public_", fcn_id))
rp <- geocene_reconcilable(public)
for (name in setdiff(names(r), c("household_values", "overall_household_values"))) {
  stopifnot(isTRUE(all.equal(r[[name]], rp[[name]])),
    !any(c("fcn_id", "date", "hh_id") %in% names(r[[name]])))
}
windows <- geocene_window_tables(d)
stopifnot(!anyNA(windows$prevalence$window_definition),
  !any(windows$household_values$fcn_id == "C" & windows$household_values$summary_scope %in% c("post_receipt", "exact_day")))
cat("Household weighting, SD, category contributions, replication, empty/singleton, privacy and receipt-window tests passed.\n")

