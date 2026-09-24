# Read-only validation of regenerated Geocene outputs; run after the reviewed analysis.
source(file.path(Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = "."), "5_analysis_RF105", "reviewed", "geocene_analysis_helpers.R"))
stamp <- format(Sys.Date(), "%Y%m%d")
for (variant in geocene_variants) {
  root <- raw_import_path("7_tables", paste0("RF105_reviewed_", stamp))
  if (variant != geocene_variants[1]) root <- file.path(root, paste0("sensitivity_", variant))
  read_table <- function(name) readr::read_csv(file.path(root,
    geocene_output_filename(paste0(name, ".csv"), variant)), show_col_types = FALSE)
  d <- readRDS(file.path(geocene_clean_data_root(), "geocene", variant, "household_days.rds"))
  e <- readRDS(file.path(geocene_clean_data_root(), "geocene", variant, "events.rds"))
  public_root <- raw_import_path("4_data", "clean_final_public", "geocene", variant)
  pd <- readRDS(file.path(public_root, "household_days.rds"))
  pe <- readRDS(file.path(public_root, "events.rds"))
  stopifnot(isTRUE(all.equal(geocene_tables(e, d), geocene_tables(pe, pd), check.attributes = FALSE)))
  # Validate linked results for every scope from public inputs as well.
  public_linked <- geocene_window_tables(pd)
  for (name in setdiff(names(public_linked), c("household_values", "overall_household_values"))) {
    actual <- read_table(paste0("table_descriptive_geocene_household_weighted_", name))
    # CSV readers infer all-empty numeric columns as logical; the ratio SD is intentionally all NA.
    if (name == "reconstruction") actual$sd <- as.numeric(actual$sd)
    comparison <- all.equal(as.data.frame(actual), as.data.frame(public_linked[[name]]), check.attributes = FALSE, tolerance = 1e-9)
    if (!isTRUE(comparison)) stop(variant, " / ", name, ": ", paste(comparison, collapse = "; "))
    stopifnot(!any(c("fcn_id", "hh_id", "date", "mission_key") %in% names(actual)))
    if (name != "reconstruction") {
      deviations <- if (name == "prevalence") actual$sd_prevalence_fraction else actual$sd
      stopifnot(all(!is.na(actual$sd_reason[is.na(deviations)])))
    }
  }
  s <- read_table("table_descriptive_geocene_daily_summary")
  stopifnot(isTRUE(all.equal(s, read_table("table_descriptive_stove_daily_summary"), check.attributes = FALSE)))
  scope <- read_table("table_descriptive_geocene_monitoring_scope_summary") %>% filter(summary_scope == "overall")
  stopifnot(scope$n_household_days_monitored == nrow(d), scope$n_refugee_households_monitored == n_distinct(d$fcn_id))
  baseline <- filter(s, timepoint == "baseline", study_arm_overall == "intervention")
  stopifnot(baseline$n_exclusive_lpg_days == 0)
  # Daily inputs/curves keep their original zero-inclusive household-day means.
  expected <- d %>% filter(days_after_first_receiving >= 0, !is.na(days_after_first_receiving)) %>%
    group_by(days_after_first_receiving) %>% summarise(
      mean_lpg_minutes_per_day = mean(stove_on_min_sum_lpg_zero),
      mean_biomass_minutes_per_day = mean(stove_on_min_sum_biomass_zero),
      n_household_days = n(), .groups = "drop")
  actual <- read_table("table_descriptive_stove_use_composite_day_summary") %>% rename(days_after_first_receiving = period_value)
  for (name in c("mean_lpg_minutes_per_day", "mean_biomass_minutes_per_day", "n_household_days")) {
    stopifnot(isTRUE(all.equal(actual[[name]], expected[[name]], check.attributes = FALSE, tolerance = 1e-10)))
  }
  # Earlier versions must agree on raw totals even though their averages differ.
  older <- raw_import_path("7_tables", "RF105_reviewed_20260919")
  if (variant != geocene_variants[1]) older <- file.path(older, paste0("sensitivity_", variant))
  old_file <- file.path(older, "table_descriptive_geocene_daily_summary.csv")
  if (file.exists(old_file)) {
    old <- readr::read_csv(old_file, show_col_types = FALSE)
    count_cols <- c("timepoint", "study_arm_overall", "n_household_days_monitored", "n_households", "n_stoves_monitored",
      "n_exclusive_lpg_days", "n_exclusive_biomass_days", "n_mixed_use_days")
    stopifnot(isTRUE(all.equal(as.data.frame(s[count_cols]), as.data.frame(old[count_cols]), check.attributes = FALSE)))
  }
  cat("Verified", variant, ": public/private linked summaries, SD reasons, raw counts, and unchanged exact-day means.\n")
}
