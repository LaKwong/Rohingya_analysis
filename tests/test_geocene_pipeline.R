# Standalone focused tests: Rscript --vanilla tests/test_geocene_pipeline.R
source(file.path("5_analysis_RF105", "reviewed", "geocene_analysis_helpers.R"))

cooking_fixture <- tibble(fcn_id = rep(c("test_household_1", "test_household_2"), 3), observed_stove_use_day = TRUE,
  exclusive_lpg_recalc = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
  exclusive_biomass_recalc = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
  mixed_use_recalc = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE),
  cooking_events_with_lpg_zero = c(1, 3, 0, 0, 1, 3),
  cooking_events_with_biomass_zero = c(0, 0, 2, 4, 2, 4),
  stove_on_min_sum_total_zero = c(10, 30, 20, 40, 30, 70))
cooking_summary <- geocene_cooking_by_use(cooking_fixture)
stopifnot(identical(cooking_summary$n_household_days, rep(2L, 3)),
  isTRUE(all.equal(cooking_summary$mean_cooking_events_per_day, c(2, 3, 5))),
  isTRUE(all.equal(cooking_summary$sd_cooking_events_per_day, sqrt(c(2, 2, 8)))),
  isTRUE(all.equal(cooking_summary$mean_daily_cooking_minutes, c(20, 30, 50))),
  isTRUE(all.equal(cooking_summary$sd_daily_cooking_minutes, sqrt(c(200, 200, 800)))))
stopifnot(all(is.na(geocene_cooking_by_use(cooking_fixture[1, ])$sd_daily_cooking_minutes)))
mixed_fixture <- cooking_fixture %>% mutate(lpg_recorded = !exclusive_biomass_recalc,
  biomass_recorded = !exclusive_lpg_recalc,
  stove_on_min_sum_lpg_zero = c(10, 30, 0, 0, 10, 30),
  stove_on_min_sum_biomass_zero = c(0, 0, 20, 40, 20, 40))
mixed_summary <- geocene_mixed_use_by_fuel(mixed_fixture)
stopifnot(all(mixed_summary$n_household_days == 2L),
  isTRUE(all.equal(mixed_summary$mean_cooking_events_per_day, c(5, 2, 3))),
  isTRUE(all.equal(mixed_summary$mean_daily_cooking_minutes, c(50, 20, 30))),
  isTRUE(all.equal(mixed_summary$sd_daily_cooking_minutes, sqrt(c(800, 200, 200)))),
  mixed_summary$total_cooking_events[1] == sum(mixed_summary$total_cooking_events[2:3]),
  mixed_summary$total_cooking_minutes[1] == sum(mixed_summary$total_cooking_minutes[2:3]))

names_fixture <- tibble(mission_name = c("CF_20191120_0_10FF34123456_11:34",
  "CF_20191120_1_4EPP21123457_11:34", "CF_ 20191120_0,10FF34123456_11:34",
  "CF_20191120_10FF34123456_11:34", "CF_20191120_0_10FF12345_11:34"))
parsed <- geocene_parse_missions(names_fixture)
stopifnot(identical(parsed$fcn_id[1:3], c("123456", "123457", "123456")),
  identical(parsed$study_arm_overall[1:3], c("intervention", "comparison", "intervention")),
  parsed$camp_id[1] == "10", parsed$block_id[1] == "F", parsed$subblock_id[1] == "F34",
  parsed$camp_id[2] == "4", parsed$block_id[2] == "E", parsed$subblock_id[2] == "PP21", all(is.na(parsed$fcn_id[4:5])))
example <- geocene_parse_missions(tibble(mission_name = "CF_20191120_0_10FF34113981_11:34"))
stopifnot(example$camp_id == "10", example$block_id == "F", example$subblock_id == "F34", example$fcn_id == "113981", example$hh_id == "10FF34113981")
camp_suffix <- geocene_parse_household_label(c("8WDI18123456", "8EFF12123457"))
stopifnot(identical(camp_suffix$camp_id, c("8W", "8E")), identical(camp_suffix$block_id, c("D", "F")), identical(camp_suffix$subblock_id, c("I18", "F12")))
variable_subblock <- geocene_parse_household_label(paste0("10FF", c("3", "34", "345", "3456"), "001234"))
stopifnot(identical(variable_subblock$subblock_id, c("F3", "F34", "F345", "F3456")),
  all(variable_subblock$fcn_id == "001234"), all(variable_subblock$block_id == "F"),
  all(is.na(geocene_parse_household_label(c("10FF001234", "10FF12345001234"))$fcn_id)))

events <- tibble(analysis_variant = "100_80_5_20", event_key = as.character(1:6),
  mission_key = c("mission_a", "mission_b", "mission_b", "mission_c", "mission_c", "mission_d"),
  fcn_id = c("123456", "123456", "123456", "123457", "123457", "123458"),
  hh_id = fcn_id, study_arm_overall = c(rep("intervention", 3), rep("comparison", 2), "intervention"),
  fuel_type = c("lpg", "biomass", "biomass", "lpg", "lpg", "biomass"),
  start_time = c("2019-11-20T21:00:00Z", "2019-11-21T02:00:00Z", "2019-11-22T02:00:00Z",
    "2019-11-20T17:50:00Z", "2020-03-25T02:00:00Z", "2020-10-01T02:00:00Z"),
  stop_time = c("2019-11-20T21:10:00Z", "2019-11-21T02:10:00Z", "2019-11-22T02:10:00Z",
    "2019-11-20T18:10:00Z", "2020-03-25T02:10:00Z", "2020-10-01T02:10:00Z"),
  duration_minutes = c(10, 10, 10, 20, 10, 10),
  first_receive_lpg_ymd = as.Date(c(rep("2020-01-01", 3), rep("2019-01-01", 2), NA)),
  receipt_date_conflict = FALSE)
clean <- geocene_prepare_events(events)
exclusion_fixture <- mutate(events, mission_id = c("broken", "other", "broken", "other", "other", "other"),
  mission_name = c("exact_name", "exact_name", "different_name", "other", "other", "other"))
partition <- geocene_exclude_missions(exclusion_fixture,
  tibble(mission_id = "broken", mission_name = "exact_name", exclusion_reason = "Probe appears to be broken"))
after_exclusion <- geocene_prepare_events(partition$retained)
stopifnot(nrow(partition$excluded) == 1, nrow(partition$retained) == 5,
  "3" %in% partition$retained$event_key,
  after_exclusion$timepoint[after_exclusion$event_key == "2"] == "baseline")
daily <- geocene_collapse_days(clean)
stopifnot(nrow(clean) == 6, nrow(daily) == 5, n_distinct(daily$fcn_id) == 3,
  sum(daily$n_stoves_with_recorded_use) == 6,
  clean$date[1] == as.Date("2019-11-21"), clean$date[4] == as.Date("2019-11-20"),
  clean$stove_on_min[4] == 20, all(clean$timepoint[1:2] == "midline"),
  clean$timepoint[3] == "baseline", clean$timepoint[5] == "baseline",
  is.na(clean$days_after_first_receiving[6]))
mixed <- daily %>% filter(fcn_id == "123456", date == as.Date("2019-11-21"))
stopifnot(nrow(mixed) == 1, mixed$mixed_use_recalc, mixed$n_stoves_with_recorded_use == 2,
  mixed$days_after_first_receiving < 0)
tables <- geocene_tables(clean, daily)
household_minutes_test <- geocene_household_stove_minutes(daily)
stopifnot(nrow(household_minutes_test) == n_distinct(daily$fcn_id),
  sum(household_minutes_test$total_stove_use_minutes) == sum(daily$stove_on_min_sum_total_zero))
minute_bins_test <- geocene_household_minutes_bins(tibble(total_stove_use_minutes = c(0, 59, 60, 119, 120, 240)))
stopifnot(identical(minute_bins_test$n_households, c(2L, 2L, 1L, 0L, 1L)),
  all(minute_bins_test$bin_end_minutes - minute_bins_test$bin_start_minutes == 60))
local({
  summary <- geocene_use_summary(daily)
  minute_columns <- c(lpg = "stove_on_min_sum_lpg_zero", biomass = "stove_on_min_sum_biomass_zero", total_stove = "stove_on_min_sum_total_zero")
  for (fuel in names(minute_columns)) {
    mean_name <- paste0("mean_", fuel, "_minutes_per_day")
    sd_name <- paste0("sd_", fuel, "_minutes_per_day")
    stopifnot(match(sd_name, names(summary)) == match(mean_name, names(summary)) + 1L,
      isTRUE(all.equal(summary[[sd_name]], sd(daily %>%
        filter(if (fuel == "lpg") lpg_recorded else if (fuel == "biomass") biomass_recorded else TRUE) %>%
        group_by(fcn_id) %>% summarise(value = mean(.data[[minute_columns[[fuel]]]]), .groups = "drop") %>% pull(value)))),
      is.na(geocene_use_summary(daily[1, ])[[sd_name]]))
  }
})
stopifnot(sum(tables$table_descriptive_geocene_post_lpg_exclusive_use_summary$n_household_days) == 2,
  sum(tables$table_descriptive_geocene_stoves_monitored_by_days_after_receipt$n_household_days_monitored) == 5,
  identical(tables$table_descriptive_geocene_daily_summary, tables$table_descriptive_stove_daily_summary))
public_events <- mutate(clean, fcn_id = paste0("public_", fcn_id), hh_id = paste0("public_", hh_id))
calendar_expected <- daily %>% group_by(date) %>% summarise(n_stoves = sum(lpg_recorded) + sum(biomass_recorded), .groups = "drop")
stopifnot(tables$table_descriptive_geocene_max_stoves_monitored_on_calendar_date$max_n_stoves_monitored_on_calendar_date == max(calendar_expected$n_stoves))
concurrent_events <- bind_rows(clean, mutate(clean, fcn_id = paste0("other_", fcn_id), hh_id = paste0("other_", hh_id)))
concurrent_tables <- geocene_tables(concurrent_events, geocene_collapse_days(concurrent_events))
stopifnot(concurrent_tables$table_descriptive_geocene_max_stoves_monitored_on_calendar_date$max_n_stoves_monitored_on_calendar_date == 2 * max(calendar_expected$n_stoves))
public_daily <- geocene_collapse_days(public_events)
local({
  # Energy must still be summarized when no receipt-relative plots are possible.
  arm_levels <- c("comparison", "intervention")
  as_number <- function(x) suppressWarnings(as.numeric(as.character(x)))
  energy_results <- list()
  write_reviewed_csv <- function(data, filename, ...) energy_results[[filename]] <<- data
  for (receipt_offset in c(NA_real_, -1)) {
    stove_daily <- mutate(daily, days_after_first_receiving = receipt_offset)
    source(raw_import_path("5_analysis_RF105", "reviewed", "geocene_composite_figures.R"), local = TRUE)
    energy <- energy_results[["table_descriptive_stove_energy_by_cooking_method_summary.csv"]]
    expected <- c("exclusive biomass" = sum(daily$exclusive_biomass_recalc), "exclusive LPG" = sum(daily$exclusive_lpg_recalc),
      "mixed use combined" = sum(daily$mixed_use_recalc), "mixed use LPG" = sum(daily$mixed_use_recalc), "mixed use biomass" = sum(daily$mixed_use_recalc))
    stopifnot(nrow(stove_composite_data) == 0,
      all(energy$n_household_days == unname(expected[as.character(energy$cooking_method)])))
    for (metric in unique(energy$energy_metric)) {
      e <- filter(energy, energy_metric == metric)
      means <- setNames(e$mean_energy_mj, as.character(e$cooking_method))
      stopifnot(isTRUE(all.equal(unname(means['mixed use combined']), unname(means['mixed use LPG'] + means['mixed use biomass']))),
        sum(e$n_household_days[e$cooking_method %in% c('exclusive biomass', 'exclusive LPG', 'mixed use combined')]) == nrow(daily))
    }
  }
})
stopifnot(isTRUE(all.equal(tables, geocene_tables(public_events, public_daily))))
stopifnot(inherits(try(geocene_tables(clean, bind_rows(daily, daily[1, ])), silent = TRUE), "try-error"))
stopifnot(is.na(geocene_timepoint(as.Date("2021-01-01"))), geocene_timepoint(as.Date("2022-01-02")) == "endline")
stopifnot(geocene_timepoint(as.Date("2020-08-31")) == "baseline", geocene_timepoint(as.Date("2020-09-01")) == "midline")
cat("Geocene parser, local-date, all-event, recoding, denominator, receipt-subset, and public-equivalence tests passed.\n")

if ("--figures" %in% commandArgs(trailingOnly = TRUE)) local({
  # All synthetic outputs stay restricted and cannot replace study results.
  root <- file.path(geocene_private, "tests")
  fixture <- bind_rows(lapply(1:8, function(i) clean %>% mutate(
    fcn_id = paste0(fcn_id, "_", i), hh_id = paste0(hh_id, "_", i),
    first_receive_lpg_ymd = as.Date("2019-01-01"),
    days_after_first_receiving = as.numeric(date - first_receive_lpg_ymd))))
  fixture_daily <- geocene_collapse_days(fixture)
  fixture_root <- file.path(root, "clean")
  for (variant in geocene_variants) {
    geocene_write(mutate(fixture, analysis_variant = variant), file.path(fixture_root, "geocene", variant, "events.rds"))
    geocene_write(mutate(fixture_daily, analysis_variant = variant), file.path(fixture_root, "geocene", variant, "household_days.rds"))
  }
  geocene_export_public(private_root = fixture_root, public_root = file.path(root, "public"), crosswalk_dir = file.path(root, "crosswalks"))
  public_fixture <- readRDS(file.path(root, "public", "geocene", geocene_variants[1], "events.rds"))
  public_days <- readRDS(file.path(root, "public", "geocene", geocene_variants[1], "household_days.rds"))
  stopifnot(!any(public_fixture$fcn_id %in% fixture$fcn_id), identical(public_fixture$mission_key, fixture$mission_key),
    isTRUE(all.equal(geocene_tables(fixture, fixture_daily), geocene_tables(public_fixture, public_days))))
  previous <- Sys.getenv("RF105_CLEAN_DATA_DIR", unset = "")
  on.exit(if (nzchar(previous)) Sys.setenv(RF105_CLEAN_DATA_DIR = previous) else Sys.unsetenv("RF105_CLEAN_DATA_DIR"))
  Sys.setenv(RF105_CLEAN_DATA_DIR = fixture_root)
  for (variant in geocene_variants) geocene_run_analysis(variant, output_root = file.path(root, "outputs"))
  cat("Synthetic primary and sensitivity figure integration tests passed.\n")
})
