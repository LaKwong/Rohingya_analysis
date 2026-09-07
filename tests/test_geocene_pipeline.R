# Standalone focused tests: Rscript --vanilla tests/test_geocene_pipeline.R
source(file.path("5_analysis_RF105", "reviewed", "geocene_analysis_helpers.R"))

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
stopifnot(sum(tables$table_descriptive_geocene_post_lpg_exclusive_use_summary$n_household_days) == 2,
  sum(tables$table_descriptive_geocene_stoves_monitored_by_days_after_receipt$n_household_days_monitored) == 5,
  identical(tables$table_descriptive_geocene_daily_summary, tables$table_descriptive_stove_daily_summary))
public_events <- mutate(clean, fcn_id = paste0("public_", fcn_id), hh_id = paste0("public_", hh_id))
public_daily <- geocene_collapse_days(public_events)
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
