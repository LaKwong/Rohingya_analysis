# Reproducible all-event Geocene analysis, primary and sensitivity exports.
# Run 1_run_geocene.R to rebuild import/cleaning as well as these outputs.
source(file.path(Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = "."),
  "5_analysis_RF105", "reviewed", "geocene_analysis_helpers.R"))
geocene_results <- lapply(geocene_variants, geocene_run_analysis)
geocene_comparison <- bind_rows(lapply(seq_along(geocene_variants), function(i) {
  scope <- geocene_results[[i]]$table_descriptive_geocene_monitoring_scope_summary %>% filter(summary_scope == "overall")
  post <- geocene_results[[i]]$table_descriptive_geocene_post_lpg_exclusive_use_summary
  input_root <- geocene_clean_data_root()
  d <- readRDS(file.path(input_root, "geocene", geocene_variants[i], "household_days.rds"))
  geocene_use_summary(d) %>% mutate(analysis_variant = geocene_variants[i],
    n_events = sum(d$cooking_events_with_lpg_zero + d$cooking_events_with_biomass_zero),
    n_missing_receipt_household_days = sum(is.na(d$days_after_first_receiving)))
}))
geocene_write(geocene_comparison, raw_import_path("7_tables", paste0("RF105_reviewed_", format(Sys.Date(), "%Y%m%d")), "table_descriptive_geocene_primary_sensitivity_comparison.csv"))
