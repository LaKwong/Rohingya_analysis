# Clean all imported events; collapse each variant to household/local-start-date.
# Input: 8_restricted/geocene_pipeline/events_linked.rds plus existing survey import.
# Outputs: 4_data/clean_final/geocene/<variant>/{events,household_days}.rds
source(file.path("1_data_import", "fixed", "geocene_pipeline_helpers.R"))
geocene_clean()
source(file.path("3_data_cleaning", "fixed", "0_clean_helpers_20260805_2141.R"))
for (variant in geocene_variants) for (kind in c("events", "household_days")) {
  output <- raw_import_path("4_data", "clean_final", "geocene", variant, paste0(kind, ".rds"))
  update_inventory(make_inventory_entry(
    dataset_name = paste("geocene", variant, kind, sep = "_"), data = readRDS(output),
    output_path = output, source_paths = file.path(geocene_private, "events_imported.rds"),
    removed_identifier_columns = c("mission_name", "mission_id", "device_id", "camp_id", "block_id", "subblock_id"),
    notes = "All events outside documented broken-probe mission exclusions retained; one household-day per household and Bangladesh-local event start date. See docs/geocene_pipeline.md."
  ))
}
output <- raw_import_path("4_data", "clean_final", "stove_use_geocene_refugee_daily.rds")
update_inventory(make_inventory_entry(
  dataset_name = "stove_use_geocene_refugee_daily", data = readRDS(output), output_path = output,
  source_paths = raw_import_path("4_data", "clean_final", "geocene", "100_80_5_20", "household_days.rds"),
  removed_identifier_columns = c("mission_name", "mission_id", "device_id"),
  notes = "Compatibility alias for the primary all-event household-day dataset."
))
