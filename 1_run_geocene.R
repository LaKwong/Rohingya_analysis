# Geocene-only pipeline. Does not re-import surveys or run PM/health analyses.
# Use Rscript --vanilla 1_run_geocene.R with the project's required packages installed.
source(file.path("1_data_import", "fixed", "import_geocene_refugee_raw.R"))
source(file.path("3_data_cleaning", "fixed", "clean_geocene_refugee_20260805_2141.R"))
geocene_export_public()
source(file.path("5_analysis_RF105", "reviewed", "geocene_analysis_helpers.R"))
for (variant in geocene_variants) {
  private_root <- file.path("4_data", "clean_final", "geocene", variant)
  public_root <- file.path("4_data", "clean_final_public", "geocene", variant)
  private_tables <- geocene_tables(readRDS(file.path(private_root, "events.rds")), readRDS(file.path(private_root, "household_days.rds")))
  public_tables <- geocene_tables(readRDS(file.path(public_root, "events.rds")), readRDS(file.path(public_root, "household_days.rds")))
  stopifnot(isTRUE(all.equal(private_tables, public_tables, check.attributes = FALSE)))
}
source(file.path("5_analysis_RF105", "reviewed", "1_geocene_stove_use_20260805_2213.R"))
message("Geocene primary/sensitivity outputs regenerated; public/private aggregate equivalence passed.")
