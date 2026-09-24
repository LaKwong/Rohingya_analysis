# Run after the Geocene analysis to verify parallel, definition-labelled outputs.
source(file.path(Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = "."),
  "5_analysis_RF105", "reviewed", "geocene_analysis_helpers.R"))
stopifnot(geocene_output_filename("example.csv", "100_80_5_20") == "example_100_80_5_20.csv",
  geocene_output_filename("example.png", "100_80_5_30") == "example_100_80_5_30.png",
  grepl("20 minutes", geocene_event_definition("100_80_5_20")),
  grepl("30 minutes", geocene_event_definition("100_80_5_30")),
  inherits(try(geocene_output_filename("example.csv", "unknown"), silent = TRUE), "try-error"))
stamp <- paste0("RF105_reviewed_", format(Sys.Date(), "%Y%m%d"))
for (kind in c("7_tables", "6_figures")) {
  stems <- lapply(geocene_variants, function(variant) {
    root <- raw_import_path(kind, stamp)
    if (variant != geocene_variants[1]) root <- file.path(root, paste0("sensitivity_", variant))
    files <- list.files(root, pattern = paste0("_", variant, "\\.(csv|png|pdf)$"))
    files <- files[!grepl("_vs_", files)]
    stopifnot(length(files) > 0, all(file.info(file.path(root, files))$size > 0))
    if (kind == "7_tables") {
      notes <- readr::read_csv(file.path(root,
        geocene_output_filename("table_descriptive_geocene_analysis_notes.csv", variant)), show_col_types = FALSE)
      stopifnot(geocene_event_definition(variant) %in% notes$note)
    }
    cat(kind, variant, ":", length(files), "labelled outputs\n")
    sort(sub(paste0("_", variant, "\\."), ".", files))
  })
  stopifnot(identical(stems[[1]], stems[[2]]))
}
cat("Primary and sensitivity output inventories and event-definition notes agree.\n")
