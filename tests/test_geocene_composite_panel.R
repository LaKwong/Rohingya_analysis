# Existing clean inputs only. --regenerate saves only the affected composite figures.
source(file.path(Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = "."),
  "5_analysis_RF105", "reviewed", "geocene_analysis_helpers.R"))
for (variant in geocene_variants) local({
  source(raw_import_path("5_analysis_RF105", "reviewed", "0_RF105_config_20260805_2213.R"), local = TRUE)
  if (variant != geocene_variants[1]) dir_figures_reviewed <- file.path(dir_figures_reviewed, paste0("sensitivity_", variant))
  dir.create(dir_figures_reviewed, recursive = TRUE, showWarnings = FALSE)
  stove_daily <- readRDS(file.path(geocene_clean_data_root(), "geocene", variant, "household_days.rds"))
  as_number <- function(x) suppressWarnings(as.numeric(as.character(x)))
  collapse_collection_years <- function(x) paste(sort(unique(x[!is.na(x)])), collapse = ", ")
  tables <- list()
  write_reviewed_csv <- function(x, filename, ...) tables[[filename]] <<- x
  plot_writer <- save_reviewed_plot
  save_reviewed_plot <- function(plot, filename, ...) {
    if ("--regenerate" %in% commandArgs(trailingOnly = TRUE)) {
      plot_writer(plot, geocene_output_filename(filename, variant), ...)
    }
  }
  source(raw_import_path("5_analysis_RF105", "reviewed", "geocene_composite_figures.R"), local = TRUE)
  expected <- c("exclusive biomass", "exclusive LPG", "mixed use combined")
  stopifnot(identical(levels(stove_energy_method_plot_data$cooking_method), expected))
  for (plot in list(fig_energy_consumed_method, fig_energy_pot_method)) {
    stopifnot(setequal(as.character(plot$data$cooking_method), expected),
      identical(ggplot_build(plot)$layout$panel_scales_x[[1]]$get_limits(), expected))
  }
  energy <- tables[["table_descriptive_stove_energy_by_cooking_method_summary.csv"]]
  stopifnot(setequal(as.character(energy$cooking_method), c(expected, "mixed use LPG", "mixed use biomass")))
  message(variant, ": panel B has three categories; energy tables retain all five categories.")
})
