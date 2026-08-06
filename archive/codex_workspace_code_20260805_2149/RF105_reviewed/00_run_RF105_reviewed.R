################################################################################
# RF105 reviewed analysis runner
#
# Purpose:
#   Run the reviewed companion scripts in a reproducible order.
#
# Expected use:
#   From the Rohingya_analysis project root:
#     source("5_analysis_RF105/00_run_RF105_reviewed.R")
#
# Optional:
#   If running the scripts from another folder, set:
#     Sys.setenv(ROHINGYA_ANALYSIS_ROOT = "G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis")
#
# Outputs:
#   Tables:  7_tables/RF105_reviewed_YYYYMMDD/
#   Figures: 6_figures/RF105_reviewed_YYYYMMDD/
################################################################################

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)

  if (length(file_arg) == 1) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg),
                                 winslash = "/", mustWork = FALSE)))
  }

  source_files <- vapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile)) frame$ofile else NA_character_
  }, character(1))
  source_files <- source_files[!is.na(source_files)]

  if (length(source_files) > 0) {
    return(dirname(normalizePath(source_files[[length(source_files)]],
                                 winslash = "/", mustWork = FALSE)))
  }

  getwd()
}

script_dir <- get_script_dir()

reviewed_scripts <- c(
  "1_hh_table1_reviewed.R",
  "3_fuel_outage_plastic_reviewed.R",
  "1_PM2.5_Analysis_and_figure_reviewed.R",
  "2.1_stove_monitor_uptake_reviewed.R",
  "5_respiratory health_reviewed.R"
)

for (script in reviewed_scripts) {
  message("Running ", script)
  source(file.path(script_dir, script), chdir = TRUE)
}

message("RF105 reviewed analyses complete.")

