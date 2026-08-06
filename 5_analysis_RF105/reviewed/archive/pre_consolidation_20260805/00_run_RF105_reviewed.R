################################################################################
# RF105 reviewed analysis runner
#
# Purpose:
#   Run the reviewed companion scripts in a reproducible order.
#
# Expected use:
#   From the Rohingya_analysis project root:
#     source("5_analysis_RF105/reviewed/00_run_RF105_reviewed.R")
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

project_root <- Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = "")
if (!nzchar(project_root)) {
  candidate_root <- normalizePath(file.path(script_dir, "..", ".."),
                                  winslash = "/", mustWork = FALSE)
  if (dir.exists(file.path(candidate_root, "4_data", "clean_final"))) {
    project_root <- candidate_root
  } else {
    project_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }
}

is_absolute_path <- function(x) {
  grepl("^([A-Za-z]:|/|\\\\\\\\)", x)
}

reviewed_scripts <- c(
  "1_hh_table1_reviewed.R",
  "3_fuel_outage_plastic_reviewed.R",
  "1_PM2.5_Analysis_and_figure_reviewed.R",
  "8_geocene_stove_use_combined_reviewed.R",
  "2.1_stove_monitor_uptake_reviewed.R",
  "5_respiratory health_reviewed.R",
  "6_descriptive_outcomes_reviewed.R",
  "9_additional_legacy_survey_outcomes_reviewed.R",
  file.path(project_root, "5_analysis_RF105", "6_PM_analysis", "3_pm25_ambient_adjusted_analysis.R"),
  "4_rdid_xgboost_reviewed.R",
  "4.1_rdid_result_postprocess_reviewed.R",
  "7_pm25_midline_endline_explanation_reviewed.R",
  "10_original_style_figures_reviewed.R"
)

for (script in reviewed_scripts) {
  script_path <- if (is_absolute_path(script)) script else file.path(script_dir, script)
  message("Running ", script_path)
  source(script_path, chdir = TRUE)
}

message("RF105 reviewed analyses complete.")
