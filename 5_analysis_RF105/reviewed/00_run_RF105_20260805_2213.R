################################################################################
# RF105 reviewed analysis runner
#
# Purpose:
#   Run the reviewed RF105 workflow in a reproducible order.
#
# Expected use:
#   From the Rohingya_analysis project root:
#     source("5_analysis_RF105/reviewed/00_run_RF105_20260805_2213.R")
#
# Optional:
#   If running the scripts from another folder, set:
#     Sys.setenv(ROHINGYA_ANALYSIS_ROOT = "G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis")
#
# Active reviewed workflow:
#   1. Build reviewed Geocene stove-use daily analysis products.
#   2. Build ambient-adjusted PM2.5 household-timepoint products.
#   3. Fit and post-process all rDiD/XGBoost and GLM sensitivity models.
#   4. Generate all descriptive tables and figures from one main descriptive file.
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
  grepl("^([A-Za-z]:|/|\\\\)", x)
}

reviewed_scripts <- c(
  "8_geocene_stove_use_combined_20260805_2213.R",
  "3_pm25_ambient_adjusted_analysis_20260805_2213.R",
  "4_rdid_xgboost_20260805_2213.R",
  "6_descriptive_outcomes_20260805_2213.R"
)

for (script in reviewed_scripts) {
  script_path <- if (is_absolute_path(script)) script else file.path(script_dir, script)
  message("Running ", script_path)
  source(script_path, chdir = TRUE)
}

message("RF105 reviewed analyses complete.")


