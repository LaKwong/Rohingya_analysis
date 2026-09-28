# RF111 fuel-procurement-related harassment analysis runner
#
# Run from a fresh R session with the working directory set to
# Rohingya_analysis/:
#   Rscript 5_analysis_RF111/reviewed/00_run_RF111_harassment_20260924.R

options(stringsAsFactors = FALSE, scipen = 999)

if (!file.exists("DESCRIPTION") || !file.exists("renv.lock")) {
  stop("Run this script from the Rohingya_analysis project root.")
}

source(file.path(
  "5_analysis_RF111", "reviewed",
  "1_RF111_harassment_descriptive_20260924.R"
))

source(file.path(
  "5_analysis_RF111", "reviewed",
  "2_RF111_harassment_independent_spotcheck_20260924.R"
))
