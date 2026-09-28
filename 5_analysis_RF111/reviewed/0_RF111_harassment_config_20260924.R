# RF111 fuel-procurement harassment analysis configuration
#
# Purpose: Declare the canonical inputs and dated output locations.
# Run from: Rohingya_analysis/

analysis_version <- "20260924"

input_household_rds <- file.path(
  "4_data", "clean_final", "survey_refugee_household.rds"
)
input_baseline_xlsform <- file.path(
  "2_data_raw", "survey_baseline_survey and data review", "rohingya_fuel_v64.xlsx"
)
input_midline_xlsform <- file.path(
  "2_data_raw", "survey_midline_survey", "rohingya_fuel_v94_endline_Rohingya.xlsx"
)

output_table_dir <- file.path(
  "7_tables", paste0("RF111_harassment_", analysis_version)
)
output_figure_dir <- file.path(
  "6_figures", paste0("RF111_harassment_", analysis_version)
)
output_restricted_dir <- file.path(
  "8_restricted", paste0("RF111_harassment_", analysis_version)
)
output_qa_dir <- file.path(output_restricted_dir, "qa")

public_cell_minimum <- 5L
