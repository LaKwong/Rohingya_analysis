################################################################################
# RF105 reviewed Geocene source-folder copy
#
# Purpose:
#   This file marks where the corresponding Geocene draft analysis was copied into
#   the reviewed RF105 workflow. The active reviewed implementation is the single
#   combined script one folder up:
#     5_analysis_RF105/reviewed/8_geocene_stove_use_combined_20260805_2213.R
#
# Notes:
#   Do not source draft Geocene scripts from 5_analysis/. The reviewed workflow
#   imports raw Geocene exports with 1_data_import/fixed/import_geocene_refugee_raw.R,
#   cleans them with 3_data_cleaning/fixed/clean_geocene_refugee_20260805_2141.R,
#   and writes reviewed tables/figures through the combined Geocene script.
################################################################################

message(
  "Run 5_analysis_RF105/reviewed/8_geocene_stove_use_combined_20260805_2213.R ",
  "for the reviewed Geocene stove-use analysis."
)