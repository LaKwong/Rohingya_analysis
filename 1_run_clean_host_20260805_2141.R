################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Run raw-first final host clean-data pipeline
################################################################################

source(file.path("1_data_import", "fixed", "0_import_raw_helpers.R"))
source(file.path("1_data_import", "fixed", "import_survey_host_raw.R"))

source(file.path("3_data_cleaning", "fixed", "0_clean_helpers_20260805_2141.R"))
source(file.path("3_data_cleaning", "fixed", "clean_survey_host_20260805_2141.R"))
source(file.path("3_data_cleaning", "fixed", "clean_survey_host_related_20260805_2141.R"))

message("Host raw-first final clean-data pipeline complete.")
