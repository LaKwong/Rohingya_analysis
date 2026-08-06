################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Run raw-first final refugee clean-data pipeline
################################################################################

source(file.path("1_data_import", "fixed", "0_import_raw_helpers.R"))
source(file.path("1_data_import", "fixed", "import_survey_refugee_raw.R"))
source(file.path("1_data_import", "fixed", "import_pm_pats_refugee_raw.R"))
source(file.path("1_data_import", "fixed", "import_geocene_refugee_raw.R"))

source(file.path("3_data_cleaning", "fixed", "0_clean_helpers_20260805_2141.R"))
source(file.path("3_data_cleaning", "fixed", "clean_survey_refugee_20260805_2141.R"))
source(file.path("3_data_cleaning", "fixed", "clean_survey_refugee_related_20260805_2141.R"))
source(file.path("3_data_cleaning", "fixed", "clean_pm_pats_refugee_20260805_2141.R"))
source(file.path("3_data_cleaning", "fixed", "clean_geocene_refugee_20260805_2141.R"))

message("Refugee raw-first final clean-data pipeline complete.")
