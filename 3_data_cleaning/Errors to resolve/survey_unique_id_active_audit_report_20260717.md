# Active Survey Unique ID Audit

Run date: 20260717

Identifier rule:
- HOST household survey: audit `hh_id`.
- Rohingya refugee household survey: audit `fcn_id`, because refugee analysis files use `fcn_id` as the unique household identifier.
- Dataset-level repeats across timepoints are expected for panel data; row-level duplicate problems are duplicates within the same timepoint.

## Files Written
- `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/3_data_cleaning/Errors to resolve/survey_unique_id_active_audit_summary_20260717.csv`
- `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/3_data_cleaning/Errors to resolve/survey_unique_id_active_problem_rows_20260717.csv`
- `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/3_data_cleaning/Errors to resolve/survey_unique_id_active_duplicate_groups_20260717.csv`

## Dataset Summary

### `4_data/clean_final/survey_host_household.rds`
- Identifier audited: `hh_id` (T0 followed by four digits)
- Rows: 513
- Missing identifier: 239
- Placeholder identifier: 0
- Malformed identifier: 0
- Duplicate groups across dataset: 74 (expected when the same household appears at multiple timepoints)
- Duplicate groups within timepoint: 0
- Row-level problem rows: 239
- Raw sources for row-level problems: raw_source_file=rohingya_fuel_v95_endline_Rohingya_host.csv; rohingya_fuel_v119_host.csv | raw_survey_version=v95; v119 | raw_collection_round=baseline_2020_survey_HOST_folder; endline_2022_survey_HOST_folder

### `4_data/clean_final/survey_refugee_household.rds`
- Identifier audited: `fcn_id` (six digits)
- Rows: 3291
- Missing identifier: 0
- Placeholder identifier: 0
- Malformed identifier: 0
- Duplicate groups across dataset: 1136 (expected when the same household appears at multiple timepoints)
- Duplicate groups within timepoint: 6
- Row-level problem rows: 12
- Raw sources for row-level problems: raw_source_file=rohingya_fuel_v113.csv; rohingya_fuel_v115.csv; rohingya_fuel_v116.csv | raw_survey_version=v113; v115; v116 | raw_collection_round=endline_2022_survey_endline_folder
