# Active survey household ID audit

> Note: survey identifier auditing is superseded by survey_unique_id_active_audit_report_20260717.md, which audits hh_id for HOST survey data and cn_id for Rohingya refugee survey data.

Run date: 20260717
Audited active household-level survey datasets in 4_data/clean_final and imported raw survey household/review datasets.
Generated correction/audit CSVs and long subrecord files without hh_id were excluded.

## Files written
- `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/3_data_cleaning/Errors to resolve/hh_id_active_survey_audit_summary_20260717.csv`
- `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/3_data_cleaning/Errors to resolve/hh_id_active_survey_problem_rows_20260717.csv`
- `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/3_data_cleaning/Errors to resolve/hh_id_active_survey_duplicate_groups_20260717.csv`

## Datasets needing cleaning review

### 4_data/clean_final/imported_raw/survey_refugee_household_raw.rds
- Rows: 3651
- Missing: 355; placeholder: 19; illegal chars: 117; component mismatches/wrong hh_id: 1094
- Duplicate groups: dataset=456; within timepoint=5
- Related raw files/source context: raw_source_file=RohingyaFuelMaster_Corrected_20200419_refugee.csv; rohingya_fuel_v86_endline_Rohingya.csv; rohingya_fuel_v89_endline_Rohingya.csv; rohingya_fuel_v90_endline_Rohingya.csv; rohingya_fuel_v92_endline_Rohingya.csv; rohingya_fuel_v93_endline_Rohingya.csv; rohingya_fuel_v94_endline_Rohingya.csv; rohingya_fuel_v113.csv; ... +2 more | raw_survey_version=v86; v89; v90; v92; v93; v94; v113; v115; ... +1 more | raw_collection_round=baseline_2019_2020_refugee_master; midline_2021_survey_midline_folder; endline_2022_survey_endline_folder | raw_source_path=G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/2_data_raw/survey_baseline/RohingyaFuelMaster_Corrected_20200419_refugee.csv; G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/2_data_raw/survey_midline/rohingya_fuel_v86_endline_Rohingya.csv; G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/2_data_raw/survey_midline/rohingya_fuel_v89_endline_Rohingya.csv; G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/2_data_raw/survey_midline/rohingya_fuel_v90_endline_Rohingya.csv; G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/2_data_raw/survey_midline/rohingya_fuel_v92_endline_Rohingya.csv; G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/2_data_raw/survey_midline/rohingya_fuel_v93_endline_Rohingya.csv; G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/2_data_raw/survey_midline/rohingya_fuel_v94_endline_Rohingya.csv; G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/2_data_raw/survey_endline/rohingya_fuel_v113.csv; ... +2 more

### 4_data/clean_final/survey_refugee_household.rds
- Rows: 3636
- Missing: 344; placeholder: 24; illegal chars: 99; component mismatches/wrong hh_id: 154
- Duplicate groups: dataset=561; within timepoint=5
- Related raw files/source context: raw_source_file=RohingyaFuelMaster_Corrected_20200419_refugee.csv; rohingya_fuel_v86_endline_Rohingya.csv; rohingya_fuel_v89_endline_Rohingya.csv; rohingya_fuel_v90_endline_Rohingya.csv; rohingya_fuel_v92_endline_Rohingya.csv; rohingya_fuel_v93_endline_Rohingya.csv; rohingya_fuel_v94_endline_Rohingya.csv; rohingya_fuel_v113.csv; ... +2 more | raw_survey_version=v86; v89; v90; v92; v93; v94; v113; v115; ... +1 more | raw_collection_round=baseline_2019_2020_refugee_master; midline_2021_survey_midline_folder; endline_2022_survey_endline_folder

### 4_data/clean_final/survey_host_household.rds
- Rows: 513
- Missing: 239; placeholder: 0; illegal chars: 0; component mismatches/wrong hh_id: 0
- Duplicate groups: dataset=74; within timepoint=0
- Related raw files/source context: raw_source_file=RohingyaFuelMaster_20200220_Corrected_20200308_HOST.csv; rohingya_fuel_20200106_v1_host.csv; rohingya_fuel_v95_endline_Rohingya_host.csv; rohingya_fuel_v119_host.csv | raw_survey_version=v1; v95; v119 | raw_collection_round=baseline_2020_survey_HOST_folder; endline_2022_survey_HOST_folder
- Baseline HOST generated IDs: row 98 = `T09001`; rows 35, 36, 37, and 179 = `T09002`-`T09005`. These are flagged with `hh_id_analysis_note = "hh_id made up for purposes of analysis"`.
- Endline HOST matched IDs: 74 high-confidence rows assigned from baseline using identifying fields; 58 endline rows remain unresolved in `host_endline_hh_id_unresolved_candidate_matches_20260717.csv`.
### 4_data/clean_final/imported_raw/survey_refugee_duplicate_review_raw.rds
- Rows: 402
- Missing: 0; placeholder: 0; illegal chars: 22; component mismatches/wrong hh_id: 0
- Duplicate groups: dataset=126; within timepoint=126
- Related raw files/source context: raw_source_file=rohingya_fuel_v116_duplicates.csv | raw_survey_version=v116 | raw_collection_round=endline_2022_duplicate_review_file | raw_source_path=G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/2_data_raw/survey_endline/rohingya_fuel_v116_duplicates.csv

