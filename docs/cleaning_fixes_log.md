# Cleaning fixes log

Generated: 2026-08-06 21:49:13 PDT

## Fixes implemented

- Added a raw-first import layer under `1_data_import/fixed/` and a separated final-cleaning pipeline under `3_data_cleaning/fixed/` as a self-contained cleaning workflow.
- Wrote raw-derived imports to `4_data/clean_final/imported_raw/` and final analysis datasets to `4_data/clean_final/` so existing non-clean_final `4_data` files remain untouched.
- Reclassified survey and stove-use timepoints by parsed collection timestamps. The PM2.5 final cleaner uses PM-specific round labels: raw household sensor folders take precedence, otherwise dates are labeled as baseline 2019-08-01 to 2019-12-01, midline 2020-09-01 to 2020-11-15, and endline 2022-01-01 to 2022-06-15.
- Kept refugee and host survey workflows separate, including separate raw survey imports and final survey cleaners.
- Treated PATS+ PM2.5 and Geocene stove-use data as refugee-only; no host PATS+ or host Geocene outputs were created.
- Excluded HAPEX personal PM2.5 from final outputs, following the project decision about damaged or less reliable monitors.
- Removed direct identifier columns from final datasets while preserving stable analysis IDs needed for joins.
- Uses a final host-cleaning script with corrected host household-member import logic.
- Standardized PATS+ ambient labels so school files map to the comparison ambient site and mosque files map to the intervention ambient site.
- Corrected the earlier final-pipeline mistake by rebuilding final outputs from raw imports rather than relying on intermediate RDS/CSV files outside clean_final.
- Explicitly included the 2022 refugee survey folder `2_data_raw/survey_endline`, the 2022 host survey folder `2_data_raw/survey_endline_HOST`, and the 2022 sensor folder `2_data_raw/ALL DATA_ENDLINE_2022_220703`.
- Applied the baseline household correction workbook and structured endline refugee survey review workbooks inside `3_data_cleaning/fixed/clean_survey_refugee_20260805_2141.R`; correction audit CSVs are written under `4_data/clean_final/`.
- Treats root `4_data/clean_final/*.rds` files as restricted internal outputs when they retain linkage identifiers needed for cleaning, joins, or audits; shareable de-identified RDS copies are written under `4_data/clean_final/shareable/` with direct identifiers, raw form keys, UUIDs, UNHCR-like fields, household IDs, and source filenames removed.

## Resulting datasets

- `pm25_pats_refugee_ambient`:  139456 rows,   52 columns; timepoints: endline=52526; baseline=52014; midline=34916
- `pm25_pats_refugee_indoor`: 1362519 rows,   52 columns; timepoints: baseline=559683; endline=529975; midline=272861
- `pm25_pats_refugee_indoor_household_counts_by_arm_timepoint`:       6 rows,    7 columns; timepoints: baseline=2; endline=2; midline=2
- `pm25_pats_refugee_qc_files`:      61 rows,    8 columns; timepoints: baseline=32; endline=19; midline=10
- `stove_use_geocene_refugee_daily`:    2635 rows,   28 columns; timepoints: endline=1486; baseline=878; midline=271
- `stove_use_geocene_refugee_monitor_days`:    2130 rows,   26 columns; timepoints: endline=965; baseline=718; midline=447
- `survey_host_hh_members`:    1767 rows,   54 columns; timepoints: midline=1029; endline=738
- `survey_host_household`:     513 rows, 2879 columns; timepoints: baseline=200; midline=181; endline=132
- `survey_host_location`:    1029 rows,   40 columns; timepoints: midline=1029
- `survey_host_symptoms`:    1767 rows,   34 columns; timepoints: midline=1029; endline=738
- `survey_refugee_hh_members`:   11967 rows,   57 columns; timepoints: midline=6355; endline=5612
- `survey_refugee_household`:    3284 rows, 3150 columns; timepoints: baseline=1193; midline=1145; endline=946
- `survey_refugee_location`:    6355 rows,   44 columns; timepoints: midline=6355
- `survey_refugee_symptoms`:   11967 rows,   37 columns; timepoints: midline=6355; endline=5612

## Archive handling

- Archive manifest records 135 moved or reviewed files.

## Notes

- Existing outputs outside `4_data/clean_final/` were preserved.
- `4_data_pre_cleaning_changes` was left untouched and treated as an archival snapshot unless a future script explicitly depends on it.
- Exact row and column effects are recorded in `4_data/clean_final/dataset_inventory.csv` and the `timepoint_changes_*.csv` files; timepoint-change files include the parsed `collection_date` used for classification.
