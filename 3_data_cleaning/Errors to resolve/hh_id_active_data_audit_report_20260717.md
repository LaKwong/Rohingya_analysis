# Active data household ID audit

> Note: survey identifier auditing is superseded by survey_unique_id_active_audit_report_20260717.md, which audits hh_id for HOST survey data and cn_id for Rohingya refugee survey data.

Run date: 20260717

Scope audited:
- Active household-level survey datasets in `4_data/clean_final`
- Imported raw household survey datasets in `4_data/clean_final/imported_raw` when they still contain a final `hh_id` column
- Active PM/geocene repeated-measure datasets in `4_data/clean_final` with an `hh_id` column

Generated correction/audit CSVs were excluded from the data audit. The host survey section was updated after revising the host import/cleaning scripts to preserve raw `hh_id` as `hh_id_original` and derive final `hh_id` from `house_id` or `serial_id`.

## Output files

- `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/3_data_cleaning/Errors to resolve/hh_id_active_survey_audit_summary_20260717.csv`
- `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/3_data_cleaning/Errors to resolve/hh_id_active_survey_problem_rows_20260717.csv`
- `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/3_data_cleaning/Errors to resolve/hh_id_active_survey_duplicate_groups_20260717.csv`
- `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/3_data_cleaning/Errors to resolve/hh_id_active_measurement_audit_summary_20260717.csv`
- `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/4_data/clean_final/survey_host_household_hh_id_derivation_audit.csv`

## Active final datasets needing cleaning review

### `4_data/clean_final/survey_refugee_household.rds`

- Missing `hh_id`: 344 rows
- Placeholder `hh_id`: 24 rows, including `0` and `00`
- Malformed `hh_id`: 99 rows with punctuation/spacing/other illegal characters
- Component mismatches / likely wrong `hh_id`: 154 rows
- Duplicate `hh_id` within timepoint: 5 groups
- Raw files tied to missing/duplicate rows: baseline master, midline v86-v94 files, and endline v113/v115/v116 files. See duplicate/problem CSVs for row-level source context.

Duplicate timepoint groups:
- endline `0`: 4 rows, raw file `rohingya_fuel_v116.csv`
- endline `4epp11277012`: 2 rows, raw files `rohingya_fuel_v115.csv`; `rohingya_fuel_v116.csv`
- endline `4GPP5181051`: 2 rows, raw files `rohingya_fuel_v113.csv`; `rohingya_fuel_v116.csv`
- midline `0`: 17 rows, raw file `rohingya_fuel_v86_endline_Rohingya.csv`
- midline `00`: 2 rows, raw file `rohingya_fuel_v86_endline_Rohingya.csv`

Examples of likely wrong `hh_id` values from the final refugee household dataset:
- `4E184708PP 15_348` should align with component-derived `4EPP 15184708_348`
- `4E179029Pp 15_398` should align with component-derived `4EPp 15179086_398`
- `4E184706PP 15_346` should align with component-derived `4EPP 15184706_346`
- `18B279773L ,17_6358` should align with component-derived `18BL 17279773_6358`
- Midline placeholder IDs such as `0` and `00` should be replaced using `camp_id`, `block_id`, `subblock_id`, and `fcn_id`

Likely cleaning script: `3_data_cleaning/fixed/clean_survey_refugee_final.R` and its sourced helper `3_data_cleaning/fixed/legacy_refugee_manual_corrections.R`.

### `4_data/clean_final/survey_host_household.rds`

- Missing `hh_id`: 239 rows after baseline HOST assignments and high-confidence endline matching.
- Remaining unresolved rows: 58 endline rows from `rohingya_fuel_v119_host.csv` and 181 midline rows from `rohingya_fuel_v95_endline_Rohingya_host.csv`.
- Malformed `hh_id`: 0 rows after deriving and normalizing host IDs to `T0####`.
- Duplicate `hh_id` within timepoint: 0 groups.
- Dataset-level duplicate `hh_id` groups: 74, expected from assigning endline records to their matched baseline HOST household IDs.
- Row-level host problem rows in `hh_id_active_survey_problem_rows_20260717.csv`: 239 rows, all `missing_hh_id`.

Baseline/endline HOST `hh_id` cleaning now used by `3_data_cleaning/fixed/clean_survey_host_final.R`:
- Baseline row 98 was relabeled from duplicate derived ID `T03700` to `T09001`.
- Baseline rows 35, 36, 37, and 179 had missing derived IDs and were assigned `T09002`, `T09003`, `T09004`, and `T09005`, respectively.
- These generated baseline IDs have `hh_id_analysis_note = "hh_id made up for purposes of analysis"`.
- High-confidence endline matches: 74 rows assigned from baseline HOST records using identifying fields; these have `hh_id_source = "matched_baseline_host_endline"`.
- Unresolved endline matches: 58 rows requiring manual review.
- Match diagnostics are in `host_endline_missing_hh_id_candidate_matches_20260717.csv`, `host_endline_hh_id_accepted_matches_20260717.csv`, and `host_endline_hh_id_unresolved_candidate_matches_20260717.csv`.
- Row-level derivation details are in `4_data/clean_final/survey_host_household_hh_id_derivation_audit.csv`.
### `4_data/clean_final/pm25_pats_refugee_ambient.rds`

- Missing `hh_id`: 139,456 rows, all rows
- This may be intentional for ambient/school/mosque site files because `ambient_site_id` is populated and `hh_id` is set to `NA` in `clean_pm_pats_refugee_final.R`.
- Raw source examples: outdoor PATS+ files such as `107_PM07761V_20220407_0_10DD12_Mosque.csv` and `11_PM07761Z_20220514_I_4EPP11_School.csv`.

Likely cleaning script: `3_data_cleaning/fixed/clean_pm_pats_refugee_final.R`. Decide whether ambient output should keep `hh_id = NA` or remove/rename the column to avoid being interpreted as household-level data.

### `4_data/clean_final/pm25_pats_refugee_indoor.rds`

- Malformed/too-short `hh_id`: 3,413 rows
- Problem value observed: `8wDIx`
- Duplicate `hh_id` values are expected because this is repeated-measure PM data.

Likely cleaning script: `3_data_cleaning/fixed/clean_pm_pats_refugee_final.R`.

### `4_data/clean_final/stove_use_geocene_refugee_daily.rds`

- Missing/malformed `hh_id`: 0 rows in the lightweight audit
- Duplicate `hh_id` values are expected because this is daily repeated-measure data.

Likely cleaning script: `3_data_cleaning/fixed/clean_geocene_refugee_final.R`.

### `4_data/clean_final/pm25_pats_refugee_qc_files.rds`

- Missing/malformed `hh_id`: 0 rows in the lightweight audit
- Duplicate `hh_id` values are expected in a QC-file list.

Likely cleaning script: `3_data_cleaning/fixed/clean_pm_pats_refugee_final.R`.

## Imported raw files needing cleaning review

### `4_data/clean_final/imported_raw/survey_refugee_household_raw.rds`

- Missing `hh_id`: 355 rows
- Placeholder `hh_id`: 19 rows
- Malformed `hh_id`: 117 rows
- Component mismatches / likely wrong `hh_id`: 1,094 rows
- Duplicate `hh_id` within timepoint: 5 groups
- Related raw source files include:
  - `2_data_raw/survey_baseline/RohingyaFuelMaster_Corrected_20200419_refugee.csv`
  - `2_data_raw/survey_midline/rohingya_fuel_v86_endline_Rohingya.csv`
  - `2_data_raw/survey_midline/rohingya_fuel_v89_endline_Rohingya.csv`
  - `2_data_raw/survey_midline/rohingya_fuel_v90_endline_Rohingya.csv`
  - `2_data_raw/survey_midline/rohingya_fuel_v92_endline_Rohingya.csv`
  - `2_data_raw/survey_midline/rohingya_fuel_v93_endline_Rohingya.csv`
  - `2_data_raw/survey_midline/rohingya_fuel_v94_endline_Rohingya.csv`
  - `2_data_raw/survey_endline/rohingya_fuel_v113.csv`
  - `2_data_raw/survey_endline/rohingya_fuel_v115.csv`
  - `2_data_raw/survey_endline/rohingya_fuel_v116.csv`

### `4_data/clean_final/imported_raw/survey_host_household_raw.rds`

- The original raw host `hh_id` column has been renamed to `hh_id_original` during import, so this raw object no longer has a final `hh_id` column for the malformed/duplicate `hh_id` audit.
- Final host household IDs are derived in `3_data_cleaning/fixed/clean_survey_host_final.R` from `house_id` first and `serial_id` second.
- The row-level derivation audit is `4_data/clean_final/survey_host_household_hh_id_derivation_audit.csv`.

### `4_data/clean_final/imported_raw/survey_refugee_duplicate_review_raw.rds`

- This is a duplicate-review source file rather than an analysis dataset.
- Malformed `hh_id`: 22 rows with spaces in household IDs, e.g., `5AG57 141284`
- Duplicate `hh_id` within timepoint: 126 groups by design of the duplicate-review file
- Related raw source file: `2_data_raw/survey_endline/rohingya_fuel_v116_duplicates.csv`