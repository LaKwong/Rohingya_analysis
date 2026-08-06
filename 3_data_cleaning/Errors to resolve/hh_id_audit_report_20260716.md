# hh_id audit

Generated: 2026-07-16 14:48:02 PDT

## Scope

- Data scanned: `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/4_data/clean_final`
- Files scanned: `.rds` and `.csv` files with an exact `hh_id` column.
- Related survey subforms without `hh_id` were not flagged; they use `PARENT_KEY`/`KEY` instead.
- Repeated `hh_id` values in PM and daily stove-use time series are expected repeated measures; the actionable duplicate screen is restricted to household survey `hh_id` + `timepoint` repeats.

## Final analysis outputs with actionable issues

- `pm25_pats_refugee_ambient.rds`:      0 missing `hh_id` rows;   0 malformed values;  11 duplicate household-timepoint values;  11 problem-value rows in detailed CSV.
- `pm25_pats_refugee_indoor.rds`: 851544 missing `hh_id` rows;   1 malformed values; 182 duplicate household-timepoint values;   5 problem-value rows in detailed CSV.
- `survey_host_household.rds`:    179 missing `hh_id` rows; 293 malformed values;   0 duplicate household-timepoint values; 294 problem-value rows in detailed CSV.
- `survey_refugee_household.rds`:    344 missing `hh_id` rows; 116 malformed values;   5 duplicate household-timepoint values; 117 problem-value rows in detailed CSV.

## Imported raw/audit files with issues

- `imported_raw/geocene_refugee_no_survey_match.csv`:      0 missing `hh_id` rows;   1 malformed values;   0 duplicate household-timepoint values;   2 problem-value rows in detailed CSV.
- `imported_raw/geocene_refugee_stove_events_derived_raw.rds`:      0 missing `hh_id` rows;   6 malformed values;   0 duplicate household-timepoint values;  12 problem-value rows in detailed CSV.
- `imported_raw/pm25_pats_refugee_raw.rds`: 851544 missing `hh_id` rows;   1 malformed values; 196 duplicate household-timepoint values;   9 problem-value rows in detailed CSV.
- `imported_raw/survey_host_household_raw.rds`:    179 missing `hh_id` rows; 293 malformed values;   0 duplicate household-timepoint values; 294 problem-value rows in detailed CSV.
- `imported_raw/survey_refugee_duplicate_review_raw.rds`:      0 missing `hh_id` rows;   7 malformed values; 126 duplicate household-timepoint values;   7 problem-value rows in detailed CSV.
- `imported_raw/survey_refugee_household_raw.rds`:    355 missing `hh_id` rows; 116 malformed values;   5 duplicate household-timepoint values; 117 problem-value rows in detailed CSV.

## Output files

- Summary: `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/3_data_cleaning/Errors to resolve/hh_id_audit_summary_20260716.csv`
- Problem values: `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/3_data_cleaning/Errors to resolve/hh_id_audit_problem_values_20260716.csv`
- Duplicate keys: `G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/3_data_cleaning/Errors to resolve/hh_id_audit_duplicate_keys_20260716.csv`

## Cleaning-script implications

- Add explicit `hh_id` validation to the active final cleaning workflow before analysis outputs are used.
- Fix PATS+ filename parsing so the household token is captured before optional labels such as QC, School, Mosque/Mosjid, or insufficient-data notes.
- Decide whether ambient PM output should omit `hh_id` or rename it to a site/location identifier.
- Add a documented manual mapping for malformed/missing host and refugee survey household IDs; keep old value, new value, source row/key, and rationale.
- Resolve duplicate refugee household survey `hh_id` + `timepoint` keys before paired/longitudinal analyses.
