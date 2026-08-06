# PM2.5 PATS+ folder-vs-timestamp-window timepoint audit

Generated: 2026-07-16 12:48:02 PDT

This audit compares the timepoint implied by the raw PM2.5 PATS+ folder name against the timepoint implied by parsed collection dates using the corrected study windows.

Timestamp-window rules used here:

- Baseline: 2019-08-01 through 2020-03-31.
- Midline: 2020-08-01 through 2021-03-31.
- Endline: 2022-04-01 through 2022-08-31.

Detailed file-level records needing review are in `docs/pm25_pats_folder_year_timepoint_mismatch_audit.csv`.

Scope note: this audit covers PATS+ files that feed the final PM2.5 outputs. HAPEX personal PM2.5 files remain excluded from the final pipeline and are documented separately in `4_data/clean_final/imported_raw/hapex_exclusion_manifest.csv`.

## Summary by audit status

- `match`: 448 files
- `timestamp_outside_defined_windows_all_rows`: 104 files

## Summary by raw folder and audit status

- `baseline_2019_2020_sensor_folder` / `match`: 229 files
- `endline_2022_sensor_folder` / `match`: 111 files
- `endline_2022_sensor_folder` / `timestamp_outside_defined_windows_all_rows`: 104 files
- `midline_2021_sensor_folder` / `match`: 108 files

## Interpretation notes

- `timestamp_outside_defined_windows_all_rows` means the file has parsed dates, but all dates fall outside baseline/midline/endline windows.
- `timestamp_outside_defined_windows_some_rows` means only some rows in a file fall outside the defined windows.
- Under the corrected windows, October 2020 PATS+ files in the midline folder now classify as midline.
- Most review burden is now in the endline folder, where some PATS+ files have February/March 2022 collection dates before the April 2022 start of the endline window.
