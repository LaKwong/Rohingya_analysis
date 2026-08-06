# Code cleanup summary

Generated: 2026-08-05T21:46:19

Active entry points:
- Final refugee cleaning: `1_run_clean_refugee_20260805_2141.R`
- Final host cleaning: `1_run_clean_host_20260805_2141.R`
- RF105 reviewed analysis: `5_analysis_RF105/reviewed/00_run_RF105_20260805_2141.R`

Checks completed:
- Active timestamped R files parse successfully with Rscript.
- Active reviewed/final code search returned no references to legacy code/data paths, old unadjusted-DiD artifacts, retired reviewed/final script names, or `RohingyaFuel_` files.
- Refugee manual correction rules are embedded in the final refugee cleaner; the separate correction helper was archived.

Audit files:
- `docs/code_file_rename_audit_20260805_2141.csv`
- `docs/code_cleanup_archive_audit_20260805_2141.csv`
- `docs/five_analysis_loose_file_archive_audit_20260805_2141.csv`
- `docs/active_code_manifest_20260805_2141.csv`
