# Codex Workspace Code Reconciliation - 2026-08-05 21:49

Purpose: document code that was accidentally created under the Codex workspace folder rather than inside the canonical Rohingya_analysis project folder.

Canonical project root:

`G:\My Drive\Coding in r (lakwong@stanford.edu)\Rohingya_analysis`

Stray workspace source folder:

`G:\My Drive\Codex workspace\Projects\Rohingya clean cooking\code`

Action taken:

- Copied all 15 code/helper files from the stray workspace code folder into the project archive for provenance.
- No files were deleted from the Codex workspace folder.
- The copied archive is: `G:\My Drive\Coding in r (lakwong@stanford.edu)\Rohingya_analysis\archive\codex_workspace_code_20260805_2149`.
- The active project code remains the timestamped code listed in `docs\active_code_manifest_20260805_2141.csv`.

Important cleaner decision:

- The stray file `clean_survey_refugee_final.R` is not identical to the active timestamped project cleaner.
- The canonical active refugee survey cleaner is `3_data_cleaning\fixed\clean_survey_refugee_20260805_2141.R`.
- The stray `clean_survey_refugee_final.R` copy is retained only in the archive for provenance.

Audit files:

- `docs\codex_workspace_code_reconciliation_20260805_2149.csv`
- `docs\codex_workspace_cleaner_comparison_20260805_2149.csv`

Verification:

- Active manifest files were checked for references to the Codex workspace code folder and retired script names; no active-reference matches were found.
- Archived files may still contain old paths or old script names because they preserve prior code history.
