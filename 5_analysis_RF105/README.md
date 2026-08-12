# RF105 analysis folder organization

This folder separates inactive historical materials from the current reproducible RF105 workflow.

- `reviewed/`: active timestamped RF105 analysis scripts. Use this folder for the current reproducible workflow.
- `original/`: historical RF105 analysis scripts retained for reference only; the active workflow does not source these files.
- `6_PM_analysis/archive/`: inactive PM2.5 scripts retained for reference only. The active PM2.5 analysis code is in `reviewed/`.
- `7_deforestation_analysis/archive/`: inactive deforestation scripts retained for reference only.
- `archive/`: other files not used by the current workflow.

Run the active RF105 workflow from the `Rohingya_analysis` project root with:

```r
source("5_analysis_RF105/reviewed/00_run_RF105_20260805_2213.R")
```

Or from a shell:

```powershell
& "C:/Program Files/R/R-4.5.3/bin/Rscript.exe" "5_analysis_RF105/reviewed/00_run_RF105_20260805_2213.R"
```

The active code writes shareable tables to `7_tables/RF105_reviewed_YYYYMMDD/`, restricted ID-level/record-level tables to `8_restricted/RF105_reviewed_YYYYMMDD/`, and figures to `6_figures/RF105_reviewed_YYYYMMDD/`. It uses `4_data/clean_final` as the default cleaned data source. If running from another working directory, set `ROHINGYA_ANALYSIS_ROOT` first.

Public clean-data reruns can set `RF105_CLEAN_DATA_DIR` to a de-identified/shareable clean-data folder. In that mode, private allocation-master reconciliation is skipped by default because public pseudonymous IDs are not designed to match the private allocation workbook. To run private allocation QA, use the private clean data and set `RF105_RUN_PRIVATE_ALLOCATION_QA=true` with the private workbook available under `2_data_raw/`.

Cleanup notes from 2026-08-12:

- `reviewed/3_descriptive_outcomes_20260805_2213.R` is the canonical descriptive workflow. The temporary companion script `3.1_descriptive_requested_outcomes_20260812.R` was incorporated into that file and archived under `reviewed/archive/code_cleanup_20260812/` for provenance.
- Public-table restricted-field quarantine is an explicit step in `reviewed/00_run_RF105_20260805_2213.R`; sourcing `0_RF105_config_20260805_2213.R` defines the quarantine helper but does not move files by itself.
- The top-level `Makefile` uses `RSCRIPT ?= Rscript`. On Windows, run `make RSCRIPT="C:/Program Files/R/R-4.5.3/bin/Rscript.exe" rf105-reviewed` if Rscript is not on PATH.
- The child physical-health sample-size diagnostic is embedded in `reviewed/3_descriptive_outcomes_20260805_2213.R` near the health outcomes section; the former standalone `6.1_child_physical_health_sample_size_diagnostic_20260807_0828.R` script was archived under `reviewed/archive/code_cleanup_20260812/` for provenance.
