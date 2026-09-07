# Rohingya_analysis

Analysis of data from the Elrha-funded study of Rohingya households transitioning from firewood to LPG. Fieldwork began in August 2019.

For the fuel-specific Geocene exports, use `make geocene-reviewed` to rebuild only
Geocene imports, cleaned data, tables, and figures. See [Geocene workflow](docs/geocene_pipeline.md)
for source folders, metadata corrections, denominator rules, and tests.

## Reproducible R environment

This project uses `renv` to record and restore R package versions. From the project root, run:

```r
install.packages("renv") # only needed if renv is not already installed
renv::restore()
```

The project `.Rprofile` sets `RENV_PATHS_LIBRARY_ROOT` and
`RENV_PATHS_LIBRARY_STAGING` to a short local app-data path before activating
`renv`. This keeps installed packages out of the Google Drive project tree and
avoids Windows path-length/sync failures for packages with deep folder trees.

When adding or updating packages, update `DESCRIPTION` and then run:

```r
renv::snapshot(type = "explicit", prompt = FALSE)
```

Analysis scripts should check for required packages but should not call `install.packages()` at runtime.

## Workflow

With GNU Make available:

```sh
make all
```

This restores the `renv` environment, imports and cleans the private data, rebuilds `4_data/clean_final_public`, and runs the reviewed RF105 analysis workflow.

From R or RStudio without Make:

```r
renv::restore()
source("0_script to run all code/00_run_all_import_clean_analyze_20260812.R")
```
