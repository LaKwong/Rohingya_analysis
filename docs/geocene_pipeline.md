# Geocene event pipeline

Run from the project root with `make geocene-reviewed`, or:

```sh
Rscript --vanilla 1_run_geocene.R
```

Required R packages are the existing reviewed-analysis dependencies (tidyverse,
lubridate, here, readxl, janitor, broom, scales), plus gridExtra and Hmisc for the
preserved figures. This target uses the installed R library and does not invoke
package installation or restore the environment. The project's current renv
library must be restored separately before running without `--vanilla`.

## Sources and provenance

Primary inputs are `2_data_raw/geocene_biomass_100_80_5_20` and
`2_data_raw/geocene_lpg_100_80_5_20`; sensitivity inputs use the corresponding
`100_80_5_30` suffix. Only `events_by_mission.csv` supplies events. The two summary
CSVs in each folder verify event counts and durations, including mission totals.
The primary snapshot contains 51,163 events and the sensitivity snapshot 44,955.

Raw imports, original mission names, mission IDs, device IDs, source rows, receipt
date audits, and persistent opaque mission-key crosswalks remain in
`8_restricted/geocene_pipeline`. No source CSV is modified. Public analysis inputs
need only opaque mission keys, pseudonymous household IDs, and cleaned measures.

## Cleaning rules

Fuel is assigned from the source folder. Mission names supply household identity
and arm (`0` intervention, `1` comparison). The household component is camp +
block + subblock + six-digit household number: `10FF34113981` decomposes to
camp `10`, block `F`, subblock `F34`, and fcn_id `113981`. Subblock numeric
suffixes may have one to four digits; the final six digits always form `fcn_id`,
including leading zeros. Camp suffixes in
`8E`/`8W` are retained; `4EPP21...` has camp `4`, block `E`, subblock `PP21`.
Spaces and unambiguous separator
errors are normalized; ambiguous identifiers are not guessed. Run
`Rscript --vanilla 1_data_import/fixed/geocene_metadata_review.R` to prepare
restricted proposals from historical metadata and survey evidence. These are
not applied automatically.

Confirmed corrections belong in `8_restricted/geocene_pipeline/mission_metadata_corrections.csv`.
Required columns are `mission_id`, `fcn_id`, `camp_id`, `block_id`, `subblock_id`,
`study_arm_overall`, `reason`, and `reviewed_by`. The generated template is separate
from the applied correction file. If `reviewed_by` is blank in the user-confirmed
file, the separate application audit records user-supplied provenance and the
correction-file checksum, without editing that file. The template is separate
from the applied correction file. Unresolved identities and conflicting arms stop
publication; affected events are retained in the restricted imported dataset.

Timestamps are converted from their explicit offset to Asia/Dhaka. The entire
exported event duration is attributed to the local start date, even across
midnight. Sub-minute differences from timestamp intervals are recorded in QA;
exported durations remain unchanged. Invalid times or nonpositive durations
stop publication, and duplicate records require review rather than automatic removal.

Geocene baseline is September 1, 2019 through August 31, 2020 (user clarification,
September 7, 2026). Midline is September 1 through December 15, 2020. Endline is
January 1 through August 15, 2022. These Geocene-specific windows do not change
survey or PM timepoint definitions. Other dates require explicit review.

Within each variant, any baseline intervention household-date with LPG use is
recoded to midline for both LPG and biomass. Biomass-only baseline dates remain
baseline. Original timepoint and recoding reason are retained.

Except for user-confirmed broken-probe missions, every event is retained, without
mission-duration or LPG-availability exclusions. Exact mission-ID/name pairs and
reasons are maintained in `8_restricted/geocene_pipeline/mission_exclusions.csv`.
The exclusion applies to both variants before baseline LPG recoding or daily
aggregation; other missions from the same household are retained. Excluded raw
event rows and imported = retained + excluded checks are saved in restricted QA.
A public mission-exclusion summary reports only aggregate counts and the reason
"Probe appears to be broken", without mission or household identifiers.
The primary denominator is one row per household/local start date. Two fuels
recorded on that date still contribute only one household-day. The separate
stove count is the number of recorded fuel types (one or two), not physical devices.

Receipt linkage preserves the earlier survey-round-specific parsing: comparison
baseline receipt dates use day/month/year, intervention midline dates use
month/day/year. The confirmed enrollment-date correction for household 123970
is retained in restricted receipt QA. Multiple conflicting receipt dates become
unknown with an explicit conflict flag; they do not remove events. Post-receipt
analyses require known elapsed days >=0; the 30-day analysis requires >=30.

## Outputs and checks

Cleaned events and household-days are written to
`4_data/clean_final/geocene/<variant>/`; public counterparts use
`4_data/clean_final_public/geocene/<variant>/`. Primary reviewed tables and figures
retain their filenames under the current dated RF105 folders. Sensitivity results
use the `sensitivity_100_80_5_30` subfolder. Household/date-level outputs stay restricted.
The comparison table reports both variants' monitoring and use totals.

The targeted runner reuses existing survey imports and does not rerun survey,
PM, health, or causal analyses. The descriptive script calls the shared primary
Geocene analysis to avoid maintaining a second denominator implementation.

```sh
Rscript --vanilla tests/test_geocene_pipeline.R
Rscript --vanilla tests/test_geocene_pipeline.R --figures
```

Tests cover name parsing, local start dates, midnight crossing, long missions,
concurrent fuels, baseline recoding, missing receipts, duplicate household-days,
and aggregate equivalence after pseudonymization. Figure integration tests use
synthetic data and write only under `8_restricted/geocene_pipeline/tests`.
The real-data runner also checks public/private aggregate equality before publishing.

The reviewed Geocene analysis can be sourced with `chdir = FALSE` from another
working directory when `ROHINGYA_ANALYSIS_ROOT` points to this repository.
Nested helpers and figure modules resolve against that root. For Geocene,
relative `RF105_CLEAN_DATA_DIR` overrides are also project-root-relative;
absolute overrides remain supported. The caller's working directory is unchanged.
Run `Rscript --vanilla tests/test_geocene_external_workdir.R` from the repository
root to test this workflow. This integration test uses existing cleaned inputs
and regenerates primary and sensitivity Geocene outputs, not unrelated analyses.
