# RF111 fuel-procurement-related harassment workflow

This reviewed workflow describes fuel-procurement-related harassment and violence reported in the Rohingya household survey. It is descriptive. It does not estimate an LPG intervention effect, a difference-in-differences contrast, mediation, or another causal parameter.

## Inputs

- `4_data/clean_final/survey_refugee_household.rds`: canonical cleaned household survey.
- `2_data_raw/survey_baseline_survey and data review/rohingya_fuel_v64.xlsx`: baseline XLSForm.
- `2_data_raw/survey_midline_survey/rohingya_fuel_v94_endline_Rohingya.xlsx`: midline XLSForm.

The workflow reads these files and never modifies them. Endline is excluded because its questionnaire did not contain the harassment module.

## Estimand

Collector fields ending in `_w`, `_g`, `_m`, or `_b` identify whether a household reported that women, girls, men, or boys procured a fuel. They are not counts of people. Detailed harassment fields report the number or frequency category of events for that demographic.

For each wave, fuel, demographic, and event, the numerator is the number of collector households with a positive detailed report. The denominator is collector households that consented to the module and had an interpretable response path for that event. Refusals, "don't know" responses, and unresolved skip-pattern contradictions are excluded from the applicable denominator. Results are household-level occurrence among collector households, not individual prevalence.

Fuels and demographic groups are nonexclusive. Their denominators must not be summed or treated as independent populations.

## Run

From a fresh R session using the project `renv`:

```r
source("renv/activate.R")
source("5_analysis_RF111/reviewed/00_run_RF111_harassment_20260924.R")
```

The runner creates dated outputs under:

- `7_tables/RF111_harassment_20260924/`
- `6_figures/RF111_harassment_20260924/`
- `8_restricted/RF111_harassment_20260924/`

Public tables suppress estimates when an event numerator or applicable denominator is below five. Restricted files contain unsuppressed aggregate cells only; they contain no household identifiers.

After the main workflow completes, `2_RF111_harassment_independent_spotcheck_20260924.R` recomputes the baseline and midline headline estimates for men collecting wood using base R and explicit variable names. The run stops unless these values agree exactly with the manuscript-value table.

## Output interpretation

- Wilson 95% confidence intervals accompany occurrence estimates.
- Perpetrator summaries use one household-event-type report as the unit and deduplicate reports spanning multiple fuels or demographic groups.
- Frequency tables are secondary distributions among positive reports. Baseline used ordered response categories; midline used integer counts. Neither wave supplies person-time at risk, so the workflow does not calculate incidence rates.
- Arm-stratified and matched-panel outputs are sensitivity descriptions only. Both forms asked about events since arrival in the camp, the cumulative measure reverses for some matched households, baseline arm imbalance is substantial, and endline did not repeat the module.
