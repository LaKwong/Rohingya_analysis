################################################################################
# RF105 reviewed participant flow by arm and timepoint
#
# Purpose:
#   Create aggregate participant-flow tables and a figure that show household
#   participation by study arm across baseline, midline, and endline.
#
# Inputs:
#   4_data/clean_final/survey_refugee_household.rds
#
# Outputs:
#   Tables:
#     7_tables/RF105_reviewed_YYYYMMDD/table_participant_flow_timepoint_counts.csv
#     7_tables/RF105_reviewed_YYYYMMDD/table_participant_flow_presence_patterns.csv
#     7_tables/RF105_reviewed_YYYYMMDD/table_participant_flow_attrition_summary.csv
#     7_tables/RF105_reviewed_YYYYMMDD/qa/table_participant_flow_deduplication_and_arm_checks.csv
#   Figures:
#     6_figures/RF105_reviewed_YYYYMMDD/fig_participant_flow_by_arm_timepoint.png
#     6_figures/RF105_reviewed_YYYYMMDD/fig_participant_flow_by_arm_timepoint.pdf
#
# Notes:
#   - Participants are household-level participants, identified by fcn_id in the
#     restricted cleaned source file.
#   - Public outputs contain aggregate counts only and do not include fcn_id.
#   - Counts use one deduplicated household record per fcn_id-timepoint, matching
#     the reviewed RF105 helper function make_analysis_population().
################################################################################

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)

  if (length(file_arg) == 1) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg),
                                 winslash = "/", mustWork = FALSE)))
  }

  source_files <- vapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile)) frame$ofile else NA_character_
  }, character(1))
  source_files <- source_files[!is.na(source_files)]

  if (length(source_files) > 0) {
    return(dirname(normalizePath(source_files[[length(source_files)]],
                                 winslash = "/", mustWork = FALSE)))
  }

  getwd()
}

script_dir <- get_script_dir()
config_file_candidates <- c(
  file.path(script_dir, "0_RF105_config_20260805_2213.R"),
  file.path(getwd(), "5_analysis_RF105", "reviewed", "0_RF105_config_20260805_2213.R"),
  file.path(Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = ""),
            "5_analysis_RF105", "reviewed", "0_RF105_config_20260805_2213.R")
)
config_file <- config_file_candidates[file.exists(config_file_candidates)][1]

if (is.na(config_file)) {
  stop("Could not find 0_RF105_config_20260805_2213.R. Set ROHINGYA_ANALYSIS_ROOT.", call. = FALSE)
}

source(config_file)

participant_arm_colors <- c(comparison = "#430154", intervention = "#138B87")

pct_label <- function(numerator, denominator, digits = 1) {
  ifelse(
    is.na(denominator) | denominator <= 0,
    "NA",
    paste0(scales::number(100 * numerator / denominator, accuracy = 10^(-digits)), "%")
  )
}

presence_pattern_label <- function(baseline, midline, endline) {
  dplyr::case_when(
    baseline & midline & endline ~ "baseline_midline_endline",
    baseline & midline & !endline ~ "baseline_midline_only",
    baseline & !midline & endline ~ "baseline_endline_no_midline",
    baseline & !midline & !endline ~ "baseline_only",
    !baseline & midline & endline ~ "midline_endline_no_baseline",
    !baseline & midline & !endline ~ "midline_only_no_baseline",
    !baseline & !midline & endline ~ "endline_only_no_baseline_or_midline",
    TRUE ~ "no_observed_timepoint"
  )
}

make_box_label <- function(timepoint, total_n, baseline_cohort_n = NA_integer_, all_three_n = NA_integer_) {
  if (timepoint == "baseline") {
    return(paste0("Baseline\n", "n = ", total_n))
  }

  if (timepoint == "midline") {
    return(paste0(
      "Midline\n",
      "all observed: n = ", total_n, "\n",
      "baseline cohort: n = ", baseline_cohort_n
    ))
  }

  paste0(
    "Endline\n",
    "all observed: n = ", total_n, "\n",
    "baseline cohort: n = ", baseline_cohort_n, "\n",
    "all 3 waves: n = ", all_three_n
  )
}

survey_raw <- readRDS(file_survey_refugee_household) %>%
  add_rf105_aliases()

survey_population <- make_analysis_population(survey_raw)
survey <- survey_population$all_deduplicated %>%
  clean_timepoint_arm() %>%
  mutate(
    fcn_id_clean = stringr::str_squish(as.character(fcn_id)),
    fcn_id_clean = na_if(fcn_id_clean, "")
  ) %>%
  filter(
    !is.na(fcn_id_clean),
    !is.na(timepoint),
    timepoint %in% timepoint_levels,
    !is.na(study_arm_overall)
  )

timepoint_records <- survey %>%
  distinct(fcn_id_clean, timepoint, study_arm_overall)

participant_presence <- timepoint_records %>%
  group_by(fcn_id_clean) %>%
  summarise(
    baseline = any(timepoint == "baseline"),
    midline = any(timepoint == "midline"),
    endline = any(timepoint == "endline"),
    observed_arm_count = n_distinct(study_arm_overall[!is.na(study_arm_overall)]),
    baseline_arm = first(study_arm_overall[timepoint == "baseline" & !is.na(study_arm_overall)], default = NA),
    first_observed_arm = first(study_arm_overall[!is.na(study_arm_overall)], default = NA),
    .groups = "drop"
  ) %>%
  mutate(
    study_arm_overall = coalesce(baseline_arm, first_observed_arm),
    study_arm_overall = factor(as.character(study_arm_overall), levels = arm_levels),
    presence_pattern = presence_pattern_label(baseline, midline, endline)
  ) %>%
  filter(!is.na(study_arm_overall))

flow_timepoint_counts <- participant_presence %>%
  select(study_arm_overall, baseline, midline, endline) %>%
  tidyr::pivot_longer(
    cols = all_of(timepoint_levels),
    names_to = "timepoint",
    values_to = "present"
  ) %>%
  filter(present) %>%
  count(study_arm_overall, timepoint, name = "n_households") %>%
  complete(
    study_arm_overall = factor(arm_levels, levels = arm_levels),
    timepoint = timepoint_levels,
    fill = list(n_households = 0L)
  ) %>%
  mutate(
    timepoint = factor(timepoint, levels = timepoint_levels, ordered = TRUE),
    timepoint_label = timepoint_label_with_year(timepoint)
  ) %>%
  arrange(study_arm_overall, timepoint)

flow_presence_patterns <- participant_presence %>%
  count(study_arm_overall, presence_pattern, name = "n_households") %>%
  group_by(study_arm_overall) %>%
  mutate(
    pct_within_arm = 100 * n_households / sum(n_households)
  ) %>%
  ungroup() %>%
  arrange(study_arm_overall, desc(n_households), presence_pattern)

flow_attrition_summary <- participant_presence %>%
  group_by(study_arm_overall) %>%
  summarise(
    n_unique_households_observed_any_timepoint = n(),
    n_baseline = sum(baseline),
    n_midline = sum(midline),
    n_endline = sum(endline),
    n_baseline_and_midline = sum(baseline & midline),
    n_baseline_and_endline = sum(baseline & endline),
    n_baseline_midline_endline = sum(baseline & midline & endline),
    n_baseline_not_midline = sum(baseline & !midline),
    n_baseline_midline_not_endline = sum(baseline & midline & !endline),
    n_baseline_endline_no_midline = sum(baseline & !midline & endline),
    n_midline_no_baseline = sum(!baseline & midline),
    n_endline_no_baseline = sum(!baseline & endline),
    pct_baseline_retained_at_midline = 100 * n_baseline_and_midline / n_baseline,
    pct_baseline_retained_at_endline = 100 * n_baseline_and_endline / n_baseline,
    pct_baseline_midline_retained_at_endline = 100 * n_baseline_midline_endline / n_baseline_and_midline,
    .groups = "drop"
  ) %>%
  mutate(
    across(starts_with("pct_"), ~round(.x, 1))
  ) %>%
  arrange(study_arm_overall)

participant_flow_checks <- tibble(
  check = c(
    "cleaned_survey_rows_loaded",
    "deduplicated_household_timepoint_rows_used",
    "duplicate_household_timepoint_rows_collapsed_by_make_analysis_population",
    "participants_with_records_in_more_than_one_arm_after_cleaning",
    "participants_without_arm_after_cleaning"
  ),
  value = c(
    nrow(survey_raw),
    nrow(survey),
    nrow(survey_population$duplicate_records),
    sum(participant_presence$observed_arm_count > 1, na.rm = TRUE),
    sum(is.na(participant_presence$study_arm_overall))
  ),
  notes = c(
    "Rows in survey_refugee_household.rds before participant-flow exclusions.",
    "One household record per fcn_id-timepoint, excluding missing fcn_id/timepoint/arm.",
    "Collapsed deterministically by make_analysis_population(); ID-level details remain restricted in upstream QA.",
    "Aggregate count only; no household identifiers are written by this script.",
    "Aggregate count only; these records are excluded from the flow diagram."
  )
)

write_reviewed_csv(
  flow_timepoint_counts,
  "table_participant_flow_timepoint_counts.csv"
)
write_reviewed_csv(
  flow_presence_patterns,
  "table_participant_flow_presence_patterns.csv"
)
write_reviewed_csv(
  flow_attrition_summary,
  "table_participant_flow_attrition_summary.csv"
)
write_reviewed_csv(
  participant_flow_checks,
  "table_participant_flow_deduplication_and_arm_checks.csv",
  subfolder = "qa"
)

box_data <- flow_attrition_summary %>%
  transmute(
    study_arm_overall,
    arm_label = stringr::str_to_title(as.character(study_arm_overall)),
    baseline_n = n_baseline,
    midline_n = n_midline,
    endline_n = n_endline,
    baseline_midline_n = n_baseline_and_midline,
    baseline_endline_n = n_baseline_and_endline,
    all_three_n = n_baseline_midline_endline
  ) %>%
  tidyr::expand_grid(timepoint = factor(timepoint_levels, levels = timepoint_levels, ordered = TRUE)) %>%
  mutate(
    x = as.numeric(timepoint),
    y = 0.58,
    label = pmap_chr(
      list(as.character(timepoint), baseline_n, midline_n, endline_n,
           baseline_midline_n, baseline_endline_n, all_three_n),
      function(timepoint, baseline_n, midline_n, endline_n,
               baseline_midline_n, baseline_endline_n, all_three_n) {
        if (timepoint == "baseline") {
          make_box_label(timepoint, baseline_n)
        } else if (timepoint == "midline") {
          make_box_label(timepoint, midline_n, baseline_midline_n)
        } else {
          make_box_label(timepoint, endline_n, baseline_endline_n, all_three_n)
        }
      }
    ),
    xmin = x - 0.31,
    xmax = x + 0.31,
    ymin = y - 0.16,
    ymax = y + 0.16
  )

arrow_data <- flow_attrition_summary %>%
  transmute(
    study_arm_overall,
    arm_label = stringr::str_to_title(as.character(study_arm_overall))
  ) %>%
  tidyr::expand_grid(
    transition = c("baseline_to_midline", "midline_to_endline")
  ) %>%
  mutate(
    x = if_else(transition == "baseline_to_midline", 1.32, 2.32),
    xend = if_else(transition == "baseline_to_midline", 1.68, 2.68),
    y = 0.58,
    yend = 0.58
  )

label_data <- flow_attrition_summary %>%
  transmute(
    study_arm_overall,
    arm_label = stringr::str_to_title(as.character(study_arm_overall)),
    baseline_n = n_baseline,
    baseline_midline_n = n_baseline_and_midline,
    all_three_n = n_baseline_midline_endline,
    baseline_not_midline_n = n_baseline_not_midline,
    baseline_midline_not_endline_n = n_baseline_midline_not_endline,
    baseline_endline_no_midline_n = n_baseline_endline_no_midline
  ) %>%
  mutate(
    baseline_midline_pct = pct_label(baseline_midline_n, baseline_n),
    all_three_pct = pct_label(all_three_n, baseline_midline_n)
  ) %>%
  select(
    study_arm_overall, arm_label,
    baseline_midline_n, all_three_n, baseline_not_midline_n,
    baseline_midline_not_endline_n, baseline_endline_no_midline_n,
    baseline_midline_pct, all_three_pct
  ) %>%
  tidyr::pivot_longer(
    cols = c(
      baseline_midline_n,
      baseline_not_midline_n,
      all_three_n,
      baseline_midline_not_endline_n,
      baseline_endline_no_midline_n
    ),
    names_to = "flow_component",
    values_to = "n"
  ) %>%
  mutate(
    x = case_when(
      flow_component %in% c("baseline_midline_n", "baseline_not_midline_n") ~ 1.5,
      TRUE ~ 2.5
    ),
    y = case_when(
      flow_component %in% c("baseline_midline_n", "all_three_n") ~ 0.86,
      flow_component == "baseline_endline_no_midline_n" ~ 0.18,
      TRUE ~ 0.30
    ),
    label = case_when(
      flow_component == "baseline_midline_n" ~ paste0(
        "Baseline households observed at midline\n",
        "n = ", n, " (", baseline_midline_pct, " of baseline)"
      ),
      flow_component == "baseline_not_midline_n" ~ paste0(
        "Baseline households not observed at midline\n",
        "n = ", n
      ),
      flow_component == "all_three_n" ~ paste0(
        "Baseline+midline households observed at endline\n",
        "n = ", n, " (", all_three_pct, " of baseline+midline)"
      ),
      flow_component == "baseline_midline_not_endline_n" ~ paste0(
        "Baseline+midline households not observed at endline\n",
        "n = ", n
      ),
      flow_component == "baseline_endline_no_midline_n" ~ paste0(
        "Baseline households observed at endline but not midline\n",
        "n = ", n
      ),
      TRUE ~ paste0("n = ", n)
    )
  )

participant_flow_plot <- ggplot() +
  geom_segment(
    data = arrow_data,
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = grid::arrow(length = grid::unit(0.18, "in"), type = "closed"),
    linewidth = 0.8,
    color = "grey35"
  ) +
  geom_rect(
    data = box_data,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = study_arm_overall),
    color = "grey20",
    linewidth = 0.45,
    alpha = 0.92
  ) +
  geom_text(
    data = box_data,
    aes(x = x, y = y, label = label),
    color = "white",
    fontface = "bold",
    lineheight = 0.92,
    size = 3.25
  ) +
  geom_label(
    data = label_data,
    aes(x = x, y = y, label = label),
    linewidth = 0.18,
    label.padding = grid::unit(0.12, "lines"),
    fill = "white",
    color = "grey20",
    lineheight = 0.9,
    size = 2.7
  ) +
  facet_wrap(~arm_label, ncol = 1) +
  scale_fill_manual(values = participant_arm_colors, guide = "none") +
  scale_x_continuous(
    breaks = seq_along(timepoint_levels),
    labels = timepoint_label_with_year_levels,
    limits = c(0.55, 3.45),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  coord_cartesian(ylim = c(0.08, 0.96), clip = "off") +
  labs(
    title = "Household participant flow by study arm and survey wave",
    subtitle = "Counts are deduplicated household-level participants; follow-up arm is aligned to baseline arm when available.",
    x = NULL,
    y = NULL,
    caption = "Baseline cohort labels show attrition relevant to baseline-midline and baseline-midline-endline analyses."
  ) +
  theme_bw(base_size = 10) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "grey92", color = "grey35"),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9),
    plot.caption = element_text(size = 8, color = "grey35", hjust = 0),
    plot.margin = margin(10, 18, 10, 18)
  )

save_reviewed_plot(
  participant_flow_plot,
  "fig_participant_flow_by_arm_timepoint.png",
  width = 10,
  height = 7.2
)
save_reviewed_plot(
  participant_flow_plot,
  "fig_participant_flow_by_arm_timepoint.pdf",
  width = 10,
  height = 7.2
)

message("Participant-flow tables and figure complete.")