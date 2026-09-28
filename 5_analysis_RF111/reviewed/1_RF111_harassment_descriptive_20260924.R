# RF111 descriptive analysis of fuel-procurement-related harassment and violence
#
# Inputs:
#   - 4_data/clean_final/survey_refugee_household.rds
#   - baseline and midline XLSForms declared in the configuration file
# Outputs:
#   - disclosure-controlled tables in 7_tables/RF111_harassment_20260924/
#   - one publication figure in 6_figures/RF111_harassment_20260924/
#   - unsuppressed aggregate diagnostics in 8_restricted/
#
# Unit: respondent-reported household-level occurrence among households that
# reported the relevant demographic procured the relevant fuel.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(readxl)
  library(ggplot2)
  library(digest)
  library(scales)
})

source(file.path(
  "5_analysis_RF111", "reviewed", "0_RF111_harassment_config_20260924.R"
))

required_inputs <- c(
  input_household_rds,
  input_baseline_xlsform,
  input_midline_xlsform
)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0L) {
  stop("Missing required input(s): ", paste(missing_inputs, collapse = ", "))
}

dir.create(output_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_restricted_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_qa_dir, recursive = TRUE, showWarnings = FALSE)

legacy_public_qa_dir <- file.path(output_table_dir, "qa")
if (dir.exists(legacy_public_qa_dir)) {
  legacy_archive <- tempfile("legacy_public_qa_", tmpdir = output_restricted_dir)
  if (!file.rename(legacy_public_qa_dir, legacy_archive)) {
    stop(
      "Could not move the legacy unsuppressed public QA directory into restricted storage.",
      call. = FALSE
    )
  }
}

write_public <- function(x, filename) {
  readr::write_csv(x, file.path(output_table_dir, filename), na = "")
}

write_restricted <- function(x, filename) {
  readr::write_csv(x, file.path(output_restricted_dir, filename), na = "")
}

write_qa <- function(x, filename) {
  readr::write_csv(x, file.path(output_qa_dir, filename), na = "")
}

as_numeric_code <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

wilson_interval <- function(numerator, denominator, conf_level = 0.95) {
  z <- qnorm(1 - (1 - conf_level) / 2)
  p <- ifelse(denominator > 0, numerator / denominator, NA_real_)
  adjustment <- 1 + z^2 / denominator
  center <- (p + z^2 / (2 * denominator)) / adjustment
  half_width <- z * sqrt(
    p * (1 - p) / denominator + z^2 / (4 * denominator^2)
  ) / adjustment
  tibble(
    proportion = p,
    percent = 100 * p,
    ci_low = 100 * pmax(0, center - half_width),
    ci_high = 100 * pmin(1, center + half_width)
  )
}

add_wilson <- function(x) {
  bind_cols(
    x,
    wilson_interval(x$numerator, x$denominator)
  )
}

suppress_event_cells <- function(x) {
  x %>%
    mutate(
      suppressed = denominator < public_cell_minimum |
        numerator < public_cell_minimum,
      denominator_public = if_else(
        denominator < public_cell_minimum,
        NA_integer_, as.integer(denominator)
      ),
      numerator_public = if_else(
        suppressed, NA_integer_, as.integer(numerator)
      ),
      numerator_display = if_else(
        suppressed,
        paste0("<", public_cell_minimum),
        as.character(numerator)
      ),
      percent = if_else(suppressed, NA_real_, percent),
      ci_low = if_else(suppressed, NA_real_, ci_low),
      ci_high = if_else(suppressed, NA_real_, ci_high)
    ) %>%
    select(-numerator, -denominator, -proportion)
}

suppress_count_cells <- function(x, count_columns) {
  out <- x
  for (column in count_columns) {
    display_column <- paste0(column, "_display")
    value <- as.integer(out[[column]])
    out[[display_column]] <- ifelse(
      value < public_cell_minimum,
      paste0("<", public_cell_minimum),
      as.character(value)
    )
    out[[column]] <- ifelse(value < public_cell_minimum, NA_integer_, value)
  }
  out
}

event_lookup <- tribble(
  ~event, ~event_label, ~category,
  "insult", "Insulted", "Verbal/emotional",
  "belittle", "Belittled", "Verbal/emotional",
  "scare", "Scared or intimidated", "Verbal/emotional",
  "push", "Pushed, shoved, or hair pulled", "Physical",
  "hit", "Hit", "Physical",
  "kick", "Kicked", "Physical",
  "choke", "Choked", "Physical",
  "weapon", "Threatened with a weapon", "Physical",
  "sex_lang", "Sexual comments, jokes, movements, or looks", "Sexual",
  "sex_contact", "Unwanted sexual contact", "Sexual",
  "sex_rumor", "Sexual rumors", "Sexual",
  "clothing_pull", "Clothing pulled", "Sexual",
  "sex_corner", "Cornered for sexual purposes", "Sexual"
)

fuel_lookup <- tribble(
  ~fuel, ~fuel_label,
  "gather_scraps", "Gathering scraps, leaves, or twigs",
  "collect_wood", "Collecting wood",
  "buy_wood", "Buying wood",
  "receive_wood", "Receiving distributed wood",
  "receive_lpg", "Receiving LPG",
  "buy_lpg", "Buying LPG",
  "receive_crh", "Receiving compressed rice husks",
  "buy_crh", "Buying compressed rice husks"
)

demographic_lookup <- tribble(
  ~demographic, ~demographic_label,
  "w", "Women",
  "g", "Girls",
  "m", "Men",
  "b", "Boys"
)

category_levels <- c(
  "Any harassment", "Verbal/emotional", "Physical", "Sexual"
)

read_form_metadata <- function(path, wave) {
  form <- readxl::read_excel(path, sheet = "survey")
  label_column <- names(form)[str_detect(names(form), "^label::English")][1]
  if (is.na(label_column)) {
    label_column <- names(form)[str_detect(names(form), "^label$")][1]
  }
  get_or_na <- function(column) {
    if (column %in% names(form)) form[[column]] else rep(NA_character_, nrow(form))
  }
  tibble(
    wave = wave,
    variable = as.character(get_or_na("name")),
    form_type = as.character(get_or_na("type")),
    form_label = as.character(form[[label_column]]),
    form_relevant = as.character(get_or_na("relevant")),
    form_required = as.character(get_or_na("required"))
  ) %>%
    filter(!is.na(variable), variable != "") %>%
    distinct(wave, variable, .keep_all = TRUE)
}

form_metadata <- bind_rows(
  read_form_metadata(input_baseline_xlsform, "baseline"),
  read_form_metadata(input_midline_xlsform, "midline")
)

variable_map <- crossing(
  wave = c("baseline", "midline"),
  fuel_lookup,
  demographic_lookup,
  event_lookup
) %>%
  mutate(
    suffix = if_else(wave == "midline", "_ever", ""),
    collector_variable = paste0(fuel, "_", demographic, suffix),
    top_level_variable = paste0(event, "_hh", suffix),
    fuel_specific_variable = paste0(fuel, "_", event, "_hh", suffix),
    detailed_variable = paste0(
      fuel, "_", event, "_", demographic, suffix
    )
  )

attach_metadata <- function(map, variable_column, prefix) {
  index <- match(
    paste(map$wave, map[[variable_column]]),
    paste(form_metadata$wave, form_metadata$variable)
  )
  map[[paste0(prefix, "_form_present")]] <- !is.na(index)
  map[[paste0(prefix, "_form_type")]] <- form_metadata$form_type[index]
  map[[paste0(prefix, "_form_label")]] <- form_metadata$form_label[index]
  map[[paste0(prefix, "_form_relevant")]] <- form_metadata$form_relevant[index]
  map[[paste0(prefix, "_form_required")]] <- form_metadata$form_required[index]
  map
}

variable_map <- variable_map %>%
  attach_metadata("collector_variable", "collector") %>%
  attach_metadata("top_level_variable", "top_level") %>%
  attach_metadata("fuel_specific_variable", "fuel_specific") %>%
  attach_metadata("detailed_variable", "detailed")

# The RDS contains multilingual labels that the Windows C locale translates to
# UTF-8 on import. Analysis variables and values used below are ASCII-coded.
survey <- suppressWarnings(readRDS(input_household_rds))

required_data_variables <- unique(c(
  "fcn_id", "timepoint", "study_arm_overall", "harassment_continue",
  variable_map$collector_variable,
  variable_map$top_level_variable,
  variable_map$fuel_specific_variable,
  variable_map$detailed_variable,
  paste0("harassment_who_", setdiff(event_lookup$event, "sex_corner"))
))

variable_map <- variable_map %>%
  mutate(
    collector_data_present = collector_variable %in% names(survey),
    top_level_data_present = top_level_variable %in% names(survey),
    fuel_specific_data_present = fuel_specific_variable %in% names(survey),
    detailed_data_present = detailed_variable %in% names(survey),
    mapping_complete = collector_form_present & top_level_form_present &
      fuel_specific_form_present & detailed_form_present &
      collector_data_present & top_level_data_present &
      fuel_specific_data_present & detailed_data_present
  )

write_qa(
  variable_map %>%
    select(-suffix) %>%
    arrange(wave, fuel, demographic, category, event),
  "table_rf111_variable_map.csv"
)

missing_data_variables <- setdiff(required_data_variables, names(survey))
if (length(missing_data_variables) > 0L) {
  stop(
    "Expected cleaned-data variables are missing: ",
    paste(missing_data_variables, collapse = ", ")
  )
}
if (any(!variable_map$mapping_complete)) {
  stop("At least one expected harassment variable did not map to its XLSForm.")
}

baseline_count_types <- variable_map %>%
  filter(wave == "baseline") %>%
  pull(detailed_form_type)
midline_count_types <- variable_map %>%
  filter(wave == "midline") %>%
  pull(detailed_form_type)
if (!all(str_detect(baseline_count_types, "frequency_times"))) {
  stop("Baseline detailed fields are not consistently frequency categories.")
}
if (!all(midline_count_types == "integer")) {
  stop("Midline detailed fields are not consistently integer counts.")
}

survey_abm <- survey %>%
  filter(timepoint %in% c("baseline", "midline")) %>%
  mutate(
    fcn_id = as.character(fcn_id),
    timepoint = as.character(timepoint),
    study_arm_overall = as.character(study_arm_overall)
  )

duplicate_household_wave_n <- survey_abm %>%
  count(fcn_id, timepoint) %>%
  filter(n > 1L) %>%
  nrow()

make_event_long <- function(wave) {
  wave_data <- survey_abm %>% filter(timepoint == wave)
  wave_map <- variable_map %>% filter(.data$wave == .env$wave)

  map_dfr(seq_len(nrow(wave_map)), function(i) {
    spec <- wave_map[i, ]
    collector_raw <- as_numeric_code(wave_data[[spec$collector_variable]])
    top_raw <- as_numeric_code(wave_data[[spec$top_level_variable]])
    fuel_raw <- as_numeric_code(wave_data[[spec$fuel_specific_variable]])
    count_raw <- as_numeric_code(wave_data[[spec$detailed_variable]])
    module_raw <- as_numeric_code(wave_data$harassment_continue)

    collector <- collector_raw == 1
    module_yes <- module_raw == 1
    valid_count <- !is.na(count_raw) & count_raw >= 0 &
      count_raw == floor(count_raw)

    occurrence <- rep(NA_integer_, nrow(wave_data))
    top_no_valid <- module_yes & collector & top_raw == 0 &
      (is.na(fuel_raw) | fuel_raw == 0) &
      (is.na(count_raw) | count_raw == 0)
    fuel_no_valid <- module_yes & collector & top_raw == 1 &
      fuel_raw == 0 & (is.na(count_raw) | count_raw == 0)
    detailed_valid <- module_yes & collector & top_raw == 1 &
      fuel_raw == 1 & valid_count

    occurrence[top_no_valid] <- 0L
    occurrence[fuel_no_valid] <- 0L
    occurrence[detailed_valid] <- as.integer(count_raw[detailed_valid] > 0)

    tibble(
      fcn_id = wave_data$fcn_id,
      wave = wave,
      study_arm_overall = wave_data$study_arm_overall,
      fuel = spec$fuel,
      fuel_label = spec$fuel_label,
      demographic = spec$demographic,
      demographic_label = spec$demographic_label,
      event = spec$event,
      event_label = spec$event_label,
      category = spec$category,
      module_response = module_raw,
      module_yes = module_yes,
      collector_response = collector_raw,
      collector = collector,
      top_level_response = top_raw,
      fuel_specific_response = fuel_raw,
      reported_frequency = count_raw,
      occurrence = occurrence
    )
  })
}

event_long <- bind_rows(
  make_event_long("baseline"),
  make_event_long("midline")
)

collapse_occurrence <- function(x) {
  if (any(x == 1L, na.rm = TRUE)) {
    1L
  } else if (all(!is.na(x)) && all(x == 0L)) {
    0L
  } else {
    NA_integer_
  }
}

category_household <- event_long %>%
  group_by(
    fcn_id, wave, study_arm_overall, fuel, fuel_label,
    demographic, demographic_label, category
  ) %>%
  summarise(occurrence = collapse_occurrence(occurrence), .groups = "drop")

any_household <- event_long %>%
  group_by(
    fcn_id, wave, study_arm_overall, fuel, fuel_label,
    demographic, demographic_label
  ) %>%
  summarise(occurrence = collapse_occurrence(occurrence), .groups = "drop") %>%
  mutate(category = "Any harassment")

category_household <- bind_rows(any_household, category_household) %>%
  mutate(category = factor(category, levels = category_levels)) %>%
  arrange(wave, fuel, demographic, category)

summarise_occurrence <- function(data, grouping_variables) {
  data %>%
    group_by(across(all_of(grouping_variables))) %>%
    summarise(
      denominator = sum(!is.na(occurrence)),
      numerator = sum(occurrence == 1L, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    add_wilson()
}

type_summary <- summarise_occurrence(
  event_long,
  c(
    "wave", "fuel", "fuel_label", "demographic", "demographic_label",
    "category", "event", "event_label"
  )
) %>%
  arrange(wave, fuel, demographic, category, event)

category_summary <- summarise_occurrence(
  category_household,
  c(
    "wave", "fuel", "fuel_label", "demographic", "demographic_label",
    "category"
  )
) %>%
  arrange(wave, fuel, demographic, category)

arm_summary <- summarise_occurrence(
  category_household,
  c(
    "wave", "study_arm_overall", "fuel", "fuel_label", "demographic",
    "demographic_label", "category"
  )
) %>%
  arrange(wave, study_arm_overall, fuel, demographic, category)

collector_household <- event_long %>%
  distinct(
    fcn_id, wave, study_arm_overall, fuel, fuel_label,
    demographic, demographic_label, collector_response, collector,
    module_response, module_yes
  )

collector_summary <- collector_household %>%
  group_by(wave, fuel, fuel_label, demographic, demographic_label) %>%
  summarise(
    denominator = n(),
    numerator = sum(collector, na.rm = TRUE),
    collector_households_consenting = sum(collector & module_yes, na.rm = TRUE),
    collector_households_missing_indicator = sum(is.na(collector_response)),
    .groups = "drop"
  ) %>%
  add_wilson() %>%
  arrange(wave, fuel, demographic)

write_restricted(
  type_summary,
  "table_rf111_harassment_type_by_fuel_demographic_unsuppressed.csv"
)
write_restricted(
  category_summary,
  "table_rf111_harassment_category_by_fuel_demographic_unsuppressed.csv"
)
write_restricted(
  arm_summary,
  "table_rf111_harassment_category_by_arm_unsuppressed.csv"
)
write_restricted(
  collector_summary,
  "table_rf111_collector_households_unsuppressed.csv"
)

write_public(
  suppress_event_cells(type_summary),
  "table_rf111_harassment_type_by_fuel_demographic.csv"
)
write_public(
  suppress_event_cells(category_summary),
  "table_rf111_harassment_category_by_fuel_demographic.csv"
)
write_public(
  suppress_event_cells(arm_summary),
  "table_rf111_harassment_category_by_arm.csv"
)
write_public(
  suppress_event_cells(collector_summary) %>%
    select(
      -collector_households_consenting,
      -collector_households_missing_indicator
    ),
  "table_rf111_collector_households.csv"
)

frequency_distribution <- event_long %>%
  filter(occurrence == 1L, reported_frequency > 0) %>%
  mutate(
    frequency_band = case_when(
      wave == "baseline" & reported_frequency == 1 ~ "Once",
      wave == "baseline" & reported_frequency == 2 ~ "Twice",
      wave == "baseline" & reported_frequency == 3 ~ "Three times",
      wave == "baseline" & reported_frequency == 4 ~ "More than three times",
      wave == "midline" & reported_frequency == 1 ~ "1",
      wave == "midline" & reported_frequency == 2 ~ "2",
      wave == "midline" & reported_frequency == 3 ~ "3",
      wave == "midline" & reported_frequency %in% 4:5 ~ "4-5",
      wave == "midline" & reported_frequency >= 6 ~ "6 or more",
      TRUE ~ "Unclassified"
    ),
    measurement = if_else(
      wave == "baseline", "Ordered response category", "Integer count"
    )
  ) %>%
  count(
    wave, fuel, fuel_label, demographic, demographic_label,
    category, frequency_band, measurement,
    name = "positive_item_reports"
  ) %>%
  arrange(wave, fuel, demographic, category, frequency_band)

write_restricted(
  frequency_distribution,
  "table_rf111_event_frequency_distribution_unsuppressed.csv"
)
write_public(
  suppress_count_cells(frequency_distribution, "positive_item_reports"),
  "table_rf111_event_frequency_distribution.csv"
)

midline_data <- survey_abm %>% filter(timepoint == "midline")
perpetrator_long <- map_dfr(event_lookup$event, function(event_name) {
  perpetrator_variable <- paste0("harassment_who_", event_name)
  code <- if (perpetrator_variable %in% names(midline_data)) {
    as_numeric_code(midline_data[[perpetrator_variable]])
  } else {
    rep(NA_real_, nrow(midline_data))
  }
  tibble(
    fcn_id = midline_data$fcn_id,
    event = event_name,
    perpetrator_code = code
  )
})

positive_event_reports <- event_long %>%
  filter(wave == "midline", occurrence == 1L) %>%
  distinct(fcn_id, event, event_label, category) %>%
  left_join(perpetrator_long, by = c("fcn_id", "event")) %>%
  mutate(
    perpetrator = case_when(
      perpetrator_code == 1 ~ "Unknown person",
      perpetrator_code == 2 ~ "Known non-relative",
      perpetrator_code == 3 ~ "Relative outside the household",
      perpetrator_code == 4 ~ "Household member",
      TRUE ~ "Not recorded"
    )
  )

perpetrator_missing_qa <- positive_event_reports %>%
  filter(perpetrator == "Not recorded") %>%
  count(event, event_label, category, name = "positive_event_reports")
write_qa(
  perpetrator_missing_qa,
  "table_rf111_perpetrator_missingness.csv"
)

positive_event_reports_recorded <- positive_event_reports %>%
  filter(perpetrator != "Not recorded")

summarise_perpetrators <- function(data, category_value) {
  data %>%
    count(perpetrator, name = "numerator") %>%
    mutate(
      category = category_value,
      denominator = sum(numerator),
      percent = 100 * numerator / denominator
    ) %>%
    select(category, perpetrator, numerator, denominator, percent)
}

perpetrator_summary <- bind_rows(
  summarise_perpetrators(positive_event_reports_recorded, "All event types"),
  positive_event_reports_recorded %>%
    group_split(category) %>%
    map_dfr(~ summarise_perpetrators(.x, unique(.x$category)))
) %>%
  arrange(category, perpetrator)

write_restricted(
  perpetrator_summary,
  "table_rf111_perpetrator_midline_unsuppressed.csv"
)
write_public(
  suppress_event_cells(
    perpetrator_summary %>%
      mutate(
        proportion = numerator / denominator,
        ci_low = NA_real_,
        ci_high = NA_real_
      )
  ),
  "table_rf111_perpetrator_midline.csv"
)

panel_records <- category_household %>%
  mutate(category = as.character(category)) %>%
  select(
    fcn_id, wave, fuel, fuel_label, demographic, demographic_label,
    category, occurrence
  ) %>%
  pivot_wider(names_from = wave, values_from = occurrence) %>%
  left_join(
    survey_abm %>%
      filter(timepoint == "baseline") %>%
      distinct(fcn_id, study_arm_overall),
    by = "fcn_id"
  ) %>%
  filter(!is.na(baseline), !is.na(midline)) %>%
  mutate(
    transition = case_when(
      baseline == 0L & midline == 0L ~ "No to no",
      baseline == 0L & midline == 1L ~ "No to yes",
      baseline == 1L & midline == 0L ~ "Yes to no (cumulative reversal)",
      baseline == 1L & midline == 1L ~ "Yes to yes"
    )
  )

panel_summary <- panel_records %>%
  group_by(
    study_arm_overall, fuel, fuel_label, demographic,
    demographic_label, category
  ) %>%
  summarise(
    complete_panel_households = n(),
    baseline_yes = sum(baseline == 1L),
    midline_yes = sum(midline == 1L),
    no_to_yes = sum(baseline == 0L & midline == 1L),
    yes_to_no_reversals = sum(baseline == 1L & midline == 0L),
    yes_to_yes = sum(baseline == 1L & midline == 1L),
    .groups = "drop"
  ) %>%
  arrange(study_arm_overall, fuel, demographic, category)

panel_transitions <- panel_records %>%
  count(
    study_arm_overall, fuel, fuel_label, demographic,
    demographic_label, category, transition,
    name = "households"
  ) %>%
  arrange(study_arm_overall, fuel, demographic, category, transition)

write_restricted(
  panel_summary,
  "table_rf111_panel_summary_unsuppressed.csv"
)
write_restricted(
  panel_transitions,
  "table_rf111_panel_transitions_unsuppressed.csv"
)
write_public(
  suppress_count_cells(
    panel_summary,
    c(
      "complete_panel_households", "baseline_yes", "midline_yes",
      "no_to_yes", "yes_to_no_reversals", "yes_to_yes"
    )
  ),
  "table_rf111_panel_summary.csv"
)
write_public(
  suppress_count_cells(panel_transitions, "households"),
  "table_rf111_panel_transitions.csv"
)

module_response_qa <- survey_abm %>%
  transmute(
    wave = timepoint,
    module_response = as_numeric_code(harassment_continue),
    response_label = case_when(
      module_response == 1 ~ "Consented",
      module_response == 0 ~ "Did not consent",
      module_response == 77 ~ "Refused",
      module_response == 99 ~ "Don't know",
      is.na(module_response) ~ "Missing",
      TRUE ~ "Other"
    )
  ) %>%
  count(wave, module_response, response_label, name = "households")

event_response_qa <- event_long %>%
  distinct(fcn_id, wave, event, top_level_response) %>%
  mutate(
    response_label = case_when(
      top_level_response == 1 ~ "Yes",
      top_level_response == 0 ~ "No",
      top_level_response == 77 ~ "Refused",
      top_level_response == 99 ~ "Don't know",
      is.na(top_level_response) ~ "Missing",
      TRUE ~ "Other"
    )
  ) %>%
  count(wave, event, top_level_response, response_label, name = "households")

write_qa(module_response_qa, "table_rf111_module_consent.csv")
write_qa(event_response_qa, "table_rf111_top_level_response_audit.csv")

denominator_qa <- bind_rows(
  type_summary %>% mutate(table = "type"),
  category_summary %>% mutate(table = "category"),
  arm_summary %>% mutate(table = "arm")
) %>%
  transmute(
    table,
    wave,
    fuel,
    demographic,
    category = as.character(category),
    numerator,
    denominator,
    numerator_exceeds_denominator = numerator > denominator,
    ci_outside_0_100 = (!is.na(ci_low) & ci_low < 0) |
      (!is.na(ci_high) & ci_high > 100)
  )
write_qa(denominator_qa, "table_rf111_denominator_checks.csv")

skip_pattern_qa <- tibble(
  check = c(
    "Positive detailed count without collector indicator",
    "Positive detailed count without module consent",
    "Positive detailed count when top-level response is not yes",
    "Positive detailed count when fuel-specific response is not yes",
    "Negative detailed count",
    "Noninteger detailed count",
    "Missing detailed count after positive parent path for a collector",
    "Collector indicator outside 0/1/missing"
  ),
  unit = c(
    "fuel-demographic-event item", "fuel-demographic-event item",
    "fuel-demographic-event item", "fuel-demographic-event item",
    "fuel-demographic-event item", "fuel-demographic-event item",
    "fuel-demographic-event item", "fuel-demographic household item"
  ),
  n_issues = c(
    sum(event_long$reported_frequency > 0 & !event_long$collector, na.rm = TRUE),
    sum(event_long$reported_frequency > 0 & !event_long$module_yes, na.rm = TRUE),
    sum(
      event_long$reported_frequency > 0 &
        event_long$top_level_response != 1,
      na.rm = TRUE
    ),
    sum(
      event_long$reported_frequency > 0 &
        event_long$fuel_specific_response != 1,
      na.rm = TRUE
    ),
    sum(event_long$reported_frequency < 0, na.rm = TRUE),
    sum(
      !is.na(event_long$reported_frequency) &
        event_long$reported_frequency != floor(event_long$reported_frequency),
      na.rm = TRUE
    ),
    sum(
      event_long$module_yes & event_long$collector &
        event_long$top_level_response == 1 &
        event_long$fuel_specific_response == 1 &
        is.na(event_long$reported_frequency),
      na.rm = TRUE
    ),
    collector_household %>%
      filter(
        !is.na(collector_response),
        !collector_response %in% c(0, 1)
      ) %>%
      nrow()
  )
) %>%
  mutate(status = if_else(n_issues == 0L, "PASS", "REVIEW"))
write_qa(skip_pattern_qa, "table_rf111_skip_pattern_checks.csv")

structural_qa <- tibble(
  check = c(
    "Duplicate household-wave records",
    "Incomplete variable-to-XLSForm mappings",
    "Missing required cleaned-data variables",
    "Numerators exceeding applicable denominators",
    "Confidence limits outside 0-100 percent"
  ),
  n_issues = c(
    duplicate_household_wave_n,
    sum(!variable_map$mapping_complete),
    length(missing_data_variables),
    sum(denominator_qa$numerator_exceeds_denominator),
    sum(denominator_qa$ci_outside_0_100)
  )
) %>%
  mutate(status = if_else(n_issues == 0L, "PASS", "FAIL"))
write_qa(structural_qa, "table_rf111_structural_checks.csv")

if (any(structural_qa$status == "FAIL")) {
  stop("A structural QA check failed; inspect the QA output before use.")
}

input_manifest <- tibble(
  input_role = c("cleaned household survey", "baseline XLSForm", "midline XLSForm"),
  path = required_inputs,
  bytes = file.info(required_inputs)$size,
  modified_utc = format(
    as.POSIXct(file.info(required_inputs)$mtime, tz = "UTC"),
    "%Y-%m-%dT%H:%M:%SZ"
  ),
  sha256 = map_chr(required_inputs, ~ digest::digest(file = .x, algo = "sha256"))
)
write_qa(input_manifest, "input_manifest.csv")

main_values <- category_summary %>%
  filter(
    fuel %in% c("collect_wood", "gather_scraps"),
    category == "Any harassment"
  ) %>%
  mutate(
    result_key = paste(wave, fuel, demographic, "any", sep = "__"),
    result_text = sprintf(
      "%d/%d (%.1f%%; 95%% CI %.1f-%.1f)",
      numerator, denominator, percent, ci_low, ci_high
    )
  ) %>%
  select(
    result_key, wave, fuel, fuel_label, demographic, demographic_label,
    category, numerator, denominator, percent, ci_low, ci_high, result_text
  )

perpetrator_values <- perpetrator_summary %>%
  filter(category == "All event types") %>%
  mutate(
    result_key = paste("midline", "perpetrator", str_replace_all(
      str_to_lower(perpetrator), "[^a-z0-9]+", "_"
    ), sep = "__"),
    wave = "midline",
    fuel = NA_character_,
    fuel_label = "Any fuel-specific positive report",
    demographic = NA_character_,
    demographic_label = "Any demographic",
    result_text = sprintf(
      "%d/%d (%.1f%%)", numerator, denominator, percent
    ),
    ci_low = NA_real_,
    ci_high = NA_real_
  ) %>%
  rename(category_detail = perpetrator) %>%
  mutate(category = paste("Perpetrator:", category_detail)) %>%
  select(
    result_key, wave, fuel, fuel_label, demographic, demographic_label,
    category, numerator, denominator, percent, ci_low, ci_high, result_text
  )

manuscript_values <- bind_rows(main_values, perpetrator_values)
write_qa(manuscript_values, "table_rf111_manuscript_values.csv")

figure_data <- category_summary %>%
  filter(
    fuel %in% c("collect_wood", "gather_scraps"),
    category %in% c("Any harassment", "Verbal/emotional", "Physical"),
    denominator >= public_cell_minimum,
    numerator >= public_cell_minimum
  ) %>%
  mutate(
    wave = factor(wave, levels = c("baseline", "midline"),
                  labels = c("Baseline", "Midline")),
    fuel_label = factor(
      fuel_label,
      levels = c("Collecting wood", "Gathering scraps, leaves, or twigs")
    ),
    demographic_label = factor(
      demographic_label,
      levels = c("Boys", "Men", "Girls", "Women")
    ),
    category = factor(
      as.character(category),
      levels = c("Any harassment", "Verbal/emotional", "Physical")
    )
  )

harassment_figure <- ggplot(
  figure_data,
  aes(
    x = percent, y = demographic_label,
    colour = category, shape = category
  )
) +
  geom_errorbar(
    aes(xmin = ci_low, xmax = ci_high),
    orientation = "y",
    width = 0.12,
    linewidth = 0.55,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(size = 2.2, position = position_dodge(width = 0.55)) +
  facet_grid(wave ~ fuel_label) +
  scale_colour_manual(
    values = c(
      "Any harassment" = "#1a80bb",
      "Verbal/emotional" = "#009E73",
      "Physical" = "#CC79A7"
    )
  ) +
  scale_shape_manual(values = c(16, 17, 15)) +
  scale_x_continuous(
    labels = label_number(suffix = "%", accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Fuel-procurement-related harassment reported for collector households",
    subtitle = paste(
      "Household-level occurrence among households reporting that the demographic procured the fuel;",
      "Wilson 95% confidence intervals"
    ),
    x = "Collector households reporting the event category",
    y = NULL,
    colour = NULL,
    shape = NULL,
    caption = paste(
      "Fuels and demographic groups are nonexclusive. Estimates with an event numerator or",
      "applicable denominator below five are omitted.\nThe cumulative outcome is descriptive, not an LPG effect estimate."
    )
  ) +
  theme_minimal(base_size = 10.5) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9.5),
    plot.caption = element_text(size = 8, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.text = element_text(face = "bold", size = 9),
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(
    output_figure_dir,
    "fig_rf111_harassment_by_fuel_demographic.png"
  ),
  plot = harassment_figure,
  width = 10.2,
  height = 7.2,
  units = "in",
  dpi = 320,
  bg = "white"
)
ggsave(
  filename = file.path(
    output_figure_dir,
    "fig_rf111_harassment_by_fuel_demographic.tiff"
  ),
  plot = harassment_figure,
  width = 10.2,
  height = 7.2,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

renv_output <- capture.output(renv_state <- renv::status())
writeLines(
  c(
    paste("Analysis version:", analysis_version),
    paste("Run time UTC:", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")),
    "",
    "renv status:",
    renv_output,
    "",
    "sessionInfo():",
    capture.output(sessionInfo())
  ),
  file.path(output_qa_dir, "session_info.txt")
)

message("RF111 harassment analysis completed successfully.")
message("Public tables: ", normalizePath(output_table_dir, winslash = "/"))
message("Figure: ", normalizePath(output_figure_dir, winslash = "/"))
message("Restricted aggregates: ", normalizePath(output_restricted_dir, winslash = "/"))
