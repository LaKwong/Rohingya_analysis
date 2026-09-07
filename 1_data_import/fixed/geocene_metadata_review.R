# Prepare restricted correction proposals, never apply them automatically.
source(file.path("1_data_import", "fixed", "geocene_pipeline_helpers.R"))
unresolved <- geocene_read_csv(file.path(geocene_private, "unresolved_mission_metadata.csv")) %>% mutate(issue = "unresolved_mission_name")
arm_conflicts <- geocene_read_csv(file.path(geocene_private, "household_arm_conflicts.csv")) %>% mutate(issue = "conflicting_household_study_arm")
unresolved <- bind_rows(unresolved, arm_conflicts) %>% distinct(mission_id, .keep_all = TRUE)
historical_path <- raw_import_path("4_data", "clean_final", "imported_raw", "geocene_refugee_stove_events_derived_raw.rds")
historical <- if (file.exists(historical_path)) as_tibble(readRDS(historical_path)) else tibble()
if (all(c("mission_name", "fcn_id", "hh_id", "study_arm_overall") %in% names(historical))) {
  candidates <- historical %>% transmute(mission_name = str_squish(mission_name),
    candidate_fcn_id = as.character(fcn_id), candidate_hh_id = as.character(hh_id),
    candidate_study_arm_overall = as.character(study_arm_overall)) %>% distinct()
  proposals <- unresolved %>% mutate(mission_name = str_squish(mission_name)) %>%
    left_join(candidates, by = "mission_name", relationship = "many-to-many") %>%
    mutate(proposal_source = if_else(!is.na(candidate_fcn_id), "previous derived metadata; not independently validated", NA_character_))
} else proposals <- unresolved %>% mutate(candidate_fcn_id = NA_character_, candidate_hh_id = NA_character_, candidate_study_arm_overall = NA_character_, proposal_source = NA_character_)
survey <- readRDS(raw_import_path("4_data", "clean_final", "survey_refugee_household.rds"))
proposals <- proposals %>% mutate(candidate_in_survey = !is.na(candidate_fcn_id) & candidate_fcn_id %in% as.character(survey$fcn_id),
  reviewed_by = NA_character_, reason = NA_character_)
survey_support <- as_tibble(survey) %>% mutate(across(any_of(c("fcn_id", "hh_id", "camp_id", "block_id", "subblock_id", "study_arm_overall")), as.character)) %>%
  group_by(fcn_id) %>% summarise(
    survey_household_ids = paste(sort(unique(na.omit(hh_id))), collapse = ";"),
    survey_camp_ids = paste(sort(unique(na.omit(camp_id))), collapse = ";"),
    survey_block_ids = paste(sort(unique(na.omit(block_id))), collapse = ";"),
    survey_subblock_ids = paste(sort(unique(na.omit(subblock_id))), collapse = ";"),
    survey_study_arms = paste(sort(unique(na.omit(study_arm_overall))), collapse = ";"), .groups = "drop")
proposals <- proposals %>% left_join(survey_support, by = c("candidate_fcn_id" = "fcn_id"))
hh_parts <- geocene_parse_household_label(proposals$candidate_hh_id)
proposals <- proposals %>% mutate(candidate_camp_id = hh_parts$camp_id, candidate_block_id = hh_parts$block_id, candidate_subblock_id = hh_parts$subblock_id,
  requires_resolution = case_when(!candidate_in_survey ~ "No matching survey household for historical candidate",
    is.na(candidate_camp_id) | is.na(candidate_subblock_id) ~ "Historical household label is malformed; use survey evidence",
    TRUE ~ "Confirm historical candidate and study arm before applying"))
geocene_write(proposals, file.path(geocene_private, "mission_metadata_proposals_for_review.csv"))
geocene_write(unresolved %>% transmute(mission_id, fcn_id = NA_character_, camp_id = NA_character_, block_id = NA_character_, subblock_id = NA_character_,
  study_arm_overall = NA_character_, reason = NA_character_, reviewed_by = NA_character_), file.path(geocene_private, "mission_metadata_corrections_TEMPLATE.csv"))
cat("Mission records needing review:", n_distinct(proposals$mission_id), "\nMissions with a historical candidate also present in survey:", n_distinct(proposals$mission_id[proposals$candidate_in_survey]), "\n")
