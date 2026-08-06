################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Final refugee household survey clean dataset
################################################################################

if (!exists("clean_final_project_root", mode = "function")) {
  helper_from_root <- file.path("3_data_cleaning", "fixed", "0_clean_helpers_20260805_2141.R")
  if (file.exists(helper_from_root)) {
    source(helper_from_root)
  } else {
    stop("Run from the Rohingya_analysis project root or source 0_clean_helpers_20260805_2141.R first.", call. = FALSE)
  }
}

################################################################################
# Manual correction rules
#
# These rules are embedded here so the final refugee household cleaner is
# self-contained. They document row-level corrections that were ported into the
# final raw-first cleaning workflow and write an audit file under clean_final.
################################################################################
apply_refugee_manual_corrections <- function(data) {
  audit_rows <- list()

  clean_chr <- function(x) {
    x <- trimws(as.character(x))
    x[x %in% c("", "NA", "NaN", "NAN", "NULL", "null")] <- NA_character_
    x
  }

  clean_upper <- function(x) toupper(clean_chr(x))

  normalize_arm <- function(x) {
    x <- clean_upper(x)
    out <- rep(NA_character_, length(x))
    out[x %in% c("1", "3", "7", "PRE-INTERVENTION", "POST-INTERVENTION", "POST-INTERVENTION RD3", "INTERVENTION")] <- "intervention"
    out[x %in% c("2", "6", "8", "INTERVENTION FOLLOW-UP", "COMPARISON RD3", "COMPARISON")] <- "comparison"
    out
  }

  normalize_timepoint <- function(x) {
    x <- clean_upper(x)
    out <- rep(NA_character_, length(x))
    out[x %in% c("BASELINE", "PRE-INTERVENTION", "INTERVENTION", "1", "2")] <- "baseline"
    out[x %in% c("MIDLINE", "POST-INTERVENTION", "INTERVENTION FOLLOW-UP", "3", "6")] <- "midline"
    out[x %in% c("ENDLINE", "POST-INTERVENTION RD3", "COMPARISON RD3", "7", "8")] <- "endline"
    out
  }

  values_equal <- function(a, b) {
    a <- as.character(a)
    b <- as.character(b)
    a[is.na(a)] <- "<NA>"
    b[is.na(b)] <- "<NA>"
    a == b
  }

  add_audit <- function(rule_id, variable, row_index, old_value, corrected_value, note) {
    audit_rows[[length(audit_rows) + 1L]] <<- data.frame(
      rule_id = rule_id,
      variable = variable,
      row_index = row_index,
      fcn_id = if ("fcn_id" %in% names(data) && !is.na(row_index)) as.character(data$fcn_id[row_index]) else NA_character_,
      hh_id = if ("hh_id" %in% names(data) && !is.na(row_index)) as.character(data$hh_id[row_index]) else NA_character_,
      timepoint = if ("timepoint" %in% names(data) && !is.na(row_index)) as.character(data$timepoint[row_index]) else NA_character_,
      study_arm = if ("study_arm" %in% names(data) && !is.na(row_index)) as.character(data$study_arm[row_index]) else NA_character_,
      old_value = as.character(old_value),
      corrected_value = as.character(corrected_value),
      note = note,
      source_script = "the embedded manual-correction rules in this file",
      stringsAsFactors = FALSE
    )
  }

  set_value <- function(col, idx, value, rule_id, note) {
    if (!col %in% names(data)) {
      add_audit(rule_id, col, NA_integer_, NA_character_, as.character(value)[1], paste("Skipped because column is absent.", note))
      return(invisible(NULL))
    }
    idx <- which(idx %in% TRUE)
    if (!length(idx)) return(invisible(NULL))
    if (is.factor(data[[col]])) data[[col]] <<- as.character(data[[col]])
    old <- data[[col]][idx]
    replacement <- if (length(value) == 1L) rep(value, length(idx)) else value
    suppressWarnings(data[[col]][idx] <<- replacement)
    new <- data[[col]][idx]
    changed <- !values_equal(old, new)
    if (any(changed)) {
      for (j in which(changed)) {
        add_audit(rule_id, col, idx[j], old[j], new[j], note)
      }
    }
    invisible(NULL)
  }

  drop_rows <- function(idx, rule_id, note) {
    idx <- which(idx %in% TRUE)
    if (!length(idx)) return(invisible(NULL))
    for (row_idx in idx) {
      add_audit(rule_id, "<row>", row_idx, "present", "removed", note)
    }
    data <<- data[-idx, , drop = FALSE]
    invisible(NULL)
  }

  is_timepoint <- function(value) {
    "timepoint" %in% names(data) & clean_chr(data$timepoint) == value
  }

  is_arm <- function(value) {
    "study_arm" %in% names(data) & clean_chr(data$study_arm) == value
  }

  fcn_in <- function(ids) {
    "fcn_id" %in% names(data) & clean_chr(data$fcn_id) %in% ids
  }

  col_equals <- function(col, value, ignore_case = FALSE) {
    if (!col %in% names(data)) return(rep(FALSE, nrow(data)))
    current <- if (ignore_case) clean_upper(data[[col]]) else clean_chr(data[[col]])
    target <- if (ignore_case) toupper(value) else value
    current %in% target
  }

  col_missing_or <- function(col, values = character(0)) {
    if (!col %in% names(data)) return(rep(FALSE, nrow(data)))
    current <- clean_chr(data[[col]])
    is.na(current) | current %in% values
  }

  set_by_fcn <- function(col, mapping, timepoint, arm = NULL, rule_id, note) {
    idx <- is_timepoint(timepoint) & fcn_in(names(mapping))
    if (!is.null(arm)) idx <- idx & is_arm(arm)
    if (!any(idx)) return(invisible(NULL))
    values <- unname(mapping[clean_chr(data$fcn_id)[idx]])
    set_value(col, idx, values, rule_id, note)
  }

  set_constant <- function(col, ids, value, timepoint, arm = NULL, rule_id, note) {
    mapping <- stats::setNames(rep(value, length(ids)), ids)
    set_by_fcn(col, mapping, timepoint, arm, rule_id, note)
  }

  data$.manual_source_row_number <- if ("raw_source_file" %in% names(data)) {
    ave(seq_len(nrow(data)), data$raw_source_file, FUN = seq_along)
  } else {
    rep(NA_integer_, nrow(data))
  }

  manual_case <- function(source_file, raw_row_number) {
    if (!all(c("raw_source_file", ".manual_source_row_number") %in% names(data))) {
      return(rep(FALSE, nrow(data)))
    }
    clean_chr(data$raw_source_file) == source_file &
      data$.manual_source_row_number %in% raw_row_number
  }

  if (!"study_arm_original" %in% names(data) && "study_arm" %in% names(data)) {
    data$study_arm_original <- as.character(data$study_arm)
  }
  if (!"study_arm" %in% names(data)) data$study_arm <- NA_character_
  if (!"study_arm_overall" %in% names(data)) data$study_arm_overall <- NA_character_
  if (!"timepoint" %in% names(data)) data$timepoint <- NA_character_

  arm_from_overall <- normalize_arm(data$study_arm_overall)
  arm_from_study_arm <- normalize_arm(data$study_arm)
  standardized_arm <- ifelse(!is.na(arm_from_overall), arm_from_overall, arm_from_study_arm)
  idx <- !values_equal(data$study_arm, standardized_arm) & !is.na(standardized_arm)
  set_value("study_arm", idx, standardized_arm[idx], "manual_standardize_study_arm", "Use intervention/comparison study_arm names for manual corrections.")
  idx <- !values_equal(data$study_arm_overall, standardized_arm) & !is.na(standardized_arm)
  set_value("study_arm_overall", idx, standardized_arm[idx], "manual_standardize_study_arm_overall", "Align study_arm_overall with intervention/comparison names.")

  standardized_timepoint <- normalize_timepoint(data$timepoint)
  idx <- is.na(clean_chr(data$timepoint)) & !is.na(standardized_timepoint)
  set_value("timepoint", idx, standardized_timepoint[idx], "manual_standardize_timepoint", "Normalize timepoint labels when needed before applying manual rules.")

  # manual study-arm correction using the new two-arm naming convention.
  set_constant(
    "study_arm",
    c("109334", "106082", "124022", "102768", "106136", "119604", "112172", "115267", "110767"),
    "intervention", "midline", "comparison", "manual_midline_arm_reclassification",
    "manual script changed these from intervention follow-up to post-intervention; expressed here as intervention/comparison."
  )
  set_constant(
    "study_arm_overall",
    c("109334", "106082", "124022", "102768", "106136", "119604", "112172", "115267", "110767"),
    "intervention", "midline", "intervention", "manual_midline_arm_reclassification",
    "Keep study_arm_overall aligned with the reclassified intervention arm."
  )

  # Baseline household listing and duplicate-survey corrections. Cases that
  # previously matched on participant/child names now use non-identifying raw
  # source row numbers from the imported baseline file.
  idx <- manual_case("RohingyaFuelMaster_Corrected_20200419_refugee.csv", 2) &
    is_timepoint("baseline") & is_arm("intervention") &
    col_equals("camp_id", "8W", TRUE) &
    col_equals("block_id", "D", TRUE) &
    col_equals("subblock_id", "I21", TRUE) &
    col_missing_or("fcn_id", c("x", "999999"))
  set_value("fcn_id", idx, "101595", "manual_baseline_fcn_8wdi21_geocene_resolution", "Resolved using geocene_data_but_no_survey_error_fix_fcn_id.xlsx after a discussion with the team to clarify the hh_id.")
  set_value("hh_id", idx, "8WDI21101595", "manual_baseline_hh_id_8wdi21_geocene_resolution", "Resolved using geocene_data_but_no_survey_error_fix_fcn_id.xlsx after a discussion with the team to clarify the hh_id.")

  idx <- manual_case("RohingyaFuelMaster_Corrected_20200419_refugee.csv", 42) &
    is_timepoint("baseline") & is_arm("intervention") &
    col_equals("camp_id", "8W", TRUE) &
    col_equals("block_id", "D", TRUE) &
    col_equals("subblock_id", "I21", TRUE) &
    col_missing_or("fcn_id")
  set_value("fcn_id", idx, "999998", "manual_baseline_fcn_placeholder_002", "Updated after a discussion with the team to clarify the hh_id.")

  enum_num <- if ("enumerator" %in% names(data)) suppressWarnings(as.numeric(as.character(data$enumerator))) else rep(NA_real_, nrow(data))
  drop_rows(is_timepoint("baseline") & is_arm("intervention") & fcn_in("117722") & enum_num == 8, "manual_drop_duplicate_baseline_117722", "Accidentally surveyed twice in baseline.")
  enum_num <- if ("enumerator" %in% names(data)) suppressWarnings(as.numeric(as.character(data$enumerator))) else rep(NA_real_, nrow(data))
  drop_rows(is_timepoint("baseline") & is_arm("intervention") & fcn_in("197654") & enum_num == 1, "manual_drop_duplicate_baseline_197654", "Accidentally surveyed twice in baseline.")
  enum_num <- if ("enumerator" %in% names(data)) suppressWarnings(as.numeric(as.character(data$enumerator))) else rep(NA_real_, nrow(data))
  drop_rows(is_timepoint("baseline") & is_arm("intervention") & fcn_in("100976") & enum_num == 3, "manual_drop_duplicate_baseline_100976", "Accidentally surveyed twice in baseline.")
  enum_num <- if ("enumerator" %in% names(data)) suppressWarnings(as.numeric(as.character(data$enumerator))) else rep(NA_real_, nrow(data))
  drop_rows(is_timepoint("baseline") & is_arm("comparison") & fcn_in("186890") & enum_num == 1, "manual_drop_duplicate_baseline_186890", "Accidentally surveyed twice in baseline.")

  idx <- manual_case("RohingyaFuelMaster_Corrected_20200419_refugee.csv", 201) &
    is_timepoint("baseline") & is_arm("intervention") &
    col_equals("camp_id", "8W", TRUE) &
    col_equals("block_id", "B", TRUE) &
    col_equals("subblock_id", "A13", TRUE)
  set_value("fcn_id", idx, "122063", "manual_baseline_fcn_001", "HH listing correction from manual cleaner.")
  idx <- manual_case("RohingyaFuelMaster_Corrected_20200419_refugee.csv", 1322) &
    is_timepoint("baseline") & is_arm("comparison") &
    col_equals("camp_id", "8E", TRUE) &
    col_equals("block_id", "C", TRUE) &
    col_equals("subblock_id", "B33", TRUE)
  set_value("fcn_id", idx, "114444", "manual_baseline_fcn_002", "HH listing correction from manual cleaner.")

  # Targeted hh_id updates for the baseline fcn corrections above. These replace
  # the old fcn embedded in hh_id without reintroducing the manual blanket rebuild.
  idx <- manual_case("RohingyaFuelMaster_Corrected_20200419_refugee.csv", 201) &
    is_timepoint("baseline") & is_arm("intervention") &
    col_equals("camp_id", "8W", TRUE) &
    col_equals("block_id", "B", TRUE) &
    col_equals("subblock_id", "A13", TRUE)
  set_value("hh_id", idx, "8wBA13122063_1067", "manual_baseline_hh_id_001", "Targeted hh_id update paired with corrected baseline fcn_id.")
  idx <- manual_case("RohingyaFuelMaster_Corrected_20200419_refugee.csv", 1322) &
    is_timepoint("baseline") & is_arm("comparison") &
    col_equals("camp_id", "8E", TRUE) &
    col_equals("block_id", "C", TRUE) &
    col_equals("subblock_id", "B33", TRUE)
  set_value("hh_id", idx, "8ECB33114444_4857", "manual_baseline_hh_id_002", "Targeted hh_id update paired with corrected baseline fcn_id.")

  # Legacy direct-name corrections are intentionally not ported. Name fields are
  # removed from final datasets, and shareable outputs get an additional strict
  # de-identification pass.

  set_by_fcn(
    "collect_wood_forest_start",
    c("600061" = "1-Apr-17", "124870" = "1-Jul-17", "120773" = "1-Jul-18", "124869" = "1-Mar-17", "107198" = "1-Sep-18"),
    "baseline", rule_id = "manual_baseline_collect_wood_start_swap",
    note = "manual correction for swapped firewood collection dates."
  )
  set_by_fcn(
    "collect_wood_forest_stop",
    c("600061" = "1-Aug-17", "124870" = "1-Aug-17", "120773" = "1-Aug-18", "124869" = "1-Aug-17", "107198" = "1-Sep-19"),
    "baseline", rule_id = "manual_baseline_collect_wood_stop_swap",
    note = "manual correction for swapped firewood collection dates; corrected obvious Sep spelling typo from the manual script."
  )

  # Do not port the manual script's blanket baseline hh_id rebuild. The fixed
  # cleaner applies the baseline correction workbook before this function, and
  # that workbook is the authoritative source for corrected baseline hh_id values.
  # Rebuilding every baseline hh_id from components can alter confirmed casing or
  # create malformed duplicates when any component is missing.
  # Midline fcn_id and household-location corrections.
  set_by_fcn(
    "fcn_id",
    c("101841" = "101840", "101016" = "106615", "109663" = "109697", "122066" = "112066", "118211" = "112811", "123825" = "113825", "115717" = "117517", "133506" = "123506", "123970" = "123969", "193748" = "193738", "106850" = "206850", "101096" = "225997", "291236" = "290437"),
    "midline", "intervention", "manual_midline_intervention_fcn", "Manual fcn_id correction from manual cleaner."
  )
  set_by_fcn(
    "fcn_id",
    c("186294" = "285592", "199748" = "299748", "245292" = "145292", "200677" = "200667", "194490" = "194499", "197472" = "197422", "383686" = "283686", "297028" = "207028", "168661" = "451059", "172965" = "172964", "183398" = "283398", "180887" = "180837", "184810" = "184809", "185054" = "185055", "193911" = "185415", "185979" = "185980", "157669" = "177669", "177668" = "188577", "177772" = "157574", "177471" = "173073", "287797" = "278797", "296727" = "296729", "650712" = "295896", "650705" = "295894", "650711" = "295890", "451786" = "166147", "164741" = "164742", "120733" = "120773"),
    "midline", "comparison", "manual_midline_comparison_fcn", "Manual fcn_id correction from manual cleaner."
  )

  set_constant("camp_id", "165336", "5", "midline", rule_id = "manual_midline_camp_165336", note = "Manual camp_id correction from manual cleaner.")
  set_constant("camp_id", c("100959", "100971", "100972", "100976", "100999", "101034", "101038", "101109", "101172", "101239", "101251", "101574", "101618", "101667", "101705", "101723", "101737", "101762", "101777", "106976", "113898", "117152", "117719", "117729", "119902", "122567", "123342", "123677", "123970", "123976", "125795", "274944", "290417", "290439", "290495", "291236", "291525", "296044", "300629"), "8W", "midline", rule_id = "manual_midline_camp_8w", note = "Manual camp_id correction from manual cleaner.")
  set_constant("camp_id", c("114476", "114556", "124614"), "8E", "midline", rule_id = "manual_midline_camp_8e", note = "Manual camp_id correction from manual cleaner.")
  set_constant("camp_id", "115663", "9", "midline", rule_id = "manual_midline_camp_115663", note = "Manual camp_id correction from manual cleaner.")
  set_constant("camp_id", c("115815", "110636", "109334", "193582"), "10", "midline", rule_id = "manual_midline_camp_10", note = "Manual camp_id correction from manual cleaner.")
  set_constant("camp_id", c("196929", "245292"), "18", "midline", rule_id = "manual_midline_camp_18", note = "Manual camp_id correction from manual cleaner.")
  set_constant("block_id", "122063", "b", "midline", rule_id = "manual_midline_block_122063", note = "Manual block_id correction from manual cleaner.")
  set_constant("block_id", "296782", "f", "midline", rule_id = "manual_midline_block_296782", note = "Manual block_id correction from manual cleaner.")
  set_constant("block_id", c("115643", "115646", "115767"), "g", "midline", rule_id = "manual_midline_block_g", note = "Manual block_id correction from manual cleaner.")
  set_constant("subblock_id", "296782", "UU13", "midline", rule_id = "manual_midline_subblock_296782", note = "Manual subblock_id correction from manual cleaner.")
  set_constant("subblock_id", "295872", "UU14", "midline", rule_id = "manual_midline_subblock_295872", note = "Manual subblock_id correction from manual cleaner.")
  set_constant("subblock_id", "286224", "G30", "midline", rule_id = "manual_midline_subblock_286224", note = "Manual subblock_id correction from manual cleaner.")
  set_constant("subblock_id", "187195", "DD22", "midline", rule_id = "manual_midline_subblock_187195", note = "Manual subblock_id correction from manual cleaner.")
  set_constant("subblock_id", "114741", "B33", "midline", rule_id = "manual_midline_subblock_114741", note = "Manual subblock_id correction from manual cleaner.")
  set_constant("subblock_id", c("289608", "123657"), "G29", "midline", rule_id = "manual_midline_subblock_g29", note = "Manual subblock_id correction from manual cleaner.")

  set_by_fcn("collect_wood_forest_start", c("121839" = "Apr 1, 2017", "182703" = "Aug 1, 2017", "110349" = "Feb 1, 2017", "295889" = "Feb 1, 2017", "184266" = "Mar 1, 2017", "109816" = "May 1, 2017", "184565" = "Nov 1, 2017"), "midline", rule_id = "manual_midline_collect_wood_start_swap", note = "manual correction for swapped firewood collection dates.")
  set_by_fcn("collect_wood_forest_stop", c("121839" = "Sep 1, 2017", "182703" = "Oct 1, 2017", "110349" = "Aug 1, 2017", "295889" = "Aug 1, 2017", "184266" = "Sep 1, 2017", "109816" = "Aug 1, 2017", "184565" = "Dec 1, 2017"), "midline", rule_id = "manual_midline_collect_wood_stop_swap", note = "manual correction for swapped firewood collection dates.")

  set_constant("first_receive_lpg", "286053", "Nov 1, 2019", "midline", rule_id = "manual_midline_first_receive_lpg_286053", note = "Manual LPG receipt correction from Geocene review.")
  set_constant("bread_adults_week", c("114537", "120722"), 2, "midline", rule_id = "manual_midline_bread_adults_week", note = "Manual value correction from manual cleaner.")
  set_constant("bread_adults_week", c("286043", "125336"), 3, "midline", rule_id = "manual_midline_bread_adults_week", note = "Manual value correction from manual cleaner.")
  set_constant("bread_adults_week", "111198", 4, "midline", rule_id = "manual_midline_bread_adults_week", note = "Manual value correction from manual cleaner.")
  set_constant("clothing", "193342", 15000, "midline", rule_id = "manual_midline_clothing", note = "Manual value correction from manual cleaner.")
  set_constant("clothing", "120564", 20000, "midline", rule_id = "manual_midline_clothing", note = "Manual value correction from manual cleaner.")
  set_constant("clothing", c("114338", "193130"), 30000, "midline", rule_id = "manual_midline_clothing", note = "Manual value correction from manual cleaner.")
  set_constant("debt", c("116548", "289648"), 0, "midline", rule_id = "manual_midline_debt", note = "Manual value correction from manual cleaner.")
  set_constant("debt", c("122093", "152796"), 3000, "midline", rule_id = "manual_midline_debt", note = "Manual value correction from manual cleaner.")
  set_constant("debt", "152797", 4000, "midline", rule_id = "manual_midline_debt", note = "Manual value correction from manual cleaner.")
  set_constant("debt", "118858", 8000, "midline", rule_id = "manual_midline_debt", note = "Manual value correction from manual cleaner.")
  set_constant("debt", "108549", 13700, "midline", rule_id = "manual_midline_debt", note = "Manual value correction from manual cleaner.")
  set_constant("debt", "123665", 300000, "midline", rule_id = "manual_midline_debt", note = "Manual value correction from manual cleaner.")
  set_constant("drudgery_most_diff", "115756", 4, "midline", rule_id = "manual_midline_drudgery_most_diff", note = "Manual value correction from manual cleaner.")
  set_constant("drudgery_second_most_diff", "116103", 4, "midline", rule_id = "manual_midline_drudgery_second_most_diff", note = "Manual value correction from manual cleaner.")
  set_constant("drudgery_easiest", c("116267", "122489", "102745"), 3, "midline", rule_id = "manual_midline_drudgery_easiest", note = "Manual value correction from manual cleaner.")
  set_constant("fish_adults_week", "106841", 4, "midline", rule_id = "manual_midline_fish_adults_week", note = "Manual value correction from manual cleaner.")
  set_constant("fish_adults_week", "102330", 5, "midline", rule_id = "manual_midline_fish_adults_week", note = "Manual value correction from manual cleaner.")
  set_constant("floor_material", "296044", 5, "midline", rule_id = "manual_midline_floor_material", note = "Manual value correction from manual cleaner.")
  set_constant("flooring_below_stove", c("100959", "101172", "122489", "124325"), 3, "midline", rule_id = "manual_midline_flooring_below_stove", note = "Manual value correction from manual cleaner.")
  set_constant("flooring_below_stove", c("111593", "123012", "122063", "102722", "118250", "102745"), 6, "midline", rule_id = "manual_midline_flooring_below_stove", note = "Manual value correction from manual cleaner.")
  set_constant("flooring_below_stove", c("291525", "106976", "123864"), 8, "midline", rule_id = "manual_midline_flooring_below_stove", note = "Manual value correction from manual cleaner.")
  set_constant("food_cant_afford_2wk", "115741", 0, "midline", rule_id = "manual_midline_food_cant_afford_2wk", note = "Manual value correction from manual cleaner.")
  set_constant("fuel_cant_afford_action", c("116267", "124381"), "8", "midline", rule_id = "manual_midline_fuel_cant_afford_action", note = "Manual value correction from manual cleaner.")
  set_constant("fuel_cant_afford_action", "115741", NA_character_, "midline", rule_id = "manual_midline_fuel_cant_afford_action", note = "Manual value correction from manual cleaner.")
  set_constant("forest_wood_fee", "109334", 100, "midline", rule_id = "manual_midline_forest_wood_fee", note = "Manual value correction from manual cleaner.")
  set_constant("fuel_30_receive_lpg", "106582", 1, "midline", rule_id = "manual_midline_fuel_30_receive_lpg", note = "Manual value correction from manual cleaner.")
  set_constant("happy", "122929", 1, "midline", rule_id = "manual_midline_happy", note = "Manual value correction from manual cleaner.")
  set_constant("happy", c("121962", "119364"), 2, "midline", rule_id = "manual_midline_happy", note = "Manual value correction from manual cleaner.")
  set_constant("happy", c("101723", "111944", "112726", "115771", "117505", "117514", "122241", "123005"), 4, "midline", rule_id = "manual_midline_happy", note = "Manual value correction from manual cleaner.")
  set_constant("income_cash_ngo", "179622", 1500, "midline", rule_id = "manual_midline_income_cash_ngo", note = "Manual value correction from manual cleaner.")
  set_constant("income_cash_ngo", "123568", 9000, "midline", rule_id = "manual_midline_income_cash_ngo", note = "manual comment says -9000 was assumed to be an accidental negative sign.")
  set_constant("income_humanitarian_asst", "111287", 400, "midline", rule_id = "manual_midline_income_humanitarian_asst", note = "Manual value correction from manual cleaner.")
  set_constant("income_humanitarian_asst", "124768", 900, "midline", rule_id = "manual_midline_income_humanitarian_asst", note = "Manual value correction from manual cleaner.")
  set_constant("income_humanitarian_asst", "153470", 1000, "midline", rule_id = "manual_midline_income_humanitarian_asst", note = "Manual value correction from manual cleaner.")
  set_constant("income_own_business", "105366", 30000, "midline", rule_id = "manual_midline_income_own_business", note = "Manual value correction from manual cleaner.")
  set_constant("income_wage_labor", "109673", 6000, "midline", rule_id = "manual_midline_income_wage_labor", note = "Manual value correction from manual cleaner.")
  set_constant("income_wage_labor", "113921", 6500, "midline", rule_id = "manual_midline_income_wage_labor", note = "Manual value correction from manual cleaner.")
  set_constant("income_wage_labor", "109334", 7000, "midline", rule_id = "manual_midline_income_wage_labor", note = "Manual value correction from manual cleaner.")
  set_constant("lpg_cylinder_repair", "122489", "1", "midline", rule_id = "manual_midline_lpg_cylinder_repair", note = "Manual value correction from manual cleaner.")
  set_constant("lpg_repair_costs", "108877", 150, "midline", rule_id = "manual_midline_lpg_repair_costs", note = "Manual value correction from manual cleaner.")
  set_constant("lpg_repair_costs", "291525", 250, "midline", rule_id = "manual_midline_lpg_repair_costs", note = "Manual value correction from manual cleaner.")
  set_constant("lpg_stove_repair", "187831", "1", "midline", rule_id = "manual_midline_lpg_stove_repair", note = "Manual value correction from manual cleaner.")
  set_constant("medical", "120564", 1000, "midline", rule_id = "manual_midline_medical", note = "Manual value correction from manual cleaner.")
  set_constant("medical", "124088", 2000, "midline", rule_id = "manual_midline_medical", note = "Manual value correction from manual cleaner.")
  set_constant("medical", "290455", 3000, "midline", rule_id = "manual_midline_medical", note = "Manual value correction from manual cleaner.")
  set_constant("medical", c("106260", "200851"), 6000, "midline", rule_id = "manual_midline_medical", note = "Manual value correction from manual cleaner.")
  set_constant("medical", "101456", 7000, "midline", rule_id = "manual_midline_medical", note = "Manual value correction from manual cleaner.")
  set_constant("medical", "112638", 13000, "midline", rule_id = "manual_midline_medical", note = "Manual value correction from manual cleaner.")
  set_constant("medical", "108549", 15000, "midline", rule_id = "manual_midline_medical", note = "Manual value correction from manual cleaner.")
  set_constant("medical", "124321", 20000, "midline", rule_id = "manual_midline_medical", note = "Manual value correction from manual cleaner.")
  set_constant("medical", "281201", 30000, "midline", rule_id = "manual_midline_medical", note = "Manual value correction from manual cleaner.")
  set_constant("medical", "292310", 70000, "midline", rule_id = "manual_midline_medical", note = "Manual value correction from manual cleaner.")
  set_constant("medical", "179975", 7000, "midline", rule_id = "manual_midline_medical", note = "Manual value correction from manual cleaner.")
  set_constant("medical", c("106585", "106586", "111597", "112019", "115651", "115661", "115662", "115663", "115678", "115682", "115685", "116450", "119405", "119603", "123571"), NA_real_, "midline", rule_id = "manual_midline_medical", note = "Manual value correction from manual cleaner.")
  set_constant("potatoes_adults_week", c("101274", "109334", "115756"), 5, "midline", rule_id = "manual_midline_potatoes_adults_week", note = "Manual value correction from manual cleaner.")
  set_constant("potatoes_adults_week", c("178204", "289648"), 6, "midline", rule_id = "manual_midline_potatoes_adults_week", note = "Manual value correction from manual cleaner.")
  set_constant("potatoes_adults_week", "120982", 7, "midline", rule_id = "manual_midline_potatoes_adults_week", note = "Manual value correction from manual cleaner.")
  set_constant("shelter", "111327", 20000, "midline", rule_id = "manual_midline_shelter", note = "Manual value correction from manual cleaner.")
  set_constant("shelter", "110693", 30000, "midline", rule_id = "manual_midline_shelter", note = "Manual value correction from manual cleaner.")
  set_constant("spent_total_month", "128915", 3120, "midline", rule_id = "manual_midline_spent_total_month", note = "Manual value correction from manual cleaner.")
  # Direct-name-only manual corrections are omitted from the active cleaner.
  set_constant("time_cooking", c("107012", "107019", "108549", "111470", "111944", "112138", "113921", "115548", "115756", "115771", "115792", "115817", "117514", "123568", "123569", "123570", "123657", "123670", "123845", "123976", "192633", "192730", "193582", "195381", "201161", "289608", "289648"), 3, "midline", rule_id = "manual_midline_time_cooking", note = "Manual value correction from manual cleaner.")
  set_constant("time_harvesting_wood", c("123571", "123665", "201612"), 2, "midline", rule_id = "manual_midline_time_harvesting_wood", note = "Manual value correction from manual cleaner.")
  set_constant("time_harvesting_wood", c("100959", "100971", "100972", "100999", "101034", "101038", "101618", "101705", "102330", "106585", "106586", "108169", "108351", "108549", "109673", "109807", "111470", "111944", "112138", "115371", "115465", "115756", "115771", "115817", "116542", "116707", "116710", "116932", "116985", "116987", "117514", "117943", "119603", "121962", "122092", "122093", "122150", "122241", "122378", "123005", "123568", "123656", "123657", "123677", "123845", "123847", "123976", "193130", "193582", "195381", "289608", "289648", "600019"), 3, "midline", rule_id = "manual_midline_time_harvesting_wood", note = "Manual value correction from manual cleaner.")
  set_constant("traditional_use_yesterday", "115818", 1, "midline", rule_id = "manual_midline_traditional_use_yesterday", note = "Manual value correction from manual cleaner.")
  set_constant("traditional_use_yesterday", "122929", 3, "midline", rule_id = "manual_midline_traditional_use_yesterday", note = "Manual value correction from manual cleaner.")

  # Endline corrections.
  set_constant("study_arm", c("115651", "115661"), "intervention", "endline", rule_id = "manual_endline_arm_reclassification", note = "manual script changed these two households to the intervention endline arm.")
  set_constant("study_arm_overall", c("115651", "115661"), "intervention", "endline", rule_id = "manual_endline_arm_reclassification", note = "Keep study_arm_overall aligned with the intervention endline arm.")
  # These endline fcn_id corrections were decided by examining camp_id,
  # block_id, subblock_id, and names of household head and respondent.
  set_by_fcn(
    "fcn_id",
    c("201677" = "201628", "147235" = "174235", "451023" = "451022"),
    "endline",
    rule_id = "manual_endline_fcn_location_name_review",
    note = "Decision based on camp_id, block_id, subblock_id, household-head name, and respondent name review."
  )
  set_by_fcn("fcn_id", c("101841" = "101840", "108306" = "109306", "108796" = "108799", "111272" = "112172", "115603" = "115643", "123825" = "113825", "124324" = "124321", "191395" = "191359", "207984" = "107984"), "endline", "intervention", "manual_endline_intervention_fcn", "Manual fcn_id correction from manual cleaner.")
  set_by_fcn("fcn_id", c("157669" = "177669", "171094" = "171098", "183398" = "283398", "297872" = "295872", "650711" = "295890", "207984" = "107984"), "endline", "comparison", "manual_endline_comparison_fcn", "Manual fcn_id correction from manual cleaner.")
  set_value("camp_id", is_timepoint("endline") & col_equals("hh_id", "DDH21291234", TRUE), "8W", "manual_endline_camp_ddh21291234", "Manual camp_id correction from manual cleaner.")
  set_constant("camp_id", "115766", "10", "endline", rule_id = "manual_endline_camp_115766", note = "Manual camp_id correction from manual cleaner.")
  set_value("block_id", is_timepoint("endline") & col_equals("block_id", "292302", TRUE), "E", "manual_endline_block_292302", "Manual block_id correction from manual cleaner.")
  if ("subblock_id" %in% names(data)) {
    idx <- is_timepoint("endline") & !is.na(clean_chr(data$subblock_id)) & grepl("\\(", clean_chr(data$subblock_id))
    set_value("subblock_id", idx, sub("\\s*\\(.*$", "", clean_chr(data$subblock_id)[idx]), "manual_endline_subblock_strip_parenthetical", "Strip parenthetical notes from subblock_id as in the manual cleaner.")
  }
  set_constant("subblock_id", "292302", "UU16", "endline", rule_id = "manual_endline_subblock_292302", note = "Manual subblock_id correction from manual cleaner.")
  set_constant("subblock_id", "115663", "g39", "endline", rule_id = "manual_endline_subblock_115663", note = "Manual subblock_id correction from manual cleaner.")
  set_constant("subblock_id", "123976", "I14", "endline", rule_id = "manual_endline_subblock_123976", note = "Manual subblock_id correction from manual cleaner.")
  set_constant("subblock_id", c("650711", "295890", "125333"), "UU14", "endline", rule_id = "manual_endline_subblock_uu14", note = "Manual subblock_id correction from manual cleaner.")
  set_constant("subblock_id", "125336", "B33", "endline", rule_id = "manual_endline_subblock_125336", note = "Manual subblock_id correction from manual cleaner.")
  set_value("blanket", is_timepoint("endline") & col_equals("blanket", "182703", TRUE), 8, "manual_endline_blanket", "Manual value correction from manual cleaner.")
  set_constant("buy_wood_cost_bundle", "302244", 50, "endline", rule_id = "manual_endline_buy_wood_cost_bundle", note = "Manual value correction from manual cleaner.")
  set_constant("buy_wood_cost_month_estimate", "302244", 100, "endline", rule_id = "manual_endline_buy_wood_cost_month_estimate", note = "Manual value correction from manual cleaner.")
  set_constant("chicken_duck_pigeon", "175537", 2, "endline", rule_id = "manual_endline_chicken_duck_pigeon", note = "Manual value correction from manual cleaner.")
  set_constant("child_books", "283059", 10, "endline", rule_id = "manual_endline_child_books", note = "Manual value correction from manual cleaner.")
  set_constant("child_books", "277017", 4, "endline", rule_id = "manual_endline_child_books", note = "Manual value correction from manual cleaner.")
  set_constant("child_books", "207030", 5, "endline", rule_id = "manual_endline_child_books", note = "Manual value correction from manual cleaner.")
  set_constant("collect_wood_when_last", c("175715", "187828"), "", "endline", rule_id = "manual_endline_collect_wood_when_last", note = "Manual value correction from manual cleaner.")
  set_constant("cook_who_w", "249671", 1, "endline", rule_id = "manual_endline_cook_who_w", note = "Manual value correction from manual cleaner.")
  set_constant("debt", "112064", 1500, "endline", rule_id = "manual_endline_debt", note = "Manual value correction from manual cleaner.")
  set_constant("food_source", c("112064", "283008", "277068", "277066", "169327", "280794", "179622", "179621", "147235", "184565", "283077", "175489", "179354", "147887", "180644", "168593", "147235"), 1, "endline", rule_id = "manual_endline_food_source", note = "Manual value correction from manual cleaner.")
  set_constant("healthcare_visits_6mo", "286010", 8, "endline", rule_id = "manual_endline_healthcare_visits_6mo", note = "Manual value correction from manual cleaner.")
  set_constant("healthcare_visits_6mo", "184510", 2, "endline", rule_id = "manual_endline_healthcare_visits_6mo", note = "Manual value correction from manual cleaner.")
  set_constant("hh_size", "122132", 14, "endline", rule_id = "manual_endline_hh_size", note = "Manual value correction from manual cleaner.")
  set_constant("income_home_garden", c("111472", "111470", "111589", "112010"), 0, "endline", rule_id = "manual_endline_income_home_garden", note = "Manual value correction from manual cleaner.")
  set_constant("lpg_willingness_to_pay", "302244", 100, "endline", rule_id = "manual_endline_lpg_willingness_to_pay", note = "Manual value correction from manual cleaner.")
  set_constant("meat_consumption", "278797", 0, "endline", rule_id = "manual_endline_meat_consumption", note = "Manual value correction from manual cleaner.")
  # Direct respondent-name corrections are omitted from the active cleaner.
  set_constant("sleep_fall_asleep_min", c("292314", "174188", "171478", " 186825"), 10, "endline", rule_id = "manual_endline_sleep_fall_asleep_min", note = "Manual value correction from manual cleaner.")
  set_constant("sleep_hours", "278797", 10, "endline", rule_id = "manual_endline_sleep_hours", note = "Manual value correction from manual cleaner.")
  set_constant("sleep_hours", "181279", 7, "endline", rule_id = "manual_endline_sleep_hours", note = "Manual value correction from manual cleaner.")
  set_constant("solar_panel", "106582", 2, "endline", rule_id = "manual_endline_solar_panel", note = "Manual value correction from manual cleaner.")
  set_constant("spent_food", "173639", 3000, "endline", rule_id = "manual_endline_spent_food", note = "Manual value correction from manual cleaner.")
  set_constant("veggies_adults_week", "152849", 3, "endline", rule_id = "manual_endline_veggies_adults_week", note = "Manual value correction from manual cleaner.")
  set_constant("veggies_source", "123571", "2 3", "endline", rule_id = "manual_endline_veggies_source", note = "Manual value correction from manual cleaner.")

  # Enumerator names are not derived in the active cleaner; keep numeric
  # enumerator codes only unless a separate restricted lookup is needed.

  # manual fill: add LPG timing fields to endline when they were not asked.
  fill_endline_from_midline <- function(col) {
    if (!all(c("timepoint", "fcn_id", col) %in% names(data))) return(invisible(NULL))
    donor_idx <- which(is_timepoint("midline") & !is.na(clean_chr(data$fcn_id)) & !is.na(clean_chr(data[[col]])))
    if (!length(donor_idx)) return(invisible(NULL))
    donor_fcn <- clean_chr(data$fcn_id[donor_idx])
    keep <- !duplicated(donor_fcn)
    donor_idx <- donor_idx[keep]
    donor_fcn <- donor_fcn[keep]
    target_idx <- which(is_timepoint("endline") & is.na(clean_chr(data[[col]])) & clean_chr(data$fcn_id) %in% donor_fcn)
    if (!length(target_idx)) return(invisible(NULL))
    replacement <- data[[col]][donor_idx[match(clean_chr(data$fcn_id[target_idx]), donor_fcn)]]
    set_value(col, target_idx, replacement, paste0("manual_endline_fill_", col, "_from_midline"), "Fill missing endline LPG timing fields from matching midline record, as intended in the manual cleaner.")
  }
  fill_endline_from_midline("first_receive_lpg")
  fill_endline_from_midline("first_enrolled_lpg")

  audit <- if (length(audit_rows)) do.call(rbind, audit_rows) else data.frame(stringsAsFactors = FALSE)
  audit_path <- clean_final_path(
    "4_data",
    "clean_final",
    "survey_refugee_household_manual_correction_audit.csv"
  )
  ensure_parent_dir(audit_path)
  write.csv(audit, audit_path, row.names = FALSE, na = "")

  if (".manual_source_row_number" %in% names(data)) {
    data$.manual_source_row_number <- NULL
  }
  data
}


clean_refugee_exclusion_value <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[x %in% c("", "NA", "NAN", "NULL")] <- NA_character_
  x
}

append_refugee_exclusion_reason <- function(existing, reason) {
  ifelse(
    is.na(existing) | existing == "",
    reason,
    paste(existing, reason, sep = ";")
  )
}

write_refugee_household_exclusion_audit <- function(data, row_idx) {
  audit_path <- clean_final_path(
    "4_data",
    "clean_final",
    "survey_refugee_household_exclusion_audit.csv"
  )
  audit <- data.frame(
    row_index = row_idx,
    KEY = if ("KEY" %in% names(data)) data$KEY[row_idx] else NA_character_,
    raw_source_file = if ("raw_source_file" %in% names(data)) data$raw_source_file[row_idx] else NA_character_,
    timepoint = if ("timepoint" %in% names(data)) data$timepoint[row_idx] else NA_character_,
    study_arm = if ("study_arm" %in% names(data)) data$study_arm[row_idx] else NA_character_,
    fcn_id = if ("fcn_id" %in% names(data)) data$fcn_id[row_idx] else NA_character_,
    hh_id = if ("hh_id" %in% names(data)) data$hh_id[row_idx] else NA_character_,
    exclusion_reason = data$refugee_cleaning_exclusion_reason[row_idx],
    stringsAsFactors = FALSE
  )
  ensure_parent_dir(audit_path)
  write.csv(audit, audit_path, row.names = FALSE, na = "")
  audit_path
}
collapse_unique_clean <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NAN", "NULL")] <- NA_character_
  x <- sort(unique(x[!is.na(x)]))

  if (!length(x)) {
    return(NA_character_)
  }

  paste(x, collapse = "; ")
}

clean_refugee_arm_value <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("", "na", "nan", "null")] <- NA_character_
  out <- rep(NA_character_, length(x))

  out[x %in% c("1", "3", "7", "intervention", "post-intervention", "pre-intervention")] <- "intervention"
  out[x %in% c("2", "6", "8", "comparison", "intervention follow-up")] <- "comparison"

  out
}

derive_followup_study_arm_from_baseline <- function(data) {
  required <- c("fcn_id", "timepoint", "study_arm_overall")
  missing_required <- setdiff(required, names(data))
  if (length(missing_required)) {
    stop(
      "Cannot derive follow-up study arm from baseline; missing columns: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  if (!"study_arm" %in% names(data)) {
    data$study_arm <- data$study_arm_overall
  }
  if (!"study_arm_original" %in% names(data)) {
    data$study_arm_original <- data$study_arm
  }
  if (!"study_arm_overall_original" %in% names(data)) {
    data$study_arm_overall_original <- data$study_arm_overall
  }

  data$study_arm <- as.character(data$study_arm)
  data$study_arm_overall <- as.character(data$study_arm_overall)

  fcn_clean <- clean_refugee_exclusion_value(data$fcn_id)
  timepoint_clean <- tolower(trimws(as.character(data$timepoint)))
  arm_clean <- clean_refugee_arm_value(data$study_arm_overall)

  baseline_idx <- which(
    timepoint_clean == "baseline" &
      !is.na(fcn_clean) &
      !is.na(arm_clean)
  )

  baseline_ids <- sort(unique(fcn_clean[baseline_idx]))
  baseline_lookup <- data.frame(
    fcn_id_clean = character(),
    baseline_study_arm_overall = character(),
    baseline_lookup_status = character(),
    baseline_collection_dates = character(),
    baseline_raw_source_files = character(),
    stringsAsFactors = FALSE
  )

  for (fcn in baseline_ids) {
    idx <- baseline_idx[fcn_clean[baseline_idx] == fcn]
    arms <- sort(unique(arm_clean[idx]))
    status <- if (length(arms) == 1L) {
      "valid_baseline_arm"
    } else {
      "conflicting_baseline_arm"
    }

    baseline_lookup <- rbind(
      baseline_lookup,
      data.frame(
        fcn_id_clean = fcn,
        baseline_study_arm_overall = if (length(arms) == 1L) arms else paste(arms, collapse = "; "),
        baseline_lookup_status = status,
        baseline_collection_dates = if ("collection_date" %in% names(data)) collapse_unique_clean(data$collection_date[idx]) else NA_character_,
        baseline_raw_source_files = if ("raw_source_file" %in% names(data)) collapse_unique_clean(data$raw_source_file[idx]) else NA_character_,
        stringsAsFactors = FALSE
      )
    )
  }

  followup_idx <- which(
    timepoint_clean %in% c("midline", "endline") &
      !is.na(fcn_clean)
  )
  lookup_match <- match(fcn_clean[followup_idx], baseline_lookup$fcn_id_clean)
  baseline_arm <- baseline_lookup$baseline_study_arm_overall[lookup_match]
  lookup_status <- baseline_lookup$baseline_lookup_status[lookup_match]
  lookup_status[is.na(lookup_status)] <- "no_valid_baseline_arm"

  valid_lookup <- lookup_status == "valid_baseline_arm" &
    baseline_arm %in% c("comparison", "intervention")

  old_study_arm <- as.character(data$study_arm[followup_idx])
  old_study_arm_overall <- as.character(data$study_arm_overall[followup_idx])
  old_study_arm_overall_clean <- clean_refugee_arm_value(old_study_arm_overall)

  change_logical <- valid_lookup &
    (is.na(old_study_arm_overall_clean) | old_study_arm_overall_clean != baseline_arm)
  change_idx <- followup_idx[change_logical]
  change_baseline_arm <- baseline_arm[change_logical]

  if (length(change_idx)) {
    data$study_arm_overall[change_idx] <- change_baseline_arm
    data$study_arm[change_idx] <- change_baseline_arm
  }

  new_study_arm <- as.character(data$study_arm[followup_idx])
  new_study_arm_overall <- as.character(data$study_arm_overall[followup_idx])
  arm_changed <- old_study_arm_overall != new_study_arm_overall
  arm_changed[is.na(arm_changed)] <- FALSE

  audit <- data.frame(
    row_index = followup_idx,
    fcn_id = data$fcn_id[followup_idx],
    timepoint = data$timepoint[followup_idx],
    collection_date = if ("collection_date" %in% names(data)) data$collection_date[followup_idx] else NA_character_,
    raw_source_file = if ("raw_source_file" %in% names(data)) data$raw_source_file[followup_idx] else NA_character_,
    raw_collection_round = if ("raw_collection_round" %in% names(data)) data$raw_collection_round[followup_idx] else NA_character_,
    raw_survey_version = if ("raw_survey_version" %in% names(data)) data$raw_survey_version[followup_idx] else NA_character_,
    study_arm_old = old_study_arm,
    study_arm_overall_old = old_study_arm_overall,
    baseline_study_arm_overall = baseline_arm,
    study_arm_new = new_study_arm,
    study_arm_overall_new = new_study_arm_overall,
    baseline_lookup_status = lookup_status,
    baseline_collection_dates = baseline_lookup$baseline_collection_dates[lookup_match],
    baseline_raw_source_files = baseline_lookup$baseline_raw_source_files[lookup_match],
    arm_changed_from_imported = arm_changed,
    stringsAsFactors = FALSE
  )

  audit_path <- clean_final_path(
    "4_data",
    "clean_final",
    "survey_refugee_household_baseline_study_arm_audit.csv"
  )
  ensure_parent_dir(audit_path)
  write.csv(audit, audit_path, row.names = FALSE, na = "")

  list(
    data = data,
    audit_path = audit_path,
    n_followup_rows = length(followup_idx),
    n_changed = length(change_idx),
    n_no_valid_baseline_arm = sum(lookup_status == "no_valid_baseline_arm"),
    n_conflicting_baseline_arm = sum(lookup_status == "conflicting_baseline_arm")
  )
}
write_refugee_baseline_endline_no_midline_raw_audit <- function(data, raw_data) {
  required <- c("fcn_id", "timepoint")
  missing_required <- setdiff(required, names(data))
  if (length(missing_required)) {
    stop(
      "Cannot audit baseline/endline households missing midline; missing columns: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  id_values <- function(x) {
    x <- clean_refugee_exclusion_value(x)
    unique(x[!is.na(x)])
  }

  hh_id_core <- function(x) {
    x <- clean_refugee_exclusion_value(x)
    x[!is.na(x)] <- sub("_.*$", "", x[!is.na(x)])
    x
  }

  collapse_col <- function(df, col, idx = seq_len(nrow(df))) {
    if (!col %in% names(df) || !length(idx)) {
      return(NA_character_)
    }
    collapse_unique_clean(df[[col]][idx])
  }

  derive_raw_timepoint <- function(df) {
    imported_timepoint <- if ("timepoint" %in% names(df)) {
      tolower(trimws(as.character(df$timepoint)))
    } else {
      rep(NA_character_, nrow(df))
    }

    candidate_cols <- intersect(
      c("collection_date", "start_date", "SubmissionDate", "starttime", "endtime", "date", "datetime"),
      names(df)
    )
    collection_date <- as.Date(rep(NA_character_, nrow(df)))
    for (col in candidate_cols) {
      parsed_date <- extract_date_any(df[[col]])
      fill <- is.na(collection_date) & !is.na(parsed_date)
      collection_date[fill] <- parsed_date[fill]
    }

    parsed_timepoint <- timepoint_from_date(collection_date)
    ifelse(!is.na(parsed_timepoint), parsed_timepoint, imported_timepoint)
  }

  fcn_clean <- clean_refugee_exclusion_value(data$fcn_id)
  timepoint_clean <- tolower(trimws(as.character(data$timepoint)))
  valid_fcn <- !is.na(fcn_clean)

  baseline_fcn <- unique(fcn_clean[valid_fcn & timepoint_clean == "baseline"])
  endline_fcn <- unique(fcn_clean[valid_fcn & timepoint_clean == "endline"])
  midline_fcn <- unique(fcn_clean[valid_fcn & timepoint_clean == "midline"])
  target_fcn <- sort(setdiff(intersect(baseline_fcn, endline_fcn), midline_fcn))

  raw_refugee <- raw_data
  if ("community" %in% names(raw_refugee)) {
    raw_refugee <- raw_refugee[clean_refugee_exclusion_value(raw_refugee$community) == "REFUGEE", , drop = FALSE]
  }
  if ("study_arm_overall" %in% names(raw_refugee)) {
    raw_arm <- tolower(trimws(as.character(raw_refugee$study_arm_overall)))
    raw_refugee <- raw_refugee[is.na(raw_arm) | !(raw_arm %in% c("host", "host_in_refugee_form")), , drop = FALSE]
  }

  raw_timepoint <- derive_raw_timepoint(raw_refugee)
  raw_midline <- raw_refugee[raw_timepoint == "midline", , drop = FALSE]
  raw_midline_source_files_checked <- collapse_col(raw_midline, "raw_source_file")
  raw_midline_source_paths_checked <- collapse_col(raw_midline, "raw_source_path")

  audit_cols <- c(
    "fcn_id", "cleaned_timepoints_present", "cleaned_row_indices",
    "cleaned_baseline_rows", "cleaned_midline_rows", "cleaned_endline_rows",
    "cleaned_baseline_hh_id", "cleaned_endline_hh_id", "cleaned_hh_id_all",
    "cleaned_baseline_UNHCR_id", "cleaned_endline_UNHCR_id", "cleaned_UNHCR_id_all",
    "cleaned_baseline_raw_source_files", "cleaned_endline_raw_source_files",
    "raw_midline_source_files_checked", "raw_midline_source_paths_checked",
    "raw_midline_fcn_id_match_n", "raw_midline_hh_id_exact_match_n",
    "raw_midline_hh_id_core_match_n", "raw_midline_UNHCR_id_match_n",
    "raw_midline_any_identifier_match_n", "raw_midline_matching_fcn_ids",
    "raw_midline_matching_hh_ids", "raw_midline_matching_UNHCR_ids",
    "raw_midline_matching_source_files", "raw_midline_matching_source_paths", "raw_midline_matching_keys",
    "audit_status", "audit_note"
  )

  empty_audit <- as.data.frame(setNames(replicate(length(audit_cols), character(), simplify = FALSE), audit_cols))

  audit <- empty_audit
  if (length(target_fcn)) {
    audit <- do.call(rbind, lapply(target_fcn, function(fcn) {
      clean_idx <- which(fcn_clean == fcn)
      baseline_idx <- clean_idx[timepoint_clean[clean_idx] == "baseline"]
      midline_idx <- clean_idx[timepoint_clean[clean_idx] == "midline"]
      endline_idx <- clean_idx[timepoint_clean[clean_idx] == "endline"]

      target_hh <- if ("hh_id" %in% names(data)) id_values(data$hh_id[clean_idx]) else character()
      target_hh_core <- if ("hh_id" %in% names(data)) id_values(hh_id_core(data$hh_id[clean_idx])) else character()
      target_unhcr <- if ("UNHCR_id" %in% names(data)) id_values(data$UNHCR_id[clean_idx]) else character()

      raw_match_fcn <- if ("fcn_id" %in% names(raw_midline)) {
        which(clean_refugee_exclusion_value(raw_midline$fcn_id) == fcn)
      } else {
        integer(0)
      }
      raw_match_hh_exact <- if ("hh_id" %in% names(raw_midline) && length(target_hh)) {
        which(clean_refugee_exclusion_value(raw_midline$hh_id) %in% target_hh)
      } else {
        integer(0)
      }
      raw_match_hh_core <- if ("hh_id" %in% names(raw_midline) && length(target_hh_core)) {
        which(hh_id_core(raw_midline$hh_id) %in% target_hh_core)
      } else {
        integer(0)
      }
      raw_match_unhcr <- if ("UNHCR_id" %in% names(raw_midline) && length(target_unhcr)) {
        which(clean_refugee_exclusion_value(raw_midline$UNHCR_id) %in% target_unhcr)
      } else {
        integer(0)
      }
      raw_match_any <- sort(unique(c(raw_match_fcn, raw_match_hh_exact, raw_match_hh_core, raw_match_unhcr)))
      audit_status <- if (length(raw_match_any)) {
        "possible_raw_midline_match_needs_review"
      } else {
        "confirmed_no_raw_midline_match_by_fcn_hh_or_unhcr_id"
      }
      audit_note <- if (length(raw_match_any)) {
        "At least one raw midline row matched this cleaned baseline/endline household by fcn_id, exact hh_id, hh_id before underscore suffix, or UNHCR_id; review before treating midline as missing."
      } else {
        "No raw midline row matched this cleaned baseline/endline household by fcn_id, exact hh_id, hh_id before underscore suffix, or UNHCR_id."
      }

      data.frame(
        fcn_id = fcn,
        cleaned_timepoints_present = collapse_unique_clean(data$timepoint[clean_idx]),
        cleaned_row_indices = paste(clean_idx, collapse = "; "),
        cleaned_baseline_rows = length(baseline_idx),
        cleaned_midline_rows = length(midline_idx),
        cleaned_endline_rows = length(endline_idx),
        cleaned_baseline_hh_id = collapse_col(data, "hh_id", baseline_idx),
        cleaned_endline_hh_id = collapse_col(data, "hh_id", endline_idx),
        cleaned_hh_id_all = collapse_col(data, "hh_id", clean_idx),
        cleaned_baseline_UNHCR_id = collapse_col(data, "UNHCR_id", baseline_idx),
        cleaned_endline_UNHCR_id = collapse_col(data, "UNHCR_id", endline_idx),
        cleaned_UNHCR_id_all = collapse_col(data, "UNHCR_id", clean_idx),
        cleaned_baseline_raw_source_files = collapse_col(data, "raw_source_file", baseline_idx),
        cleaned_endline_raw_source_files = collapse_col(data, "raw_source_file", endline_idx),
        raw_midline_source_files_checked = raw_midline_source_files_checked,
        raw_midline_source_paths_checked = raw_midline_source_paths_checked,
        raw_midline_fcn_id_match_n = length(raw_match_fcn),
        raw_midline_hh_id_exact_match_n = length(raw_match_hh_exact),
        raw_midline_hh_id_core_match_n = length(raw_match_hh_core),
        raw_midline_UNHCR_id_match_n = length(raw_match_unhcr),
        raw_midline_any_identifier_match_n = length(raw_match_any),
        raw_midline_matching_fcn_ids = collapse_col(raw_midline, "fcn_id", raw_match_any),
        raw_midline_matching_hh_ids = collapse_col(raw_midline, "hh_id", raw_match_any),
        raw_midline_matching_UNHCR_ids = collapse_col(raw_midline, "UNHCR_id", raw_match_any),
        raw_midline_matching_source_files = collapse_col(raw_midline, "raw_source_file", raw_match_any),
        raw_midline_matching_source_paths = collapse_col(raw_midline, "raw_source_path", raw_match_any),
        raw_midline_matching_keys = collapse_col(raw_midline, "KEY", raw_match_any),
        audit_status = audit_status,
        audit_note = audit_note,
        stringsAsFactors = FALSE
      )
    }))
  }

  audit_path <- clean_final_path(
    "4_data",
    "clean_final",
    "survey_refugee_baseline_endline_no_midline_raw_check.csv"
  )
  ensure_parent_dir(audit_path)
  write.csv(audit, audit_path, row.names = FALSE, na = "")

  list(
    audit_path = normalizePath(audit_path, winslash = "/", mustWork = TRUE),
    n_households = length(target_fcn),
    n_possible_raw_midline_matches = if (nrow(audit)) sum(audit$audit_status == "possible_raw_midline_match_needs_review") else 0L
  )
}

clean_food_frequency_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

row_sum_food_vars <- function(data, cols, cap_at = NA_real_) {
  cols <- intersect(cols, names(data))
  if (!length(cols)) {
    return(rep(NA_real_, nrow(data)))
  }

  values <- as.data.frame(
    lapply(data[cols], clean_food_frequency_numeric),
    optional = TRUE
  )
  has_any_value <- rowSums(!is.na(values)) > 0
  out <- rowSums(values, na.rm = TRUE)
  out[!has_any_value] <- NA_real_

  if (!is.na(cap_at)) {
    out <- pmin(out, cap_at)
  }

  out
}

row_any_food_group <- function(data, cols) {
  cols <- intersect(cols, names(data))
  if (!length(cols)) {
    return(rep(NA_integer_, nrow(data)))
  }

  values <- as.data.frame(
    lapply(data[cols], clean_food_frequency_numeric),
    optional = TRUE
  )
  has_any_value <- rowSums(!is.na(values)) > 0
  out <- as.integer(rowSums(values > 0, na.rm = TRUE) > 0)
  out[!has_any_value] <- NA_integer_

  out
}

write_refugee_dietary_diversity_audit <- function(data) {
  audit_path <- clean_final_path(
    "4_data",
    "clean_final",
    "survey_refugee_household_dietary_diversity_audit.csv"
  )
  ensure_parent_dir(audit_path)

  audit_data <- data.frame(
    timepoint = if ("timepoint" %in% names(data)) data$timepoint else NA_character_,
    study_arm_overall = if ("study_arm_overall" %in% names(data)) data$study_arm_overall else NA_character_,
    rows = 1L,
    hdds_assume_misc_1_nonmissing = !is.na(data$hdds_assume_misc_1),
    food_diversity_num_groups_nonmissing = !is.na(data$food_diversity_num_groups),
    stringsAsFactors = FALSE
  )

  if (nrow(audit_data)) {
    audit <- aggregate(
      cbind(rows, hdds_assume_misc_1_nonmissing, food_diversity_num_groups_nonmissing) ~ timepoint + study_arm_overall,
      data = audit_data,
      FUN = sum
    )
    audit <- audit[order(audit$timepoint, audit$study_arm_overall), , drop = FALSE]
  } else {
    audit <- data.frame(stringsAsFactors = FALSE)
  }

  write.csv(audit, audit_path, row.names = FALSE, na = "")
  normalizePath(audit_path, winslash = "/", mustWork = TRUE)
}

derive_refugee_dietary_diversity <- function(data) {
  data$hdds_cereals <- row_any_food_group(
    data,
    c("rice_adults_week", "bread_adults_week", "corn_adults_week")
  )
  data$hdds_tubers <- row_any_food_group(data, "potatoes_adults_week")
  data$hdds_pulses <- row_any_food_group(data, "lentils_adults_week")
  data$hdds_eggs <- row_any_food_group(data, "eggs_adults_week")
  data$hdds_milk <- row_any_food_group(data, "dairy_adults_week")
  data$hdds_veggies <- row_any_food_group(data, "veggies_adults_week")
  data$hdds_fruit <- row_any_food_group(data, "fruit_adults_week")
  data$hdds_fish <- row_any_food_group(data, "fish_adults_week")
  data$hdds_meat <- row_any_food_group(
    data,
    c("poultry_adults_week", "goat_sheep_adults_week", "beef_adults_week")
  )
  data$hdds_oil <- row_any_food_group(data, "oil_adults_week")
  data$hdds_sugar <- row_any_food_group(data, "sugar_adults_week")

  hdds_cols <- c(
    "hdds_cereals", "hdds_tubers", "hdds_pulses", "hdds_eggs",
    "hdds_milk", "hdds_veggies", "hdds_fruit", "hdds_fish",
    "hdds_meat", "hdds_oil", "hdds_sugar"
  )
  data$hdds_no_misc <- row_sum_food_vars(data, hdds_cols)
  data$hdds_assume_misc_1 <- ifelse(
    !is.na(data$hdds_no_misc),
    data$hdds_no_misc + 1,
    NA_real_
  )
  data$hdds_recall_basis <- ifelse(
    !is.na(data$hdds_assume_misc_1),
    "past_7_day_food_frequency",
    NA_character_
  )

  data$food_diversity_starch <- row_any_food_group(
    data,
    c("rice_adults_week", "bread_adults_week", "potatoes_adults_week", "corn_adults_week", "lentils_adults_week")
  )
  data$food_diversity_legumes <- row_any_food_group(data, "lentils_adults_week")
  data$food_diversity_dairy <- row_any_food_group(data, "dairy_adults_week")
  data$food_diversity_veggies <- row_any_food_group(data, "veggies_adults_week")
  data$food_diversity_fruit <- row_any_food_group(data, "fruit_adults_week")
  data$food_diversity_meat <- row_any_food_group(
    data,
    c("eggs_adults_week", "fish_adults_week", "poultry_adults_week", "goat_sheep_adults_week", "beef_adults_week")
  )
  data$food_diversity_fat <- row_any_food_group(data, "oil_adults_week")
  data$food_diversity_sugar <- row_any_food_group(data, "sugar_adults_week")

  food_diversity_cols <- c(
    "food_diversity_starch", "food_diversity_legumes", "food_diversity_dairy",
    "food_diversity_veggies", "food_diversity_fruit", "food_diversity_meat",
    "food_diversity_fat", "food_diversity_sugar"
  )
  data$food_diversity_num_groups <- row_sum_food_vars(data, food_diversity_cols)
  data$food_diversity_eight_groups <- ifelse(
    !is.na(data$food_diversity_num_groups),
    as.integer(data$food_diversity_num_groups == 8),
    NA_integer_
  )

  required_timepoints <- c("baseline", "midline", "endline")
  timepoint_counts <- tapply(
    !is.na(data$hdds_assume_misc_1),
    data$timepoint,
    sum,
    na.rm = TRUE
  )
  timepoints_with_hdds <- names(timepoint_counts)[timepoint_counts > 0]
  missing_timepoints <- setdiff(required_timepoints, timepoints_with_hdds)
  if (length(missing_timepoints)) {
    stop(
      "Dietary diversity derivation produced no nonmissing hdds_assume_misc_1 values for: ",
      paste(missing_timepoints, collapse = ", "),
      call. = FALSE
    )
  }

  audit_path <- write_refugee_dietary_diversity_audit(data)
  nonmissing_summary <- paste(
    paste(names(timepoint_counts), as.integer(timepoint_counts), sep = "="),
    collapse = "; "
  )

  list(
    data = data,
    audit_path = audit_path,
    nonmissing_summary = nonmissing_summary
  )
}
dataset_name <- "survey_refugee_household"
source_rel <- "4_data/clean_final/imported_raw/survey_refugee_household_raw.rds"
survey <- read_rds_required(source_rel)
survey_raw_imported <- survey
apply_refugee_household_correction_workbook <- function(data) {
  workbook_rel <- file.path(
    "2_data_raw",
    "survey_baseline_survey and data review",
    "Rohingya HH Data_Correction_Saeed_20210124.xlsx"
  )
  workbook_path <- clean_final_path(workbook_rel)
  if (!file.exists(workbook_path)) {
    stop("Required household correction workbook is missing: ", workbook_path, call. = FALSE)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package `readxl` is required to apply household correction workbook.", call. = FALSE)
  }

  lookup <- as.data.frame(
    readxl::read_excel(
      workbook_path,
      sheet = "Rohingyafuelmaster_hh_data",
      col_types = "text"
    ),
    stringsAsFactors = FALSE
  )

  required <- c(
    "camp_id", "block_id", "subblock_id", "fcn_id", "Rand_nu", "hh_id",
    "name_hh_head", "name_respondent", "target_child_name"
  )
  missing_lookup <- setdiff(required, names(lookup))
  missing_data <- setdiff(required, names(data))
  if (length(missing_lookup) || length(missing_data)) {
    stop(
      "Cannot apply correction workbook; missing columns. ",
      "Workbook: ", paste(missing_lookup, collapse = ", "),
      "; data: ", paste(missing_data, collapse = ", "),
      call. = FALSE
    )
  }

  clean_key_value <- function(x) {
    x <- toupper(trimws(as.character(x)))
    x <- gsub("\\s+", " ", x)
    x[x %in% c("", "NA", "NAN", "NULL")] <- NA_character_
    x
  }

  make_key <- function(df, cols) {
    do.call(
      paste,
      c(lapply(cols, function(col) clean_key_value(df[[col]])), sep = "||")
    )
  }

  match_unique_lookup <- function(df, lookup_df, cols, eligible = rep(TRUE, nrow(df))) {
    data_key <- make_key(df, cols)
    lookup_key <- make_key(lookup_df, cols)
    lookup_counts <- table(lookup_key, useNA = "no")
    unique_lookup_keys <- names(lookup_counts)[lookup_counts == 1L]
    idx <- match(data_key, lookup_key)
    idx[!(data_key %in% unique_lookup_keys) | !eligible] <- NA_integer_
    idx
  }

  primary_key_cols <- c(
    "camp_id", "block_id", "subblock_id", "fcn_id",
    "name_hh_head", "name_respondent", "target_child_name"
  )
  primary_idx <- match_unique_lookup(data, lookup, primary_key_cols)

  fcn_clean <- clean_key_value(data$fcn_id)
  missing_or_placeholder_fcn <- is.na(fcn_clean) | fcn_clean %in% c("X", "0", "00")
  secondary_key_cols <- c(
    "camp_id", "block_id", "subblock_id",
    "name_hh_head", "name_respondent", "target_child_name"
  )
  secondary_idx <- match_unique_lookup(
    data,
    lookup,
    secondary_key_cols,
    eligible = is.na(primary_idx) & missing_or_placeholder_fcn
  )

  lookup_idx <- primary_idx
  lookup_idx[is.na(lookup_idx)] <- secondary_idx[is.na(lookup_idx)]
  matched <- !is.na(lookup_idx)

  corrected <- data
  correction_cols <- c("camp_id", "block_id", "subblock_id", "fcn_id", "Rand_nu", "hh_id")
  audit <- data.frame(
    row_index = which(matched),
    match_rule = ifelse(!is.na(primary_idx[matched]), "primary_full_id_name_key", "secondary_missing_fcn_name_key"),
    raw_source_file = if ("raw_source_file" %in% names(data)) data$raw_source_file[matched] else NA_character_,
    KEY = if ("KEY" %in% names(data)) data$KEY[matched] else NA_character_,
    stringsAsFactors = FALSE
  )

  changed_any <- rep(FALSE, sum(matched))
  for (col in correction_cols) {
    old <- as.character(data[[col]][matched])
    new <- as.character(lookup[[col]][lookup_idx[matched]])
    changed <- ifelse(is.na(old), "<NA>", old) != ifelse(is.na(new), "<NA>", new)
    audit[[paste0(col, "_old")]] <- old
    audit[[paste0(col, "_corrected")]] <- new
    audit[[paste0(col, "_changed")]] <- changed
    changed_any <- changed_any | changed
    corrected[[col]][matched] <- new
  }

  audit <- audit[changed_any, , drop = FALSE]
  audit$correction_workbook <- normalizePath(workbook_path, winslash = "/", mustWork = TRUE)
  audit$correction_sheet <- "Rohingyafuelmaster_hh_data"

  audit_path <- clean_final_path(
    "4_data",
    "clean_final",
    "survey_refugee_household_correction_audit.csv"
  )
  ensure_parent_dir(audit_path)
  write.csv(audit, audit_path, row.names = FALSE, na = "")

  corrected
}


apply_refugee_endline_review_corrections <- function(data, identity_data = data) {
  review_dir <- clean_final_path("2_data_raw", "survey_endline_survey and data review")
  if (!dir.exists(review_dir)) {
    stop("Required endline review folder is missing: ", review_dir, call. = FALSE)
  }
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package `readxl` is required to apply endline review corrections.", call. = FALSE)
  }
  if (nrow(data) != nrow(identity_data)) {
    stop("Endline correction identity data must have the same row count as data.", call. = FALSE)
  }
  if (!"raw_source_file" %in% names(identity_data)) {
    stop("Cannot apply endline review corrections; raw_source_file is missing.", call. = FALSE)
  }

  clean_text <- function(x) {
    x <- trimws(as.character(x))
    x[x %in% c("", "NA", "NaN", "NAN", "NULL", "null")] <- NA_character_
    x
  }

  normalize_id <- function(x) {
    x <- clean_text(x)
    x <- toupper(gsub("\\s+", "", x))
    x[x %in% c("", "NA", "NAN", "NULL")] <- NA_character_
    x
  }

  parse_review_row <- function(x) {
    x <- clean_text(x)
    value <- suppressWarnings(as.numeric(x))
    out <- rep(NA_integer_, length(value))
    ok <- !is.na(value) & is.finite(value) & abs(value - round(value)) < 1e-6
    out[ok] <- as.integer(round(value[ok]))
    out
  }

  get_first_col <- function(df, candidates) {
    exact <- candidates[candidates %in% names(df)]
    if (length(exact)) return(exact[1])
    lower_names <- tolower(names(df))
    for (candidate in tolower(candidates)) {
      idx <- match(candidate, lower_names)
      if (!is.na(idx)) return(names(df)[idx])
    }
    NA_character_
  }

  get_cell <- function(df, col, row) {
    if (is.na(col) || !col %in% names(df)) return(NA_character_)
    clean_text(df[[col]][row])
  }

  valid_review_id <- function(x, label) {
    x <- normalize_id(x)
    !is.na(x) & !(x %in% toupper(label))
  }

  find_data_col <- function(qid) {
    qid <- clean_text(qid)[1]
    if (is.na(qid)) return(NA_character_)
    idx <- match(qid, names(data))
    if (!is.na(idx)) return(names(data)[idx])
    idx <- match(tolower(qid), tolower(names(data)))
    if (!is.na(idx)) return(names(data)[idx])
    NA_character_
  }

  numeric_like <- function(x) {
    x <- clean_text(x)
    !is.na(x) & grepl("^[-+]?[0-9]+(\\.[0-9]+)?$", x)
  }

  review_note_response <- function(value) {
    value <- tolower(clean_text(value))
    if (is.na(value)) return(FALSE)
    grepl(
      paste(
        c(
          "these are fine", "do not show", "is not other", "problem fixed",
          "skip this row", "it is correct", "yes correct", "redo the survey",
          "please check", "sorry for mistake", "one means"
        ),
        collapse = "|"
      ),
      value
    )
  }

  source_row_number <- ave(
    seq_len(nrow(identity_data)),
    identity_data$raw_source_file,
    FUN = seq_along
  )

  audit_rows <- list()
  add_audit <- function(
    workbook, sheet, review_row_index, source_file, raw_row_number, qid, variable,
    match_status, skip_reason, old_value, corrected_value, original_response,
    response, resolution, remarks, change_made, notes
  ) {
    audit_rows[[length(audit_rows) + 1L]] <<- data.frame(
      correction_workbook = normalizePath(workbook, winslash = "/", mustWork = TRUE),
      correction_sheet = sheet,
      review_row_index = review_row_index,
      raw_source_file = source_file,
      raw_row_number = raw_row_number,
      qid = qid,
      variable = variable,
      match_status = match_status,
      skip_reason = skip_reason,
      old_value = old_value,
      corrected_value = corrected_value,
      original_response = original_response,
      response = response,
      resolution = resolution,
      remarks = remarks,
      change_made = change_made,
      notes = notes,
      stringsAsFactors = FALSE
    )
  }

  match_target_rows <- function(source_file, review_row_number, review_hh_id, review_fcn_id) {
    in_source <- identity_data$raw_source_file == source_file
    if (!is.na(review_row_number)) {
      idx <- which(in_source & source_row_number == review_row_number)
      return(idx)
    }

    idx <- which(in_source)
    if (length(idx) && "hh_id" %in% names(identity_data) && valid_review_id(review_hh_id, "hh_id")) {
      idx <- idx[normalize_id(identity_data$hh_id[idx]) == normalize_id(review_hh_id)]
    }
    if (length(idx) && "fcn_id" %in% names(identity_data) && valid_review_id(review_fcn_id, "fcn_id")) {
      idx <- idx[normalize_id(identity_data$fcn_id[idx]) == normalize_id(review_fcn_id)]
    }
    idx
  }

  identity_matches <- function(row_idx, review_hh_id, review_fcn_id) {
    checks <- logical(0)
    if ("hh_id" %in% names(identity_data) && valid_review_id(review_hh_id, "hh_id")) {
      checks <- c(checks, normalize_id(identity_data$hh_id[row_idx]) == normalize_id(review_hh_id))
    }
    if ("fcn_id" %in% names(identity_data) && valid_review_id(review_fcn_id, "fcn_id")) {
      checks <- c(checks, normalize_id(identity_data$fcn_id[row_idx]) == normalize_id(review_fcn_id))
    }
    if (!length(checks)) return(TRUE)
    any(checks, na.rm = TRUE)
  }

  review_files <- list.files(review_dir, pattern = "\\.xlsx$", full.names = TRUE)
  review_files <- review_files[!grepl("^~\\$", basename(review_files))]
  corrected <- data

  for (workbook_path in review_files) {
    workbook_name <- basename(workbook_path)
    version <- regmatches(workbook_name, regexpr("v[0-9]+", workbook_name, ignore.case = TRUE))
    version <- if (length(version)) tolower(version) else NA_character_
    source_file <- if (!is.na(version)) paste0("rohingya_fuel_", version, ".csv") else NA_character_

    sheets <- readxl::excel_sheets(workbook_path)
    if (identical(version, "v119")) {
      add_audit(
        workbook_path, NA_character_, NA_integer_, source_file, NA_integer_, NA_character_,
        NA_character_, "skipped", "host_review_workbook_not_applied_to_refugee_cleaner",
        NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
        NA_character_, NA_character_, NA_character_
      )
      next
    }

    if (!"review" %in% sheets) {
      note_sheet <- sheets[1]
      notes <- as.data.frame(
        readxl::read_excel(workbook_path, sheet = note_sheet, col_types = "text", .name_repair = "unique"),
        stringsAsFactors = FALSE
      )
      if (!nrow(notes)) {
        add_audit(
          workbook_path, note_sheet, NA_integer_, source_file, NA_integer_, NA_character_,
          NA_character_, "skipped", "no_structured_review_sheet",
          NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
          NA_character_, NA_character_, NA_character_
        )
        next
      }
      for (i in seq_len(nrow(notes))) {
        note_text <- paste(clean_text(unlist(notes[i, , drop = TRUE])), collapse = " | ")
        if (is.na(note_text) || !nzchar(note_text)) next
        add_audit(
          workbook_path, note_sheet, i, source_file, NA_integer_, NA_character_,
          NA_character_, "skipped", "unstructured_note_needs_manual_review",
          NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
          NA_character_, NA_character_, note_text
        )
      }
      next
    }

    review <- as.data.frame(
      readxl::read_excel(workbook_path, sheet = "review", col_types = "text", .name_repair = "unique"),
      stringsAsFactors = FALSE
    )
    if (!nrow(review)) next

    row_col <- get_first_col(review, "Row")
    qid_col <- get_first_col(review, "QID")
    hh_col <- get_first_col(review, "hh_id")
    fcn_col <- get_first_col(review, "fcn_id")
    original_col <- get_first_col(review, "original_response")
    response_col <- get_first_col(review, c("Response", "Responses"))
    resolution_col <- get_first_col(review, "Resolution")
    remarks_col <- get_first_col(review, "Remarks")
    change_col <- get_first_col(review, "Change Made")
    notes_col <- get_first_col(review, "Notes")

    for (i in seq_len(nrow(review))) {
      qid <- get_cell(review, qid_col, i)
      variable <- find_data_col(qid)
      review_row_number <- parse_review_row(get_cell(review, row_col, i))
      review_hh_id <- get_cell(review, hh_col, i)
      review_fcn_id <- get_cell(review, fcn_col, i)
      original_response <- get_cell(review, original_col, i)
      response <- get_cell(review, response_col, i)
      resolution <- get_cell(review, resolution_col, i)
      remarks <- get_cell(review, remarks_col, i)
      change_made <- get_cell(review, change_col, i)
      notes <- get_cell(review, notes_col, i)
      candidate <- if (!is.na(resolution)) resolution else response
      has_resolution <- !is.na(resolution)

      if (is.na(qid)) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "skipped", "missing_qid", NA_character_, candidate, original_response, response, resolution, remarks, change_made, notes)
        next
      }
      if (is.na(variable)) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "skipped", "qid_not_found_in_clean_data", NA_character_, candidate, original_response, response, resolution, remarks, change_made, notes)
        next
      }
      if (is.na(candidate)) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "skipped", "missing_correction_value", NA_character_, candidate, original_response, response, resolution, remarks, change_made, notes)
        next
      }

      target_rows <- match_target_rows(source_file, review_row_number, review_hh_id, review_fcn_id)
      if (length(target_rows) != 1L) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "skipped", paste0("target_row_count_", length(target_rows)), NA_character_, candidate, original_response, response, resolution, remarks, change_made, notes)
        next
      }
      target_row <- target_rows[1]
      if (!identity_matches(target_row, review_hh_id, review_fcn_id)) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "skipped", "row_identity_mismatch", as.character(corrected[[variable]][target_row]), candidate, original_response, response, resolution, remarks, change_made, notes)
        next
      }

      old_value <- as.character(corrected[[variable]][target_row])
      if (!has_resolution && review_note_response(candidate)) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "skipped", "response_is_review_note_not_data_value", old_value, candidate, original_response, response, resolution, remarks, change_made, notes)
        next
      }
      if (!has_resolution && numeric_like(old_value) && grepl("[A-Za-z]", candidate) && (grepl("\\s", candidate) || nchar(candidate) > 8L)) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "skipped", "non_numeric_response_for_numeric_field", old_value, candidate, original_response, response, resolution, remarks, change_made, notes)
        next
      }

      if (
        numeric_like(old_value) && numeric_like(candidate) &&
          isTRUE(all.equal(as.numeric(old_value), as.numeric(candidate)))
      ) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "unchanged_numeric_equivalent", NA_character_, old_value, candidate, original_response, response, resolution, remarks, change_made, notes)
      } else if (identical(ifelse(is.na(old_value), "<NA>", old_value), ifelse(is.na(candidate), "<NA>", candidate))) {
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "unchanged_already_correct", NA_character_, old_value, candidate, original_response, response, resolution, remarks, change_made, notes)
      } else {
        corrected[[variable]][target_row] <- candidate
        add_audit(workbook_path, "review", i, source_file, review_row_number, qid, variable, "applied", NA_character_, old_value, candidate, original_response, response, resolution, remarks, change_made, notes)
      }
    }
  }

  audit <- if (length(audit_rows)) {
    do.call(rbind, audit_rows)
  } else {
    data.frame(stringsAsFactors = FALSE)
  }
  audit_path <- clean_final_path(
    "4_data",
    "clean_final",
    "survey_refugee_household_endline_review_correction_audit.csv"
  )
  ensure_parent_dir(audit_path)
  write.csv(audit, audit_path, row.names = FALSE, na = "")

  corrected
}

survey <- survey[survey$community == "refugee", , drop = FALSE]
if ("study_arm_overall" %in% names(survey)) {
  survey <- survey[is.na(survey$study_arm_overall) | !(survey$study_arm_overall %in% c("host", "host_in_refugee_form")), , drop = FALSE]
}

survey$refugee_cleaning_exclusion_reason <- NA_character_
# The baseline 8wDI21x row is resolved in apply_refugee_manual_corrections()
# using geocene_data_but_no_survey_error_fix_fcn_id.xlsx.
survey_before_endline_review_identity <- survey
survey <- apply_refugee_household_correction_workbook(survey)
survey <- apply_refugee_endline_review_corrections(survey, identity_data = survey_before_endline_review_identity)
survey <- apply_refugee_manual_corrections(survey)

survey$community <- "refugee"
survey$data_type <- "household_survey"

recoded <- recode_timepoint_by_timestamp(
  survey,
  date_cols = c("collection_date", "start_date", "SubmissionDate", "starttime", "endtime", "date", "datetime"),
  dataset_name = dataset_name
)
survey <- recoded$data
if (!"fcn_id" %in% names(survey)) survey$fcn_id <- NA_character_
missing_fcn_idx <- which(is.na(clean_refugee_exclusion_value(survey$fcn_id)))
if (length(missing_fcn_idx)) {
  survey$refugee_cleaning_exclusion_reason[missing_fcn_idx] <- append_refugee_exclusion_reason(
    survey$refugee_cleaning_exclusion_reason[missing_fcn_idx],
    "missing_fcn_id_declined_participation"
  )
}

# For duplicate refugee endline fcn_id values, keep the first completed survey
# and arbitrarily drop the second survey from the cleaned dataset.
duplicate_endline_drop_idx <- integer(0)
if (all(c("timepoint", "fcn_id") %in% names(survey))) {
  eligible_endline_idx <- which(
    survey$timepoint == "endline" &
      is.na(survey$refugee_cleaning_exclusion_reason) &
      !is.na(clean_refugee_exclusion_value(survey$fcn_id))
  )
  if (length(eligible_endline_idx)) {
    fcn_endline <- clean_refugee_exclusion_value(survey$fcn_id[eligible_endline_idx])
    duplicate_fcn <- names(table(fcn_endline))[table(fcn_endline) > 1L]
    if (length(duplicate_fcn)) {
      completion_date <- extract_date_any(survey$collection_date)
      if (all(is.na(completion_date)) && "start_date" %in% names(survey)) {
        completion_date <- extract_date_any(survey$start_date)
      }
      for (fcn in duplicate_fcn) {
        group_idx <- eligible_endline_idx[fcn_endline == fcn]
        group_order <- order(
          completion_date[group_idx],
          group_idx,
          na.last = TRUE
        )
        drop_idx <- group_idx[group_order[-1L]]
        duplicate_endline_drop_idx <- c(duplicate_endline_drop_idx, drop_idx)
      }
    }
  }
}
if (length(duplicate_endline_drop_idx)) {
  survey$refugee_cleaning_exclusion_reason[duplicate_endline_drop_idx] <- append_refugee_exclusion_reason(
    survey$refugee_cleaning_exclusion_reason[duplicate_endline_drop_idx],
    "duplicate_endline_fcn_id_arbitrarily_drop_second_survey_from_cleaned_dataset"
  )
}

duplicate_endline_drop_count <- length(duplicate_endline_drop_idx)
excluded_refugee_idx <- which(!is.na(survey$refugee_cleaning_exclusion_reason))
excluded_refugee_count <- length(excluded_refugee_idx)
refugee_exclusion_audit_path <- write_refugee_household_exclusion_audit(survey, excluded_refugee_idx)
if (excluded_refugee_count) {
  survey <- survey[-excluded_refugee_idx, , drop = FALSE]
}
survey$refugee_cleaning_exclusion_reason <- NULL
baseline_arm_result <- derive_followup_study_arm_from_baseline(survey)
survey <- baseline_arm_result$data
baseline_endline_no_midline_result <- write_refugee_baseline_endline_no_midline_raw_audit(
  survey,
  survey_raw_imported
)
dietary_diversity_result <- derive_refugee_dietary_diversity(survey)
survey <- dietary_diversity_result$data
write_timepoint_summary(make_timepoint_summary(survey, dataset_name), dataset_name)

deidentified <- drop_identifier_columns(survey)
survey <- deidentified$data
survey <- move_columns_first(
  survey,
  c(
    "community", "data_type", "fcn_id", "hh_id", "unique_id", "uuid",
    "timepoint", "timepoint_original", "collection_date", "collection_year",
    "timepoint_source_col", "raw_collection_round", "raw_survey_version",
    "raw_source_file", "study_arm_overall", "study_arm",
    "study_arm_overall_original", "study_arm_original", "hdds_assume_misc_1",
    "hdds_no_misc", "hdds_recall_basis", "food_diversity_num_groups",
    "food_diversity_eight_groups"
  )
)

output_path <- write_final_rds(survey, "4_data/clean_final/survey_refugee_household.rds")
shareable <- make_shareable_dataset(survey, dataset_name)
shareable_path <- write_shareable_rds(shareable$data, "survey_refugee_household.rds")

entry <- make_inventory_entry(
  dataset_name = dataset_name,
  data = survey,
  output_path = output_path,
  source_paths = clean_final_path(source_rel),
  removed_identifier_columns = deidentified$removed,
  shareable_output_path = shareable_path,
  shareable_removed_identifier_columns = shareable$removed,
  notes = paste(
    "Refugee household survey final dataset rebuilt from raw-first imports in 2_data_raw.",
    "Baseline household IDs were corrected using 2_data_raw/survey_baseline_survey and data review/Rohingya HH Data_Correction_Saeed_20210124.xlsx; embedded manual corrections in this file were applied using intervention/comparison study_arm names; structured endline review corrections were applied from 2_data_raw/survey_endline_survey and data review.",
    "Includes 2022 refugee survey files from survey_endline and 2021 midline files from survey_midline despite misleading endline filenames.",
    "Timepoint was recoded from parsed collection timestamps, so 2019/2020 are baseline, 2021 is midline, and 2022 is endline.",
    paste0("Added baseline-compatible dietary diversity variables from weekly food-frequency items using the HDDS food-group mapping in the embedded HDDS/FCS mappings in this file; hdds_assume_misc_1 nonmissing by timepoint: ", dietary_diversity_result$nonmissing_summary, ". Audit: ", dietary_diversity_result$audit_path, "."),
    paste0(
      "Checked cleaned refugee households with baseline and endline records but no midline record against imported raw midline rows by fcn_id, hh_id, hh_id before the underscore suffix, and UNHCR_id; the audit records the raw source files and paths checked. Households flagged: ",
      baseline_endline_no_midline_result$n_households,
      ". Possible raw midline identifier matches: ",
      baseline_endline_no_midline_result$n_possible_raw_midline_matches,
      ". Audit: ",
      baseline_endline_no_midline_result$audit_path,
      "."
    ),
    paste0(
      "For midline and endline household surveys, study_arm and study_arm_overall were derived from each fcn_id's baseline study_arm_overall where available. Follow-up rows changed from imported arm: ",
      baseline_arm_result$n_changed,
      ". Follow-up rows without a valid baseline arm: ",
      baseline_arm_result$n_no_valid_baseline_arm,
      ". Follow-up rows with conflicting baseline arms: ",
      baseline_arm_result$n_conflicting_baseline_arm,
      ". Audit: ",
      baseline_arm_result$audit_path,
      "."
    ),
    paste0("Excluded refugee household rows with missing fcn_id or duplicate endline fcn_id second surveys: ", excluded_refugee_count, ". The baseline 8wDI21x row was resolved to fcn_id 101595 and hh_id 8WDI21101595 using geocene_data_but_no_survey_error_fix_fcn_id.xlsx. Duplicate endline second surveys arbitrarily dropped: ", duplicate_endline_drop_count, ". Audit: ", refugee_exclusion_audit_path, ".")
  )
)
update_inventory(entry)
write_cleaning_fix_log()

message("Wrote ", output_path)
