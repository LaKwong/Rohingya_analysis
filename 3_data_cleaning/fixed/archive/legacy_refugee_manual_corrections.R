################################################################################
# @Project: Rohingya LPG Evaluation
# @Title: Legacy manual corrections for refugee household survey
# @Description: Ported from 3_data_cleaning/1_clean_Rohingya_data.R.
################################################################################

apply_refugee_legacy_manual_corrections <- function(data) {
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
      source_script = "3_data_cleaning/1_clean_Rohingya_data.R",
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

  data$.legacy_source_row_number <- if ("raw_source_file" %in% names(data)) {
    ave(seq_len(nrow(data)), data$raw_source_file, FUN = seq_along)
  } else {
    rep(NA_integer_, nrow(data))
  }

  legacy_case <- function(source_file, raw_row_number) {
    if (!all(c("raw_source_file", ".legacy_source_row_number") %in% names(data))) {
      return(rep(FALSE, nrow(data)))
    }
    clean_chr(data$raw_source_file) == source_file &
      data$.legacy_source_row_number %in% raw_row_number
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
  set_value("study_arm", idx, standardized_arm[idx], "legacy_standardize_study_arm", "Use intervention/comparison study_arm names for ported legacy corrections.")
  idx <- !values_equal(data$study_arm_overall, standardized_arm) & !is.na(standardized_arm)
  set_value("study_arm_overall", idx, standardized_arm[idx], "legacy_standardize_study_arm_overall", "Align study_arm_overall with intervention/comparison names.")

  standardized_timepoint <- normalize_timepoint(data$timepoint)
  idx <- is.na(clean_chr(data$timepoint)) & !is.na(standardized_timepoint)
  set_value("timepoint", idx, standardized_timepoint[idx], "legacy_standardize_timepoint", "Normalize timepoint labels when needed before applying legacy rules.")

  # Legacy study-arm correction using the new two-arm naming convention.
  set_constant(
    "study_arm",
    c("109334", "106082", "124022", "102768", "106136", "119604", "112172", "115267", "110767"),
    "intervention", "midline", "comparison", "legacy_midline_arm_reclassification",
    "Legacy script changed these from intervention follow-up to post-intervention; expressed here as intervention/comparison."
  )
  set_constant(
    "study_arm_overall",
    c("109334", "106082", "124022", "102768", "106136", "119604", "112172", "115267", "110767"),
    "intervention", "midline", "intervention", "legacy_midline_arm_reclassification",
    "Keep study_arm_overall aligned with the reclassified intervention arm."
  )

  # Baseline household listing and duplicate-survey corrections. Cases that
  # previously matched on participant/child names now use non-identifying raw
  # source row numbers from the imported baseline file.
  idx <- legacy_case("RohingyaFuelMaster_Corrected_20200419_refugee.csv", 2) &
    is_timepoint("baseline") & is_arm("intervention") &
    col_equals("camp_id", "8W", TRUE) &
    col_equals("block_id", "D", TRUE) &
    col_equals("subblock_id", "I21", TRUE) &
    col_missing_or("fcn_id", "x")
  set_value("fcn_id", idx, "999999", "legacy_baseline_fcn_placeholder_001", "Updated after a discussion with the team to clarify the hh_id.")

  idx <- legacy_case("RohingyaFuelMaster_Corrected_20200419_refugee.csv", 42) &
    is_timepoint("baseline") & is_arm("intervention") &
    col_equals("camp_id", "8W", TRUE) &
    col_equals("block_id", "D", TRUE) &
    col_equals("subblock_id", "I21", TRUE) &
    col_missing_or("fcn_id")
  set_value("fcn_id", idx, "999998", "legacy_baseline_fcn_placeholder_002", "Updated after a discussion with the team to clarify the hh_id.")

  enum_num <- if ("enumerator" %in% names(data)) suppressWarnings(as.numeric(as.character(data$enumerator))) else rep(NA_real_, nrow(data))
  drop_rows(is_timepoint("baseline") & is_arm("intervention") & fcn_in("117722") & enum_num == 8, "legacy_drop_duplicate_baseline_117722", "Accidentally surveyed twice in baseline.")
  enum_num <- if ("enumerator" %in% names(data)) suppressWarnings(as.numeric(as.character(data$enumerator))) else rep(NA_real_, nrow(data))
  drop_rows(is_timepoint("baseline") & is_arm("intervention") & fcn_in("197654") & enum_num == 1, "legacy_drop_duplicate_baseline_197654", "Accidentally surveyed twice in baseline.")
  enum_num <- if ("enumerator" %in% names(data)) suppressWarnings(as.numeric(as.character(data$enumerator))) else rep(NA_real_, nrow(data))
  drop_rows(is_timepoint("baseline") & is_arm("intervention") & fcn_in("100976") & enum_num == 3, "legacy_drop_duplicate_baseline_100976", "Accidentally surveyed twice in baseline.")
  enum_num <- if ("enumerator" %in% names(data)) suppressWarnings(as.numeric(as.character(data$enumerator))) else rep(NA_real_, nrow(data))
  drop_rows(is_timepoint("baseline") & is_arm("comparison") & fcn_in("186890") & enum_num == 1, "legacy_drop_duplicate_baseline_186890", "Accidentally surveyed twice in baseline.")

  idx <- legacy_case("RohingyaFuelMaster_Corrected_20200419_refugee.csv", 201) &
    is_timepoint("baseline") & is_arm("intervention") &
    col_equals("camp_id", "8W", TRUE) &
    col_equals("block_id", "B", TRUE) &
    col_equals("subblock_id", "A13", TRUE)
  set_value("fcn_id", idx, "122063", "legacy_baseline_fcn_001", "HH listing correction from legacy cleaner.")
  idx <- legacy_case("RohingyaFuelMaster_Corrected_20200419_refugee.csv", 1322) &
    is_timepoint("baseline") & is_arm("comparison") &
    col_equals("camp_id", "8E", TRUE) &
    col_equals("block_id", "C", TRUE) &
    col_equals("subblock_id", "B33", TRUE)
  set_value("fcn_id", idx, "114444", "legacy_baseline_fcn_002", "HH listing correction from legacy cleaner.")

  # Targeted hh_id updates for the baseline fcn corrections above. These replace
  # the old fcn embedded in hh_id without reintroducing the legacy blanket rebuild.
  idx <- legacy_case("RohingyaFuelMaster_Corrected_20200419_refugee.csv", 201) &
    is_timepoint("baseline") & is_arm("intervention") &
    col_equals("camp_id", "8W", TRUE) &
    col_equals("block_id", "B", TRUE) &
    col_equals("subblock_id", "A13", TRUE)
  set_value("hh_id", idx, "8wBA13122063_1067", "legacy_baseline_hh_id_001", "Targeted hh_id update paired with corrected baseline fcn_id.")
  idx <- legacy_case("RohingyaFuelMaster_Corrected_20200419_refugee.csv", 1322) &
    is_timepoint("baseline") & is_arm("comparison") &
    col_equals("camp_id", "8E", TRUE) &
    col_equals("block_id", "C", TRUE) &
    col_equals("subblock_id", "B33", TRUE)
  set_value("hh_id", idx, "8ECB33114444_4857", "legacy_baseline_hh_id_002", "Targeted hh_id update paired with corrected baseline fcn_id.")

  # Legacy direct-name corrections are intentionally not ported. Name fields are
  # removed from final datasets, and shareable outputs get an additional strict
  # de-identification pass.

  set_by_fcn(
    "collect_wood_forest_start",
    c("600061" = "1-Apr-17", "124870" = "1-Jul-17", "120773" = "1-Jul-18", "124869" = "1-Mar-17", "107198" = "1-Sep-18"),
    "baseline", rule_id = "legacy_baseline_collect_wood_start_swap",
    note = "Legacy correction for swapped firewood collection dates."
  )
  set_by_fcn(
    "collect_wood_forest_stop",
    c("600061" = "1-Aug-17", "124870" = "1-Aug-17", "120773" = "1-Aug-18", "124869" = "1-Aug-17", "107198" = "1-Sep-19"),
    "baseline", rule_id = "legacy_baseline_collect_wood_stop_swap",
    note = "Legacy correction for swapped firewood collection dates; corrected obvious Sep spelling typo from the legacy script."
  )

  # Do not port the legacy script's blanket baseline hh_id rebuild. The fixed
  # cleaner applies the baseline correction workbook before this function, and
  # that workbook is the authoritative source for corrected baseline hh_id values.
  # Rebuilding every baseline hh_id from components can alter confirmed casing or
  # create malformed duplicates when any component is missing.
  # Midline fcn_id and household-location corrections.
  set_by_fcn(
    "fcn_id",
    c("101841" = "101840", "101016" = "106615", "109663" = "109697", "122066" = "112066", "118211" = "112811", "123825" = "113825", "115717" = "117517", "133506" = "123506", "123970" = "123969", "193748" = "193738", "106850" = "206850", "101096" = "225997", "291236" = "290437"),
    "midline", "intervention", "legacy_midline_intervention_fcn", "Manual fcn_id correction from legacy cleaner."
  )
  set_by_fcn(
    "fcn_id",
    c("186294" = "285592", "199748" = "299748", "245292" = "145292", "200677" = "200667", "194490" = "194499", "197472" = "197422", "383686" = "283686", "297028" = "207028", "168661" = "451059", "172965" = "172964", "183398" = "283398", "180887" = "180837", "184810" = "184809", "185054" = "185055", "193911" = "185415", "185979" = "185980", "157669" = "177669", "177668" = "188577", "177772" = "157574", "177471" = "173073", "287797" = "278797", "296727" = "296729", "650712" = "295896", "650705" = "295894", "650711" = "295890", "451786" = "166147", "164741" = "164742", "120733" = "120773"),
    "midline", "comparison", "legacy_midline_comparison_fcn", "Manual fcn_id correction from legacy cleaner."
  )

  set_constant("camp_id", "165336", "5", "midline", rule_id = "legacy_midline_camp_165336", note = "Manual camp_id correction from legacy cleaner.")
  set_constant("camp_id", c("100959", "100971", "100972", "100976", "100999", "101034", "101038", "101109", "101172", "101239", "101251", "101574", "101618", "101667", "101705", "101723", "101737", "101762", "101777", "106976", "113898", "117152", "117719", "117729", "119902", "122567", "123342", "123677", "123970", "123976", "125795", "274944", "290417", "290439", "290495", "291236", "291525", "296044", "300629"), "8W", "midline", rule_id = "legacy_midline_camp_8w", note = "Manual camp_id correction from legacy cleaner.")
  set_constant("camp_id", c("114476", "114556", "124614"), "8E", "midline", rule_id = "legacy_midline_camp_8e", note = "Manual camp_id correction from legacy cleaner.")
  set_constant("camp_id", "115663", "9", "midline", rule_id = "legacy_midline_camp_115663", note = "Manual camp_id correction from legacy cleaner.")
  set_constant("camp_id", c("115815", "110636", "109334", "193582"), "10", "midline", rule_id = "legacy_midline_camp_10", note = "Manual camp_id correction from legacy cleaner.")
  set_constant("camp_id", c("196929", "245292"), "18", "midline", rule_id = "legacy_midline_camp_18", note = "Manual camp_id correction from legacy cleaner.")
  set_constant("block_id", "122063", "b", "midline", rule_id = "legacy_midline_block_122063", note = "Manual block_id correction from legacy cleaner.")
  set_constant("block_id", "296782", "f", "midline", rule_id = "legacy_midline_block_296782", note = "Manual block_id correction from legacy cleaner.")
  set_constant("block_id", c("115643", "115646", "115767"), "g", "midline", rule_id = "legacy_midline_block_g", note = "Manual block_id correction from legacy cleaner.")
  set_constant("subblock_id", "296782", "UU13", "midline", rule_id = "legacy_midline_subblock_296782", note = "Manual subblock_id correction from legacy cleaner.")
  set_constant("subblock_id", "295872", "UU14", "midline", rule_id = "legacy_midline_subblock_295872", note = "Manual subblock_id correction from legacy cleaner.")
  set_constant("subblock_id", "286224", "G30", "midline", rule_id = "legacy_midline_subblock_286224", note = "Manual subblock_id correction from legacy cleaner.")
  set_constant("subblock_id", "187195", "DD22", "midline", rule_id = "legacy_midline_subblock_187195", note = "Manual subblock_id correction from legacy cleaner.")
  set_constant("subblock_id", "114741", "B33", "midline", rule_id = "legacy_midline_subblock_114741", note = "Manual subblock_id correction from legacy cleaner.")
  set_constant("subblock_id", c("289608", "123657"), "G29", "midline", rule_id = "legacy_midline_subblock_g29", note = "Manual subblock_id correction from legacy cleaner.")

  set_by_fcn("collect_wood_forest_start", c("121839" = "Apr 1, 2017", "182703" = "Aug 1, 2017", "110349" = "Feb 1, 2017", "295889" = "Feb 1, 2017", "184266" = "Mar 1, 2017", "109816" = "May 1, 2017", "184565" = "Nov 1, 2017"), "midline", rule_id = "legacy_midline_collect_wood_start_swap", note = "Legacy correction for swapped firewood collection dates.")
  set_by_fcn("collect_wood_forest_stop", c("121839" = "Sep 1, 2017", "182703" = "Oct 1, 2017", "110349" = "Aug 1, 2017", "295889" = "Aug 1, 2017", "184266" = "Sep 1, 2017", "109816" = "Aug 1, 2017", "184565" = "Dec 1, 2017"), "midline", rule_id = "legacy_midline_collect_wood_stop_swap", note = "Legacy correction for swapped firewood collection dates.")

  set_constant("first_receive_lpg", "286053", "Nov 1, 2019", "midline", rule_id = "legacy_midline_first_receive_lpg_286053", note = "Manual LPG receipt correction from Geocene review.")
  set_constant("bread_adults_week", c("114537", "120722"), 2, "midline", rule_id = "legacy_midline_bread_adults_week", note = "Manual value correction from legacy cleaner.")
  set_constant("bread_adults_week", c("286043", "125336"), 3, "midline", rule_id = "legacy_midline_bread_adults_week", note = "Manual value correction from legacy cleaner.")
  set_constant("bread_adults_week", "111198", 4, "midline", rule_id = "legacy_midline_bread_adults_week", note = "Manual value correction from legacy cleaner.")
  set_constant("clothing", "193342", 15000, "midline", rule_id = "legacy_midline_clothing", note = "Manual value correction from legacy cleaner.")
  set_constant("clothing", "120564", 20000, "midline", rule_id = "legacy_midline_clothing", note = "Manual value correction from legacy cleaner.")
  set_constant("clothing", c("114338", "193130"), 30000, "midline", rule_id = "legacy_midline_clothing", note = "Manual value correction from legacy cleaner.")
  set_constant("debt", c("116548", "289648"), 0, "midline", rule_id = "legacy_midline_debt", note = "Manual value correction from legacy cleaner.")
  set_constant("debt", c("122093", "152796"), 3000, "midline", rule_id = "legacy_midline_debt", note = "Manual value correction from legacy cleaner.")
  set_constant("debt", "152797", 4000, "midline", rule_id = "legacy_midline_debt", note = "Manual value correction from legacy cleaner.")
  set_constant("debt", "118858", 8000, "midline", rule_id = "legacy_midline_debt", note = "Manual value correction from legacy cleaner.")
  set_constant("debt", "108549", 13700, "midline", rule_id = "legacy_midline_debt", note = "Manual value correction from legacy cleaner.")
  set_constant("debt", "123665", 300000, "midline", rule_id = "legacy_midline_debt", note = "Manual value correction from legacy cleaner.")
  set_constant("drudgery_most_diff", "115756", 4, "midline", rule_id = "legacy_midline_drudgery_most_diff", note = "Manual value correction from legacy cleaner.")
  set_constant("drudgery_second_most_diff", "116103", 4, "midline", rule_id = "legacy_midline_drudgery_second_most_diff", note = "Manual value correction from legacy cleaner.")
  set_constant("drudgery_easiest", c("116267", "122489", "102745"), 3, "midline", rule_id = "legacy_midline_drudgery_easiest", note = "Manual value correction from legacy cleaner.")
  set_constant("fish_adults_week", "106841", 4, "midline", rule_id = "legacy_midline_fish_adults_week", note = "Manual value correction from legacy cleaner.")
  set_constant("fish_adults_week", "102330", 5, "midline", rule_id = "legacy_midline_fish_adults_week", note = "Manual value correction from legacy cleaner.")
  set_constant("floor_material", "296044", 5, "midline", rule_id = "legacy_midline_floor_material", note = "Manual value correction from legacy cleaner.")
  set_constant("flooring_below_stove", c("100959", "101172", "122489", "124325"), 3, "midline", rule_id = "legacy_midline_flooring_below_stove", note = "Manual value correction from legacy cleaner.")
  set_constant("flooring_below_stove", c("111593", "123012", "122063", "102722", "118250", "102745"), 6, "midline", rule_id = "legacy_midline_flooring_below_stove", note = "Manual value correction from legacy cleaner.")
  set_constant("flooring_below_stove", c("291525", "106976", "123864"), 8, "midline", rule_id = "legacy_midline_flooring_below_stove", note = "Manual value correction from legacy cleaner.")
  set_constant("food_cant_afford_2wk", "115741", 0, "midline", rule_id = "legacy_midline_food_cant_afford_2wk", note = "Manual value correction from legacy cleaner.")
  set_constant("fuel_cant_afford_action", c("116267", "124381"), "8", "midline", rule_id = "legacy_midline_fuel_cant_afford_action", note = "Manual value correction from legacy cleaner.")
  set_constant("fuel_cant_afford_action", "115741", NA_character_, "midline", rule_id = "legacy_midline_fuel_cant_afford_action", note = "Manual value correction from legacy cleaner.")
  set_constant("forest_wood_fee", "109334", 100, "midline", rule_id = "legacy_midline_forest_wood_fee", note = "Manual value correction from legacy cleaner.")
  set_constant("fuel_30_receive_lpg", "106582", 1, "midline", rule_id = "legacy_midline_fuel_30_receive_lpg", note = "Manual value correction from legacy cleaner.")
  set_constant("happy", "122929", 1, "midline", rule_id = "legacy_midline_happy", note = "Manual value correction from legacy cleaner.")
  set_constant("happy", c("121962", "119364"), 2, "midline", rule_id = "legacy_midline_happy", note = "Manual value correction from legacy cleaner.")
  set_constant("happy", c("101723", "111944", "112726", "115771", "117505", "117514", "122241", "123005"), 4, "midline", rule_id = "legacy_midline_happy", note = "Manual value correction from legacy cleaner.")
  set_constant("income_cash_ngo", "179622", 1500, "midline", rule_id = "legacy_midline_income_cash_ngo", note = "Manual value correction from legacy cleaner.")
  set_constant("income_cash_ngo", "123568", 9000, "midline", rule_id = "legacy_midline_income_cash_ngo", note = "Legacy comment says -9000 was assumed to be an accidental negative sign.")
  set_constant("income_humanitarian_asst", "111287", 400, "midline", rule_id = "legacy_midline_income_humanitarian_asst", note = "Manual value correction from legacy cleaner.")
  set_constant("income_humanitarian_asst", "124768", 900, "midline", rule_id = "legacy_midline_income_humanitarian_asst", note = "Manual value correction from legacy cleaner.")
  set_constant("income_humanitarian_asst", "153470", 1000, "midline", rule_id = "legacy_midline_income_humanitarian_asst", note = "Manual value correction from legacy cleaner.")
  set_constant("income_own_business", "105366", 30000, "midline", rule_id = "legacy_midline_income_own_business", note = "Manual value correction from legacy cleaner.")
  set_constant("income_wage_labor", "109673", 6000, "midline", rule_id = "legacy_midline_income_wage_labor", note = "Manual value correction from legacy cleaner.")
  set_constant("income_wage_labor", "113921", 6500, "midline", rule_id = "legacy_midline_income_wage_labor", note = "Manual value correction from legacy cleaner.")
  set_constant("income_wage_labor", "109334", 7000, "midline", rule_id = "legacy_midline_income_wage_labor", note = "Manual value correction from legacy cleaner.")
  set_constant("lpg_cylinder_repair", "122489", "1", "midline", rule_id = "legacy_midline_lpg_cylinder_repair", note = "Manual value correction from legacy cleaner.")
  set_constant("lpg_repair_costs", "108877", 150, "midline", rule_id = "legacy_midline_lpg_repair_costs", note = "Manual value correction from legacy cleaner.")
  set_constant("lpg_repair_costs", "291525", 250, "midline", rule_id = "legacy_midline_lpg_repair_costs", note = "Manual value correction from legacy cleaner.")
  set_constant("lpg_stove_repair", "187831", "1", "midline", rule_id = "legacy_midline_lpg_stove_repair", note = "Manual value correction from legacy cleaner.")
  set_constant("medical", "120564", 1000, "midline", rule_id = "legacy_midline_medical", note = "Manual value correction from legacy cleaner.")
  set_constant("medical", "124088", 2000, "midline", rule_id = "legacy_midline_medical", note = "Manual value correction from legacy cleaner.")
  set_constant("medical", "290455", 3000, "midline", rule_id = "legacy_midline_medical", note = "Manual value correction from legacy cleaner.")
  set_constant("medical", c("106260", "200851"), 6000, "midline", rule_id = "legacy_midline_medical", note = "Manual value correction from legacy cleaner.")
  set_constant("medical", "101456", 7000, "midline", rule_id = "legacy_midline_medical", note = "Manual value correction from legacy cleaner.")
  set_constant("medical", "112638", 13000, "midline", rule_id = "legacy_midline_medical", note = "Manual value correction from legacy cleaner.")
  set_constant("medical", "108549", 15000, "midline", rule_id = "legacy_midline_medical", note = "Manual value correction from legacy cleaner.")
  set_constant("medical", "124321", 20000, "midline", rule_id = "legacy_midline_medical", note = "Manual value correction from legacy cleaner.")
  set_constant("medical", "281201", 30000, "midline", rule_id = "legacy_midline_medical", note = "Manual value correction from legacy cleaner.")
  set_constant("medical", "292310", 70000, "midline", rule_id = "legacy_midline_medical", note = "Manual value correction from legacy cleaner.")
  set_constant("medical", "179975", 7000, "midline", rule_id = "legacy_midline_medical", note = "Manual value correction from legacy cleaner.")
  set_constant("medical", c("106585", "106586", "111597", "112019", "115651", "115661", "115662", "115663", "115678", "115682", "115685", "116450", "119405", "119603", "123571"), NA_real_, "midline", rule_id = "legacy_midline_medical", note = "Manual value correction from legacy cleaner.")
  set_constant("potatoes_adults_week", c("101274", "109334", "115756"), 5, "midline", rule_id = "legacy_midline_potatoes_adults_week", note = "Manual value correction from legacy cleaner.")
  set_constant("potatoes_adults_week", c("178204", "289648"), 6, "midline", rule_id = "legacy_midline_potatoes_adults_week", note = "Manual value correction from legacy cleaner.")
  set_constant("potatoes_adults_week", "120982", 7, "midline", rule_id = "legacy_midline_potatoes_adults_week", note = "Manual value correction from legacy cleaner.")
  set_constant("shelter", "111327", 20000, "midline", rule_id = "legacy_midline_shelter", note = "Manual value correction from legacy cleaner.")
  set_constant("shelter", "110693", 30000, "midline", rule_id = "legacy_midline_shelter", note = "Manual value correction from legacy cleaner.")
  set_constant("spent_total_month", "128915", 3120, "midline", rule_id = "legacy_midline_spent_total_month", note = "Manual value correction from legacy cleaner.")
  # Direct-name-only legacy corrections are omitted from this archived copy.
  set_constant("time_cooking", c("107012", "107019", "108549", "111470", "111944", "112138", "113921", "115548", "115756", "115771", "115792", "115817", "117514", "123568", "123569", "123570", "123657", "123670", "123845", "123976", "192633", "192730", "193582", "195381", "201161", "289608", "289648"), 3, "midline", rule_id = "legacy_midline_time_cooking", note = "Manual value correction from legacy cleaner.")
  set_constant("time_harvesting_wood", c("123571", "123665", "201612"), 2, "midline", rule_id = "legacy_midline_time_harvesting_wood", note = "Manual value correction from legacy cleaner.")
  set_constant("time_harvesting_wood", c("100959", "100971", "100972", "100999", "101034", "101038", "101618", "101705", "102330", "106585", "106586", "108169", "108351", "108549", "109673", "109807", "111470", "111944", "112138", "115371", "115465", "115756", "115771", "115817", "116542", "116707", "116710", "116932", "116985", "116987", "117514", "117943", "119603", "121962", "122092", "122093", "122150", "122241", "122378", "123005", "123568", "123656", "123657", "123677", "123845", "123847", "123976", "193130", "193582", "195381", "289608", "289648", "600019"), 3, "midline", rule_id = "legacy_midline_time_harvesting_wood", note = "Manual value correction from legacy cleaner.")
  set_constant("traditional_use_yesterday", "115818", 1, "midline", rule_id = "legacy_midline_traditional_use_yesterday", note = "Manual value correction from legacy cleaner.")
  set_constant("traditional_use_yesterday", "122929", 3, "midline", rule_id = "legacy_midline_traditional_use_yesterday", note = "Manual value correction from legacy cleaner.")

  # Endline corrections.
  set_constant("study_arm", c("115651", "115661"), "intervention", "endline", rule_id = "legacy_endline_arm_reclassification", note = "Legacy script changed these two households to the intervention endline arm.")
  set_constant("study_arm_overall", c("115651", "115661"), "intervention", "endline", rule_id = "legacy_endline_arm_reclassification", note = "Keep study_arm_overall aligned with the intervention endline arm.")
  set_by_fcn("fcn_id", c("101841" = "101840", "108306" = "109306", "108796" = "108799", "111272" = "112172", "115603" = "115643", "123825" = "113825", "124324" = "124321", "191395" = "191359", "207984" = "107984"), "endline", "intervention", "legacy_endline_intervention_fcn", "Manual fcn_id correction from legacy cleaner.")
  set_by_fcn("fcn_id", c("157669" = "177669", "171094" = "171098", "183398" = "283398", "297872" = "295872", "650711" = "295890", "207984" = "107984", "147235" = "174235"), "endline", "comparison", "legacy_endline_comparison_fcn", "Manual fcn_id correction from legacy cleaner.")
  set_value("camp_id", is_timepoint("endline") & col_equals("hh_id", "DDH21291234", TRUE), "8W", "legacy_endline_camp_ddh21291234", "Manual camp_id correction from legacy cleaner.")
  set_constant("camp_id", "115766", "10", "endline", rule_id = "legacy_endline_camp_115766", note = "Manual camp_id correction from legacy cleaner.")
  set_value("block_id", is_timepoint("endline") & col_equals("block_id", "292302", TRUE), "E", "legacy_endline_block_292302", "Manual block_id correction from legacy cleaner.")
  if ("subblock_id" %in% names(data)) {
    idx <- is_timepoint("endline") & !is.na(clean_chr(data$subblock_id)) & grepl("\\(", clean_chr(data$subblock_id))
    set_value("subblock_id", idx, sub("\\s*\\(.*$", "", clean_chr(data$subblock_id)[idx]), "legacy_endline_subblock_strip_parenthetical", "Strip parenthetical notes from subblock_id as in the legacy cleaner.")
  }
  set_constant("subblock_id", "292302", "UU16", "endline", rule_id = "legacy_endline_subblock_292302", note = "Manual subblock_id correction from legacy cleaner.")
  set_constant("subblock_id", "115663", "g39", "endline", rule_id = "legacy_endline_subblock_115663", note = "Manual subblock_id correction from legacy cleaner.")
  set_constant("subblock_id", "123976", "I14", "endline", rule_id = "legacy_endline_subblock_123976", note = "Manual subblock_id correction from legacy cleaner.")
  set_constant("subblock_id", c("650711", "295890", "125333"), "UU14", "endline", rule_id = "legacy_endline_subblock_uu14", note = "Manual subblock_id correction from legacy cleaner.")
  set_constant("subblock_id", "125336", "B33", "endline", rule_id = "legacy_endline_subblock_125336", note = "Manual subblock_id correction from legacy cleaner.")
  set_value("blanket", is_timepoint("endline") & col_equals("blanket", "182703", TRUE), 8, "legacy_endline_blanket", "Manual value correction from legacy cleaner.")
  set_constant("buy_wood_cost_bundle", "302244", 50, "endline", rule_id = "legacy_endline_buy_wood_cost_bundle", note = "Manual value correction from legacy cleaner.")
  set_constant("buy_wood_cost_month_estimate", "302244", 100, "endline", rule_id = "legacy_endline_buy_wood_cost_month_estimate", note = "Manual value correction from legacy cleaner.")
  set_constant("chicken_duck_pigeon", "175537", 2, "endline", rule_id = "legacy_endline_chicken_duck_pigeon", note = "Manual value correction from legacy cleaner.")
  set_constant("child_books", "283059", 10, "endline", rule_id = "legacy_endline_child_books", note = "Manual value correction from legacy cleaner.")
  set_constant("child_books", "277017", 4, "endline", rule_id = "legacy_endline_child_books", note = "Manual value correction from legacy cleaner.")
  set_constant("child_books", "207030", 5, "endline", rule_id = "legacy_endline_child_books", note = "Manual value correction from legacy cleaner.")
  set_constant("collect_wood_when_last", c("175715", "187828"), "", "endline", rule_id = "legacy_endline_collect_wood_when_last", note = "Manual value correction from legacy cleaner.")
  set_constant("cook_who_w", "249671", 1, "endline", rule_id = "legacy_endline_cook_who_w", note = "Manual value correction from legacy cleaner.")
  set_constant("debt", "112064", 1500, "endline", rule_id = "legacy_endline_debt", note = "Manual value correction from legacy cleaner.")
  set_constant("food_source", c("112064", "283008", "277068", "277066", "169327", "280794", "179622", "179621", "147235", "184565", "283077", "175489", "179354", "147887", "180644", "168593", "147235"), 1, "endline", rule_id = "legacy_endline_food_source", note = "Manual value correction from legacy cleaner.")
  set_constant("healthcare_visits_6mo", "286010", 8, "endline", rule_id = "legacy_endline_healthcare_visits_6mo", note = "Manual value correction from legacy cleaner.")
  set_constant("healthcare_visits_6mo", "184510", 2, "endline", rule_id = "legacy_endline_healthcare_visits_6mo", note = "Manual value correction from legacy cleaner.")
  set_constant("hh_size", "122132", 14, "endline", rule_id = "legacy_endline_hh_size", note = "Manual value correction from legacy cleaner.")
  set_constant("income_home_garden", c("111472", "111470", "111589", "112010"), 0, "endline", rule_id = "legacy_endline_income_home_garden", note = "Manual value correction from legacy cleaner.")
  set_constant("lpg_willingness_to_pay", "302244", 100, "endline", rule_id = "legacy_endline_lpg_willingness_to_pay", note = "Manual value correction from legacy cleaner.")
  set_constant("meat_consumption", "278797", 0, "endline", rule_id = "legacy_endline_meat_consumption", note = "Manual value correction from legacy cleaner.")
  # Direct respondent-name legacy corrections are omitted from this archived copy.
  set_constant("sleep_fall_asleep_min", c("292314", "174188", "171478", " 186825"), 10, "endline", rule_id = "legacy_endline_sleep_fall_asleep_min", note = "Manual value correction from legacy cleaner.")
  set_constant("sleep_hours", "278797", 10, "endline", rule_id = "legacy_endline_sleep_hours", note = "Manual value correction from legacy cleaner.")
  set_constant("sleep_hours", "181279", 7, "endline", rule_id = "legacy_endline_sleep_hours", note = "Manual value correction from legacy cleaner.")
  set_constant("solar_panel", "106582", 2, "endline", rule_id = "legacy_endline_solar_panel", note = "Manual value correction from legacy cleaner.")
  set_constant("spent_food", "173639", 3000, "endline", rule_id = "legacy_endline_spent_food", note = "Manual value correction from legacy cleaner.")
  set_constant("veggies_adults_week", "152849", 3, "endline", rule_id = "legacy_endline_veggies_adults_week", note = "Manual value correction from legacy cleaner.")
  set_constant("veggies_source", "123571", "2 3", "endline", rule_id = "legacy_endline_veggies_source", note = "Manual value correction from legacy cleaner.")

  # Enumerator names are not derived in this archived copy; keep numeric
  # enumerator codes only unless a separate restricted lookup is needed.

  # Legacy fill: add LPG timing fields to endline when they were not asked.
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
    set_value(col, target_idx, replacement, paste0("legacy_endline_fill_", col, "_from_midline"), "Fill missing endline LPG timing fields from matching midline record, as intended in the legacy cleaner.")
  }
  fill_endline_from_midline("first_receive_lpg")
  fill_endline_from_midline("first_enrolled_lpg")

  audit <- if (length(audit_rows)) do.call(rbind, audit_rows) else data.frame(stringsAsFactors = FALSE)
  audit_path <- clean_final_path(
    "4_data",
    "clean_final",
    "survey_refugee_household_legacy_manual_correction_audit.csv"
  )
  ensure_parent_dir(audit_path)
  write.csv(audit, audit_path, row.names = FALSE, na = "")

  if (".legacy_source_row_number" %in% names(data)) {
    data$.legacy_source_row_number <- NULL
  }
  data
}
