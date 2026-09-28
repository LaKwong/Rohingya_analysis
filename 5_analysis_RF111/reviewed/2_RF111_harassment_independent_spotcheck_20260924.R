# Independent spot-check of headline RF111 harassment estimates
#
# Purpose: Recompute two manuscript values using base R and explicit variable
# names, without calling the main analysis helpers.

as_code <- function(x) suppressWarnings(as.numeric(x))

survey_spotcheck <- suppressWarnings(readRDS(input_household_rds))
events_spotcheck <- c(
  "insult", "belittle", "scare", "push", "hit", "kick", "choke",
  "weapon", "sex_lang", "sex_contact", "sex_rumor", "clothing_pull",
  "sex_corner"
)

recompute_collect_wood_men_any <- function(wave) {
  dat <- survey_spotcheck[as.character(survey_spotcheck$timepoint) == wave, ]
  suffix <- if (wave == "baseline") "" else "_ever"
  module <- as_code(dat$harassment_continue)
  collector <- as_code(dat[[paste0("collect_wood_m", suffix)]])
  item_occurrence <- matrix(
    NA_integer_, nrow = nrow(dat), ncol = length(events_spotcheck)
  )

  for (j in seq_along(events_spotcheck)) {
    event <- events_spotcheck[j]
    top <- as_code(dat[[paste0(event, "_hh", suffix)]])
    fuel <- as_code(dat[[paste0("collect_wood_", event, "_hh", suffix)]])
    count <- as_code(dat[[paste0("collect_wood_", event, "_m", suffix)]])
    eligible <- module == 1 & collector == 1
    valid_count <- !is.na(count) & count >= 0 & count == floor(count)

    top_no <- eligible & top == 0 &
      (is.na(fuel) | fuel == 0) & (is.na(count) | count == 0)
    fuel_no <- eligible & top == 1 & fuel == 0 &
      (is.na(count) | count == 0)
    detail_known <- eligible & top == 1 & fuel == 1 & valid_count

    item_occurrence[top_no, j] <- 0L
    item_occurrence[fuel_no, j] <- 0L
    item_occurrence[detail_known, j] <- as.integer(count[detail_known] > 0)
  }

  any_occurrence <- apply(item_occurrence, 1, function(x) {
    if (any(x == 1L, na.rm = TRUE)) return(1L)
    if (all(!is.na(x)) && all(x == 0L)) return(0L)
    NA_integer_
  })

  data.frame(
    result_key = paste0(wave, "__collect_wood__m__any"),
    independent_numerator = sum(any_occurrence == 1L, na.rm = TRUE),
    independent_denominator = sum(!is.na(any_occurrence)),
    stringsAsFactors = FALSE
  )
}

spotcheck <- do.call(
  rbind,
  lapply(c("baseline", "midline"), recompute_collect_wood_men_any)
)
manuscript_values <- read.csv(
  file.path(output_qa_dir, "table_rf111_manuscript_values.csv"),
  stringsAsFactors = FALSE
)
comparison <- merge(
  spotcheck,
  manuscript_values[c("result_key", "numerator", "denominator")],
  by = "result_key",
  all.x = TRUE
)
comparison$exact_match <- with(
  comparison,
  independent_numerator == numerator & independent_denominator == denominator
)

write.csv(
  comparison,
  file.path(output_qa_dir, "table_rf111_independent_spotcheck.csv"),
  row.names = FALSE,
  na = ""
)

if (any(!comparison$exact_match) || any(is.na(comparison$exact_match))) {
  stop("Independent headline-value spot-check failed.")
}

message("Independent spot-check passed for baseline and midline wood-collecting men.")
