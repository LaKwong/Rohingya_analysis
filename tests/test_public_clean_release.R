# Standalone release checks: Rscript --vanilla tests/test_public_clean_release.R

find_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (file.exists(file.path(path, "Rohingya_analysis.Rproj"))) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) stop("Could not locate Rohingya_analysis.Rproj.", call. = FALSE)
    path <- parent
  }
}

root <- find_project_root()
private_root <- file.path(root, "4_data", "clean_final")
public_root <- file.path(root, "4_data", "clean_final_public")
private_survey <- readRDS(file.path(private_root, "survey_refugee_household.rds"))
public_survey <- readRDS(file.path(public_root, "survey_refugee_household.rds"))

stopifnot(
  all(c("mobile_phone_yn", "smartphone_yn") %in% names(private_survey)),
  all(c("mobile_phone_yn", "smartphone_yn") %in% names(public_survey)),
  !any(c("instanceName", "instanceID") %in% names(public_survey)),
  !any(c("mobile_phone", "smartphone") %in% names(public_survey)),
  nrow(private_survey) == nrow(public_survey),
  sum(!is.na(private_survey$mobile_phone_yn)) ==
    sum(!is.na(public_survey$mobile_phone_yn)),
  sum(!is.na(private_survey$smartphone_yn)) ==
    sum(!is.na(public_survey$smartphone_yn))
)

manifest <- read.csv(
  file.path(public_root, "public_clean_final_manifest.csv"),
  stringsAsFactors = FALSE
)
stopifnot(
  "has_instance_columns" %in% names(manifest),
  !any(manifest$has_instance_columns),
  !file.exists(file.path(public_root, "household_key_crosswalk.csv")),
  file.exists(file.path(
    root, "8_restricted", "public_clean_final", "household_key_crosswalk.csv"
  ))
)

private_geocene <- readRDS(file.path(
  private_root, "geocene", "100_80_5_20", "events.rds"
))
public_geocene <- readRDS(file.path(
  public_root, "geocene", "100_80_5_20", "events.rds"
))
private_linked_households <- length(intersect(
  unique(private_survey$fcn_id), unique(private_geocene$fcn_id)
))
public_linked_households <- length(intersect(
  unique(public_survey$fcn_id), unique(public_geocene$fcn_id)
))
stopifnot(
  private_linked_households > 0L,
  public_linked_households == private_linked_households
)

cat("Public clean release privacy, phone ownership, and Geocene linkage checks passed.\n")
