# Integration regression: uses existing cleaned data and regenerates Geocene only.
# Run from the repository root: Rscript --vanilla tests/test_geocene_external_workdir.R
local({
  root <- normalizePath(Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = "."), winslash = "/", mustWork = TRUE)
  old_wd <- getwd()
  old_env <- Sys.getenv(c("ROHINGYA_ANALYSIS_ROOT", "RF105_CLEAN_DATA_DIR"), unset = NA_character_)
  on.exit({
    setwd(old_wd)
    for (name in names(old_env)) {
      if (is.na(old_env[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, setNames(list(old_env[[name]]), name))
    }
  }, add = TRUE)
  Sys.setenv(ROHINGYA_ANALYSIS_ROOT = root)
  setwd(tempdir())
  external_wd <- getwd()
  source(file.path(root, "5_analysis_RF105", "reviewed", "geocene_analysis_helpers.R"), chdir = FALSE)
  Sys.unsetenv("RF105_CLEAN_DATA_DIR")
  expected <- normalizePath(file.path(root, "4_data", "clean_final"), winslash = "/")
  stopifnot(identical(geocene_clean_data_root(), expected))
  Sys.setenv(RF105_CLEAN_DATA_DIR = expected)
  stopifnot(identical(geocene_clean_data_root(), expected))
  Sys.setenv(RF105_CLEAN_DATA_DIR = "4_data/clean_final")
  stopifnot(identical(geocene_clean_data_root(), expected))
  source(file.path(root, "5_analysis_RF105", "reviewed", "1_geocene_stove_use_20260805_2213.R"), chdir = FALSE)
  stopifnot(identical(getwd(), external_wd), length(geocene_results) == 2L,
    nrow(geocene_comparison) == 2L)
  message("External-working-directory Geocene analysis and nested figures passed.")
})
