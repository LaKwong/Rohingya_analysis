# Regression: a package visible in the parent must not mask a missing child dependency.
# Run from the repository root: Rscript --vanilla tests/test_pipeline_dependencies.R
local({
  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  old_env <- Sys.getenv(c("ROHINGYA_ANALYSIS_ROOT", "ROHINGYA_RUN_DRY_RUN",
                         "ROHINGYA_RUN_RESTRICTED_EXCEL_EXPORTS"), unset = NA_character_)
  on.exit({
    setwd(root)
    for (name in names(old_env)) {
      if (is.na(old_env[[name]])) Sys.unsetenv(name) else {
        do.call(Sys.setenv, setNames(list(old_env[[name]]), name))
      }
    }
  }, add = TRUE)
  Sys.unsetenv("ROHINGYA_RUN_RESTRICTED_EXCEL_EXPORTS")
  runner <- "0_script to run all code/00_run_all_import_clean_analyze_20260812.R"
  checker <- "0_script to run all code/check_pipeline_dependencies.R"

  make_fixture <- function(imports) {
    fixture <- tempfile("pipeline dependency test ")
    dir.create(fixture)
    fixture <- normalizePath(fixture, winslash = "/", mustWork = TRUE)
    for (dir in c("1_data_import", "3_data_cleaning/fixed", "5_analysis_RF105/reviewed",
                  "0_script to run all code")) dir.create(file.path(fixture, dir), recursive = TRUE)
    stopifnot(file.copy(file.path(root, runner), file.path(fixture, runner)),
              file.copy(file.path(root, checker), file.path(fixture, checker)))
    writeLines(c("Package: PipelineTest", "Depends: R (>= 4.5.0)", paste("Imports:", imports)),
               file.path(fixture, "DESCRIPTION"))
    writeLines('message("Synthetic project profile loaded")', file.path(fixture, ".Rprofile"))
    for (file in c("1_run_clean_host_20260805_2141.R", "1_run_clean_refugee_20260805_2141.R",
                   "3_data_cleaning/fixed/create_public_clean_final_20260806_1815.R",
                   "5_analysis_RF105/reviewed/00_run_RF105_20260805_2213.R")) {
      writeLines('stop("Pipeline stage should not execute in this test")', file.path(fixture, file))
    }
    fixture
  }

  run_fixture <- function(fixture, dry_run) {
    Sys.setenv(ROHINGYA_ANALYSIS_ROOT = fixture, ROHINGYA_RUN_DRY_RUN = dry_run)
    env <- new.env(parent = globalenv())
    # Simulate a parent whose dependency checks all pass (for example, RStudio).
    env$requireNamespace <- function(...) TRUE
    setwd(root)
    error <- tryCatch({ source(file.path(fixture, runner), local = env); NULL },
                      error = function(e) conditionMessage(e))
    stopifnot(identical(normalizePath(getwd(), winslash = "/"), root))
    error
  }

  missing <- make_fixture("stats, codexMissingPipelineDependency")
  error <- run_fixture(missing, "false")
  stopifnot(!is.null(error), grepl("codexMissingPipelineDependency", error, fixed = TRUE),
            grepl("before imports started", error, fixed = TRUE),
            grepl("renv::restore(project =", error, fixed = TRUE),
            !grepl("Pipeline stage should not execute", error, fixed = TRUE))
  log <- list.files(file.path(missing, "logs/run_all"), pattern = "[.]log$", full.names = TRUE)
  stopifnot(length(log) == 1L,
            any(grepl("Synthetic project profile loaded", readLines(log), fixed = TRUE)),
            length(list.files(file.path(missing, "logs/run_all"), pattern = "manifest")) == 0L)

  available <- make_fixture("stats, utils (>= 4.0.0)")
  stopifnot(is.null(run_fixture(available, "true")))
  manifest <- list.files(file.path(available, "logs/run_all"), pattern = "manifest[.]csv$", full.names = TRUE)
  stopifnot(length(manifest) == 1L)
  result <- read.csv(manifest)
  stopifnot(nrow(result) == 4L, all(result$status == "validated_not_run"))
  message("Child-process missing-dependency and successful dry-run checks passed.")
})
