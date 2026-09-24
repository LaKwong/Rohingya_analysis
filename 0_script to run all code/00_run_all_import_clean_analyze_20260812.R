################################################################################
# Run all Rohingya import, cleaning, and reviewed RF105 analysis code
#
# Purpose:
#   Run the current raw-first workflow from source data import through cleaned
#   data creation and reviewed RF105 analysis outputs.
#
# Default pipeline:
#   1. Import and clean host survey data.
#   2. Import and clean refugee survey, PM2.5, and Geocene stove-use data.
#   3. Refresh the de-identified public cleaned RF105 analysis data.
#   4. Run the reviewed RF105 analysis pipeline that creates manuscript tables,
#      QA tables, release manifests, restricted QA outputs, and figures.
#
# Optional stages:
#   - Set ROHINGYA_RUN_RESTRICTED_EXCEL_EXPORTS=true to refresh restricted
#     identified Excel QA workbooks. These outputs contain identifiers and remain
#     in 8_restricted.
#
# First-time package setup:
#   renv::restore()
#
# Usage:
#   Rscript "0_script to run all code/00_run_all_import_clean_analyze_20260812.R"
#
# From R/RStudio:
#   source("G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/0_script to run all code/00_run_all_import_clean_analyze_20260812.R")
#
# Validate setup without running the pipeline:
#   Sys.setenv(ROHINGYA_RUN_DRY_RUN = "true")
#   source("G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis/0_script to run all code/00_run_all_import_clean_analyze_20260812.R")
#   Sys.unsetenv("ROHINGYA_RUN_DRY_RUN")
#
# Optional if running from outside the project root:
#   Sys.setenv(ROHINGYA_ANALYSIS_ROOT =
#     "G:/My Drive/Coding in r (lakwong@stanford.edu)/Rohingya_analysis")
################################################################################

# Keep the project working directory active until a sourced run finishes.
local({
get_script_path <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)
  if (length(file_arg) == 1) {
    return(normalizePath(sub("^--file=", "", file_arg),
                         winslash = "/", mustWork = FALSE))
  }

  source_files <- vapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile)) frame$ofile else NA_character_
  }, character(1))
  source_files <- source_files[!is.na(source_files)]
  if (length(source_files) > 0) {
    return(normalizePath(source_files[[length(source_files)]],
                         winslash = "/", mustWork = FALSE))
  }

  NA_character_
}

get_rstudio_active_path <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    return(NA_character_)
  }
  if (!isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(err) FALSE))) {
    return(NA_character_)
  }

  path <- tryCatch(
    rstudioapi::getActiveDocumentContext()$path,
    error = function(err) NA_character_
  )
  if (length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(NA_character_)
  }

  normalizePath(path, winslash = "/", mustWork = FALSE)
}

candidate_with_parents <- function(path, max_depth = 5) {
  if (length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(character())
  }

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (file.exists(path) && !dir.exists(path)) {
    path <- dirname(path)
  }

  candidates <- path
  for (i in seq_len(max_depth)) {
    parent <- dirname(candidates[[length(candidates)]])
    if (identical(parent, candidates[[length(candidates)]])) {
      break
    }
    candidates <- c(candidates, parent)
  }

  unique(candidates)
}

looks_like_project_root <- function(path) {
  if (length(path) != 1 || is.na(path) || !nzchar(path)) {
    return(FALSE)
  }

  all(file.exists(file.path(path, c(
    "1_data_import",
    "3_data_cleaning",
    "5_analysis_RF105",
    "1_run_clean_host_20260805_2141.R",
    "1_run_clean_refugee_20260805_2141.R"
  ))))
}

resolve_project_root <- function() {
  env_root <- Sys.getenv("ROHINGYA_ANALYSIS_ROOT", unset = "")
  if (nzchar(env_root)) {
    env_root <- normalizePath(env_root, winslash = "/", mustWork = TRUE)
    if (!looks_like_project_root(env_root)) {
      stop("ROHINGYA_ANALYSIS_ROOT does not look like the Rohingya_analysis root: ",
           env_root, call. = FALSE)
    }
    return(env_root)
  }

  candidates <- unique(c(
    candidate_with_parents(get_script_path()),
    candidate_with_parents(get_rstudio_active_path()),
    candidate_with_parents(getwd())
  ))
  matches <- candidates[vapply(candidates, looks_like_project_root, logical(1))]
  if (length(matches) > 0) {
    return(matches[[1]])
  }

  stop(
    "Could not determine project root. Run from the Rohingya_analysis root, ",
    "open/source this script from RStudio, or set ROHINGYA_ANALYSIS_ROOT. ",
    "Checked: ", paste(candidates, collapse = "; "),
    call. = FALSE
  )
}
env_flag <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = if (isTRUE(default)) "true" else "false")
  tolower(trimws(value)) %in% c("1", "true", "t", "yes", "y")
}

set_env_default <- function(name, value) {
  if (!nzchar(Sys.getenv(name, unset = ""))) {
    do.call(Sys.setenv, as.list(setNames(value, name)))
  }
  invisible(NULL)
}

timestamp_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
project_root <- resolve_project_root()
old_wd <- setwd(project_root)
on.exit(setwd(old_wd), add = TRUE)

Sys.setenv(ROHINGYA_ANALYSIS_ROOT = project_root)

# Keep threaded libraries conservative for reproducible runs on shared laptops.
invisible(lapply(names(c(
  OMP_NUM_THREADS = "1",
  OMP_THREAD_LIMIT = "1",
  OPENBLAS_NUM_THREADS = "1",
  MKL_NUM_THREADS = "1",
  VECLIB_MAXIMUM_THREADS = "1",
  NUMEXPR_NUM_THREADS = "1",
  RF105_XGB_NTHREAD = "1"
)), function(name) {
  set_env_default(name, c(
    OMP_NUM_THREADS = "1",
    OMP_THREAD_LIMIT = "1",
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1",
    NUMEXPR_NUM_THREADS = "1",
    RF105_XGB_NTHREAD = "1"
  )[[name]])
}))

log_dir <- file.path(project_root, "logs", "run_all")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(log_dir, paste0(
  "run_all_import_clean_analyze_", timestamp_id, ".log"
))
manifest_file <- file.path(log_dir, paste0(
  "run_all_import_clean_analyze_", timestamp_id, "_manifest.csv"
))

log_message <- function(...) {
  text <- paste0(...)
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", text)
  message(line)
  cat(line, "\n", file = log_file, append = TRUE, sep = "")
}

write_manifest <- function(results) {
  if (!length(results)) {
    return(invisible(NULL))
  }
  manifest <- do.call(rbind, lapply(results, as.data.frame))
  utils::write.csv(manifest, manifest_file, row.names = FALSE, na = "")
  invisible(manifest)
}
check_project_dependencies <- function() {
  # RStudio may use a different library than fresh Rscript pipeline processes.
  check_script <- file.path(project_root, "0_script to run all code",
                            "check_pipeline_dependencies.R")
  output <- suppressWarnings(
    system2(rscript, args = shQuote(check_script), stdout = TRUE, stderr = TRUE)
  )
  if (length(output)) {
    cat(paste(output, collapse = "\n"), "\n", file = log_file, append = TRUE, sep = "")
  }
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L) {
    stop(paste(c("Pipeline dependency check failed before imports started.",
                 output, paste("See log:", log_file)), collapse = "\n"), call. = FALSE)
  }
  log_message("Dependency check passed in the pipeline Rscript environment.")
  invisible(TRUE)
}

rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") {
  "Rscript.exe"
} else {
  "Rscript"
})
if (!file.exists(rscript)) {
  rscript <- "Rscript"
}

core_steps <- data.frame(
  key = c("host_clean", "refugee_clean", "public_clean_export", "rf105_reviewed"),
  stage = c("import_clean", "import_clean", "clean_public", "analysis"),
  label = c(
    "Import and clean host survey data",
    "Import and clean refugee survey, PM2.5, and Geocene stove-use data",
    "Create de-identified public cleaned RF105 analysis data",
    "Run reviewed RF105 analyses and manuscript outputs"
  ),
  script = c(
    "1_run_clean_host_20260805_2141.R",
    "1_run_clean_refugee_20260805_2141.R",
    "3_data_cleaning/fixed/create_public_clean_final_20260806_1815.R",
    "5_analysis_RF105/reviewed/00_run_RF105_20260805_2213.R"
  ),
  stringsAsFactors = FALSE
)

optional_steps <- data.frame(
  key = character(),
  stage = character(),
  label = character(),
  script = character(),
  stringsAsFactors = FALSE
)


if (env_flag("ROHINGYA_RUN_RESTRICTED_EXCEL_EXPORTS")) {
  optional_steps <- rbind(optional_steps, data.frame(
    key = c("restricted_midline_excel", "restricted_baseline_no_midline_excel"),
    stage = c("restricted_qa", "restricted_qa"),
    label = c(
      "Export restricted identified midline survey QA workbooks",
      "Export restricted identified baseline-without-midline survey QA workbook"
    ),
    script = c(
      "5_analysis_RF105/reviewed/6.1_export_midline_survey_excel_20260812.R",
      "5_analysis_RF105/reviewed/6.2_export_baseline_no_midline_survey_excel_20260812.R"
    ),
    stringsAsFactors = FALSE
  ))
}

steps <- rbind(
  core_steps,
  optional_steps
)

validate_steps <- function(steps) {
  missing_scripts <- steps$script[!file.exists(file.path(project_root, steps$script))]
  if (length(missing_scripts) > 0) {
    stop("Missing pipeline script(s): ",
         paste(missing_scripts, collapse = "; "), call. = FALSE)
  }
  invisible(TRUE)
}

run_r_script <- function(step) {
  script_path <- normalizePath(file.path(project_root, step$script),
                               winslash = "/", mustWork = TRUE)
  start_time <- Sys.time()

  log_message("START ", step$key, " - ", step$label)
  log_message("SCRIPT ", script_path)

  output <- character()
  status <- 0L
  error_message <- NA_character_

  output <- withCallingHandlers(
    tryCatch(
      system2(rscript, args = c(shQuote(script_path)), stdout = TRUE, stderr = TRUE),
      error = function(err) {
        status <<- 1L
        error_message <<- conditionMessage(err)
        character()
      }
    ),
    warning = function(warn) {
      invokeRestart("muffleWarning")
    }
  )

  script_status <- attr(output, "status")
  if (!is.null(script_status)) {
    status <- as.integer(script_status)
  }

  if (length(output) > 0) {
    cat(paste(output, collapse = "\n"), "\n",
        file = log_file, append = TRUE, sep = "")
  }
  if (!is.na(error_message)) {
    cat("ERROR: ", error_message, "\n",
        file = log_file, append = TRUE, sep = "")
  }

  end_time <- Sys.time()
  elapsed_minutes <- round(
    as.numeric(difftime(end_time, start_time, units = "mins")),
    2
  )

  result <- list(
    key = step$key,
    stage = step$stage,
    label = step$label,
    script = step$script,
    status = if (identical(status, 0L)) "success" else "failed",
    exit_status = status,
    started_at = format(start_time, "%Y-%m-%d %H:%M:%S"),
    finished_at = format(end_time, "%Y-%m-%d %H:%M:%S"),
    elapsed_minutes = elapsed_minutes,
    log_file = normalizePath(log_file, winslash = "/", mustWork = FALSE)
  )

  log_message("END ", step$key, " - ", result$status,
              " (", elapsed_minutes, " minutes)")

  result
}

validate_steps(steps)

log_message("Project root: ", project_root)
log_message("Rscript: ", rscript)
log_message("Log file: ", log_file)
log_message("Manifest file: ", manifest_file)
log_message("Steps: ", paste(steps$key, collapse = ", "))
check_project_dependencies()

if (env_flag("ROHINGYA_RUN_DRY_RUN")) {
  dry_results <- lapply(seq_len(nrow(steps)), function(i) {
    list(
      key = steps$key[[i]],
      stage = steps$stage[[i]],
      label = steps$label[[i]],
      script = steps$script[[i]],
      status = "validated_not_run",
      exit_status = NA_integer_,
      started_at = NA_character_,
      finished_at = NA_character_,
      elapsed_minutes = NA_real_,
      log_file = normalizePath(log_file, winslash = "/", mustWork = FALSE)
    )
  })
  write_manifest(dry_results)
  log_message("Dry run complete. Validated paths and run order; no pipeline scripts were executed.")
  log_message("Run manifest written to: ", manifest_file)
} else {
  results <- list()
  for (i in seq_len(nrow(steps))) {
    result <- run_r_script(steps[i, ])
    results[[length(results) + 1]] <- result
    write_manifest(results)

    if (!identical(result$exit_status, 0L)) {
      stop("Pipeline failed at step `", result$key, "`. See log: ",
           log_file, call. = FALSE)
    }
  }

  log_message("Pipeline complete.")
  log_message("Run manifest written to: ", manifest_file)
}
})
