# Check dependencies in a fresh R process with the pipeline's startup settings.
# Run from the project root; no data are read and no packages are installed.
project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
description_path <- file.path(project_root, "DESCRIPTION")
if (!file.exists(description_path)) {
  stop("Missing project dependency metadata: ", description_path, call. = FALSE)
}

dependency_fields <- read.dcf(description_path, fields = c("Depends", "Imports"))
dependency_text <- paste(dependency_fields[!is.na(dependency_fields)], collapse = ",")
dependency_entries <- unlist(strsplit(dependency_text, ",", fixed = TRUE))
dependency_names <- trimws(gsub("\\s*\\(.*\\)", "", dependency_entries))
required_packages <- sort(setdiff(unique(dependency_names[nzchar(dependency_names)]), "R"))

message("Pipeline R library paths: ", paste(.libPaths(), collapse = "; "))
available_packages <- vapply(required_packages, function(package) {
  requireNamespace(package, quietly = TRUE)
}, logical(1))
missing_packages <- required_packages[!available_packages]
if (length(missing_packages)) {
  root_literal <- encodeString(project_root, quote = '"')
  stop(paste(c(
    "The project package library is incomplete.",
    paste("Missing or unloadable required package(s):", paste(missing_packages, collapse = ", ")),
    "Restore the project library before running the pipeline:",
    paste0("  renv::restore(project = ", root_literal, ")"),
    "Then restart R and rerun the full pipeline."
  ), collapse = "\n"), call. = FALSE)
}
message("All ", length(required_packages), " required packages load successfully.")
