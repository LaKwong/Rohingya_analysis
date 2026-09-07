# Keep renv package libraries out of the Google Drive project tree.
# This avoids Windows path-length and sync issues for packages with deep folders.
local({
  local_app_data <- Sys.getenv("LOCALAPPDATA", unset = "")
  renv_root <- if (.Platform$OS.type == "windows" && nzchar(local_app_data)) {
    file.path(local_app_data, "R", "renv")
  } else {
    file.path("~", ".local", "share", "renv")
  }

  set_renv_path_default <- function(name, path) {
    if (!nzchar(Sys.getenv(name, unset = ""))) {
      path <- normalizePath(path, winslash = "/", mustWork = FALSE)
      args <- list(path); names(args) <- name; do.call(Sys.setenv, args)
    }
    invisible(NULL)
  }

  set_renv_path_default(
    "RENV_PATHS_LIBRARY_ROOT",
    file.path(renv_root, "project-library")
  )
  set_renv_path_default(
    "RENV_PATHS_LIBRARY_STAGING",
    file.path(renv_root, "staging")
  )
})

source("renv/activate.R")