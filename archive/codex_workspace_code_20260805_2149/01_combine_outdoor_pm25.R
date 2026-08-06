# Purpose: Combine outdoor PM2.5 CSV files and create a time-series figure.
# Inputs:  ALL PM 2.5 Outdoor data/*.csv
# Outputs: data/processed/outdoor_pm25_combined.rds
#          outputs/tables/outdoor_pm25_file_metadata.csv
#          outputs/figures/outdoor_pm25_by_location.png

script_file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)

project_root <- if (length(script_file_arg) > 0) {
  script_path <- normalizePath(
    sub("^--file=", "", script_file_arg[1]),
    winslash = "/",
    mustWork = TRUE
  )
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

data_dir <- file.path(project_root, "ALL PM 2.5 Outdoor data")
output_data_dir <- file.path(project_root, "data", "processed")
output_table_dir <- file.path(project_root, "outputs", "tables")
output_figure_dir <- file.path(project_root, "outputs", "figures")

dir.create(output_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_figure_dir, recursive = TRUE, showWarnings = FALSE)

output_rds <- file.path(output_data_dir, "outdoor_pm25_combined.rds")
output_metadata <- file.path(output_table_dir, "outdoor_pm25_file_metadata.csv")
output_plot <- file.path(output_figure_dir, "outdoor_pm25_by_location.png")

# Timestamps appear to be local field time. Change this if the monitor clock used
# another time zone.
time_zone <- "Asia/Dhaka"

csv_files <- list.files(
  data_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(csv_files) == 0) {
  stop("No CSV files found in: ", data_dir)
}

parse_file_metadata <- function(path) {
  source_file <- basename(path)
  stem <- sub("\\.[Cc][Ss][Vv]$", "", source_file)
  prefix_index <- if (grepl("^[0-9]+_", stem)) {
    as.integer(sub("^([0-9]+)_.*$", "\\1", stem))
  } else {
    NA_integer_
  }
  stem_no_prefix <- sub("^[0-9]+_", "", stem)

  file_pattern <- paste0(
    "^(PM[^_]+)_",
    "([0-9]{8})_",
    "([01Ii])_",
    "([[:alnum:]]{5,6})",
    "(?:_(.*))?$"
  )

  match <- regexec(file_pattern, stem_no_prefix, perl = TRUE)
  pieces <- regmatches(stem_no_prefix, match)[[1]]

  if (length(pieces) == 0) {
    warning("Could not parse filename metadata for: ", source_file)
    return(data.frame(
      source_file = source_file,
      file_index = prefix_index,
      monitor_name = NA_character_,
      sample_start_date = as.Date(NA),
      study_arm_code_raw = NA_character_,
      study_arm_code = NA_character_,
      study_arm = NA_character_,
      location_code = NA_character_,
      location_type = NA_character_,
      parsed_letter_i_as_one = NA,
      filename_parse_ok = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  arm_code_raw <- pieces[4]
  arm_code <- if (toupper(arm_code_raw) == "I") "1" else arm_code_raw
  study_arm <- if (arm_code == "0") {
    "intervention"
  } else if (arm_code == "1") {
    "comparison"
  } else {
    NA_character_
  }

  data.frame(
    source_file = source_file,
    file_index = prefix_index,
    monitor_name = pieces[2],
    sample_start_date = as.Date(pieces[3], format = "%Y%m%d"),
    study_arm_code_raw = arm_code_raw,
    study_arm_code = arm_code,
    study_arm = study_arm,
    location_code = pieces[5],
    location_type = if (length(pieces) >= 6) pieces[6] else NA_character_,
    parsed_letter_i_as_one = toupper(arm_code_raw) == "I",
    filename_parse_ok = TRUE,
    stringsAsFactors = FALSE
  )
}

find_data_header_line <- function(path) {
  lines <- readLines(path, warn = FALSE)
  header_line <- grep("^\\s*dateTime\\s*,", lines)

  if (length(header_line) == 0) {
    stop("Could not find the data header row beginning with 'dateTime' in: ", basename(path))
  }

  header_line[1]
}

parse_monitor_datetime <- function(x) {
  x <- trimws(as.character(x))
  parsed <- as.POSIXct(rep(NA_character_, length(x)), tz = time_zone)
  formats <- c(
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%m/%d/%y %H:%M:%S",
    "%m/%d/%y %H:%M"
  )

  for (one_format in formats) {
    needs_parse <- is.na(parsed) & !is.na(x) & nzchar(x)
    if (!any(needs_parse)) {
      break
    }
    parsed[needs_parse] <- as.POSIXct(
      x[needs_parse],
      format = one_format,
      tz = time_zone
    )
  }

  parsed
}

read_pm_file <- function(path) {
  metadata <- parse_file_metadata(path)
  header_line <- find_data_header_line(path)

  dat <- read.csv(
    path,
    skip = header_line - 1,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )

  if (!"dateTime" %in% names(dat)) {
    stop("Expected a 'dateTime' column in: ", basename(path))
  }

  if (!"PM_Estimate" %in% names(dat)) {
    stop("Expected a 'PM_Estimate' PM2.5 column in: ", basename(path))
  }

  dat$date_time <- parse_monitor_datetime(dat[["dateTime"]])
  dat$pm25_ug_m3 <- suppressWarnings(as.numeric(dat[["PM_Estimate"]]))

  metadata_rows <- metadata[rep(1, nrow(dat)), , drop = FALSE]
  row.names(metadata_rows) <- NULL
  row.names(dat) <- NULL

  cbind(metadata_rows, dat)
}

bind_rows_base <- function(dfs) {
  all_names <- unique(unlist(lapply(dfs, names), use.names = FALSE))

  dfs_aligned <- lapply(dfs, function(x) {
    missing_names <- setdiff(all_names, names(x))
    for (missing_name in missing_names) {
      x[[missing_name]] <- NA
    }
    x[all_names]
  })

  out <- do.call(rbind, dfs_aligned)
  row.names(out) <- NULL
  out
}

message("Reading ", length(csv_files), " CSV files...")
pm_files <- lapply(csv_files, read_pm_file)
outdoor_pm25 <- bind_rows_base(pm_files)
outdoor_pm25 <- outdoor_pm25[order(outdoor_pm25$date_time, outdoor_pm25$source_file), ]
row.names(outdoor_pm25) <- NULL

metadata_columns <- c(
  "source_file",
  "file_index",
  "monitor_name",
  "sample_start_date",
  "study_arm_code_raw",
  "study_arm_code",
  "study_arm",
  "location_code",
  "location_type",
  "parsed_letter_i_as_one",
  "filename_parse_ok"
)

summarize_file <- function(dat) {
  out <- dat[1, metadata_columns, drop = FALSE]

  valid_time <- !is.na(dat$date_time)
  valid_pm <- !is.na(dat$pm25_ug_m3)

  out$n_rows <- nrow(dat)
  out$n_missing_date_time <- sum(!valid_time)
  out$n_missing_pm25 <- sum(!valid_pm)
  out$first_date_time <- if (any(valid_time)) {
    format(min(dat$date_time[valid_time]), "%Y-%m-%d %H:%M:%S %Z")
  } else {
    NA_character_
  }
  out$last_date_time <- if (any(valid_time)) {
    format(max(dat$date_time[valid_time]), "%Y-%m-%d %H:%M:%S %Z")
  } else {
    NA_character_
  }
  out$min_pm25_ug_m3 <- if (any(valid_pm)) min(dat$pm25_ug_m3[valid_pm]) else NA_real_
  out$max_pm25_ug_m3 <- if (any(valid_pm)) max(dat$pm25_ug_m3[valid_pm]) else NA_real_

  out
}

file_metadata <- do.call(
  rbind,
  lapply(split(outdoor_pm25, outdoor_pm25$source_file), summarize_file)
)
row.names(file_metadata) <- NULL

saveRDS(outdoor_pm25, output_rds)
write.csv(file_metadata, output_metadata, row.names = FALSE)

plot_data <- outdoor_pm25[
  !is.na(outdoor_pm25$date_time) & !is.na(outdoor_pm25$pm25_ug_m3),
]

if (nrow(plot_data) == 0) {
  stop("No rows with both date_time and pm25_ug_m3 were available for plotting.")
}

if (requireNamespace("ggplot2", quietly = TRUE)) {
  pm_plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = date_time,
      y = pm25_ug_m3,
      color = location_code,
      group = source_file
    )
  ) +
    ggplot2::geom_line(linewidth = 0.25, alpha = 0.65) +
    ggplot2::labs(
      x = paste0("Time (", time_zone, ")"),
      y = "PM2.5 concentration (ug/m^3)",
      color = "Location",
      title = "Outdoor PM2.5 by Location"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title.position = "plot"
    )

  ggplot2::ggsave(
    filename = output_plot,
    plot = pm_plot,
    width = 12,
    height = 7,
    dpi = 300
  )
} else {
  location_levels <- sort(unique(plot_data$location_code))
  location_colors <- stats::setNames(
    grDevices::hcl.colors(length(location_levels), palette = "Dark 3"),
    location_levels
  )

  grDevices::png(output_plot, width = 12, height = 7, units = "in", res = 300)
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(5, 5, 3, 1) + 0.1)

  graphics::plot(
    range(plot_data$date_time),
    range(plot_data$pm25_ug_m3, na.rm = TRUE),
    type = "n",
    xlab = paste0("Time (", time_zone, ")"),
    ylab = "PM2.5 concentration (ug/m^3)",
    main = "Outdoor PM2.5 by Location"
  )

  for (source_file in unique(plot_data$source_file)) {
    one_file <- plot_data[plot_data$source_file == source_file, ]
    one_file <- one_file[order(one_file$date_time), ]
    line_color <- grDevices::adjustcolor(
      location_colors[[one_file$location_code[1]]],
      alpha.f = 0.65
    )
    graphics::lines(one_file$date_time, one_file$pm25_ug_m3, col = line_color, lwd = 0.6)
  }

  graphics::legend(
    "topright",
    legend = location_levels,
    col = location_colors,
    lty = 1,
    lwd = 2,
    title = "Location",
    bty = "n",
    cex = 0.8
  )

  grDevices::dev.off()
}

message("Saved combined data to: ", output_rds)
message("Saved file metadata to: ", output_metadata)
message("Saved plot to: ", output_plot)

