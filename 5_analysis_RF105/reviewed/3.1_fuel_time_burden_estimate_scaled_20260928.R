################################################################################
# Estimate-scaled household fuel-time burden figure
#
# Purpose:
#   Create a version of the fuel-time burden figure whose panel ranges retain
#   every estimate without allowing a small number of extreme upper confidence
#   limits to determine the y-axis scales. Clipped upper limits extend to the
#   panel boundary and are labeled with their full values.
#
# Input:
#   7_tables/RF105_reviewed_YYYYMMDD/
#     table_descriptive_fuel_time_burden_plot_data_100_80_5_20.csv
#
# Outputs:
#   6_figures/RF105_reviewed_YYYYMMDD/
#     fig_descriptive_fuel_time_burden_components_estimate_scaled_100_80_5_20.png
#   7_tables/RF105_reviewed_YYYYMMDD/qa/
#     table_descriptive_fuel_time_burden_clipped_upper_ci_100_80_5_20.csv
#
# Expected use from the project root:
#   source("5_analysis_RF105/reviewed/3.1_fuel_time_burden_estimate_scaled_20260928.R")
################################################################################

get_fuel_time_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) == 1L) {
    return(dirname(normalizePath(
      sub("^--file=", "", file_arg),
      winslash = "/",
      mustWork = FALSE
    )))
  }

  source_files <- vapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile)) frame$ofile else NA_character_
  }, character(1))
  source_files <- source_files[!is.na(source_files)]

  if (length(source_files) > 0) {
    return(dirname(normalizePath(
      source_files[[length(source_files)]],
      winslash = "/",
      mustWork = FALSE
    )))
  }

  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

fuel_time_script_dir <- get_fuel_time_script_dir()
if (!exists("dir_tables_reviewed", inherits = TRUE) ||
    !exists("save_reviewed_plot", inherits = TRUE)) {
  source(file.path(
    fuel_time_script_dir,
    "0_RF105_config_20260805_2213.R"
  ))
}

if (!requireNamespace("gridExtra", quietly = TRUE)) {
  stop(
    "Package `gridExtra` is required to assemble the estimate-scaled figure. ",
    "Run renv::restore() from the project root, then rerun.",
    call. = FALSE
  )
}

fuel_time_variant <- "100_80_5_20"
fuel_time_input_file <- file.path(
  dir_tables_reviewed,
  paste0(
    "table_descriptive_fuel_time_burden_plot_data_",
    fuel_time_variant,
    ".csv"
  )
)

if (!file.exists(fuel_time_input_file)) {
  stop(
    "Required fuel-time plot-data table is missing: ", fuel_time_input_file,
    "\nRun 3_descriptive_outcomes_20260805_2213.R first.",
    call. = FALSE
  )
}

fuel_time_panel_limits <- tribble(
  ~panel_order, ~panel, ~panel_y_max,
  1L, "A. Fuel procurement frequency (times/week)", 5,
  2L, "B. Two-way walking time (minutes/procurement event)", 500,
  3L, "C. Additional procurement time (minutes/procurement event)", 160,
  4L, "D. Stove-on time (minutes/day)", 320,
  5L, "E. Pot-cleaning time (minutes/day)", 55
)

fuel_time_component_colors <- c(
  "Biomass" = "#D55E00",
  "LPG" = "#0072B2",
  "Biomass component" = "#A65628",
  "LPG component" = "#56B4E9",
  "Wood collection" = "#CC79A7",
  "Wood preparation" = "#E69F00",
  "Waiting in line for LPG" = "#009E73"
)

fuel_time_component_order <- names(fuel_time_component_colors)
fuel_time_shape_values <- c(
  "Survey reported" = 16,
  "Published refill schedule" = 17,
  "Prespecified duration" = 15,
  "Stove-use monitor" = 18
)

fuel_time_plot_data <- readr::read_csv(
  fuel_time_input_file,
  show_col_types = FALSE
) %>%
  filter(display_in_figure, !is.na(mean)) %>%
  left_join(fuel_time_panel_limits, by = c("panel_order", "panel")) %>%
  mutate(
    panel = factor(panel, levels = fuel_time_panel_limits$panel),
    group_label = factor(group_label, levels = unique(group_label)),
    component = factor(component, levels = fuel_time_component_order),
    estimate_basis_figure = case_when(
      estimate_basis == "SAFE Plus schedule" ~ "Published refill schedule",
      estimate_basis == "Prespecified assumption" ~ "Prespecified duration",
      str_detect(estimate_basis, "Stove-use monitor") ~ "Stove-use monitor",
      TRUE ~ "Survey reported"
    ),
    estimate_basis_figure = factor(
      estimate_basis_figure,
      levels = names(fuel_time_shape_values)
    ),
    plot_ci_lower = pmax(0, ci_lower),
    upper_ci_clipped = is.finite(ci_upper) & ci_upper > panel_y_max,
    upper_ci_label = if_else(
      upper_ci_clipped,
      paste0(
        "Upper 95% CI: ",
        scales::number(ci_upper, accuracy = 0.1, big.mark = ",")
      ),
      NA_character_
    )
  )

if (anyNA(fuel_time_plot_data$panel_y_max)) {
  stop("At least one fuel-time panel is missing a prespecified y-axis maximum.", call. = FALSE)
}
if (any(fuel_time_plot_data$mean > fuel_time_plot_data$panel_y_max, na.rm = TRUE)) {
  stop("A fuel-time estimate exceeds its panel y-axis maximum.", call. = FALSE)
}

fuel_time_clipped_ci <- fuel_time_plot_data %>%
  filter(upper_ci_clipped) %>%
  transmute(
    panel_order,
    panel = as.character(panel),
    timepoint,
    study_arm_overall,
    stove_use_category,
    component = as.character(component),
    group_label = as.character(group_label),
    mean,
    ci_lower,
    ci_upper,
    panel_y_max,
    figure_note = upper_ci_label
  )

write_reviewed_csv(
  fuel_time_clipped_ci,
  paste0(
    "table_descriptive_fuel_time_burden_clipped_upper_ci_",
    fuel_time_variant,
    ".csv"
  ),
  subfolder = "qa"
)

make_fuel_time_panel <- function(panel_name, panel_y_max, show_legend = FALSE) {
  panel_data <- fuel_time_plot_data %>%
    filter(as.character(panel) == panel_name)
  clipped_data <- panel_data %>%
    filter(upper_ci_clipped) %>%
    mutate(
      label_hjust = if_else(
        as.integer(group_label) >= 0.8 * n_distinct(panel_data$group_label),
        1,
        0.5
      ),
      label_y = panel_y_max * 0.98
    )

  ggplot(
    panel_data,
    aes(
      x = group_label,
      y = mean,
      color = component,
      shape = estimate_basis_figure
    )
  ) +
    geom_errorbar(
      aes(ymin = plot_ci_lower, ymax = ci_upper),
      position = position_dodge(width = 0.65),
      width = 0.18,
      linewidth = 0.4
    ) +
    geom_point(position = position_dodge(width = 0.65), size = 2.1) +
    geom_text(
      data = clipped_data,
      aes(
        x = group_label,
        y = label_y,
        label = upper_ci_label,
        hjust = label_hjust
      ),
      inherit.aes = FALSE,
      vjust = 1.1,
      size = 2.7,
      color = "black"
    ) +
    facet_wrap(~ panel, ncol = 1) +
    scale_color_manual(
      values = fuel_time_component_colors,
      breaks = fuel_time_component_order,
      drop = FALSE
    ) +
    scale_shape_manual(
      values = fuel_time_shape_values,
      breaks = names(fuel_time_shape_values),
      drop = FALSE
    ) +
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE, order = 1),
      shape = guide_legend(order = 2)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    coord_cartesian(
      ylim = c(0, panel_y_max),
      expand = c(bottom = FALSE, left = TRUE, top = FALSE, right = TRUE),
      clip = "on"
    ) +
    theme_classic(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
      strip.text = element_text(face = "bold", size = 9),
      legend.position = if (isTRUE(show_legend)) "bottom" else "none",
      legend.box = "vertical",
      plot.margin = margin(3, 5.5, 3, 5.5)
    ) +
    labs(
      x = NULL,
      y = NULL,
      color = "Fuel or component",
      shape = "Source of data"
    )
}

extract_fuel_time_legend <- function(plot) {
  plot_grob <- ggplotGrob(plot + theme(legend.position = "bottom"))
  legend_index <- which(
    grepl("^guide-box", plot_grob$layout$name) &
      !vapply(plot_grob$grobs, inherits, logical(1), what = "zeroGrob")
  )
  if (length(legend_index) != 1L) {
    stop("Expected exactly one fuel-time legend grob.", call. = FALSE)
  }
  plot_grob$grobs[[legend_index]]
}

fuel_time_panels <- Map(
  function(panel_name, panel_y_max) {
    make_fuel_time_panel(panel_name, panel_y_max, show_legend = FALSE)
  },
  fuel_time_panel_limits$panel,
  fuel_time_panel_limits$panel_y_max
)

fuel_time_legend <- extract_fuel_time_legend(make_fuel_time_panel(
  fuel_time_panel_limits$panel[[1]],
  fuel_time_panel_limits$panel_y_max[[1]],
  show_legend = TRUE
))

fuel_time_title <- grid::textGrob(
  "Household time associated with fuel procurement, cooking, and cleaning",
  x = 0,
  hjust = 0,
  gp = grid::gpar(fontsize = 12)
)
fuel_time_caption <- grid::textGrob(
  paste0(
    "Upper 95% confidence limits extending beyond a panel's y-axis are ",
    "shown by a line reaching the upper boundary and labeled with their full value."
  ),
  x = 0,
  hjust = 0,
  gp = grid::gpar(fontsize = 8)
)

fig_fuel_time_burden_estimate_scaled <- gridExtra::arrangeGrob(
  grobs = c(
    list(fuel_time_title),
    lapply(fuel_time_panels, ggplotGrob),
    list(fuel_time_legend, fuel_time_caption)
  ),
  ncol = 1,
  heights = c(0.1, 1, 1, 1, 1.2, 1.2, 0.45, 0.1)
)

save_reviewed_plot(
  fig_fuel_time_burden_estimate_scaled,
  paste0(
    "fig_descriptive_fuel_time_burden_components_estimate_scaled_",
    fuel_time_variant,
    ".png"
  ),
  width = 11,
  height = 17,
  units = "in",
  dpi = 300
)

message("Estimate-scaled fuel-time burden figure complete.")
