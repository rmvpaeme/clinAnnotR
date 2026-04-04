#' clinAnnotR: Publication-Quality Clinical Time-Series Figures
#'
#' @description
#' Generates multi-panel figures combining arbitrary numerical laboratory
#' time-series with per-case treatment Gantt charts. Any parameter can be
#' displayed as a line or point series on log\u2081\u2080 or linear scales,
#' with below-detection-limit (BDL) handling and optional direct value labels.
#'
#' ## Typical workflow
#'
#' ```r
#' # 1. Load data
#' lab <- load_lab_data("labvals.xlsx", cases)
#' tx  <- load_treatment_data("treatment.xlsx", cases)
#'
#' # 2. Define panels
#' panels <- list(
#'   lab_panel(line_params  = "WBC (/\u00b5L)",
#'             point_params = "peripheral blasts (/\u00b5L)",
#'             y_label      = "Count (/\u00b5L)"),
#'   lab_panel(point_params = "BM blasts with IF (%)",
#'             show_labels  = TRUE, label_suffix = "%",
#'             y_label      = "BM blasts (%)")
#' )
#'
#' # 3. Assemble and save
#' fig <- make_clinical_figure(lab, tx, panels)
#' save_clinical_figure(fig, "figure.pdf")
#' ```
#'
#' ## Key functions
#'
#' * [load_lab_data()] / [prep_lab_data()] — import lab measurements
#' * [load_treatment_data()] / [prep_treatment_data()] — import treatment records
#' * [example_data()] — built-in fictional example datasets
#' * [lab_panel()] — construct a panel specification
#' * [make_clinical_figure()] — assemble the full figure
#' * [make_timeseries_panel()] — build a single lab panel (power users)
#' * [make_gantt_panel()] — build a treatment Gantt panel (power users)
#' * [theme_clinical()] / [named_vlines()] — theme helpers
#' * [save_clinical_figure()] — save to file
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import ggplot2
#' @importFrom dplyr arrange desc first group_by left_join mutate select
#'   summarise ungroup filter
#' @importFrom ggrepel geom_text_repel
#' @importFrom patchwork plot_layout plot_annotation wrap_plots
#' @importFrom readxl read_excel
#' @importFrom rlang .data
#' @importFrom scales cut_short_scale label_number
#' @importFrom tools file_ext
## usethis namespace: end
NULL
