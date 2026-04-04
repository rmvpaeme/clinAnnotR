#' Construct a lab panel specification
#'
#' Builds a specification object describing one time-series panel for use in
#' [make_clinical_figure()]. Each spec controls which parameters are shown,
#' how the y-axis is scaled, and all visual details for that panel. The
#' specification is a plain named list of class `"lab_panel_spec"`; no plotting
#' occurs here.
#'
#' @param line_params Character vector. Parameter values (from `lab_data$parameter`)
#'   to display as connected lines. Each distinct parameter receives a unique
#'   linetype. Default: `character(0)` (no lines).
#' @param point_params Character vector. Parameters to display as points. Each
#'   distinct parameter receives a unique point shape. Below-detection-limit
#'   (BDL) observations are rendered with a dedicated symbol regardless of
#'   parameter. Default: `character(0)` (no points).
#' @param y_scale Character. `"log10"` for a logarithmic y-axis (recommended for
#'   counts and percentages spanning several orders of magnitude) or `"linear"`
#'   for a standard linear axis. Default: `"log10"`.
#' @param y_label Character. y-axis title. `NULL` joins all parameter names with
#'   `" / "` as an automatic label. Default: `NULL`.
#' @param y_limits Numeric vector of length 2 `c(lo, hi)`. `NULL` = auto-computed
#'   from the data (rounded to powers of 10 for log scale). Default: `NULL`.
#' @param y_breaks Numeric vector of break positions. `NULL` = default breaks
#'   (powers of 10 for log scale; `ggplot2` default for linear). Default: `NULL`.
#' @param y_labels Character vector or labelling function for axis tick labels.
#'   `NULL` = SI-suffixed numbers for log scale, default for linear.
#'   Default: `NULL`.
#' @param bdl_floor Numeric. Value assigned to BDL observations for log-axis
#'   rendering. Overrides the global `bdl_floor` in [make_clinical_figure()]
#'   for this panel only. `NULL` = use global default. Default: `NULL`.
#' @param show_labels Logical. Draw value labels next to each non-BDL point
#'   using [ggrepel::geom_text_repel()]? Useful for sparse panels such as bone
#'   marrow blast percentages. Default: `FALSE`.
#' @param label_suffix Character appended to every value label (e.g. `"%"` for
#'   percentage parameters). Default: `""`.
#' @param label_size Numeric text size for value labels. Default: `2.8`.
#' @param label_nudge_y Numeric. `nudge_y` for detected-value labels (additive
#'   on the log10 scale when `y_scale = "log10"`). Default: `0.18`.
#' @param label_nudge_y_bdl Numeric. `nudge_y` for BDL labels. Default: `-0.25`.
#' @param label_segment_size Numeric. Width of the ggrepel connector segment.
#'   Default: `0.3`.
#' @param label_min_segment Numeric. Minimum segment length below which no
#'   connector is drawn. Default: `0.2`.
#' @param highlight_days Named numeric vector of reference line positions for
#'   this panel only. `NULL` = inherit the global `highlight_days` argument of
#'   [make_clinical_figure()]. Default: `NULL`.
#' @param vline_labels Logical. Show rotated day labels on reference lines?
#'   Default: `TRUE`.
#' @param line_linewidth Numeric. Stroke width for line geometries. Default: `0.65`.
#' @param point_size Numeric. Size of detected (non-BDL) point geometries.
#'   Default: `2.5`.
#' @param bdl_shape Integer. ggplot2 shape code for BDL points. Shape `6` is an
#'   open downward-pointing triangle (∇). Default: `6L`.
#' @param bdl_size Numeric. Size of BDL points. Default: `2.5`.
#' @param logticks Logical. Annotate log10 panels with minor log ticks
#'   on the left axis ([ggplot2::annotation_logticks()])? Default: `TRUE`.
#' @param height_weight Positive numeric. Relative panel height in the assembled
#'   figure (passed to [patchwork::plot_layout()]). Default: `3`.
#'
#' @return A named list of class `c("lab_panel_spec", "list")`.
#'
#' @examples
#' # WBC line + peripheral blast points on a shared log10 count axis
#' lab_panel(
#'   line_params  = "WBC (/µL)",
#'   point_params = "peripheral blasts (/µL)",
#'   y_label      = "Count (/µL)",
#'   height_weight = 4
#' )
#'
#' # BM blast percentages with direct value labels
#' lab_panel(
#'   point_params  = "BM blasts with IF (%)",
#'   show_labels   = TRUE,
#'   label_suffix  = "%",
#'   y_limits      = c(0.05, 200),
#'   y_breaks      = c(0.1, 1, 5, 20, 100),
#'   y_labels      = paste0(c(0.1, 1, 5, 20, 100), "%"),
#'   y_label       = "BM blasts (%)",
#'   height_weight = 3
#' )
#'
#' # Any linear-scale parameter
#' lab_panel(
#'   line_params  = c("Hemoglobin (g/dL)", "Albumin (g/dL)"),
#'   y_scale      = "linear",
#'   y_label      = "Concentration (g/dL)"
#' )
#'
#' @export
lab_panel <- function(
    line_params        = character(0),
    point_params       = character(0),
    y_scale            = c("log10", "linear"),
    y_label            = NULL,
    y_limits           = NULL,
    y_breaks           = NULL,
    y_labels           = NULL,
    bdl_floor          = NULL,
    show_labels        = FALSE,
    label_suffix       = "",
    label_size         = 2.8,
    label_nudge_y      = 0.18,
    label_nudge_y_bdl  = -0.25,
    label_segment_size = 0.3,
    label_min_segment  = 0.2,
    highlight_days     = NULL,
    vline_labels       = TRUE,
    line_linewidth     = 0.65,
    point_size         = 2.5,
    bdl_shape          = 6L,
    bdl_size           = 2.5,
    logticks           = TRUE,
    height_weight      = 3
) {
  y_scale <- match.arg(y_scale)

  if (length(line_params) == 0L && length(point_params) == 0L) {
    stop("At least one of `line_params` or `point_params` must be non-empty.")
  }

  overlap <- intersect(line_params, point_params)
  if (length(overlap) > 0L) {
    warning(
      "The following parameters appear in both `line_params` and `point_params`: ",
      paste(overlap, collapse = ", "),
      ". They will be drawn as both a line and points."
    )
  }

  spec <- list(
    line_params        = line_params,
    point_params       = point_params,
    y_scale            = y_scale,
    y_label            = y_label,
    y_limits           = y_limits,
    y_breaks           = y_breaks,
    y_labels           = y_labels,
    bdl_floor          = bdl_floor,
    show_labels        = show_labels,
    label_suffix       = label_suffix,
    label_size         = label_size,
    label_nudge_y      = label_nudge_y,
    label_nudge_y_bdl  = label_nudge_y_bdl,
    label_segment_size = label_segment_size,
    label_min_segment  = label_min_segment,
    highlight_days     = highlight_days,
    vline_labels       = vline_labels,
    line_linewidth     = line_linewidth,
    point_size         = point_size,
    bdl_shape          = bdl_shape,
    bdl_size           = bdl_size,
    logticks           = logticks,
    height_weight      = height_weight
  )
  class(spec) <- c("lab_panel_spec", "list")
  spec
}

#' Print a lab panel specification
#'
#' Compact summary of a `lab_panel_spec` object showing the most important
#' fields: y-axis scale, label, the parameters assigned to lines and points,
#' whether value labels are enabled, and the relative panel height.
#'
#' @param x A `lab_panel_spec` object created by [lab_panel()].
#' @param ... Ignored; present for S3 method compatibility.
#'
#' @return `x`, invisibly.
#'
#' @examples
#' spec <- lab_panel(line_params = "WBC (/µL)", y_label = "Count (/µL)")
#' print(spec)
#'
#' @export
print.lab_panel_spec <- function(x, ...) {
  cat("<lab_panel_spec>\n")
  cat("  y_scale      :", x$y_scale, "\n")
  cat("  y_label      :", x$y_label %||% "(auto)", "\n")
  cat("  line_params  :", if (length(x$line_params))  paste(x$line_params,  collapse = ", ") else "(none)", "\n")
  cat("  point_params :", if (length(x$point_params)) paste(x$point_params, collapse = ", ") else "(none)", "\n")
  cat("  show_labels  :", x$show_labels, "\n")
  cat("  height_weight:", x$height_weight, "\n")
  invisible(x)
}
