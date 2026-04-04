#' Clinical figure theme
#'
#' A [ggplot2::theme_bw()]-based theme tuned for stacked multi-panel clinical
#' figures. Minor grid lines and superfluous decorations are removed; major grid
#' lines are retained in a soft colour.
#'
#' @param show_x Logical. Show x-axis text, title, and ticks? Set `TRUE` on the
#'   bottom-most panel. Default: `FALSE`.
#' @param legend_pos Legend position passed to [ggplot2::theme()]. Default: `"right"`.
#' @param base_size Numeric. Base font size (pt). Default: `9`.
#' @param grid_color Hex colour for major grid lines. Default: `"#D8DEE9"`.
#' @param muted_color Hex colour for axis decorations (logticks, etc.). Default: `"#4C566A"`.
#' @param axis_title_size Numeric. y-axis title font size. Default: `base_size - 1`.
#' @param legend_text_size Numeric. Legend text font size. Default: `base_size - 1`.
#' @param legend_key_size Numeric. Legend key height in `"lines"` units. Default: `0.65`.
#' @param plot_margin Numeric vector `c(top, right, bottom, left)` in pt.
#'   Default: `c(3, 5, 3, 5)`.
#'
#' @return A [ggplot2::theme()] object (additive).
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_clinical(show_x = TRUE)
#'
#' @export
theme_clinical <- function(
    show_x           = FALSE,
    legend_pos       = "right",
    base_size        = 9,
    grid_color       = "#D8DEE9",
    muted_color      = "#4C566A",
    axis_title_size  = base_size - 1,
    legend_text_size = base_size - 1,
    legend_key_size  = 0.65,
    plot_margin      = c(3, 5, 3, 5)
) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = grid_color, linewidth = 0.3),
      axis.title.y     = element_text(
        size   = axis_title_size,
        angle  = 90,
        margin = margin(r = 4)
      ),
      axis.title.x  = if (show_x) element_text(size = axis_title_size) else element_blank(),
      axis.text.x   = if (show_x) element_text() else element_blank(),
      axis.ticks.x  = if (show_x) element_line() else element_blank(),
      legend.position  = legend_pos,
      legend.key.size  = unit(legend_key_size, "lines"),
      legend.text      = element_text(size = legend_text_size),
      legend.title     = element_text(size = legend_text_size, face = "bold"),
      plot.margin      = margin(
        plot_margin[[1]], plot_margin[[2]],
        plot_margin[[3]], plot_margin[[4]]
      )
    )
}

#' Add named vertical reference lines to a ggplot
#'
#' Returns a list of ggplot2 layer objects—a [ggplot2::geom_vline()] and
#' (optionally) a [ggplot2::geom_text()] with 90° rotated labels—that can be
#' added to any ggplot with `+`. Designed for marking protocol timepoints or
#' clinical events on a shared x-axis.
#'
#' @param timepoints Named numeric vector. x-axis positions, e.g.
#'   `c("D1" = 1, "D22" = 22, "D49" = 49)`. `NULL` or length-0 returns `list()`.
#' @param y_label Numeric or `Inf`. y coordinate for text labels. `Inf` places
#'   labels just inside the top of the panel (via `vjust`). Default: `Inf`.
#' @param labels Logical. Draw rotated text labels? Default: `TRUE`.
#' @param color Hex colour for lines and labels. Default: `"#4C566A"`.
#' @param linetype Character linetype. Default: `"dashed"`.
#' @param linewidth Numeric line width. Default: `0.4`.
#' @param alpha Numeric line alpha. Default: `0.5`.
#' @param label_size Numeric text size for labels. Default: `2.5`.
#' @param label_angle Numeric label rotation in degrees. Default: `90`.
#' @param label_hjust Numeric horizontal justification. Default: `1.1`.
#' @param label_vjust Numeric vertical justification. Default: `0.4`.
#'
#' @return A list of ggplot2 layer objects.
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   named_vlines(c("D1" = 1, "D22" = 22)) +
#'   coord_cartesian(xlim = c(-5, 30), ylim = c(0, 100))
#'
#' @export
named_vlines <- function(
    timepoints,
    y_label     = Inf,
    labels      = TRUE,
    color       = "#4C566A",
    linetype    = "dashed",
    linewidth   = 0.4,
    alpha       = 0.5,
    label_size  = 2.5,
    label_angle = 90,
    label_hjust = 1.1,
    label_vjust = 0.4
) {
  if (is.null(timepoints) || length(timepoints) == 0L) return(list())

  layers <- list(
    geom_vline(
      xintercept = unname(timepoints),
      linetype   = linetype,
      color      = color,
      linewidth  = linewidth,
      alpha      = alpha
    )
  )

  if (labels && !is.null(names(timepoints))) {
    label_df <- data.frame(
      x     = unname(timepoints),
      label = names(timepoints),
      stringsAsFactors = FALSE
    )
    layers <- c(layers, list(
      geom_text(
        data        = label_df,
        mapping     = aes(x = .data$x, y = y_label, label = .data$label),
        angle       = label_angle,
        hjust       = label_hjust,
        vjust       = label_vjust,
        size        = label_size,
        color       = color,
        inherit.aes = FALSE
      )
    ))
  }

  layers
}
