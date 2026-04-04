#' Build a treatment Gantt panel for one case
#'
#' Creates a horizontal Gantt chart showing drug treatment bars for a single
#' case. Bars are coloured by the `COLOR` column in the data (user-controlled
#' per row), allowing full control over class-level colour coding without
#' requiring a legend. Drug name labels are placed to the left of each bar's
#' first segment.
#'
#' When `group_by_class = TRUE` (default), treatments are sorted by drug class
#' (ordered by the earliest treatment start within each class) and a thin
#' separator line is drawn between classes. A class label is placed to the
#' right of each group.
#'
#' Duplicate segments (identical `TREATMENT`, `START_rel`, and `END_rel`) are
#' silently removed before plotting.
#'
#' @param treatment_data A data frame produced by [load_treatment_data()] or
#'   [prep_treatment_data()], filtered to **one case** (i.e. a single unique
#'   `case_id`). Columns required: `TREATMENT`, `START_rel`, `END_rel`,
#'   `COLOR`. Column `CLASS` is required when `group_by_class = TRUE`.
#' @param case_label Character. Panel title shown above the chart (e.g.
#'   `"Case 1 \u2014 treatments"`). Default: `NULL` (no title).
#' @param highlight_days Named numeric vector of reference line positions
#'   (dashed vertical lines). `NULL` suppresses lines. Default: `NULL`.
#' @param x_range Numeric length-2. x-axis limits in days. `NULL` = auto
#'   (data range with 5-day padding). Default: `NULL`.
#' @param show_x Logical. Show x-axis text, title, and ticks? Default: `FALSE`.
#' @param bar_height Numeric. Half-height of each bar on the y-axis (bars span
#'   `y - bar_height` to `y + bar_height`). Default: `0.35`.
#' @param bar_alpha Numeric. Bar fill alpha. Default: `0.85`.
#' @param label_size Numeric. Text size for drug name labels. Default: `3`.
#' @param label_nudge_x Numeric. Horizontal offset (in days) applied to drug
#'   name labels relative to `first_start`. Negative values push labels to the
#'   left. Default: `-1`.
#' @param x_label Character. x-axis title (shown only when `show_x = TRUE`).
#'   Default: `"Days from diagnosis"`.
#' @param x_breaks_step Numeric. Step size for automatic x-axis break positions.
#'   Default: `50`.
#' @param group_by_class Logical. Sort treatments by drug class (ordered by the
#'   earliest start in that class), draw separator lines between classes, and
#'   add class labels to the right? Default: `TRUE`.
#' @param show_class_labels Logical. Draw class name labels to the right of
#'   each class group when `group_by_class = TRUE`. Default: `TRUE`.
#' @param class_label_size Numeric. Text size for class labels. Default: `2.5`.
#' @param nord Named list with `$dark`, `$muted`, `$grid` (hex colours).
#'   Default: Nord palette.
#' @param base_size Numeric. Base font size. Default: `9`.
#' @param title_size Numeric. Font size for the panel title. Default: `9`.
#' @param vline_linetype Character. Linetype for reference lines. Default:
#'   `"dashed"`.
#' @param vline_linewidth Numeric. Width of reference lines. Default: `0.4`.
#' @param vline_alpha Numeric. Alpha of reference lines. Default: `0.5`.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @examples
#' tx <- example_data("treatment")
#' make_gantt_panel(
#'   tx[tx$case_id == "Case 1", ],
#'   case_label     = "Case 1 \u2014 treatments",
#'   highlight_days = c("D1" = 1, "D22" = 22, "D49" = 49),
#'   x_range        = c(-5, 190),
#'   show_x         = TRUE
#' )
#'
#' @export
make_gantt_panel <- function(
    treatment_data,
    case_label        = NULL,
    highlight_days    = NULL,
    x_range           = NULL,
    show_x            = FALSE,
    bar_height        = 0.35,
    bar_alpha         = 0.85,
    label_size        = 3,
    label_nudge_x     = -1,
    x_label           = "Days from diagnosis",
    x_breaks_step     = 50,
    group_by_class    = TRUE,
    show_class_labels = TRUE,
    class_label_size  = 2.5,
    nord              = NULL,
    base_size         = 9,
    title_size        = 9,
    vline_linetype    = "dashed",
    vline_linewidth   = 0.4,
    vline_alpha       = 0.5
) {
  nord <- nord %||% .nord_defaults()

  if (is.null(x_range)) {
    pad     <- 5
    x_range <- c(
      floor(min(treatment_data$START_rel, na.rm = TRUE)) - pad,
      ceiling(max(treatment_data$END_rel, na.rm = TRUE)) + pad
    )
  }

  # Remove duplicate segments (same TREATMENT / START / END)
  treatment_data <- dplyr::distinct(
    treatment_data,
    .data$TREATMENT, .data$START_rel, .data$END_rel,
    .keep_all = TRUE
  )

  has_class <- group_by_class && "CLASS" %in% names(treatment_data)

  # ---- Per-treatment summary -------------------------------------------------
  tr_summary <- treatment_data |>
    group_by(.data$TREATMENT) |>
    summarise(
      first_start = min(.data$START_rel, na.rm = TRUE),
      COLOR       = first(.data$COLOR),
      CLASS       = if (has_class) first(.data$CLASS) else NA_character_,
      .groups     = "drop"
    )

  # ---- Row ordering ----------------------------------------------------------
  if (has_class) {
    # Order classes by their earliest treatment start, then treatments within
    # each class by their own first_start (ascending = top of the group first)
    class_order <- tr_summary |>
      group_by(.data$CLASS) |>
      summarise(class_start = min(.data$first_start), .groups = "drop") |>
      arrange(.data$class_start) |>
      dplyr::pull(.data$CLASS)

    tr_order <- tr_summary |>
      mutate(CLASS = factor(.data$CLASS, levels = class_order)) |>
      arrange(.data$CLASS, .data$first_start) |>
      mutate(y = rev(seq_len(dplyr::n())))
  } else {
    tr_order <- tr_summary |>
      arrange(desc(.data$first_start)) |>
      mutate(y = seq_len(dplyr::n()))
  }

  plot_data <- treatment_data |>
    left_join(tr_order |> select("TREATMENT", "y"), by = "TREATMENT")

  n_rows <- max(tr_order$y)

  # ---- Class group geometry (separators + labels) ----------------------------
  if (has_class) {
    class_groups <- tr_order |>
      group_by(.data$CLASS) |>
      summarise(
        y_min = min(.data$y),
        y_max = max(.data$y),
        y_mid = mean(.data$y),
        .groups = "drop"
      ) |>
      arrange(desc(.data$y_mid))

    # Separator lines sit halfway between adjacent class groups
    sep_y <- class_groups$y_min[-nrow(class_groups)] - 0.5
  }

  # ---- Auto x-axis breaks ----------------------------------------------------
  x_break_lo <- floor(x_range[[1]]   / x_breaks_step) * x_breaks_step
  x_break_hi <- ceiling(x_range[[2]] / x_breaks_step) * x_breaks_step
  all_manual_breaks <- sort(unique(c(
    seq(x_break_lo, x_break_hi, by = x_breaks_step),
    unname(highlight_days)
  )))

  # ---- Build plot ------------------------------------------------------------
  p <- ggplot() +
    geom_vline(
      xintercept = unname(highlight_days),
      linetype   = vline_linetype,
      color      = nord$muted,
      linewidth  = vline_linewidth,
      alpha      = vline_alpha
    ) +
    geom_rect(
      data    = plot_data,
      mapping = aes(
        xmin = .data$START_rel,
        xmax = .data$END_rel,
        ymin = .data$y - bar_height,
        ymax = .data$y + bar_height,
        fill = .data$COLOR
      ),
      alpha = bar_alpha
    ) +
    scale_fill_identity() +
    geom_text(
      data    = tr_order,
      mapping = aes(
        x     = .data$first_start + label_nudge_x,
        y     = .data$y,
        label = .data$TREATMENT
      ),
      hjust = 1,
      size  = label_size,
      color = nord$dark
    )

  # ---- Class separators and labels -------------------------------------------
  if (has_class && length(sep_y) > 0L) {
    p <- p + geom_hline(
      yintercept = sep_y,
      color      = nord$muted,
      linewidth  = 0.3,
      linetype   = "solid",
      alpha      = 0.4
    )
  }

  if (has_class && show_class_labels) {
    x_label_pos <- x_range[[2]]
    p <- p + geom_text(
      data    = class_groups,
      mapping = aes(
        x     = x_label_pos,
        y     = .data$y_mid,
        label = .data$CLASS
      ),
      hjust       = -0.1,
      size        = class_label_size,
      color       = nord$muted,
      fontface    = "italic",
      inherit.aes = FALSE
    )
  }

  p <- p +
    scale_x_continuous(
      name   = x_label,
      breaks = all_manual_breaks
    ) +
    coord_cartesian(
      xlim = x_range,
      ylim = c(0.5, n_rows + 0.5),
      clip = "off"
    ) +
    theme_clinical(
      show_x      = show_x,
      legend_pos  = "none",
      base_size   = base_size,
      grid_color  = nord$grid,
      muted_color = nord$muted
    ) +
    theme(
      axis.text.y        = element_blank(),
      axis.ticks.y       = element_blank(),
      axis.title.y       = element_blank(),
      panel.grid.major.y = element_blank()
    )

  if (!is.null(case_label)) {
    p <- p + ggtitle(case_label) +
      theme(
        plot.title = element_text(
          size = title_size, face = "bold", margin = margin(b = 3)
        )
      )
  }

  p
}
