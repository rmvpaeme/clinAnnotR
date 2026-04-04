#' Build a time-series panel for any numerical lab parameters
#'
#' Creates a [ggplot2::ggplot()] panel showing one or more lab parameters over
#' time. Parameters can be rendered as connected lines, points, or both.
#' Below-detection-limit (BDL) values—those whose raw string starts with `"<"`
#' or whose numeric value is zero—are shown with a dedicated open-triangle
#' symbol regardless of the parameter type.
#'
#' When **only one case** is present in `lab_data`, colour is mapped to
#' **parameter** (each parameter gets a distinct colour; all lines are solid).
#' When **multiple cases** are present, colour is mapped to **case** and
#' linetype distinguishes parameters (existing multi-case behaviour).
#'
#' @param lab_data A data frame produced by [load_lab_data()] or
#'   [prep_lab_data()], containing columns `case_id`, `relday`, `parameter`,
#'   `value_num`, `value_label`, and `bdl`. Extra columns are ignored.
#' @param spec A `lab_panel_spec` object from [lab_panel()].
#' @param case_palette Named character vector mapping case IDs to hex colours.
#'   Unknown case IDs are assigned colours automatically.
#' @param case_shapes Named integer vector mapping case IDs to shapes used in
#'   the **Case** legend key. Default shapes are filled circles/triangles.
#' @param line_linetypes Named character vector mapping `line_param` values to
#'   linetype strings (e.g. `"solid"`, `"dashed"`). Used only in multi-case
#'   mode; ignored when there is a single case (all lines are solid).
#' @param point_shapes_det Named integer vector mapping `point_param` values to
#'   ggplot2 shape codes for detected (non-BDL) points. Auto-assigned if `NULL`.
#' @param x_range Numeric vector of length 2. x-axis limits in days.
#'   Default: `c(-5, 190)`.
#' @param show_x Logical. Show x-axis text, title, and ticks? Default: `FALSE`.
#' @param nord Named list with elements `$dark`, `$muted`, `$grid` (hex
#'   colours). Use [.nord_defaults()] or pass your own. Default: Nord palette.
#' @param base_size Numeric. Base font size. Default: `9`.
#'
#' @return A [ggplot2::ggplot()] object.
#'
#' @examples
#' lab <- example_data("lab")
#' spec <- lab_panel(
#'   line_params  = "WBC (/\u00b5L)",
#'   point_params = "peripheral blasts (/\u00b5L)",
#'   y_label      = "Count (/\u00b5L)"
#' )
#' make_timeseries_panel(lab, spec)
#'
#' @export
make_timeseries_panel <- function(
    lab_data,
    spec,
    case_palette     = NULL,
    case_shapes      = NULL,
    line_linetypes   = NULL,
    point_shapes_det = NULL,
    x_range          = c(-5, 190),
    show_x           = FALSE,
    nord             = NULL,
    base_size        = 9
) {
  if (!inherits(spec, "lab_panel_spec")) {
    stop("`spec` must be a `lab_panel_spec` created with `lab_panel()`.")
  }

  nord        <- nord %||% .nord_defaults()
  line_params  <- spec$line_params
  point_params <- spec$point_params
  bdl_floor    <- spec$bdl_floor %||% 0.1
  bdl_shape    <- as.integer(spec$bdl_shape)
  bdl_size     <- spec$bdl_size

  # ---- Resolve visual identity -----------------------------------------------
  case_ids    <- unique(lab_data$case_id)
  single_case <- length(case_ids) == 1L

  pal      <- .resolve_palette(case_palette, case_ids)
  c_shapes <- .resolve_shapes(case_shapes, case_ids)

  all_params <- c(line_params, point_params)

  # Single-case: colours map to parameters (all lines solid)
  # Multi-case:  colours map to case, linetypes map to parameters
  if (single_case) {
    param_pal <- .resolve_param_palette(all_params)
  } else {
    if (is.null(line_linetypes) && length(line_params) > 0L) {
      line_linetypes <- .assign_linetypes(line_params)
    }
    if (is.null(point_shapes_det) && length(point_params) > 0L) {
      point_shapes_det <- .assign_param_shapes(point_params)
    }
  }

  # ---- Subset data -----------------------------------------------------------
  data_used  <- lab_data[lab_data$parameter %in% all_params, , drop = FALSE]
  line_data  <- data_used[data_used$parameter %in% line_params,  , drop = FALSE]
  point_data <- data_used[data_used$parameter %in% point_params, , drop = FALSE]
  point_det  <- point_data[!point_data$bdl, , drop = FALSE]
  point_bdl  <- point_data[ point_data$bdl, , drop = FALSE]

  has_lines  <- nrow(line_data)  > 0L
  has_det    <- nrow(point_det)  > 0L
  has_bdl    <- nrow(point_bdl)  > 0L

  # ---- Y-axis ----------------------------------------------------------------
  all_vals <- data_used$value_num
  y_lims   <- spec$y_limits

  if (spec$y_scale == "log10") {
    if (is.null(y_lims)) {
      y_lo  <- 10^floor(log10(max(bdl_floor, min(all_vals, na.rm = TRUE))))
      y_hi  <- 10^ceiling(log10(max(all_vals, na.rm = TRUE)))
      y_lims <- c(y_lo, y_hi)
    }
    y_breaks_eff <- spec$y_breaks %||%
      10^seq(log10(y_lims[[1]]), log10(y_lims[[2]]))
    y_labels_eff <- spec$y_labels %||%
      scales::label_number(scale_cut = scales::cut_short_scale())
  } else {
    if (is.null(y_lims)) {
      y_lo  <- min(0, min(all_vals, na.rm = TRUE))
      y_hi  <- max(all_vals, na.rm = TRUE) * 1.05
      y_lims <- c(y_lo, y_hi)
    }
    y_breaks_eff <- spec$y_breaks %||% ggplot2::waiver()
    y_labels_eff <- spec$y_labels %||% ggplot2::waiver()
  }

  # ---- y-axis label ----------------------------------------------------------
  y_label <- spec$y_label %||% paste(all_params, collapse = " / ")

  # ---- Effective highlight days ----------------------------------------------
  hl_days <- spec$highlight_days  # NULL means caller supplies them

  # ---- Reference lines -------------------------------------------------------
  p <- ggplot() +
    named_vlines(
      hl_days,
      labels    = spec$vline_labels,
      color     = nord$muted,
      linewidth = 0.4,
      alpha     = 0.5
    )

  # ---- Line layers -----------------------------------------------------------
  if (has_lines) {
    if (single_case) {
      p <- p + geom_line(
        data      = line_data,
        mapping   = aes(
          x     = .data$relday,
          y     = .data$value_num,
          color = .data$parameter,
          group = .data$parameter
        ),
        linetype  = "solid",
        linewidth = spec$line_linewidth
      )
    } else {
      p <- p + geom_line(
        data    = line_data,
        mapping = aes(
          x        = .data$relday,
          y        = .data$value_num,
          color    = .data$case_id,
          group    = interaction(.data$case_id, .data$parameter),
          linetype = .data$parameter
        ),
        linewidth = spec$line_linewidth
      )
    }
  }

  # ---- Detected-point layer --------------------------------------------------
  if (has_det) {
    if (single_case) {
      # Fixed circle shape; colour already distinguishes parameters
      p <- p + geom_point(
        data    = point_det,
        mapping = aes(
          x     = .data$relday,
          y     = .data$value_num,
          color = .data$parameter
        ),
        shape = 16L,
        size  = spec$point_size
      )
    } else {
      p <- p + geom_point(
        data    = point_det,
        mapping = aes(
          x     = .data$relday,
          y     = .data$value_num,
          color = .data$case_id,
          shape = .data$parameter
        ),
        size = spec$point_size
      )
    }
  }

  # ---- Value-label layer (ggrepel) -------------------------------------------
  if (spec$show_labels && has_det) {
    if (single_case) {
      p <- p + ggrepel::geom_text_repel(
        data               = point_det,
        mapping            = aes(
          x     = .data$relday,
          y     = .data$value_num,
          label = paste0(.data$value_label, spec$label_suffix),
          color = .data$parameter
        ),
        size               = spec$label_size,
        nudge_y            = spec$label_nudge_y,
        segment.size       = spec$label_segment_size,
        min.segment.length = spec$label_min_segment,
        show.legend        = FALSE
      )
    } else {
      p <- p + ggrepel::geom_text_repel(
        data               = point_det,
        mapping            = aes(
          x     = .data$relday,
          y     = .data$value_num,
          label = paste0(.data$value_label, spec$label_suffix),
          color = .data$case_id
        ),
        size               = spec$label_size,
        nudge_y            = spec$label_nudge_y,
        segment.size       = spec$label_segment_size,
        min.segment.length = spec$label_min_segment,
        show.legend        = FALSE
      )
    }
  }

  # ---- BDL point layer -------------------------------------------------------
  if (has_bdl) {
    if (single_case) {
      p <- p + geom_point(
        data    = point_bdl,
        mapping = aes(
          x     = .data$relday,
          y     = .data$value_num,
          color = .data$parameter
        ),
        shape = bdl_shape,
        size  = bdl_size
      )
    } else {
      p <- p + geom_point(
        data    = point_bdl,
        mapping = aes(
          x     = .data$relday,
          y     = .data$value_num,
          color = .data$case_id
        ),
        shape = bdl_shape,
        size  = bdl_size
      )
    }

    if (spec$show_labels) {
      if (single_case) {
        p <- p + ggrepel::geom_text_repel(
          data               = point_bdl,
          mapping            = aes(
            x     = .data$relday,
            y     = .data$value_num,
            label = paste0(.data$value_label, spec$label_suffix),
            color = .data$parameter
          ),
          size               = spec$label_size,
          nudge_y            = spec$label_nudge_y_bdl,
          segment.size       = spec$label_segment_size,
          min.segment.length = spec$label_min_segment,
          show.legend        = FALSE
        )
      } else {
        p <- p + ggrepel::geom_text_repel(
          data               = point_bdl,
          mapping            = aes(
            x     = .data$relday,
            y     = .data$value_num,
            label = paste0(.data$value_label, spec$label_suffix),
            color = .data$case_id
          ),
          size               = spec$label_size,
          nudge_y            = spec$label_nudge_y_bdl,
          segment.size       = spec$label_segment_size,
          min.segment.length = spec$label_min_segment,
          show.legend        = FALSE
        )
      }
    }

    # Ghost point — placed far off-screen, registers a legend entry only
    ghost_df <- data.frame(
      relday    = -99999,
      value_num = y_lims[[1]],
      bdl_label = "BDL (\u2207)",
      stringsAsFactors = FALSE
    )
    p <- p + geom_point(
      data        = ghost_df,
      mapping     = aes(
        x     = .data$relday,
        y     = .data$value_num,
        shape = .data$bdl_label
      ),
      colour      = nord$dark,
      size        = bdl_size,
      inherit.aes = FALSE
    )
  }

  # ---- Scales ----------------------------------------------------------------

  # Linetype scale — multi-case only (single-case uses solid everywhere)
  if (!single_case && has_lines && length(line_params) > 0L) {
    p <- p + scale_linetype_manual(
      name   = NULL,
      values = line_linetypes[line_params],
      guide  = guide_legend(order = 1)
    )
  }

  # Shape scale — multi-case point params + BDL ghost
  if (!single_case) {
    all_shapes <- c(
      if (has_det)  point_shapes_det[point_params],
      if (has_bdl)  c("BDL (\u2207)" = bdl_shape)
    )
    if (length(all_shapes) > 0L) {
      p <- p + scale_shape_manual(
        name   = NULL,
        values = all_shapes,
        guide  = guide_legend(
          order        = 2,
          override.aes = list(colour = nord$dark)
        )
      )
    }
  } else if (has_bdl) {
    # Single-case: only the BDL ghost needs a shape entry
    p <- p + scale_shape_manual(
      name   = NULL,
      values = c("BDL (\u2207)" = bdl_shape),
      guide  = guide_legend(
        order        = 2,
        override.aes = list(colour = nord$dark)
      )
    )
  }

  # Colour scale
  if (single_case) {
    p <- p + scale_color_manual(
      name   = NULL,
      values = param_pal[all_params],
      guide  = guide_legend(
        order        = 1,
        override.aes = list(linetype = "solid", shape = 16L)
      )
    )
  } else {
    p <- p + scale_color_manual(
      name   = "Case",
      values = pal,
      guide  = guide_legend(
        order        = 3,
        override.aes = list(linetype = "blank", shape = 16L)
      )
    )
  }

  # Y-axis
  if (spec$y_scale == "log10") {
    p <- p +
      scale_y_log10(limits = y_lims, breaks = y_breaks_eff,
                    labels = y_labels_eff) +
      annotation_logticks(sides = "l", linewidth = 0.2, color = nord$muted)
  } else {
    p <- p +
      scale_y_continuous(limits = y_lims, breaks = y_breaks_eff,
                         labels = y_labels_eff)
  }

  p +
    coord_cartesian(xlim = x_range, clip = "off") +
    labs(y = y_label) +
    theme_clinical(
      show_x      = show_x,
      base_size   = base_size,
      grid_color  = nord$grid,
      muted_color = nord$muted
    )
}
