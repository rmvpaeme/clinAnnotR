#' Assemble a full multi-panel clinical figure
#'
#' Builds a stacked figure by combining one or more lab time-series panels
#' (defined by [lab_panel()] specifications) with one Gantt chart panel per
#' case. All panels share a common x-axis range; the x-axis label and tick
#' marks are shown only on the bottom-most panel.
#'
#' @param lab_data A data frame produced by [load_lab_data()] or
#'   [prep_lab_data()].
#' @param treatment_data A data frame produced by [load_treatment_data()] or
#'   [prep_treatment_data()]. Pass `NULL` to omit all Gantt panels.
#' @param lab_panels A list of `lab_panel_spec` objects created with
#'   [lab_panel()]. At least one element is required.
#' @param cases Character vector of case IDs to include and the order in which
#'   their Gantt panels are stacked. `NULL` = all unique `case_id` values in
#'   `lab_data`, in the order they first appear. Default: `NULL`.
#' @param case_palette Named character vector mapping case IDs to hex colours.
#'   `NULL` = auto-assigned from the built-in Nord-inspired palette.
#'   Default: `NULL`.
#' @param case_shapes Named integer vector mapping case IDs to ggplot2 shape
#'   codes used in the **Case** legend key. `NULL` = auto-assigned.
#'   Default: `NULL`.
#' @param x_range Numeric length-2. Shared x-axis limits in days applied to
#'   every panel. `NULL` = auto-computed as `c(min(relday) - 5,
#'   max(relday) + 5)` across the entire `lab_data`. Default: `NULL`.
#' @param highlight_days Named numeric vector of reference line positions shown
#'   on **every** panel (e.g. `c("D1" = 1, "D22" = 22, "D49" = 49)`).
#'   Individual lab panels can override this via [lab_panel()]'s
#'   `highlight_days` argument. `NULL` = no reference lines. Default: `NULL`.
#' @param highlight_days_by_case Named list of reference line overrides for
#'   individual Gantt panels, keyed by case ID. Each element is a named numeric
#'   vector in the same format as `highlight_days`. When a case ID is present
#'   here its Gantt panel shows *only* those lines; cases not listed fall back to
#'   the global `highlight_days`. Use `NULL` (or omit the entry) to show the
#'   global lines for a case, or supply an empty vector `c()` to suppress all
#'   lines for that case. Default: `NULL` (global `highlight_days` used for all
#'   Gantt panels).
#'
#'   Example (Case 1 gets its own subset of lines; Case 2 gets the global set):
#'   ```r
#'   highlight_days_by_case = list(
#'     "Case 1" = c("D0" = 0, "D22" = 22, "ADE C1" = 41)
#'   )
#'   ```
#' @param gantt_height_weight Positive numeric or `NULL`. Relative height of
#'   each Gantt panel in the patchwork stack. When `NULL` (default), the height
#'   is computed automatically as `max(1.5, n_treatments * 0.4)` so panels with
#'   many treatment rows are taller. Supply a fixed number to override for all
#'   Gantt panels. Default: `NULL`.
#' @param caption Character. Caption shown below the figure. `NULL` = a
#'   built-in default mentioning protocol timepoints and BDL. Default: `NULL`.
#' @param caption_size Numeric. Caption font size. Default: `7`.
#' @param shade_regions Named list of background shading regions, one entry per
#'   case ID. Each entry is a list of region specs, where each spec is a list
#'   with `xmin`, `xmax`, `fill`, and optional `alpha` (default `0.15`).
#'   Regions are drawn behind all data layers on every lab panel **and** on the
#'   corresponding Gantt panel. `NULL` (default) draws no shading.
#'
#'   Example (one case, three treatment phases):
#'   ```r
#'   shade_regions = list(
#'     "Case 1" = list(
#'       list(xmin = 5,  xmax = 7,  fill = "#BF616A", alpha = 0.15),
#'       list(xmin = 19, xmax = 38, fill = "#D08770", alpha = 0.15),
#'       list(xmin = 43, xmax = 53, fill = "#B48EAD", alpha = 0.20)
#'     )
#'   )
#'   ```
#' @param nord Named list with `$dark`, `$muted`, `$grid` (hex colours).
#'   `NULL` = built-in Nord palette. Default: `NULL`.
#' @param base_size Numeric. Base font size applied to all panels. Default: `9`.
#'
#' @return A [patchwork][patchwork::patchwork-package] object. Print it to
#'   display, or pass it to [save_clinical_figure()].
#'
#' @examples
#' lab <- example_data("lab")
#' tx  <- example_data("treatment")
#'
#' panels <- list(
#'   lab_panel(
#'     line_params   = "WBC (/µL)",
#'     point_params  = "peripheral blasts (/µL)",
#'     y_label       = "Count (/µL)",
#'     height_weight = 4
#'   ),
#'   lab_panel(
#'     point_params  = "BM blasts with IF (%)",
#'     show_labels   = TRUE,
#'     label_suffix  = "%",
#'     y_limits      = c(0.05, 200),
#'     y_breaks      = c(0.1, 1, 5, 20, 100),
#'     y_labels      = paste0(c(0.1, 1, 5, 20, 100), "%"),
#'     y_label       = "BM blasts (%)",
#'     height_weight = 3
#'   )
#' )
#'
#' fig <- make_clinical_figure(
#'   lab_data       = lab,
#'   treatment_data = tx,
#'   lab_panels     = panels,
#'   highlight_days = c("D1" = 1, "D22" = 22, "D49" = 49)
#' )
#' \dontrun{
#' print(fig)
#' }
#'
#' @export
make_clinical_figure <- function(
    lab_data,
    treatment_data      = NULL,
    lab_panels,
    cases               = NULL,
    case_palette        = NULL,
    case_shapes         = NULL,
    x_range             = NULL,
    highlight_days      = NULL,
    highlight_days_by_case = NULL,
    shade_regions       = NULL,
    gantt_height_weight = NULL,
    point_shapes_det    = NULL,
    caption             = NULL,
    caption_size        = 7,
    nord                = NULL,
    base_size           = 9
) {
  # ---- Resolve globals -------------------------------------------------------
  nord     <- nord %||% .nord_defaults()
  case_ids <- cases %||% unique(lab_data$case_id)
  pal      <- .resolve_palette(case_palette, case_ids)
  c_shapes <- .resolve_shapes(case_shapes, case_ids)

  if (is.null(x_range)) {
    rd      <- lab_data$relday
    x_range <- c(floor(min(rd, na.rm = TRUE)) - 5,
                 ceiling(max(rd, na.rm = TRUE)) + 5)
  }

  # ---- Derive shared linetypes/shapes across all lab_panels -----------------
  all_line_params  <- unique(unlist(lapply(lab_panels, `[[`, "line_params")))
  all_point_params <- unique(unlist(lapply(lab_panels, `[[`, "point_params")))
  global_linetypes  <- .assign_linetypes(all_line_params)
  global_pt_shapes  <- .assign_param_shapes(all_point_params)
  if (!is.null(point_shapes_det)) {
    global_pt_shapes[names(point_shapes_det)] <- as.integer(point_shapes_det)
  }

  # ---- Union of all highlight positions for lab panels ----------------------
  # Lab panels show data for all cases, so any day listed in
  # highlight_days_by_case must also appear there (in addition to the global
  # highlight_days set).
  lab_highlight_days <- highlight_days
  if (!is.null(highlight_days_by_case)) {
    all_case_days <- do.call(c, highlight_days_by_case)
    new_days      <- all_case_days[!unname(all_case_days) %in%
                                     unname(lab_highlight_days)]
    new_days      <- new_days[!duplicated(unname(new_days))]
    lab_highlight_days <- c(lab_highlight_days, new_days)
  }

  # ---- Build lab panels -----------------------------------------------------
  n_lab  <- length(lab_panels)
  n_gantt <- if (!is.null(treatment_data)) length(case_ids) else 0L
  n_total <- n_lab + n_gantt

  lab_plots <- vector("list", n_lab)
  for (i in seq_len(n_lab)) {
    spec <- lab_panels[[i]]

    # Inject combined highlight_days when the spec doesn't override
    if (is.null(spec$highlight_days)) {
      spec$highlight_days <- lab_highlight_days
    }

    # Show day labels on reference lines only on the first (topmost) panel
    if (i > 1L) spec$vline_labels <- FALSE

    is_last <- (n_gantt == 0L) && (i == n_lab)

    # Flatten shade regions across all cases for shared timeseries panels
    panel_shade <- if (!is.null(shade_regions)) {
      unlist(shade_regions, recursive = FALSE)
    } else NULL

    lab_plots[[i]] <- make_timeseries_panel(
      lab_data         = lab_data,
      spec             = spec,
      case_palette     = pal,
      case_shapes      = c_shapes,
      line_linetypes   = global_linetypes,
      point_shapes_det = global_pt_shapes,
      x_range          = x_range,
      show_x           = is_last,
      shade_regions    = panel_shade,
      nord             = nord,
      base_size        = base_size
    )
  }

  # ---- Build Gantt panels ---------------------------------------------------
  gantt_plots <- vector("list", n_gantt)
  for (j in seq_len(n_gantt)) {
    case_j <- case_ids[[j]]
    tx_j   <- treatment_data[treatment_data$case_id == case_j, , drop = FALSE]
    tx_j$COLOR <- pal[[case_j]]   # colour bars by case, not by treatment class
    is_last <- (j == n_gantt)
    gantt_hl <- if (!is.null(highlight_days_by_case) &&
                    case_j %in% names(highlight_days_by_case)) {
      highlight_days_by_case[[case_j]]
    } else {
      highlight_days
    }
    gantt_plots[[j]] <- make_gantt_panel(
      treatment_data  = tx_j,
      case_label      = paste0(case_j, " \u2014 treatments"),
      highlight_days  = gantt_hl,
      x_range         = x_range,
      show_x          = is_last,
      nord            = nord,
      base_size       = base_size
    )
  }

  # ---- Suppress legends on all panels except the first lab panel ------------
  # The first lab panel carries the complete global legend (all params + BDL,
  # all cases) via explicit scale breaks.  All other panels hide their legends
  # so patchwork collects exactly one copy regardless of deduplication behaviour.
  if (n_lab > 1L) {
    for (i in seq(2L, n_lab)) {
      lab_plots[[i]] <- lab_plots[[i]] + theme(legend.position = "none")
    }
  }

  # ---- Stack with patchwork -------------------------------------------------
  all_plots   <- c(lab_plots, gantt_plots)
  lab_weights <- vapply(lab_panels, `[[`, numeric(1L), "height_weight")
  if (n_gantt > 0L) {
    gantt_weights <- if (!is.null(gantt_height_weight)) {
      rep(gantt_height_weight, n_gantt)
    } else {
      vapply(case_ids, function(cid) {
        n_tx <- length(unique(
          treatment_data$TREATMENT[treatment_data$case_id == cid]
        ))
        max(1.5, n_tx * 0.4)
      }, numeric(1L))
    }
  } else {
    gantt_weights <- numeric(0L)
  }
  all_weights <- c(lab_weights, gantt_weights)

  stacked <- Reduce(`/`, all_plots)

  cap <- caption %||% paste0(
    if (!is.null(highlight_days))
      paste0("Dashed lines: ", paste(names(highlight_days), collapse = "/"),
             ". "),
    "BDL = below detection limit."
  )

  stacked +
    patchwork::plot_layout(heights = all_weights, guides = "collect") +
    patchwork::plot_annotation(
      caption = cap,
      theme   = theme(
        plot.caption = element_text(size = caption_size, color = nord$muted)
      )
    )
}
