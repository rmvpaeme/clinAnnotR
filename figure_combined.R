library(tidyverse)
library(readxl)
library(patchwork)
library(scales)
library(ggrepel)

# =============================================================================
# CONFIGURATION
# =============================================================================
TREATMENT_PATH <- "~/2026_TP53/treatment_TP53.xlsx"
LAB_PATH       <- "~/2026_TP53/labvals.xlsx"

cases <- list(
  list(id = "Case 1", ref_date = "2025-06-03", highlight_days = c(0, 29)),
  list(id = "Case 2", ref_date = "2025-08-12", highlight_days = c(0, 29))
)

# X-axis: common range for the two shared panels (days from diagnosis).
# Both cases run ~0–185 days; a shared axis lets readers compare directly.
X_RANGE <- c(-5, 190)

# Per-case visual identity — colour + point shape so both dimensions encode case
CASE_PALETTE <- c(
  "Case 1" = "#5E81AC",   # steel blue
  "Case 2" = "#BF616A"    # muted red
)
CASE_SHAPE <- c(
  "Case 1" = 16,   # filled circle
  "Case 2" = 17    # filled triangle
)

# =============================================================================
# PALETTE
# =============================================================================
NORD <- list(
  dark  = "#2E3440",
  muted = "#4C566A",
  grid  = "#D8DEE9"
)

# Reference thresholds for BM blasts
BLAST_REF <- c(`CR (<5%)` = 5, `Active (>=20%)` = 20)

# =============================================================================
# DATA HELPERS
# =============================================================================
combine_date_time <- function(date_col, time_col) {
  as.POSIXct(as.Date(date_col, tz = "UTC"), tz = "UTC") +
    as.integer(format(time_col, "%H", tz = "UTC")) * 3600L +
    as.integer(format(time_col, "%M", tz = "UTC")) *   60L +
    as.integer(format(time_col, "%S", tz = "UTC"))
}

load_treatment_data <- function(path, case_id, ref_date) {
  .origin <- as.POSIXct(ref_date, format = "%Y-%m-%d", tz = "UTC")
  read_excel(path) %>%
    filter(PATIENTID == case_id) %>%
    mutate(
      START_rel = as.numeric(difftime(combine_date_time(START, START), .origin, units = "days")),
      END_rel   = as.numeric(difftime(combine_date_time(END,   START), .origin, units = "days")),
      case_id   = case_id
    )
}

load_lab_data <- function(path, case_id, ref_date) {
  .origin <- as.POSIXct(ref_date, format = "%Y-%m-%d", tz = "UTC")
  read_excel(path) %>%
    filter(patientID == case_id) %>%
    mutate(
      value_raw   = as.character(value),
      value_coerce = as.numeric(sub("<", "", value_raw)),
      bdl         = startsWith(value_raw, "<") | value_coerce == 0,
      # BDL and zeros get a floor so they sit on the log axis with a marker
      value_num   = pmax(value_coerce, 0.1),
      value_label = ifelse(startsWith(value_raw, "<"),
                           value_raw,
                           paste0(round(value_coerce, 1))),
      reldate     = as.numeric(difftime(combine_date_time(date, time), .origin, units = "days")),
      case_id     = case_id
    )
}

# Load and combine both cases
all_labs <- map_dfr(cases, ~ load_lab_data(LAB_PATH, .x$id, .x$ref_date))
all_tx   <- map_dfr(cases, ~ load_treatment_data(TREATMENT_PATH, .x$id, .x$ref_date))

# =============================================================================
# SHARED THEME
# =============================================================================
theme_clinical <- function(show_x = FALSE, legend_pos = "right") {
  theme_bw(base_size = 9) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(color = NORD$grid, linewidth = 0.3),
      axis.title.y      = element_text(size = 8, angle = 90, margin = margin(r = 4)),
      axis.title.x      = if (show_x) element_text(size = 8) else element_blank(),
      axis.text.x       = if (show_x) element_text() else element_blank(),
      axis.ticks.x      = if (show_x) element_line() else element_blank(),
      legend.position   = legend_pos,
      legend.key.size   = unit(0.65, "lines"),
      legend.text       = element_text(size = 8),
      legend.title      = element_text(size = 8, face = "bold"),
      plot.margin       = margin(3, 5, 3, 5)
    )
}

# Draw named vertical reference lines with rotated labels at the top.
# `timepoints` is a named numeric vector, e.g. c("D1" = 1, "D22" = 22).
# `y_label` is the y coordinate (in data units) where labels are placed;
# pass Inf to sit just inside the top of the panel via vjust.
named_vlines <- function(timepoints, y_label = Inf) {
  if (is.null(timepoints) || length(timepoints) == 0) return(list())
  label_df <- tibble(
    x     = unname(timepoints),
    label = names(timepoints)
  )
  list(
    geom_vline(
      xintercept = unname(timepoints),
      linetype   = "dashed", color = NORD$muted,
      linewidth  = 0.4, alpha = 0.5
    ),
    geom_text(
      data        = label_df,
      aes(x = x, y = y_label, label = label),
      angle       = 90,
      hjust       = 1.1,
      vjust       = 0.4,
      size        = 2.5,
      color       = NORD$muted,
      inherit.aes = FALSE
    )
  )
}

# =============================================================================
# PANEL 1 — Counts: WBC (line) + Peripheral blasts (points)
#
# Both parameters share /µL units and overlapping ranges → one log10 axis.
# Case is encoded by colour; parameter by geometry (line vs point) AND
# linetype/shape so the figure is still readable in greyscale.
# =============================================================================
make_counts_panel <- function(lab_data, highlight_days) {

  wbc  <- lab_data %>% filter(parameter == "WBC (/µL)")
  peri <- lab_data %>% filter(parameter == "peripheral blasts (/µL)")
  peri_det <- peri %>% filter(!bdl)
  peri_bdl <- peri %>% filter(bdl)

  all_vals <- c(wbc$value_num, peri$value_num)
  y_lo <- 10^floor(log10(max(5,   min(all_vals, na.rm = TRUE))))
  y_hi <- 10^ceiling(log10(       max(all_vals, na.rm = TRUE)))

  ggplot() +
    named_vlines(highlight_days) +
    # WBC: colored by case; linetype mapped to a constant string so the
    # legend can show a single "WBC (/µL)" line entry (order 1).
    geom_line(
      data = wbc,
      aes(x = reldate, y = value_num,
          color    = case_id,
          group    = case_id,
          linetype = "WBC (/\u00b5L)"),
      linewidth = 0.65
    ) +
    # Peripheral blasts: colour + shape encode case (order 2)
    geom_point(
      data  = peri_det,
      aes(x = reldate, y = value_num, color = case_id, shape = case_id),
      size  = 2
    ) +
    geom_point(
      data  = peri_bdl,
      aes(x = reldate, y = value_num, color = case_id),
      shape = 6, size = 2.5
    ) +
    scale_color_manual(name = "Case", values = CASE_PALETTE) +
    scale_linetype_manual(
      name   = NULL,
      values = c("WBC (/\u00b5L)" = "solid"),
      guide  = guide_legend(
        order        = 1,
        override.aes = list(color = NORD$dark, linewidth = 0.65, shape = NA)
      )
    ) +
    scale_shape_manual(name = "Case", values = CASE_SHAPE) +
    scale_y_log10(
      limits = c(y_lo, y_hi),
      breaks = 10^seq(0, 6),
      labels = label_number(scale_cut = cut_short_scale())
    ) +
    annotation_logticks(sides = "l", linewidth = 0.2, color = NORD$muted) +
    coord_cartesian(xlim = X_RANGE, clip = "off") +
    labs(y = "Count (/\u00b5L)") +
    guides(
      color = guide_legend(
        order        = 2,
        override.aes = list(shape = unname(CASE_SHAPE), linetype = "blank")
      ),
      shape = "none"
    ) +
    theme_clinical(show_x = FALSE)
}

# =============================================================================
# PANEL 2 — BM blasts with IF (%)
#
# Percentages cannot share the /µL axis.  Log10 scale spans 0.1–100 % to
# accommodate BDL values alongside near-complete blast infiltration.
# Clinical reference lines at 5 % (CR) and 20 % (active disease) are drawn.
# Direct ggrepel labels avoid axis look-up.
# =============================================================================
make_blasts_panel <- function(lab_data, highlight_days) {

  bl     <- lab_data %>% filter(parameter == "BM blasts with IF (%)")
  bl_det <- bl %>% filter(!bdl)
  bl_bdl <- bl %>% filter(bdl)

  ggplot() +
    named_vlines(highlight_days) +
    # Detected blasts: colour = case, shape = "Detected"
    geom_point(
      data  = bl_det,
      aes(x = reldate, y = value_num,
          color = case_id,
          shape = "Detected"),
      size  = 3
    ) +
    # BDL blasts: colour = case, shape = "Below detection limit"
    geom_point(
      data  = bl_bdl,
      aes(x = reldate, y = value_num,
          color = case_id,
          shape = "Below detection limit"),
      size  = 3
    ) +
    # Value labels for detected points
    geom_text_repel(
      data         = bl_det,
      aes(x = reldate, y = value_num,
          label = paste0(value_label, "%"),
          color = case_id),
      size         = 2.8,
      nudge_y      = 0.18,
      segment.size = 0.3,
      min.segment.length = 0.2,
      show.legend  = FALSE
    ) +
    # Value labels for BDL points
    geom_text_repel(
      data         = bl_bdl,
      aes(x = reldate, y = value_num,
          label = paste0(value_label, "%"),
          color = case_id),
      size         = 2.8,
      nudge_y      = -0.25,
      segment.size = 0.3,
      min.segment.length = 0.2,
      show.legend  = FALSE
    ) +
    scale_color_manual(
      name   = "Case",
      values = CASE_PALETTE
    ) +
    scale_shape_manual(
      name   = NULL,
      values = c("Detected" = 16, "Below detection limit" = 6),
      guide  = guide_legend(
        order        = 2,
        override.aes = list(color = NORD$dark)
      )
    ) +
    scale_y_log10(
      limits = c(0.05, 200),
      breaks = c(0.1, 1, 5, 20, 100),
      labels = paste0(c(0.1, 1, 5, 20, 100), "%")
    ) +
    annotation_logticks(sides = "l", linewidth = 0.2, color = NORD$muted) +
    coord_cartesian(xlim = X_RANGE, clip = "off") +
    labs(y = "BM blasts (%)") +
    guides(
      color = guide_legend(
        order        = 1,
        override.aes = list(shape = 16)
      )
    ) +
    theme_clinical(show_x = FALSE)
}

# =============================================================================
# PANEL 3 / 4 — Treatment Gantt per case
#
# Rows ordered chronologically (earliest treatment at top).
# CLASS encoded by fill colour from the source data + a right-side bracket.
# Alternating row shading aids row-tracking when many treatments overlap.
# =============================================================================
make_gantt_panel <- function(treatment_data, case_label, highlight_days, show_x = FALSE) {

  # Row order: earliest first_start → top (i = n_rows), latest → bottom (i = 1)
  tr_order <- treatment_data %>%
    group_by(TREATMENT) %>%
    summarise(
      first_start = min(START_rel, na.rm = TRUE),
      COLOR       = first(COLOR),
      CLASS       = first(CLASS),
      .groups     = "drop"
    ) %>%
    arrange(desc(first_start)) %>%
    mutate(y = row_number())        # y=1 bottom, y=n top

  plot_data <- treatment_data %>%
    left_join(tr_order %>% select(TREATMENT, y), by = "TREATMENT")

  n_rows  <- max(tr_order$y)

  ggplot() +
    geom_vline(
      xintercept = highlight_days,
      linetype = "dashed", color = NORD$muted, linewidth = 0.4, alpha = 0.5
    ) +
    # Treatment bars coloured by CLASS via the COLOR column in the source data
    geom_rect(
      data = plot_data,
      aes(xmin = START_rel, xmax = END_rel,
          ymin = y - 0.35,  ymax = y + 0.35,
          fill = COLOR),
      alpha = 0.85
    ) +
    scale_fill_identity() +
    # Labels placed just left of each treatment's own first bar (nudge = -1 day)
    geom_text(
      data  = tr_order,
      aes(x = first_start - 1, y = y, label = TREATMENT),
      hjust = 1, size = 3, color = NORD$dark
    ) +
    scale_x_continuous(
      name   = "Days from diagnosis",
      breaks = sort(unique(c(seq(0, 200, by = 50), highlight_days))) %>%
                 setdiff(c(1, 49))
    ) +
    coord_cartesian(
      xlim = X_RANGE,
      ylim = c(0.5, n_rows + 0.5),
      clip = "off"
    ) +
    ggtitle(case_label) +
    theme_clinical(show_x = show_x, legend_pos = "none") +
    theme(
      axis.text.y        = element_blank(),
      axis.ticks.y       = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title         = element_text(size = 9, face = "bold", margin = margin(b = 3)),
      plot.margin        = margin(3, 5, 3, 5)
    )
}

# =============================================================================
# ASSEMBLE
# =============================================================================

# Protocol-fixed timepoints (same x for both cases — shown on all panels)
protocol_days <- c("D1" = 1, "D22" = 22, "D49" = 49)

# Per-case treatment-start timepoints derived from the data.
# Only treatments listed per case are shown; others are omitted.
TX_SHOW <- list(
  "Case 1" = c("ADE", "FLA"),   # MEC removed
  "Case 2" = character(0)        # MEC, ADE, HAM, FLA all removed
)

tx_starts <- function(case) {
  treatments <- TX_SHOW[[case]]
  if (length(treatments) == 0) return(setNames(numeric(0), character(0)))
  all_tx %>%
    filter(case_id == case, TREATMENT %in% treatments) %>%
    group_by(TREATMENT) %>%
    summarise(day = min(START_rel, na.rm = TRUE), .groups = "drop") %>%
    { setNames(.$day, .$TREATMENT) }
}

gantt_highlights <- function(case) {
  c(protocol_days, tx_starts(case))
}

p_counts <- make_counts_panel(all_labs, protocol_days)
p_blasts <- make_blasts_panel(all_labs, protocol_days)

p_gantt1 <- make_gantt_panel(
  all_tx %>% filter(case_id == "Case 1"),
  "Case 1 - treatments",
  gantt_highlights("Case 1"),
  show_x = FALSE
)
p_gantt2 <- make_gantt_panel(
  all_tx %>% filter(case_id == "Case 2"),
  "Case 2 - treatments",
  gantt_highlights("Case 2"),
  show_x = TRUE
)

# Layout — all four panels stacked, full width, sharing the same x domain:
#   Row 1 (tall):   counts (WBC + peripheral blasts)
#   Row 2 (short):  BM blasts
#   Row 3 (medium): Case 1 Gantt  (protocol + treatment-start lines)
#   Row 4 (medium): Case 2 Gantt  (protocol + treatment-start lines)
final <- (p_counts / p_blasts / p_gantt1 / p_gantt2) +
  plot_layout(heights = c(4, 2, 2, 2)) +
  plot_annotation(
    caption = "Dashed lines: CHIP-AML protocol timepoints (D1/D22/D49) and treatment starts. BDL = below detection limit.",
    theme   = theme(plot.caption = element_text(size = 7, color = NORD$muted))
  )

print(final)

ggsave(
  "~/2026_TP53/figure_combined.svg",
  plot = final, width = 14, height = 16, units = "in", dpi = 300, device = "svg"
)
ggsave(
  "~/2026_TP53/figure_combined.png",
  plot = final, width = 7, height = 8, units = "in", dpi = 300, device = "png"
)
