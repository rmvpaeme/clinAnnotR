library(tidyverse)
library(readxl)
library(ggpubr)

# =============================================================================
# CASE CONFIGURATION
# Add a new entry here to add a new case to the figure.
# =============================================================================
cases <- list(
  list(id = "Case 1", ref_date = "2025-06-03", important_days = c(0, 29)),
  list(id = "Case 2", ref_date = "2025-08-12", important_days = c(0, 29))
)

# =============================================================================
# PALETTE (Nord-inspired)
# =============================================================================
NORD <- list(
  red    = "#BF616A",
  blue   = "#5E81AC",
  yellow = "#EBCB8B",
  purple = "#B48EAD",
  green  = "#A3BE8C",
  dark   = "#3B4252",
  muted  = "#4C566A"
)

# Named per-parameter colour map.  scale_color_manual() requires names that
# match the 'parameter' column values; unnamed or category-keyed vectors cause
# "No shared levels found" warnings and silently break colour assignment.
PARAM_COLORS <- c(
  "WBC (/µL)"               = NORD$blue,
  "peripheral blasts (/µL)" = NORD$yellow,
  "BM blasts with IF (%)"   = NORD$red
)

# =============================================================================
# DATA LOADING
# =============================================================================
# Combine a date column and a time column (both POSIXct) into a single POSIXct.
#
# readxl reads date-only cells as POSIXct at midnight UTC and time-only cells
# as POSIXct with a dummy date of 1899-12-31.  We cannot reliably round-trip
# through character strings (timezone suffixes, locale formats, pre-epoch
# arithmetic on the 1899 dummy date all cause as_datetime() to fail).
# Instead we extract the date as midnight-UTC and the time as elapsed seconds,
# then add them as numbers.
combine_date_time <- function(date_col, time_col) {
  date_midnight <- as.POSIXct(as.Date(date_col, tz = "UTC"), tz = "UTC")
  time_secs <- as.integer(format(time_col, "%H", tz = "UTC")) * 3600L +
               as.integer(format(time_col, "%M", tz = "UTC")) * 60L +
               as.integer(format(time_col, "%S", tz = "UTC"))
  date_midnight + time_secs
}

load_treatment_data <- function(path, case_id, ref_date) {
  .origin <- as.POSIXct(ref_date, format = "%Y-%m-%d", tz = "UTC")
  read_excel(path) %>%
    filter(PATIENTID == case_id) %>%
    mutate(
      START_rel = as.numeric(difftime(combine_date_time(START, START), .origin, units = "days")),
      END_rel   = as.numeric(difftime(combine_date_time(END,   START), .origin, units = "days"))
    )
}

load_lab_data <- function(path, case_id, ref_date) {
  .origin <- as.POSIXct(ref_date, format = "%Y-%m-%d", tz = "UTC")
  read_excel(path) %>%
    filter(patientID == case_id) %>%
    mutate(
      # Flag below-detection-limit entries ("<X") and zero values: both are
      # "undetectable" and cannot be shown on a log10 axis without a floor.
      # Zero values are plotted at 0.5 (one step below the "<1" limit of 1)
      # so the downward-triangle marker is still visible on the log scale.
      bdl       = startsWith(as.character(value), "<") |
                    as.numeric(sub("<", "", as.character(value))) == 0,
      value_num = pmax(as.numeric(sub("<", "", as.character(value))), 0.5),
      reldate   = as.numeric(difftime(combine_date_time(date, time), .origin, units = "days"))
    )
}

# =============================================================================
# GANTT ANNOTATION LAYER
# Draws one horizontal bar per treatment, labelled on the left.
# 'hospitalisation' is always placed at the bottom.
# =============================================================================
make_gantt_layers <- function(data, highlight_days = NULL) {
  other_classes  <- sort(setdiff(unique(data$CLASS), "hospitalisation"))
  class_levels   <- c("hospitalisation", other_classes)

  sorted <- data %>%
    mutate(CLASS = factor(CLASS, levels = class_levels)) %>%
    arrange(CLASS, TREATMENT)

  unique_treatments <- unique(sorted$TREATMENT)
  layers <- list()

  for (i in seq_along(unique_treatments)) {
    tr_name  <- unique_treatments[i]
    tr_rows  <- sorted %>% filter(TREATMENT == tr_name)
    tr_color <- tr_rows$COLOR[1]

    y_min <- i - 0.9
    y_max <- i - 0.1
    y_mid <- (y_min + y_max) / 2

    layers <- c(layers, list(
      annotate("rect",
        xmin = tr_rows$START_rel, xmax = tr_rows$END_rel,
        ymin = y_min, ymax = y_max,
        fill = tr_color, alpha = 0.8
      ),
      annotate("text",
        label = tr_name,
        x     = min(tr_rows$START_rel, na.rm = TRUE) - 2,
        y     = y_mid,
        hjust = 1, size = 3.5, color = NORD$dark
      )
    ))
  }

  if (!is.null(highlight_days)) {
    layers <- c(layers, list(
      annotate("segment",
        x = highlight_days, xend = highlight_days,
        y = -Inf, yend = Inf,
        linetype = "dashed", color = NORD$muted,
        linewidth = 0.6, alpha = 0.3
      )
    ))
  }

  layers
}

# =============================================================================
# LAB-VALUE PANEL (log10 y-axis, categories A / B / C mixed)
# =============================================================================
make_lab_panel <- function(lab_data, x_max) {
  # Y-axis limits must span ALL parameters, not just WBC.
  # Restricting to WBC clips BM blast values (different unit/scale) entirely.
  pos_vals <- lab_data$value_num[lab_data$value_num > 0]
  ymin_val <- max(0.5, min(pos_vals, na.rm = TRUE))
  ymax_val <- max(pos_vals, na.rm = TRUE)

  bdl_data  <- lab_data %>% filter(bdl)
  blast_data <- lab_data %>% filter(parameter == "BM blasts with IF (%)")

  ggplot() +
    # Category A: WBC — line
    geom_line(
      data      = lab_data %>% filter(category == "A"),
      aes(x = reldate, y = value_num, col = parameter)
    ) +
    # Category B: peripheral blasts — points only
    geom_point(
      data = lab_data %>% filter(category == "B"),
      aes(x = reldate, y = value_num, col = parameter)
    ) +
    # Category C: BM blasts — points only (no connecting line; measurements
    # are sparse bone marrow samples, not a continuous time series)
    geom_point(
      data  = blast_data %>% filter(!bdl),
      aes(x = reldate, y = value_num, col = parameter),
      size  = 2.5
    ) +
    # Direct value labels for BM blasts (avoids relying on a shared log axis)
    geom_text(
      data    = blast_data %>% filter(!bdl),
      aes(x = reldate, y = value_num, label = round(value_num, 1)),
      vjust   = -0.8,
      size    = 2.8,
      color   = PARAM_COLORS[["BM blasts with IF (%)"]],
      show.legend = FALSE
    ) +
    # Below-detection-limit: downward open triangle at the detection limit
    geom_point(
      data  = bdl_data,
      aes(x = reldate, y = value_num, col = parameter,
          shape = "Below detection limit"),
      size  = 3
    ) +
    scale_color_manual(values = PARAM_COLORS) +
    scale_shape_manual(
      name   = NULL,
      values = c("Below detection limit" = 6)
    ) +
    scale_y_continuous(
      trans   = "log10",
      limits  = c(ymin_val, ymax_val),
      breaks  = 10^seq(-1, 6),
      labels  = scales::label_number(big.mark = ",", drop0trailing = TRUE)
    ) +
    annotation_logticks(sides = "l", linewidth = 0.2) +
    xlim(-10, x_max) +
    theme_bw() +
    labs(x = NULL, y = NULL, color = NULL)
}

# =============================================================================
# GANTT PANEL
# =============================================================================
make_gantt_panel <- function(treatment_data, important_days, x_max) {
  ggplot() +
    make_gantt_layers(treatment_data, highlight_days = important_days) +
    coord_cartesian(clip = "off") +
    scale_x_continuous(
      breaks = sort(unique(c(seq(0, 500, by = 100), important_days))),
      name   = "Time (days) relative to diagnosis"
    ) +
    xlim(-10, x_max) +
    theme_bw() +
    theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank()
    )
}

# =============================================================================
# BUILD ONE CASE FIGURE
# =============================================================================
build_case_figure <- function(case_id, ref_date, important_days,
                              treatment_path = "/Users/rmvpaeme/2026_TP53/treatment_TP53.xlsx",
                              lab_path       = "/Users/rmvpaeme/2026_TP53/labvals.xlsx",
                              gantt_height   = 0.35) {
  treatments <- load_treatment_data(treatment_path, case_id, ref_date)
  labs_df    <- load_lab_data(lab_path, case_id, ref_date)
  x_max      <- max(labs_df$reldate, na.rm = TRUE)

  p_lab <- make_lab_panel(labs_df, x_max) +
    theme(
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "right"
    )

  p_gantt <- make_gantt_panel(treatments, important_days, x_max)

  ggarrange(
    p_lab,
    p_gantt,
    ncol           = 1,
    nrow           = 2,
    common.legend  = FALSE,
    legend         = NULL,
    heights        = c(1 - gantt_height, gantt_height),
    align          = "v"
  )
}

# =============================================================================
# ASSEMBLE FULL FIGURE
# =============================================================================
panels <- lapply(cases, function(cfg) {
  build_case_figure(
    case_id       = cfg$id,
    ref_date      = cfg$ref_date,
    important_days = cfg$important_days
  )
})

final_figure <- ggarrange(plotlist = panels, ncol = 1)
print(final_figure)

ggsave(
  "/Users/rmvpaeme/2026_TP53/figure.svg",
  plot   = final_figure,
  width  = 16.8,
  height = 14,
  units  = "in",
  dpi    = 300,
  device = "svg"
)

ggsave(
  "/Users/rmvpaeme/2026_TP53/figure.png",
  plot   = final_figure,
  width  = 10,
  height = 12,
  units  = "in",
  dpi    = 300,
  device = "png"
)
