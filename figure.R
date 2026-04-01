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

CATEGORY_COLORS <- list(
  A = c(NORD$red,    NORD$blue,   NORD$yellow),
  B = c(NORD$blue,   NORD$yellow, NORD$purple, NORD$green),
  C = c(NORD$red,    NORD$yellow, NORD$purple)
)

# =============================================================================
# DATA LOADING
# =============================================================================
load_treatment_data <- function(path, case_id, ref_date) {
  ref <- ymd_hms(paste(ref_date, "00:00:00"))
  read_excel(path) %>%
    filter(PATIENTID == case_id) %>%
    mutate(
      START_rel = as.numeric(difftime(
        as_datetime(paste(START, strftime(START, "%H:%M:%S"))), ref, units = "days"
      )),
      END_rel = as.numeric(difftime(
        as_datetime(paste(END,   strftime(START, "%H:%M:%S"))), ref, units = "days"
      ))
    )
}

load_lab_data <- function(path, case_id, ref_date) {
  ref <- ymd_hms(paste(ref_date, "00:00:00"))
  read_excel(path) %>%
    filter(patientID == case_id) %>%
    mutate(
      reldate = as.numeric(difftime(
        as_datetime(paste(date, strftime(time, "%H:%M:%S"))), ref, units = "days"
      ))
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
  wbc_data <- lab_data %>% filter(parameter == "WBC (/µL)")
  ymin_val <- if (nrow(wbc_data) > 0) max(1, min(wbc_data$value, na.rm = TRUE)) else 1
  ymax_val <- if (nrow(wbc_data) > 0) max(wbc_data$value, na.rm = TRUE)         else 1e4

  ggplot() +
    geom_line(
      data = lab_data %>% filter(category == "A"),
      aes(x = reldate, y = value, col = parameter)
    ) +
    geom_point(
      data = lab_data %>% filter(category == "B"),
      aes(x = reldate, y = value, col = parameter)
    ) +
    geom_line(
      data = lab_data %>% filter(category == "C"),
      aes(x = reldate, y = value, col = parameter)
    ) +
    scale_color_manual(values = unlist(CATEGORY_COLORS)) +
    scale_y_continuous(
      trans  = "log10",
      limits = c(ymin_val, ymax_val)
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
                              treatment_path = "~/2026_TP53/treatment_TP53.xlsx",
                              lab_path       = "~/2026_TP53/labvals.xlsx",
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
  "~/2026_TP53/figure.svg",
  plot   = final_figure,
  width  = 16.8,
  height = 14,
  units  = "in",
  dpi    = 300,
  device = "svg"
)

ggsave(
  "~/2026_TP53/figure.png",
  plot   = final_figure,
  width  = 14,
  height = 16,
  units  = "in",
  dpi    = 300,
  device = "png"
)
