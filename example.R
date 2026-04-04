# example.R
# Reproduces the clinical timeline figure for the published HS case report
# using the clinAnnotR package.
#
# Reference: [published case report citation]
# Reference date (day 0): 2025-01-08 (HS diagnosis)
# Data: casereport_3/labvals_HS.xlsx, casereport_3/treatment_HS.xlsx

devtools::load_all(".")

# ---- Case definition -------------------------------------------------------

cases <- list(
  list(id = "Case 1", ref_date = "2025-01-08")
)

# ---- Load data -------------------------------------------------------------

lab <- load_lab_data(
  "casereport_3/labvals_HS.xlsx",
  cases,
  col_time = NULL   # datetime already combined in the date column
)

tx <- load_treatment_data(
  "casereport_3/treatment_HS.xlsx",
  cases
)

# ---- Panel definitions -----------------------------------------------------
# Panel layout matches the published figure:
#   Panel 1 (log10): haematology + inflammatory markers (high dynamic range)
#   Panel 2 (linear): CRP, platelets, triglycerides
#   Panel 3 (linear): haemoglobin, total bilirubin

panels <- list(
  lab_panel(
    line_params   = c("ALT (U/L)", "ferritin (µg/L)", "fibrinogen (mg/dL)",
                      "neutrophils (/µL)", "WBC (/µL)"),
    y_scale       = "log10",
    y_label       = "",
    height_weight = 4
  ),
  lab_panel(
    line_params   = c("CRP (mg/L)", "platelets (10E3/µL)", "triglycerides (mg/dL)"),
    y_scale       = "linear",
    y_label       = "",
    height_weight = 3
  ),
  lab_panel(
    line_params   = c("hemoglobin (g/dL)", "total bilirubin (mg/dL)"),
    y_scale       = "linear",
    y_label       = "",
    height_weight = 3
  )
)

# ---- Assemble figure -------------------------------------------------------

fig <- make_clinical_figure(
  lab_data       = lab,
  treatment_data = tx,
  lab_panels     = panels,
  x_range        = c(-5, 55),
  highlight_days = NULL
)

# ---- Save ------------------------------------------------------------------

save_clinical_figure(fig, "casereport_3/figure_HS.pdf")
save_clinical_figure(fig, "casereport_3/figure_HS.png", dpi = 150)
message("HS case report figure saved.")
