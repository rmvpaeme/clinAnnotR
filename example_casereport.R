# example_casereport.R
# Reproduces the clinical timeline figure for the published HS case report
# using the clinAnnotR package.
#
# Input data (example_casereport/):
#   labvals.xlsx   — lab measurements (case_id, relday, parameter, value)
#   treatment.xlsx — treatment segments (case_id, TREATMENT, START_rel, END_rel, COLOR, CLASS)
#
# Day 0 = HS diagnosis.

devtools::load_all(".")
library(readxl)

# ---- Load data -------------------------------------------------------------

lab <- prep_lab_data(read_excel("example_casereport/labvals.xlsx"))
tx  <- prep_treatment_data(read_excel("example_casereport/treatment.xlsx"))

# ---- Panel definitions -----------------------------------------------------
# Panel 1 (log10): haematology + inflammatory markers (high dynamic range)
# Panel 2 (linear): CRP, platelets, triglycerides
# Panel 3 (linear): haemoglobin, total bilirubin

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
    point_params  = c("triglycerides (mg/dL)"), 
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

# ---- Background shading: treatment phases ----------------------------------
# Three phases visible in the published figure:
#   red    — initial chemotherapy (days 5–7)
#   orange — trametinib + salvage chemotherapy (days 19–38)
#   purple — cyclosporin A + etoposide + dexamethasone (days 43–53)

shade <- list(
  "Case 1" = list(
    list(xmin = 5,  xmax = 7,  fill = "#BF616A", alpha = 0.15),
    list(xmin = 19, xmax = 38, fill = "#D08770", alpha = 0.15),
    list(xmin = 43, xmax = 53, fill = "#B48EAD", alpha = 0.20)
  )
)

# ---- Assemble and save -----------------------------------------------------

fig <- make_clinical_figure(
  lab_data       = lab,
  treatment_data = tx,
  lab_panels     = panels,
  x_range        = c(-5, 55),
  highlight_days = NULL,
  shade_regions  = shade
)

save_clinical_figure(fig, "example_casereport/figure.pdf")
save_clinical_figure(fig, "example_casereport/figure.png", dpi = 300)
message("Figure saved to example_casereport/")
