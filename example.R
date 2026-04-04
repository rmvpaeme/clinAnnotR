# example.R
# Demonstrates all data loading paths in clinAnnotR using the built-in
# example data in inst/extdata/. Run from the package root directory.

devtools::load_all(".")
library(readxl)

# ============================================================
# 1. Built-in example data (fictional, two cases)
#    Loaded via example_data() — no files needed
# ============================================================

lab <- example_data("lab")
tx  <- example_data("treatment")

panels_2case <- list(
  lab_panel(
    line_params   = "WBC (/µL)",
    point_params  = "peripheral blasts (/µL)",
    y_label       = "Count (/µL)",
    height_weight = 4
  ),
  lab_panel(
    point_params  = "BM blasts with IF (%)",
    show_labels   = TRUE,
    label_suffix  = "%",
    y_limits      = c(0.05, 200),
    y_breaks      = c(0.1, 1, 5, 20, 100),
    y_labels      = paste0(c(0.1, 1, 5, 20, 100), "%"),
    y_label       = "BM blasts (%)",
    height_weight = 3
  )
)

fig_builtin <- make_clinical_figure(
  lab_data       = lab,
  treatment_data = tx,
  lab_panels     = panels_2case,
  highlight_days = c("D1" = 1, "D22" = 22, "D49" = 49)
)
save_clinical_figure(fig_builtin, "inst/extdata/example_figure.pdf")
save_clinical_figure(fig_builtin, "inst/extdata/example_figure.png", dpi = 150)
message("1. Built-in example: OK")

# ============================================================
# 2. Relative-day Excel (inst/extdata/example_labvals.xlsx)
#    Standard format — same data as above, from file
# ============================================================

lab_rel <- prep_lab_data(read_excel("inst/extdata/example_labvals.xlsx"))
tx_rel  <- prep_treatment_data(read_excel("inst/extdata/example_treatment.xlsx"))

fig_rel <- make_clinical_figure(
  lab_data       = lab_rel,
  treatment_data = tx_rel,
  lab_panels     = panels_2case,
  highlight_days = c("D1" = 1, "D22" = 22, "D49" = 49)
)
message("2. Relative-day xlsx: OK (", nrow(lab_rel), " lab rows, ",
        nrow(tx_rel), " tx rows)")

# ============================================================
# 3. Absolute-date Excel (inst/extdata/example_labvals_dates.xlsx)
#    Reference dates per case; load_lab_data() computes relday
# ============================================================

cases <- list(
  list(id = "Case 1", ref_date = "2025-01-01"),
  list(id = "Case 2", ref_date = "2025-03-15")
)

lab_abs <- load_lab_data(
  "inst/extdata/example_labvals_dates.xlsx",
  cases
)
tx_abs <- load_treatment_data(
  "inst/extdata/example_treatment_dates.xlsx",
  cases
)

fig_abs <- make_clinical_figure(
  lab_data       = lab_abs,
  treatment_data = tx_abs,
  lab_panels     = panels_2case,
  highlight_days = c("D1" = 1, "D22" = 22, "D49" = 49)
)
message("3. Absolute-date xlsx: OK (relday range ",
        round(min(lab_abs$relday), 1), " to ", round(max(lab_abs$relday), 1), ")")

message("\nAll examples completed successfully.")
message("For the published HS case figure, run: source('example_casereport.R')")
