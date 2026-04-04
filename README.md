# clinAnnotR

<p align="center"><img src="man/figures/logo.png" width="180" alt="clinAnnotR hex logo"/></p>

R package for generating publication-quality multi-panel clinical figures that combine **any numerical laboratory time-series** with per-case **treatment Gantt charts**.

![Example figure](figure_combined_example.png)

*Fictional example data shown above.*

---

## Features

- Any lab parameter can be shown as a **line**, a **point series**, or both — no hardcoded parameter names
- Supports **log₁₀ and linear** y-axes per panel
- **Below-detection-limit** (BDL) values (`<X` notation) are automatically flagged and plotted with a dedicated open-triangle symbol (∇)
- Optional **direct value labels** on points via ggrepel
- **Single-case mode**: when only one case is present, colour is automatically mapped to **parameter** (each parameter gets a distinct colour; all lines are solid) instead of to case
- **Treatment Gantt charts** with multi-segment bars (repeated cycles), custom colours, and drug name labels
- **Gantt panels scale automatically** in height based on the number of treatment rows (`max(1.5, n_treatments × 0.4)`)
- **Treatments grouped by drug class**: rows are ordered by class (earliest first), with separator lines and italic class labels; duplicate segments are silently removed
- **Shared x-axis** across all panels; supports **negative relative days** (measurements before the reference date); protocol timepoint reference lines on every panel
- Every visual element is configurable: colours, shapes, axis ranges, BDL floor, label sizes, bar heights, panel heights, and more
- Reads from **Excel** (absolute dates → relative days computed automatically) or from **pre-processed data frames / CSV files**

---

## Installation

```r
# install.packages("devtools")
devtools::install_github("rmvpaeme/clinAnnotR")
```

---

## Quick start

```r
library(clinAnnotR)

# Load built-in example data (fictional, two cases)
lab <- example_data("lab")
tx  <- example_data("treatment")

# Define panels — any parameters, any combination of lines and points
panels <- list(
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

# Assemble
fig <- make_clinical_figure(
  lab_data       = lab,
  treatment_data = tx,
  lab_panels     = panels,
  highlight_days = c("D1" = 1, "D22" = 22, "D49" = 49)
)

# Save
save_clinical_figure(fig, "figure.pdf")
save_clinical_figure(fig, "figure.png", dpi = 300)
save_clinical_figure(fig, "figure.svg", width = 14, height = 16)
```

---

## Input data

### Lab values

The package expects **long-format** lab data: one row per measurement per parameter per case.

**From Excel** (dates computed automatically):

```r
cases <- list(
  list(id = "Case 1", ref_date = "2025-06-03"),
  list(id = "Case 2", ref_date = "2025-08-12")
)
lab <- load_lab_data("labvals.xlsx", cases)
```

**From CSV / data frame** (relative days already computed):

```r
lab <- prep_lab_data(read.csv("labvals.csv"))
```

Expected columns (`prep_lab_data` defaults):

```
case_id    relday    parameter                    value
Case 1     0         WBC (/µL)                    48500
Case 1     14        peripheral blasts (/µL)      <100
Case 1     0         BM blasts with IF (%)        74
Case 2     0         WBC (/µL)                    27300
```

| Column | Type | Notes |
|--------|------|-------|
| `case_id` | character | Case identifier |
| `relday` | numeric | Days from reference date |
| `parameter` | character | Any label; used in panel specs and legends |
| `value` | character | Numeric string, or `<X` for BDL |

See [`inst/extdata/example_labvals.csv`](inst/extdata/example_labvals.csv) for a complete example.

### Treatment data

**From Excel**:

```r
tx <- load_treatment_data("treatment.xlsx", cases)
```

**From CSV / data frame**:

```r
tx <- prep_treatment_data(read.csv("treatment.csv"))
```

Expected columns:

```
case_id    TREATMENT       START_rel    END_rel    COLOR      CLASS
Case 1     Cytarabine      1            7          #88C0D0    Antimetabolite
Case 1     Venetoclax      49           185        #B48EAD    BCL2 inhibitor
Case 1     Azacitidine     49           53         #EBCB8B    Hypomethylating
Case 1     Azacitidine     77           81         #EBCB8B    Hypomethylating
```

| Column | Type | Notes |
|--------|------|-------|
| `case_id` | character | Must match lab data |
| `TREATMENT` | character | Rows with the same name → multi-segment bar |
| `START_rel` | numeric | Days from reference date |
| `END_rel` | numeric | Days from reference date |
| `COLOR` | character | Hex fill colour per row |
| `CLASS` | character | Drug class label (retained for reference) |

See [`inst/extdata/example_treatment.csv`](inst/extdata/example_treatment.csv) for a complete example.

---

## Panel specification

Each lab panel is defined with `lab_panel()`. The most important arguments:

| Argument | Default | Description |
|----------|---------|-------------|
| `line_params` | `character(0)` | Parameters shown as connected lines |
| `point_params` | `character(0)` | Parameters shown as points |
| `y_scale` | `"log10"` | `"log10"` or `"linear"` |
| `y_label` | `NULL` (auto) | y-axis title |
| `y_limits` | `NULL` (auto) | `c(lo, hi)` |
| `y_breaks` | `NULL` (auto) | Numeric break positions |
| `y_labels` | `NULL` (auto) | Break labels (character vector or function) |
| `show_labels` | `FALSE` | Draw ggrepel value labels on points |
| `label_suffix` | `""` | Appended to labels (e.g. `"%"`) |
| `bdl_floor` | `NULL` → global `0.1` | Floor for BDL values on log axis |
| `highlight_days` | `NULL` → global | Per-panel override of reference lines |
| `height_weight` | `3` | Relative height in figure stack |

Any number of panels can be stacked. Examples:

```r
# Multiple line parameters on a shared linear axis
lab_panel(
  line_params = c("Hemoglobin (g/dL)", "Albumin (g/dL)"),
  y_scale     = "linear",
  y_label     = "Concentration (g/dL)"
)

# Two separate point parameters, log scale, with labels
lab_panel(
  point_params = c("CD34+ (/µL)", "CD3+ (/µL)"),
  y_scale      = "log10",
  y_label      = "Immunophenotype (/µL)",
  show_labels  = TRUE
)

# One panel with different highlight lines than the global set
lab_panel(
  point_params   = "Ferritin (µg/L)",
  y_scale        = "log10",
  highlight_days = c("Transfusion" = 35, "Transfusion" = 90)
)
```

---

## Figure assembly

```r
fig <- make_clinical_figure(
  lab_data            = lab,
  treatment_data      = tx,       # NULL = no Gantt panels
  lab_panels          = panels,
  cases               = NULL,     # NULL = all cases in lab_data
  case_palette        = NULL,     # NULL = auto (Nord-inspired)
  case_shapes         = NULL,     # NULL = auto
  x_range             = NULL,     # NULL = auto (data ± 5 days)
  highlight_days      = c("D1" = 1, "D22" = 22, "D49" = 49),
  gantt_height_weight = 1.5,
  caption             = NULL,     # NULL = built-in default
  base_size           = 9
)
```

**Custom colours and shapes:**

```r
fig <- make_clinical_figure(
  lab, tx, panels,
  case_palette = c("Case 1" = "#1B7837", "Case 2" = "#762A83"),
  case_shapes  = c("Case 1" = 16L,       "Case 2" = 17L),
  x_range      = c(-5, 365),
  highlight_days = c("D1" = 1, "D29" = 29, "D85" = 85)
)
```

**Lab-panel-only figure (no Gantt):**

```r
fig <- make_clinical_figure(lab, treatment_data = NULL, panels)
```

---

## Saving

```r
save_clinical_figure(fig, "figure.pdf")                        # 7 × 8 in, 300 dpi
save_clinical_figure(fig, "figure.png",  dpi = 150)           # raster
save_clinical_figure(fig, "figure.svg",  width = 14, height = 16)  # vector
save_clinical_figure(fig, "figure.tiff", dpi = 600)           # high-res raster
```

The file extension determines the device automatically.

---

## Advanced: building panels manually

For complete control, call the panel functions directly and assemble with [patchwork](https://patchwork.data-imaginist.com/):

```r
library(patchwork)

p1 <- make_timeseries_panel(
  lab_data  = lab,
  spec      = lab_panel(line_params = "WBC (/µL)", y_label = "Count (/µL)"),
  x_range   = c(-5, 190)
)

p2 <- make_gantt_panel(
  treatment_data = tx[tx$case_id == "Case 1", ],
  case_label     = "Case 1 — treatments",
  highlight_days = c("D1" = 1, "D22" = 22),
  x_range        = c(-5, 190),
  show_x         = TRUE
)

(p1 / p2) + plot_layout(heights = c(3, 1.5))
```

---

## Dependencies

```r
install.packages(c(
  "ggplot2", "ggrepel", "patchwork", "scales", "readxl",
  "dplyr", "tibble", "rlang"
))
```

---

## Original script

The original standalone R script (before packaging) is preserved at
[`inst/scripts/figure_combined.R`](inst/scripts/figure_combined.R).
