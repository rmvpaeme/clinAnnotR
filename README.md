# clinAnnotR

R script to generate a combined four-panel clinical figure showing laboratory values and treatment timelines across multiple cases.

![Example figure](figure_combined_example.png)

*Fictional example data shown above.*

---

## What the figure shows

| Panel | Content |
|-------|---------|
| **Top** | WBC (line) and peripheral blasts (points) on a shared log₁₀ axis (counts/µL) |
| **Second** | Bone marrow blasts by immunofluorescence (%), log₁₀ scale, with direct value labels |
| **Third / Fourth** | Treatment Gantt chart per case — one row per drug, bars coloured by drug class |

Dashed vertical lines mark protocol timepoints (D1, D22, D49). Below-detection-limit values are plotted at a floor of 0.1 with an open-triangle symbol (∇).

---

## Dependencies

```r
install.packages(c("tidyverse", "readxl", "patchwork", "scales", "ggrepel"))
```

---

## Input data

The script reads two Excel files. The expected column structure is illustrated by the CSV examples below.

### Lab values — `labvals.xlsx`

See [`example_labvals.csv`](example_labvals.csv) for the expected structure.

| Column | Type | Description |
|--------|------|-------------|
| `patientID` | character | Case identifier, must match `cases` list in script |
| `parameter` | character | One of `WBC (/µL)`, `peripheral blasts (/µL)`, `BM blasts with IF (%)` |
| `value` | character | Numeric value, or `<X` for below-detection-limit results |
| `date` | date | Date of measurement (Excel date type) |
| `time` | time | Time of measurement (Excel time type) |

```
patientID    parameter                   value    date          time
Case 1       WBC (/µL)                   48500    2025-06-03    08:00
Case 1       peripheral blasts (/µL)     <100     2025-06-17    08:00
Case 1       BM blasts with IF (%)       74       2025-06-03    10:30
Case 2       WBC (/µL)                   27300    2025-08-12    09:15
```

### Treatment data — `treatment.xlsx`

See [`example_treatment.csv`](example_treatment.csv) for the expected structure.

| Column | Type | Description |
|--------|------|-------------|
| `PATIENTID` | character | Case identifier |
| `TREATMENT` | character | Drug name (rows with the same name appear on one Gantt row) |
| `START` | date/time | Treatment start (Excel date-time) |
| `END` | date/time | Treatment end (Excel date-time) |
| `COLOR` | character | Hex fill colour for the bar, e.g. `#88C0D0` |
| `CLASS` | character | Drug class label |

```
PATIENTID    TREATMENT       START                END                  COLOR      CLASS
Case 1       Daunorubicin    2025-06-04 08:00     2025-06-06 08:00     #D08770    Anthracycline
Case 1       Cytarabine      2025-06-04 08:00     2025-06-10 08:00     #88C0D0    Antimetabolite
Case 1       Venetoclax      2025-07-22 08:00     2025-11-05 08:00     #B48EAD    BCL2 inhibitor
Case 1       Azacitidine     2025-07-22 08:00     2025-07-26 08:00     #EBCB8B    Hypomethylating
...
```

Multiple rows with the same `TREATMENT` name produce a multi-segment bar (e.g. repeated Azacitidine cycles).

---

## Configuration

Edit the top section of `figure_combined.R`:

```r
TREATMENT_PATH <- "treatment.xlsx"
LAB_PATH       <- "labvals.xlsx"

cases <- list(
  list(id = "Case 1", ref_date = "2025-06-03", highlight_days = c(0, 29)),
  list(id = "Case 2", ref_date = "2025-08-12", highlight_days = c(0, 29))
)
```

- `id` must match the `PATIENTID` / `patientID` column in the Excel files.
- `ref_date` is the diagnosis date; all x-axis values are days relative to this date.
- `highlight_days` adds extra dashed reference lines per case (in addition to D1/D22/D49).

To add or remove cases, extend or shorten the `cases` list accordingly.

---

## Output

Running the script writes three files to the working directory:

| File | Dimensions |
|------|-----------|
| `figure_combined.svg` | 14 × 16 in |
| `figure_combined.png` | 7 × 8 in, 300 dpi |
| `figure_combined.pdf` | 7 × 8 in |
