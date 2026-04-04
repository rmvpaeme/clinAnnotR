# data-raw/create_example_data.R
#
# This script documents how the example CSV files in inst/extdata/ were
# created. All patient data are entirely fictional and serve only to
# demonstrate the clinAnnotR package workflow.
#
# The CSVs are read directly by example_data() via system.file() and do not
# need to be re-generated unless you modify them.

# Lab values -------------------------------------------------------------------
# Two fictional cases; three parameters per case:
#   WBC (/µL)                   — displayed as a line
#   peripheral blasts (/µL)     — displayed as points; some BDL (<100)
#   BM blasts with IF (%)       — displayed as points with value labels

lab <- data.frame(
  case_id = c(
    rep("Case 1", 25),
    rep("Case 2", 25)
  ),
  relday = c(
    # Case 1 WBC
    0, 7, 14, 22, 29, 49, 70, 100, 140, 185,
    # Case 1 peripheral blasts
    0, 7, 14, 22, 29, 49, 70, 100, 140, 185,
    # Case 1 BM blasts
    0, 22, 49, 100, 185,
    # Case 2 WBC
    0, 7, 14, 22, 29, 49, 70, 100, 140, 185,
    # Case 2 peripheral blasts
    0, 7, 14, 22, 29, 49, 70, 100, 140, 185,
    # Case 2 BM blasts
    0, 22, 49, 100, 185
  ),
  parameter = c(
    rep("WBC (/\u00b5L)",                  10),
    rep("peripheral blasts (/\u00b5L)",    10),
    rep("BM blasts with IF (%)",            5),
    rep("WBC (/\u00b5L)",                  10),
    rep("peripheral blasts (/\u00b5L)",    10),
    rep("BM blasts with IF (%)",            5)
  ),
  value = c(
    # Case 1 WBC
    "48500", "9200", "3100", "4400", "5200",
    "2800", "3600", "4800", "6200", "7100",
    # Case 1 peripheral blasts (BDL from day 14)
    "19400", "740", "<100", "<100", "<100",
    "<100", "<100", "180", "620", "2100",
    # Case 1 BM blasts
    "74", "38", "<5", "8", "52",
    # Case 2 WBC
    "27300", "5800", "2400", "3200", "3900",
    "2300", "3700", "4200", "5600", "8400",
    # Case 2 peripheral blasts
    "10900", "290", "<100", "<100", "<100",
    "<100", "130", "480", "1350", "3800",
    # Case 2 BM blasts
    "58", "12", "<5", "22", "65"
  ),
  stringsAsFactors = FALSE
)

write.csv(lab, "inst/extdata/example_labvals.csv", row.names = FALSE)

# Treatment records ------------------------------------------------------------
# Repeated rows with the same TREATMENT name produce multi-segment Gantt bars
# (e.g. Azacitidine/Decitabine administered in monthly cycles).

tx <- data.frame(
  case_id   = c(
    rep("Case 1", 10),
    rep("Case 2", 10)
  ),
  TREATMENT = c(
    "Daunorubicin", "Cytarabine",
    "Etoposide", "Cytarabine",
    "Venetoclax",
    "Azacitidine", "Azacitidine", "Azacitidine", "Azacitidine", "Azacitidine",
    "Cytarabine", "Daunorubicin",
    "Etoposide", "Cytarabine",
    "Venetoclax",
    "Decitabine", "Decitabine", "Decitabine", "Decitabine", "Decitabine"
  ),
  START_rel = c(
    1,  1,  22,  22,  49,  49,  77, 105, 133, 161,
    1,  1,  22,  22,  49,  49,  77, 105, 133, 161
  ),
  END_rel = c(
    3,  7,  28,  28, 185,  53,  81, 109, 137, 165,
    7,  3,  28,  28, 185,  53,  81, 109, 137, 165
  ),
  COLOR = c(
    "#D08770", "#88C0D0",
    "#A3BE8C", "#88C0D0",
    "#B48EAD",
    rep("#EBCB8B", 5),
    "#88C0D0", "#D08770",
    "#A3BE8C", "#88C0D0",
    "#B48EAD",
    rep("#EBCB8B", 5)
  ),
  CLASS = c(
    "Anthracycline", "Antimetabolite",
    "Topoisomerase inhibitor", "Antimetabolite",
    "BCL2 inhibitor",
    rep("Hypomethylating", 5),
    "Antimetabolite", "Anthracycline",
    "Topoisomerase inhibitor", "Antimetabolite",
    "BCL2 inhibitor",
    rep("Hypomethylating", 5)
  ),
  stringsAsFactors = FALSE
)

write.csv(tx, "inst/extdata/example_treatment.csv", row.names = FALSE)

message("Example data written to inst/extdata/")
