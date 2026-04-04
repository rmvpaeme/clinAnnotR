#' Prepare a data frame of lab values with pre-computed relative days
#'
#' Use this function when your data already contains a column of days relative
#' to a reference date (e.g., read from a CSV or built with
#' [tibble::tribble()]). For Excel files with absolute dates, use
#' [load_lab_data()] instead.
#'
#' @param data A data frame. Must contain at minimum the columns identified by
#'   `col_caseid`, `col_relday`, `col_parameter`, and `col_value`.
#' @param col_caseid Column name for the case identifier. Default: `"case_id"`.
#' @param col_relday Column name for relative days from reference date.
#'   Default: `"relday"`.
#' @param col_parameter Column name for the measurement parameter label.
#'   Default: `"parameter"`.
#' @param col_value Column name for the raw value. May contain `"<X"` prefixes
#'   to indicate below-detection-limit values. Default: `"value"`.
#' @param bdl_floor Numeric floor for BDL/zero values (for log-axis plotting).
#'   Default: `0.1`.
#' @param label_digits Integer rounding digits for numeric value labels.
#'   Default: `1`.
#'
#' @return A data frame with standardised columns: `case_id`, `relday`,
#'   `parameter`, `value_raw`, `value_coerce`, `bdl`, `value_num`,
#'   `value_label`.
#'
#' @examples
#' raw <- data.frame(
#'   case_id   = c("Case 1", "Case 1", "Case 2"),
#'   relday    = c(0, 22, 0),
#'   parameter = c("Hemoglobin (g/dL)", "Hemoglobin (g/dL)", "WBC (/µL)"),
#'   value     = c("10.5", "<8", "27300")
#' )
#' prep_lab_data(raw)
#'
#' @export
prep_lab_data <- function(
    data,
    col_caseid    = "case_id",
    col_relday    = "relday",
    col_parameter = "parameter",
    col_value     = "value",
    bdl_floor     = 0.1,
    label_digits  = 1L
) {
  val <- .parse_bdl(data[[col_value]], bdl_floor = bdl_floor,
                    label_digits = label_digits)
  data.frame(
    case_id      = as.character(data[[col_caseid]]),
    relday       = as.numeric(data[[col_relday]]),
    parameter    = as.character(data[[col_parameter]]),
    value_raw    = val$value_raw,
    value_coerce = val$value_coerce,
    bdl          = val$bdl,
    value_num    = val$value_num,
    value_label  = val$value_label,
    stringsAsFactors = FALSE
  )
}

#' Prepare a data frame of treatment records with pre-computed relative days
#'
#' Use this when treatment start/end times are already expressed as days
#' relative to a reference date. For Excel files with absolute dates, use
#' [load_treatment_data()] instead.
#'
#' @param data A data frame.
#' @param col_caseid Column for case identifier. Default: `"case_id"`.
#' @param col_treatment Column for drug/treatment name. Default: `"TREATMENT"`.
#' @param col_start_rel Column for treatment start (relative days).
#'   Default: `"START_rel"`.
#' @param col_end_rel Column for treatment end (relative days). Default:
#'   `"END_rel"`.
#' @param col_color Column for bar fill colour (hex string). Default: `"COLOR"`.
#' @param col_class Column for drug class label. Default: `"CLASS"`.
#'
#' @return A data frame with standardised columns: `case_id`, `TREATMENT`,
#'   `START_rel`, `END_rel`, `COLOR`, `CLASS`.
#'
#' @examples
#' raw <- data.frame(
#'   case_id   = "Case 1",
#'   TREATMENT = c("Cytarabine", "Venetoclax"),
#'   START_rel = c(1, 49),
#'   END_rel   = c(7, 185),
#'   COLOR     = c("#88C0D0", "#B48EAD"),
#'   CLASS     = c("Antimetabolite", "BCL2 inhibitor")
#' )
#' prep_treatment_data(raw)
#'
#' @export
prep_treatment_data <- function(
    data,
    col_caseid    = "case_id",
    col_treatment = "TREATMENT",
    col_start_rel = "START_rel",
    col_end_rel   = "END_rel",
    col_color     = "COLOR",
    col_class     = "CLASS"
) {
  data.frame(
    case_id   = as.character(data[[col_caseid]]),
    TREATMENT = as.character(data[[col_treatment]]),
    START_rel = as.numeric(data[[col_start_rel]]),
    END_rel   = as.numeric(data[[col_end_rel]]),
    COLOR     = as.character(data[[col_color]]),
    CLASS     = as.character(data[[col_class]]),
    stringsAsFactors = FALSE
  )
}

#' Load the built-in fictional example datasets
#'
#' Returns pre-processed lab or treatment data for two fictional cases,
#' suitable for demonstrating and testing the clinAnnotR workflow. The data
#' do not represent real patients.
#'
#' @param dataset Character. `"lab"` returns lab values; `"treatment"` returns
#'   treatment records. Default: `"lab"`.
#'
#' @return For `"lab"`: output of [prep_lab_data()]. For `"treatment"`: output
#'   of [prep_treatment_data()].
#'
#' @examples
#' lab <- example_data("lab")
#' tx  <- example_data("treatment")
#' head(lab)
#'
#' @export
example_data <- function(dataset = c("lab", "treatment")) {
  dataset <- match.arg(dataset)
  if (dataset == "lab") {
    path <- system.file("extdata", "example_labvals.csv",
                        package = "clinAnnotR", mustWork = TRUE)
    raw  <- utils::read.csv(path, check.names = FALSE,
                            stringsAsFactors = FALSE)
    prep_lab_data(raw)
  } else {
    path <- system.file("extdata", "example_treatment.csv",
                        package = "clinAnnotR", mustWork = TRUE)
    raw  <- utils::read.csv(path, check.names = FALSE,
                            stringsAsFactors = FALSE)
    prep_treatment_data(raw)
  }
}
