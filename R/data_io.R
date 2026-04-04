#' Load and pre-process lab data from an Excel file
#'
#' Reads an Excel workbook, filters to the requested cases, computes relative
#' days from each case's reference date, and flags below-detection-limit (BDL)
#' values. BDL values are those whose raw string starts with `"<"` or whose
#' numeric value is zero.
#'
#' @param path Character. Path to the Excel file.
#' @param cases A list of case specifications. Each element must be a named list
#'   with:
#'   * `id` — character, must match the patient ID column in the file.
#'   * `ref_date` — character in `"YYYY-MM-DD"` format, used as day 0.
#'
#'   Example: `list(list(id = "Case 1", ref_date = "2025-06-03"))`.
#' @param sheet Sheet name or index passed to [readxl::read_excel()]. Default: `1`.
#' @param col_patientid Column name for the patient/case identifier. Default:
#'   `"patientID"`.
#' @param col_parameter Column name for the measurement parameter label. Default:
#'   `"parameter"`.
#' @param col_value Column name for the raw value. May be character (supports
#'   `"<X"` BDL notation) or numeric. Default: `"value"`.
#' @param col_date Column name for the date of measurement (Excel date or
#'   date-time). Default: `"date"`.
#' @param col_time Column name for the time of measurement (Excel time).
#'   Pass `NULL` if time information is embedded in `col_date` or not available;
#'   midnight (00:00:00) is then assumed. Default: `"time"`.
#' @param bdl_floor Numeric. Floor value applied to BDL and zero observations
#'   so they can be plotted on a log10 axis. Default: `0.1`.
#' @param label_digits Integer. Rounding digits for numeric value labels shown
#'   on plot points. Default: `1`.
#' @param tz Character. Time zone for all date-time arithmetic. Default: `"UTC"`.
#'
#' @return A data frame with columns:
#'   `case_id`, `relday`, `parameter`, `value_raw`, `value_coerce`,
#'   `bdl`, `value_num`, `value_label`.
#'
#' @seealso [prep_lab_data()] for data frames with pre-computed relative days.
#'
#' @examples
#' \dontrun{
#' cases <- list(
#'   list(id = "Case 1", ref_date = "2025-06-03"),
#'   list(id = "Case 2", ref_date = "2025-08-12")
#' )
#' lab <- load_lab_data("labvals.xlsx", cases)
#' }
#'
#' @export
load_lab_data <- function(
    path,
    cases,
    sheet          = 1,
    col_patientid  = "patientID",
    col_parameter  = "parameter",
    col_value      = "value",
    col_date       = "date",
    col_time       = "time",
    bdl_floor      = 0.1,
    label_digits   = 1L,
    tz             = "UTC"
) {
  raw_all <- readxl::read_excel(path, sheet = sheet)

  rows <- lapply(cases, function(case) {
    .origin <- as.POSIXct(case$ref_date, format = "%Y-%m-%d", tz = tz)
    raw <- raw_all[raw_all[[col_patientid]] == case$id, , drop = FALSE]
    if (nrow(raw) == 0L) {
      warning("No rows found for case '", case$id, "' in '", path, "'.")
      return(NULL)
    }
    date_dt <- as.POSIXct(raw[[col_date]], tz = tz)
    if (!is.null(col_time) && col_time %in% names(raw)) {
      date_dt <- .combine_date_time(raw[[col_date]], raw[[col_time]], tz = tz)
    }
    val <- .parse_bdl(raw[[col_value]], bdl_floor = bdl_floor,
                      label_digits = label_digits)
    data.frame(
      case_id      = case$id,
      relday       = as.numeric(difftime(date_dt, .origin, units = "days")),
      parameter    = as.character(raw[[col_parameter]]),
      value_raw    = val$value_raw,
      value_coerce = val$value_coerce,
      bdl          = val$bdl,
      value_num    = val$value_num,
      value_label  = val$value_label,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows[!vapply(rows, is.null, logical(1L))])
}

#' Load and pre-process treatment data from an Excel file
#'
#' Reads treatment records from an Excel workbook and computes start and end
#' times as days relative to each case's reference date.
#'
#' @param path Character. Path to the Excel file.
#' @param cases A list of case specifications (same format as [load_lab_data()]).
#' @param sheet Sheet name or index. Default: `1`.
#' @param col_patientid Column name for the patient/case identifier.
#'   Default: `"PATIENTID"`.
#' @param col_treatment Column name for the drug/treatment name. Rows with the
#'   same name for one case are rendered as a multi-segment Gantt bar.
#'   Default: `"TREATMENT"`.
#' @param col_start Column name for the treatment start date-time (Excel
#'   date-time). Default: `"START"`.
#' @param col_end Column name for the treatment end date-time. Default: `"END"`.
#' @param col_color Column name for the bar fill colour (hex string, e.g.
#'   `"#88C0D0"`). Colour is stored per row so different segments of the same
#'   treatment can differ. Default: `"COLOR"`.
#' @param col_class Column name for the drug class label (e.g. `"Anthracycline"`).
#'   Retained for user reference; not currently used as a visual aesthetic.
#'   Default: `"CLASS"`.
#' @param tz Character. Time zone. Default: `"UTC"`.
#'
#' @return A data frame with columns:
#'   `case_id`, `TREATMENT`, `START_rel`, `END_rel`, `COLOR`, `CLASS`.
#'
#' @seealso [prep_treatment_data()] for data frames with pre-computed relative days.
#'
#' @examples
#' \dontrun{
#' cases <- list(list(id = "Case 1", ref_date = "2025-06-03"))
#' tx <- load_treatment_data("treatment.xlsx", cases)
#' }
#'
#' @export
load_treatment_data <- function(
    path,
    cases,
    sheet          = 1,
    col_patientid  = "PATIENTID",
    col_treatment  = "TREATMENT",
    col_start      = "START",
    col_end        = "END",
    col_color      = "COLOR",
    col_class      = "CLASS",
    tz             = "UTC"
) {
  raw_all <- readxl::read_excel(path, sheet = sheet)

  rows <- lapply(cases, function(case) {
    .origin <- as.POSIXct(case$ref_date, format = "%Y-%m-%d", tz = tz)
    raw <- raw_all[raw_all[[col_patientid]] == case$id, , drop = FALSE]
    if (nrow(raw) == 0L) {
      warning("No rows found for case '", case$id, "' in '", path, "'.")
      return(NULL)
    }
    data.frame(
      case_id   = case$id,
      TREATMENT = as.character(raw[[col_treatment]]),
      START_rel = as.numeric(difftime(
        .combine_date_time(raw[[col_start]], raw[[col_start]], tz = tz),
        .origin, units = "days"
      )),
      END_rel   = as.numeric(difftime(
        .combine_date_time(raw[[col_end]],   raw[[col_start]], tz = tz),
        .origin, units = "days"
      )),
      COLOR     = as.character(raw[[col_color]]),
      CLASS     = as.character(raw[[col_class]]),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows[!vapply(rows, is.null, logical(1L))])
}
