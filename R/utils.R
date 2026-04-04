# Internal utilities — not exported ============================================

# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Nord-inspired colour defaults
.nord_defaults <- function() {
  list(
    dark  = "#2E3440",
    muted = "#4C566A",
    grid  = "#D8DEE9"
  )
}

# Built-in palettes for auto-assignment
.CA_PALETTE <- c(
  "Case 1" = "#5E81AC", "Case 2" = "#BF616A", "Case 3" = "#A3BE8C",
  "Case 4" = "#EBCB8B", "Case 5" = "#B48EAD", "Case 6" = "#88C0D0",
  "Case 7" = "#D08770", "Case 8" = "#8FBCBB"
)

.CA_SHAPES <- c(16L, 17L, 15L, 18L, 8L, 7L, 9L, 10L)

.CA_LINETYPES <- c("solid", "dashed", "dotted", "longdash", "twodash", "dotdash")

.CA_PARAM_SHAPES <- c(16L, 17L, 15L, 18L, 8L, 3L, 4L, 12L, 11L, 10L)

# Resolve a case palette: extend .CA_PALETTE for unknown case IDs
.resolve_palette <- function(palette, case_ids) {
  palette <- palette %||% .CA_PALETTE
  missing <- setdiff(case_ids, names(palette))
  if (length(missing)) {
    extras <- grDevices::hcl.colors(length(missing), palette = "Dark 3")
    palette <- c(palette, setNames(extras, missing))
  }
  palette[case_ids]
}

# Resolve case shapes
.resolve_shapes <- function(shapes, case_ids) {
  shapes <- shapes %||% setNames(.CA_SHAPES[seq_along(case_ids)], case_ids)
  missing <- setdiff(case_ids, names(shapes))
  if (length(missing)) {
    n <- length(shapes)
    extras <- .CA_SHAPES[(n + 1):(n + length(missing))]
    shapes <- c(shapes, setNames(extras, missing))
  }
  shapes[case_ids]
}

# Assign linetypes to a set of parameter names (cycles if > 6)
.assign_linetypes <- function(params) {
  if (length(params) == 0L) return(character(0))
  lts <- .CA_LINETYPES
  setNames(lts[((seq_along(params) - 1L) %% length(lts)) + 1L], params)
}

# Assign shapes to a set of parameter names (cycles if > 10)
.assign_param_shapes <- function(params) {
  if (length(params) == 0L) return(integer(0))
  sh <- .CA_PARAM_SHAPES
  setNames(sh[((seq_along(params) - 1L) %% length(sh)) + 1L], params)
}

# Assign Nord colours to parameter names (used in single-case mode)
.resolve_param_palette <- function(params) {
  if (length(params) == 0L) return(character(0))
  cols <- unname(.CA_PALETTE)
  setNames(cols[((seq_along(params) - 1L) %% length(cols)) + 1L], params)
}

# Combine an Excel date column and an Excel time column into a single POSIXct.
# readxl returns both as POSIXct; the date column has time 00:00:00 and the
# time column uses a dummy date of 1899-12-31.
.combine_date_time <- function(date_col, time_col, tz = "UTC") {
  as.POSIXct(as.Date(date_col, tz = tz), tz = tz) +
    as.integer(format(time_col, "%H", tz = tz)) * 3600L +
    as.integer(format(time_col, "%M", tz = tz)) *   60L +
    as.integer(format(time_col, "%S", tz = tz))
}

# Parse a character vector of raw values, handling "<X" BDL prefixes.
# Returns a named list: value_num, bdl (logical), value_label (character).
.parse_bdl <- function(value_raw, bdl_floor = 0.1, label_digits = 1L) {
  value_raw   <- as.character(value_raw)
  value_coerce <- suppressWarnings(as.numeric(sub("<", "", value_raw)))
  bdl <- startsWith(value_raw, "<") |
    (!is.na(value_coerce) & value_coerce == 0)
  list(
    value_raw    = value_raw,
    value_coerce = value_coerce,
    bdl          = bdl,
    value_num    = pmax(value_coerce, bdl_floor, na.rm = TRUE),
    value_label  = ifelse(
      startsWith(value_raw, "<"),
      value_raw,
      as.character(round(value_coerce, label_digits))
    )
  )
}
