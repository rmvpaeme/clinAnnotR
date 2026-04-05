# clinAnnotR Shiny App
# Launch with: shiny::runApp(system.file("shiny/clinannotr", package = "clinAnnotR"))
# or:          shiny::runApp("inst/shiny/clinannotr")

library(shiny)
library(readxl)

# Load clinAnnotR: prefer installed package, fall back to package source.
# When launched via shiny::runApp("inst/shiny/clinannotr") the working
# directory is the app folder, so "../../../" is the package root.
if (requireNamespace("clinAnnotR", quietly = TRUE)) {
  library(clinAnnotR)
} else {
  pkg_root <- normalizePath(file.path(getwd(), "../../.."), mustWork = FALSE)
  if (!file.exists(file.path(pkg_root, "DESCRIPTION"))) {
    stop(
      "clinAnnotR is not installed and the package root could not be located.\n",
      "Install the package with: devtools::install(\"<path-to-clinAnnotR>\")",
      call. = FALSE
    )
  }
  loader <- if (requireNamespace("pkgload",  quietly = TRUE)) pkgload::load_all
       else if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all
       else stop("Install 'pkgload' or 'devtools' to run the app from source.",
                 call. = FALSE)
  loader(pkg_root, quiet = TRUE)
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# Parse "Name1=5, Name2=22" -> named numeric vector
parse_named_days <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(NULL)
  parts <- trimws(strsplit(x, ",")[[1]])
  parts <- parts[nzchar(parts)]
  out <- vapply(parts, function(p) {
    kv <- trimws(strsplit(p, "=")[[1]])
    if (length(kv) != 2L) return(setNames(NA_real_, p))
    setNames(suppressWarnings(as.numeric(kv[2])), kv[1])
  }, numeric(1))
  if (any(is.na(out))) return(NULL)
  out
}

# Parse "Case1=#hex1, Case2=#hex2" -> named character vector
parse_named_colors <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(NULL)
  parts <- trimws(strsplit(x, ",")[[1]])
  parts <- parts[nzchar(parts)]
  out <- vapply(parts, function(p) {
    kv <- trimws(strsplit(p, "=")[[1]])
    if (length(kv) != 2L) return(setNames(NA_character_, p))
    setNames(kv[2], kv[1])
  }, character(1))
  if (any(is.na(out))) return(NULL)
  out
}

# Guess likely column name from a list of candidates (case-sensitive first)
guess_col <- function(cols, candidates) {
  m <- match(candidates, cols)
  m <- m[!is.na(m)]
  if (length(m)) cols[m[1]] else ""
}

# Build a selectInput for column mapping
col_select <- function(id, label, choices, selected = "") {
  selectInput(id, label,
              choices  = c("(none / not in file)" = "", choices),
              selected = selected)
}

# Load and pre-process treatment data with optional CLASS / COLOR columns.
# Duplicates the core logic of load_treatment_data() but adds fallbacks for
# optional columns so the Shiny app does not error when they are absent.
load_trt_flexible <- function(path, sheet, cases,
                              col_patientid, col_treatment,
                              col_start, col_end,
                              col_color, col_class,
                              tz = "UTC") {
  raw_all <- readxl::read_excel(path, sheet = as.integer(sheet))

  rows <- lapply(cases, function(case) {
    origin <- as.POSIXct(case$ref_date, format = "%Y-%m-%d", tz = tz)
    raw    <- raw_all[raw_all[[col_patientid]] == case$id, , drop = FALSE]
    if (nrow(raw) == 0L) {
      warning("No rows found for case '", case$id, "'.")
      return(NULL)
    }

    start_dt <- as.POSIXct(raw[[col_start]], tz = tz)
    end_dt   <- as.POSIXct(raw[[col_end]],   tz = tz)

    color_val <- if (!is.null(col_color) && nzchar(col_color) &&
                     col_color %in% names(raw)) {
      as.character(raw[[col_color]])
    } else {
      rep("#88C0D0", nrow(raw))
    }

    class_val <- if (!is.null(col_class) && nzchar(col_class) &&
                     col_class %in% names(raw)) {
      as.character(raw[[col_class]])
    } else {
      rep("Treatment", nrow(raw))
    }

    data.frame(
      case_id   = case$id,
      TREATMENT = as.character(raw[[col_treatment]]),
      START_rel = as.numeric(difftime(start_dt, origin, units = "days")),
      END_rel   = as.numeric(difftime(end_dt,   origin, units = "days")),
      COLOR     = color_val,
      CLASS     = class_val,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows[!vapply(rows, is.null, logical(1L))])
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .well { background: #f8f9fa; border: 1px solid #dee2e6; }
    h4    { margin-top: 0; font-weight: 600; }
    .section-title { font-weight: 600; margin-bottom: 4px; color: #444; }
  "))),

  titlePanel("clinAnnotR — Clinical Figure Builder"),

  tabsetPanel(id = "main_tabs",

    # -----------------------------------------------------------------------
    # Tab 1 — Upload & Column Mapping
    # -----------------------------------------------------------------------
    tabPanel("1. Upload & Columns",
      br(),
      fluidRow(
        # --- Lab file ---
        column(6,
          wellPanel(
            h4("Lab Data (required)"),
            fileInput("lab_file", NULL,
                      label    = "Upload Excel file",
                      accept   = c(".xlsx", ".xls"),
                      buttonLabel = "Browse…"),
            numericInput("lab_sheet", "Sheet number", value = 1, min = 1),
            conditionalPanel("output.lab_cols_ready",
              hr(),
              p(class = "section-title", "Column mapping"),
              uiOutput("ui_lab_patientid"),
              uiOutput("ui_lab_parameter"),
              uiOutput("ui_lab_value"),
              uiOutput("ui_lab_date"),
              uiOutput("ui_lab_time"),
              hr(),
              p(class = "section-title", "Preview (first 8 rows)"),
              div(style = "overflow-x:auto", tableOutput("lab_preview"))
            )
          )
        ),
        # --- Treatment file ---
        column(6,
          wellPanel(
            h4("Treatment Data (optional)"),
            fileInput("trt_file", NULL,
                      label       = "Upload Excel file",
                      accept      = c(".xlsx", ".xls"),
                      buttonLabel = "Browse…"),
            numericInput("trt_sheet", "Sheet number", value = 1, min = 1),
            conditionalPanel("output.trt_cols_ready",
              hr(),
              p(class = "section-title", "Column mapping"),
              uiOutput("ui_trt_patientid"),
              uiOutput("ui_trt_treatment"),
              uiOutput("ui_trt_start"),
              uiOutput("ui_trt_end"),
              uiOutput("ui_trt_color"),
              uiOutput("ui_trt_class"),
              hr(),
              p(class = "section-title", "Preview (first 8 rows)"),
              div(style = "overflow-x:auto", tableOutput("trt_preview"))
            )
          )
        )
      )
    ), # end Tab 1

    # -----------------------------------------------------------------------
    # Tab 2 — Case Definitions
    # -----------------------------------------------------------------------
    tabPanel("2. Cases & Load",
      br(),
      sidebarLayout(
        sidebarPanel(width = 3,
          h4("Case definitions"),
          p("Each case ID must exactly match the patient ID column in the uploaded files."),
          numericInput("n_cases", "Number of cases", value = 1, min = 1, max = 12),
          hr(),
          h4("Options"),
          numericInput("bdl_floor", "BDL floor (log axis)", value = 0.1, min = 0, step = 0.01),
          numericInput("label_digits", "Label digits", value = 1, min = 0, max = 4),
          hr(),
          actionButton("btn_load", "Load Data", class = "btn-primary btn-lg",
                       width = "100%")
        ),
        mainPanel(width = 9,
          uiOutput("ui_cases"),
          br(),
          verbatimTextOutput("load_status")
        )
      )
    ), # end Tab 2

    # -----------------------------------------------------------------------
    # Tab 3 — Figure Setup
    # -----------------------------------------------------------------------
    tabPanel("3. Figure Setup",
      br(),
      sidebarLayout(
        sidebarPanel(width = 4,
          h4("Global options"),
          fluidRow(
            column(6, numericInput("x_min", "X min (days)", value = NA)),
            column(6, numericInput("x_max", "X max (days)", value = NA))
          ),
          textInput("highlight_days", "Highlight days",
                    placeholder = "e.g.  D1=0, D22=21",
                    value = ""),
          textInput("shade_fill",   "Shade regions fill",
                    placeholder  = "hex colour per case, e.g. Case1=#EBCB8B",
                    value = ""),
          textInput("shade_xmin",   "Shade xmin per case",
                    placeholder  = "e.g. Case1=10",
                    value = ""),
          textInput("shade_xmax",   "Shade xmax per case",
                    placeholder  = "e.g. Case1=40",
                    value = ""),
          hr(),
          h4("Appearance"),
          numericInput("base_size", "Base font size", value = 9, min = 5, max = 20),
          numericInput("caption_size", "Caption font size", value = 7, min = 5, max = 16),
          textInput("caption_txt", "Caption text (leave blank for default)", value = ""),
          textInput("case_palette_txt",
                    "Case colour palette",
                    placeholder = "e.g. Case1=#5E81AC, Case2=#BF616A",
                    value = ""),
          hr(),
          h4("Gantt options"),
          checkboxInput("group_by_class",    "Group by drug class",    value = TRUE),
          checkboxInput("show_class_labels", "Show class labels",      value = TRUE),
          numericInput("class_label_margin", "Class label margin",     value = 60,  min = 20, max = 200),
          numericInput("bar_height",         "Bar half-height",        value = 0.35, min = 0.1, max = 1, step = 0.05),
          numericInput("gantt_height_weight","Gantt height weight (blank = auto)",
                       value = NA, min = 1, max = 20)
        ),
        mainPanel(width = 8,
          h4("Lab panels"),
          p("Each panel stacks on a shared x-axis. Add up to 6 panels."),
          numericInput("n_panels", "Number of panels", value = 1, min = 1, max = 6),
          uiOutput("ui_panels")
        )
      )
    ), # end Tab 3

    # -----------------------------------------------------------------------
    # Tab 4 — Preview & Download
    # -----------------------------------------------------------------------
    tabPanel("4. Preview & Download",
      br(),
      sidebarLayout(
        sidebarPanel(width = 3,
          h4("Build figure"),
          actionButton("btn_build", "Build / Refresh", class = "btn-primary btn-lg",
                       width = "100%"),
          hr(),
          h4("Download"),
          selectInput("dl_format", "Format",
                      choices  = c("PDF" = "pdf", "PNG" = "png",
                                   "SVG" = "svg", "TIFF" = "tiff")),
          numericInput("dl_width",  "Width (in)",  value = 7,   min = 2, max = 30),
          numericInput("dl_height", "Height (in)", value = 8,   min = 2, max = 40),
          numericInput("dl_dpi",    "DPI (raster)", value = 300, min = 72, max = 1200),
          downloadButton("btn_download", "Download Figure", style = "width:100%")
        ),
        mainPanel(width = 9,
          plotOutput("fig_preview", height = "680px"),
          br(),
          verbatimTextOutput("fig_error")
        )
      )
    ) # end Tab 4

  ) # end tabsetPanel
) # end fluidPage

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # --- Raw reads for column sniffing ---
  lab_raw <- reactive({
    req(input$lab_file)
    readxl::read_excel(input$lab_file$datapath,
                       sheet = as.integer(input$lab_sheet), n_max = 8)
  })
  trt_raw <- reactive({
    req(input$trt_file)
    readxl::read_excel(input$trt_file$datapath,
                       sheet = as.integer(input$trt_sheet), n_max = 8)
  })

  output$lab_cols_ready <- reactive(!is.null(input$lab_file))
  output$trt_cols_ready <- reactive(!is.null(input$trt_file))
  outputOptions(output, "lab_cols_ready", suspendWhenHidden = FALSE)
  outputOptions(output, "trt_cols_ready", suspendWhenHidden = FALSE)

  # --- Lab column selectors ---
  lab_cols <- reactive(names(lab_raw()))

  output$ui_lab_patientid <- renderUI({
    col_select("lab_patientid", "Patient ID",
               lab_cols(), guess_col(lab_cols(), c("patientID", "patientid", "patient_id", "ID", "id")))
  })
  output$ui_lab_parameter <- renderUI({
    col_select("lab_parameter", "Parameter",
               lab_cols(), guess_col(lab_cols(), c("parameter", "param", "test", "analyte")))
  })
  output$ui_lab_value <- renderUI({
    col_select("lab_value", "Value",
               lab_cols(), guess_col(lab_cols(), c("value", "result", "val", "result_value")))
  })
  output$ui_lab_date <- renderUI({
    col_select("lab_date", "Date",
               lab_cols(), guess_col(lab_cols(), c("date", "Date", "datum", "datetime")))
  })
  output$ui_lab_time <- renderUI({
    col_select("lab_time", "Time (optional — leave blank if embedded in date)",
               lab_cols(), guess_col(lab_cols(), c("time", "Time", "tijd")))
  })

  output$lab_preview <- renderTable({
    req(lab_raw())
    lab_raw()
  }, striped = TRUE, bordered = TRUE, small = TRUE)

  # --- Treatment column selectors ---
  trt_cols <- reactive(names(trt_raw()))

  output$ui_trt_patientid <- renderUI({
    col_select("trt_patientid", "Patient ID",
               trt_cols(), guess_col(trt_cols(), c("PATIENTID", "patientID", "patientid", "patient_id", "ID")))
  })
  output$ui_trt_treatment <- renderUI({
    col_select("trt_treatment", "Treatment name",
               trt_cols(), guess_col(trt_cols(), c("TREATMENT", "treatment", "drug", "Drug", "name")))
  })
  output$ui_trt_start <- renderUI({
    col_select("trt_start", "Start date",
               trt_cols(), guess_col(trt_cols(), c("START", "start", "Start", "start_date")))
  })
  output$ui_trt_end <- renderUI({
    col_select("trt_end", "End date",
               trt_cols(), guess_col(trt_cols(), c("END", "end", "End", "end_date")))
  })
  output$ui_trt_color <- renderUI({
    col_select("trt_color", "Bar colour / hex (optional)",
               trt_cols(), guess_col(trt_cols(), c("COLOR", "color", "colour", "Color")))
  })
  output$ui_trt_class <- renderUI({
    col_select("trt_class", "Drug class (optional)",
               trt_cols(), guess_col(trt_cols(), c("CLASS", "class", "Class", "drug_class")))
  })

  output$trt_preview <- renderTable({
    req(trt_raw())
    trt_raw()
  }, striped = TRUE, bordered = TRUE, small = TRUE)

  # --- Case definition UI ---
  output$ui_cases <- renderUI({
    n <- max(1L, as.integer(input$n_cases %||% 1L))
    lapply(seq_len(n), function(i) {
      wellPanel(
        fluidRow(
          column(1, tags$b(paste0("Case ", i), style = "line-height:34px")),
          column(4, textInput(paste0("case_id_",  i),
                              label       = "Case ID (as in file)",
                              placeholder = paste0("e.g. Patient ", i))),
          column(4, textInput(paste0("case_ref_", i),
                              label       = "Reference date",
                              placeholder = "YYYY-MM-DD"))
        )
      )
    })
  })

  # --- Reactive data store ---
  store <- reactiveValues(lab = NULL, trt = NULL, params = NULL, status = "")

  observeEvent(input$btn_load, {
    store$lab    <- NULL
    store$trt    <- NULL
    store$params <- NULL
    store$status <- ""

    tryCatch({
      req(input$lab_file, input$lab_patientid, input$lab_parameter,
          input$lab_value, input$lab_date)

      n_cases <- max(1L, as.integer(input$n_cases %||% 1L))
      cases <- lapply(seq_len(n_cases), function(i) {
        id  <- trimws(input[[paste0("case_id_",  i)]] %||% "")
        ref <- trimws(input[[paste0("case_ref_", i)]] %||% "")
        if (!nzchar(id))  stop(sprintf("Case %d: ID is empty.", i))
        if (!nzchar(ref)) stop(sprintf("Case %d: reference date is empty.", i))
        if (is.na(as.Date(ref, format = "%Y-%m-%d")))
          stop(sprintf("Case %d: reference date '%s' is not YYYY-MM-DD.", i, ref))
        list(id = id, ref_date = ref)
      })

      col_time_val <- if (!is.null(input$lab_time) && nzchar(input$lab_time))
        input$lab_time else NULL

      lab <- load_lab_data(
        path          = input$lab_file$datapath,
        cases         = cases,
        sheet         = as.integer(input$lab_sheet %||% 1L),
        col_patientid = input$lab_patientid,
        col_parameter = input$lab_parameter,
        col_value     = input$lab_value,
        col_date      = input$lab_date,
        col_time      = col_time_val,
        bdl_floor     = as.numeric(input$bdl_floor  %||% 0.1),
        label_digits  = as.integer(input$label_digits %||% 1L)
      )
      store$lab    <- lab
      store$params <- sort(unique(lab$parameter))

      trt_msg <- "No treatment data loaded."
      if (!is.null(input$trt_file) && nzchar(input$trt_patientid %||% "") &&
          nzchar(input$trt_treatment %||% "") &&
          nzchar(input$trt_start %||% "") &&
          nzchar(input$trt_end   %||% "")) {

        trt <- load_trt_flexible(
          path          = input$trt_file$datapath,
          sheet         = as.integer(input$trt_sheet %||% 1L),
          cases         = cases,
          col_patientid = input$trt_patientid,
          col_treatment = input$trt_treatment,
          col_start     = input$trt_start,
          col_end       = input$trt_end,
          col_color     = input$trt_color %||% "",
          col_class     = input$trt_class %||% ""
        )
        store$trt <- trt
        trt_msg <- paste(nrow(trt), "treatment rows loaded.")
      }

      store$status <- paste0(
        "Data loaded successfully.\n",
        nrow(lab), " lab rows | ",
        length(store$params), " parameter(s): ",
        paste(store$params, collapse = ", "), "\n",
        trt_msg
      )

    }, error = function(e) {
      store$status <- paste("Error:", conditionMessage(e))
    })
  })

  output$load_status <- renderText(store$status)

  # --- Panel spec UI (dynamic) ---
  output$ui_panels <- renderUI({
    n      <- max(1L, as.integer(input$n_panels %||% 1L))
    params <- store$params %||% character(0)

    lapply(seq_len(n), function(i) {
      wellPanel(
        h4(paste("Panel", i)),
        fluidRow(
          column(6,
            selectInput(paste0("panel_line_", i), "Line parameters",
                        choices  = params,
                        multiple = TRUE,
                        selected = if (i == 1 && length(params)) params[1] else NULL)
          ),
          column(6,
            selectInput(paste0("panel_point_", i), "Point-only parameters",
                        choices  = params,
                        multiple = TRUE,
                        selected = NULL)
          )
        ),
        fluidRow(
          column(3,
            selectInput(paste0("panel_yscale_", i), "Y scale",
                        choices = c("log10", "linear"), selected = "log10")
          ),
          column(5,
            textInput(paste0("panel_ylabel_", i), "Y-axis label (blank = auto)", value = "")
          ),
          column(2,
            numericInput(paste0("panel_height_", i), "Height", value = 3, min = 1, max = 10)
          ),
          column(2,
            checkboxInput(paste0("panel_labels_", i), "Value labels", value = FALSE)
          )
        ),
        fluidRow(
          column(3,
            textInput(paste0("panel_ylo_", i), "Y min (blank = auto)", value = "")
          ),
          column(3,
            textInput(paste0("panel_yhi_", i), "Y max (blank = auto)", value = "")
          ),
          column(3,
            textInput(paste0("panel_suffix_", i), "Label suffix (e.g. %)", value = "")
          ),
          column(3,
            numericInput(paste0("panel_bdl_floor_", i),
                         "BDL floor (override)", value = NA, min = 0, step = 0.01)
          )
        )
      )
    })
  })

  # --- Build figure ---
  built_fig <- eventReactive(input$btn_build, {
    req(store$lab)

    n_panels <- max(1L, as.integer(input$n_panels %||% 1L))

    panels <- lapply(seq_len(n_panels), function(i) {
      line_p  <- input[[paste0("panel_line_",   i)]]
      point_p <- input[[paste0("panel_point_",  i)]]
      yscale  <- input[[paste0("panel_yscale_", i)]] %||% "log10"
      ylabel  <- input[[paste0("panel_ylabel_", i)]] %||% ""
      height  <- as.numeric(input[[paste0("panel_height_", i)]] %||% 3)
      show_lb <- isTRUE(input[[paste0("panel_labels_", i)]])
      suffix  <- input[[paste0("panel_suffix_", i)]] %||% ""
      ylo     <- suppressWarnings(as.numeric(input[[paste0("panel_ylo_", i)]]))
      yhi     <- suppressWarnings(as.numeric(input[[paste0("panel_yhi_", i)]]))
      bdl_ov  <- suppressWarnings(as.numeric(input[[paste0("panel_bdl_floor_", i)]]))

      if (is.null(line_p))  line_p  <- character(0)
      if (is.null(point_p)) point_p <- character(0)
      if (!length(line_p) && !length(point_p)) return(NULL)

      ylimits <- if (!is.na(ylo) && !is.na(yhi)) c(ylo, yhi) else NULL

      lab_panel(
        line_params   = line_p,
        point_params  = point_p,
        y_scale       = yscale,
        y_label       = if (nzchar(trimws(ylabel))) ylabel else NULL,
        y_limits      = ylimits,
        show_labels   = show_lb,
        label_suffix  = suffix,
        bdl_floor     = if (!is.na(bdl_ov)) bdl_ov else NULL,
        height_weight = height
      )
    })

    panels <- Filter(Negate(is.null), panels)
    if (!length(panels)) stop("No panels defined — select at least one parameter in Panel 1.")

    # x range
    x_range <- NULL
    xmin <- suppressWarnings(as.numeric(input$x_min))
    xmax <- suppressWarnings(as.numeric(input$x_max))
    if (!is.na(xmin) && !is.na(xmax)) x_range <- c(xmin, xmax)

    # highlight days
    hl_days <- parse_named_days(input$highlight_days)

    # shade regions  (simple: one region per case)
    shade <- NULL
    fills <- parse_named_colors(input$shade_fill)
    xmins <- parse_named_days(input$shade_xmin)
    xmaxs <- parse_named_days(input$shade_xmax)
    if (!is.null(fills) && !is.null(xmins) && !is.null(xmaxs)) {
      shade <- lapply(names(fills), function(cid) {
        if (!cid %in% names(xmins) || !cid %in% names(xmaxs)) return(NULL)
        list(list(xmin = xmins[[cid]], xmax = xmaxs[[cid]],
                  fill = fills[[cid]], alpha = 0.15))
      })
      names(shade) <- names(fills)
      shade <- Filter(Negate(is.null), shade)
      if (!length(shade)) shade <- NULL
    }

    # case palette
    case_pal <- parse_named_colors(input$case_palette_txt)

    # caption
    cap <- if (nzchar(trimws(input$caption_txt %||% ""))) input$caption_txt else NULL

    # gantt height weight
    gantt_hw <- suppressWarnings(as.numeric(input$gantt_height_weight))
    if (is.na(gantt_hw)) gantt_hw <- NULL

    make_clinical_figure(
      lab_data           = store$lab,
      treatment_data     = store$trt,
      lab_panels         = panels,
      case_palette       = case_pal,
      x_range            = x_range,
      highlight_days     = hl_days,
      shade_regions      = shade,
      gantt_height_weight = gantt_hw,
      caption            = cap,
      caption_size       = as.numeric(input$caption_size %||% 7),
      base_size          = as.numeric(input$base_size    %||% 9)
    )
  })

  output$fig_preview <- renderPlot({
    tryCatch(
      built_fig(),
      error = function(e) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, paste("Error:\n", conditionMessage(e)),
                       col = "firebrick", cex = 1.1)
      }
    )
  }, res = 120)

  output$fig_error <- renderText({
    tryCatch({ built_fig(); "" },
             error = function(e) paste("Error:", conditionMessage(e)))
  })

  # --- Download ---
  output$btn_download <- downloadHandler(
    filename = function() {
      paste0("clinannotr_figure.", input$dl_format %||% "pdf")
    },
    content = function(file) {
      save_clinical_figure(
        figure = built_fig(),
        path   = file,
        width  = as.numeric(input$dl_width  %||% 7),
        height = as.numeric(input$dl_height %||% 8),
        dpi    = as.numeric(input$dl_dpi    %||% 300)
      )
    }
  )

} # end server

shinyApp(ui, server)
