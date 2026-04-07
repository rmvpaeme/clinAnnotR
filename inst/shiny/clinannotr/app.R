# clinAnnotR Shiny App
# Launch: shiny::runApp("inst/shiny/clinannotr")  OR  clinAnnotR::run_app()

library(shiny)
library(readxl)
# Explicitly loaded so rsconnect detects and installs these on shinyapps.io.
# They are imports of clinAnnotR and must be present before the package loads.
library(ggplot2)
library(ggrepel)
library(patchwork)
library(scales)
library(dplyr)
library(rlang)

# Load clinAnnotR: installed package first, then fall back to package source.
# When launched via shiny::runApp("inst/shiny/clinannotr") the working
# directory is the app folder, so "../../../" resolves to the package root.
if (requireNamespace("clinAnnotR", quietly = TRUE)) {
  library(clinAnnotR)
} else {
  pkg_root <- normalizePath(file.path(getwd(), "../../.."), mustWork = FALSE)
  if (file.exists(file.path(pkg_root, "DESCRIPTION"))) {
    # Running from local source (devtools / shiny::runApp)
    loader <- if (requireNamespace("pkgload",  quietly = TRUE)) pkgload::load_all
         else if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all
         else stop("Install 'pkgload' or 'devtools' to run from source.", call. = FALSE)
    loader(pkg_root, quiet = TRUE)
  } else {
    # shinyapps.io: install from GitHub
    if (!requireNamespace("remotes", quietly = TRUE))
      install.packages("remotes")
    remotes::install_github("rmvpaeme/clinAnnotR")
    library(clinAnnotR)
  }
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

guess_col <- function(cols, candidates) {
  m <- match(candidates, cols)
  m <- m[!is.na(m)]
  if (length(m)) cols[m[1]] else ""
}

parse_named_days <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(NULL)
  parts <- trimws(strsplit(x, ",")[[1]])
  parts <- parts[nzchar(parts)]
  out <- vapply(parts, function(p) {
    kv <- trimws(strsplit(p, "=")[[1]])
    if (length(kv) != 2L) return(setNames(NA_real_, p))
    setNames(suppressWarnings(as.numeric(kv[2])), kv[1])
  }, numeric(1))
  if (any(is.na(out))) NULL else out
}

load_trt_flexible <- function(path, sheet, cases,
                              col_patientid, col_treatment,
                              col_start, col_end,
                              col_color = "", col_class = "",
                              tz = "UTC") {
  raw_all <- readxl::read_excel(path, sheet = as.integer(sheet))
  rows <- lapply(cases, function(case) {
    origin <- as.POSIXct(case$ref_date, format = "%Y-%m-%d", tz = tz)
    raw    <- raw_all[raw_all[[col_patientid]] == case$id, , drop = FALSE]
    if (nrow(raw) == 0L) { warning("No rows for case '", case$id, "'."); return(NULL) }
    data.frame(
      case_id   = case$id,
      TREATMENT = as.character(raw[[col_treatment]]),
      START_rel = as.numeric(difftime(as.POSIXct(raw[[col_start]], tz = tz), origin, units = "days")),
      END_rel   = as.numeric(difftime(as.POSIXct(raw[[col_end]],   tz = tz), origin, units = "days")),
      COLOR     = if (nzchar(col_color) && col_color %in% names(raw)) as.character(raw[[col_color]])
                  else rep("#88C0D0", nrow(raw)),
      CLASS     = if (nzchar(col_class) && col_class %in% names(raw)) as.character(raw[[col_class]])
                  else rep("Treatment", nrow(raw)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows[!vapply(rows, is.null, logical(1L))])
}

col_sel <- function(id, label, choices, selected = "")
  selectInput(id, label, choices = c("(none)" = "", choices), selected = selected,
              width = "100%")

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-size: 13px; }
    .well { padding: 10px; margin-bottom: 8px; }
    h5 { font-weight: 600; margin: 10px 0 4px; }
    .form-group { margin-bottom: 6px; }
    #fig { border: 1px solid #dee2e6; border-radius: 4px; }
  "))),

  titlePanel("clinAnnotR — Clinical Figure Builder", windowTitle = "clinAnnotR"),

  sidebarLayout(
    sidebarPanel(width = 4, style = "overflow-y:auto; max-height:90vh;",

      # --- Lab file ---
      h5("Lab data (required)"),
      fileInput("lab_file", label = NULL,
                accept = c(".xlsx", ".xls"), buttonLabel = "Browse…",
                placeholder = "No file selected"),

      conditionalPanel("output.lab_ready",
        fluidRow(
          column(6, uiOutput("ui_lab_pid")),
          column(6, uiOutput("ui_lab_param"))
        ),
        fluidRow(
          column(6, uiOutput("ui_lab_val")),
          column(6, uiOutput("ui_lab_date"))
        ),
        uiOutput("ui_lab_time")
      ),

      hr(),

      # --- Treatment file ---
      h5("Treatment data (optional)"),
      fileInput("trt_file", label = NULL,
                accept = c(".xlsx", ".xls"), buttonLabel = "Browse…",
                placeholder = "No file selected"),

      conditionalPanel("output.trt_ready",
        fluidRow(
          column(6, uiOutput("ui_trt_pid")),
          column(6, uiOutput("ui_trt_trt"))
        ),
        fluidRow(
          column(6, uiOutput("ui_trt_start")),
          column(6, uiOutput("ui_trt_end"))
        ),
        fluidRow(
          column(6, uiOutput("ui_trt_color")),
          column(6, uiOutput("ui_trt_class"))
        )
      ),

      hr(),

      # --- Cases ---
      h5("Cases"),
      numericInput("n_cases", "Number of cases", value = 1, min = 1, max = 12,
                   width = "120px"),
      uiOutput("ui_cases"),

      actionButton("btn_load", "Load & build figure",
                   class = "btn-primary btn-block", width = "100%"),
      verbatimTextOutput("load_msg"),

      # --- Figure controls (visible after data loaded) ---
      conditionalPanel("output.data_loaded",

        hr(),
        h5("Lab panels"),
        p(style = "color:#666; font-size:11px",
          "Assign parameters to panels. Each panel stacks on the same x-axis."),
        numericInput("n_panels", "Number of panels", value = 1, min = 1, max = 4,
                     width = "120px"),
        uiOutput("ui_panels"),

        hr(),
        h5("Options"),
        fluidRow(
          column(6, numericInput("x_min", "X min (days)", value = NA)),
          column(6, numericInput("x_max", "X max (days)", value = NA))
        ),
        textInput("highlight_days", "Highlight days",
                  placeholder = "D1=0, D22=21", value = ""),
        fluidRow(
          column(6, numericInput("base_size", "Font size", value = 9, min = 6, max = 16)),
          column(6, checkboxInput("group_by_class", "Group Gantt by class", value = TRUE))
        ),

        hr(),
        h5("Download"),
        fluidRow(
          column(5, selectInput("dl_fmt", "Format",
                                choices = c("PDF"="pdf","PNG"="png","SVG"="svg","TIFF"="tiff"))),
          column(4, numericInput("dl_w", "Width (in)", value = 7, min = 3, max = 30)),
          column(3, numericInput("dl_h", "Height (in)", value = 8, min = 3, max = 40))
        ),
        downloadButton("btn_dl", "Download figure", style = "width:100%")
      )
    ),

    mainPanel(width = 8,
      tabsetPanel(
        tabPanel("Figure",
          br(),
          plotOutput("fig", height = "720px"),
          verbatimTextOutput("fig_err")
        ),
        tabPanel("Lab data",
          br(),
          div(style = "overflow-x:auto", tableOutput("tbl_lab"))
        ),
        tabPanel("Treatment data",
          br(),
          div(style = "overflow-x:auto", tableOutput("tbl_trt"))
        )
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  # Raw reads (8 rows) for column sniffing
  lab_raw <- reactive({
    req(input$lab_file)
    readxl::read_excel(input$lab_file$datapath, n_max = 8)
  })
  trt_raw <- reactive({
    req(input$trt_file)
    readxl::read_excel(input$trt_file$datapath, n_max = 8)
  })

  output$lab_ready <- reactive(!is.null(input$lab_file))
  output$trt_ready <- reactive(!is.null(input$trt_file))
  output$data_loaded <- reactive(!is.null(store$lab))

  # Detected case IDs (populated before ui_cases is rendered)
  lab_ids <- reactiveVal(character(0))
  trt_ids <- reactiveVal(character(0))
  outputOptions(output, "lab_ready",   suspendWhenHidden = FALSE)
  outputOptions(output, "trt_ready",   suspendWhenHidden = FALSE)
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  # Column selectors — lab
  lc <- reactive(names(lab_raw()))
  output$ui_lab_pid   <- renderUI(col_sel("lab_pid",   "Patient ID",
    lc(), guess_col(lc(), c("patientID","patientid","patient_id","ID"))))
  output$ui_lab_param <- renderUI(col_sel("lab_param", "Parameter",
    lc(), guess_col(lc(), c("parameter","param","test","analyte"))))
  output$ui_lab_val   <- renderUI(col_sel("lab_val",   "Value",
    lc(), guess_col(lc(), c("value","result","val"))))
  output$ui_lab_date  <- renderUI(col_sel("lab_date",  "Date",
    lc(), guess_col(lc(), c("date","Date","datum","datetime"))))
  output$ui_lab_time  <- renderUI(col_sel("lab_time",  "Time (optional)",
    lc(), guess_col(lc(), c("time","Time","tijd"))))

  # Column selectors — treatment
  tc <- reactive(names(trt_raw()))
  output$ui_trt_pid   <- renderUI(col_sel("trt_pid",   "Patient ID",
    tc(), guess_col(tc(), c("PATIENTID","patientID","patientid","patient_id"))))
  output$ui_trt_trt   <- renderUI(col_sel("trt_trt",   "Treatment",
    tc(), guess_col(tc(), c("TREATMENT","treatment","drug","Drug"))))
  output$ui_trt_start <- renderUI(col_sel("trt_start", "Start",
    tc(), guess_col(tc(), c("START","start","Start","start_date"))))
  output$ui_trt_end   <- renderUI(col_sel("trt_end",   "End",
    tc(), guess_col(tc(), c("END","end","End","end_date"))))
  output$ui_trt_color <- renderUI(col_sel("trt_color", "Color (opt.)",
    tc(), guess_col(tc(), c("COLOR","color","Color","colour"))))
  output$ui_trt_class <- renderUI(col_sel("trt_class", "Class (opt.)",
    tc(), guess_col(tc(), c("CLASS","class","Class","drug_class"))))

  # Case input rows — case IDs pre-filled from the lab file
  output$ui_cases <- renderUI({
    ids <- lab_ids()
    n   <- max(1L, as.integer(input$n_cases %||% 1L))
    lapply(seq_len(n), function(i) {
      fluidRow(
        column(6, textInput(paste0("cid_", i), if (i==1) "Case ID" else NULL,
                            value       = if (i <= length(ids)) ids[i] else "",
                            placeholder = paste0("Patient ", i))),
        column(6, textInput(paste0("cref_", i), if (i==1) "Ref date (YYYY-MM-DD)" else NULL,
                            placeholder = "YYYY-MM-DD"))
      )
    })
  })

  # Read all unique patient IDs from lab file whenever file or pid column changes
  observeEvent(list(input$lab_file, input$lab_pid), {
    req(input$lab_file, input$lab_pid, nzchar(input$lab_pid %||% ""))
    raw <- tryCatch(readxl::read_excel(input$lab_file$datapath), error = function(e) NULL)
    if (is.null(raw) || !input$lab_pid %in% names(raw)) return()
    ids <- sort(unique(as.character(raw[[input$lab_pid]])))
    ids <- ids[!is.na(ids) & nzchar(ids)]
    lab_ids(ids)
    updateNumericInput(session, "n_cases", value = length(ids))
  }, ignoreNULL = TRUE)

  # Read all unique patient IDs from treatment file and check against lab IDs
  observeEvent(list(input$trt_file, input$trt_pid), {
    req(input$trt_file, input$trt_pid, nzchar(input$trt_pid %||% ""))
    raw <- tryCatch(readxl::read_excel(input$trt_file$datapath), error = function(e) NULL)
    if (is.null(raw) || !input$trt_pid %in% names(raw)) return()
    ids <- sort(unique(as.character(raw[[input$trt_pid]])))
    ids <- ids[!is.na(ids) & nzchar(ids)]
    trt_ids(ids)
    lab <- lab_ids()
    if (length(lab) > 0) {
      only_trt <- setdiff(ids, lab)
      only_lab <- setdiff(lab, ids)
      if (length(only_trt) > 0 || length(only_lab) > 0) {
        parts <- c(
          if (length(only_trt)) paste("in treatment but not lab:", paste(only_trt, collapse = ", ")),
          if (length(only_lab)) paste("in lab but not treatment:", paste(only_lab, collapse = ", "))
        )
        showNotification(
          paste("Case ID mismatch —", paste(parts, collapse = "; ")),
          type = "error", duration = 15
        )
      }
    }
  }, ignoreNULL = TRUE)

  # Data store
  store <- reactiveValues(lab = NULL, trt = NULL, params = NULL)

  observeEvent(input$btn_load, {
    store$lab <- store$trt <- store$params <- NULL
    tryCatch({
      req(input$lab_file, input$lab_pid, input$lab_param, input$lab_val, input$lab_date)
      n <- max(1L, as.integer(input$n_cases %||% 1L))
      cases <- lapply(seq_len(n), function(i) {
        id  <- trimws(input[[paste0("cid_",  i)]] %||% "")
        ref <- trimws(input[[paste0("cref_", i)]] %||% "")
        if (!nzchar(id))  stop(sprintf("Case %d: ID is empty.", i))
        if (!nzchar(ref)) stop(sprintf("Case %d: reference date is empty.", i))
        if (is.na(as.Date(ref, "%Y-%m-%d")))
          stop(sprintf("Case %d: '%s' is not YYYY-MM-DD.", i, ref))
        list(id = id, ref_date = ref)
      })

      col_t <- if (nzchar(input$lab_time %||% "")) input$lab_time else NULL
      lab <- load_lab_data(input$lab_file$datapath, cases,
                           col_patientid = input$lab_pid,
                           col_parameter = input$lab_param,
                           col_value     = input$lab_val,
                           col_date      = input$lab_date,
                           col_time      = col_t)
      store$lab    <- lab
      store$params <- sort(unique(lab$parameter))

      if (!is.null(input$trt_file) &&
          nzchar(input$trt_pid %||% "") && nzchar(input$trt_trt   %||% "") &&
          nzchar(input$trt_start %||% "") && nzchar(input$trt_end %||% "")) {
        store$trt <- load_trt_flexible(input$trt_file$datapath, 1, cases,
                                       input$trt_pid, input$trt_trt,
                                       input$trt_start, input$trt_end,
                                       input$trt_color %||% "",
                                       input$trt_class %||% "")
      }

      # Default: all params in panel 1, log10
      updateNumericInput(session, "n_panels", value = 1)

    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
    })
  })

  output$load_msg <- renderText({
    req(store$lab)
    paste0(nrow(store$lab), " lab rows | ", length(store$params), " parameter(s)\n",
           if (!is.null(store$trt)) paste0(nrow(store$trt), " treatment rows") else "No treatment data")
  })

  # Panel assignment UI
  output$ui_panels <- renderUI({
    params <- store$params %||% character(0)
    n <- max(1L, as.integer(input$n_panels %||% 1L))
    lapply(seq_len(n), function(i) {
      wellPanel(style = "padding:8px",
        tags$b(paste("Panel", i)),
        fluidRow(
          column(5,
            selectInput(paste0("p_line_", i), "Line graph",
                        choices = params, multiple = TRUE,
                        selected = if (i == 1) params else NULL,
                        width = "100%")
          ),
          column(5,
            selectInput(paste0("p_point_", i), "Point graph",
                        choices = params, multiple = TRUE,
                        selected = NULL, width = "100%")
          ),
          column(2,
            selectInput(paste0("p_scale_", i), "Scale",
                        choices = c("log10", "linear"), selected = "log10",
                        width = "100%")
          )
        )
      )
    })
  })

  output$tbl_lab <- renderTable({
    req(store$lab)
    head(store$lab, 50)
  }, striped = TRUE, bordered = TRUE, small = TRUE)

  output$tbl_trt <- renderTable({
    req(store$trt)
    store$trt
  }, striped = TRUE, bordered = TRUE, small = TRUE)

  # Figure
  the_fig <- reactive({
    req(store$lab)
    n <- max(1L, as.integer(input$n_panels %||% 1L))

    panels <- lapply(seq_len(n), function(i) {
      lp <- input[[paste0("p_line_",  i)]] %||% character(0)
      pp <- input[[paste0("p_point_", i)]] %||% character(0)
      if (!length(lp) && !length(pp)) return(NULL)
      lab_panel(line_params  = lp,
                point_params = pp,
                y_scale      = input[[paste0("p_scale_", i)]] %||% "log10",
                height_weight = 3)
    })
    panels <- Filter(Negate(is.null), panels)
    if (!length(panels)) panels <- list(lab_panel(line_params = store$params))

    xmin <- suppressWarnings(as.numeric(input$x_min))
    xmax <- suppressWarnings(as.numeric(input$x_max))

    make_clinical_figure(
      lab_data       = store$lab,
      treatment_data = store$trt,
      lab_panels     = panels,
      x_range        = if (!is.na(xmin) && !is.na(xmax)) c(xmin, xmax) else NULL,
      highlight_days = parse_named_days(input$highlight_days),
      base_size      = as.numeric(input$base_size %||% 9)
    )
  })

  output$fig <- renderPlot({
    tryCatch(the_fig(), error = function(e) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, paste("Error:\n", conditionMessage(e)),
                     col = "firebrick", cex = 1.1)
    })
  }, res = 110)

  output$fig_err <- renderText(
    tryCatch({ the_fig(); "" }, error = function(e) paste("Error:", conditionMessage(e)))
  )

  output$btn_dl <- downloadHandler(
    filename = function() paste0("clinannotr_figure.", input$dl_fmt %||% "pdf"),
    content  = function(file) save_clinical_figure(the_fig(), file,
                 width  = as.numeric(input$dl_w %||% 7),
                 height = as.numeric(input$dl_h %||% 8))
  )
}

shinyApp(ui, server)
