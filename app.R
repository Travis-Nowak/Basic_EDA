# README ---------------------------------------------------------------------
# EDA Companion - Single-file Shiny app
# Required packages: shiny, bslib, readxl, dplyr, tidyr, stringr, lubridate,
#   janitor, ggplot2, DT, writexl, scales, purrr, tibble, forcats
# Optional packages: plotly, naniar, moments, rmarkdown
# How to run: setwd() into the project directory and execute shiny::runApp()
# Download outputs: written to a temporary file returned by Shiny download handlers
# -----------------------------------------------------------------------------

library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(janitor)
library(ggplot2)
library(DT)
library(writexl)
library(glue)
library(scales)
library(purrr)
library(tibble)
library(forcats)

# Optional dependencies with graceful degradation
has_plotly <- requireNamespace("plotly", quietly = TRUE)
has_naniar <- requireNamespace("naniar", quietly = TRUE)
has_moments <- requireNamespace("moments", quietly = TRUE)
has_rmarkdown <- requireNamespace("rmarkdown", quietly = TRUE)

# Helper to compute skewness without the moments package
base_skewness <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3) return(NA_real_)
  m <- mean(x)
  s <- sd(x)
  if (s == 0) return(0)
  mean((x - m)^3) / (s^3)
}

`%||%` <- function(x, y) if (!is.null(x)) x else y

# Demo dataset and in-memory Excel for onboarding
sample_data <- tibble(
  id = 1:10,
  group = rep(LETTERS[1:2], each = 5),
  score = c(52, 64, 71, 59, 83, 48, 75, 69, 91, 66),
  measured_at = seq(as.Date("2023-01-01"), by = "month", length.out = 10),
  notes = c("On track", "Slight delay", "On track", "Blocked", "Ahead",
            "On track", "Review", "Slight delay", "Ahead", "On track")
)

temp_demo_path <- tempfile(fileext = ".xlsx")
write_xlsx(list(Sample = sample_data), temp_demo_path)

# Utility to compute type friendly labels
friendly_type <- function(x) {
  if (inherits(x, c("POSIXct", "POSIXt"))) return("datetime")
  if (inherits(x, "Date")) return("date")
  if (is.numeric(x)) return("numeric")
  if (is.logical(x)) return("logical")
  "categorical"
}

# UI helpers ---------------------------------------------------------------

filter_ops <- c("==", "!=", ">", ">=", "<", "<=", "contains", "startsWith", "endsWith", "between")

mutate_helpers <- c(
  "Arithmetic" = "arithmetic",
  "Log / Ln" = "log",
  "Z-score" = "zscore",
  "Min-Max Scale" = "minmax",
  "Parse Dates" = "parsedate",
  "String Case" = "stringcase",
  "Winsorize" = "winsor"
)

string_case_opts <- c("Lower" = "lower", "Upper" = "upper", "Title" = "title", "Trim" = "trim")

summary_funcs <- c("Mean" = "mean", "Median" = "median", "SD" = "sd", "Min" = "min",
                   "Max" = "max", "Count" = "n", "Distinct" = "n_distinct", "IQR" = "IQR")

corr_methods <- c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")

# Reactive app --------------------------------------------------------------

ui <- page_sidebar(
  title = "EDA Companion",
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  sidebar = sidebar(
    h4("Data Import"),
    fileInput("file", "Upload Excel file", accept = c(".xlsx", ".xls")),
    uiOutput("sheet_picker"),
    checkboxInput("use_header", "First row has headers", value = TRUE),
    checkboxInput("trim_ws", "Trim whitespace", value = TRUE),
    checkboxInput("clean_names", "Clean column names", value = TRUE),
    checkboxInput("guess_types", "Guess column types", value = TRUE),
    hr(),
    strong("Ingest diagnostics"),
    tableOutput("ingest_summary"),
    tableOutput("missing_summary")
  ),
  # Main navbar with analytical tabs
  navbarPage(
    title = NULL,
    id = "main_tabs",
    tabPanel(
      "Overview",
      fluidRow(
        column(3, uiOutput("summary_cards")),
        column(9, plotOutput("missing_bar"))
      ),
      hr(),
      h4("Data dictionary"),
      DTOutput("data_dictionary"),
      hr(),
      downloadButton("download_report", "Download HTML summary")
    ),
    tabPanel(
      "Data",
      h4("Preview"),
      DTOutput("data_preview"),
      hr(),
      downloadButton("download_csv", "Download CSV"),
      downloadButton("download_xlsx", "Download XLSX")
    ),
    tabPanel(
      "Transform",
      fluidRow(
        column(
          width = 12,
          h4("Filters"),
          actionButton("add_filter", "Add filter", icon = icon("plus")),
          actionButton("remove_filter", "Remove filter", icon = icon("minus")),
          uiOutput("filter_builder"),
          hr(),
          h4("Select & Rename"),
          uiOutput("select_columns"),
          uiOutput("rename_ui"),
          hr(),
          h4("Mutate helpers"),
          fluidRow(
            column(4, selectInput("mutate_helper", "Helper", choices = mutate_helpers)),
            column(4, uiOutput("mutate_params")),
            column(4, uiOutput("mutate_params_2"))
          ),
          uiOutput("mutate_params_extra"),
          actionButton("add_mutate", "Add transformation", icon = icon("magic")),
          uiOutput("mutate_list"),
          hr(),
          h4("Group & Summarise"),
          uiOutput("group_summarise"),
          hr(),
          h4("Pivot"),
          uiOutput("pivot_controls"),
          hr(),
          checkboxInput("drop_outliers", "Drop Tukey outliers (numeric columns)", value = FALSE)
        )
      )
    ),
    tabPanel(
      "Distributions",
      uiOutput("dist_controls"),
      uiOutput("dist_outputs")
    ),
    tabPanel(
      "Relationships",
      uiOutput("rel_controls"),
      uiOutput("rel_outputs")
    ),
    tabPanel(
      "Correlation",
      uiOutput("corr_controls"),
      uiOutput("corr_outputs")
    ),
    tabPanel(
      "Quality",
      uiOutput("quality_outputs")
    ),
    tabPanel(
      "Code/Export",
      h4("Current pipeline"),
      verbatimTextOutput("pipeline_code"),
      downloadButton("download_script", "Download R script"),
      hr(),
      h4("Download transformed data"),
      downloadButton("download_tr_csv", "Download CSV"),
      downloadButton("download_tr_xlsx", "Download XLSX")
    )
  )
)

server <- function(input, output, session) {
  # Track added filters and mutate steps
  filter_count <- reactiveVal(1)
  mutate_steps <- reactiveVal(list())
  # Determine whether we are using demo data
  is_demo <- reactive({ is.null(input$file$datapath) })

  valueBox <- function(title, value) {
    div(class = 'card text-center mb-3', div(class = 'card-body', h5(class = 'card-title', title), h3(class = 'card-text', value)))
  }

  # Update sheet picker when a file is uploaded
  observe({
    if (is_demo()) {
      updateSelectInput(session, "sheet", choices = "Sample", selected = "Sample")
    }
  })

  observeEvent(input$file, {
    req(input$file$datapath)
    sheets <- tryCatch(excel_sheets(input$file$datapath), error = function(e) NULL)
    if (is.null(sheets) || length(sheets) == 0) {
      showNotification("Unable to read sheets from the uploaded file.", type = "error")
    } else {
      updateSelectInput(session, "sheet", choices = sheets, selected = sheets[1])
    }
  }, ignoreNULL = FALSE)

  output$sheet_picker <- renderUI({
    if (is_demo()) {
      selectInput("sheet", "Sheet", choices = "Sample", selected = "Sample")
    } else {
      req(input$file)
      sheets <- excel_sheets(input$file$datapath)
      selectInput("sheet", "Sheet", choices = sheets, selected = sheets[1])
    }
  })

  # Build the raw data reactive -------------------------------------------
  data_raw <- reactive({
    if (is_demo()) {
      df <- sample_data
    } else {
      req(input$file, input$sheet)
      col_names <- if (isTRUE(input$use_header)) TRUE else FALSE
      col_types <- if (isTRUE(input$guess_types)) NULL else "text"
      df <- tryCatch(
        read_excel(
          path = input$file$datapath,
          sheet = input$sheet,
          col_names = col_names,
          trim_ws = isTRUE(input$trim_ws),
          col_types = col_types
        ),
        error = function(e) {
          showNotification(paste("Failed to read file:", e$message), type = "error")
          return(tibble())
        }
      )
      if (!isTRUE(input$use_header)) {
        names(df) <- paste0("col_", seq_along(df))
      }
    }
    if (isTRUE(input$clean_names)) {
      df <- janitor::clean_names(df)
    }
    df
  })

  # Diagnostics ------------------------------------------------------------
  ingest_summary_tbl <- reactive({
    df <- data_raw()
    tibble(
      metric = c("Rows", "Columns"),
      value = c(nrow(df), ncol(df))
    )
  })

  output$ingest_summary <- renderTable({ ingest_summary_tbl() })

  missing_summary_tbl <- reactive({
    df <- data_raw()
    tibble(
      column = names(df),
      type = map_chr(df, friendly_type),
      pct_missing = map_dbl(df, ~ mean(is.na(.)) * 100)
    )
  })

  output$missing_summary <- renderTable({ missing_summary_tbl() })

  # Filter builder UI ------------------------------------------------------
  observeEvent(input$add_filter, {
    filter_count(filter_count() + 1)
  })

  observeEvent(input$remove_filter, {
    filter_count(max(1, filter_count() - 1))
  })

  output$filter_builder <- renderUI({
    req(data_raw())
    cols <- names(data_raw())
    n <- filter_count()
    tagList(
      lapply(seq_len(n), function(i) {
        fluidRow(
          column(4, selectInput(paste0("filter_col_", i), label = if (i == 1) "Column" else NULL, choices = cols)),
          column(4, selectInput(paste0("filter_op_", i), label = if (i == 1) "Operator" else NULL, choices = filter_ops)),
          column(4, textInput(paste0("filter_val_", i), label = if (i == 1) "Value" else NULL))
        )
      })
    )
  })

  # Select & rename --------------------------------------------------------
  rename_mapping <- reactive({
    cols <- input$selected_cols
    if (is.null(cols) || length(cols) == 0) {
      cols <- names(data_raw())
    }
    rename_vals <- map_chr(cols, function(col) {
      val <- input[[paste0('rename_', col)]]
      if (is.null(val) || val == '') col else val
    })
    tibble(original = cols, renamed = rename_vals)
  })

  rename_lookup <- reactive({
    mapping <- rename_mapping()
    setNames(mapping$renamed, mapping$original)
  })

  output$select_columns <- renderUI({
    cols <- names(data_raw())
    req(length(cols) > 0)
    checkboxGroupInput("selected_cols", "Columns to keep", choices = cols, selected = cols)
  })

  output$rename_ui <- renderUI({
    req(input$selected_cols)
    tagList(lapply(input$selected_cols, function(col) {
      textInput(paste0("rename_", col), label = paste0("Rename ", col, " to"), value = col)
    }))
  })

  # Mutate helpers ---------------------------------------------------------
  mutate_param_inputs <- reactive({
    df <- data_raw()
    cols <- names(df)
    numeric_cols <- cols[vapply(df, is.numeric, logical(1))]
    list(cols = cols, numeric = numeric_cols)
  })

  output$mutate_params <- renderUI({
    params <- mutate_param_inputs()
    helper <- req(input$mutate_helper)
    switch(helper,
      arithmetic = tagList(
        textInput("mutate_new", "New column name"),
        selectInput("mutate_col_a", "Column A", choices = params$numeric),
        selectInput("mutate_arith_op", "Operator", choices = c("+", "-", "*", "/")),
        selectInput("mutate_col_b", "Column B", choices = params$numeric)
      ),
      log = tagList(
        textInput("mutate_new", "New column name"),
        selectInput("mutate_col_a", "Column", choices = params$numeric),
        radioButtons("mutate_log_base", "Base", choices = c("Natural" = "e", "Base 10" = "10"), inline = TRUE)
      ),
      zscore = tagList(
        textInput("mutate_new", "New column name"),
        selectInput("mutate_col_a", "Column", choices = params$numeric)
      ),
      minmax = tagList(
        textInput("mutate_new", "New column name"),
        selectInput("mutate_col_a", "Column", choices = params$numeric)
      ),
      parsedate = tagList(
        textInput("mutate_new", "New column name"),
        selectInput("mutate_col_a", "Column", choices = params$cols),
        textInput("mutate_format", "Date format (lubridate::parse_date_time)", value = "ymd")
      ),
      stringcase = tagList(
        textInput("mutate_new", "New column name"),
        selectInput("mutate_col_a", "Column", choices = params$cols),
        selectInput("mutate_case", "Transform", choices = string_case_opts)
      ),
      winsor = tagList(
        textInput("mutate_new", "New column name"),
        selectInput("mutate_col_a", "Column", choices = params$numeric),
        sliderInput("mutate_winsor", "Percentile", min = 0, max = 20, value = 5)
      )
    )
  })

  output$mutate_params_2 <- renderUI({ NULL })
  output$mutate_params_extra <- renderUI({ NULL })

  observeEvent(input$add_mutate, {
    helper <- req(input$mutate_helper)
    params <- list(type = helper)
    params$new <- input$mutate_new
    params$col_a <- input$mutate_col_a
    params$col_b <- input$mutate_col_b
    params$op <- input$mutate_arith_op
    params$log_base <- input$mutate_log_base
    params$case <- input$mutate_case
    params$format <- input$mutate_format
    params$winsor <- input$mutate_winsor
    if (is.null(params$new) || params$new == "") {
      showNotification("Please provide a name for the new column.", type = "warning")
      return()
    }
    current <- mutate_steps()
    current[[paste0("step_", length(current) + 1)]] <- params
    mutate_steps(current)
  })

  observeEvent(input$remove_mutate, {
    req(length(mutate_steps()) > 0)
    idx <- as.integer(input$mutate_remove)
    current <- mutate_steps()
    if (!is.na(idx) && idx >= 1 && idx <= length(current)) {
      current[[idx]] <- NULL
      mutate_steps(current)
    }
  })

  output$mutate_list <- renderUI({
    steps <- mutate_steps()
    if (length(steps) == 0) return(p('No transformations added yet.'))
    summaries <- purrr::imap_chr(steps, function(step, idx) {
      glue::glue('{idx}. {step$type} on {step$col_a} -> {step$new}')
    })
    tagList(
      tags$ul(lapply(summaries, tags$li)),
      selectInput('mutate_remove', 'Remove step', choices = seq_along(steps), selected = 1),
      actionButton('remove_mutate', 'Remove selected', class = 'btn btn-sm btn-danger')
    )
  })

  # Group summarise UI -----------------------------------------------------
  output$group_summarise <- renderUI({
    df <- data_raw()
    req(ncol(df) > 0)
    cols <- names(df)
    numeric_cols <- cols[vapply(df, is.numeric, logical(1))]
    tagList(
      selectizeInput("group_vars", "Grouping columns", choices = cols, multiple = TRUE),
      selectizeInput("summary_cols", "Summary columns", choices = numeric_cols, multiple = TRUE),
      checkboxGroupInput("summary_fns", "Summary functions", choices = summary_funcs, selected = c("mean", "median"))
    )
  })

  # Pivot UI ---------------------------------------------------------------
  output$pivot_controls <- renderUI({
    df <- data_raw()
    cols <- names(df)
    tagList(
      radioButtons("pivot_mode", "Mode", choices = c("None" = "none", "Longer" = "longer", "Wider" = "wider"), inline = TRUE),
      conditionalPanel(
        condition = "input.pivot_mode == 'longer'",
        selectizeInput("pivot_longer_ids", "ID columns", choices = cols, multiple = TRUE),
        selectizeInput("pivot_longer_vals", "Measure columns", choices = cols, multiple = TRUE)
      ),
      conditionalPanel(
        condition = "input.pivot_mode == 'wider'",
        selectInput("pivot_wider_names", "Names from", choices = cols),
        selectInput("pivot_wider_values", "Values from", choices = cols)
      )
    )
  })


  # Transformation pipeline ------------------------------------------------
  current_filters <- reactive({
    n <- filter_count()
    filters <- list()
    if (n == 0) return(filters)
    for (i in seq_len(n)) {
      col <- input[[paste0('filter_col_', i)]]
      op <- input[[paste0('filter_op_', i)]]
      val <- input[[paste0('filter_val_', i)]]
      if (is.null(col) || is.null(op) || is.null(val) || identical(val, '')) next
      filters[[length(filters) + 1]] <- list(column = col, operator = op, value = val)
    }
    filters
  })

  apply_filter <- function(df, flt) {
    col <- flt$column
    op <- flt$operator
    val <- flt$value
    if (!col %in% names(df)) return(df)
    vec <- df[[col]]
    tryCatch({
      if (op %in% c('==', '!=', '>', '>=', '<', '<=')) {
        if (is.numeric(vec)) {
          num_val <- suppressWarnings(as.numeric(val))
          if (is.na(num_val)) stop('numeric required')
          keep <- switch(op,
            '==' = vec == num_val,
            '!=' = vec != num_val,
            '>' = vec > num_val,
            '>=' = vec >= num_val,
            '<' = vec < num_val,
            '<=' = vec <= num_val
          )
        } else if (inherits(vec, c('Date', 'POSIXct', 'POSIXt'))) {
          parsed <- suppressWarnings(lubridate::ymd(val))
          if (is.na(parsed)) stop('date required')
          keep <- switch(op,
            '==' = vec == parsed,
            '!=' = vec != parsed,
            '>' = vec > parsed,
            '>=' = vec >= parsed,
            '<' = vec < parsed,
            '<=' = vec <= parsed
          )
        } else {
          keep <- switch(op,
            '==' = vec == val,
            '!=' = vec != val,
            '>' = vec > val,
            '>=' = vec >= val,
            '<' = vec < val,
            '<=' = vec <= val
          )
        }
        df <- df[which(keep | is.na(keep)), , drop = FALSE]
      } else if (op == 'contains') {
        df <- df %>% dplyr::filter(stringr::str_detect(as.character(.data[[col]]), stringr::fixed(val)))
      } else if (op == 'startsWith') {
        df <- df %>% dplyr::filter(startsWith(as.character(.data[[col]]), val))
      } else if (op == 'endsWith') {
        df <- df %>% dplyr::filter(endsWith(as.character(.data[[col]]), val))
      } else if (op == 'between') {
        bounds <- strsplit(val, ',')[[1]]
        if (length(bounds) < 2) stop('two bounds needed')
        lower <- suppressWarnings(as.numeric(trimws(bounds[1])))
        upper <- suppressWarnings(as.numeric(trimws(bounds[2])))
        if (!is.numeric(vec) || any(is.na(c(lower, upper)))) stop('numeric required')
        df <- df %>% dplyr::filter(dplyr::between(.data[[col]], lower, upper))
      }
      df
    }, error = function(e) {
      showNotification(glue::glue('Filter on {col} ignored: {e$message}'), type = 'warning')
      df
    })
  }

  filter_code <- function(flt) {
    col <- flt$column
    op <- flt$operator
    val <- flt$value
    if (op == 'between') {
      bounds <- strsplit(val, ',')[[1]]
      lower <- if (length(bounds) >= 1) trimws(bounds[1]) else ''
      upper <- if (length(bounds) >= 2) trimws(bounds[2]) else ''
      glue::glue("|> dplyr::filter(dplyr::between(.data[['{col}']], as.numeric('{lower}'), as.numeric('{upper}')))")
    } else if (op %in% c('contains', 'startsWith', 'endsWith')) {
      glue::glue("|> dplyr::filter({op}(as.character(.data[['{col}']]), '{val}'))")
    } else {
      glue::glue("|> dplyr::filter(.data[['{col}']] {op} '{val}')")
    }
  }

  apply_mutate_step <- function(df, step, lookup = NULL) {
    col <- step$col_a
    new_col <- step$new
    if (!is.null(lookup) && length(lookup) > 0 && !is.null(col) && col %in% names(lookup)) {
      col <- lookup[[col]]
    }
    if (is.null(col) || is.null(new_col) || !col %in% names(df)) return(df)
    if (step$type == "arithmetic") {
      col_b <- step$col_b
      if (!is.null(lookup) && length(lookup) > 0 && !is.null(col_b) && col_b %in% names(lookup)) {
        col_b <- lookup[[col_b]]
      }
      if (is.null(col_b) || !col_b %in% names(df)) {
        showNotification(paste("Column", step$col_b, "not found for arithmetic step."), type = "error")
        return(df)
      }
      df <- df %>% mutate(!!new_col := {
        a <- .data[[col]]
        b <- .data[[col_b]]
        if (step$op == "+") a + b else if (step$op == "-") a - b else if (step$op == "*") a * b else a / b
      })
    } else if (step$type == "log") {
      base_fun <- if (identical(step$log_base, "10")) log10 else log
      df <- df %>% mutate(!!new_col := base_fun(.data[[col]]))
    } else if (step$type == "zscore") {
      df <- df %>% mutate(!!new_col := (.data[[col]] - mean(.data[[col]], na.rm = TRUE)) / sd(.data[[col]], na.rm = TRUE))
    } else if (step$type == "minmax") {
      rng <- range(df[[col]], na.rm = TRUE)
      df <- df %>% mutate(!!new_col := (.data[[col]] - rng[1]) / (rng[2] - rng[1]))
    } else if (step$type == "parsedate") {
      fmt <- if (is.null(step$format) || step$format == "") "ymd" else step$format
      df <- df %>% mutate(!!new_col := suppressWarnings(lubridate::parse_date_time(.data[[col]], orders = fmt)))
    } else if (step$type == "stringcase") {
      df <- df %>% mutate(!!new_col := {
        val <- as.character(.data[[col]])
        switch(step$case,
          lower = stringr::str_to_lower(val),
          upper = stringr::str_to_upper(val),
          title = stringr::str_to_title(val),
          trim = stringr::str_trim(val),
          val
        )
      })
    } else if (step$type == "winsor") {
      p <- ifelse(is.null(step$winsor), 5, step$winsor)
      lower <- p / 100
      upper <- 1 - lower
      q <- quantile(df[[col]], probs = c(lower, upper), na.rm = TRUE)
      df <- df %>% mutate(!!new_col := pmin(pmax(.data[[col]], q[1]), q[2]))
    }
    df
  }

  mutate_code <- function(step, lookup = NULL) {
    resolve <- function(name) {
      if (!is.null(lookup) && length(lookup) > 0 && name %in% names(lookup)) lookup[[name]] else name
    }
    col <- resolve(step$col_a)
    new_col <- step$new
    if (step$type == "arithmetic") {
      col_b <- resolve(step$col_b)
      glue::glue("dplyr::mutate({new_col} = .data[['{col}']] {step$op} .data[['{col_b}']])")
    } else if (step$type == "log") {
      base_fun <- if (identical(step$log_base, "10")) "log10" else "log"
      glue::glue("dplyr::mutate({new_col} = {base_fun}(.data[['{col}']]))")
    } else if (step$type == "zscore") {
      glue::glue("dplyr::mutate({new_col} = (.data[['{col}']] - mean(.data[['{col}']], na.rm = TRUE)) / sd(.data[['{col}']], na.rm = TRUE))")
    } else if (step$type == "minmax") {
      glue::glue("dplyr::mutate({new_col} = (.data[['{col}']] - min(.data[['{col}']], na.rm = TRUE)) / (max(.data[['{col}']], na.rm = TRUE) - min(.data[['{col}']], na.rm = TRUE)))")
    } else if (step$type == "parsedate") {
      fmt <- if (is.null(step$format) || step$format == "") "ymd" else step$format
      glue::glue("dplyr::mutate({new_col} = lubridate::parse_date_time(.data[['{col}']], orders = '{fmt}'))")
    } else if (step$type == "stringcase") {
      fun <- switch(step$case, lower = "stringr::str_to_lower", upper = "stringr::str_to_upper", title = "stringr::str_to_title", trim = "stringr::str_trim", "as.character")
      glue::glue("dplyr::mutate({new_col} = {fun}(.data[['{col}']]))")
    } else if (step$type == "winsor") {
      p <- ifelse(is.null(step$winsor), 5, step$winsor)
      glue::glue("dplyr::mutate({new_col} = scales::squish(.data[['{col}']], range = quantile(.data[['{col}']], probs = c({p}/100, 1-{p}/100), na.rm = TRUE)))")
    } else {
      NULL
    }
  }

  # Pipeline application ---------------------------------------------------
  data_transformed <- reactive({
    df <- data_raw()
    # Apply filters
    filters <- current_filters()
    if (length(filters) > 0) {
      for (flt in filters) {
        df <- apply_filter(df, flt)
      }
    }
    # Select and rename
    mapping <- rename_mapping()
    if (nrow(mapping) > 0) {
      selected <- mapping$original
      rename_map <- mapping$renamed
      df <- df %>% select(all_of(selected))
      names(df) <- rename_map
    }
    lookup <- rename_lookup()
    # Mutate steps
    steps <- mutate_steps()
    if (length(steps) > 0) {
      for (step in steps) {
        df <- apply_mutate_step(df, step, lookup)
      }
    }
    # Group & summarise
    if (!is.null(input$summary_cols) && length(input$summary_cols) > 0 && !is.null(input$summary_fns) && length(input$summary_fns) > 0) {
      summary_cols <- lookup[input$summary_cols]
      summary_cols <- summary_cols[!is.na(summary_cols) & summary_cols %in% names(df)]
      if (length(summary_cols) > 0) {
        group_vars <- lookup[input$group_vars]
        group_vars <- group_vars[!is.na(group_vars) & group_vars %in% names(df)]
        fun_map <- list(
          mean = ~mean(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          min = ~min(.x, na.rm = TRUE),
          max = ~max(.x, na.rm = TRUE),
          n = ~dplyr::n(),
          n_distinct = ~dplyr::n_distinct(.x),
          IQR = ~IQR(.x, na.rm = TRUE)
        )
        chosen <- fun_map[input$summary_fns]
        chosen <- chosen[!vapply(chosen, is.null, logical(1))]
        if (length(chosen) > 0) {
          if (length(group_vars) > 0) {
            df <- df %>% group_by(across(all_of(group_vars)))
          }
          df <- df %>% summarise(across(all_of(summary_cols), chosen, .names = "{.fn}_{.col}"), .groups = "drop")
        }
      }
    }
    # Pivoting
    if (identical(input$pivot_mode, "longer")) {
      ids <- lookup[input$pivot_longer_ids]
      ids <- ids[!is.na(ids) & ids %in% names(df)]
      vals <- lookup[input$pivot_longer_vals]
      vals <- vals[!is.na(vals) & vals %in% names(df)]
      if (!is.null(vals) && length(vals) > 0) {
        df <- df %>% pivot_longer(cols = all_of(vals), names_to = "name", values_to = "value", values_drop_na = FALSE)
        if (!is.null(ids) && length(ids) > 0) df <- df %>% relocate(all_of(ids))
      }
    } else if (identical(input$pivot_mode, "wider")) {
      nm <- lookup[input$pivot_wider_names]
      val <- lookup[input$pivot_wider_values]
      if (!is.null(nm) && !is.null(val) && nm %in% names(df) && val %in% names(df)) {
        df <- df %>% pivot_wider(names_from = all_of(nm), values_from = all_of(val))
      }
    }
    # Outlier removal
    if (isTRUE(input$drop_outliers)) {
      num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
      for (col in num_cols) {
        vec <- df[[col]]
        stats <- quantile(vec, probs = c(0.25, 0.75), na.rm = TRUE)
        iqr <- stats[2] - stats[1]
        lower <- stats[1] - 1.5 * iqr
        upper <- stats[2] + 1.5 * iqr
        df <- df %>% filter(is.na(.data[[col]]) | (.data[[col]] >= lower & .data[[col]] <= upper))
      }
    }
    df
  })

  # Pipeline code ----------------------------------------------------------
  pipeline_code <- reactive({
    code <- c("data_raw()")
    lookup <- rename_lookup()
    # Filters
    filters <- current_filters()
    if (length(filters) > 0) {
      code <- c(code, map_chr(filters, filter_code))
    }
    # Select & rename
    mapping <- rename_mapping()
    if (nrow(mapping) > 0) {
      rename_calls <- purrr::pmap_chr(mapping, function(original, renamed) {
        if (!identical(original, renamed)) {
          glue::glue("{renamed} = {original}")
        } else {
          original
        }
      })
      code <- c(code, glue::glue("|> dplyr::select({paste(rename_calls, collapse = ', ')})"))
    }
    # Mutates
    steps <- mutate_steps()
    if (length(steps) > 0) {
      lookup <- rename_lookup()
      code <- c(code, purrr::map_chr(steps, ~mutate_code(.x, lookup)))
    }
    # Summaries
    if (!is.null(input$summary_cols) && length(input$summary_cols) > 0 && !is.null(input$summary_fns) && length(input$summary_fns) > 0) {
      group_vars <- lookup[input$group_vars]
      group_vars <- group_vars[!is.na(group_vars)]
      if (length(group_vars) > 0) {
        group_label <- paste(group_vars, collapse = "', '")
        code <- c(code, glue::glue("|> dplyr::group_by(dplyr::across(dplyr::all_of(c('{group_label}'))))"))
      }
      cols_mapped <- lookup[input$summary_cols]
      cols_mapped <- cols_mapped[!is.na(cols_mapped)]
      if (length(cols_mapped) > 0 && length(input$summary_fns) > 0) {
        fn_strings <- c(
          mean = "mean = ~mean(.x, na.rm = TRUE)",
          median = "median = ~median(.x, na.rm = TRUE)",
          sd = "sd = ~sd(.x, na.rm = TRUE)",
          min = "min = ~min(.x, na.rm = TRUE)",
          max = "max = ~max(.x, na.rm = TRUE)",
          n = "n = ~dplyr::n()",
          n_distinct = "n_distinct = ~dplyr::n_distinct(.x)",
          IQR = "IQR = ~IQR(.x, na.rm = TRUE)"
        )
        chosen <- fn_strings[input$summary_fns]
        chosen <- chosen[!is.na(chosen)]
        if (length(chosen) > 0) {
          col_labels <- paste(cols_mapped, collapse = "', '")
          fn_labels <- paste(chosen, collapse = ", ")
          code <- c(code, glue::glue("|> dplyr::summarise(dplyr::across(dplyr::all_of(c('{col_labels}')), list({fn_labels}), .names = '{.fn}_{.col}'), .groups = 'drop')"))
        }
      }
    }
    if (identical(input$pivot_mode, "longer")) {
      ids <- lookup[input$pivot_longer_ids]
      ids <- ids[!is.na(ids)]
      vals <- lookup[input$pivot_longer_vals]
      vals <- vals[!is.na(vals)]
      if (length(vals) > 0) {
        val_label <- paste(vals, collapse = "', '")
        code <- c(code, glue::glue("|> tidyr::pivot_longer(cols = dplyr::all_of(c('{val_label}')), names_to = 'name', values_to = 'value')"))
      }
    } else if (identical(input$pivot_mode, "wider")) {
      nm <- lookup[input$pivot_wider_names]
      val <- lookup[input$pivot_wider_values]
      if (!is.na(nm) && !is.na(val)) {
        code <- c(code, glue::glue("|> tidyr::pivot_wider(names_from = '{nm}', values_from = '{val}')"))
      }
    }
    if (isTRUE(input$drop_outliers)) {
      code <- c(code, "|> dplyr::filter(!if_any(where(is.numeric), ~ (. < quantile(., 0.25, na.rm = TRUE) - 1.5 * IQR(., na.rm = TRUE)) | (. > quantile(., 0.75, na.rm = TRUE) + 1.5 * IQR(., na.rm = TRUE))))")
    }
    paste(c("data_tr <-", code), collapse = "\n")
  })

  output$pipeline_code <- renderText({ pipeline_code() })

  # Data preview -----------------------------------------------------------
  output$data_preview <- renderDT({
    datatable(data_transformed(), filter = "top", options = list(scrollX = TRUE, pageLength = 10))
  })

  # Downloads --------------------------------------------------------------
  output$download_csv <- downloadHandler(
    filename = function() paste0("raw_data_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(data_raw(), file, row.names = FALSE)
    }
  )

  output$download_xlsx <- downloadHandler(
    filename = function() paste0("raw_data_", Sys.Date(), ".xlsx"),
    content = function(file) {
      write_xlsx(data_raw(), file)
    }
  )

  output$download_tr_csv <- downloadHandler(
    filename = function() paste0("transformed_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(data_transformed(), file, row.names = FALSE)
    }
  )

  output$download_tr_xlsx <- downloadHandler(
    filename = function() paste0("transformed_", Sys.Date(), ".xlsx"),
    content = function(file) {
      write_xlsx(data_transformed(), file)
    }
  )

  output$download_script <- downloadHandler(
    filename = function() "pipeline.R",
    content = function(file) {
      code <- paste0("library(dplyr)\n", "library(tidyr)\n", "library(lubridate)\n\n", pipeline_code(), "\n")
      writeLines(code, file)
    }
  )

  # Overview tab ----------------------------------------------------------
  output$summary_cards <- renderUI({
    df <- data_transformed()
    total_missing <- mean(is.na(df)) * 100
    numeric_cols <- sum(vapply(df, is.numeric, logical(1)))
    cat_cols <- sum(vapply(df, function(x) is.character(x) || is.factor(x), logical(1)))
    tagList(
      valueBox("Rows", format(nrow(df), big.mark = ",")),
      valueBox("Columns", format(ncol(df), big.mark = ",")),
      valueBox("% Missing", sprintf("%.1f%%", total_missing)),
      valueBox("# Numeric", numeric_cols),
      valueBox("# Categorical", cat_cols)
    )
  })

  output$missing_bar <- renderPlot({
    df <- data_raw()
    req(ncol(df) > 0)
    miss <- missing_summary_tbl()
    ggplot(miss, aes(x = forcats::fct_reorder(column, pct_missing), y = pct_missing, fill = pct_missing)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "% missing", title = "Missingness by column") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
      theme_minimal()
  })

  output$data_dictionary <- renderDT({
    df <- data_raw()
    preview_vals <- map_chr(df, function(x) {
      vals <- unique(x[!is.na(x)])
      paste(head(vals, 3), collapse = ", ")
    })
    dict <- tibble(
      column = names(df),
      type = map_chr(df, friendly_type),
      pct_missing = sprintf("%.1f%%", map_dbl(df, ~ mean(is.na(.)) * 100)),
      examples = preview_vals
    )
    datatable(dict, options = list(pageLength = 10))
  })

  output$download_report <- downloadHandler(
    filename = function() paste0("eda_report_", Sys.Date(), ".html"),
    content = function(file) {
      df <- data_transformed()
      if (has_rmarkdown) {
        tmp <- tempfile(fileext = '.Rmd')
        writeLines("---\ntitle: 'EDA Companion Summary'\noutput: rmarkdown::html_document\n---\n\n````{r}\nlibrary(dplyr)\nlibrary(ggplot2)\nsummary(df)\n````\n", tmp)
        env <- new.env(parent = globalenv())
        env$df <- df
        rmarkdown::render(tmp, output_file = file, envir = env, quiet = TRUE)
      } else {
        html <- htmltools::tagList(
          htmltools::h2('EDA Companion Summary'),
          htmltools::p('Install rmarkdown for richer reports.'),
          htmltools::pre(capture.output(str(df)))
        )
        htmltools::save_html(html, file)
      }
    }
  )

  # Distributions tab ------------------------------------------------------
  output$dist_controls <- renderUI({
    df <- data_transformed()
    req(ncol(df) > 0)
    selectInput("dist_var", "Variable", choices = names(df))
  })

  output$dist_outputs <- renderUI({
    req(input$dist_var)
    if (is_demo()) {
      wellPanel("Upload your own data to view full distribution diagnostics.")
    } else {
      df <- data_transformed()
      var <- input$dist_var
      if (is.numeric(df[[var]])) {
        tagList(
          plotOutput("hist_plot"),
          plotOutput("box_plot"),
          tableOutput("numeric_stats")
        )
      } else {
        tagList(
          sliderInput("top_k", "Top categories", min = 3, max = 20, value = 10, step = 1),
          plotOutput("bar_plot"),
          tableOutput("cat_stats")
        )
      }
    }
  })

  output$hist_plot <- renderPlot({
    req(input$dist_var)
    df <- data_transformed()
    var <- input$dist_var
    req(is.numeric(df[[var]]))
    ggplot(df, aes(x = .data[[var]])) +
      geom_histogram(aes(y = ..density..), fill = "#4E79A7", alpha = 0.7, bins = 30) +
      geom_density(color = "#F28E2B", linewidth = 1) +
      labs(title = paste("Distribution of", var), x = var, y = "Density") +
      theme_minimal()
  })

  output$box_plot <- renderPlot({
    req(input$dist_var)
    df <- data_transformed()
    var <- input$dist_var
    req(is.numeric(df[[var]]))
    ggplot(df, aes(y = .data[[var]])) +
      geom_boxplot(fill = "#59A14F") +
      labs(title = paste("Boxplot of", var), y = var) +
      theme_minimal()
  })

  output$numeric_stats <- renderTable({
    req(input$dist_var)
    df <- data_transformed()
    var <- input$dist_var
    req(is.numeric(df[[var]]))
    x <- df[[var]]
    skew <- if (has_moments) moments::skewness(x, na.rm = TRUE) else base_skewness(x)
    tibble(
      metric = c("n", "Missing %", "Mean", "Median", "SD", "IQR", "Min", "Max", "Skewness", "Q1", "Q3"),
      value = c(sum(!is.na(x)), sprintf("%.1f%%", mean(is.na(x)) * 100),
                mean(x, na.rm = TRUE), median(x, na.rm = TRUE), sd(x, na.rm = TRUE), IQR(x, na.rm = TRUE),
                min(x, na.rm = TRUE), max(x, na.rm = TRUE), skew, quantile(x, 0.25, na.rm = TRUE), quantile(x, 0.75, na.rm = TRUE))
    )
  })

  output$bar_plot <- renderPlot({
    req(input$dist_var)
    df <- data_transformed()
    var <- input$dist_var
    req(!is.numeric(df[[var]]))
    top_k <- input$top_k %||% 10
    counts <- df %>% count(.data[[var]], name = "n") %>% mutate(prop = n / sum(n)) %>% arrange(desc(n)) %>% slice_head(n = top_k)
    ggplot(counts, aes(x = reorder(as.character(.data[[var]]), n), y = n, fill = n)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = paste("Top", top_k, var), x = var, y = "Count") +
      theme_minimal()
  })

  output$cat_stats <- renderTable({
    req(input$dist_var)
    df <- data_transformed()
    var <- input$dist_var
    req(!is.numeric(df[[var]]))
    counts <- df %>% count(.data[[var]], name = "count") %>% mutate(prop = count / sum(count)) %>% arrange(desc(count))
    head(counts, input$top_k %||% 10)
  })

  # Relationships tab ------------------------------------------------------
  output$rel_controls <- renderUI({
    df <- data_transformed()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    factor_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
    tagList(
      fluidRow(
        column(4, selectInput("rel_x", "X", choices = numeric_cols)),
        column(4, selectInput("rel_y", "Y", choices = numeric_cols, selected = numeric_cols[min(2, length(numeric_cols))])),
        column(4, selectInput("rel_color", "Colour", choices = c("None", factor_cols)))
      ),
      selectInput("rel_corr_method", "Correlation", choices = c("Pearson", "Spearman"), selected = "Pearson"),
      hr(),
      fluidRow(
        column(6, selectInput("rel_group_num", "Numeric", choices = numeric_cols)),
        column(6, selectInput("rel_group_cat", "Group", choices = factor_cols))
      )
    )
  })

  output$rel_outputs <- renderUI({
    if (is_demo()) {
      wellPanel("Upload your own data to explore relationships.")
    } else {
      tagList(
        plotOutput("scatter_plot"),
        verbatimTextOutput("scatter_stats"),
        plotOutput("group_summary_plot")
      )
    }
  })

  output$scatter_plot <- renderPlot({
    req(input$rel_x, input$rel_y)
    df <- data_transformed()
    req(is.numeric(df[[input$rel_x]]), is.numeric(df[[input$rel_y]]))
    plt <- ggplot(df, aes(x = .data[[input$rel_x]], y = .data[[input$rel_y]])) +
      geom_point(alpha = 0.7)
    if (!is.null(input$rel_color) && input$rel_color != "None" && input$rel_color %in% names(df)) {
      plt <- plt + aes(color = .data[[input$rel_color]])
    }
    plt + geom_smooth(method = "lm", se = FALSE, color = "#E15759") +
      theme_minimal() +
      labs(title = paste("Scatter:", input$rel_x, "vs", input$rel_y))
  })

  output$scatter_stats <- renderText({
    req(input$rel_x, input$rel_y)
    df <- data_transformed()
    method <- tolower(input$rel_corr_method %||% "pearson")
    if (nrow(df) < 3) return("Insufficient data")
    ctest <- tryCatch(cor.test(df[[input$rel_x]], df[[input$rel_y]], method = method, use = "pairwise.complete.obs"), error = function(e) NULL)
    if (is.null(ctest)) return("Unable to compute correlation.")
    sprintf("%s correlation: %.3f (p = %.3f)", str_to_title(method), ctest$estimate, ctest$p.value)
  })

  output$group_summary_plot <- renderPlot({
    req(input$rel_group_num, input$rel_group_cat)
    df <- data_transformed()
    req(is.numeric(df[[input$rel_group_num]]))
    req(input$rel_group_cat %in% names(df))
    summary_df <- df %>% group_by(.data[[input$rel_group_cat]]) %>% summarise(
      mean = mean(.data[[input$rel_group_num]], na.rm = TRUE),
      sd = sd(.data[[input$rel_group_num]], na.rm = TRUE),
      n = dplyr::n(),
      se = sd / sqrt(pmax(n, 1)),
      ci_low = mean - qt(0.975, pmax(n - 1, 1)) * se,
      ci_high = mean + qt(0.975, pmax(n - 1, 1)) * se,
      .groups = "drop"
    )
    ggplot(summary_df, aes(x = .data[[input$rel_group_cat]], y = mean)) +
      geom_col(fill = "#4E79A7", alpha = 0.8) +
      geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
      labs(title = paste("Grouped means of", input$rel_group_num), x = input$rel_group_cat, y = "Mean ± 95% CI") +
      theme_minimal()
  })

  # Correlation tab -------------------------------------------------------
  output$corr_controls <- renderUI({
    df <- data_transformed()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    tagList(
      selectInput("corr_method", "Method", choices = corr_methods),
      selectizeInput("corr_cols", "Columns", choices = numeric_cols, multiple = TRUE, selected = numeric_cols)
    )
  })

  corr_matrix <- reactive({
    cols <- input$corr_cols
    df <- data_transformed()
    req(!is.null(cols), length(cols) >= 2)
    mat <- df %>% select(all_of(cols)) %>% mutate(across(everything(), as.numeric))
    cor(mat, use = "pairwise.complete.obs", method = input$corr_method %||% "pearson")
  })

  output$corr_outputs <- renderUI({
    if (is_demo()) {
      wellPanel("Upload a dataset to calculate correlations.")
    } else {
      tagList(
        plotOutput("corr_heatmap"),
        downloadButton("download_corr_png", "Download heatmap PNG"),
        downloadButton("download_corr_csv", "Download matrix CSV")
      )
    }
  })

  output$corr_heatmap <- renderPlot({
    mat <- corr_matrix()
    df <- as.data.frame(as.table(mat))
    ggplot(df, aes(Var1, Var2, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = sprintf("%.2f", Freq)), color = "white") +
      scale_fill_gradient2(low = "#B2182B", mid = "#F7F7F7", high = "#2166AC", midpoint = 0) +
      labs(x = NULL, y = NULL, title = paste(str_to_title(input$corr_method), "correlation")) +
      theme_minimal()
  })

  output$download_corr_png <- downloadHandler(
    filename = function() paste0("correlation_heatmap_", Sys.Date(), ".png"),
    content = function(file) {
      mat <- corr_matrix()
      df <- as.data.frame(as.table(mat))
      plt <- ggplot(df, aes(Var1, Var2, fill = Freq)) +
        geom_tile() +
        geom_text(aes(label = sprintf("%.2f", Freq)), color = "white") +
        scale_fill_gradient2(low = "#B2182B", mid = "#F7F7F7", high = "#2166AC", midpoint = 0) +
        theme_minimal()
      ggsave(file, plt, width = 6, height = 5)
    }
  )

  output$download_corr_csv <- downloadHandler(
    filename = function() paste0("correlation_matrix_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(corr_matrix(), file, row.names = TRUE)
    }
  )

  # Quality tab -----------------------------------------------------------
  output$quality_outputs <- renderUI({
    df <- data_transformed()
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    miss_tbl <- missing_summary_tbl()
    outlier_tbl <- tibble(column = num_cols) %>% mutate(
      q1 = map_dbl(column, ~ quantile(df[[.x]], 0.25, na.rm = TRUE)),
      q3 = map_dbl(column, ~ quantile(df[[.x]], 0.75, na.rm = TRUE)),
      iqr = q3 - q1,
      lower = q1 - 1.5 * iqr,
      upper = q3 + 1.5 * iqr,
      outliers = map_int(column, ~ sum(df[[.x]] < lower[column == .x] | df[[.x]] > upper[column == .x], na.rm = TRUE))
    )
    output$quality_outliers <- renderTable(outlier_tbl)
    tagList(
      h4('Missingness'),
      tableOutput('quality_missing'),
      if (is_demo()) {
        wellPanel('Upload your data to inspect detailed missingness patterns.')
      } else {
        plotOutput('missing_map')
      },
      h4('Outliers'),
      tableOutput('quality_outliers'),
      p('Toggle "Drop Tukey outliers" in the Transform tab to remove them from downstream analysis.')
    )
  })

  output$quality_missing <- renderTable({
    missing_summary_tbl()
  })

  output$missing_map <- renderPlot({
    req(!is_demo())
    df <- data_transformed() %>% head(100)
    if (has_naniar) {
      naniar::vis_miss(df)
    } else {
      miss_df <- df %>% mutate(row = row_number()) %>% pivot_longer(-row, names_to = 'column', values_to = 'value') %>%
        mutate(is_missing = is.na(value))
      ggplot(miss_df, aes(x = column, y = row, fill = is_missing)) +
        geom_tile() +
        scale_fill_manual(values = c('TRUE' = '#E15759', 'FALSE' = '#59A14F')) +
        labs(x = NULL, y = 'Row', title = 'Missingness map (first 100 rows)') +
        theme_minimal()
    }
  })
}

shinyApp(ui, server)

