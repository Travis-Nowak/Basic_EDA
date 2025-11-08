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
  "Winsorize" = "winsor",
  "Date Parts" = "dateparts",
  "String Split / Extract" = "stringextract",
  "Factor Lumping" = "factorlump",
  "Simple Impute" = "impute"
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
    checkboxInput("use_sample", "Profile on sample (max 10000 rows)", value = FALSE),
    hr(),
    strong("Ingest diagnostics"),
    tableOutput("ingest_summary"),
    tableOutput("missing_summary"),
    hr(),
    h4("Additional Data"),
    fileInput("secondary_file", "Upload secondary file", accept = c(".xlsx", ".xls", ".csv")),
    uiOutput("secondary_sheet_picker"),
    checkboxInput("secondary_use_header", "First row has headers", value = TRUE),
    checkboxInput("secondary_trim_ws", "Trim whitespace", value = TRUE),
    checkboxInput("secondary_clean_names", "Clean column names", value = TRUE),
    checkboxInput("secondary_guess_types", "Guess column types", value = TRUE)
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
      downloadButton("download_dictionary", "Download data dictionary"),
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
          h4("Column types"),
          uiOutput("type_editor"),
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
    # --- Joins tab ------------------------------------------------------
    tabPanel(
      "Joins",
      fluidRow(
        column(
          width = 4,
          selectInput("join_left_table", "Left table", choices = NULL),
          selectInput("join_right_table", "Right table", choices = NULL),
          uiOutput("join_key_inputs"),
          selectInput("join_type", "Join type", choices = c(
            "Left" = "left",
            "Right" = "right",
            "Inner" = "inner",
            "Full" = "full",
            "Semi" = "semi",
            "Anti" = "anti"
          )),
          helpText("Joined data will be available to other tabs when keys are selected.")
        ),
        column(
          width = 8,
          DTOutput("join_preview")
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
    # --- Target EDA tab -------------------------------------------------
    tabPanel(
      "Target EDA",
      fluidRow(
        column(4, uiOutput("target_controls")),
        column(8, uiOutput("target_outputs"))
      )
    ),
    # --- Time Series tab ------------------------------------------------
    tabPanel(
      "Time Series",
      fluidRow(
        column(4, uiOutput("time_series_controls")),
        column(8, uiOutput("time_series_outputs"))
      )
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
      downloadButton("download_tr_xlsx", "Download XLSX"),
      hr(),
      h4("Pipeline state"),
      textAreaInput("pipeline_state_json", "Pipeline JSON", value = "", rows = 10),
      fileInput("pipeline_state_upload", "Import pipeline JSON", accept = c(".json"))
    )
  )
)

server <- function(input, output, session) {
  # Track added filters and mutate steps
  filter_count <- reactiveVal(1)
  mutate_steps <- reactiveVal(list())
  type_overrides <- reactiveVal(list())
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
  
  output$secondary_sheet_picker <- renderUI({
    file <- input$secondary_file
    if (is.null(file)) return(NULL)
    ext <- tolower(tools::file_ext(file$name))
    if (!ext %in% c("xlsx", "xls")) return(NULL)
    sheets <- tryCatch(excel_sheets(file$datapath), error = function(e) {
      showNotification("Unable to read sheets from secondary file.", type = "error")
      NULL
    })
    if (is.null(sheets) || length(sheets) == 0) return(NULL)
    selectInput("secondary_sheet", "Sheet", choices = sheets, selected = sheets[1])
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
  
  data_secondary_raw <- reactive({
    file <- input$secondary_file
    if (is.null(file)) return(NULL)
    ext <- tolower(tools::file_ext(file$name))
    df <- tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        sheet <- req(input$secondary_sheet)
        col_names <- if (isTRUE(input$secondary_use_header)) TRUE else FALSE
        col_types <- if (isTRUE(input$secondary_guess_types)) NULL else "text"
        read_excel(
          path = file$datapath,
          sheet = sheet,
          col_names = col_names,
          trim_ws = isTRUE(input$secondary_trim_ws),
          col_types = col_types
        )
      } else if (ext == "csv") {
        read.csv(
          file$datapath,
          header = isTRUE(input$secondary_use_header),
          stringsAsFactors = FALSE,
          check.names = FALSE,
          strip.white = isTRUE(input$secondary_trim_ws)
        )
      } else {
        showNotification("Unsupported secondary file type.", type = "error")
        return(NULL)
      }
    }, error = function(e) {
      showNotification(paste("Failed to read secondary file:", e$message), type = "error")
      NULL
    })
    if (is.null(df)) return(NULL)
    if (!isTRUE(input$secondary_use_header)) {
      names(df) <- paste0("col_", seq_along(df))
    }
    df <- tibble::as_tibble(df)
    if (isTRUE(input$secondary_trim_ws)) {
      df <- df %>% mutate(across(where(is.character), ~ stringr::str_trim(.x)))
    }
    if (isTRUE(input$secondary_clean_names)) {
      df <- janitor::clean_names(df)
    }
    df
  })
  
  data_secondary_transformed <- reactive({
    df <- data_secondary_raw()
    if (is.null(df)) return(NULL)
    df
  })
  
  table_choices <- reactive({
    choices <- c("Primary (transformed)" = "primary_transformed", "Primary (raw)" = "primary_raw")
    if (!is.null(data_secondary_raw())) {
      choices <- c(choices, "Secondary (raw)" = "secondary_raw")
    }
    if (!is.null(data_secondary_transformed())) {
      choices <- c(choices, "Secondary (transformed)" = "secondary_transformed")
    }
    choices
  })
  
  join_table_data <- function(code) {
    switch(
      code,
      primary_raw = data_raw(),
      primary_transformed = data_transformed(),
      secondary_raw = data_secondary_raw(),
      secondary_transformed = data_secondary_transformed(),
      NULL
    )
  }
  
  observe({
    choices <- table_choices()
    if (is.null(choices) || length(choices) == 0) return()
    current_left <- input$join_left_table
    left_selected <- if (!is.null(current_left) && current_left %in% unname(choices)) current_left else "primary_transformed"
    default_right <- if ("secondary_raw" %in% unname(choices)) "secondary_raw" else "primary_raw"
    current_right <- input$join_right_table
    right_selected <- if (!is.null(current_right) && current_right %in% unname(choices)) current_right else default_right
    updateSelectInput(session, "join_left_table", choices = choices, selected = left_selected)
    updateSelectInput(session, "join_right_table", choices = choices, selected = right_selected)
  })
  
  output$join_key_inputs <- renderUI({
    left_df <- join_table_data(input$join_left_table)
    right_df <- join_table_data(input$join_right_table)
    if (is.null(left_df) || is.null(right_df)) {
      return(helpText("Load both datasets to configure join keys."))
    }
    choices_left <- names(left_df)
    choices_right <- names(right_df)
    defaults <- intersect(choices_left, choices_right)
    selected_left <- isolate(input$join_key_left)
    if (is.null(selected_left) || !all(selected_left %in% choices_left)) {
      selected_left <- defaults
    }
    selected_right <- isolate(input$join_key_right)
    if (is.null(selected_right) || length(selected_right) != length(selected_left) || !all(selected_right %in% choices_right)) {
      selected_right <- defaults
    }
    tagList(
      selectizeInput("join_key_left", "Left key(s)", choices = choices_left, selected = selected_left, multiple = TRUE, options = list(placeholder = "Select join keys")),
      selectizeInput("join_key_right", "Right key(s)", choices = choices_right, selected = selected_right, multiple = TRUE, options = list(placeholder = "Select join keys"))
    )
  })
  
  data_joined <- reactive({
    left_code <- input$join_left_table
    right_code <- input$join_right_table
    left_keys <- input$join_key_left
    right_keys <- input$join_key_right
    join_type <- input$join_type %||% "left"
    left_df <- join_table_data(left_code)
    right_df <- join_table_data(right_code)
    if (is.null(left_df) || is.null(right_df)) return(NULL)
    left_keys <- left_keys[left_keys %in% names(left_df)]
    right_keys <- right_keys[right_keys %in% names(right_df)]
    if (is.null(left_keys) || is.null(right_keys) || length(left_keys) == 0 || length(left_keys) != length(right_keys)) {
      return(NULL)
    }
    by <- setNames(right_keys, left_keys)
    tryCatch({
      res <- switch(
        join_type,
        left = dplyr::left_join(left_df, right_df, by = by),
        right = dplyr::right_join(left_df, right_df, by = by),
        inner = dplyr::inner_join(left_df, right_df, by = by),
        full = dplyr::full_join(left_df, right_df, by = by),
        semi = dplyr::semi_join(left_df, right_df, by = by),
        anti = dplyr::anti_join(left_df, right_df, by = by),
        dplyr::left_join(left_df, right_df, by = by)
      )
      tibble::as_tibble(res)
    }, error = function(e) {
      showNotification(paste("Join failed:", e$message), type = "error")
      NULL
    })
  })
  
  output$join_preview <- renderDT({
    df <- data_joined()
    validate(need(!is.null(df), "Configure join inputs to view preview."))
    datatable(df, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  dataset_source_choices <- reactive({
    choices <- c("Transformed" = "transformed")
    joined <- data_joined()
    if (!is.null(joined) && nrow(joined) > 0 && ncol(joined) > 0) {
      choices <- c(choices, "Joined" = "joined")
    }
    choices
  })
  
  resolve_dataset <- function(source) {
    if (identical(source, "joined")) {
      joined <- data_joined()
      validate(need(!is.null(joined) && nrow(joined) > 0, "Join data to use this source."))
      joined
    } else {
      data_transformed()
    }
  }
  
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

  # --- Column type editor -------------------------------------------------
  output$type_editor <- renderUI({
    df <- data_transformed()
    if (ncol(df) == 0) {
      return(wellPanel("No columns available."))
    }
    cols <- names(df)
    overrides <- type_overrides()
    selected_col <- input$type_override_col
    if (is.null(selected_col) || !selected_col %in% cols) {
      selected_col <- cols[1]
    }
    current_type <- overrides[[selected_col]] %||% "Keep as is"
    tagList(
      fluidRow(
        column(6, selectInput("type_override_col", "Column", choices = cols, selected = selected_col)),
        column(6, selectInput("type_override_type", "Target type", choices = c("Keep as is", "numeric", "character", "factor", "date"), selected = current_type))
      ),
      tableOutput("type_overrides_table")
    )
  })

  observeEvent(input$type_override_col, {
    overrides <- type_overrides()
    col <- input$type_override_col
    if (is.null(col)) return()
    current <- overrides[[col]] %||% "Keep as is"
    updateSelectInput(session, "type_override_type", selected = current)
  }, ignoreNULL = TRUE)

  observeEvent(input$type_override_type, {
    col <- input$type_override_col
    req(col)
    type <- input$type_override_type
    overrides <- type_overrides()
    if (is.null(type) || identical(type, "Keep as is")) {
      if (!is.null(overrides[[col]])) {
        overrides[[col]] <- NULL
        type_overrides(overrides)
      }
    } else {
      overrides[[col]] <- type
      type_overrides(overrides)
    }
  }, ignoreNULL = TRUE)

  output$type_overrides_table <- renderTable({
    overrides <- type_overrides()
    if (length(overrides) == 0) {
      return(tibble(message = "No overrides set."))
    }
    tibble(column = names(overrides), target_type = unname(unlist(overrides)))
  })

  observe({
    df <- data_transformed()
    overrides <- type_overrides()
    if (length(overrides) == 0) return()
    keep <- overrides[names(overrides) %in% names(df)]
    if (length(keep) != length(overrides)) {
      type_overrides(keep)
    }
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
    date_cols <- cols[vapply(df, function(x) inherits(x, c("Date", "POSIXct", "POSIXt")), logical(1))]
    string_cols <- cols[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
    list(cols = cols, numeric = numeric_cols, date = date_cols, string = string_cols, categorical = string_cols)
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
           ),
           dateparts = {
             date_choices <- if (length(params$date) > 0) params$date else params$cols
             tagList(
               selectInput("mutate_col_a", "Date column", choices = date_choices),
               textInput("mutate_date_prefix", "Prefix for new columns", value = if (length(date_choices) > 0) paste0(date_choices[1], "_") else "date_"),
               checkboxGroupInput("mutate_date_parts", "Parts to extract", choices = c("Year" = "year", "Quarter" = "quarter", "Month" = "month", "Week" = "week", "Weekday" = "wday"), selected = c("year", "month"))
             )
           },
           stringextract = {
             str_choices <- if (length(params$string) > 0) params$string else params$cols
             tagList(
               textInput("mutate_new", "New column name"),
               selectInput("mutate_col_a", "Text column", choices = str_choices),
               radioButtons("mutate_extract_mode", "Mode", choices = c("Split" = "split", "Regex" = "regex"), inline = TRUE),
               textInput("mutate_pattern", "Separator or pattern"),
               textInput("mutate_regex_group", "Regex group (optional)")
             )
           },
           factorlump = {
             cat_choices <- if (length(params$categorical) > 0) params$categorical else params$cols
             tagList(
               textInput("mutate_new", "New column name"),
               selectInput("mutate_col_a", "Categorical column", choices = cat_choices),
               numericInput("mutate_lump_n", "Keep top N", value = 5, min = 1, step = 1)
             )
           },
           impute = tagList(
             textInput("mutate_new", "New column name"),
             selectInput("mutate_col_a", "Column", choices = params$cols),
             selectInput("mutate_impute_method", "Strategy", choices = c("Mean" = "mean", "Median" = "median", "Mode" = "mode", "Constant" = "constant")),
             conditionalPanel(
               condition = "input.mutate_impute_method == 'constant'",
               textInput("mutate_impute_constant", "Constant value")
             ),
             checkboxInput("mutate_impute_flag", "Create _is_imputed flag", value = FALSE)
           )
    )
  })
  
  output$mutate_params_2 <- renderUI({ NULL })
  output$mutate_params_extra <- renderUI({ NULL })
  
  observeEvent(input$add_mutate, {
    helper <- req(input$mutate_helper)
    params <- list(type = helper)
    if (helper == "dateparts") {
      params$col_a <- input$mutate_col_a
      params$parts <- input$mutate_date_parts
      params$prefix <- input$mutate_date_prefix
      if (is.null(params$col_a) || params$col_a == "") {
        showNotification("Select a date column for date parts.", type = "warning")
        return()
      }
      if (is.null(params$parts) || length(params$parts) == 0) {
        showNotification("Choose at least one date part to extract.", type = "warning")
        return()
      }
      params$new <- params$prefix %||% paste0(params$col_a, "_")
    } else {
      params$new <- input$mutate_new
      if (is.null(params$new) || params$new == "") {
        showNotification("Please provide a name for the new column.", type = "warning")
        return()
      }
      params$col_a <- input$mutate_col_a
      if (is.null(params$col_a) || params$col_a == "") {
        showNotification("Select a source column.", type = "warning")
        return()
      }
    }
    if (helper == "arithmetic") {
      params$col_b <- input$mutate_col_b
      params$op <- input$mutate_arith_op
      if (is.null(params$col_b) || params$col_b == "") {
        showNotification("Select a second column for arithmetic operations.", type = "warning")
        return()
      }
    } else if (helper == "log") {
      params$log_base <- input$mutate_log_base
    } else if (helper == "stringcase") {
      params$case <- input$mutate_case
    } else if (helper == "parsedate") {
      params$format <- input$mutate_format
    } else if (helper == "winsor") {
      params$winsor <- input$mutate_winsor
    } else if (helper == "stringextract") {
      params$pattern <- input$mutate_pattern
      params$mode <- input$mutate_extract_mode %||% "split"
      params$regex_group <- input$mutate_regex_group
      if (is.null(params$pattern) || params$pattern == "") {
        showNotification("Provide a separator or pattern for extraction.", type = "warning")
        return()
      }
    } else if (helper == "factorlump") {
      params$lump_n <- input$mutate_lump_n %||% 5
    } else if (helper == "impute") {
      params$method <- input$mutate_impute_method %||% "mean"
      params$constant <- input$mutate_impute_constant
      params$flag <- isTRUE(input$mutate_impute_flag)
      if (identical(params$method, "constant") && (is.null(params$constant) || params$constant == "")) {
        showNotification("Provide a constant value for imputation.", type = "warning")
        return()
      }
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
    } else if (step$type == "dateparts") {
      parts <- step$parts %||% character(0)
      prefix <- step$new %||% paste0(col, "_")
      if (length(parts) > 0) {
        for (part in parts) {
          new_name <- paste0(prefix, part)
          df <- df %>% mutate(!!new_name := {
            val <- .data[[col]]
            if (!inherits(val, c("Date", "POSIXct", "POSIXt"))) {
              val <- suppressWarnings(lubridate::ymd(val))
            }
            if (part == "year") {
              lubridate::year(val)
            } else if (part == "quarter") {
              lubridate::quarter(val)
            } else if (part == "month") {
              lubridate::month(val)
            } else if (part == "week") {
              lubridate::isoweek(val)
            } else if (part == "wday") {
              lubridate::wday(val, label = TRUE, abbr = TRUE)
            } else {
              NA
            }
          })
        }
      }
    } else if (step$type == "stringextract") {
      pattern <- step$pattern %||% ""
      mode <- step$mode %||% "split"
      df <- df %>% mutate(!!new_col := {
        val <- as.character(.data[[col]])
        if (mode == "regex") {
          grp <- suppressWarnings(as.integer(step$regex_group))
          if (!is.na(grp) && grp >= 1) {
            mat <- stringr::str_match(val, pattern)
            if (!is.null(mat) && ncol(mat) > grp) mat[, grp + 1] else NA_character_
          } else {
            stringr::str_extract(val, pattern)
          }
        } else {
          pieces <- stringr::str_split(val, pattern)
          vapply(pieces, function(x) if (length(x) >= 1) x[[1]] else NA_character_, character(1))
        }
      })
    } else if (step$type == "factorlump") {
      n <- step$lump_n %||% 5
      df <- df %>% mutate(!!new_col := forcats::fct_lump_n(as.factor(.data[[col]]), n = n))
    } else if (step$type == "impute") {
      method <- step$method %||% "mean"
      flag_name <- if (isTRUE(step$flag)) paste0(step$new, "_is_imputed") else NULL
      df <- df %>% mutate(
        !!new_col := {
          vec <- .data[[col]]
          miss <- is.na(vec)
          fill <- if (method == "mean") {
            mean(vec, na.rm = TRUE)
          } else if (method == "median") {
            median(vec, na.rm = TRUE)
          } else if (method == "mode") {
            tbl <- table(vec)
            if (length(tbl) == 0) NA else names(tbl)[which.max(tbl)]
          } else if (method == "constant") {
            if (is.numeric(vec)) {
              suppressWarnings(as.numeric(step$constant))
            } else {
              step$constant
            }
          } else {
            NA
          }
          if (is.numeric(vec) && !is.numeric(fill)) {
            fill <- suppressWarnings(as.numeric(fill))
          }
          ifelse(miss, fill, vec)
        }
      )
      if (!is.null(flag_name)) {
        df <- df %>% mutate(!!flag_name := is.na(.data[[col]]))
      }
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
    } else if (step$type == "dateparts") {
      parts <- step$parts %||% character(0)
      if (length(parts) == 0) return(NULL)
      prefix <- step$new %||% paste0(col, "_")
      part_expr <- purrr::map_chr(parts, function(part) {
        expr <- switch(part,
                       year = "lubridate::year(.data[['{col}']])",
                       quarter = "lubridate::quarter(.data[['{col}']])",
                       month = "lubridate::month(.data[['{col}']])",
                       week = "lubridate::isoweek(.data[['{col}']])",
                       wday = "lubridate::wday(.data[['{col}']], label = TRUE, abbr = TRUE)",
                       "NA"
        )
        glue::glue("{prefix}{part} = {expr}")
      })
      glue::glue("dplyr::mutate({paste(part_expr, collapse = ', ')})")
    } else if (step$type == "stringextract") {
      pattern <- stringr::str_replace_all(step$pattern %||% "", "'", "\\\\'")
      mode <- step$mode %||% "split"
      if (mode == "regex") {
        grp <- suppressWarnings(as.integer(step$regex_group))
        if (!is.na(grp) && grp >= 1) {
          glue::glue("dplyr::mutate({new_col} = stringr::str_match(.data[['{col}']], '{pattern}')[, {grp + 1}])")
        } else {
          glue::glue("dplyr::mutate({new_col} = stringr::str_extract(.data[['{col}']], '{pattern}'))")
        }
      } else {
        glue::glue("dplyr::mutate({new_col} = stringr::str_split(.data[['{col}']], '{pattern}', n = 2, simplify = TRUE)[,1])")
      }
    } else if (step$type == "factorlump") {
      n <- step$lump_n %||% 5
      glue::glue("dplyr::mutate({new_col} = forcats::fct_lump_n(as.factor(.data[['{col}']]), n = {n}))")
    } else if (step$type == "impute") {
      method <- step$method %||% "mean"
      fill_expr <- switch(
        method,
        mean   = glue::glue("mean(.data[['{col}']], na.rm = TRUE)"),
        median = glue::glue("stats::median(.data[['{col}']], na.rm = TRUE)"),
        mode   = glue::glue("names(sort(table(.data[['{col}']]), decreasing = TRUE))[1]"),
        constant = {
          const <- step$constant %||% ""
          num_const <- suppressWarnings(as.numeric(const))
          if (!is.na(num_const)) {
            # numeric constant – just return the number text
            as.character(num_const)
          } else {
            # text constant – escape single quotes, then wrap in single quotes
            const_escaped <- stringr::str_replace_all(const, "'", "\\\\'")
            paste0("'", const_escaped, "'")
          }
        },
        "NA"
      )
      flag_code <- if (isTRUE(step$flag)) glue::glue(", {new_col}_is_imputed = is.na(.data[['{col}']])") else ""
      glue::glue("dplyr::mutate({new_col} = ifelse(is.na(.data[['{col}']]), {fill_expr}, .data[['{col}']]){flag_code})")
      
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
    # --- Column type editor ---
    overrides <- type_overrides()
    if (length(overrides) > 0) {
      for (col in names(overrides)) {
        if (!col %in% names(df)) next
        target <- overrides[[col]]
        if (is.null(target) || identical(target, "Keep as is")) next
        vec <- df[[col]]
        if (identical(target, "numeric")) {
          df[[col]] <- suppressWarnings(as.numeric(vec))
        } else if (identical(target, "character")) {
          df[[col]] <- as.character(vec)
        } else if (identical(target, "factor")) {
          df[[col]] <- as.factor(vec)
        } else if (identical(target, "date")) {
          converted <- suppressWarnings({
            if (is.numeric(vec)) {
              tryCatch(as.Date(vec, origin = "1970-01-01"), error = function(e) rep(as.Date(NA), length(vec)))
            } else {
              as.Date(vec)
            }
          })
          if (is.character(vec)) {
            if (all(is.na(converted) & !is.na(vec))) {
              converted <- suppressWarnings(lubridate::ymd(vec))
            }
          }
          if (!(all(is.na(converted)) && any(!is.na(vec)))) {
            df[[col]] <- converted
          }
        }
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

  # --- Sampling helper ----------------------------------------------------
  data_for_profile <- reactive({
    df <- data_transformed()
    if (isTRUE(input$use_sample) && nrow(df) > 10000) {
      df <- dplyr::slice_head(df, n = 10000)
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
  
  export_pipeline_state <- function() {
    list(
      filters = current_filters(),
      selected_cols = input$selected_cols,
      rename_mapping = as.data.frame(rename_mapping(), stringsAsFactors = FALSE),
      mutate_steps = mutate_steps(),
      group_vars = input$group_vars,
      summary_cols = input$summary_cols,
      summary_fns = input$summary_fns,
      pivot = list(
        mode = input$pivot_mode,
        longer_ids = input$pivot_longer_ids,
        longer_vals = input$pivot_longer_vals,
        wider_names = input$pivot_wider_names,
        wider_values = input$pivot_wider_values
      ),
      drop_outliers = isTRUE(input$drop_outliers)
    )
  }
  
  import_pipeline_state <- function(state) {
    if (is.null(state) || !is.list(state)) return()
    filters <- state$filters
    filter_total <- max(1, length(filters %||% list()))
    filter_count(filter_total)
    session$onFlushed(function() {
      if (!is.null(filters)) {
        for (i in seq_len(filter_total)) {
          flt <- filters[[i]]
          if (is.null(flt)) next
          if (!is.null(flt$column) && flt$column %in% names(data_raw())) updateSelectInput(session, paste0("filter_col_", i), selected = flt$column)
          if (!is.null(flt$operator)) updateSelectInput(session, paste0("filter_op_", i), selected = flt$operator)
          if (!is.null(flt$value)) updateTextInput(session, paste0("filter_val_", i), value = flt$value)
        }
      } else {
        updateTextInput(session, "filter_val_1", value = "")
      }
    }, once = TRUE)
    
    if (!is.null(state$selected_cols)) {
      cols <- intersect(state$selected_cols, names(data_raw()))
      updateCheckboxGroupInput(session, "selected_cols", selected = cols)
    }
    
    if (!is.null(state$rename_mapping)) {
      mapping <- as.data.frame(state$rename_mapping, stringsAsFactors = FALSE)
      session$onFlushed(function() {
        if (!is.null(mapping$original) && !is.null(mapping$renamed)) {
          for (i in seq_len(nrow(mapping))) {
            original <- mapping$original[i]
            if (!is.null(original) && original %in% names(data_raw())) {
              updateTextInput(session, paste0("rename_", original), value = mapping$renamed[i])
            }
          }
        }
      }, once = TRUE)
    }
    
    mutate_steps(state$mutate_steps %||% list())
    updateSelectizeInput(session, "group_vars", selected = intersect(state$group_vars %||% character(0), names(data_raw())))
    updateSelectizeInput(session, "summary_cols", selected = intersect(state$summary_cols %||% character(0), names(data_raw())))
    updateCheckboxGroupInput(session, "summary_fns", selected = intersect(state$summary_fns %||% character(0), summary_funcs))
    
    if (!is.null(state$pivot)) {
      updateRadioButtons(session, "pivot_mode", selected = state$pivot$mode %||% "none")
      session$onFlushed(function() {
        updateSelectizeInput(session, "pivot_longer_ids", selected = state$pivot$longer_ids %||% character(0))
        updateSelectizeInput(session, "pivot_longer_vals", selected = state$pivot$longer_vals %||% character(0))
        if (!is.null(state$pivot$wider_names)) updateSelectInput(session, "pivot_wider_names", selected = state$pivot$wider_names)
        if (!is.null(state$pivot$wider_values)) updateSelectInput(session, "pivot_wider_values", selected = state$pivot$wider_values)
      }, once = TRUE)
    }
    
    updateCheckboxInput(session, "drop_outliers", value = isTRUE(state$drop_outliers))
    showNotification("Pipeline state imported.", type = "message")
  }
  
  observe({
    json_txt <- jsonlite::toJSON(export_pipeline_state(), auto_unbox = TRUE, pretty = TRUE)
    if (!is.null(input$pipeline_state_json)) {
      updateTextAreaInput(session, "pipeline_state_json", value = json_txt)
    }
  })
  
  observeEvent(input$pipeline_state_upload, {
    file <- input$pipeline_state_upload
    req(file$datapath)
    state <- tryCatch(jsonlite::fromJSON(file$datapath, simplifyVector = FALSE), error = function(e) {
      showNotification(paste("Unable to import pipeline:", e$message), type = "error")
      NULL
    })
    if (!is.null(state)) import_pipeline_state(state)
  })
  
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
    df <- data_for_profile()
    req(ncol(df) > 0)
    miss <- tibble(
      column = names(df),
      pct_missing = map_dbl(df, ~ mean(is.na(.)) * 100)
    )
    ggplot(miss, aes(x = forcats::fct_reorder(column, pct_missing), y = pct_missing, fill = pct_missing)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "% missing", title = "Missingness by column") +
      scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
      theme_minimal()
  })
  
  data_dictionary_tbl <- reactive({
    df <- data_raw()
    preview_vals <- map_chr(df, function(x) {
      vals <- unique(x[!is.na(x)])
      paste(head(vals, 3), collapse = ", ")
    })
    tibble(
      column = names(df),
      type = map_chr(df, friendly_type),
      pct_missing = map_dbl(df, ~ mean(is.na(.)) * 100),
      examples = preview_vals
    )
  })
  
  output$data_dictionary <- renderDT({
    dict <- data_dictionary_tbl()
    datatable(dict %>% mutate(pct_missing = sprintf("%.1f%%", pct_missing)), options = list(pageLength = 10))
  })
  
  output$download_dictionary <- downloadHandler(
    filename = function() paste0("data_dictionary_", Sys.Date(), ".csv"),
    content = function(file) {
      dict <- data_dictionary_tbl()
      write.csv(dict, file, row.names = FALSE)
    }
  )
  
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
  dist_data <- reactive({
    source <- input$dist_data_source %||% "transformed"
    if (identical(source, "transformed")) {
      data_for_profile()
    } else {
      resolve_dataset(source)
    }
  })
  
  output$dist_controls <- renderUI({
    df <- dist_data()
    req(ncol(df) > 0)
    choices <- dataset_source_choices()
    selected_source <- input$dist_data_source
    if (is.null(selected_source) || !selected_source %in% choices) {
      selected_source <- "transformed"
    }
    cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
    group_choices <- c("None", cat_cols)
    selected_group <- input$dist_group_by
    if (is.null(selected_group) || !selected_group %in% group_choices) {
      selected_group <- "None"
    }
    tagList(
      selectInput("dist_data_source", "Use dataset", choices = choices, selected = selected_source),
      selectInput("dist_var", "Variable", choices = names(df)),
      selectInput("dist_group_by", "Group / Facet by (optional)", choices = group_choices, selected = selected_group)
    )
  })
  
  output$dist_outputs <- renderUI({
    req(input$dist_var)
    if (is_demo()) {
      wellPanel("Upload your own data to view full distribution diagnostics.")
    } else {
      df <- dist_data()
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
    df <- dist_data()
    var <- input$dist_var
    req(is.numeric(df[[var]]))
    group_col <- input$dist_group_by %||% "None"
    if (!identical(group_col, "None") && group_col %in% names(df)) {
      ggplot(df, aes(x = .data[[var]], fill = .data[[group_col]])) +
        geom_histogram(bins = 30, alpha = 0.7, position = "identity", na.rm = TRUE) +
        facet_wrap(vars(.data[[group_col]]), scales = "free_y") +
        labs(title = paste("Distribution of", var, "by", group_col), x = var, y = "Count", fill = group_col) +
        theme_minimal()
    } else {
      ggplot(df, aes(x = .data[[var]])) +
        geom_histogram(aes(y = ..density..), fill = "#4E79A7", alpha = 0.7, bins = 30) +
        geom_density(color = "#F28E2B", linewidth = 1) +
        labs(title = paste("Distribution of", var), x = var, y = "Density") +
        theme_minimal()
    }
  })
  
  output$box_plot <- renderPlot({
    req(input$dist_var)
    df <- dist_data()
    var <- input$dist_var
    req(is.numeric(df[[var]]))
    group_col <- input$dist_group_by %||% "None"
    if (!identical(group_col, "None") && group_col %in% names(df)) {
      ggplot(df, aes(x = .data[[group_col]], y = .data[[var]], fill = .data[[group_col]])) +
        geom_boxplot(show.legend = FALSE, na.rm = TRUE) +
        labs(title = paste(var, "by", group_col), x = group_col, y = var) +
        theme_minimal()
    } else {
      ggplot(df, aes(y = .data[[var]])) +
        geom_boxplot(fill = "#59A14F") +
        labs(title = paste("Boxplot of", var), y = var) +
        theme_minimal()
    }
  })
  
  output$numeric_stats <- renderTable({
    req(input$dist_var)
    df <- dist_data()
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
    df <- dist_data()
    var <- input$dist_var
    req(!is.numeric(df[[var]]))
    top_k <- input$top_k %||% 10
    group_col <- input$dist_group_by %||% "None"
    if (!identical(group_col, "None") && group_col %in% names(df)) {
      var_sym <- rlang::sym(var)
      group_sym <- rlang::sym(group_col)
      counts <- df %>% count(!!var_sym, !!group_sym, name = "n")
      totals <- counts %>% group_by(!!var_sym) %>% summarise(total = sum(n), .groups = "drop") %>% arrange(desc(total))
      top_levels <- totals %>% slice_head(n = top_k) %>% pull(!!var_sym)
      counts <- counts %>% filter(!!var_sym %in% top_levels) %>% group_by(!!var_sym) %>% mutate(total = sum(n)) %>% ungroup()
      counts <- counts %>% mutate(var_label = forcats::fct_reorder(as.character(!!var_sym), total))
      ggplot(counts, aes(x = var_label, y = n, fill = .data[[group_col]])) +
        geom_col() +
        coord_flip() +
        labs(title = paste("Counts of", var, "by", group_col), x = var, y = "Count", fill = group_col) +
        theme_minimal()
    } else {
      counts <- df %>% count(.data[[var]], name = "n") %>% mutate(prop = n / sum(n)) %>% arrange(desc(n)) %>% slice_head(n = top_k)
      ggplot(counts, aes(x = reorder(as.character(.data[[var]]), n), y = n, fill = n)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(title = paste("Top", top_k, var), x = var, y = "Count") +
        theme_minimal()
    }
  })

  output$cat_stats <- renderTable({
    req(input$dist_var)
    df <- dist_data()
    var <- input$dist_var
    req(!is.numeric(df[[var]]))
    group_col <- input$dist_group_by %||% "None"
    top_k <- input$top_k %||% 10
    if (!identical(group_col, "None") && group_col %in% names(df)) {
      var_sym <- rlang::sym(var)
      group_sym <- rlang::sym(group_col)
      counts <- df %>% count(!!var_sym, !!group_sym, name = "count")
      totals <- counts %>% group_by(!!var_sym) %>% summarise(total = sum(count), .groups = "drop") %>% arrange(desc(total))
      top_levels <- totals %>% slice_head(n = top_k) %>% pull(!!var_sym)
      counts <- counts %>% filter(!!var_sym %in% top_levels) %>% group_by(!!var_sym) %>% mutate(total = sum(count), prop = count / sum(count)) %>% ungroup()
      counts %>% arrange(desc(total), desc(count)) %>% select(!!var_sym, !!group_sym, count, prop) %>% mutate(prop = scales::percent(prop))
    } else {
      counts <- df %>% count(.data[[var]], name = "count") %>% mutate(prop = count / sum(count)) %>% arrange(desc(count))
      head(counts, top_k)
    }
  })
  
  # Relationships tab ------------------------------------------------------
  rel_data <- reactive({
    source <- input$rel_data_source %||% "transformed"
    resolve_dataset(source)
  })
  
  output$rel_controls <- renderUI({
    df <- rel_data()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    factor_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
    if (length(numeric_cols) == 0) {
      return(wellPanel("Select a dataset with numeric columns."))
    }
    choices <- dataset_source_choices()
    selected_source <- input$rel_data_source
    if (is.null(selected_source) || !selected_source %in% choices) selected_source <- "transformed"
    selected_cat_a <- input$rel_cat_a
    if (is.null(selected_cat_a) || !selected_cat_a %in% factor_cols) {
      selected_cat_a <- if (length(factor_cols) > 0) factor_cols[1] else NULL
    }
    selected_cat_b <- input$rel_cat_b
    if (is.null(selected_cat_b) || !selected_cat_b %in% factor_cols) {
      if (length(factor_cols) > 1) {
        selected_cat_b <- factor_cols[2]
      } else {
        selected_cat_b <- selected_cat_a
      }
    }
    tagList(
      selectInput("rel_data_source", "Use dataset", choices = choices, selected = selected_source),
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
      ),
      hr(),
      fluidRow(
        column(6, selectInput("rel_cat_a", "Categorical A", choices = factor_cols, selected = selected_cat_a)),
        column(6, selectInput("rel_cat_b", "Categorical B", choices = factor_cols, selected = selected_cat_b))
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
        plotOutput("group_summary_plot"),
        tableOutput("rel_crosstab"),
        verbatimTextOutput("rel_chisq")
      )
    }
  })
  
  output$scatter_plot <- renderPlot({
    req(input$rel_x, input$rel_y)
    df <- rel_data()
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
    df <- rel_data()
    method <- tolower(input$rel_corr_method %||% "pearson")
    if (nrow(df) < 3) return("Insufficient data")
    ctest <- tryCatch(cor.test(df[[input$rel_x]], df[[input$rel_y]], method = method, use = "pairwise.complete.obs"), error = function(e) NULL)
    if (is.null(ctest)) return("Unable to compute correlation.")
    sprintf("%s correlation: %.3f (p = %.3f)", str_to_title(method), ctest$estimate, ctest$p.value)
  })
  
  output$group_summary_plot <- renderPlot({
    req(input$rel_group_num, input$rel_group_cat)
    df <- rel_data()
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

  # --- Relationships: categorical insights -------------------------------
  output$rel_crosstab <- renderTable({
    cat_a <- input$rel_cat_a
    cat_b <- input$rel_cat_b
    if (is.null(cat_a) || is.null(cat_b) || cat_a == "" || cat_b == "") {
      return(tibble(message = "Select categorical columns."))
    }
    df <- rel_data()
    if (!cat_a %in% names(df) || !cat_b %in% names(df)) {
      return(tibble(message = "Select categorical columns."))
    }
    cat_a_sym <- rlang::sym(cat_a)
    cat_b_sym <- rlang::sym(cat_b)
    counts <- df %>% filter(!is.na(.data[[cat_a]]), !is.na(.data[[cat_b]])) %>% count(!!cat_a_sym, !!cat_b_sym, name = "count")
    if (nrow(counts) == 0) {
      return(tibble(message = "No combinations to display."))
    }
    counts %>% tidyr::pivot_wider(names_from = !!cat_b_sym, values_from = count, values_fill = 0)
  })

  output$rel_chisq <- renderText({
    cat_a <- input$rel_cat_a
    cat_b <- input$rel_cat_b
    if (is.null(cat_a) || is.null(cat_b) || cat_a == "" || cat_b == "") {
      return("Select categorical columns.")
    }
    df <- rel_data()
    if (!cat_a %in% names(df) || !cat_b %in% names(df)) {
      return("Select categorical columns.")
    }
    df_clean <- df %>% select(all_of(c(cat_a, cat_b))) %>% filter(!is.na(.data[[cat_a]]), !is.na(.data[[cat_b]]))
    if (nrow(df_clean) == 0) {
      return("No data for chi-squared test.")
    }
    tbl <- table(df_clean[[cat_a]], df_clean[[cat_b]])
    ctest <- tryCatch(chisq.test(tbl), error = function(e) e)
    if (inherits(ctest, "htest")) {
      sprintf("Chi-squared test: X-squared = %.3f, df = %s, p-value = %.5f", ctest$statistic, paste(ctest$parameter, collapse = ","), ctest$p.value)
    } else {
      paste("Chi-squared test unavailable:", ctest$message)
    }
  })
  
  # Correlation tab -------------------------------------------------------
  corr_data <- reactive({
    source <- input$corr_data_source %||% "transformed"
    if (identical(source, "transformed")) {
      data_for_profile()
    } else {
      resolve_dataset(source)
    }
  })
  
  output$corr_controls <- renderUI({
    df <- corr_data()
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    if (length(numeric_cols) < 2) {
      return(wellPanel("Select a dataset with at least two numeric columns."));
    }
    choices <- dataset_source_choices()
    selected_source <- input$corr_data_source
    if (is.null(selected_source) || !selected_source %in% choices) selected_source <- "transformed"
    tagList(
      selectInput("corr_data_source", "Use dataset", choices = choices, selected = selected_source),
      selectInput("corr_method", "Method", choices = corr_methods),
      selectizeInput("corr_cols", "Columns", choices = numeric_cols, multiple = TRUE, selected = numeric_cols)
    )
  })
  
  corr_matrix <- reactive({
    cols <- input$corr_cols
    df <- corr_data()
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
  
  # --- Target EDA tab ----------------------------------------------------
  target_data <- reactive({
    source <- input$target_data_source %||% "transformed"
    resolve_dataset(source)
  })
  
  observeEvent(input$target_column, {
    df <- target_data()
    col <- input$target_column
    if (is.null(col) || is.null(df) || !col %in% names(df)) return()
    auto_type <- if (is.numeric(df[[col]])) "numeric" else "categorical"
    updateSelectInput(session, "target_type", selected = auto_type)
  }, ignoreNULL = TRUE)
  
  output$target_controls <- renderUI({
    df <- target_data()
    if (is.null(df) || ncol(df) == 0) {
      return(wellPanel("Select a dataset with columns to analyse."));
    }
    choices <- dataset_source_choices()
    selected_source <- input$target_data_source
    if (is.null(selected_source) || !selected_source %in% choices) selected_source <- "transformed"
    cols <- names(df)
    selected_target <- input$target_column
    if (is.null(selected_target) || !selected_target %in% cols) selected_target <- cols[1]
    auto_type <- if (is.numeric(df[[selected_target]])) "numeric" else "categorical"
    selected_type <- input$target_type
    if (is.null(selected_type) || !selected_type %in% c("numeric", "categorical")) selected_type <- auto_type
    cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    tagList(
      selectInput("target_data_source", "Use dataset", choices = choices, selected = selected_source),
      selectInput("target_column", "Target column", choices = cols, selected = selected_target),
      selectInput("target_type", "Target type", choices = c("Numeric" = "numeric", "Categorical" = "categorical"), selected = selected_type),
      helpText(sprintf("Auto-detected: %s", str_to_title(auto_type))),
      conditionalPanel(
        condition = "input.target_type == 'numeric'",
        selectInput("target_cat_for_plot", "Categorical for grouping", choices = c("None", cat_cols))
      ),
      conditionalPanel(
        condition = "input.target_type == 'categorical'",
        selectizeInput("target_numeric_cols", "Numeric columns", choices = setdiff(num_cols, selected_target), multiple = TRUE)
      )
    )
  })
  
  target_correlation_tbl <- reactive({
    df <- target_data()
    target <- input$target_column
    type <- input$target_type %||% "numeric"
    if (is.null(df) || is.null(target) || !target %in% names(df) || type != "numeric") return(tibble())
    if (!is.numeric(df[[target]])) return(tibble())
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    others <- setdiff(num_cols, target)
    if (length(others) == 0) return(tibble())
    tibble(
      feature = others,
      correlation = map_dbl(others, ~ suppressWarnings(cor(df[[target]], df[[.x]], use = "pairwise.complete.obs")))
    ) %>% filter(!is.na(correlation)) %>% arrange(desc(abs(correlation)))
  })
  
  output$target_outputs <- renderUI({
    df <- target_data()
    target <- input$target_column
    type <- input$target_type %||% "numeric"
    if (is.null(df) || is.null(target) || !target %in% names(df)) {
      return(wellPanel("Select a valid target column."));
    }
    if (type == "numeric" && !is.numeric(df[[target]])) {
      return(wellPanel("Target is not numeric; switch to categorical analysis."));
    }
    if (type == "categorical" && is.numeric(df[[target]])) {
      return(wellPanel("Target is numeric; switch to numeric analysis."));
    }
    if (type == "numeric") {
      tagList(
        h4("Correlation with numeric features"),
        tableOutput("target_correlation"),
        h4("Distribution by category"),
        plotOutput("target_numeric_plot")
      )
    } else {
      tagList(
        h4("Class frequencies"),
        plotOutput("target_class_plot"),
        uiOutput("target_numeric_plots")
      )
    }
  })
  
  output$target_correlation <- renderTable({
    tbl <- target_correlation_tbl()
    if (nrow(tbl) == 0) return(tibble(message = "No numeric correlations available."))
    tbl
  })
  
  output$target_numeric_plot <- renderPlot({
    df <- target_data()
    target <- req(input$target_column)
    req(is.numeric(df[[target]]))
    cat_col <- input$target_cat_for_plot
    if (is.null(cat_col) || cat_col == "None" || !cat_col %in% names(df)) {
      ggplot(df, aes(x = "", y = .data[[target]])) +
        geom_violin(fill = "#A0CBE8", alpha = 0.6) +
        geom_boxplot(width = 0.1, outlier.alpha = 0.3) +
        labs(x = NULL, y = target, title = paste("Distribution of", target)) +
        theme_minimal()
    } else {
      ggplot(df, aes(x = .data[[cat_col]], y = .data[[target]])) +
        geom_violin(fill = "#A0CBE8", alpha = 0.6, na.rm = TRUE) +
        geom_boxplot(width = 0.2, outlier.alpha = 0.3, na.rm = TRUE) +
        labs(x = cat_col, y = target, title = paste(target, "by", cat_col)) +
        theme_minimal()
    }
  })
  
  output$target_class_plot <- renderPlot({
    df <- target_data()
    target <- req(input$target_column)
    counts <- df %>% count(.data[[target]], name = "n") %>% mutate(prop = n / sum(n))
    ggplot(counts, aes(x = reorder(as.character(.data[[target]]), n), y = n, fill = n)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = target, y = "Count", title = paste("Class distribution for", target)) +
      theme_minimal()
  })
  
  output$target_numeric_plots <- renderUI({
    df <- target_data()
    target <- input$target_column
    req(!is.null(df), !is.null(target), target %in% names(df))
    cols <- input$target_numeric_cols
    if (is.null(cols) || length(cols) == 0) {
      return(wellPanel("Select numeric columns to compare means."));
    }
    tagList(lapply(seq_along(cols), function(i) {
      col <- cols[[i]]
      output_id <- paste0("target_num_plot_", i)
      local({
        col_local <- col
        output[[output_id]] <- renderPlot({
          df <- target_data()
          req(col_local %in% names(df), input$target_column %in% names(df))
          req(!is.numeric(df[[input$target_column]]))
          summary_df <- df %>% group_by(.data[[input$target_column]]) %>% summarise(
            mean = mean(.data[[col_local]], na.rm = TRUE),
            sd = sd(.data[[col_local]], na.rm = TRUE),
            n = dplyr::n(),
            se = sd / sqrt(pmax(n, 1)),
            ci_low = mean - qt(0.975, pmax(n - 1, 1)) * se,
            ci_high = mean + qt(0.975, pmax(n - 1, 1)) * se,
            .groups = 'drop'
          )
          ggplot(summary_df, aes(x = .data[[input$target_column]], y = mean)) +
            geom_col(fill = "#4E79A7", alpha = 0.8) +
            geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
            labs(x = input$target_column, y = paste("Mean", col_local), title = paste("Mean", col_local, "by", input$target_column)) +
            theme_minimal()
        })
      })
      plotOutput(output_id, height = "300px")
    }))
  })
  
  # --- Time Series tab ---------------------------------------------------
  time_series_data <- reactive({
    source <- input$time_data_source %||% "transformed"
    resolve_dataset(source)
  })
  
  output$time_series_controls <- renderUI({
    df <- time_series_data()
    if (is.null(df) || ncol(df) == 0) {
      return(wellPanel("Select a dataset to begin."))
    }
    date_cols <- names(df)[vapply(df, function(x) inherits(x, c("Date", "POSIXct", "POSIXt")), logical(1))]
    if (length(date_cols) == 0) {
      return(tagList(
        selectInput("time_data_source", "Use dataset", choices = dataset_source_choices(), selected = input$time_data_source %||% "transformed"),
        wellPanel("No date/datetime columns detected.")
      ))
    }
    numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
    # categorical = not date/datetime and not numeric
    cat_cols <- setdiff(names(df), c(date_cols, numeric_cols))
    choices <- dataset_source_choices()
    selected_source <- input$time_data_source
    if (is.null(selected_source) || !selected_source %in% choices) selected_source <- "transformed"
    if (length(numeric_cols) == 0) {
      return(tagList(
        selectInput("time_data_source", "Use dataset", choices = choices, selected = selected_source),
        selectInput("time_date_col", "Date column", choices = date_cols, selected = input$time_date_col %||% date_cols[1]),
        wellPanel("No numeric measures available.")
      ))
    }
    tagList(
      selectInput("time_data_source", "Use dataset", choices = choices, selected = selected_source),
      selectInput("time_date_col", "Date column", choices = date_cols, selected = input$time_date_col %||% date_cols[1]),
      selectInput("time_measure_col", "Numeric measure", choices = numeric_cols, selected = input$time_measure_col %||% numeric_cols[1]),
      selectInput("time_period", "Aggregation", choices = c("Day" = "day", "Week" = "week", "Month" = "month", "Quarter" = "quarter", "Year" = "year"), selected = input$time_period %||% "month"),
      selectInput("time_group_col", "Group by (optional)", choices = c("None", cat_cols), selected = input$time_group_col %||% "None"),
      selectInput("time_agg_fun", "Aggregation function", choices = c("Mean" = "mean", "Sum" = "sum", "Median" = "median", "Max" = "max", "Min" = "min"), selected = input$time_agg_fun %||% "mean"),
      dateRangeInput("time_date_range", "Date range (optional)", start = if (!is.null(input$time_date_range)) input$time_date_range[1] else NULL, end = if (!is.null(input$time_date_range)) input$time_date_range[2] else NULL)
    )
  })
  
  
  time_series_agg <- reactive({
    df <- time_series_data()
    date_col <- input$time_date_col
    value_col <- input$time_measure_col
    period <- input$time_period %||% "month"
    group_col <- input$time_group_col %||% "None"
    agg_fun <- input$time_agg_fun %||% "mean"

    if (is.null(df) || is.null(date_col) || is.null(value_col)) return(tibble())
    if (!date_col %in% names(df) || !value_col %in% names(df)) return(tibble())
    if (!is.numeric(df[[value_col]])) return(tibble())

    dt <- df[[date_col]]
    dt_posix <- suppressWarnings(as.POSIXct(dt, tz = "UTC"))
    if (all(is.na(dt_posix))) {
      dt_posix <- as.POSIXct(as.Date(dt), tz = "UTC")
    }
    date_range <- input$time_date_range
    if (!is.null(date_range) && length(date_range) == 2 && !all(is.na(dt_posix))) {
      start <- date_range[1]
      end <- date_range[2]
      if (!is.na(start) && !is.na(end)) {
        start_posix <- as.POSIXct(start, tz = "UTC")
        end_posix <- as.POSIXct(end, tz = "UTC") + (24 * 60 * 60 - 1)
        keep <- dt_posix >= start_posix & dt_posix <= end_posix
        keep[is.na(keep)] <- FALSE
        df <- df[keep, , drop = FALSE]
        dt_posix <- dt_posix[keep]
      }
    }
    if (nrow(df) == 0) return(tibble())
    floored <- lubridate::floor_date(dt_posix, unit = period)

    base_tbl <- tibble(.period = floored, value = df[[value_col]])
    fun <- tryCatch(match.fun(agg_fun), error = function(e) mean)

    # if grouping is chosen and exists, keep it
    if (!identical(group_col, "None") && group_col %in% names(df)) {
      base_tbl[[group_col]] <- df[[group_col]]
      agg <- base_tbl %>%
        group_by(.period, .data[[group_col]]) %>%
        summarise(measure = fun(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(.period, .data[[group_col]])
    } else {
      agg <- base_tbl %>%
        group_by(.period) %>%
        summarise(measure = fun(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(.period)
    }
    
    if (nrow(agg) == 0) return(tibble())
    
    # we still attach a date column for plotting
    agg <- agg %>% mutate(period_date = as.Date(.period))
    agg
  })
  
  
  output$time_series_outputs <- renderUI({
    agg <- time_series_agg()
    if (nrow(agg) == 0) {
      return(wellPanel("Configure the controls to produce a time series."))
    }
    tagList(
      plotOutput("time_series_plot"),
      tableOutput("time_series_table")
    )
  })
  
  output$time_series_plot <- renderPlot({
    agg <- time_series_agg()
    validate(need(nrow(agg) > 0, "No time series data to plot."))
    group_col <- input$time_group_col %||% "None"
    
    if (!identical(group_col, "None") && group_col %in% names(agg)) {
      ggplot(agg, aes(x = period_date, y = measure, color = .data[[group_col]])) +
        geom_line(linewidth = 1, na.rm = FALSE) +
        geom_point(na.rm = TRUE) +
        labs(
          x = NULL,
          y = input$time_measure_col %||% "Value",
          color = group_col,
          title = paste("Aggregated", input$time_measure_col %||% "value", "by", stringr::str_to_title(input$time_period %||% "month"))
        ) +
        theme_minimal()
    } else {
      ggplot(agg, aes(x = period_date, y = measure)) +
        geom_line(color = "#4E79A7", linewidth = 1, na.rm = FALSE) +
        geom_point(color = "#F28E2B", na.rm = TRUE) +
        labs(
          x = NULL,
          y = input$time_measure_col %||% "Value",
          title = paste("Aggregated", input$time_measure_col %||% "value", "by", stringr::str_to_title(input$time_period %||% "month"))
        ) +
        theme_minimal()
    }
  })
  
  
  output$time_series_table <- renderTable({
    agg <- time_series_agg()
    if (nrow(agg) == 0) return(tibble())
    
    # turn the Date into a nice string so the table doesn't show negatives
    agg <- agg %>% mutate(period = format(period_date, "%Y-%m-%d"))
    
    if (!is.null(input$time_group_col) &&
        input$time_group_col != "None" &&
        input$time_group_col %in% names(agg)) {
      agg %>% select(period, group = !!rlang::sym(input$time_group_col), value = measure)
    } else {
      agg %>% select(period, value = measure)
    }
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
  duplicate_rows <- reactive({
    df <- data_transformed()
    if (nrow(df) == 0) return(tibble())
    dup_idx <- duplicated(df) | duplicated(df, fromLast = TRUE)
    if (!any(dup_idx)) return(tibble())
    tibble(row = which(dup_idx)) %>% bind_cols(df[dup_idx, , drop = FALSE])
  })
  
  duplicates_summary_tbl <- reactive({
    rows <- duplicate_rows()
    if (nrow(rows) == 0) return(tibble())
    rows %>% select(-row) %>% group_by(across(everything())) %>% summarise(count = dplyr::n(), .groups = "drop") %>% arrange(desc(count))
  })
  
  near_zero_tbl <- reactive({
    df <- data_transformed()
    if (nrow(df) == 0) return(tibble())
    tibble(column = names(df)) %>% mutate(
      unique_values = map_int(column, ~ dplyr::n_distinct(df[[.x]], na.rm = TRUE)),
      rows = nrow(df),
      ratio = ifelse(rows == 0, 0, unique_values / rows)
    ) %>% filter(ratio <= 0.01)
  })
  
  type_conflicts_tbl <- reactive({
    df <- data_transformed()
    tibble(column = names(df)) %>% mutate(
      detected_type = map_chr(column, ~ friendly_type(df[[.x]])),
      non_numeric_values = map_int(column, function(col) {
        vec <- df[[col]]
        if (is.numeric(vec) || inherits(vec, c("Date", "POSIXct", "POSIXt"))) return(0L)
        suppressWarnings(num <- as.numeric(as.character(vec)))
        sum(is.na(num) & !is.na(vec))
      })
    ) %>% filter(non_numeric_values > 0)
  })
  
  co_missing_tbl <- reactive({
    df <- data_transformed()
    if (ncol(df) < 2 || nrow(df) == 0) return(tibble())
    miss <- is.na(df)
    combos <- combn(ncol(df), 2, simplify = FALSE)
    tibble(
      column_a = map_chr(combos, ~ names(df)[.x[1]]),
      column_b = map_chr(combos, ~ names(df)[.x[2]]),
      co_missing = map_int(combos, ~ sum(miss[, .x[1]] & miss[, .x[2]], na.rm = TRUE))
    ) %>% mutate(pct_rows = ifelse(nrow(df) == 0, 0, co_missing / nrow(df))) %>% arrange(desc(co_missing)) %>% slice_head(n = 10)
  })
  
  quality_report_tbl <- reactive({
    pieces <- list(
      missingness = missing_summary_tbl(),
      duplicates = duplicates_summary_tbl(),
      near_zero = near_zero_tbl(),
      type_conflicts = type_conflicts_tbl(),
      co_missingness = co_missing_tbl()
    )
    purrr::imap_dfr(pieces, function(tbl, nm) {
      if (is.null(tbl) || nrow(tbl) == 0) return(tibble())
      tibble(section = nm) %>% bind_cols(tbl)
    })
  })
  
  output$quality_outputs <- renderUI({
    df <- data_transformed()
    num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
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
      h5('Top co-missingness pairs'),
      tableOutput('co_missing_table'),
      if (is_demo()) {
        wellPanel('Upload your data to inspect detailed missingness patterns.')
      } else {
        plotOutput('missing_map')
      },
      h4('Duplicates'),
      tableOutput('duplicates_table'),
      downloadButton('download_duplicates', 'Download duplicates'),
      h4('Near-zero / constant columns'),
      tableOutput('near_zero_table'),
      h4('Type conflicts'),
      tableOutput('type_conflicts_table'),
      h4('Outliers'),
      tableOutput('quality_outliers'),
      p('Toggle "Drop Tukey outliers" in the Transform tab to remove them from downstream analysis.'),
      downloadButton('download_quality_report', 'Download quality report')
    )
  })
  
  output$quality_missing <- renderTable({
    missing_summary_tbl() %>% mutate(pct_missing = sprintf("%.1f%%", pct_missing))
  })
  
  output$co_missing_table <- renderTable({
    tbl <- co_missing_tbl()
    if (nrow(tbl) == 0) return(tibble(message = 'No co-missingness detected.'))
    tbl %>% mutate(pct_rows = scales::percent(pct_rows))
  })
  
  output$duplicates_table <- renderTable({
    tbl <- duplicates_summary_tbl()
    if (nrow(tbl) == 0) return(tibble(message = 'No duplicate rows found.'))
    tbl
  })
  
  output$near_zero_table <- renderTable({
    tbl <- near_zero_tbl()
    if (nrow(tbl) == 0) return(tibble(message = 'No near-zero variance columns detected.'))
    tbl %>% mutate(ratio = scales::percent(ratio))
  })
  
  output$type_conflicts_table <- renderTable({
    tbl <- type_conflicts_tbl()
    if (nrow(tbl) == 0) return(tibble(message = 'No type conflicts detected.'))
    tbl
  })
  
  output$download_duplicates <- downloadHandler(
    filename = function() paste0("duplicates_", Sys.Date(), ".csv"),
    content = function(file) {
      rows <- duplicate_rows()
      if (nrow(rows) == 0) {
        write.csv(tibble(message = "No duplicates"), file, row.names = FALSE)
      } else {
        write.csv(rows, file, row.names = FALSE)
      }
    }
  )
  
  output$download_quality_report <- downloadHandler(
    filename = function() paste0("quality_report_", Sys.Date(), ".csv"),
    content = function(file) {
      report <- quality_report_tbl()
      if (nrow(report) == 0) {
        write.csv(tibble(message = "No issues detected."), file, row.names = FALSE)
      } else {
        write.csv(report, file, row.names = FALSE)
      }
    }
  )
  
  output$missing_map <- renderPlot({
    req(!is_demo())
    df <- data_for_profile() %>% head(100)
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
