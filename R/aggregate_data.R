# Aggregate Planning Data Center - Assignment 3
# Requires shiny. Excel input additionally requires the readxl package.

aggregate_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "page-header",
      shiny::div(class = "top-tag", "AGGREGATE PLANNING"),
      shiny::h2("Aggregate Planning Data Center"),
      shiny::p(class = "page-description",
        "Upload period demand and working days, enter plant and cost information, and apply a common dataset to all aggregate planning methods.")),
    shiny::div(class = "content-panel",
      shiny::h4("1. Upload Planning Data"),
      shiny::p("Upload a CSV or Excel file containing columns for period, demand, and working days. The first worksheet is used for Excel files."),
      shiny::fileInput(ns("planning_file"), "Choose CSV or Excel File", accept = c(".csv", ".xlsx", ".xls")),
      shiny::p("Period names can be edited after uploading. Separate names with commas."),
      shiny::uiOutput(ns("period_editor")),
      shiny::tableOutput(ns("data_preview"))
    ),
    shiny::div(class = "content-panel",
      shiny::h4("2. Plant Data"),
      shiny::fluidRow(
        shiny::column(4, shiny::numericInput(ns("start_workers"), "Starting Workforce", 10, min = 0, step = 1)),
        shiny::column(4, shiny::numericInput(ns("start_inventory"), "Starting Inventory (units)", 0, min = 0)),
        shiny::column(4, shiny::numericInput(ns("end_inventory"), "Required Ending Inventory (units)", 0, min = 0))
      ),
      shiny::h4("Productivity (K)"),
      shiny::radioButtons(ns("k_method"), "Productivity Input Method",
        choices = c("Enter K directly" = "direct", "Calculate K from historical data" = "history"),
        selected = "direct", inline = TRUE),
      shiny::conditionalPanel(condition = sprintf("input['%s'] == 'direct'", ns("k_method")),
        shiny::numericInput(ns("k_direct"), "K (units per worker per day)", 1, min = 0.000001)),
      shiny::conditionalPanel(condition = sprintf("input['%s'] == 'history'", ns("k_method")),
        shiny::fluidRow(
          shiny::column(4, shiny::numericInput(ns("past_units"), "Historical Units Produced", 1000, min = 0)),
          shiny::column(4, shiny::numericInput(ns("past_workers"), "Historical Workers", 10, min = 1)),
          shiny::column(4, shiny::numericInput(ns("past_days"), "Historical Working Days", 20, min = 1))
        )),
      shiny::verbatimTextOutput(ns("k_summary"))
    ),
    shiny::div(class = "content-panel",
      shiny::h4("3. Cost Inputs"),
      shiny::fluidRow(
        shiny::column(4, shiny::numericInput(ns("hire_cost"), "Hiring Cost (per worker)", 100, min = 0)),
        shiny::column(4, shiny::numericInput(ns("fire_cost"), "Firing Cost (per worker)", 150, min = 0)),
        shiny::column(4, shiny::numericInput(ns("hold_cost"), "Holding Cost (per unit per period)", 2, min = 0))
      ),
      shiny::fluidRow(
        shiny::column(4, shiny::numericInput(ns("regular_cost"), "Regular Time Cost (per unit)", 10, min = 0)),
        shiny::column(4, shiny::numericInput(ns("overtime_cost"), "Overtime Cost (per unit)", 15, min = 0)),
        shiny::column(4, shiny::numericInput(ns("subcontract_cost"), "Subcontracting Cost (per unit)", 20, min = 0))
      ),
      shiny::actionButton(ns("apply_data"), "Apply Aggregate Planning Data", class = "btn-primary"),
      shiny::p(style = "margin-top:12px;", shiny::textOutput(ns("apply_status")))
    )
  )
}

aggregate_data_server <- function(id, shared_aggregate) {
  shiny::moduleServer(id, function(input, output, session) {
    imported <- shiny::reactive({
      shiny::req(input$planning_file)
      path <- input$planning_file$datapath
      ext <- tolower(tools::file_ext(input$planning_file$name))
      if (ext == "csv") {
        raw <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("Excel support requires readxl. Run install.packages('readxl') in the RStudio Console.")
        }
        raw <- as.data.frame(readxl::read_excel(path, sheet = 1), stringsAsFactors = FALSE)
      } else {
        stop("Unsupported file type. Choose CSV, XLSX, or XLS.")
      }
      if (ncol(raw) < 3L) stop("The file must have at least three columns: period, demand, and working days.")
      cols <- tolower(gsub("[^a-z0-9]", "", names(raw)))
      locate <- function(candidates, fallback) {
        hit <- match(candidates, cols, nomatch = 0L)
        hit <- hit[hit > 0L]
        if (length(hit)) hit[[1]] else fallback
      }
      p <- locate(c("period", "periodname", "month", "week"), 1L)
      d <- locate(c("demand", "demandforecast", "forecast", "forecastdemand"), 2L)
      w <- locate(c("workingdays", "workdays", "days", "workingday"), 3L)
      if (length(unique(c(p,d,w))) != 3L) stop("Could not identify distinct period, demand, and working-days columns.")
      result <- data.frame(
        period = trimws(as.character(raw[[p]])),
        demand = suppressWarnings(as.numeric(as.character(raw[[d]]))),
        working_days = suppressWarnings(as.numeric(as.character(raw[[w]]))),
        stringsAsFactors = FALSE
      )
      if (!nrow(result)) stop("The uploaded file contains no periods.")
      if (anyNA(result$demand) || anyNA(result$working_days) ||
          any(!is.finite(result$demand)) || any(!is.finite(result$working_days)) ||
          any(result$demand < 0) || any(result$working_days <= 0)) {
        stop("Demand must be nonnegative and working days must be positive numeric values.")
      }
      if (any(!nzchar(result$period))) result$period <- paste0("Period ", seq_len(nrow(result)))
      result
    })

    output$period_editor <- shiny::renderUI({
      d <- imported()
      shiny::textAreaInput(session$ns("period_names"), "Editable Period Names",
        value = paste(d$period, collapse = ", "), rows = 2, width = "100%")
    })

    edited_data <- shiny::reactive({
      d <- imported()
      shiny::req(input$period_names)
      labels <- trimws(strsplit(input$period_names, ",", fixed = TRUE)[[1]])
      shiny::validate(shiny::need(length(labels) == nrow(d) && all(nzchar(labels)),
        sprintf("Enter exactly %s non-empty period names, separated by commas.", nrow(d))))
      d$period <- labels
      d
    })

    output$data_preview <- shiny::renderTable({ edited_data() }, striped = TRUE, bordered = TRUE)

    effective_k <- shiny::reactive({
      k <- if (identical(input$k_method, "history")) {
        input$past_units / (input$past_workers * input$past_days)
      } else input$k_direct
      shiny::validate(shiny::need(length(k) == 1L && is.finite(k) && k > 0,
        "Productivity K must be greater than zero."))
      k
    })
    output$k_summary <- shiny::renderText(sprintf("Productivity K = %.4f units / worker / day", effective_k()))

    status <- shiny::reactiveVal("Upload a planning file and click Apply Aggregate Planning Data.")
    output$apply_status <- shiny::renderText(status())

    shiny::observeEvent(input$apply_data, {
      tryCatch({
        d <- edited_data()
        k <- effective_k()
        plant <- c(start_workers = input$start_workers,
                   start_inventory = input$start_inventory,
                   end_inventory = input$end_inventory)
        costs <- c(hiring = input$hire_cost, firing = input$fire_cost,
                   holding = input$hold_cost, regular = input$regular_cost,
                   overtime = input$overtime_cost, subcontract = input$subcontract_cost)
        if (anyNA(plant) || anyNA(costs) || any(!is.finite(c(plant, costs))) ||
            any(c(plant, costs) < 0)) stop("Plant values and costs must be nonnegative numbers.")
        shared_aggregate(list(periods = d, plant = plant, costs = costs, k = k))
        status(sprintf("Applied successfully: %d periods. Other aggregate planning modules can now use these data.", nrow(d)))
        shiny::showNotification("Aggregate planning data updated.", type = "message")
      }, error = function(e) {
        status(paste("Could not apply data:", conditionMessage(e)))
        shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
      })
    })
  })
}
