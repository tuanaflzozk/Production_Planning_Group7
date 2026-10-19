
learning_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .lc-header { margin-bottom: 25px; }
        .lc-eyebrow {
          display: inline-block;
          color: #6D28D9;
          background: #EDE9FE;
          padding: 6px 10px;
          border-radius: 6px;
          font-size: 10px;
          font-weight: 800;
          letter-spacing: 1.2px;
          margin-bottom: 12px;
        }
        .lc-header h2 {
          color: #111827;
          font-size: 30px;
          font-weight: 800;
          margin: 0 0 10px;
        }
        .lc-desc {
          color: #64748B;
          line-height: 1.7;
          font-size: 14px;
        }
        .lc-card {
          background: #FFFFFF;
          border: 1px solid #E2E8F0;
          border-radius: 14px;
          padding: 22px;
          margin-bottom: 20px;
          box-shadow: 0 4px 14px rgba(15,23,42,.04);
          overflow-x: auto;
        }
        .lc-card h4 {
          color: #1E293B;
          font-size: 16px;
          font-weight: 750;
          margin-top: 0;
        }
        .lc-muted {
          color: #64748B;
          font-size: 12px;
          line-height: 1.7;
        }
        .lc-stat {
          background: #FFFFFF;
          border: 1px solid #E2E8F0;
          border-radius: 12px;
          padding: 18px;
          min-height: 112px;
          margin-bottom: 15px;
        }
        .lc-stat-label {
          color: #64748B;
          font-size: 12px;
          margin-bottom: 9px;
        }
        .lc-stat-value {
          color: #111827;
          font-size: 24px;
          font-weight: 800;
          overflow-wrap: anywhere;
        }
        .lc-stat-note {
          color: #8B5CF6;
          font-size: 11px;
          margin-top: 6px;
        }
        .lc-highlight {
          background: #F8FAFC;
          border-left: 3px solid #A78BFA;
          padding: 15px 17px;
          border-radius: 7px;
          color: #475569;
          line-height: 1.8;
          font-size: 13px;
          margin-bottom: 20px;
        }
        .lc-card .form-control {
          border-radius: 8px;
          border-color: #CBD5E1;
          box-shadow: none;
        }
        .lc-card .form-control:focus {
          border-color: #A78BFA;
          box-shadow: 0 0 0 3px rgba(167,139,250,.13);
        }
        .lc-card .irs-bar,
        .lc-card .irs-bar-edge {
          background: #8B5CF6;
          border-top-color: #8B5CF6;
          border-bottom-color: #8B5CF6;
        }
        .lc-card .irs-single {
          background: #1E293B;
        }
        .lc-card .btn-primary {
          background: #1E293B;
          border-color: #1E293B;
          border-radius: 8px;
          font-weight: 650;
        }
        .lc-card .btn-primary:hover {
          background: #334155;
          border-color: #334155;
        }
        .lc-card table,
        .lc-card .table {
          width: 100% !important;
          border-collapse: collapse;
          font-size: 13px;
        }
        .lc-card th {
          background: #F1F5F9;
          color: #1E293B;
        }
        .lc-card th,
        .lc-card td {
          padding: 12px 14px;
          border-bottom: 1px solid #E2E8F0;
          white-space: nowrap;
        }
        .lc-card th:first-child,
        .lc-card td:first-child {
          text-align: left;
        }
        .lc-card tbody tr:hover {
          background: #F5F3FF;
        }
        @media(max-width:650px) {
          .lc-header h2 { font-size: 25px; }
          .lc-card { padding: 16px; }
        }
      "))
    ),
    
    div(
      class = "lc-header",
      div(class = "lc-eyebrow", "PRODUCTION ANALYTICS"),
      h2("Learning Curve"),
      p(
        class = "lc-desc",
        "Model how production time changes with cumulative experience, compare scenarios, and export your estimates."
      )
    ),
    
    fluidRow(
      column(
        4,
        div(
          class = "lc-card",
          h4("Model Parameters"),
          p(class = "lc-muted", "Adjust the assumptions used by the model."),
          
          sliderInput(
            ns("L"), "Learning rate",
            min = 60, max = 99,
            value = 80, step = 1,
            post = "%"
          ),
          
          numericInput(
            ns("first_time"),
            "Time required for the first unit",
            value = 100, min = 0.01,
            max = 1000000, step = 5
          ),
          
          numericInput(
            ns("n_units"),
            "Number of units to analyze",
            value = 50, min = 2,
            max = 500, step = 1
          ),
          
          numericInput(
            ns("focus_unit"),
            "Unit to inspect",
            value = 10, min = 1,
            max = 500, step = 1
          ),
          
          selectInput(
            ns("chart_mode"),
            "Chart view",
            choices = c(
              "Time per unit" = "unit",
              "Cumulative production time" = "cumulative"
            ),
            selected = "unit"
          ),
          
          downloadButton(
            ns("download_csv"),
            "Download forecast CSV",
            class = "btn-primary"
          )
        )
      ),
      
      column(
        8,
        fluidRow(
          column(
            6,
            div(
              class = "lc-stat",
              div(class = "lc-stat-label", "Learning rate"),
              uiOutput(ns("rate_value")),
              div(class = "lc-stat-note", "Time retained after output doubles")
            )
          ),
          column(
            6,
            div(
              class = "lc-stat",
              div(class = "lc-stat-label", "Learning exponent"),
              uiOutput(ns("exponent_value")),
              div(class = "lc-stat-note", "Derived from the learning rate")
            )
          ),
          column(
            6,
            div(
              class = "lc-stat",
              div(class = "lc-stat-label", "Selected unit time"),
              uiOutput(ns("selected_time_value")),
              div(class = "lc-stat-note", "Estimated time for the selected unit")
            )
          ),
          column(
            6,
            div(
              class = "lc-stat",
              div(class = "lc-stat-label", "Total production time"),
              uiOutput(ns("total_time_value")),
              div(class = "lc-stat-note", "Sum of estimated unit times")
            )
          )
        ),
        
        div(
          class = "lc-card",
          h4("Production Time Analysis"),
          p(
            class = "lc-muted",
            "Change the parameters to update the chart and estimates."
          ),
          plotOutput(ns("grafik"), height = "390px", width = "100%")
        ),
        
        div(
          class = "lc-card",
          h4("Forecast Table"),
          p(
            class = "lc-muted",
            "The table shows the first 10 units and the final unit. Download the CSV for all units."
          ),
          tableOutput(ns("forecast_table"))
        )
      )
    ),
    
    div(
      class = "lc-highlight",
      strong("Model assumption: "),
      "Y(n) = a × n^(-b), where a is the first-unit time and b = -ln(L)/ln(2). ",
      "This is a theoretical learning-curve model, not a fit to actual factory data."
    )
  )
}


learning_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    model_data <- reactive({
      req(input$L, input$first_time, input$n_units, input$focus_unit)
      
      validate(
        need(input$L >= 60 && input$L <= 99,
             "Learning rate must be between 60% and 99%."),
        need(is.finite(input$first_time) && input$first_time > 0,
             "First-unit time must be greater than zero."),
        need(is.finite(input$n_units) &&
               input$n_units >= 2 &&
               input$n_units <= 500,
             "Choose between 2 and 500 units."),
        need(is.finite(input$focus_unit) &&
               input$focus_unit >= 1,
             "Selected unit must be at least 1.")
      )
      
      n <- as.integer(input$n_units)
      focus <- min(as.integer(input$focus_unit), n)
      a <- input$first_time
      L <- input$L / 100
      b <- -log(L) / log(2)
      
      units <- seq_len(n)
      time <- a * units^(-b)
      
      data.frame(
        Unit = units,
        Unit_Time = time,
        Cumulative_Time = cumsum(time)
      )
    })
    
    output$rate_value <- renderUI({
      req(input$L)
      div(
        class = "lc-stat-value",
        paste0(input$L, "%")
      )
    })
    
    output$exponent_value <- renderUI({
      req(input$L)
      div(
        class = "lc-stat-value",
        sprintf("%.4f", -log(input$L / 100) / log(2))
      )
    })
    
    output$selected_time_value <- renderUI({
      d <- model_data()
      focus <- min(as.integer(input$focus_unit), nrow(d))
      
      div(
        class = "lc-stat-value",
        sprintf("%.2f", d$Unit_Time[focus])
      )
    })
    
    output$total_time_value <- renderUI({
      d <- model_data()
      div(
        class = "lc-stat-value",
        format(
          round(sum(d$Unit_Time), 2),
          big.mark = ",",
          scientific = FALSE,
          trim = TRUE
        )
      )
    })
    
    output$grafik <- renderPlot({
      d <- model_data()
      req(input$chart_mode)
      
      x <- d$Unit
      
      if (input$chart_mode == "cumulative") {
        y <- d$Cumulative_Time
        y_label <- "Cumulative production time"
        chart_title <- "Cumulative Time Across Production"
      } else {
        y <- d$Unit_Time
        y_label <- "Estimated time per unit"
        chart_title <- "Time Required per Unit"
      }
      
      validate(need(all(is.finite(y)), "Could not calculate the chart."))
      
      par(
        mar = c(5, 5, 3, 1),
        bg = "#FFFFFF",
        col.axis = "#475569",
        col.lab = "#334155",
        cex.axis = 0.9,
        cex.lab = 1
      )
      
      plot(
        x, y,
        type = "n",
        main = chart_title,
        xlab = "Cumulative units produced",
        ylab = y_label,
        col.main = "#111827",
        cex.main = 1.15,
        font.main = 2,
        axes = FALSE
      )
      
      axis(
        1,
        col = "#CBD5E1",
        lwd = 0,
        col.axis = "#475569"
      )
      
      axis(
        2,
        las = 1,
        col = "#CBD5E1",
        lwd = 0,
        col.axis = "#475569"
      )
      
      abline(
        h = axTicks(2),
        col = "#E9EDF4",
        lty = 1
      )
      
      polygon(
        c(x, rev(x)),
        c(y, rep(0, length(y))),
        col = grDevices::adjustcolor("#A78BFA", alpha.f = 0.13),
        border = NA
      )
      
      lines(x, y, col = "#8B5CF6", lwd = 3)
      
      focus <- min(as.integer(input$focus_unit), nrow(d))
      
      points(
        x[focus], y[focus],
        pch = 21,
        cex = 1.5,
        bg = "#A78BFA",
        col = "#FFFFFF",
        lwd = 2
      )
      
      box(col = "#CBD5E1")
    }, res = 100)
    
    output$forecast_table <- renderTable({
      d <- model_data()
      
      selected <- unique(c(
        seq_len(min(10, nrow(d))),
        nrow(d)
      ))
      
      shown <- d[selected, , drop = FALSE]
      
      names(shown) <- c(
        "Unit",
        "Estimated unit time",
        "Cumulative time"
      )
      
      shown[, 2:3] <- lapply(
        shown[, 2:3, drop = FALSE],
        function(x) round(x, 2)
      )
      
      shown
    },
    striped = TRUE,
    bordered = FALSE,
    spacing = "s",
    digits = 2,
    rownames = FALSE
    )
    
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("learning_curve_", Sys.Date(), ".csv")
      },
      content = function(file) {
        d <- model_data()
        write.csv(d, file, row.names = FALSE)
      }
    )
  })
}
