
time_series_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h3("Time Series Analysis"),
    shiny::p("Data source: Data Center. Update the data in Data Center to use the same values here."),
    
    shiny::wellPanel(
      shiny::selectInput(
        ns("view"),
        "Series to display",
        choices = c(
          "Original Series" = "original",
          "First Difference" = "difference"
        ),
        selected = "original"
      ),
      shiny::numericInput(
        ns("max_lag"), "Maximum ACF lag",
        value = 10, min = 1, step = 1
      )
    ),
    
    shiny::wellPanel(
      shiny::h4("ARIMA Forecasting"),
      shiny::numericInput(
        ns("horizon"), "Forecast horizon",
        value = 5, min = 1, max = 50, step = 1
      ),
      shiny::checkboxInput(
        ns("auto_arima"), "Automatically select ARIMA model",
        value = TRUE
      ),
      shiny::conditionalPanel(
        condition = sprintf("!input['%s']", ns("auto_arima")),
        shiny::numericInput(ns("p"), "AR order (p)", value = 1, min = 0, max = 5),
        shiny::numericInput(ns("d"), "Differencing order (d)", value = 1, min = 0, max = 2),
        shiny::numericInput(ns("q"), "MA order (q)", value = 1, min = 0, max = 5)
      ),
      shiny::actionButton(ns("run_arima"), "Run ARIMA Forecast"),
      shiny::br(),
      shiny::br(),
      shiny::verbatimTextOutput(ns("arima_model")),
      shiny::tableOutput(ns("forecast_table")),
      shiny::plotOutput(ns("forecast_plot"), height = "350px"),
      shiny::tableOutput(ns("accuracy"))
    ),
    
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::wellPanel(
          shiny::h4("Descriptive Statistics"),
          shiny::tableOutput(ns("statistics"))
        )
      ),
      shiny::column(
        6,
        shiny::wellPanel(
          shiny::h4("Data Preview"),
          shiny::tableOutput(ns("preview"))
        )
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::wellPanel(
          shiny::h4("Time Series Plot"),
          shiny::plotOutput(ns("series_plot"), height = "300px")
        )
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::wellPanel(
          shiny::h4("Autocorrelation Function (ACF)"),
          shiny::plotOutput(ns("acf_plot"), height = "300px")
        )
      ),
      shiny::column(
        6,
        shiny::wellPanel(
          shiny::h4("First Difference"),
          shiny::plotOutput(ns("difference_plot"), height = "300px")
        )
      )
    ),
    
    shiny::downloadButton(ns("download"), "Download Data")
  )
}


time_series_server <- function(id, shared_data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    values <- shiny::reactive({
      data <- shared_data()
      
      if (is.null(data) || length(data) == 0) {
        return(numeric(0))
      }
      
      if (is.data.frame(data)) {
        if ("Value" %in% names(data)) {
          data <- data$Value
        } else {
          data <- data[[1]]
        }
      }
      
      data <- suppressWarnings(as.numeric(data))
      data <- data[is.finite(data)]
      data
    })
    
    series_data <- shiny::reactive({
      y <- values()
      
      if (length(y) == 0) {
        return(data.frame(
          Period = integer(0),
          Value = numeric(0),
          Difference = numeric(0)
        ))
      }
      
      data.frame(
        Period = seq_along(y),
        Value = y,
        Difference = c(NA_real_, diff(y))
      )
    })
    
    displayed_data <- shiny::reactive({
      d <- series_data()
      
      if (identical(input$view, "difference")) {
        d <- d[!is.na(d$Difference), , drop = FALSE]
        return(data.frame(Period = d$Period, Value = d$Difference))
      }
      
      data.frame(Period = d$Period, Value = d$Value)
    })
    
    arima_result <- shiny::eventReactive(input$run_arima, {
      y <- values()
      
      shiny::validate(
        shiny::need(length(y) >= 10, "ARIMA requires at least 10 observations."),
        shiny::need(
          requireNamespace("forecast", quietly = TRUE),
          "The forecast package is not installed. Run install.packages('forecast') in the Console."
        )
      )
      
      shiny::validate(
        shiny::need(
          is.numeric(input$horizon) && input$horizon >= 1,
          "Enter a valid forecast horizon."
        )
      )
      
      if (isTRUE(input$auto_arima)) {
        model <- forecast::auto.arima(y)
      } else {
        model <- stats::arima(
          y,
          order = c(
            as.integer(input$p),
            as.integer(input$d),
            as.integer(input$q)
          )
        )
      }
      
      prediction <- forecast::forecast(
        model,
        h = as.integer(input$horizon)
      )
      
      list(
        model = model,
        prediction = prediction,
        actual = y
      )
    }, ignoreInit = TRUE)
    
    output$arima_model <- shiny::renderPrint({
      result <- arima_result()
      result$model
    })
    
    output$forecast_table <- shiny::renderTable({
      result <- arima_result()
      pred <- result$prediction
      
      data.frame(
        Period = length(result$actual) +
          seq_along(as.numeric(pred$mean)),
        Forecast = as.numeric(pred$mean),
        Lower_80 = as.numeric(pred$lower[, "80%"]),
        Upper_80 = as.numeric(pred$upper[, "80%"]),
        Lower_95 = as.numeric(pred$lower[, "95%"]),
        Upper_95 = as.numeric(pred$upper[, "95%"])
      )
    }, digits = 4, striped = TRUE, bordered = TRUE)
    
    output$forecast_plot <- shiny::renderPlot({
      result <- arima_result()
      graphics::plot(
        result$prediction,
        main = "ARIMA Forecast",
        xlab = "Period",
        ylab = "Value"
      )
      graphics::lines(
        stats::ts(result$actual),
        col = "black"
      )
    })
    
    output$accuracy <- shiny::renderTable({
      result <- arima_result()
      forecast::accuracy(result$prediction)
    }, digits = 4, striped = TRUE, bordered = TRUE)
    
    output$statistics <- shiny::renderTable({
      y <- values()
      
      if (length(y) == 0) {
        return(data.frame(
          Statistic = "Status",
          Value = "Enter data in Data Center"
        ))
      }
      
      data.frame(
        Statistic = c(
          "Number of observations", "Mean", "Median",
          "Standard deviation", "Variance",
          "Minimum", "Maximum", "Range"
        ),
        Value = c(
          length(y), mean(y), median(y),
          if (length(y) >= 2) stats::sd(y) else NA_real_,
          if (length(y) >= 2) stats::var(y) else NA_real_,
          min(y), max(y), diff(range(y))
        )
      )
    }, digits = 4, striped = TRUE, bordered = TRUE)
    
    output$preview <- shiny::renderTable({
      d <- displayed_data()
      
      if (nrow(d) == 0) {
        return(data.frame(
          Message = "No data available. Add observations in Data Center."
        ))
      }
      
      head(d, 15)
    }, digits = 4, striped = TRUE, bordered = TRUE)
    
    output$series_plot <- shiny::renderPlot({
      d <- displayed_data()
      
      if (nrow(d) == 0) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Enter observations in Data Center.")
        return()
      }
      
      graphics::plot(
        d$Period, d$Value,
        type = "o", pch = 16,
        xlab = "Period",
        ylab = if (identical(input$view, "difference")) "First Difference" else "Value",
        main = if (identical(input$view, "difference")) "First-Differenced Series" else "Original Time Series"
      )
      graphics::grid()
    })
    
    output$acf_plot <- shiny::renderPlot({
      y <- values()
      
      if (length(y) < 3) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "At least 3 observations are required for ACF.")
        return()
      }
      
      max_lag <- suppressWarnings(as.integer(input$max_lag))
      if (length(max_lag) == 0 || is.na(max_lag) || max_lag < 1) max_lag <- 1
      max_lag <- min(max_lag, length(y) - 1)
      
      stats::acf(
        y,
        lag.max = max_lag,
        main = "Autocorrelation Function (ACF)",
        na.action = stats::na.pass
      )
    })
    
    output$difference_plot <- shiny::renderPlot({
      y <- values()
      
      if (length(y) < 2) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "At least 2 observations are required.")
        return()
      }
      
      difference <- diff(y)
      
      graphics::plot(
        seq_along(difference) + 1, difference,
        type = "o", pch = 16,
        xlab = "Period",
        ylab = "First Difference",
        main = "First-Differenced Time Series"
      )
      
      graphics::abline(h = 0, lty = 2)
      graphics::grid()
    })
    
    output$download <- shiny::downloadHandler(
      filename = function() {
        paste0("time_series_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(displayed_data(), file, row.names = FALSE)
      }
    )
  })
}
