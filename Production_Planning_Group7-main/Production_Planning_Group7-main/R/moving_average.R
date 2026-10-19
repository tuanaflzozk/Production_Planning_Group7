
moving_average_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .ma-header { margin-bottom: 22px; }
      .ma-header h2 { color:#111827; font-size:30px; font-weight:800; }
      .ma-desc { color:#64748B; line-height:1.7; }
      .ma-card {
        background:white;
        border:1px solid #E2E8F0;
        border-radius:14px;
        padding:20px;
        margin-bottom:18px;
        box-shadow:0 4px 14px rgba(15,23,42,.04);
      }
      .ma-stat {
        background:white;
        border:1px solid #E2E8F0;
        border-radius:12px;
        padding:16px;
        margin-bottom:14px;
      }
      .ma-value { color:#111827; font-size:23px; font-weight:800; }
      .ma-muted { color:#64748B; font-size:12px; line-height:1.6; }
    ")),
    
    shiny::div(
      class = "ma-header",
      shiny::div(class = "top-tag", "PRODUCTION ANALYTICS"),
      shiny::h2("Moving Average"),
      shiny::p(
        class = "ma-desc",
        "Forecast future demand using Simple Moving Average or Weighted Moving Average."
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::div(
          class = "ma-card",
          shiny::h4("Model Parameters"),
          shiny::p(
            class = "ma-muted",
            "The observations are loaded from Data Center."
          ),
          
          shiny::selectInput(
            ns("method"),
            "Forecasting method",
            choices = c(
              "Simple Moving Average (SMA)" = "SMA",
              "Weighted Moving Average (WMA)" = "WMA"
            ),
            selected = "SMA"
          ),
          
          shiny::numericInput(
            ns("window"),
            "Moving average period (N)",
            value = 3,
            min = 2,
            max = 100,
            step = 1
          ),
          
          shiny::conditionalPanel(
            condition = "input.method == 'WMA'",
            ns = ns,
            shiny::textInput(
              ns("weights"),
              "Weights (oldest to newest)",
              value = "1,2,3"
            ),
            shiny::p(
              class = "ma-muted",
              "Enter one positive weight for each observation in the window."
            )
          ),
          
          shiny::div(
            class = "ma-muted",
            "To change the observations, open Data Center and click Apply Shared Data."
          ),
          
          shiny::br(),
          
          shiny::downloadButton(
            ns("download_csv"),
            "Download results CSV",
            class = "btn-primary"
          )
        )
      ),
      
      shiny::column(
        8,
        
        shiny::fluidRow(
          shiny::column(
            6,
            shiny::div(
              class = "ma-stat",
              shiny::div(class = "ma-muted", "Next-period forecast"),
              shiny::uiOutput(ns("next_forecast")),
              shiny::div(class = "ma-muted", "Forecast for the next period")
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "ma-stat",
              shiny::div(class = "ma-muted", "Observations"),
              shiny::uiOutput(ns("n_observations"))
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "ma-stat",
              shiny::div(class = "ma-muted", "MAE / MAD"),
              shiny::uiOutput(ns("mae_value"))
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "ma-stat",
              shiny::div(class = "ma-muted", "MSE"),
              shiny::uiOutput(ns("mse_value"))
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "ma-stat",
              shiny::div(class = "ma-muted", "RMSE"),
              shiny::uiOutput(ns("rmse_value"))
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "ma-stat",
              shiny::div(class = "ma-muted", "MAPE"),
              shiny::uiOutput(ns("mape_value"))
            )
          )
        ),
        
        shiny::div(
          class = "ma-card",
          shiny::h4("Actual vs Forecast"),
          shiny::plotOutput(ns("grafik"), height = "340px")
        ),
        
        shiny::div(
          class = "ma-card",
          shiny::h4("Forecast Results"),
          shiny::tableOutput(ns("forecast_table"))
        )
      )
    )
  )
}


moving_average_server <- function(id, shared_data) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    parsed_data <- shiny::reactive({
      
      y <- as.numeric(shared_data())
      
      shiny::validate(
        shiny::need(
          length(y) >= 3,
          "Enter at least three observations in Data Center."
        ),
        shiny::need(
          all(is.finite(y)),
          "Data Center contains invalid values."
        )
      )
      
      y
    })
    
    
    forecast_data <- shiny::reactive({
      
      y <- parsed_data()
      n <- length(y)
      N <- as.integer(input$window)
      
      shiny::validate(
        shiny::need(N >= 2, "N must be at least 2."),
        shiny::need(
          N < n,
          "N must be smaller than the number of observations."
        )
      )
      
      method <- input$method
      forecast <- rep(NA_real_, n + 1)
      
      if (method == "WMA") {
        
        pieces <- trimws(
          unlist(strsplit(input$weights, "[,;[:space:]]+"))
        )
        pieces <- pieces[nzchar(pieces)]
        
        shiny::validate(
          shiny::need(
            length(pieces) == N,
            paste("Enter exactly", N, "weights.")
          )
        )
        
        weights <- suppressWarnings(as.numeric(pieces))
        
        shiny::validate(
          shiny::need(
            all(is.finite(weights)) && all(weights > 0),
            "Weights must be positive numeric values."
          )
        )
        
        weights <- weights / sum(weights)
      }
      
      for (t in seq.int(N + 1, n + 1)) {
        
        previous <- y[(t - N):(t - 1)]
        
        if (method == "SMA") {
          forecast[t] <- mean(previous)
        } else {
          forecast[t] <- sum(previous * weights)
        }
      }
      
      results <- data.frame(
        Period = seq_len(n + 1),
        Actual = c(y, NA_real_),
        Forecast = forecast
      )
      
      results$Error <- results$Actual - results$Forecast
      results$Absolute_Error <- abs(results$Error)
      results$Squared_Error <- results$Error^2
      
      results
    })
    
    
    metric_values <- shiny::reactive({
      
      d <- forecast_data()
      valid <- !is.na(d$Actual) & !is.na(d$Forecast)
      
      actual <- d$Actual[valid]
      errors <- d$Actual[valid] - d$Forecast[valid]
      
      shiny::validate(
        shiny::need(
          length(errors) > 0,
          "Not enough forecasts to calculate accuracy."
        )
      )
      
      nonzero <- actual != 0
      
      mape <- if (any(nonzero)) {
        mean(abs(errors[nonzero] / actual[nonzero])) * 100
      } else {
        NA_real_
      }
      
      list(
        mae = mean(abs(errors)),
        mse = mean(errors^2),
        rmse = sqrt(mean(errors^2)),
        mape = mape
      )
    })
    
    
    output$next_forecast <- shiny::renderUI({
      d <- forecast_data()
      value <- d$Forecast[nrow(d)]
      shiny::div(
        class = "ma-value",
        format(round(value, 2), nsmall = 2, big.mark = ",")
      )
    })
    
    output$n_observations <- shiny::renderUI({
      shiny::div(
        class = "ma-value",
        length(parsed_data())
      )
    })
    
    output$mae_value <- shiny::renderUI({
      shiny::div(
        class = "ma-value",
        round(metric_values()$mae, 2)
      )
    })
    
    output$mse_value <- shiny::renderUI({
      shiny::div(
        class = "ma-value",
        round(metric_values()$mse, 2)
      )
    })
    
    output$rmse_value <- shiny::renderUI({
      shiny::div(
        class = "ma-value",
        round(metric_values()$rmse, 2)
      )
    })
    
    output$mape_value <- shiny::renderUI({
      m <- metric_values()$mape
      value <- if (is.na(m)) "N/A" else paste0(round(m, 2), "%")
      
      shiny::div(
        class = "ma-value",
        value
      )
    })
    
    
    output$grafik <- shiny::renderPlot({
      
      d <- forecast_data()
      actual_rows <- !is.na(d$Actual)
      forecast_rows <- !is.na(d$Forecast)
      
      values <- c(
        d$Actual[actual_rows],
        d$Forecast[forecast_rows]
      )
      
      yrange <- range(values)
      
      if (diff(yrange) == 0) {
        yrange <- yrange + c(-1, 1)
      }
      
      par(
        mar = c(5, 5, 3, 1),
        bg = "white",
        col.axis = "#475569",
        col.lab = "#334155"
      )
      
      plot(
        d$Period[actual_rows],
        d$Actual[actual_rows],
        type = "n",
        xlim = c(1, nrow(d)),
        ylim = yrange,
        xlab = "Period",
        ylab = "Value",
        main = "Actual Values and Forecast",
        axes = FALSE
      )
      
      axis(1, col = "#CBD5E1")
      axis(2, las = 1, col = "#CBD5E1")
      abline(h = axTicks(2), col = "#E9EDF4")
      
      lines(
        d$Period[actual_rows],
        d$Actual[actual_rows],
        type = "o",
        pch = 16,
        col = "#334155",
        lwd = 2
      )
      
      lines(
        d$Period[forecast_rows],
        d$Forecast[forecast_rows],
        type = "o",
        pch = 17,
        col = "#8B5CF6",
        lty = 2,
        lwd = 2
      )
      
      legend(
        "topleft",
        legend = c("Actual", "Forecast"),
        col = c("#334155", "#8B5CF6"),
        lty = c(1, 2),
        pch = c(16, 17),
        lwd = 2,
        bty = "n"
      )
      
      box(col = "#CBD5E1")
    })
    
    
    output$forecast_table <- shiny::renderTable({
      
      shown <- forecast_data()
      
      shown$Actual <- round(shown$Actual, 2)
      shown$Forecast <- round(shown$Forecast, 2)
      shown$Error <- round(shown$Error, 2)
      shown$Absolute_Error <- round(shown$Absolute_Error, 2)
      shown$Squared_Error <- round(shown$Squared_Error, 2)
      
      names(shown) <- c(
        "Period",
        "Actual",
        "Forecast",
        "Error",
        "Absolute Error",
        "Squared Error"
      )
      
      shown
      
    }, striped = TRUE, bordered = FALSE, rownames = FALSE, na = "-")
    
    
    output$download_csv <- shiny::downloadHandler(
      
      filename = function() {
        paste0("moving_average_", Sys.Date(), ".csv")
      },
      
      content = function(file) {
        utils::write.csv(
          forecast_data(),
          file,
          row.names = FALSE,
          na = ""
        )
      }
    )
    
  })
}
