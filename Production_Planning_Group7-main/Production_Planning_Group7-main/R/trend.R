
trend_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .tr-header { margin-bottom: 22px; }
      .tr-header h2 { color:#111827; font-size:30px; font-weight:800; }
      .tr-desc { color:#64748B; line-height:1.7; }
      .tr-card {
        background:white; border:1px solid #E2E8F0;
        border-radius:14px; padding:20px; margin-bottom:18px;
        box-shadow:0 4px 14px rgba(15,23,42,.04);
      }
      .tr-stat {
        background:white; border:1px solid #E2E8F0;
        border-radius:12px; padding:16px; margin-bottom:14px;
      }
      .tr-label { color:#64748B; font-size:12px; margin-bottom:8px; }
      .tr-value { color:#111827; font-size:23px; font-weight:800; }
      .tr-muted { color:#64748B; font-size:12px; line-height:1.7; }
    ")),
    
    shiny::div(
      class = "tr-header",
      shiny::div(class = "top-tag", "PRODUCTION ANALYTICS"),
      shiny::h2("Trend Forecasting"),
      shiny::p(
        class = "tr-desc",
        "Model trend patterns and forecast future observations."
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::div(
          class = "tr-card",
          shiny::h4("Model Parameters"),
          
          shiny::selectInput(
            ns("method"),
            "Forecasting method",
            choices = c(
              "Linear Regression" = "REGRESSION",
              "Holt's Linear Trend" = "HOLT"
            ),
            selected = "REGRESSION"
          ),
          
          shiny::numericInput(
            ns("horizon"),
            "Periods to forecast",
            value = 3,
            min = 1,
            max = 50,
            step = 1
          ),
          
          shiny::conditionalPanel(
            condition = "input.method == 'HOLT'",
            ns = ns,
            shiny::sliderInput(
              ns("alpha"),
              "Level smoothing (alpha)",
              min = 0.01, max = 0.99,
              value = 0.30, step = 0.01
            ),
            shiny::sliderInput(
              ns("beta"),
              "Trend smoothing (beta)",
              min = 0.01, max = 0.99,
              value = 0.20, step = 0.01
            )
          ),
          
          shiny::p(
            class = "tr-muted",
            "Observations come from Data Center. Update them there and click Apply Shared Data."
          ),
          
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
              class = "tr-stat",
              shiny::div(class = "tr-label", "Next-period forecast"),
              shiny::uiOutput(ns("next_forecast"))
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "tr-stat",
              shiny::div(class = "tr-label", "Observations"),
              shiny::uiOutput(ns("n_observations"))
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "tr-stat",
              shiny::div(class = "tr-label", "MAE"),
              shiny::uiOutput(ns("mae_value"))
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "tr-stat",
              shiny::div(class = "tr-label", "MSE"),
              shiny::uiOutput(ns("mse_value"))
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "tr-stat",
              shiny::div(class = "tr-label", "RMSE"),
              shiny::uiOutput(ns("rmse_value"))
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "tr-stat",
              shiny::div(class = "tr-label", "MAPE"),
              shiny::uiOutput(ns("mape_value"))
            )
          )
        ),
        
        shiny::div(
          class = "tr-card",
          shiny::h4("Actual vs Forecast"),
          shiny::plotOutput(ns("grafik"), height = "350px")
        ),
        
        shiny::div(
          class = "tr-card",
          shiny::h4("Forecast Results"),
          shiny::tableOutput(ns("forecast_table"))
        )
      )
    )
  )
}


trend_server <- function(id, shared_data) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    observations <- shiny::reactive({
      
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
      
      y <- observations()
      n <- length(y)
      horizon <- as.integer(input$horizon)
      
      shiny::validate(
        shiny::need(horizon >= 1, "Forecast horizon must be at least 1.")
      )
      
      future_periods <- seq.int(n + 1, n + horizon)
      
      if (input$method == "REGRESSION") {
        
        time <- seq_len(n)
        model <- stats::lm(y ~ time)
        
        fitted_values <- as.numeric(stats::fitted(model))
        future_values <- as.numeric(
          stats::predict(
            model,
            newdata = data.frame(time = future_periods)
          )
        )
        
      } else {
        
        alpha <- input$alpha
        beta <- input$beta
        
        level <- y[1]
        trend <- y[2] - y[1]
        
        fitted_values <- rep(NA_real_, n)
        
        for (t in 2:n) {
          
          fitted_values[t] <- level + trend
          
          new_level <- alpha * y[t] +
            (1 - alpha) * (level + trend)
          
          new_trend <- beta * (new_level - level) +
            (1 - beta) * trend
          
          level <- new_level
          trend <- new_trend
        }
        
        future_values <- vapply(
          seq_len(horizon),
          function(h) level + h * trend,
          numeric(1)
        )
      }
      
      result <- data.frame(
        Period = seq_len(n + horizon),
        Actual = c(y, rep(NA_real_, horizon)),
        Forecast = c(fitted_values, future_values)
      )
      
      result$Error <- result$Actual - result$Forecast
      result$Absolute_Error <- abs(result$Error)
      result$Squared_Error <- result$Error^2
      
      result
    })
    
    
    metric_values <- shiny::reactive({
      
      d <- forecast_data()
      valid <- !is.na(d$Actual) & !is.na(d$Forecast)
      
      actual <- d$Actual[valid]
      errors <- d$Actual[valid] - d$Forecast[valid]
      
      shiny::validate(
        shiny::need(
          length(errors) > 0,
          "Not enough fitted values to calculate accuracy metrics."
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
      value <- d$Forecast[nrow(d) - as.integer(input$horizon) + 1]
      
      shiny::div(
        class = "tr-value",
        format(round(value, 2), nsmall = 2, big.mark = ",")
      )
    })
    
    output$n_observations <- shiny::renderUI({
      shiny::div(class = "tr-value", length(observations()))
    })
    
    output$mae_value <- shiny::renderUI({
      shiny::div(class = "tr-value", round(metric_values()$mae, 2))
    })
    
    output$mse_value <- shiny::renderUI({
      shiny::div(class = "tr-value", round(metric_values()$mse, 2))
    })
    
    output$rmse_value <- shiny::renderUI({
      shiny::div(class = "tr-value", round(metric_values()$rmse, 2))
    })
    
    output$mape_value <- shiny::renderUI({
      m <- metric_values()$mape
      value <- if (is.na(m)) "N/A" else paste0(round(m, 2), "%")
      shiny::div(class = "tr-value", value)
    })
    
    
    output$grafik <- shiny::renderPlot({
      
      d <- forecast_data()
      actual_rows <- !is.na(d$Actual)
      forecast_rows <- !is.na(d$Forecast)
      
      values <- c(d$Actual[actual_rows], d$Forecast[forecast_rows])
      yrange <- range(values)
      
      if (diff(yrange) == 0) {
        yrange <- yrange + c(-1, 1)
      }
      
      plot(
        d$Period[actual_rows],
        d$Actual[actual_rows],
        type = "n",
        xlim = range(d$Period),
        ylim = yrange,
        xlab = "Period",
        ylab = "Value",
        main = "Actual Values and Trend Forecast",
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
      
      abline(v = length(observations()), col = "#A78BFA", lty = 3)
      
      legend(
        "topleft",
        legend = c("Actual", "Fitted / Forecast"),
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
        "Period", "Actual", "Forecast",
        "Error", "Absolute Error", "Squared Error"
      )
      
      shown
      
    }, striped = TRUE, bordered = FALSE, rownames = FALSE, na = "-")
    
    
    output$download_csv <- shiny::downloadHandler(
      
      filename = function() {
        paste0("trend_forecast_", Sys.Date(), ".csv")
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
