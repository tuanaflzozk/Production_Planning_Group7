
stationary_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .st-header { margin-bottom: 25px; }
      .st-header h2 { color: #111827; font-size: 30px; font-weight: 800; }
      .st-desc { color: #64748B; line-height: 1.7; }
      .st-card {
        background: white; border: 1px solid #E2E8F0;
        border-radius: 14px; padding: 22px; margin-bottom: 20px;
        box-shadow: 0 4px 14px rgba(15,23,42,.04);
      }
      .st-stat {
        background: white; border: 1px solid #E2E8F0;
        border-radius: 12px; padding: 17px; min-height: 105px;
        margin-bottom: 15px;
      }
      .st-label { color: #64748B; font-size: 12px; margin-bottom: 8px; }
      .st-value { color: #111827; font-size: 23px; font-weight: 800; }
      .st-muted { color: #64748B; font-size: 12px; line-height: 1.7; }
      .st-note {
        background: #F8FAFC; border-left: 3px solid #A78BFA;
        padding: 15px; border-radius: 7px; color: #475569;
        line-height: 1.8; margin-bottom: 20px;
      }
    ")),
    
    shiny::div(
      class = "st-header",
      shiny::div(class = "top-tag", "PRODUCTION ANALYTICS"),
      shiny::h2("Stationary and Seasonal Forecasting"),
      shiny::p(
        class = "st-desc",
        "Compare forecasting methods using observations from Data Center."
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        4,
        shiny::div(
          class = "st-card",
          shiny::h4("Model Parameters"),
          shiny::p(
            class = "st-muted",
            "Observations are shared from Data Center."
          ),
          
          shiny::selectInput(
            ns("method"),
            "Forecasting method",
            choices = c(
              "Simple Moving Average (SMA)" = "SMA",
              "Weighted Moving Average (WMA)" = "WMA",
              "Simple Exponential Smoothing (SES)" = "SES",
              "Seasonal Factors" = "SEASONAL",
              "Holt-Winters Method" = "HW",
              "Seasonal Decomposition using Moving Averages" = "DECOMP"
            ),
            selected = "SMA"
          ),
          
          shiny::conditionalPanel(
            condition = "input.method == 'SMA' || input.method == 'WMA'",
            ns = ns,
            shiny::numericInput(
              ns("window"), "Moving average period (N)",
              value = 3, min = 2, max = 100
            )
          ),
          
          shiny::conditionalPanel(
            condition = "input.method == 'WMA'",
            ns = ns,
            shiny::textInput(
              ns("weights"), "Weights (oldest to newest)",
              value = "1,2,3"
            )
          ),
          
          shiny::conditionalPanel(
            condition = "input.method == 'SES'",
            ns = ns,
            shiny::sliderInput(
              ns("alpha"), "Smoothing constant (alpha)",
              min = 0.01, max = 0.99, value = 0.30, step = 0.01
            )
          ),
          
          shiny::conditionalPanel(
            condition = "input.method == 'SEASONAL' || input.method == 'HW' || input.method == 'DECOMP'",
            ns = ns,
            shiny::numericInput(
              ns("season_length"), "Periods per season",
              value = 4, min = 2, max = 100
            ),
            shiny::selectInput(
              ns("season_type"), "Seasonal model",
              choices = c(
                "Additive" = "additive",
                "Multiplicative" = "multiplicative"
              ),
              selected = "additive"
            ),
            shiny::p(
              class = "st-muted",
              "Example: 4 for quarterly data or 12 for monthly data. Multiplicative models require positive observations."
            )
          ),
          
          shiny::conditionalPanel(
            condition = "input.method == 'HW'",
            ns = ns,
            shiny::sliderInput(
              ns("hw_alpha"), "Level smoothing (alpha)",
              min = 0.01, max = 0.99, value = 0.30, step = 0.01
            ),
            shiny::sliderInput(
              ns("hw_beta"), "Trend smoothing (beta)",
              min = 0.01, max = 0.99, value = 0.10, step = 0.01
            ),
            shiny::sliderInput(
              ns("hw_gamma"), "Seasonal smoothing (gamma)",
              min = 0.01, max = 0.99, value = 0.10, step = 0.01
            )
          ),
          
          shiny::p(
            class = "st-muted",
            "To change observations, open Data Center and click Apply Shared Data."
          ),
          
          shiny::downloadButton(
            ns("download_csv"), "Download results CSV",
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
              class = "st-stat",
              shiny::div(class = "st-label", "Next-period forecast"),
              shiny::uiOutput(ns("next_forecast"))
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "st-stat",
              shiny::div(class = "st-label", "Observations"),
              shiny::uiOutput(ns("n_observations"))
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "st-stat",
              shiny::div(class = "st-label", "MAE / MAD"),
              shiny::uiOutput(ns("mae_value"))
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "st-stat",
              shiny::div(class = "st-label", "MSE"),
              shiny::uiOutput(ns("mse_value"))
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "st-stat",
              shiny::div(class = "st-label", "RMSE"),
              shiny::uiOutput(ns("rmse_value"))
            )
          ),
          shiny::column(
            6,
            shiny::div(
              class = "st-stat",
              shiny::div(class = "st-label", "MAPE"),
              shiny::uiOutput(ns("mape_value"))
            )
          )
        ),
        
        shiny::div(
          class = "st-card",
          shiny::h4("Actual vs Forecast"),
          shiny::plotOutput(ns("grafik"), height = "350px")
        ),
        
        shiny::div(
          class = "st-card",
          shiny::h4("Forecast Results"),
          shiny::tableOutput(ns("forecast_table"))
        ),
        
        shiny::conditionalPanel(
          condition = "input.method == 'SEASONAL' || input.method == 'HW' || input.method == 'DECOMP'",
          ns = ns,
          shiny::div(
            class = "st-card",
            shiny::h4("Seasonal Components"),
            shiny::tableOutput(ns("seasonal_table"))
          )
        )
      )
    ),
    
    shiny::div(
      class = "st-note",
      shiny::strong("Method guide: "),
      "SMA averages the previous N observations. WMA uses weighted observations. ",
      "SES smooths the level using alpha. Seasonal Factors estimates relative seasonal effects. ",
      "Holt-Winters models level, trend and seasonality. Seasonal Decomposition separates a series into trend, seasonal and irregular components."
    )
  )
}


stationary_server <- function(id, shared_data) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    parsed_data <- shiny::reactive({
      raw <- shared_data()
      
      if (is.data.frame(raw)) {
        if ("Value" %in% names(raw)) {
          raw <- raw$Value
        } else {
          raw <- raw[[1]]
        }
      }
      
      y <- suppressWarnings(as.numeric(raw))
      y <- y[is.finite(y)]
      
      shiny::validate(
        shiny::need(
          length(y) >= 3,
          "Enter at least three numeric observations in Data Center."
        )
      )
      
      y
    })
    
    
    forecast_data <- shiny::reactive({
      
      y <- parsed_data()
      n <- length(y)
      method <- input$method
      
      forecast_values <- rep(NA_real_, n + 1)
      seasonal_factors <- NULL
      seasonal_averages <- NULL
      
      if (method %in% c("SMA", "WMA")) {
        
        N <- as.integer(input$window)
        
        shiny::validate(
          shiny::need(N >= 2, "N must be at least 2."),
          shiny::need(N < n, "N must be smaller than the number of observations.")
        )
        
        if (method == "WMA") {
          pieces <- trimws(
            unlist(strsplit(input$weights, "[,;[:space:]]+"))
          )
          pieces <- pieces[nzchar(pieces)]
          weights <- suppressWarnings(as.numeric(pieces))
          
          shiny::validate(
            shiny::need(length(weights) == N, paste("Enter exactly", N, "weights.")),
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
            forecast_values[t] <- mean(previous)
          } else {
            forecast_values[t] <- sum(previous * weights)
          }
        }
        
      } else if (method == "SES") {
        
        alpha <- input$alpha
        level <- y[1]
        
        for (t in 2:n) {
          forecast_values[t] <- level
          level <- alpha * y[t] + (1 - alpha) * level
        }
        
        forecast_values[n + 1] <- level
        
      } else if (method %in% c("SEASONAL", "HW", "DECOMP")) {
        
        L <- as.integer(input$season_length)
        season_type <- input$season_type
        
        shiny::validate(
          shiny::need(L >= 2, "Season length must be at least 2."),
          shiny::need(
            n >= 2 * L,
            "Enter at least two complete seasons."
          ),
          shiny::need(
            season_type != "multiplicative" || all(y > 0),
            "Multiplicative models require every observation to be positive."
          )
        )
        
        ts_y <- stats::ts(y, frequency = L)
        
        if (method == "SEASONAL") {
          
          season_position <- ((seq_len(n) - 1) %% L) + 1
          
          position_means <- vapply(
            seq_len(L),
            function(j) mean(y[season_position == j]),
            numeric(1)
          )
          
          overall_mean <- mean(y)
          
          shiny::validate(
            shiny::need(
              season_type != "multiplicative" || overall_mean > 0,
              "The overall mean must be positive for multiplicative factors."
            )
          )
          
          if (season_type == "multiplicative") {
            seasonal_factors <- position_means / overall_mean
            seasonal_adjusted <- y / seasonal_factors[season_position]
          } else {
            seasonal_factors <- position_means - overall_mean
            seasonal_adjusted <- y - seasonal_factors[season_position]
          }
          
          for (t in seq.int(L + 1, n)) {
            forecast_values[t] <- if (season_type == "multiplicative") {
              y[t - L]
            } else {
              y[t - L]
            }
          }
          
          forecast_values[n + 1] <- if (season_type == "multiplicative") {
            y[n - L + 1]
          } else {
            y[n - L + 1]
          }
          
          seasonal_averages <- position_means
          
        } else if (method == "HW") {
          
          hw_model <- tryCatch(
            stats::HoltWinters(
              ts_y,
              alpha = input$hw_alpha,
              beta = input$hw_beta,
              gamma = input$hw_gamma,
              seasonal = season_type
            ),
            error = function(e) e
          )
          
          shiny::validate(
            shiny::need(
              !inherits(hw_model, "error"),
              if (inherits(hw_model, "error")) {
                paste("Holt-Winters could not fit the data:", hw_model$message)
              } else {
                ""
              }
            )
          )
          
          hw_fitted <- as.numeric(hw_model$fitted[, "xhat"])
          fitted_time <- time(hw_model$fitted)
          
          fitted_periods <- round(
            fitted_time * L - L + 1e-7
          )
          
          fitted_periods <- fitted_periods[
            fitted_periods >= 1 & fitted_periods <= n
          ]
          
          if (length(hw_fitted) > 0) {
            fitted_periods <- seq.int(
              max(1, n - length(hw_fitted) + 1),
              n
            )
            forecast_values[fitted_periods] <- tail(
              hw_fitted,
              length(fitted_periods)
            )
          }
          
          hw_prediction <- tryCatch(
            stats::predict(hw_model, n.ahead = 1),
            error = function(e) e
          )
          
          shiny::validate(
            shiny::need(
              !inherits(hw_prediction, "error"),
              "Holt-Winters could not produce a forecast for these data."
            )
          )
          
          forecast_values[n + 1] <- as.numeric(hw_prediction[1])
          
          seasonal_factors <- as.numeric(hw_model$seasonal)
          seasonal_averages <- NULL
          
        } else if (method == "DECOMP") {
          
          decomposition <- tryCatch(
            stats::decompose(ts_y, type = season_type),
            error = function(e) e
          )
          
          shiny::validate(
            shiny::need(
              !inherits(decomposition, "error"),
              if (inherits(decomposition, "error")) {
                paste("Decomposition failed:", decomposition$message)
              } else {
                ""
              }
            )
          )
          
          trend_component <- as.numeric(decomposition$trend)
          valid_trend <- is.finite(trend_component)
          
          shiny::validate(
            shiny::need(
              sum(valid_trend) >= 2,
              "Not enough valid trend values for decomposition."
            )
          )
          
          trend_model <- stats::lm(
            trend_component[valid_trend] ~
              which(valid_trend)
          )
          
          trend_fit <- rep(NA_real_, n)
          trend_fit[valid_trend] <- as.numeric(
            stats::predict(trend_model)
          )
          
          # Refit trend over all period indices so future periods can be predicted.
          trend_model <- stats::lm(
            trend_component[valid_trend] ~
              seq_len(n)[valid_trend]
          )
          
          trend_fit <- as.numeric(
            stats::predict(
              trend_model,
              newdata = data.frame(
                "seq_len(n)[valid_trend]" = seq_len(n)
              )
            )
          )
          
          seasonal_component <- as.numeric(decomposition$figure)
          seasonal_factors <- seasonal_component
          
          for (t in seq_len(n)) {
            if (season_type == "multiplicative") {
              forecast_values[t] <- trend_fit[t] * seasonal_component[((t - 1) %% L) + 1]
            } else {
              forecast_values[t] <- trend_fit[t] + seasonal_component[((t - 1) %% L) + 1]
            }
          }
          
          next_trend <- as.numeric(
            stats::predict(
              trend_model,
              newdata = data.frame(
                "seq_len(n)[valid_trend]" = n + 1
              )
            )
          )
          
          next_season <- seasonal_component[((n) %% L) + 1]
          
          forecast_values[n + 1] <- if (season_type == "multiplicative") {
            next_trend * next_season
          } else {
            next_trend + next_season
          }
          
          seasonal_averages <- NULL
        }
      }
      
      result <- data.frame(
        Period = seq_len(n + 1),
        Actual = c(y, NA_real_),
        Forecast = forecast_values
      )
      
      result$Error <- result$Actual - result$Forecast
      result$Absolute_Error <- abs(result$Error)
      result$Squared_Error <- result$Error^2
      
      list(
        results = result,
        seasonal_factors = seasonal_factors,
        seasonal_averages = seasonal_averages
      )
    })
    
    
    results_only <- shiny::reactive({
      forecast_data()$results
    })
    
    
    metric_values <- shiny::reactive({
      d <- results_only()
      valid <- is.finite(d$Actual) & is.finite(d$Forecast)
      
      actual <- d$Actual[valid]
      errors <- d$Actual[valid] - d$Forecast[valid]
      
      shiny::validate(
        shiny::need(
          length(errors) > 0,
          "Not enough fitted forecasts to calculate accuracy metrics."
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
      d <- results_only()
      value <- d$Forecast[nrow(d)]
      
      shiny::div(
        class = "st-value",
        format(round(value, 2), nsmall = 2, big.mark = ",")
      )
    })
    
    
    output$n_observations <- shiny::renderUI({
      shiny::div(class = "st-value", length(parsed_data()))
    })
    
    
    output$mae_value <- shiny::renderUI({
      shiny::div(class = "st-value", round(metric_values()$mae, 2))
    })
    
    
    output$mse_value <- shiny::renderUI({
      shiny::div(class = "st-value", round(metric_values()$mse, 2))
    })
    
    
    output$rmse_value <- shiny::renderUI({
      shiny::div(class = "st-value", round(metric_values()$rmse, 2))
    })
    
    
    output$mape_value <- shiny::renderUI({
      m <- metric_values()$mape
      value <- if (is.na(m)) "N/A" else paste0(round(m, 2), "%")
      shiny::div(class = "st-value", value)
    })
    
    
    output$grafik <- shiny::renderPlot({
      d <- results_only()
      actual_rows <- is.finite(d$Actual)
      forecast_rows <- is.finite(d$Forecast)
      
      values <- c(
        d$Actual[actual_rows],
        d$Forecast[forecast_rows]
      )
      
      if (length(values) == 0) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "No data available.")
        return()
      }
      
      yrange <- range(values)
      
      if (diff(yrange) == 0) {
        yrange <- yrange + c(-1, 1)
      }
      
      graphics::par(
        mar = c(5, 5, 3, 1),
        bg = "white",
        col.axis = "#475569",
        col.lab = "#334155"
      )
      
      graphics::plot(
        d$Period[actual_rows],
        d$Actual[actual_rows],
        type = "n",
        xlim = c(1, nrow(d)),
        ylim = yrange,
        xlab = "Period",
        ylab = "Observed value",
        main = "Actual Values and Forecast",
        axes = FALSE
      )
      
      graphics::axis(1, col = "#CBD5E1")
      graphics::axis(2, las = 1, col = "#CBD5E1")
      graphics::abline(h = graphics::axTicks(2), col = "#E9EDF4")
      
      graphics::lines(
        d$Period[actual_rows],
        d$Actual[actual_rows],
        type = "o", pch = 16, col = "#334155", lwd = 2
      )
      
      graphics::lines(
        d$Period[forecast_rows],
        d$Forecast[forecast_rows],
        type = "o", pch = 17,
        col = "#8B5CF6", lty = 2, lwd = 2
      )
      
      graphics::legend(
        "topleft",
        legend = c("Actual", "Forecast"),
        col = c("#334155", "#8B5CF6"),
        lty = c(1, 2), pch = c(16, 17), lwd = 2, bty = "n"
      )
      
      graphics::box(col = "#CBD5E1")
    })
    
    
    output$forecast_table <- shiny::renderTable({
      shown <- results_only()
      
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
    
    
    output$seasonal_table <- shiny::renderTable({
      obj <- forecast_data()
      factors <- obj$seasonal_factors
      
      shiny::req(factors)
      
      if (input$method == "SEASONAL") {
        averages <- obj$seasonal_averages
        
        return(data.frame(
          `Seasonal Position` = seq_along(factors),
          `Average Observation` = round(averages, 3),
          `Seasonal Effect` = round(factors, 4),
          check.names = FALSE
        ))
      }
      
      if (input$method == "HW") {
        return(data.frame(
          `Seasonal Position` = seq_along(factors),
          `Seasonal Component` = round(factors, 4),
          check.names = FALSE
        ))
      }
      
      data.frame(
        `Seasonal Position` = seq_along(factors),
        `Seasonal Component` = round(factors, 4),
        check.names = FALSE
      )
    }, striped = TRUE, bordered = FALSE, rownames = FALSE)
    
    
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("stationary_series_", Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(
          results_only(),
          file,
          row.names = FALSE,
          na = ""
        )
      }
    )
    
  })
}
