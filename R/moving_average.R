library(shiny)

moving_average_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      fileInput(ns("file"), "Upload an Excel file (.xlsx)", accept = c(".xlsx")),
      selectInput(ns("col_demand"), "Demand column", choices = NULL),
      radioButtons(ns("n_choice_type"), "How should N be chosen?",
                   choices = c("I will choose N" = "manual", "Find the best N" = "auto")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'manual'", ns("n_choice_type")),
        numericInput(ns("n_val"), "N (number of past periods)", value = 3, min = 1, step = 1)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'auto'", ns("n_choice_type")),
        numericInput(ns("max_n"), "Maximum N to evaluate", value = 5, min = 2, step = 1),
        selectInput(ns("kriter"), "Best error measure", choices = c("MAD", "MSE", "MAPE"))
      )
    ),
    mainPanel(
      h4(textOutput(ns("forecast_text"))),
      plotOutput(ns("forecast_plot")),
      br(),
      h4("Error measures"),
      tableOutput(ns("error_table")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'auto'", ns("n_choice_type")),
        h4("Evaluation of all N values"),
        plotOutput(ns("bar_plot")),
        tableOutput(ns("n_tablo"))
      ),
      br(),
      h4("Forecasts by period"),
      tableOutput(ns("donem_tablo"))
    )
  )
}

moving_average_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    excel_data <- reactive({
      req(input$file)
      ext <- tools::file_ext(input$file$datapath)
      if (ext != "xlsx") return(NULL)
      readxl::read_excel(input$file$datapath)
    })
    
    observeEvent(excel_data(), {
      df <- excel_data()
      req(df)
      updateSelectInput(session, "col_demand", choices = names(df), selected = names(df)[1])
    })
    
    calc_ma <- function(demand, N) {
      n <- length(demand)
      forecast <- rep(NA, n + 1)
      if (N < n) {
        for (t in (N + 1):n) {
          forecast[t] <- mean(demand[(t - N):(t - 1)])
        }
        forecast[n + 1] <- mean(demand[(n - N + 1):n])
      }
      
      eval_idx <- (N + 1):n
      err <- forecast[eval_idx] - demand[eval_idx]
      
      mad <- mean(abs(err), na.rm = TRUE)
      mse <- mean(err^2, na.rm = TRUE)
      mape <- mean(abs(err / demand[eval_idx]), na.rm = TRUE) * 100
      
      list(forecast = forecast, err = err, mad = mad, mse = mse, mape = mape,
           eval_range = paste(N + 1, n, sep = "–"), demand = demand, N = N)
    }
    
    arama <- reactive({
      df <- excel_data()
      req(df, input$col_demand)
      d <- as.numeric(df[[input$col_demand]])
      max_n <- min(input$max_n, length(d) - 1)
      
      res <- data.frame(N = integer(), MAD = numeric(), MSE = numeric(), MAPE = numeric())
      for (k in 1:max_n) {
        m <- calc_ma(d, k)
        res <- rbind(res, data.frame(N = k, MAD = m$mad, MSE = m$mse, MAPE = m$mape))
      }
      res
    })
    
    secilen_N <- reactive({
      if (input$n_choice_type == "manual") {
        return(input$n_val)
      } else {
        tablo <- arama()
        req(nrow(tablo) > 0)
        crit <- input$kriter
        tablo$N[which.min(tablo[[crit]])]
      }
    })
    
    sonuc <- reactive({
      df <- excel_data()
      req(df, input$col_demand)
      d <- as.numeric(df[[input$col_demand]])
      N <- secilen_N()
      req(N)
      calc_ma(d, N)
    })
    
    output$forecast_text <- renderText({
      s <- sonuc()
      req(s)
      next_f <- s$forecast[length(s$forecast)]
      paste0("MA(", s$N, ") — next-period forecast: ", round(next_f, 2))
    })
    
    output$forecast_plot <- renderPlot({
      s <- sonuc()
      req(s)
      n <- length(s$demand)
      all_vals <- c(s$demand, s$forecast)
      ylim <- range(all_vals, na.rm = TRUE)
      
      plot(1:n, s$demand, type = "b", pch = 16, col = "gray40",
           ylim = ylim, xlim = c(1, n + 1), xlab = "Period", ylab = "Demand",
           main = paste0("Actual demand and MA(", s$N, ") forecast"))
      lines(1:(n + 1), s$forecast, type = "b", pch = 17, col = "#2b6cb0", lty = 2)
      points(n + 1, s$forecast[n + 1], pch = 17, col = "#c53030", cex = 1.5)
      legend("topleft", legend = c("Actual demand", "Forecast", "Next-period forecast"),
             col = c("gray40", "#2b6cb0", "#c53030"), pch = c(16, 17, 17), lty = c(1, 2, NA), bty = "n")
    })
    
    output$bar_plot <- renderPlot({
      req(input$n_choice_type == "auto")
      tablo <- arama()
      k <- input$kriter
      renk <- ifelse(tablo$N == secilen_N(), "#B4501E", "#9DB4CF")
      barplot(tablo[[k]], names.arg = tablo$N, col = renk, border = NA,
              xlab = "N", ylab = k,
              main = paste0(k, " değerine göre en iyi N = ", secilen_N()))
    })
    
    output$error_table <- renderTable({
      s <- sonuc()
      req(s)
      data.frame(
        Measure = c("MAD", "MSE", "MAPE (%)"),
        Value = c(round(s$mad, 2), round(s$mse, 2), round(s$mape, 2)),
        "Periods evaluated" = rep(s$eval_range, 3),
        check.names = FALSE
      )
    })
    
    output$n_tablo <- renderTable({
      req(input$n_choice_type == "auto")
      tablo <- arama()
      tablo$MAD <- round(tablo$MAD, 3)
      tablo$MSE <- round(tablo$MSE, 3)
      tablo$MAPE <- round(tablo$MAPE, 3)
      tablo$N <- as.integer(tablo$N)
      tablo
    })
    
    output$donem_tablo <- renderTable({
      s <- sonuc()
      req(s)
      n <- length(s$demand)
      f_sub <- s$forecast[1:n]
      err <- f_sub - s$demand
      data.frame(
        "Period" = 1:(n + 1),
        "Demand (D)" = c(round(s$demand, 2), NA),
        "Forecast (F)" = round(s$forecast, 2),
        "Error (e = F - D)" = c(round(err, 2), NA),
        check.names = FALSE
      )
    })
  })
}