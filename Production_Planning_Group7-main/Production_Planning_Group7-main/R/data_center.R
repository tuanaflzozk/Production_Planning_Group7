
data_center_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .dc-header {
        background: #14213D;
        color: white;
        padding: 24px;
        border-radius: 16px;
        margin-bottom: 20px;
      }
      .dc-card {
        background: white;
        border: 1px solid #E2E8F0;
        border-radius: 14px;
        padding: 22px;
        margin-bottom: 20px;
      }
      .dc-note {
        color: #64748B;
        font-size: 13px;
        line-height: 1.6;
      }
    ")),
    
    shiny::div(
      class = "dc-header",
      shiny::h2("Data Center"),
      shiny::p("Enter your historical observations once and share them across forecasting modules.")
    ),
    
    shiny::div(
      class = "dc-card",
      shiny::h4("Historical Data"),
      shiny::p(
        class = "dc-note",
        "Enter numeric observations separated by commas, spaces, semicolons, or new lines."
      ),
      
      shiny::textAreaInput(
        ns("data"),
        "Observations",
        value = paste(
          c(120, 126, 123, 132, 138, 135, 145, 149, 144, 155,
            161, 158, 169, 174, 171, 181, 188, 185, 196, 201),
          collapse = ", "
        ),
        rows = 6,
        width = "100%"
      ),
      
      shiny::fileInput(
        ns("csv"),
        "Or upload a CSV file",
        accept = ".csv"
      ),
      
      shiny::p(
        class = "dc-note",
        "For CSV files, the first column should contain numeric observations."
      ),
      
      shiny::actionButton(
        ns("sample"),
        "Load Sample Data"
      ),
      
      shiny::actionButton(
        ns("apply"),
        "Apply Shared Data",
        class = "btn-primary"
      ),
      
      shiny::br(),
      shiny::br(),
      
      shiny::uiOutput(ns("status")),
      shiny::uiOutput(ns("summary")),
      shiny::tableOutput(ns("preview"))
    )
  )
}


data_center_server <- function(id, shared_data) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    sample_values <- c(
      120, 126, 123, 132, 138,
      135, 145, 149, 144, 155,
      161, 158, 169, 174, 171,
      181, 188, 185, 196, 201
    )
    
    shiny::observeEvent(input$sample, {
      shiny::updateTextAreaInput(
        session,
        "data",
        value = paste(sample_values, collapse = ", ")
      )
      shiny::updateTextInput(session, "unused", value = "")
    })
    
    candidate <- shiny::reactive({
      
      if (!is.null(input$csv)) {
        
        df <- tryCatch(
          utils::read.csv(
            input$csv$datapath,
            header = TRUE,
            check.names = FALSE
          ),
          error = function(e) NULL
        )
        
        if (is.null(df) || ncol(df) < 1) {
          return(list(
            ok = FALSE,
            message = "CSV could not be read."
          ))
        }
        
        values <- suppressWarnings(
          as.numeric(as.character(df[[1]]))
        )
        
        values <- values[!is.na(values)]
        
      } else {
        
        raw <- gsub(
          "[,;[:space:]]+",
          " ",
          input$data
        )
        
        pieces <- trimws(
          unlist(strsplit(raw, "\\s+"))
        )
        
        pieces <- pieces[nzchar(pieces)]
        values <- suppressWarnings(as.numeric(pieces))
      }
      
      if (length(values) < 3) {
        return(list(
          ok = FALSE,
          message = "Enter at least 3 numeric observations."
        ))
      }
      
      if (any(!is.finite(values))) {
        return(list(
          ok = FALSE,
          message = "Check your data: all observations must be numeric."
        ))
      }
      
      list(
        ok = TRUE,
        values = values,
        message = NULL
      )
    })
    
    
    output$status <- shiny::renderUI({
      
      d <- candidate()
      
      if (!d$ok) {
        return(
          shiny::div(
            style = "background:#FEF2F2;color:#991B1B;padding:12px;border-radius:8px;",
            d$message
          )
        )
      }
      
      shiny::div(
        style = "background:#F0FDF4;color:#166534;padding:12px;border-radius:8px;",
        paste(
          "Valid data:",
          length(d$values),
          "observations. Click Apply Shared Data to update the modules."
        )
      )
    })
    
    
    output$summary <- shiny::renderUI({
      
      d <- candidate()
      
      if (!d$ok) {
        return(NULL)
      }
      
      shiny::tagList(
        shiny::h4("Data Summary"),
        shiny::p(paste("Observations:", length(d$values))),
        shiny::p(paste("Minimum:", round(min(d$values), 2))),
        shiny::p(paste("Mean:", round(mean(d$values), 2))),
        shiny::p(paste("Maximum:", round(max(d$values), 2)))
      )
    })
    
    
    output$preview <- shiny::renderTable({
      
      d <- candidate()
      
      if (!d$ok) {
        return(data.frame(Message = d$message))
      }
      
      head(
        data.frame(
          Period = seq_along(d$values),
          Value = d$values
        ),
        10
      )
    })
    
    
    shiny::observeEvent(input$apply, {
      
      d <- candidate()
      
      if (!d$ok) {
        shiny::showNotification(
          d$message,
          type = "error"
        )
        return()
      }
      
      shared_data(d$values)
      
      shiny::showNotification(
        paste(
          length(d$values),
          "observations shared with forecasting modules."
        ),
        type = "message"
      )
    })
    
  })
}
