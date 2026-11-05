
library(shiny)

# Load all modules
source(file.path("R", "ogrenme.R"))
source(file.path("R", "moving_average.R"))
source(file.path("R", "stationary_series.R"))
source(file.path("R", "trend.R"))
source(file.path("R", "time_series.R"))
source(file.path("R", "data_center.R"))

# Assignment 3: aggregate planning modules (use a separate planning dataset)
source(file.path("R", "aggregate_data.R"))
source(file.path("R", "aggregate_heuristic.R"))
source(file.path("R", "aggregate_lp.R"))
source(file.path("R", "aggregate_transport.R"))
source(file.path("R", "aggregate_disaggregation.R"))

# Initial shared dataset
initial_data <- c(
  120, 126, 123, 132, 138,
  135, 145, 149, 144, 155,
  161, 158, 169, 174, 171,
  181, 188, 185, 196, 201
)

ui <- fluidPage(
  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    tags$style(HTML("
      :root {
        --navy: #111827;
        --navy-2: #1E293B;
        --lavender: #A78BFA;
        --background: #F3F5F9;
        --white: #FFFFFF;
        --text: #1F2937;
        --muted: #64748B;
        --border: #E2E8F0;
      }

      * { box-sizing: border-box; }

      body {
        margin: 0;
        background: var(--background);
        color: var(--text);
        font-family: 'Segoe UI', Arial, sans-serif;
        font-size: 14px;
      }

      .container-fluid {
        width: 100%;
        padding: 0;
      }

      .sidebar {
        position: fixed;
        top: 0;
        left: 0;
        bottom: 0;
        width: 250px;
        padding: 30px 18px;
        background: linear-gradient(180deg, #111827, #1E293B);
        color: white;
        z-index: 1000;
        overflow-y: auto;
      }

      .brand {
        font-size: 21px;
        font-weight: 800;
        letter-spacing: 1.2px;
        padding: 0 12px 5px;
      }

      .brand-subtitle {
        color: #A78BFA;
        font-size: 10px;
        letter-spacing: 2px;
        padding: 0 12px 30px;
      }

      .menu-label {
        color: #94A3B8;
        font-size: 10px;
        font-weight: 700;
        letter-spacing: 2px;
        padding: 0 12px 12px;
      }

      .sidebar .nav > li > a {
        color: #CBD5E1;
        padding: 13px 14px;
        margin: 5px 0;
        border-radius: 10px;
        font-size: 13px;
      }

      .sidebar .nav > li > a:hover {
        color: white;
        background: rgba(167,139,250,0.12);
      }

      .main-area {
        margin-left: 250px;
        padding: 30px;
      }

      .page-header {
        margin-bottom: 24px;
      }

      .page-header h2 {
        color: #111827;
        font-weight: 800;
      }

      .top-tag {
        display: inline-block;
        color: #6D28D9;
        background: #EDE9FE;
        padding: 6px 10px;
        border-radius: 6px;
        font-size: 10px;
        font-weight: 800;
        letter-spacing: 1px;
      }

      .page-description {
        color: #64748B;
        line-height: 1.7;
      }

      .welcome-card,
      .content-panel,
      .module-card {
        background: white;
        border: 1px solid #E2E8F0;
        border-radius: 14px;
        padding: 22px;
        margin-bottom: 20px;
        box-shadow: 0 4px 14px rgba(15,23,42,.04);
      }

      .welcome-card {
        border-left: 4px solid #A78BFA;
      }

      .module-card h4,
      .content-panel h4 {
        color: #1E293B;
        font-weight: 750;
      }

      .module-card p,
      .content-panel p {
        color: #64748B;
        line-height: 1.7;
      }

      .module-icon {
        color: #8B5CF6;
        font-size: 24px;
        margin-bottom: 12px;
      }

      .section-title {
        color: #1E293B;
        font-weight: 750;
        margin: 25px 0 16px;
      }

      .form-control {
        border: 1px solid #CBD5E1;
        border-radius: 8px;
        box-shadow: none;
      }

      .btn-primary {
        background: #1E293B;
        border-color: #1E293B;
        border-radius: 8px;
      }

      .btn-primary:hover {
        background: #334155;
        border-color: #334155;
      }

      @media(max-width: 800px) {
        .sidebar {
          position: relative;
          width: 100%;
          padding: 18px;
        }

        .brand-subtitle {
          padding-bottom: 18px;
        }

        .main-area {
          margin-left: 0;
          padding: 20px 15px;
        }
      }
    "))
  ),
  
  div(
    class = "sidebar",
    div(class = "brand", "PRODUCTION"),
    div(class = "brand-subtitle", "PLANNING & ANALYTICS"),
    div(class = "menu-label", "FORECASTING"),
    
    tags$ul(
      class = "nav nav-pills nav-stacked",
      
      tags$li(tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('active_page','overview',{priority:'event'}); return false;",
        "Overview"
      )),
      
      tags$li(tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('active_page','learning',{priority:'event'}); return false;",
        "Learning Curve"
      )),
      
      tags$li(tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('active_page','moving',{priority:'event'}); return false;",
        "Moving Average"
      )),
      
      tags$li(tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('active_page','stationary',{priority:'event'}); return false;",
        "Stationary Series"
      )),
      
      tags$li(tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('active_page','trend',{priority:'event'}); return false;",
        "Trend Forecasting"
      )),
      
      tags$li(tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('active_page','time_series',{priority:'event'}); return false;",
        "Time Series Analysis"
      )),
      
      tags$li(tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('active_page','data_center',{priority:'event'}); return false;",
        "Data Center"
      )),

      # Assignment 3 — separate group in the same sidebar
      tags$li(tags$div(
        class = "menu-label",
        style = "padding-top:22px; padding-bottom:6px;",
        "AGGREGATE PLANNING"
      )),
      tags$li(tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('active_page','aggregate_data',{priority:'event'}); return false;",
        "Aggregate Data Center"
      )),
      tags$li(tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('active_page','aggregate_heuristic',{priority:'event'}); return false;",
        "Heuristic Plans"
      )),
      tags$li(tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('active_page','aggregate_lp',{priority:'event'}); return false;",
        "Linear Programming"
      )),
      tags$li(tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('active_page','aggregate_transport',{priority:'event'}); return false;",
        "Transportation Method"
      )),
      tags$li(tags$a(
        href = "#",
        onclick = "Shiny.setInputValue('active_page','aggregate_disaggregation',{priority:'event'}); return false;",
        "Disaggregation"
      ))
    )
  ),
  
  div(
    class = "main-area",
    uiOutput("page_content")
  )
)

server <- function(input, output, session) {
  
  shared_data <- reactiveVal(initial_data)
  # Assignment 3 data is independent from forecasting observations.
  shared_aggregate <- reactiveVal(NULL)
  current_page <- reactiveVal("overview")
  
  observeEvent(input$active_page, {
    current_page(input$active_page)
  }, ignoreInit = TRUE)
  
  # Connect the central data module
  data_center_server("data_center", shared_data)
  aggregate_data_server("aggregate_data", shared_aggregate)
  
  output$page_content <- renderUI({
    
    page <- current_page()
    
    if (page == "overview") {
      
      tagList(
        div(
          class = "page-header",
          div(class = "top-tag", "PRODUCTION ANALYTICS"),
          h2("Forecasting Overview"),
          p(
            class = "page-description",
            "Analyze production patterns, explore forecasting methods, and evaluate prediction accuracy."
          )
        ),
        
        div(
          class = "welcome-card",
          h3("Turn Data Into Better Decisions"),
          p("Enter your historical observations once in Data Center and use them across forecasting modules.")
        ),
        
        h3(class = "section-title", "Forecasting Modules"),
        
        fluidRow(
          column(4, div(
            class = "module-card",
            div(class = "module-icon", "◇"),
            h4("Learning Curve"),
            p("Explore production time and cumulative experience.")
          )),
          column(4, div(
            class = "module-card",
            div(class = "module-icon", "▦"),
            h4("Moving Average"),
            p("Generate forecasts using recent observations.")
          )),
          column(4, div(
            class = "module-card",
            div(class = "module-icon", "✦"),
            h4("Stationary Series"),
            p("Compare forecasting methods for stationary series.")
          ))
        ),
        
        div(
          class = "content-panel",
          h4("Getting Started"),
          p("Choose a module from the sidebar to begin.")
        ),

        h3(class = "section-title", "Aggregate Planning"),
        div(
          class = "welcome-card",
          h4("Plan Production, Workforce, and Inventory"),
          p("Start with Aggregate Data Center to upload period demand and working days. ",
            "Then compare Heuristic Plans, Linear Programming, Transportation Method, ",
            "and Disaggregation using the same planning data.")
        )
      )
      
    } else if (page == "learning") {
      
      learning_ui("learning")
      
    } else if (page == "moving") {
      
      moving_average_ui("moving_average")
      
    } else if (page == "stationary") {
      
      stationary_ui("stationary")
      
    } else if (page == "trend") {
      
      trend_ui("trend")
      
    } else if (page == "time_series") {
      
      time_series_ui("time_series")
      
    } else if (page == "data_center") {

      data_center_ui("data_center")

    } else if (page == "aggregate_data") {

      aggregate_data_ui("aggregate_data")

    } else if (page == "aggregate_heuristic") {

      aggregate_heuristic_ui("aggregate_heuristic")

    } else if (page == "aggregate_lp") {

      aggregate_lp_ui("aggregate_lp")

    } else if (page == "aggregate_transport") {

      aggregate_transport_ui("aggregate_transport")

    } else if (page == "aggregate_disaggregation") {

      aggregate_disaggregation_ui("aggregate_disaggregation")

    }
  })
  
  # All forecasting modules use the same shared dataset
  learning_server("learning")
  moving_average_server("moving_average", shared_data)
  stationary_server("stationary", shared_data)
  trend_server("trend", shared_data)
  time_series_server("time_series", shared_data)

  # Assignment 3 methods share the same aggregate-planning inputs.
  aggregate_heuristic_server("aggregate_heuristic", shared_aggregate)
  aggregate_lp_server("aggregate_lp", shared_aggregate)
  aggregate_transport_server("aggregate_transport", shared_aggregate)
  aggregate_disaggregation_server("aggregate_disaggregation", shared_aggregate)
}

shinyApp(ui = ui, server = server)
