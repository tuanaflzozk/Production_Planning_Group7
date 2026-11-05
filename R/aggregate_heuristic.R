# Aggregate Planning: Heuristic Plans (Assignment 3)
# Uses the data list supplied by aggregate_data_server():
# list(periods = data.frame(period, demand, working_days),
#      plant = c(start_workers, start_inventory, end_inventory),
#      costs = c(hiring, firing, holding, regular, overtime, subcontract),
#      k = units per worker per working day)
# No additional R packages are required beyond shiny.

# All computations are pure R functions so they can be checked independently.
aggregate_heuristic_calculate <- function(data, method = "chase",
                                          change_periods = integer(),
                                          whole_workers = FALSE) {
  if (is.null(data) || !is.list(data)) stop("Apply aggregate planning data first.")
  p <- data$periods
  if (is.null(p) || !is.data.frame(p) || !all(c("period", "demand", "working_days") %in% names(p))) {
    stop("Planning data must contain period, demand, and working_days.")
  }
  if (!nrow(p)) stop("At least one planning period is needed.")
  demand <- as.numeric(p$demand)
  days <- as.numeric(p$working_days)
  k <- as.numeric(data$k)
  start_workers <- as.numeric(data$plant[["start_workers"]])
  start_inv <- as.numeric(data$plant[["start_inventory"]])
  target_inv <- as.numeric(data$plant[["end_inventory"]])
  cst <- data$costs
  needed_costs <- c("hiring", "firing", "holding", "regular")
  if (any(!needed_costs %in% names(cst))) stop("Required cost inputs are missing.")
  if (length(k) != 1 || !is.finite(k) || k <= 0 ||
      any(!is.finite(demand)) || any(demand < 0) ||
      any(!is.finite(days)) || any(days <= 0) ||
      any(!is.finite(c(start_workers, start_inv, target_inv))) ||
      any(c(start_workers, start_inv, target_inv) < 0) ||
      any(!is.finite(cst[needed_costs])) || any(cst[needed_costs] < 0)) {
    stop("Check demand, working days, productivity, workforce, inventory and costs.")
  }
  n <- length(demand)
  cap <- k * days
  method <- match.arg(method, c("chase", "level", "mixed"))
  required_total <- sum(demand) + target_inv - start_inv
  workers <- rep(0, n)

  # Each plan is a non-backlogging, regular-time-only heuristic.
  # Chase: meet each period's demand, and the terminal stock target in the final period.
  if (method == "chase") {
    inv <- start_inv
    for (i in seq_len(n)) {
      production_needed <- max(0, demand[i] - inv + if (i == n) target_inv else 0)
      workers[i] <- production_needed / cap[i]
      inv <- inv + production_needed - demand[i]
    }
  }

  # Level: minimum constant workforce satisfying EVERY period's cumulative
  # demand, with final target stock included at the horizon endpoint.
  if (method == "level") {
    running_need <- cumsum(demand) - start_inv
    running_need[n] <- running_need[n] + target_inv
    workers[] <- max(0, running_need / cumsum(cap))
  }

  # Mixed: workforce is constant within each user-selected block.
  # The selected period is the FIRST period at the new workforce level.
  if (method == "mixed") {
    change_periods <- sort(unique(as.integer(change_periods)))
    change_periods <- change_periods[!is.na(change_periods)]
    if (any(change_periods < 2 | change_periods > n)) {
      stop("Workforce change periods must be between period 2 and the last period.")
    }
    starts <- c(1L, change_periods)
    ends <- c(change_periods - 1L, n)
    inv <- start_inv
    for (j in seq_along(starts)) {
      idx <- starts[j]:ends[j]
      need <- cumsum(demand[idx]) - inv
      if (ends[j] == n) need[length(need)] <- need[length(need)] + target_inv
      w <- max(0, need / cumsum(cap[idx]))
      workers[idx] <- w
      inv <- inv + sum(cap[idx] * w) - sum(demand[idx])
    }
  }

  # Rounding UP the workforce is not a rounding of the displayed output:
  # production, inventory, workforce movements, and costs are recomputed.
  if (isTRUE(whole_workers)) workers <- ceiling(pmax(0, workers) - 1e-10)

  hired <- pmax(0, workers - c(start_workers, head(workers, -1)))
  fired <- pmax(0, c(start_workers, head(workers, -1)) - workers)
  production <- workers * cap
  inventory <- start_inv + cumsum(production - demand)
  if (any(inventory < -1e-7)) {
    stop("Internal planning error: the plan would create a backlog.")
  }
  if (tail(inventory, 1) + 1e-7 < target_inv) {
    stop("Internal planning error: ending inventory target is unmet.")
  }
  inventory <- pmax(0, inventory)
  hiring_cost <- hired * unname(cst[["hiring"]])
  firing_cost <- fired * unname(cst[["firing"]])
  regular_cost <- production * unname(cst[["regular"]])
  holding_cost <- inventory * unname(cst[["holding"]])
  table <- data.frame(
    Period = as.character(p$period),
    Demand = demand,
    Working_Days = days,
    Workers = workers,
    Hired = hired,
    Fired = fired,
    Regular_Production = production,
    Overtime = rep(0, n),
    Subcontracting = rep(0, n),
    Ending_Inventory = inventory,
    Hiring_Cost = hiring_cost,
    Firing_Cost = firing_cost,
    Regular_Cost = regular_cost,
    Holding_Cost = holding_cost,
    Total_Period_Cost = hiring_cost + firing_cost + regular_cost + holding_cost,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  costs <- c(
    Hiring = sum(hiring_cost),
    Firing = sum(firing_cost),
    Holding = sum(holding_cost),
    `Regular Time` = sum(regular_cost),
    Overtime = 0,
    Subcontracting = 0
  )
  list(
    plan = table,
    costs = costs,
    total_cost = sum(costs),
    cumulative_production = cumsum(production),
    cumulative_net_demand = cumsum(demand) - start_inv,
    ending_inventory = tail(inventory, 1),
    target_inventory = target_inv,
    method = method,
    whole_workers = whole_workers
  )
}

aggregate_heuristic_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Local styles: only the Heuristic Plans page is affected.
    shiny::tags$style(shiny::HTML("
      .heuristic-results .col-sm-6 { min-width: 0; }
      .heuristic-results .content-panel { min-width: 0; max-width: 100%; }
      .heuristic-table-scroll {
        display: block; width: 100%; max-width: 100%;
        overflow-x: auto; overflow-y: hidden;
        border: 1px solid #E2E8F0; border-radius: 10px;
        background: #FFFFFF; margin: 12px 0 18px;
      }
      .heuristic-table-scroll .shiny-html-output { max-width: 100%; }
      .heuristic-table-scroll table {
        width: max-content !important; min-width: 100%;
        margin-bottom: 0; font-size: 12px;
      }
      .heuristic-table-scroll table th,
      .heuristic-table-scroll table td {
        padding: 9px 10px !important; white-space: nowrap;
        vertical-align: middle;
      }
      .heuristic-table-scroll table thead th {
        background: #F1EDFF; color: #334155; font-weight: 700;
      }
      .heuristic-table-scroll table tbody tr:nth-child(even) {
        background: #F8FAFC;
      }
      .heuristic-plan-note { color: #64748B; font-size: 12px; margin-top: 3px; }
      .heuristic-details { margin: 0 0 18px; }
      .heuristic-details summary {
        color: #6D28D9; font-weight: 600; cursor: pointer;
        padding: 6px 0; font-size: 12px;
      }
      .heuristic-total {
        border-top: 1px solid #E2E8F0; margin-top: 14px;
        padding-top: 14px; color: #1E293B; font-weight: 700;
      }
      .heuristic-results .content-panel h4 { line-height: 1.4; }
    ")),
    shiny::div(class = "page-header",
      shiny::div(class = "top-tag", "AGGREGATE PLANNING"),
      shiny::h2("Heuristic Plans"),
      shiny::p(class = "page-description",
        "Compare chase, level, and user-defined mixed workforce plans using the common Aggregate Planning Data Center.")),
    shiny::div(class = "content-panel",
      shiny::h4("1. Select Planning Method"),
      shiny::selectInput(ns("method"), "Heuristic Method",
        choices = c("Zero-Inventory (Chase) Plan" = "chase",
                    "Constant-Workforce (Level) Plan" = "level",
                    "Mixed Plan (Selected Workforce Change Periods)" = "mixed")),
      shiny::uiOutput(ns("mixed_controls")),
      shiny::actionButton(ns("calculate"), "Calculate Heuristic Plan", class = "btn-primary"),
      shiny::p(style = "margin-top:12px;",
        "Fractional-worker and rounded-up whole-worker plans are recalculated and shown side by side."),
      shiny::verbatimTextOutput(ns("status"))
    ),
    shiny::div(class = "heuristic-results", shiny::fluidRow(
      shiny::column(6, shiny::div(class = "content-panel",
        shiny::h4("2. Fractional-Worker Plan"),
        shiny::p(class = "heuristic-plan-note",
          "Key results by period. Scroll within the table if needed."),
        shiny::div(class = "heuristic-table-scroll",
          shiny::tableOutput(ns("fractional_plan"))),
        shiny::tags$details(class = "heuristic-details",
          shiny::tags$summary("View all planning and cost columns"),
          shiny::div(class = "heuristic-table-scroll",
            shiny::tableOutput(ns("fractional_full_plan")))
        ),
        shiny::h4("Cost Breakdown"),
        shiny::tableOutput(ns("fractional_costs")),
        shiny::div(class = "heuristic-total",
          shiny::textOutput(ns("fractional_total")))
      )),
      shiny::column(6, shiny::div(class = "content-panel",
        shiny::h4("3. Whole-Worker Plan (Rounded Up)"),
        shiny::p(class = "heuristic-plan-note",
          "The workforce, production, inventory and costs are recalculated."),
        shiny::div(class = "heuristic-table-scroll",
          shiny::tableOutput(ns("whole_plan"))),
        shiny::tags$details(class = "heuristic-details",
          shiny::tags$summary("View all planning and cost columns"),
          shiny::div(class = "heuristic-table-scroll",
            shiny::tableOutput(ns("whole_full_plan")))
        ),
        shiny::h4("Cost Breakdown"),
        shiny::tableOutput(ns("whole_costs")),
        shiny::div(class = "heuristic-total",
          shiny::textOutput(ns("whole_total")))
      ))
    )),
    shiny::div(class = "content-panel",
      shiny::h4("4. Cumulative Production vs. Cumulative Net Demand"),
      shiny::plotOutput(ns("comparison_plot"), height = "390px"),
      shiny::p("Cumulative net demand = cumulative demand minus starting inventory. ",
        "A separate ending-inventory target is also enforced in each plan. ",
        "The whole-worker plan may create surplus ending inventory because workers are rounded upward.")
    ),
    shiny::div(class = "content-panel",
      shiny::h4("Model Notes"),
      shiny::p("These heuristics use regular-time production only: production = workers × K × working days. ",
        "Overtime and subcontracting quantities and costs are zero here; they belong to the LP and transportation methods."),
      shiny::p("Chase meets demand each period using available inventory; Level chooses the smallest constant workforce that prevents shortages; ",
        "Mixed holds workforce constant within user-defined segments. No backorders are allowed."),
      shiny::p("All costs use the values in Aggregate Planning Data Center. Holding cost is charged on each period's ending inventory, including the final period.")
    )
  )
}

aggregate_heuristic_server <- function(id, shared_aggregate) {
  shiny::moduleServer(id, function(input, output, session) {
    output$mixed_controls <- shiny::renderUI({
      if (!identical(input$method, "mixed")) return(NULL)
      d <- shared_aggregate()
      if (is.null(d)) return(shiny::p("First apply your data in Aggregate Planning Data Center."))
      periods <- as.character(d$periods$period)
      if (length(periods) <= 1) return(shiny::p("Only one period is available; no workforce change period can be chosen."))
      shiny::checkboxGroupInput(session$ns("change_periods"),
        "Periods where a new workforce level begins (select any):",
        choices = stats::setNames(as.character(seq.int(2, length(periods))),
                                  paste0(seq.int(2, length(periods)), ": ", periods[-1])))
    })

    result <- shiny::eventReactive(input$calculate, {
      d <- shared_aggregate()
      if (is.null(d)) stop("First upload and apply data in Aggregate Planning Data Center.")
      cp <- if (identical(input$method, "mixed")) as.integer(input$change_periods) else integer()
      list(
        fractional = aggregate_heuristic_calculate(d, input$method, cp, FALSE),
        whole = aggregate_heuristic_calculate(d, input$method, cp, TRUE)
      )
    }, ignoreInit = TRUE)

    output$status <- shiny::renderText({
      if (is.null(shared_aggregate())) return("First apply data in Aggregate Planning Data Center.")
      if (!isTRUE(input$calculate > 0)) return("Ready. Click Calculate Heuristic Plan.")
      "Results are based on the last calculation. Recalculate after changing input data or the method."
    })

    format_table <- function(x) {
      numeric_cols <- vapply(x, is.numeric, logical(1))
      x[numeric_cols] <- lapply(x[numeric_cols], function(y) round(y, 2))
      x
    }
    # Keep the primary comparison concise; the full original plan is accessible
    # under "View all planning and cost columns" without losing any fields.
    compact_plan <- function(full_plan) {
      x <- full_plan[c("Period", "Demand", "Workers", "Hired", "Fired",
                       "Regular_Production", "Ending_Inventory", "Total_Period_Cost")]
      names(x) <- c("Period", "Demand", "Workers", "Hired", "Fired",
                    "Production", "Inventory", "Period Cost")
      format_table(x)
    }
    output$fractional_plan <- shiny::renderTable({
      compact_plan(result()$fractional$plan)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    output$whole_plan <- shiny::renderTable({
      compact_plan(result()$whole$plan)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    output$fractional_full_plan <- shiny::renderTable({
      format_table(result()$fractional$plan)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    output$whole_full_plan <- shiny::renderTable({
      format_table(result()$whole$plan)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    cost_table <- function(res) data.frame(
      `Cost Item` = names(res$costs),
      `Amount` = round(unname(res$costs), 2),
      check.names = FALSE
    )
    output$fractional_costs <- shiny::renderTable(cost_table(result()$fractional), striped = TRUE)
    output$whole_costs <- shiny::renderTable(cost_table(result()$whole), striped = TRUE)
    output$fractional_total <- shiny::renderText({
      r <- result()$fractional
      sprintf("Total Cost: %.2f | Ending Inventory: %.2f (minimum required: %.2f)",
              r$total_cost, r$ending_inventory, r$target_inventory)
    })
    output$whole_total <- shiny::renderText({
      r <- result()$whole
      sprintf("Total Cost: %.2f | Ending Inventory: %.2f (minimum required: %.2f)",
              r$total_cost, r$ending_inventory, r$target_inventory)
    })
    output$comparison_plot <- shiny::renderPlot({
      r <- result()
      x <- seq_len(nrow(r$fractional$plan))
      y1 <- r$fractional$cumulative_production
      y2 <- r$whole$cumulative_production
      y3 <- r$fractional$cumulative_net_demand
      yr <- range(c(0, y1, y2, y3), finite = TRUE)
      graphics::plot(x, y1, type = "o", col = "#A78BFA", pch = 16, lwd = 2,
                     xlab = "Planning Period", ylab = "Cumulative Units",
                     xaxt = "n", ylim = yr, main = "Cumulative Production and Net Demand")
      graphics::axis(1, at = x, labels = r$fractional$plan$Period, las = 2,
                     cex.axis = if (length(x) > 9) 0.7 else 0.9)
      graphics::lines(x, y2, type = "o", col = "#1E293B", pch = 17, lwd = 2)
      graphics::lines(x, y3, type = "o", col = "#64748B", pch = 15, lwd = 2, lty = 2)
      graphics::legend("topleft",
        legend = c("Fractional: Cumulative Production", "Whole: Cumulative Production",
                   "Cumulative Net Demand"),
        col = c("#A78BFA", "#1E293B", "#64748B"),
        pch = c(16, 17, 15), lty = c(1, 1, 2), lwd = 2, cex = 0.82, bty = "n")
    })
  })
}
