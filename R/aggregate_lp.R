# Aggregate Planning: Linear Programming (Assignment 3)
# Uses shared_aggregate supplied by aggregate_data_server().
# Requires: shiny, lpSolve (install.packages("lpSolve") once in RStudio Console).
# All UI text and code comments are in English.
#
# MODEL ASSUMPTIONS
# - No backorders. Ending inventory must be at least the user-entered target.
# - Regular production + idle regular capacity = workforce * K * working days.
# - Overtime cannot exceed the selected fraction of regular capacity.
# - Subcontracting has no additional capacity limit.
# - The additional idle cost entered on this page is per unused regular-capacity unit.
# - The "whole-worker" plan rounds the fractional solution's workforce UP by
#   period, then re-optimizes production/inventory with those workforce values fixed.
#   It is a rounded comparison plan, not a globally optimal integer program.

aggregate_lp_calculate <- function(data,
                                   allow_overtime = TRUE,
                                   allow_idle = TRUE,
                                   allow_subcontract = TRUE,
                                   overtime_fraction = 0.25,
                                   idle_unit_cost = 10,
                                   fixed_workers = NULL) {
  if (!requireNamespace("lpSolve", quietly = TRUE)) {
    stop("The LP solver is missing. Run install.packages('lpSolve') in the RStudio Console, then try again.")
  }
  if (is.null(data) || !is.list(data)) stop("Apply aggregate planning data first.")
  p <- data$periods
  if (!is.data.frame(p) || !all(c("period", "demand", "working_days") %in% names(p)) || nrow(p) < 1L) {
    stop("Data must include at least one period, with period, demand, and working_days columns.")
  }
  n <- nrow(p)
  demand <- as.numeric(p$demand)
  days <- as.numeric(p$working_days)
  k <- as.numeric(data$k)
  plant <- data$plant
  costs <- data$costs
  required_plant <- c("start_workers", "start_inventory", "end_inventory")
  required_costs <- c("hiring", "firing", "holding", "regular", "overtime", "subcontract")
  if (!all(required_plant %in% names(plant)) || !all(required_costs %in% names(costs))) {
    stop("Some required plant values or costs are missing from Aggregate Planning Data Center.")
  }
  start_workers <- as.numeric(plant[["start_workers"]])
  start_inv <- as.numeric(plant[["start_inventory"]])
  target_inv <- as.numeric(plant[["end_inventory"]])
  numeric_inputs <- c(demand, days, k, unname(plant[required_plant]),
                      unname(costs[required_costs]), overtime_fraction, idle_unit_cost)
  if (anyNA(numeric_inputs) || any(!is.finite(numeric_inputs)) ||
      any(demand < 0) || any(days <= 0) || length(k) != 1L || k <= 0 ||
      any(unname(plant[required_plant]) < 0) ||
      any(unname(costs[required_costs]) < 0) ||
      length(overtime_fraction) != 1L || overtime_fraction < 0 ||
      length(idle_unit_cost) != 1L || idle_unit_cost < 0) {
    stop("Check demand, days, K, workforce, inventories, costs, and LP options. All must be valid nonnegative numbers (days and K must be positive).")
  }
  capacity_per_worker <- days * k
  if (!is.null(fixed_workers)) {
    fixed_workers <- as.numeric(fixed_workers)
    if (length(fixed_workers) != n || anyNA(fixed_workers) ||
        any(!is.finite(fixed_workers)) || any(fixed_workers < 0)) {
      stop("The rounded workforce vector is invalid.")
    }
  }

  groups <- c("Workers", "Hired", "Fired", "Regular_Production",
              "Idle_Capacity", "Overtime", "Subcontracting", "Ending_Inventory")
  count <- n * length(groups)
  idx <- function(group, t) (match(group, groups) - 1L) * n + t
  objective <- numeric(count)
  for (t in seq_len(n)) {
    objective[idx("Hired", t)] <- costs[["hiring"]]
    objective[idx("Fired", t)] <- costs[["firing"]]
    objective[idx("Regular_Production", t)] <- costs[["regular"]]
    objective[idx("Idle_Capacity", t)] <- idle_unit_cost
    objective[idx("Overtime", t)] <- costs[["overtime"]]
    objective[idx("Subcontracting", t)] <- costs[["subcontract"]]
    objective[idx("Ending_Inventory", t)] <- costs[["holding"]]
  }

  constraint_rows <- list()
  constraint_dirs <- character()
  constraint_rhs <- numeric()
  add_row <- function(row, direction, value) {
    constraint_rows[[length(constraint_rows) + 1L]] <<- row
    constraint_dirs <<- c(constraint_dirs, direction)
    constraint_rhs <<- c(constraint_rhs, value)
  }
  for (t in seq_len(n)) {
    # Workforce continuity: W(t) = W(t-1) + hires(t) - fires(t).
    row <- numeric(count)
    row[idx("Workers", t)] <- 1
    row[idx("Hired", t)] <- -1
    row[idx("Fired", t)] <- 1
    if (t > 1L) row[idx("Workers", t - 1L)] <- -1
    add_row(row, "=", if (t == 1L) start_workers else 0)

    # Capacity conservation. Idle regular-time units are explicitly measured.
    row <- numeric(count)
    row[idx("Regular_Production", t)] <- 1
    row[idx("Idle_Capacity", t)] <- 1
    row[idx("Workers", t)] <- -capacity_per_worker[t]
    add_row(row, "=", 0)

    # Inventory balance: I(t) = I(t-1) + regular + overtime + sub - demand.
    row <- numeric(count)
    row[idx("Ending_Inventory", t)] <- 1
    row[idx("Regular_Production", t)] <- -1
    row[idx("Overtime", t)] <- -1
    row[idx("Subcontracting", t)] <- -1
    if (t > 1L) row[idx("Ending_Inventory", t - 1L)] <- -1
    add_row(row, "=", if (t == 1L) start_inv - demand[t] else -demand[t])

    # Overtime is limited relative to the available regular-time capacity.
    row <- numeric(count)
    row[idx("Overtime", t)] <- 1
    row[idx("Workers", t)] <- -overtime_fraction * capacity_per_worker[t]
    add_row(row, "<=", 0)

    # Method options disable the associated decision variable when unchecked.
    if (!isTRUE(allow_overtime)) {
      row <- numeric(count)
      row[idx("Overtime", t)] <- 1
      add_row(row, "=", 0)
    }
    if (!isTRUE(allow_idle)) {
      row <- numeric(count)
      row[idx("Idle_Capacity", t)] <- 1
      add_row(row, "=", 0)
    }
    if (!isTRUE(allow_subcontract)) {
      row <- numeric(count)
      row[idx("Subcontracting", t)] <- 1
      add_row(row, "=", 0)
    }
    if (!is.null(fixed_workers)) {
      row <- numeric(count)
      row[idx("Workers", t)] <- 1
      add_row(row, "=", fixed_workers[t])
    }
  }

  # Required ending inventory is a MINIMUM: rounding workers can create surplus.
  row <- numeric(count)
  row[idx("Ending_Inventory", n)] <- 1
  add_row(row, ">=", target_inv)

  matrix_constraints <- do.call(rbind, constraint_rows)
  solution <- lpSolve::lp(direction = "min", objective.in = objective,
                          const.mat = matrix_constraints,
                          const.dir = constraint_dirs,
                          const.rhs = constraint_rhs)
  if (solution$status != 0L) {
    stop(paste0("The LP solver did not find an optimal feasible plan (status ",
                solution$status, "). Check inventory targets and selected options."))
  }
  x <- as.numeric(solution$solution)
  # Remove harmless solver tolerance errors close to zero.
  x[abs(x) < 1e-7] <- 0
  get <- function(group) x[vapply(seq_len(n), function(t) idx(group, t), integer(1))]
  workers <- get("Workers")
  hired <- get("Hired")
  fired <- get("Fired")
  regular <- get("Regular_Production")
  idle <- get("Idle_Capacity")
  overtime <- get("Overtime")
  subcontract <- get("Subcontracting")
  inventory <- get("Ending_Inventory")
  total_production <- regular + overtime + subcontract

  # Independent numerical checks catch indexing and solver formulation mistakes.
  previous_workers <- c(start_workers, head(workers, -1L))
  previous_inventory <- c(start_inv, head(inventory, -1L))
  if (any(abs(workers - previous_workers - hired + fired) > 1e-5) ||
      any(abs(regular + idle - capacity_per_worker * workers) > 1e-5) ||
      any(abs(inventory - previous_inventory - total_production + demand) > 1e-5) ||
      any(inventory < -1e-5) || tail(inventory, 1) + 1e-5 < target_inv ||
      any(overtime > overtime_fraction * capacity_per_worker * workers + 1e-5)) {
    stop("The solver returned a plan that failed the balance checks.")
  }

  hired_cost <- hired * costs[["hiring"]]
  fired_cost <- fired * costs[["firing"]]
  regular_cost <- regular * costs[["regular"]]
  idle_cost <- idle * idle_unit_cost
  overtime_cost <- overtime * costs[["overtime"]]
  subcontract_cost <- subcontract * costs[["subcontract"]]
  holding_cost <- inventory * costs[["holding"]]
  period_cost <- hired_cost + fired_cost + regular_cost + idle_cost +
    overtime_cost + subcontract_cost + holding_cost
  cost_breakdown <- c(
    Hiring = sum(hired_cost),
    Firing = sum(fired_cost),
    `Regular Time` = sum(regular_cost),
    `Idle Capacity` = sum(idle_cost),
    Overtime = sum(overtime_cost),
    Subcontracting = sum(subcontract_cost),
    Holding = sum(holding_cost)
  )
  plan <- data.frame(
    Period = as.character(p$period),
    Demand = demand,
    Working_Days = days,
    Workers = workers,
    Hired = hired,
    Fired = fired,
    Regular_Production = regular,
    Idle_Capacity = idle,
    Overtime = overtime,
    Subcontracting = subcontract,
    Total_Production = total_production,
    Ending_Inventory = inventory,
    Hiring_Cost = hired_cost,
    Firing_Cost = fired_cost,
    Regular_Cost = regular_cost,
    Idle_Cost = idle_cost,
    Overtime_Cost = overtime_cost,
    Subcontracting_Cost = subcontract_cost,
    Holding_Cost = holding_cost,
    Total_Period_Cost = period_cost,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  list(
    plan = plan,
    costs = cost_breakdown,
    total_cost = sum(cost_breakdown),
    ending_inventory = tail(inventory, 1L),
    target_inventory = target_inv,
    cumulative_production = cumsum(total_production),
    cumulative_net_demand = cumsum(demand) - start_inv,
    fixed_workers = !is.null(fixed_workers)
  )
}

aggregate_lp_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "page-header",
      shiny::div(class = "top-tag", "AGGREGATE PLANNING"),
      shiny::h2("Linear Programming"),
      shiny::p(class = "page-description",
        "Minimize aggregate planning costs subject to workforce, capacity, and inventory constraints.")),
    shiny::div(class = "content-panel",
      shiny::h4("1. Linear Programming Options"),
      shiny::p("First upload and apply your planning data in Aggregate Planning Data Center."),
      shiny::fluidRow(
        shiny::column(4, shiny::checkboxInput(ns("allow_overtime"), "Allow Overtime", TRUE)),
        shiny::column(4, shiny::checkboxInput(ns("allow_idle"), "Allow Idle Time", TRUE)),
        shiny::column(4, shiny::checkboxInput(ns("allow_subcontract"), "Allow Subcontracting", TRUE))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("max_ot_fraction"),
          "Maximum Overtime / Regular Capacity", value = 0.25, min = 0, max = 2, step = 0.05)),
        shiny::column(6, shiny::numericInput(ns("idle_unit_cost"),
          "Idle Time Cost (per unused capacity unit)", value = 10, min = 0, step = 1))
      ),
      shiny::p("Example: 0.25 permits overtime production up to 25% of a period's regular capacity. ",
        "Idle time is measured in unused regular-production capacity units."),
      shiny::actionButton(ns("calculate"), "Solve LP Model", class = "btn-primary"),
      shiny::p(style = "margin-top:12px;", shiny::textOutput(ns("status")))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::div(class = "content-panel",
        shiny::h4("2. Optimal Fractional-Worker LP Plan"),
        shiny::div(style = "overflow-x:auto;", shiny::tableOutput(ns("fractional_plan"))),
        shiny::h4("Cost Breakdown"),
        shiny::tableOutput(ns("fractional_costs")),
        shiny::strong(shiny::textOutput(ns("fractional_total")))
      )),
      shiny::column(6, shiny::div(class = "content-panel",
        shiny::h4("3. Rounded-Up Whole-Worker Plan"),
        shiny::div(style = "overflow-x:auto;", shiny::tableOutput(ns("whole_plan"))),
        shiny::h4("Cost Breakdown"),
        shiny::tableOutput(ns("whole_costs")),
        shiny::strong(shiny::textOutput(ns("whole_total")))
      ))
    ),
    shiny::div(class = "content-panel",
      shiny::h4("4. Cumulative Production vs. Cumulative Net Demand"),
      shiny::plotOutput(ns("comparison_plot"), height = "400px"),
      shiny::p("Total production includes regular time, overtime, and subcontracting. ",
        "Cumulative net demand is cumulative demand minus starting inventory.")),
    shiny::div(class = "content-panel",
      shiny::h4("Model Notes"),
      shiny::p("Regular production plus idle capacity equals workforce capacity (workers x K x working days). ",
        "Hiring and firing connect workforce levels between periods. No backorders are permitted."),
      shiny::p("The ending inventory target is a minimum. Holding cost is applied to each period's ending stock."),
      shiny::p("The whole-worker comparison rounds UP the fractional LP workforce for each period and ",
        "then re-optimizes production, idle time, overtime, subcontracting, and inventory with the rounded workforce fixed. ",
        "It is not claimed to be the globally optimal integer solution."),
      shiny::p("Idle capacity has a separate per-unit cost entered on this page. ",
        "All other costs come from Aggregate Planning Data Center."))
  )
}

aggregate_lp_server <- function(id, shared_aggregate) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal("Apply planning data first, then click Solve LP Model.")
    output$status <- shiny::renderText(status())

    shiny::observeEvent(input$calculate, {
      tryCatch({
        d <- shared_aggregate()
        if (is.null(d)) stop("First upload and apply a planning file in Aggregate Planning Data Center.")
        fractional <- aggregate_lp_calculate(
          d, allow_overtime = input$allow_overtime,
          allow_idle = input$allow_idle,
          allow_subcontract = input$allow_subcontract,
          overtime_fraction = input$max_ot_fraction,
          idle_unit_cost = input$idle_unit_cost
        )
        workforce <- fractional$plan$Workers
        start_workers <- as.numeric(d$plant[["start_workers"]])
        if (abs(start_workers - round(start_workers)) > 1e-7) {
          stop("Starting Workforce must be a whole number to create the whole-worker comparison.")
        }
        rounded_workers <- ceiling(pmax(0, workforce) - 1e-7)
        rounded <- aggregate_lp_calculate(
          d, allow_overtime = input$allow_overtime,
          allow_idle = input$allow_idle,
          allow_subcontract = input$allow_subcontract,
          overtime_fraction = input$max_ot_fraction,
          idle_unit_cost = input$idle_unit_cost,
          fixed_workers = rounded_workers
        )
        result(list(fractional = fractional, whole = rounded))
        status("Both plans solved successfully. Results reflect the most recent click; solve again after changing inputs.")
      }, error = function(e) {
        result(NULL)
        status(paste("Could not solve the LP model:", conditionMessage(e)))
        shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    display_plan <- function(plan) {
      is_number <- vapply(plan, is.numeric, logical(1L))
      plan[is_number] <- lapply(plan[is_number], function(x) round(x, 2))
      plan
    }
    display_costs <- function(r) {
      data.frame(`Cost Item` = names(r$costs),
                 `Amount` = round(unname(r$costs), 2),
                 check.names = FALSE, stringsAsFactors = FALSE)
    }
    output$fractional_plan <- shiny::renderTable({
      r <- shiny::req(result())
      display_plan(r$fractional$plan)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    output$whole_plan <- shiny::renderTable({
      r <- shiny::req(result())
      display_plan(r$whole$plan)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    output$fractional_costs <- shiny::renderTable({
      r <- shiny::req(result())
      display_costs(r$fractional)
    }, striped = TRUE, bordered = TRUE)
    output$whole_costs <- shiny::renderTable({
      r <- shiny::req(result())
      display_costs(r$whole)
    }, striped = TRUE, bordered = TRUE)
    output$fractional_total <- shiny::renderText({
      r <- shiny::req(result())$fractional
      sprintf("Total Cost: %.2f | Ending Inventory: %.2f (target >= %.2f)",
              r$total_cost, r$ending_inventory, r$target_inventory)
    })
    output$whole_total <- shiny::renderText({
      r <- shiny::req(result())$whole
      sprintf("Total Cost: %.2f | Ending Inventory: %.2f (target >= %.2f)",
              r$total_cost, r$ending_inventory, r$target_inventory)
    })
    output$comparison_plot <- shiny::renderPlot({
      r <- shiny::req(result())
      x <- seq_len(nrow(r$fractional$plan))
      y_fractional <- r$fractional$cumulative_production
      y_whole <- r$whole$cumulative_production
      y_demand <- r$fractional$cumulative_net_demand
      ylim <- range(c(0, y_fractional, y_whole, y_demand), finite = TRUE)
      graphics::plot(x, y_fractional, type = "o", pch = 16, lwd = 2,
        col = "#A78BFA", xaxt = "n", xlab = "Planning Period",
        ylab = "Cumulative Units", ylim = ylim,
        main = "Cumulative Production vs. Net Demand")
      graphics::axis(1, at = x, labels = r$fractional$plan$Period,
        las = 2, cex.axis = if (length(x) > 9) 0.7 else 0.9)
      graphics::lines(x, y_whole, type = "o", pch = 17, lwd = 2, col = "#1E293B")
      graphics::lines(x, y_demand, type = "o", pch = 15, lwd = 2, lty = 2, col = "#64748B")
      graphics::legend("topleft",
        legend = c("Fractional LP Production", "Whole-Worker Production", "Cumulative Net Demand"),
        col = c("#A78BFA", "#1E293B", "#64748B"),
        pch = c(16, 17, 15), lty = c(1, 1, 2), lwd = 2, cex = 0.85, bty = "n")
    })
  })
}
