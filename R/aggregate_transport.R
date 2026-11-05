# Aggregate Planning - Transportation Method (Assignment 3)
# Requires shiny and lpSolve (already used in aggregate_lp.R).
# Data format: list(periods, plant, costs, k) from aggregate_data_server().
# No forecasting module or app.R changes are required to source this file.
#
# This module compares:
#   1. North-West Corner initial feasible allocation;
#   2. Cheapest Cell First (least-cost) feasible allocation;
#   3. A mathematically optimal transportation LP allocation.
# Cheapest Cell First is a heuristic and is NOT generally optimal.
#
# Production from a period cannot meet demand from an earlier period.
# Unsold production can be held for future periods at a per-unit period cost.
# Starting stock is a mandatory source up to total demand + desired final stock.
# Any additional starting stock is carried to the horizon end.
# Workforce is an INPUT to the transportation problem, not an LP decision.

aggregate_transport_prepare <- function(data, workers, overtime_fraction = 0.25,
                                        subcontract_limit = 1000000,
                                        allow_overtime = TRUE,
                                        allow_subcontract = TRUE) {
  if (is.null(data) || !is.list(data)) stop("Apply Aggregate Planning Data first.")
  p <- data$periods
  if (!is.data.frame(p) || nrow(p) < 1L ||
      !all(c("period", "demand", "working_days") %in% names(p))) {
    stop("Planning data must contain period, demand, and working_days.")
  }
  n <- nrow(p)
  workers <- as.numeric(workers)
  demand <- as.numeric(p$demand)
  days <- as.numeric(p$working_days)
  k <- as.numeric(data$k)
  plant <- data$plant
  costs <- data$costs
  required_plant <- c("start_workers", "start_inventory", "end_inventory")
  required_costs <- c("hiring", "firing", "holding", "regular", "overtime", "subcontract")
  if (!all(required_plant %in% names(plant)) ||
      !all(required_costs %in% names(costs))) {
    stop("Required plant or cost information is missing.")
  }
  values <- c(demand, days, workers, k, unname(plant[required_plant]),
              unname(costs[required_costs]), overtime_fraction, subcontract_limit)
  if (length(workers) != n || length(k) != 1L ||
      length(overtime_fraction) != 1L || length(subcontract_limit) != 1L ||
      anyNA(values) || any(!is.finite(values)) || any(values < 0) ||
      any(days <= 0) || k <= 0) {
    stop("All values must be finite and nonnegative; working days and K must be positive. Provide one workforce value per period.")
  }
  initial_inv <- as.numeric(plant[["start_inventory"]])
  target_inv <- as.numeric(plant[["end_inventory"]])
  total_required <- sum(demand) + target_inv
  initial_supply <- min(initial_inv, total_required)
  extra_initial_stock <- initial_inv - initial_supply
  regular_capacity <- workers * k * days
  overtime_capacity <- if (isTRUE(allow_overtime)) {
    regular_capacity * overtime_fraction
  } else rep(0, n)
  subcontract_capacity <- if (isTRUE(allow_subcontract)) {
    rep(subcontract_limit, n)
  } else rep(0, n)

  # Each period contributes regular, overtime, and subcontracting sources.
  source_period <- c(0L, rep(seq_len(n), each = 3L))
  source_type <- c("Initial Inventory", rep(c("Regular", "Overtime", "Subcontracting"), n))
  source_capacity <- c(initial_supply,
    as.vector(rbind(regular_capacity, overtime_capacity, subcontract_capacity)))
  unit_production_cost <- c(0, rep(c(costs[["regular"]], costs[["overtime"]],
                                     costs[["subcontract"]]), n))
  source_label <- c("Initial Inventory", paste0(
    as.character(p$period)[rep(seq_len(n), each = 3L)], " - ",
    source_type[-1L]))
  # Final-stock demand is a separate destination, after the last period.
  requirements <- c(demand, target_inv)
  destination_period <- c(seq_len(n), n + 1L)
  destination_label <- c(as.character(p$period), "Required Ending Inventory")
  m <- length(source_capacity)
  d <- length(requirements)
  unit_cost <- matrix(Inf, nrow = m, ncol = d,
                      dimnames = list(source_label, destination_label))
  for (i in seq_len(m)) {
    for (j in seq_len(d)) {
      if (source_period[i] > destination_period[j]) next
      # Starting stock is already on hand before period 1, and is held only
      # at the end of the periods prior to the period of consumption.
      periods_held <- if (source_period[i] == 0L) {
        destination_period[j] - 1L
      } else {
        destination_period[j] - source_period[i]
      }
      unit_cost[i, j] <- unit_production_cost[i] +
        costs[["holding"]] * periods_held
    }
  }

  # Prefix capacity must cover demand through every period; later sources
  # cannot be sent backwards in time. All destinations must be coverable.
  tol <- 1e-6
  for (t in seq_len(n)) {
    available <- sum(source_capacity[source_period <= t])
    if (available + tol < sum(demand[seq_len(t)])) {
      stop(sprintf("Infeasible plan through period %s: earlier supply capacity cannot meet demand. Increase workforce or available overtime/subcontracting.",
                   as.character(p$period[t])))
    }
  }
  if (sum(source_capacity) + tol < sum(requirements)) {
    stop("Total regular, overtime, subcontracting and initial-inventory capacity is insufficient for demand and ending stock.")
  }
  list(n = n, periods = p, demand = demand, days = days,
       k = k, workers = workers, plant = plant, costs = costs,
       source_period = source_period, source_type = source_type,
       source_label = source_label, supply = source_capacity,
       initial_supply = initial_supply, extra_initial_stock = extra_initial_stock,
       regular_capacity = regular_capacity,
       overtime_capacity = overtime_capacity,
       subcontract_capacity = subcontract_capacity,
       requirements = requirements,
       destination_period = destination_period,
       destination_label = destination_label,
       unit_cost = unit_cost)
}

aggregate_transport_greedy <- function(problem, method = c("northwest", "cheapest")) {
  method <- match.arg(method)
  supply_left <- problem$supply
  demand_left <- problem$requirements
  m <- length(supply_left)
  d <- length(demand_left)
  tol <- 1e-7
  allocation <- matrix(0, m, d, dimnames = dimnames(problem$unit_cost))
  if (sum(demand_left) <= tol) return(allocation)

  if (method == "northwest") {
    # North-West Corner, considering only arcs that do not backorder demand.
    for (j in seq_len(d)) {
      for (i in seq_len(m)) {
        if (demand_left[j] <= tol) break
        if (!is.finite(problem$unit_cost[i, j]) || supply_left[i] <= tol) next
        x <- min(supply_left[i], demand_left[j])
        allocation[i, j] <- allocation[i, j] + x
        supply_left[i] <- supply_left[i] - x
        demand_left[j] <- demand_left[j] - x
      }
      if (demand_left[j] > tol) {
        stop("North-West Corner could not satisfy a destination. Check capacity and timing.")
      }
    }
  } else {
    # Cheapest Cell First, with a feasibility-preserving allocation cap.
    # A naive global least-cost choice can consume capacity required for an
    # earlier deadline, so we reserve the necessary prefix capacity.
    ordered <- order(as.vector(problem$unit_cost),
                     rep(seq_len(d), each = m), rep(seq_len(m), d),
                     na.last = TRUE)
    ordered <- ordered[is.finite(as.vector(problem$unit_cost)[ordered])]
    max_steps <- max(100L, 5L * m * d)
    steps <- 0L
    while (sum(demand_left) > tol) {
      steps <- steps + 1L
      if (steps > max_steps) stop("Cheapest Cell First exceeded its allocation limit.")
      slack <- vapply(seq_len(problem$n), function(t) {
        sum(supply_left[problem$source_period <= t]) -
          sum(demand_left[seq_len(t)])
      }, numeric(1L))
      selected_i <- NA_integer_
      selected_j <- NA_integer_
      selected_x <- 0
      for (cell in ordered) {
        i <- ((cell - 1L) %% m) + 1L
        j <- ((cell - 1L) %/% m) + 1L
        if (supply_left[i] <= tol || demand_left[j] <= tol) next
        x <- min(supply_left[i], demand_left[j])
        from <- max(1L, problem$source_period[i])
        to <- problem$destination_period[j] - 1L
        if (from <= to) x <- min(x, min(pmax(0, slack[from:to])))
        # Starting inventory has to be used before other sources can exhaust
        # all destinations (the initial stock is not a disposable supply).
        if (i != 1L) {
          x <- min(x, max(0, sum(demand_left) - supply_left[1L]))
        }
        if (x > tol) {
          selected_i <- i
          selected_j <- j
          selected_x <- x
          break
        }
      }
      if (!is.finite(selected_i)) {
        stop("Cheapest Cell First could not find a feasible next assignment.")
      }
      allocation[selected_i, selected_j] <-
        allocation[selected_i, selected_j] + selected_x
      supply_left[selected_i] <- supply_left[selected_i] - selected_x
      demand_left[selected_j] <- demand_left[selected_j] - selected_x
    }
  }
  if (any(demand_left > 1e-5) ||
      abs(sum(allocation[1L, ]) - problem$initial_supply) > 1e-5) {
    stop("The transportation allocation failed its demand or initial-stock check.")
  }
  allocation
}

aggregate_transport_optimal <- function(problem) {
  if (!requireNamespace("lpSolve", quietly = TRUE)) {
    stop("Install the LP solver once in RStudio: install.packages('lpSolve')")
  }
  m <- length(problem$supply)
  d <- length(problem$requirements)
  allocation <- matrix(0, m, d, dimnames = dimnames(problem$unit_cost))
  if (sum(problem$requirements) < 1e-8) return(allocation)
  eligible <- which(is.finite(problem$unit_cost) &
    (matrix(problem$supply, m, d) > 1e-8) &
    (matrix(problem$requirements, m, d, byrow = TRUE) > 1e-8),
    arr.ind = TRUE)
  if (!nrow(eligible)) stop("No usable transportation routes are available.")
  count <- nrow(eligible)
  constraint_matrix <- matrix(0, nrow = m + d, ncol = count)
  for (q in seq_len(count)) {
    constraint_matrix[eligible[q, 1L], q] <- 1
    constraint_matrix[m + eligible[q, 2L], q] <- 1
  }
  directions <- c("=", rep("<=", m - 1L), rep("=", d))
  rhs <- c(problem$supply, problem$requirements)
  objective <- problem$unit_cost[eligible]
  fit <- lpSolve::lp(direction = "min", objective.in = objective,
                     const.mat = constraint_matrix,
                     const.dir = directions, const.rhs = rhs)
  if (fit$status != 0L) {
    stop(sprintf("The transportation LP did not find an optimal feasible solution (status %s).", fit$status))
  }
  allocation[eligible] <- pmax(0, as.numeric(fit$solution))
  allocation
}

aggregate_transport_summarize <- function(problem, allocation, method) {
  n <- problem$n
  tol <- 1e-5
  if (any(allocation < -tol) ||
      any(rowSums(allocation) > problem$supply + tol) ||
      any(abs(colSums(allocation) - problem$requirements) > tol) ||
      abs(sum(allocation[1L, ]) - problem$initial_supply) > tol ||
      any(allocation[!is.finite(problem$unit_cost)] > tol)) {
    stop("The transportation plan failed the allocation feasibility checks.")
  }
  used <- rowSums(allocation)
  get_used <- function(type) {
    ix <- which(problem$source_type == type)
    if (length(ix) != n) stop("Invalid source type indexing.")
    used[ix]
  }
  regular <- get_used("Regular")
  overtime <- get_used("Overtime")
  subcontract <- get_used("Subcontracting")
  production <- regular + overtime + subcontract
  inventory <- problem$plant[["start_inventory"]] +
    cumsum(production - problem$demand)
  ending_target <- problem$plant[["end_inventory"]]
  if (any(inventory < -tol) || tail(inventory, 1L) + tol < ending_target) {
    stop("The plan would cause backorders or miss required ending inventory.")
  }
  inventory <- pmax(0, inventory)
  workers <- problem$workers
  previous_workers <- c(problem$plant[["start_workers"]], head(workers, -1L))
  hired <- pmax(0, workers - previous_workers)
  fired <- pmax(0, previous_workers - workers)
  cc <- problem$costs
  hiring_cost <- hired * cc[["hiring"]]
  firing_cost <- fired * cc[["firing"]]
  regular_cost <- regular * cc[["regular"]]
  overtime_cost <- overtime * cc[["overtime"]]
  subcontract_cost <- subcontract * cc[["subcontract"]]
  holding_cost <- inventory * cc[["holding"]]
  period_cost <- hiring_cost + firing_cost + regular_cost +
    overtime_cost + subcontract_cost + holding_cost
  breakdown <- c(Hiring = sum(hiring_cost), Firing = sum(firing_cost),
                 `Regular Time` = sum(regular_cost),
                 Overtime = sum(overtime_cost),
                 Subcontracting = sum(subcontract_cost),
                 Holding = sum(holding_cost))
  plan <- data.frame(
    Period = as.character(problem$periods$period),
    Demand = problem$demand,
    Working_Days = problem$days,
    Workers = workers,
    Hired = hired,
    Fired = fired,
    Regular_Capacity = problem$regular_capacity,
    Regular_Production = regular,
    Unused_Regular_Capacity = pmax(0, problem$regular_capacity - regular),
    Overtime = overtime,
    Subcontracting = subcontract,
    Total_Production = production,
    Ending_Inventory = inventory,
    Hiring_Cost = hiring_cost,
    Firing_Cost = firing_cost,
    Regular_Cost = regular_cost,
    Overtime_Cost = overtime_cost,
    Subcontracting_Cost = subcontract_cost,
    Holding_Cost = holding_cost,
    Total_Period_Cost = period_cost,
    check.names = FALSE, stringsAsFactors = FALSE)
  active <- which(allocation > 1e-8, arr.ind = TRUE)
  shipments <- if (nrow(active)) {
    data.frame(Source = problem$source_label[active[, 1L]],
               Destination = problem$destination_label[active[, 2L]],
               Units = allocation[active],
               Unit_Cost_Including_Carry = problem$unit_cost[active],
               Allocation_Cost = allocation[active] * problem$unit_cost[active],
               check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    data.frame(Source = character(), Destination = character(), Units = numeric(),
               Unit_Cost_Including_Carry = numeric(), Allocation_Cost = numeric())
  }
  # Transportation costs include production and carrying; initial-stock surplus
  # is fixed and must also be charged holding costs through the final period.
  allocation_total <- sum(allocation[is.finite(problem$unit_cost)] *
                            problem$unit_cost[is.finite(problem$unit_cost)]) +
    problem$extra_initial_stock * n * cc[["holding"]] +
    sum(hiring_cost) + sum(firing_cost)
  if (abs(allocation_total - sum(breakdown)) >
      1e-4 * max(1, sum(breakdown))) {
    stop("Transportation assignment costs do not match period-by-period costs.")
  }
  list(method = method, plan = plan, shipments = shipments, costs = breakdown,
       total_cost = sum(breakdown),
       cumulative_production = cumsum(production),
       cumulative_net_demand = cumsum(problem$demand) -
         problem$plant[["start_inventory"]],
       ending_inventory = tail(inventory, 1L),
       target_inventory = ending_target)
}

aggregate_transport_solve <- function(problem, method = c("northwest", "cheapest", "optimal")) {
  method <- match.arg(method)
  allocation <- if (method == "optimal") {
    aggregate_transport_optimal(problem)
  } else {
    aggregate_transport_greedy(problem, method)
  }
  aggregate_transport_summarize(problem, allocation, method)
}

aggregate_transport_level_workers <- function(data) {
  p <- data$periods
  n <- nrow(p)
  net_need <- cumsum(as.numeric(p$demand)) -
    as.numeric(data$plant[["start_inventory"]])
  net_need[n] <- net_need[n] + as.numeric(data$plant[["end_inventory"]])
  rep(max(0, net_need / cumsum(data$k * p$working_days)), n)
}

aggregate_transport_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "page-header",
      shiny::div(class = "top-tag", "AGGREGATE PLANNING"),
      shiny::h2("Transportation Method"),
      shiny::p(class = "page-description",
        "Compare North-West Corner, Cheapest Cell First, and a verified optimal transportation allocation.")),
    shiny::div(class = "content-panel",
      shiny::h4("1. Workforce and Source Capacities"),
      shiny::p("Upload and apply planning data in Aggregate Planning Data Center first."),
      shiny::radioButtons(ns("workforce_mode"), "Workforce Basis",
        choices = c("Calculated Level Workforce (fractional)" = "level",
                    "Constant Starting Workforce" = "constant",
                    "Custom Workforce for Every Period" = "custom"),
        selected = "level", inline = TRUE),
      shiny::uiOutput(ns("workforce_controls")),
      shiny::fluidRow(
        shiny::column(4, shiny::checkboxInput(ns("allow_overtime"),
          "Allow Overtime Sources", TRUE)),
        shiny::column(4, shiny::checkboxInput(ns("allow_subcontract"),
          "Allow Subcontracting Sources", TRUE))
      ),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("overtime_fraction"),
          "Overtime Capacity / Regular Capacity", 0.25,
          min = 0, max = 2, step = 0.05)),
        shiny::column(6, shiny::numericInput(ns("subcontract_limit"),
          "Maximum Subcontracting Units per Period", 1000000,
          min = 0, step = 100))
      ),
      shiny::p("A source can supply its own period or a later period, never an earlier one. " ,
        "Unused regular, overtime, and subcontract capacity is permitted."),
      shiny::actionButton(ns("calculate"), "Calculate Transportation Plans", class = "btn-primary"),
      shiny::p(style = "margin-top:12px;", shiny::textOutput(ns("status")))
    ),
    shiny::div(class = "content-panel",
      shiny::h4("2. Choose a Transportation Solution to Inspect"),
      shiny::selectInput(ns("method"), "Allocation Method",
        choices = c("North-West Corner (Initial Feasible)" = "northwest",
                    "Cheapest Cell First (Least-Cost Heuristic)" = "cheapest",
                    "Optimal Transportation Allocation (LP Verified)" = "optimal"),
        selected = "cheapest"),
      shiny::h4("Method Cost Comparison"),
      shiny::tableOutput(ns("method_comparison")),
      shiny::p("The cheapest-cell method is not guaranteed to be mathematically optimal. " ,
        "The additional transportation LP checks the true minimum allocation cost for the given workforce and capacities.")
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::div(class = "content-panel",
        shiny::h4("3. Fractional-Worker Transportation Plan"),
        shiny::div(style = "overflow-x:auto;", shiny::tableOutput(ns("fractional_plan"))),
        shiny::h4("Cost Breakdown"),
        shiny::tableOutput(ns("fractional_costs")),
        shiny::strong(shiny::textOutput(ns("fractional_total")))
      )),
      shiny::column(6, shiny::div(class = "content-panel",
        shiny::h4("4. Rounded-Up Whole-Worker Transportation Plan"),
        shiny::div(style = "overflow-x:auto;", shiny::tableOutput(ns("whole_plan"))),
        shiny::h4("Cost Breakdown"),
        shiny::tableOutput(ns("whole_costs")),
        shiny::strong(shiny::textOutput(ns("whole_total")))
      ))
    ),
    shiny::div(class = "content-panel",
      shiny::h4("5. Cumulative Production vs. Cumulative Net Demand"),
      shiny::plotOutput(ns("comparison_plot"), height = "400px")
    ),
    shiny::div(class = "content-panel",
      shiny::h4("6. Transportation Allocation Details"),
      shiny::p("Shipment unit costs include the production cost plus holding costs " ,
        "for future-period demand. Initial inventory is a zero-production-cost source."),
      shiny::h4("Fractional-Worker Source-to-Destination Assignments"),
      shiny::div(style = "overflow-x:auto;", shiny::tableOutput(ns("fractional_shipments"))),
      shiny::h4("Whole-Worker Source-to-Destination Assignments"),
      shiny::div(style = "overflow-x:auto;", shiny::tableOutput(ns("whole_shipments")))
    ),
    shiny::div(class = "content-panel",
      shiny::h4("Model Notes"),
      shiny::p("The workforce is specified before transportation optimization. " ,
        "The calculated level option uses the minimum constant fractional workforce " ,
        "that can cover cumulative demand in regular time. A custom workforce " ,
        "vector can contain changes by period, with hiring and firing costs counted."),
      shiny::p("Initial inventory must be used to cover demand or the required final stock; " ,
        "any excess initial stock remains in ending inventory. No backorders are allowed."),
      shiny::p("The entire workforce vector is rounded UP for the whole-worker comparison. " ,
        "Both workforce cases are independently allocated for each method."),
      shiny::p("The true optimum is for the fixed workforce and source capacities shown; " ,
        "it is not a global workforce-planning optimum. Ending inventory is " ,
        "at least the target, and holding cost is charged on each period's ending inventory.")
    )
  )
}

aggregate_transport_server <- function(id, shared_aggregate) {
  shiny::moduleServer(id, function(input, output, session) {
    status <- shiny::reactiveVal("Apply planning data, then calculate transportation plans.")
    results <- shiny::reactiveVal(NULL)
    output$status <- shiny::renderText(status())
    output$workforce_controls <- shiny::renderUI({
      dat <- shared_aggregate()
      if (is.null(dat)) return(shiny::p("Apply planning data to view the workforce schedule."))
      level <- aggregate_transport_level_workers(dat)
      if (identical(input$workforce_mode, "custom")) {
        shiny::textInput(session$ns("custom_workers"),
          "Workforce by Period (comma-separated, fractional values allowed)",
          value = paste(round(level, 4), collapse = ", "), width = "100%")
      } else if (identical(input$workforce_mode, "constant")) {
        shiny::p(sprintf("Workers per period: %.3f (starting workforce)",
                         dat$plant[["start_workers"]]))
      } else {
        shiny::p(sprintf("Workers per period: %.4f (calculated level workforce)", level[1L]))
      }
    })
    shiny::observeEvent(input$calculate, {
      tryCatch({
        dat <- shared_aggregate()
        if (is.null(dat)) stop("First upload and apply planning data in Aggregate Planning Data Center.")
        n <- nrow(dat$periods)
        mode <- input$workforce_mode
        workforce <- if (identical(mode, "constant")) {
          rep(as.numeric(dat$plant[["start_workers"]]), n)
        } else if (identical(mode, "custom")) {
          raw <- input$custom_workers
          if (is.null(raw) || !nzchar(trimws(raw))) stop("Enter the custom workforce sequence.")
          x <- suppressWarnings(as.numeric(trimws(strsplit(raw, ",", fixed = TRUE)[[1L]])))
          if (length(x) != n || anyNA(x)) {
            stop(sprintf("Enter exactly %d numeric workforce values, separated by commas.", n))
          }
          x
        } else {
          aggregate_transport_level_workers(dat)
        }
        if (any(!is.finite(workforce)) || any(workforce < 0)) {
          stop("Workforce levels must be finite nonnegative numbers.")
        }
        fraction_problem <- aggregate_transport_prepare(dat, workforce,
          overtime_fraction = input$overtime_fraction,
          subcontract_limit = input$subcontract_limit,
          allow_overtime = input$allow_overtime,
          allow_subcontract = input$allow_subcontract)
        rounded_workforce <- ceiling(pmax(0, workforce) - 1e-8)
        whole_problem <- aggregate_transport_prepare(dat, rounded_workforce,
          overtime_fraction = input$overtime_fraction,
          subcontract_limit = input$subcontract_limit,
          allow_overtime = input$allow_overtime,
          allow_subcontract = input$allow_subcontract)
        methods <- c("northwest", "cheapest", "optimal")
        fraction_results <- stats::setNames(lapply(methods,
          function(m) aggregate_transport_solve(fraction_problem, m)), methods)
        whole_results <- stats::setNames(lapply(methods,
          function(m) aggregate_transport_solve(whole_problem, m)), methods)
        results(list(fractional = fraction_results, whole = whole_results))
        status("North-West Corner, Cheapest Cell First, and optimal LP allocations calculated for both workforce cases.")
      }, error = function(e) {
        results(NULL)
        status(paste("Could not calculate transportation plans:", conditionMessage(e)))
        shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
      })
    })
    selected <- shiny::reactive({
      r <- shiny::req(results())
      meth <- input$method
      list(fractional = r$fractional[[meth]], whole = r$whole[[meth]])
    })
    format_table <- function(df) {
      nums <- vapply(df, is.numeric, logical(1L))
      df[nums] <- lapply(df[nums], function(x) round(x, 2))
      df
    }
    costs_table <- function(r) {
      data.frame(`Cost Item` = names(r$costs),
        `Amount` = round(unname(r$costs), 2),
        check.names = FALSE, stringsAsFactors = FALSE)
    }
    output$method_comparison <- shiny::renderTable({
      r <- shiny::req(results())
      methods <- c("northwest", "cheapest", "optimal")
      labels <- c("North-West Corner", "Cheapest Cell First", "Optimal LP")
      data.frame(Method = labels,
        `Fractional Workforce Cost` = round(vapply(methods,
          function(m) r$fractional[[m]]$total_cost, numeric(1L)), 2),
        `Whole Workforce Cost` = round(vapply(methods,
          function(m) r$whole[[m]]$total_cost, numeric(1L)), 2),
        check.names = FALSE)
    }, striped = TRUE, bordered = TRUE)
    output$fractional_plan <- shiny::renderTable({
      format_table(selected()$fractional$plan)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    output$whole_plan <- shiny::renderTable({
      format_table(selected()$whole$plan)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    output$fractional_costs <- shiny::renderTable({
      costs_table(selected()$fractional)
    }, striped = TRUE, bordered = TRUE)
    output$whole_costs <- shiny::renderTable({
      costs_table(selected()$whole)
    }, striped = TRUE, bordered = TRUE)
    output$fractional_total <- shiny::renderText({
      r <- selected()$fractional
      sprintf("Total Cost: %.2f | Ending Inventory: %.2f (target >= %.2f)",
              r$total_cost, r$ending_inventory, r$target_inventory)
    })
    output$whole_total <- shiny::renderText({
      r <- selected()$whole
      sprintf("Total Cost: %.2f | Ending Inventory: %.2f (target >= %.2f)",
              r$total_cost, r$ending_inventory, r$target_inventory)
    })
    output$fractional_shipments <- shiny::renderTable({
      format_table(selected()$fractional$shipments)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    output$whole_shipments <- shiny::renderTable({
      format_table(selected()$whole$shipments)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    output$comparison_plot <- shiny::renderPlot({
      r <- selected()
      x <- seq_len(nrow(r$fractional$plan))
      yf <- r$fractional$cumulative_production
      yw <- r$whole$cumulative_production
      yd <- r$fractional$cumulative_net_demand
      bounds <- range(c(0, yf, yw, yd), finite = TRUE)
      if (diff(bounds) < 1e-10) bounds <- bounds + c(-1, 1)
      graphics::plot(x, yf, type = "o", pch = 16, lwd = 2,
        col = "#A78BFA", xaxt = "n", xlab = "Planning Period",
        ylab = "Cumulative Units", ylim = bounds,
        main = "Cumulative Production vs. Net Demand")
      graphics::axis(1, at = x, labels = r$fractional$plan$Period,
        las = 2, cex.axis = if (length(x) > 9L) 0.7 else 0.9)
      graphics::lines(x, yw, type = "o", pch = 17, lwd = 2,
        col = "#1E293B")
      graphics::lines(x, yd, type = "o", pch = 15, lwd = 2,
        lty = 2, col = "#64748B")
      graphics::legend("topleft", bty = "n", cex = 0.82,
        legend = c("Fractional Workforce", "Whole Workforce", "Net Demand"),
        col = c("#A78BFA", "#1E293B", "#64748B"),
        pch = c(16, 17, 15), lty = c(1, 1, 2), lwd = 2)
    })
  })
}
