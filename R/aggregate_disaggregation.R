# Aggregate Planning - Disaggregation Using the Textbook Square-Root Rule
# Assignment 3 | Production Planning Group 7
#
# Source R/aggregate_heuristic.R before sourcing this file.
# The modules aggregate_disaggregation_ui() and aggregate_disaggregation_server()
# use the existing .page-header, .content-panel and .btn-primary CSS classes.
# Only the shiny package is required.
#
# Textbook Section 3.9, Disaggregating Aggregate Plans:
#   Minimize sum_j (SetupCost_j * AnnualUsage_j / Y_j)
#   subject to sum_j Y_j = X*, Min_j <= Y_j <= Max_j.
# For nonbinding bounds: Y_j = X* sqrt(K_j * lambda_j) / sum_i sqrt(K_i * lambda_i).
#
# This module allocates EACH aggregate period's regular-time production X*.
# Bounds, when provided, apply per period. The textbook model allocates
# production quantities, not family workforce, inventory, or family demand;
# no unsupported family inventory/workforce calculations are invented.
# The objective is annualized setup cost for each period's proposed allocation;
# it is NOT added to the heuristic plan's horizon operating-cost total.

aggregate_disaggregation_parse_families <- function(text) {
  if (length(text) != 1L || is.na(text) || !nzchar(trimws(text))) {
    stop("Enter at least one family with its setup cost and annual usage rate.")
  }
  lines <- trimws(strsplit(gsub("\r", "", text), "\n", fixed = TRUE)[[1L]])
  lines <- lines[nzchar(lines)]
  parts <- strsplit(lines, ",", fixed = TRUE)
  if (any(!lengths(parts) %in% c(3L, 5L))) {
    stop("Use: Family, Setup Cost, Annual Usage [optional: Minimum, Maximum].")
  }
  parse_num <- function(value, label, allow_inf = FALSE) {
    val <- suppressWarnings(as.numeric(trimws(value)))
    if (length(val) != 1L || is.na(val) || (!allow_inf && !is.finite(val)) ||
        (allow_inf && !is.finite(val) && !is.infinite(val))) {
      stop(paste("Invalid", label, "value:", value))
    }
    val
  }
  out <- lapply(parts, function(fields) {
    fields <- trimws(fields)
    if (!nzchar(fields[1L])) stop("Family name cannot be empty.")
    k <- parse_num(fields[2L], "setup cost")
    lambda <- parse_num(fields[3L], "annual usage")
    minimum <- if (length(fields) == 5L) parse_num(fields[4L], "minimum") else 0
    maximum <- if (length(fields) == 5L) parse_num(fields[5L], "maximum", TRUE) else Inf
    if (k <= 0 || lambda <= 0 || minimum < 0 || maximum < minimum) {
      stop("Setup cost and annual usage must be positive; bounds must satisfy 0 <= minimum <= maximum.")
    }
    data.frame(Family = fields[1L], Setup_Cost = k,
               Annual_Usage = lambda, Minimum_Units = minimum,
               Maximum_Units = maximum, stringsAsFactors = FALSE)
  })
  result <- do.call(rbind, out)
  if (anyDuplicated(tolower(result$Family))) stop("Family names must be unique.")
  rownames(result) <- NULL
  result
}

# Solve strictly convex bounded allocation via Y_j = clamp(t * sqrt(K_j lambda_j), a_j, b_j)
# and bisection on t. Equal total is preserved, also with active bounds.
aggregate_disaggregation_allocate <- function(total, families) {
  if (!is.data.frame(families) || !all(c("Family", "Setup_Cost", "Annual_Usage",
             "Minimum_Units", "Maximum_Units") %in% names(families))) {
    stop("Family data must contain Family, Setup_Cost, Annual_Usage, Minimum_Units, and Maximum_Units.")
  }
  x <- as.numeric(total)
  if (length(x) != 1L || !is.finite(x) || x < 0) {
    stop("Aggregate production X* must be one nonnegative finite number.")
  }
  k <- as.numeric(families$Setup_Cost)
  lambda <- as.numeric(families$Annual_Usage)
  lower <- as.numeric(families$Minimum_Units)
  upper <- as.numeric(families$Maximum_Units)
  if (!length(k) || any(!is.finite(k)) || any(!is.finite(lambda)) ||
      any(k <= 0) || any(lambda <= 0) ||
      any(!is.finite(lower)) || any(lower < 0) ||
      any(is.na(upper)) || any(upper < lower)) {
    stop("Invalid setup costs, usage rates, or family quantity bounds.")
  }
  tolerance <- 1e-8 * max(1, x)
  if (sum(lower) > x + tolerance ||
      (all(is.finite(upper)) && sum(upper) < x - tolerance)) {
    stop(sprintf("Family production bounds are infeasible for aggregate production %.4f.", x))
  }
  if (x == 0) {
    if (any(lower > 0)) stop("Positive minimum quantity cannot be met with zero aggregate production.")
    return(list(quantity = rep(0, length(k)), objective = NA_real_))
  }

  # Normalize both arrays before multiplying to reduce overflow risk.
  weight <- sqrt(k / max(k)) * sqrt(lambda / max(lambda))
  clipped <- function(scale) pmin(upper, pmax(lower, scale * weight))

  if (abs(sum(lower) - x) <= tolerance) {
    quantity <- lower
  } else if (all(is.finite(upper)) && abs(sum(upper) - x) <= tolerance) {
    quantity <- upper
  } else {
    lo <- 0
    hi <- max(1, x / sum(weight))
    attempts <- 0L
    while (sum(clipped(hi)) < x && attempts < 1024L) {
      if (hi > .Machine$double.xmax / 2) break
      hi <- hi * 2
      attempts <- attempts + 1L
    }
    if (sum(clipped(hi)) < x - tolerance) stop("Could not bracket a feasible allocation.")
    for (iter in seq_len(250L)) {
      mid <- lo + (hi - lo) / 2
      qty <- clipped(mid)
      if (sum(qty) < x) lo <- mid else hi <- mid
    }
    quantity <- clipped(hi)
    # Correct tiny bisection floating-point residue without breaking bounds.
    remainder <- x - sum(quantity)
    if (abs(remainder) > 1e-11 * max(1, x)) {
      if (remainder > 0) {
        for (j in which(upper - quantity > 0)) {
          delta <- min(remainder, upper[j] - quantity[j])
          quantity[j] <- quantity[j] + delta
          remainder <- remainder - delta
          if (remainder <= tolerance) break
        }
      } else {
        for (j in which(quantity - lower > 0)) {
          delta <- min(-remainder, quantity[j] - lower[j])
          quantity[j] <- quantity[j] - delta
          remainder <- remainder + delta
          if (-remainder <= tolerance) break
        }
      }
    }
  }
  if (abs(sum(quantity) - x) > tolerance ||
      any(quantity < lower - tolerance) || any(quantity > upper + tolerance)) {
    stop("Internal error: family allocation violates total or bound constraints.")
  }
  # If a family receives no production while setup/usage are positive,
  # the original objective is undefined/infinite, so do not label it optimal.
  objective <- if (any(quantity <= 0)) Inf else sum((k / quantity) * lambda)
  list(quantity = quantity, objective = objective)
}

aggregate_disaggregation_calculate <- function(data, families,
                                                method = "level",
                                                change_periods = integer(),
                                                whole_workers = FALSE) {
  if (!exists("aggregate_heuristic_calculate", mode = "function")) {
    stop("Source aggregate_heuristic.R before aggregate_disaggregation.R.")
  }
  aggregate <- aggregate_heuristic_calculate(
    data, method = method, change_periods = change_periods,
    whole_workers = whole_workers
  )
  p <- aggregate$plan
  family_details <- vector("list", nrow(p))
  period_summary <- vector("list", nrow(p))
  for (t in seq_len(nrow(p))) {
    x <- as.numeric(p$Regular_Production[t])
    sol <- aggregate_disaggregation_allocate(x, families)
    q <- sol$quantity
    obj_terms <- if (x == 0) rep(NA_real_, length(q)) else
      ifelse(q > 0, families$Setup_Cost * families$Annual_Usage / q, Inf)
    family_details[[t]] <- data.frame(
      Period = rep(as.character(p$Period[t]), nrow(families)),
      Product_Family = families$Family,
      Setup_Cost = families$Setup_Cost,
      Annual_Usage = families$Annual_Usage,
      Minimum_Units = families$Minimum_Units,
      Maximum_Units = families$Maximum_Units,
      Family_Production = q,
      Production_Share_Percent = if (x > 0) 100 * q / x else rep(0, length(q)),
      Annualized_Setup_Objective_Term = obj_terms,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    period_summary[[t]] <- data.frame(
      Period = as.character(p$Period[t]),
      Aggregate_Regular_Production = x,
      Family_Production_Total = sum(q),
      Annualized_Setup_Objective = sol$objective,
      stringsAsFactors = FALSE
    )
  }
  list(aggregate = aggregate,
       details = do.call(rbind, family_details),
       period_summary = do.call(rbind, period_summary),
       families = families,
       total_cost = aggregate$total_cost,
       whole_workers = whole_workers)
}

aggregate_disaggregation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(class = "page-header",
      shiny::div(class = "top-tag", "AGGREGATE PLANNING"),
      shiny::h2("Disaggregation - Textbook Square-Root Rule"),
      shiny::p(class = "page-description",
        "Disaggregate period production into product families by minimizing setup cost, following textbook Section 3.9.")),
    shiny::div(class = "content-panel",
      shiny::h4("1. Product Families and Setup Data"),
      shiny::p("One family per line: Family Name, Setup Cost (K), Annual Usage Rate (lambda). ",
               "Optionally add Minimum Quantity (a) and Maximum Quantity (b) for each period."),
      shiny::textAreaInput(ns("family_spec"), "Family, Setup Cost, Annual Usage [optional: Min, Max]",
        value = paste("Family A, 9, 1", "Family B, 4, 1", "Family C, 1, 1", sep = "\n"),
        rows = 5, width = "100%"),
      shiny::p("Example with limits: Family A, 9, 1, 10, 1000. ",
               "When limits are omitted, minimum = 0 and maximum = unlimited."),
      shiny::p("Model: minimize sum(K_j * lambda_j / Y_j), subject to sum(Y_j) = X* ",
               "and a_j <= Y_j <= b_j. Without binding limits, Y_j is proportional to sqrt(K_j * lambda_j)."),
      shiny::h4("2. Choose an Aggregate Plan"),
      shiny::p("The plan uses the data applied in Aggregate Planning Data Center."),
      shiny::selectInput(ns("plan_method"), "Aggregate Planning Method",
        choices = c("Constant-Workforce (Level)" = "level",
                    "Zero-Inventory (Chase)" = "chase",
                    "Mixed (Selected Workforce Change Periods)" = "mixed"),
        selected = "level"),
      shiny::uiOutput(ns("mixed_controls")),
      shiny::actionButton(ns("calculate"), "Calculate Disaggregation", class = "btn-primary"),
      shiny::p(style = "margin-top:12px;", shiny::textOutput(ns("status")))
    ),
    shiny::div(class = "content-panel",
      shiny::h4("3. Product Family Inputs"),
      shiny::div(style = "overflow-x:auto;", shiny::tableOutput(ns("family_table")))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::div(class = "content-panel",
        shiny::h4("4. Fractional-Worker Aggregate Plan"),
        shiny::div(style = "overflow-x:auto;", shiny::tableOutput(ns("fractional_plan"))),
        shiny::h4("Aggregate Operating Cost Breakdown"),
        shiny::tableOutput(ns("fractional_costs")),
        shiny::strong(shiny::textOutput(ns("fractional_total")))
      )),
      shiny::column(6, shiny::div(class = "content-panel",
        shiny::h4("5. Whole-Worker Aggregate Plan (Rounded Up)"),
        shiny::div(style = "overflow-x:auto;", shiny::tableOutput(ns("whole_plan"))),
        shiny::h4("Aggregate Operating Cost Breakdown"),
        shiny::tableOutput(ns("whole_costs")),
        shiny::strong(shiny::textOutput(ns("whole_total")))
      ))
    ),
    shiny::fluidRow(
      shiny::column(6, shiny::div(class = "content-panel",
        shiny::h4("6. Fractional-Plan Family Production"),
        shiny::div(style = "overflow-x:auto;", shiny::tableOutput(ns("fractional_family_plan"))),
        shiny::h4("Setup Objective by Period"),
        shiny::tableOutput(ns("fractional_objectives"))
      )),
      shiny::column(6, shiny::div(class = "content-panel",
        shiny::h4("7. Whole-Worker Family Production"),
        shiny::div(style = "overflow-x:auto;", shiny::tableOutput(ns("whole_family_plan"))),
        shiny::h4("Setup Objective by Period"),
        shiny::tableOutput(ns("whole_objectives"))
      ))
    ),
    shiny::div(class = "content-panel",
      shiny::h4("8. Cumulative Production vs. Cumulative Net Demand"),
      shiny::plotOutput(ns("comparison_plot"), height = "400px")
    ),
    shiny::div(class = "content-panel",
      shiny::h4("Textbook Model Notes"),
      shiny::p("The textbook formula optimizes product-family production quantities Y_j ",
               "for an aggregate total X*; it does not specify family-level workforce, ",
               "demand, or inventory. These are therefore shown at the aggregate level only."),
      shiny::p("Setup objective terms K_j * lambda_j / Y_j use the annual usage rate, ",
               "so each row is an annualized objective for that period's allocation, ",
               "not an extra horizon operating cost. Do not add these figures to total ",
               "aggregate plan cost without a consistent time and cost basis."),
      shiny::p("When regular production is zero, the family production allocation is zero ",
               "and its setup objective is not evaluated. Quantity bounds are checked ",
               "separately for every planning period."),
      shiny::p("The whole-worker plan rounds the PLANT workforce up and recalculates ",
               "the aggregate plan before disaggregation.")
    )
  )
}

aggregate_disaggregation_server <- function(id, shared_aggregate) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(
      "Apply data in Aggregate Planning Data Center, then calculate disaggregation.")
    output$status <- shiny::renderText(status())

    output$mixed_controls <- shiny::renderUI({
      if (!identical(input$plan_method, "mixed")) return(NULL)
      d <- shared_aggregate()
      if (is.null(d)) return(shiny::p("Apply aggregate planning data first."))
      periods <- as.character(d$periods$period)
      if (length(periods) <= 1L) return(shiny::p("One period: no change periods needed."))
      idx <- seq.int(2L, length(periods))
      shiny::checkboxGroupInput(session$ns("change_periods"),
        "Periods where a new workforce level begins:",
        choices = stats::setNames(as.character(idx), paste0(idx, ": ", periods[-1L])))
    })

    shiny::observeEvent(input$calculate, {
      tryCatch({
        d <- shared_aggregate()
        if (is.null(d)) stop("First upload and apply data in Aggregate Planning Data Center.")
        fam <- aggregate_disaggregation_parse_families(input$family_spec)
        changes <- if (identical(input$plan_method, "mixed")) {
          as.integer(input$change_periods)
        } else integer()
        frac <- aggregate_disaggregation_calculate(d, fam, input$plan_method,
                                                   changes, whole_workers = FALSE)
        whole <- aggregate_disaggregation_calculate(d, fam, input$plan_method,
                                                    changes, whole_workers = TRUE)
        result(list(fractional = frac, whole = whole))
        status("Disaggregation calculated. Period production totals and quantity bounds were verified.")
      }, error = function(e) {
        result(NULL)
        status(paste("Could not calculate disaggregation:", conditionMessage(e)))
        shiny::showNotification(conditionMessage(e), type = "error", duration = NULL)
      })
    })

    rounded <- function(d) {
      numeric_columns <- vapply(d, is.numeric, logical(1))
      d[numeric_columns] <- lapply(d[numeric_columns], function(x) round(x, 2))
      d
    }
    cost_table <- function(r) {
      data.frame(`Cost Item` = names(r$aggregate$costs),
                 Amount = round(unname(r$aggregate$costs), 2),
                 check.names = FALSE, stringsAsFactors = FALSE)
    }
    output$family_table <- shiny::renderTable({
      shiny::req(result())
      rounded(result()$fractional$families)
    }, striped = TRUE, bordered = TRUE)
    output$fractional_plan <- shiny::renderTable({
      shiny::req(result())
      rounded(result()$fractional$aggregate$plan)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    output$whole_plan <- shiny::renderTable({
      shiny::req(result())
      rounded(result()$whole$aggregate$plan)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    output$fractional_costs <- shiny::renderTable({
      shiny::req(result())
      cost_table(result()$fractional)
    }, striped = TRUE, bordered = TRUE)
    output$whole_costs <- shiny::renderTable({
      shiny::req(result())
      cost_table(result()$whole)
    }, striped = TRUE, bordered = TRUE)
    output$fractional_total <- shiny::renderText({
      r <- shiny::req(result())$fractional
      sprintf("Total Aggregate Operating Cost: %.2f | Ending Inventory: %.2f",
              r$total_cost, r$aggregate$ending_inventory)
    })
    output$whole_total <- shiny::renderText({
      r <- shiny::req(result())$whole
      sprintf("Total Aggregate Operating Cost: %.2f | Ending Inventory: %.2f",
              r$total_cost, r$aggregate$ending_inventory)
    })
    output$fractional_family_plan <- shiny::renderTable({
      shiny::req(result())
      rounded(result()$fractional$details)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    output$whole_family_plan <- shiny::renderTable({
      shiny::req(result())
      rounded(result()$whole$details)
    }, striped = TRUE, bordered = TRUE, spacing = "xs")
    output$fractional_objectives <- shiny::renderTable({
      shiny::req(result())
      rounded(result()$fractional$period_summary)
    }, striped = TRUE, bordered = TRUE)
    output$whole_objectives <- shiny::renderTable({
      shiny::req(result())
      rounded(result()$whole$period_summary)
    }, striped = TRUE, bordered = TRUE)

    output$comparison_plot <- shiny::renderPlot({
      r <- shiny::req(result())
      frac <- r$fractional$aggregate
      whole <- r$whole$aggregate
      t <- seq_len(nrow(frac$plan))
      y1 <- frac$cumulative_production
      y2 <- whole$cumulative_production
      yd <- frac$cumulative_net_demand
      lim <- range(c(0, y1, y2, yd), finite = TRUE)
      if (diff(lim) < 1e-8) lim <- lim + c(-1, 1)
      graphics::plot(t, y1, type = "o", pch = 16, lwd = 2,
                     col = "#A78BFA", xaxt = "n", ylim = lim,
                     xlab = "Planning Period", ylab = "Cumulative Units")
      graphics::axis(1, at = t, labels = as.character(frac$plan$Period),
                     las = 2, cex.axis = 0.8)
      graphics::lines(t, y2, type = "o", pch = 17, lwd = 2,
                      col = "#1E293B")
      graphics::lines(t, yd, type = "o", pch = 15, lwd = 2,
                      col = "#64748B", lty = 2)
      graphics::grid(col = "#E5E7EB")
      graphics::legend("topleft",
        legend = c("Fractional Plan Production", "Whole-Worker Plan Production",
                   "Cumulative Net Demand"),
        col = c("#A78BFA", "#1E293B", "#64748B"),
        pch = c(16, 17, 15), lty = c(1, 1, 2), bty = "n", cex = 0.82)
    })
  })
}
