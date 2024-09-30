library(shiny)
library(ggplot2)

# Custom functions ----

calculate_fuel_cost <- function(fuel_price, mpg) {
  fuel_price / mpg
}

calculate_savings_per_mile <- function(
    current_van_fuel_cost, new_vehicle_fuel_cost
) {
  current_van_fuel_cost - new_vehicle_fuel_cost
}

calculate_payback_period <- function(
    new_vehicle_cost, trade_in_value, annual_savings
) {
  net_cost <- new_vehicle_cost - trade_in_value
  years_to_payback <- net_cost / annual_savings
  years_to_payback
}

vehicle_cost_analysis <- function(
    current_van_mpg, new_vehicle_cost, new_vehicle_mpg,
    trade_in_value, gas_price, electricity_price,
    leaf_miles_per_kwh, annual_miles, leaf_miles_percentage, 
    is_electric = FALSE
) {
  current_van_fuel_cost <- calculate_fuel_cost(gas_price, current_van_mpg)

  if (is_electric) {
    leaf_fuel_cost <- electricity_price / leaf_miles_per_kwh
    leaf_miles <- annual_miles * (leaf_miles_percentage / 100)
    van_miles <- annual_miles * (1 - leaf_miles_percentage / 100)

    annual_cost_current <- (leaf_miles * current_van_fuel_cost) + (van_miles * current_van_fuel_cost)
    annual_cost_new <- (leaf_miles * leaf_fuel_cost) + (van_miles * current_van_fuel_cost)
  } else {
    new_vehicle_fuel_cost <- calculate_fuel_cost(gas_price, new_vehicle_mpg)
    annual_cost_current <- annual_miles * current_van_fuel_cost
    annual_cost_new <- annual_miles * new_vehicle_fuel_cost
  }

  annual_savings <- annual_cost_current - annual_cost_new
  payback_period <- calculate_payback_period(new_vehicle_cost, trade_in_value, annual_savings)

  list(
    current_van_fuel_cost_per_mile = current_van_fuel_cost,
    new_vehicle_fuel_cost_per_mile = if (is_electric) leaf_fuel_cost else new_vehicle_fuel_cost,
    annual_savings = annual_savings,
    payback_period_years = payback_period
  )
}

# App UI ----

plotColors <- c("dodgerblue", "firebrick")


ui <- fluidPage(
  titlePanel("Cost Comparison"),
  p("Should I replace my 2014 Toyota Sienna with a new hybrid? Or keep it and just buy a used Nissan Leaf EV?"),

  # Four columns for input groups
  fluidRow(
    column(
      3,
      h4("General Parameters"),
      numericInput("annual_miles", "Annual Miles", value = 10000, min = 0),
      numericInput("gas_price", "Gas Price ($/gallon)", value = 3.00, min = 0, step = 0.01),
      numericInput("electricity_price", "Electricity Price ($/kWh)", value = 0.14, min = 0, step = 0.01)
    ),
    column(
      3,
      h4("Current Sienna"),
      numericInput("current_van_mpg", "Average MPG", value = 18, min = 0),
      numericInput("trade_in_value", "Trade-in Value ($)", value = 18000, min = 0)
    ),
    column(
      3,
      h4("Hybrid Sienna"),
      numericInput("hybrid_cost", "Purchase Price ($)", value = 50000, min = 0),
      numericInput("hybrid_mpg", "Average MPG", value = 45, min = 0)
    ),
    column(
      3,
      h4("Nissan Leaf"),
      numericInput("leaf_cost", "Purchase Price ($)", value = 5000, min = 0),
      numericInput("leaf_miles_per_kwh", "Miles per kWh", value = 3.5, min = 0, step = 0.1),
      sliderInput("leaf_miles_percentage", "% of Miles Driven by Leaf", min = 0, max = 100, value = 80)
    )
  ),

  # Results and plot below the inputs
  fluidRow(
    column(
      6,
      h3("Results"),
      verbatimTextOutput("results")
    ),
    column(
      6,
      h3(""),
      plotOutput("comparison_plot", width = "100%", height = "400px")
    )
  )
)

# App Server ----

server <- function(input, output) {
  results <- reactive({
    leaf_results <- vehicle_cost_analysis(
      current_van_mpg = input$current_van_mpg,
      new_vehicle_cost = input$leaf_cost,
      new_vehicle_mpg = NA,
      trade_in_value = 0, # Assuming Leaf is additional vehicle
      gas_price = input$gas_price,
      electricity_price = input$electricity_price,
      leaf_miles_per_kwh = input$leaf_miles_per_kwh,
      annual_miles = input$annual_miles,
      leaf_miles_percentage = input$leaf_miles_percentage,
      is_electric = TRUE
    )

    hybrid_results <- vehicle_cost_analysis(
      current_van_mpg = input$current_van_mpg,
      new_vehicle_cost = input$hybrid_cost,
      new_vehicle_mpg = input$hybrid_mpg,
      trade_in_value = input$trade_in_value,
      gas_price = input$gas_price,
      electricity_price = NA,
      leaf_miles_per_kwh = NA,
      annual_miles = input$annual_miles,
      leaf_miles_percentage = NA,
      is_electric = FALSE
    )

    list(leaf = leaf_results, hybrid = hybrid_results)
  })

  output$results <- renderPrint({
    res <- results()
    cat("Current Van Fuel Cost per Mile: $", sprintf("%.3f", res$leaf$current_van_fuel_cost_per_mile), "\n\n")
    cat("Nissan Leaf:\n")
    cat("  Electricity Cost per Mile: $", sprintf("%.3f", res$leaf$new_vehicle_fuel_cost_per_mile), "\n")
    cat("  Annual Savings: $", sprintf("%.2f", res$leaf$annual_savings), "\n")
    cat("  Payback Period: ", sprintf("%.2f", res$leaf$payback_period_years), " years\n\n")
    cat("Hybrid Minivan:\n")
    cat("  Fuel Cost per Mile: $", sprintf("%.3f", res$hybrid$new_vehicle_fuel_cost_per_mile), "\n")
    cat("  Annual Savings: $", sprintf("%.2f", res$hybrid$annual_savings), "\n")
    cat("  Payback Period: ", sprintf("%.2f", res$hybrid$payback_period_years), " years")
  })

  output$comparison_plot <- renderPlot({
      res <- results()
      years <- seq(0, max(res$leaf$payback_period_years, res$hybrid$payback_period_years) * 1.2, by = 0.5)
      
      leaf_savings <- cumsum(c(0, rep(res$leaf$annual_savings, length(years) - 1))) - input$leaf_cost
      hybrid_savings <- cumsum(c(0, rep(res$hybrid$annual_savings, length(years) - 1))) - (input$hybrid_cost - input$trade_in_value)
      
      data <- data.frame(
          Years = rep(years, 2),
          Savings = c(leaf_savings, hybrid_savings),
          Vehicle = rep(c("Nissan Leaf", "Hybrid Minivan"), each = length(years))
      )
      
      y_range <- range(c(leaf_savings, hybrid_savings))
      y_padding <- diff(y_range) * 0.1  # 10% padding
      
      ggplot(data, aes(x = Years, y = Savings, color = Vehicle)) +
          geom_line(size = 1) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
          labs(
              title = "Cumulative Savings Comparison",
              x = "Years",
              y = "Cumulative Savings ($)"
          ) +
          theme_bw(base_size = 16) +
          theme(
              plot.title = element_text(hjust = 0.5),
              plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
          ) +
          scale_y_continuous(
              labels = scales::dollar,
              limits = c(min(y_range) - y_padding, max(y_range) + y_padding)
          ) +
          scale_color_manual(
              values = c(
                  "Nissan Leaf" = plotColors[1], 
                  "Hybrid Minivan" = plotColors[2]
              )
          ) +
          geom_vline(
              xintercept = res$leaf$payback_period_years, 
              linetype = "dotted", 
              color = plotColors[1]
          ) +
          geom_vline(
              xintercept = res$hybrid$payback_period_years, 
              linetype = "dotted", 
              color = plotColors[2]
          ) +
          annotate(
              "text", 
              x = res$leaf$payback_period_years, 
              y = min(y_range) - y_padding/2, 
              label = sprintf("Leaf Payback:\n%.2f years", res$leaf$payback_period_years),
              vjust = 1, 
              hjust = 0, 
              color = plotColors[1]
          ) +
          annotate(
              "text", 
              x = res$hybrid$payback_period_years, 
              y = max(y_range) + y_padding/2, 
              label = sprintf("Hybrid Payback:\n%.2f years", res$hybrid$payback_period_years),
              vjust = 0, 
              hjust = 1, 
              color = plotColors[2]
          )
  }, res = 96, width = 600, height = 400)
}

shinyApp(ui = ui, server = server)
