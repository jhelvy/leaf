library(shiny)
library(ggplot2)

source('functions.R')

plotColors <- c("dodgerblue", "firebrick")

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            numericInput("current_van_mpg", "Current Van MPG", value = 18, min = 0),
            numericInput("leaf_cost", "Nissan Leaf Price ($)", value = 5000, min = 0),
            numericInput("hybrid_cost", "Hybrid Minivan Price ($)", value = 50000, min = 0),
            numericInput("hybrid_mpg", "Hybrid Minivan MPG", value = 45, min = 0),
            numericInput("trade_in_value", "Current Van Trade-in Value ($)", value = 18000, min = 0),
            numericInput("gas_price", "Gas Price ($/gallon)", value = 3.00, min = 0, step = 0.01),
            numericInput("electricity_price", "Electricity Price ($/kWh)", value = 0.14, min = 0, step = 0.01),
            numericInput("leaf_miles_per_kwh", "Leaf Miles per kWh", value = 3.5, min = 0, step = 0.1),
            numericInput("annual_miles", "Annual Miles", value = 8000, min = 0)
        ),
        
        mainPanel(
            h3("Results"),
            verbatimTextOutput("results"),
            plotOutput("comparison_plot")
        )
    )
)

server <- function(input, output) {
    results <- reactive({
        leaf_results <- vehicle_cost_analysis(
            current_van_mpg = input$current_van_mpg,
            new_vehicle_cost = input$leaf_cost,
            new_vehicle_mpg = NA,
            trade_in_value = 0,  # Assuming Leaf is additional vehicle
            gas_price = input$gas_price,
            electricity_price = input$electricity_price,
            leaf_miles_per_kwh = input$leaf_miles_per_kwh,
            annual_miles = input$annual_miles,
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
            is_electric = FALSE
        )
        
        list(leaf = leaf_results, hybrid = hybrid_results)
    })
    
    output$results <- renderPrint({
        res <- results()
        cat("Current Van Fuel Cost per Mile: $", sprintf("%.3f", res$leaf$current_van_fuel_cost_per_mile), "\n\n")
        cat("Nissan Leaf:\n")
        cat("  Electricity Cost per Mile: $", sprintf("%.3f", res$leaf$new_vehicle_fuel_cost_per_mile), "\n")
        cat("  Savings per Mile: $", sprintf("%.3f", res$leaf$savings_per_mile), "\n")
        cat("  Annual Savings: $", sprintf("%.2f", res$leaf$annual_savings), "\n")
        cat("  Payback Period: ", sprintf("%.2f", res$leaf$payback_period_years), " years\n\n")
        cat("Hybrid Minivan:\n")
        cat("  Fuel Cost per Mile: $", sprintf("%.3f", res$hybrid$new_vehicle_fuel_cost_per_mile), "\n")
        cat("  Savings per Mile: $", sprintf("%.3f", res$hybrid$savings_per_mile), "\n")
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

        ggplot(data, aes(x = Years, y = Savings, color = Vehicle)) +
            geom_line(size = 1) +
            geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
            labs(
                title = "Cumulative Savings Comparison",
                x = "Years",
                y = "Cumulative Savings ($)"
            ) +
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            scale_y_continuous(labels = scales::dollar) +
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
                y = min(leaf_savings, hybrid_savings), 
                label = sprintf("Leaf Payback: %.2f years", res$leaf$payback_period_years),
                vjust = 1.5, 
                hjust = 0, 
                color = plotColors[1]
            ) +
            annotate(
                "text", 
                x = res$hybrid$payback_period_years, 
                y = max(leaf_savings, hybrid_savings), 
                label = sprintf("Hybrid Payback: %.2f years", res$hybrid$payback_period_years),
                vjust = -0.5, 
                hjust = 1, 
                color = plotColors[2]
            )
    })
}

shinyApp(ui = ui, server = server)