# Function to calculate fuel cost per mile for any vehicle
calculate_fuel_cost <- function(fuel_price, mpg) {
    fuel_price / mpg
}

# Function to calculate savings per mile compared to current van
calculate_savings_per_mile <- function(current_van_fuel_cost, new_vehicle_fuel_cost) {
    current_van_fuel_cost - new_vehicle_fuel_cost
}

# Function to calculate payback period
calculate_payback_period <- function(new_vehicle_cost, trade_in_value, annual_miles, savings_per_mile) {
    net_cost <- new_vehicle_cost - trade_in_value
    years_to_payback <- net_cost / (annual_miles * savings_per_mile)
    years_to_payback
}

# Main function to calculate all metrics
vehicle_cost_analysis <- function(current_van_mpg, new_vehicle_cost, new_vehicle_mpg, 
                                  trade_in_value, gas_price, electricity_price, 
                                  leaf_miles_per_kwh, annual_miles, is_electric = FALSE) {
    current_van_fuel_cost <- calculate_fuel_cost(gas_price, current_van_mpg)
    
    if (is_electric) {
        new_vehicle_fuel_cost <- electricity_price / leaf_miles_per_kwh
    } else {
        new_vehicle_fuel_cost <- calculate_fuel_cost(gas_price, new_vehicle_mpg)
    }
    
    savings_per_mile <- calculate_savings_per_mile(current_van_fuel_cost, new_vehicle_fuel_cost)
    payback_period <- calculate_payback_period(new_vehicle_cost, trade_in_value, annual_miles, savings_per_mile)
    
    list(
        current_van_fuel_cost_per_mile = current_van_fuel_cost,
        new_vehicle_fuel_cost_per_mile = new_vehicle_fuel_cost,
        savings_per_mile = savings_per_mile,
        annual_savings = savings_per_mile * annual_miles,
        payback_period_years = payback_period
    )
}