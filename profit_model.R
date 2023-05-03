source(here::here("almond_yield_function.R"))

# Define the profit model function
#' Profit model function for almond yield anomaly
#'
#' @param market_price Market price of almonds in USD per ton
#' @param cost_of_production Cost in USD to produce one ton of almonds 
#' @param climate_data A data frame of columns including month, day, year and climate variables such as maximum temperature(Celsius), minimum temperature(Celsius), and precipitation(mm)
#' @param temp_min_constant_1 Minimum temperature constant 1 for almond yield
#' @param temp_min_constant_2 Minimum temperature constant 2 for almond yield
#' @param precip_constant_1 Precipitation constant 1 for almond yield
#' @param precip_constant_2 Precipitation constant 2 for almond yield
#' @param constant Constant provided for the almond yield equation from Table 2 in Lobell et al.
#'
#' @return
#' @export
#'
#' @examples
almond_profit <- function(climate_data, market_price = 4036, cost_of_production = 3600, temp_min_constant_1 = -0.015, temp_min_constant_2 = -0.0046, precip_constant_1 = -.07, precip_constant_2 = 0.0043, constant = 0.28) {
  

# Calculate the yield anomaly for a given year and store as variable
yield <- almond_model(climate_data, temp_min_constant_1, temp_min_constant_2, precip_constant_1, precip_constant_2, constant)  
# Calculate the revenue based on the market price and expected yield
revenue <- yield * market_price
  
# Calculate the total cost of production based on the cost of production and expected yield
total_cost <- yield * cost_of_production
  
# Calculate the profit as revenue minus total cost m
profit <- revenue - total_cost
  
# Return the profit as a list
return(list(profit, mean_profit = mean(profit)))
}



