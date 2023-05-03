#' Almond Yield as a Function
#'
#'Calculate the almond yield in acres per ton
#' @param climate_data A data frame of columns including month, day, year and climate variables such as maximum temperature(Celsius), minimum temperature(Celsius), and precipitation(mm)
#' @param temp_min_constant_1 Minimum temperature constant 1 for almond yield
#' @param temp_min_constant_2 Minimum temperature constant 2 for almond yield
#' @param precip_constant_1 Precipitation constant 1 for almond yield
#' @param precip_constant_2 Precipitation constant 2 for almond yield
#' @param constant Constant provided for the almond yield equation from Table 2 in Lobell et al.
#'
#' @return the mean, min, and max almond  yield in acres per tons
#' @export
#'
#' @examples almond_model(climate_data)
almond_model <- function(climate_data, temp_min_constant_1 = -0.015, temp_min_constant_2 = -0.0046, precip_constant_1 = -.07, precip_constant_2 = 0.0043, constant = 0.28) {
  
  # Filter for January precipitation
  jan_precip <- climate_data |> 
    filter(month == 1) |> 
    group_by(month, wy) |> 
    summarize(precip_sum = sum(precip, na.rm = TRUE))
  
  #Filter for February temperature
  feb_temp <- climate_data |> 
    filter(month == 2) |> 
    group_by(month, wy) |> 
    summarize(temp_min = mean(tmin_c, na.rm = TRUE))
  
  # Join the temperature and precipitaion dataframes by water year
  joined_table <- left_join(jan_precip, feb_temp, by = "wy") |> 
    select(wy, precip_sum, temp_min)
  
  # Calculate yield from the equation in Table 2 using Lobell et al.
  yield <- joined_table |>  
    mutate(almond_yield = (temp_min_constant_1 * temp_min) + (temp_min_constant_2 * (temp_min^2)) + (precip_constant_1 * precip_sum) + (precip_constant_2 * (precip_sum^2)) + constant)
  
  # # Calculate the mean, maximum, and minimun yield
  # mean_yield <- mean(yield$almond_yield)
  # max_yield <- max(yield$almond_yield)
  # min_yield <- min(yield$almond_yield)
  
  # Return the year, month, mean yield, max yield, and min yield as a list
  return(yield$almond_yield)
}
