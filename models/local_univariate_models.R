library(forecast)

# Implementations of a set of univariate forecasting models
#
# Each function takes 2 parameters
# time_series - a ts object representing the time series that should be used with model training
# forecast_horizon - expected forecast horizon
#
# If a model fails to provide forecasts, it will return snaive forecasts


# Calculate ets forecasts
get_ets_forecasts <- function(time_series, forecast_horizon){
  tryCatch(
    forecast(forecast:::ets(time_series), h = forecast_horizon)$mean
  ,error = function(e) {
    warning(e)
    get_snaive_forecasts(time_series, forecast_horizon)
  })
}


# Calculate auto.arima forecasts
get_arima_forecasts <- function(time_series, forecast_horizon){
  
  tryCatch({
    fit <- forecast:::auto.arima(time_series, lambda = 0)
  }, error = function(e) {
      tryCatch({
        fit <<- forecast:::auto.arima(time_series)
      }, error = function(e){
          fit <<- forecast:::auto.arima(time_series, seasonal = FALSE)
      })
  })
    
  tryCatch({
    forecast:::forecast.Arima(fit, h = forecast_horizon)$mean
  }, error = function(e) { 
      warning(e)
      get_snaive_forecasts(time_series, forecast_horizon)
  })
}


# Calculate snaive forecasts
get_snaive_forecasts <- function(time_series, forecast_horizon){
  forecast:::snaive(time_series, h = forecast_horizon)$mean
}
