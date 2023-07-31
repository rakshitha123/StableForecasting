# Implementation of global models

library(glmnet)
library(lightgbm)

set.seed(1)


# Forecasting with different lags
start_forecasting <- function(embedded_series, final_lags, lag, forecast_horizon, method = "pooled_regression"){
  # Fitting a global model
  model <- fit_model(embedded_series, method)
  
  # Do forecasting
  forec_recursive(model, lag, final_lags, forecast_horizon, method)
}


# Fit a global model
fit_model <- function(fitting_data, method = "pooled_regression") {
  # Create the formula
  formula <- create_formula(fitting_data)
 
  if(method == "pooled_regression")
    model <- glm(formula = formula, data = fitting_data)
  else if(method == "lightgbm"){
    model <- lightgbm(data = as.matrix(fitting_data[,-1]), 
                      label = as.matrix(fitting_data[,1]), 
                      params = list(objective = "regression",
                                     metric = "rmse",
                                     boosting_type = "gbdt",
                                     learning_rate = 0.075,
                                     min_data_in_leaf = 100,
                                     early_stopping_rounds = 30,
                                     n_estimators = 100,
                                     verbosity = -1))
  }
  
  model
}


# Recursive forecasting of the series until a given horizon
forec_recursive <- function(model, lag, final_lags, forecast_horizon, method = "pooled_regression") {
  
  # This will store the predictions corresponding with each horizon
  predictions <- NULL
  
  for (i in 1:forecast_horizon){
   
    # Get predictions for the current horizon
    if(method == "pooled_regression")
      new_predictions <- predict.glm(object = model, newdata = as.data.frame(final_lags)) 
    else if(method == "lightgbm")
      new_predictions <- predict(model, as.matrix(final_lags))
    
    
    # Adding the current forecasts to the final predictions matrix
    predictions <- cbind(predictions, new_predictions)
    
    # Updating the test set for the next horizon
    if(i < forecast_horizon){
      final_lags <- final_lags[-lag]
      final_lags <- cbind(new_predictions, final_lags)
      colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep="")
      final_lags <- as.data.frame(final_lags)
    }
  }
  
  predictions
}
