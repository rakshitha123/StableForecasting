library(tidyverse)


BASE_DIR <- "StableForecasting"


# Execution of helper scripts
source(file.path(BASE_DIR, "utils", "global_model_helper.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "error_calculator.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "interpolate.R", fsep = "/"))
source(file.path(BASE_DIR, "models", "global_models.R", fsep = "/"))



# A wrapper function to execute global forecasting models for interpolation experiments

# Parameters
# input_file_name - Dataset file name
# lag - The number of past lags that should be considered during training
# forecast_horizon - The expected forecast horizon
# dataset_name - Name of the dataset
# num_origins - No: of origins that the forecasts should be generated from
# method - Name of the global model. Current supportive global model names: pooled_regression, lightgbm
do_global_forecasting <- function(input_file_name, lag, forecast_horizon, dataset_name, num_origins, method = "pooled_regression"){
  
  # Start timestamp
  start_time <- Sys.time()
  
  file_name <- paste0(dataset_name, "_", method)
  dir.create(file.path(BASE_DIR, "results", "forecasts", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  
  all_forecasts <- NULL
  all_actuals <- NULL

  for(origin in 1:num_origins){
    # Creating training and test sets for each origin
    num_cut_last_points <- num_origins - origin
    loaded_data <- create_train_test_sets(input_file_name, forecast_horizon, num_cut_last_points)
    training_set <- loaded_data[[1]]
    test_set <- loaded_data[[2]]
    
    if(is.null(all_actuals))
      all_actuals <- test_set
    else  
      all_actuals <- rbind(all_actuals, test_set)
    
    # Forecasting from each origin
    matrices <- create_input_matrices(training_set, lag)
    embedded_series <- matrices[[1]]
    final_lags <- matrices[[2]]
    
    current_origin_forecasts <- as.data.frame(start_forecasting(embedded_series, final_lags, lag, forecast_horizon, method))
    colnames(current_origin_forecasts) <- as.character(1:forecast_horizon)
    
    if(is.null(all_forecasts))
      all_forecasts <- current_origin_forecasts
    else
      all_forecasts <- rbind(all_forecasts, current_origin_forecasts)
  }
  
  colnames(all_actuals) <- as.character(1:forecast_horizon)
  all_actuals$type <- "actual"
  all_forecasts$type <- "forecast"
  
  all_actuals <- cbind(rep(1:length(training_set), num_origins), rep(1:num_origins, each = length(training_set)), all_actuals)
  all_forecasts <- cbind(rep(1:length(training_set), num_origins), rep(1:num_origins, each = length(training_set)), all_forecasts)
  colnames(all_actuals)[1:2] <- c("item_id", "fc_origin") 
  colnames(all_forecasts)[1:2] <- c("item_id", "fc_origin") 
  
  all_actuals <- arrange(all_actuals, item_id)
  all_forecasts <- arrange(all_forecasts, item_id)
  
  # Measuring accuracy
  dir.create(file.path(BASE_DIR, "results", "errors", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  calculate_errors(as.matrix(all_forecasts[,3:(ncol(all_forecasts)-1)]), as.matrix(all_actuals[,3:(ncol(all_actuals)-1)]), file_name)
  
  all_actuals <- rbind(all_actuals, all_forecasts)
  
  # Measuring stability
  calculate_vertical_stability(all_actuals[all_actuals$type == "forecast", 1:(ncol(all_actuals)-1)], file_name)
  calculate_vertical_stability(all_actuals[all_actuals$type == "forecast", 1:(ncol(all_actuals)-1)], file_name, TRUE, TRUE)
  
  calculate_horizontal_stability(all_actuals[all_actuals$type == "forecast", 1:(ncol(all_actuals)-1)], file_name)
  calculate_horizontal_stability(all_actuals[all_actuals$type == "forecast", 1:(ncol(all_actuals)-1)], file_name, TRUE, TRUE)
  
  dir.create(file.path(BASE_DIR, "results", "forecasts", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  write.csv(all_actuals, file.path(BASE_DIR, "results", "forecasts", paste0(file_name, "_forecasts.csv")), row.names = FALSE)
  
  # Finish timestamp
  end_time <- Sys.time()
  
  # Execution time
  dir.create(file.path(BASE_DIR, "results", "execution_times", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  exec_time <- end_time - start_time
  print(exec_time)
  write(paste(exec_time, attr(exec_time, "units")), file = file.path(BASE_DIR, "results", "execution_times", paste0(file_name, ".txt"), fsep = "/"), append = FALSE)
}



# Experiments

# Running base models
do_global_forecasting("m3_monthly_dataset.txt", 15, 6, "m3_monthly", 13, "pooled_regression")
do_global_forecasting("m3_monthly_dataset.txt", 15, 6, "m3_monthly", 13, "lightgbm")

do_global_forecasting("m4_monthly_dataset.txt", 15, 6, "m4_monthly", 13, "pooled_regression")
do_global_forecasting("m4_monthly_dataset.txt", 15, 6, "m4_monthly", 13, "lightgbm")

do_global_forecasting("favorita_dataset.txt", 9, 6, "favorita", 11, "pooled_regression")
do_global_forecasting("favorita_dataset.txt", 9, 6, "favorita", 11, "lightgbm")

do_global_forecasting("m5_items.txt", 9, 16, "m5_items", 13, "pooled_regression")
do_global_forecasting("m5_items.txt", 9, 16, "m5_items", 13, "lightgbm")



############## Before running the following interpolation experiments, execute the base models using "do_global_forecasting" function and get base model forecasts

# Partial Interpolation experiments 

# Vertical Stability

vertical_partial_interpolate("results/forecasts/m3_monthly_pooled_regression_forecasts.csv", "m3_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1)) 
vertical_partial_interpolate("results/forecasts/m3_monthly_lightgbm_forecasts.csv", "m3_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))

vertical_partial_interpolate("results/forecasts/m4_monthly_pooled_regression_forecasts.csv", "m4_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1)) 
vertical_partial_interpolate("results/forecasts/m4_monthly_lightgbm_forecasts.csv", "m4_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))

vertical_partial_interpolate("results/forecasts/favorita_pooled_regression_forecasts.csv", "favorita_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1)) 
vertical_partial_interpolate("results/forecasts/favorita_lightgbm_forecasts.csv", "favorita_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))

vertical_partial_interpolate("results/forecasts/m5_items_pooled_regression_forecasts.csv", "m5_items_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1)) 
vertical_partial_interpolate("results/forecasts/m5_items_lightgbm_forecasts.csv", "m5_items_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))



# Horizontal Stability

horizontal_partial_interpolate("results/forecasts/m3_monthly_pooled_regression_forecasts.csv", "m3_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1)) 
horizontal_partial_interpolate("results/forecasts/m3_monthly_lightgbm_forecasts.csv", "m3_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))
 
horizontal_partial_interpolate("results/forecasts/m4_monthly_pooled_regression_forecasts.csv", "m4_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1)) 
horizontal_partial_interpolate("results/forecasts/m4_monthly_lightgbm_forecasts.csv", "m4_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))
 
horizontal_partial_interpolate("results/forecasts/favorita_pooled_regression_forecasts.csv", "favorita_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1)) 
horizontal_partial_interpolate("results/forecasts/favorita_lightgbm_forecasts.csv", "favorita_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))
 
horizontal_partial_interpolate("results/forecasts/m5_items_pooled_regression_forecasts.csv", "m5_items_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1)) 
horizontal_partial_interpolate("results/forecasts/m5_items_lightgbm_forecasts.csv", "m5_items_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))



# Full Interpolation experiments

# Vertical Stability

vertical_full_interpolate("results/forecasts/m3_monthly_pooled_regression_forecasts.csv", "m3_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1)) 
vertical_full_interpolate("results/forecasts/m3_monthly_lightgbm_forecasts.csv", "m3_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))

vertical_full_interpolate("results/forecasts/m4_monthly_pooled_regression_forecasts.csv", "m4_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1)) 
vertical_full_interpolate("results/forecasts/m4_monthly_lightgbm_forecasts.csv", "m4_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))

vertical_full_interpolate("results/forecasts/favorita_pooled_regression_forecasts.csv", "favorita_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1)) 
vertical_full_interpolate("results/forecasts/favorita_lightgbm_forecasts.csv", "favorita_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))

vertical_full_interpolate("results/forecasts/m5_items_pooled_regression_forecasts.csv", "m5_items_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1)) 
vertical_full_interpolate("results/forecasts/m5_items_lightgbm_forecasts.csv", "m5_items_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))



# Horizontal Stability

horizontal_full_interpolate("results/forecasts/m3_monthly_pooled_regression_forecasts.csv", "m3_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))
horizontal_full_interpolate("results/forecasts/m3_monthly_lightgbm_forecasts.csv", "m3_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))

horizontal_full_interpolate("results/forecasts/m4_monthly_pooled_regression_forecasts.csv", "m4_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))
horizontal_full_interpolate("results/forecasts/m4_monthly_lightgbm_forecasts.csv", "m4_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))

horizontal_full_interpolate("results/forecasts/favorita_pooled_regression_forecasts.csv", "favorita_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))
horizontal_full_interpolate("results/forecasts/favorita_lightgbm_forecasts.csv", "favorita_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))

horizontal_full_interpolate("results/forecasts/m5_items_pooled_regression_forecasts.csv", "m5_items_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))
horizontal_full_interpolate("results/forecasts/m5_items_lightgbm_forecasts.csv", "m5_items_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1))






