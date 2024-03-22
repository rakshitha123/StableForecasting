library(tidyverse)
library(forecast)
library(parallel)
library(doParallel)
library(foreach)


BASE_DIR <- "StableForecasting"


# Execution of helper scripts
source(file.path(BASE_DIR, "utils", "global_model_helper.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "error_calculator.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "interpolate.R", fsep = "/"))
source(file.path(BASE_DIR, "models", "global_models.R", fsep = "/"))
source(file.path(BASE_DIR, "models", "local_univariate_models.R", fsep = "/"))



# A wrapper function to execute base forecasting models for vertical interpolation experiments.

# Parameters
# input_file_name - Dataset file name
# type - whether the model is a local or global model
# forecast_horizon - The expected forecast horizon
# dataset_name - Name of the dataset
# num_origins - No: of origins that the forecasts should be generated from
# seasonality - frequency of the dataset (e.g. 12 for monthly data)
# method - Name of the model. Current supportive model names: pooled_regression, lightgbm, ets, arima
# lag - The number of past lags that should be considered during training (only requires for global models)
do_forecasting <- function(input_file_name, type, forecast_horizon, dataset_name, num_origins, seasonality, method = "pooled_regression", lag = NULL){
  
  # Start timestamp
  start_time <- Sys.time()
  
  file_name <- paste0(dataset_name, "_", method)
  
  dir.create(file.path(BASE_DIR, "results", "forecasts", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  
  all_forecasts <- NULL
  all_actuals <- NULL
  all_training <- list()

  for(origin in 1:num_origins){
    print(paste0(dataset_name, " Origin = ", origin, " Model = ", method))
    
    # Creating training and test sets for each origin
    num_cut_last_points <- num_origins - origin
    loaded_data <- create_train_test_sets(input_file_name, forecast_horizon, num_cut_last_points)
    training_set <- loaded_data[[1]]
    test_set <- loaded_data[[2]]
    
    all_training[[origin]] <- training_set
    
    if(is.null(all_actuals))
      all_actuals <- test_set
    else  
      all_actuals <- rbind(all_actuals, test_set)
    
    # Forecasting from each origin
    if(type == "global"){
      matrices <- create_input_matrices(training_set, lag)
      embedded_series <- matrices[[1]]
      final_lags <- matrices[[2]]
      
      current_origin_forecasts <- as.data.frame(start_forecasting(embedded_series, final_lags, lag, forecast_horizon, method))
      
    }else{
      numCores <- detectCores()
      cluster <- parallel::makeCluster(numCores)
      registerDoParallel(cluster)
      
      current_origin_forecasts <- foreach (s = 1:length(training_set)) %dopar% { 
        source(file.path("StableForecasting", "models", "local_univariate_models.R", fsep = "/"))
        print(forecast_horizon)
        series <- ts(training_set[[s]], frequency = seasonality)
        as.numeric(eval(parse(text = paste0("get_", method, "_forecasts(series, forecast_horizon)"))))
      }
      
      parallel::stopCluster(cluster)
      
      current_origin_forecasts <- do.call(rbind.data.frame, current_origin_forecasts)
    }
    
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
  
  acc_training <- list()
  
  
  for(s in 1:length(training_set)){
    for(or in 1:length(all_training)){
      acc_training[[length(acc_training)+1]] <- all_training[[or]][[s]]
    }
  }
  
  # Measuring accuracy
  dir.create(file.path(BASE_DIR, "results", "errors", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  calculate_errors(as.matrix(all_forecasts[,3:(ncol(all_forecasts)-1)]), as.matrix(all_actuals[,3:(ncol(all_actuals)-1)]), acc_training, seasonality, file_name)
  
  all_actuals <- rbind(all_actuals, all_forecasts)
  
  # Measuring vertical stability
  calculate_vertical_stability(all_actuals[all_actuals$type == "forecast", 1:(ncol(all_actuals)-1)], all_training, seasonality, file_name)
  calculate_vertical_stability(all_actuals[all_actuals$type == "forecast", 1:(ncol(all_actuals)-1)], all_training, seasonality, file_name, TRUE, TRUE)
  
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


# A wrapper function to execute base forecasting models for horizontal interpolation experiments.
# This first applies STL decomposition on training series and the corresponding remainder components are predicted using the forecasting models.
# Trend components are predicted using ets(ZZN) model.
# Seasonality components are predicted using stlf method.
# The addition of the forecasts of all 3 components is considered as the final forecasts. 

# Parameters
# input_file_name - Dataset file name
# type - whether the model is a local or global model
# forecast_horizon - The expected forecast horizon
# dataset_name - Name of the dataset
# num_origins - No: of origins that the forecasts should be generated from
# seasonality - frequency of the dataset (e.g. 12 for monthly data)
# method - Name of the model. Current supportive model names: pooled_regression, lightgbm, ets, arima
# lag - The number of past lags that should be considered during training (only requires for global models)
do_stl_forecasting <- function(input_file_name, type, forecast_horizon, dataset_name, num_origins, seasonality, method = "pooled_regression", lag = NULL){
  
  # Start timestamp
  start_time <- Sys.time()
  
  file_name <- paste0(dataset_name, "_stl_", method)
  
  dir.create(file.path(BASE_DIR, "results", "forecasts", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  
  all_remainder_forecasts <- NULL
  all_actuals <- NULL
  all_trend_forecasts <- NULL
  all_seasonal_forecasts <- NULL
  all_training <- list()
  
  for(origin in 1:num_origins){
    print(paste0(dataset_name, " stl Origin = ", origin, " Model = ", method))
    
    # Creating training and test sets for each origin
    num_cut_last_points <- num_origins - origin
    loaded_data <- create_train_test_sets(input_file_name, forecast_horizon, num_cut_last_points, TRUE, seasonality)
    training_set <- loaded_data[[1]]
    test_set <- loaded_data[[2]]
    remainder_list <- loaded_data[[3]]
    trend_forecasts <- loaded_data[[4]]
    seasonal_forecasts <-  loaded_data[[5]]
    
    all_training[[origin]] <- training_set

    if(is.null(all_actuals))
      all_actuals <- test_set
    else  
      all_actuals <- rbind(all_actuals, test_set)
    
    if(is.null(all_trend_forecasts))
      all_trend_forecasts <- trend_forecasts
    else  
      all_trend_forecasts <- rbind(all_trend_forecasts, trend_forecasts)
    
    if(is.null(all_seasonal_forecasts))
      all_seasonal_forecasts <- seasonal_forecasts
    else  
      all_seasonal_forecasts <- rbind(all_seasonal_forecasts, seasonal_forecasts)
    
    # Forecasting from each origin
    if(type == "global"){
      matrices <- create_input_matrices(remainder_list, lag)
      embedded_series <- matrices[[1]]
      final_lags <- matrices[[2]]
      
      current_origin_forecasts <- as.data.frame(start_forecasting(embedded_series, final_lags, lag, forecast_horizon, method))
      
    }else{
      numCores <- detectCores()
      cluster <- parallel::makeCluster(numCores)
      registerDoParallel(cluster)
      
      current_origin_forecasts <- foreach (s = 1:length(training_set)) %dopar% {
        source(file.path("StableForecasting", "models", "local_univariate_models.R", fsep = "/"))
        print(forecast_horizon)
        series <- ts(remainder_list[[s]], frequency = seasonality)
        as.numeric(eval(parse(text = paste0("get_", method, "_forecasts(series, forecast_horizon)"))))
      }
      
      parallel::stopCluster(cluster)
      
      current_origin_forecasts <- do.call(rbind.data.frame, current_origin_forecasts)
    }
    
    colnames(current_origin_forecasts) <- as.character(1:forecast_horizon)
    
    if(is.null(all_remainder_forecasts))
      all_remainder_forecasts <- current_origin_forecasts
    else
      all_remainder_forecasts <- rbind(all_remainder_forecasts, current_origin_forecasts)
  }
  
  colnames(all_actuals) <- as.character(1:forecast_horizon)
  colnames(all_trend_forecasts) <- as.character(1:forecast_horizon)
  colnames(all_seasonal_forecasts) <- as.character(1:forecast_horizon)
  
  all_forecasts <- all_remainder_forecasts + all_trend_forecasts + all_seasonal_forecasts
  
  all_actuals$type <- "actual"
  all_forecasts$type <- "forecast"
  all_trend_forecasts$type <- "trend"
  all_seasonal_forecasts$type <- "seasonal"
  all_remainder_forecasts$type <- "remainder"
  
  all_actuals <- cbind(rep(1:length(training_set), num_origins), rep(1:num_origins, each = length(training_set)), all_actuals)
  all_forecasts <- cbind(rep(1:length(training_set), num_origins), rep(1:num_origins, each = length(training_set)), all_forecasts)
  all_trend_forecasts <- cbind(rep(1:length(training_set), num_origins), rep(1:num_origins, each = length(training_set)), all_trend_forecasts)
  all_seasonal_forecasts <- cbind(rep(1:length(training_set), num_origins), rep(1:num_origins, each = length(training_set)), all_seasonal_forecasts)
  all_remainder_forecasts <- cbind(rep(1:length(training_set), num_origins), rep(1:num_origins, each = length(training_set)), all_remainder_forecasts)
  
  colnames(all_actuals)[1:2] <- c("item_id", "fc_origin") 
  colnames(all_forecasts)[1:2] <- c("item_id", "fc_origin")
  colnames(all_trend_forecasts)[1:2] <- c("item_id", "fc_origin")
  colnames(all_seasonal_forecasts)[1:2] <- c("item_id", "fc_origin")
  colnames(all_remainder_forecasts)[1:2] <- c("item_id", "fc_origin")
  
  all_actuals <- arrange(all_actuals, item_id)
  all_forecasts <- arrange(all_forecasts, item_id)
  all_trend_forecasts <- arrange(all_trend_forecasts, item_id)
  all_seasonal_forecasts <- arrange(all_seasonal_forecasts, item_id)
  all_remainder_forecasts <- arrange(all_remainder_forecasts, item_id)
  
  acc_training <- list()
  
  for(s in 1:length(training_set)){
    for(or in 1:length(all_training)){
      acc_training[[length(acc_training)+1]] <- all_training[[or]][[s]]
    }
  }
  
  # Measuring accuracy
  dir.create(file.path(BASE_DIR, "results", "errors", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  calculate_errors(as.matrix(all_forecasts[,3:(ncol(all_forecasts)-1)]), as.matrix(all_actuals[,3:(ncol(all_actuals)-1)]), acc_training, seasonality, file_name)
  
  all_actuals <- rbind(all_actuals, all_forecasts, all_trend_forecasts, all_seasonal_forecasts, all_remainder_forecasts)
  
  # Measuring horizontal stability
  calculate_horizontal_stability(all_actuals[all_actuals$type == "forecast", 1:(ncol(all_actuals)-1)], all_training, seasonality, paste0(file_name, "_horizontal"))
  calculate_horizontal_stability(all_actuals[all_actuals$type == "forecast", 1:(ncol(all_actuals)-1)], all_training, seasonality, paste0(file_name, "_horizontal"), TRUE, TRUE)
  
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

# Running base models for vertical stability experiments
do_forecasting("m3_monthly_dataset.txt", "global", 6, "m3_monthly", 13, 12, "pooled_regression", 15)
do_forecasting("m3_monthly_dataset.txt", "global", 6, "m3_monthly", 13, 12, "lightgbm", 15)
do_forecasting("m3_monthly_dataset.txt", "local", 6, "m3_monthly", 13, 12, "ets")
do_forecasting("m3_monthly_dataset.txt", "local", 6, "m3_monthly", 13, 12, "arima")

do_forecasting("m4_monthly_dataset.txt", "global", 6, "m4_monthly", 13, 12, "pooled_regression", 15)
do_forecasting("m4_monthly_dataset.txt", "global", 6, "m4_monthly", 13, 12, "lightgbm", 15)
do_forecasting("m4_monthly_dataset.txt", "local", 6, "m4_monthly", 13, 12, "ets")
do_forecasting("m4_monthly_dataset.txt", "local", 6, "m4_monthly", 13, 12, "arima")

do_forecasting("favorita_dataset.txt", "global", 6, "favorita", 11, 7, "pooled_regression", 9)
do_forecasting("favorita_dataset.txt", "global", 6, "favorita", 11, 7, "lightgbm", 9)
do_forecasting("favorita_dataset.txt", "local", 6, "favorita", 11, 7, "ets")
do_forecasting("favorita_dataset.txt", "local", 6, "favorita", 11, 7, "arima")

do_forecasting("m5_items.txt", "global", 16, "m5_items", 13, 7, "pooled_regression", 9)
do_forecasting("m5_items.txt", "global", 16, "m5_items", 13, 7, "lightgbm", 9)
do_forecasting("m5_items.txt", "local", 16, "m5_items", 13, 7, "ets")
do_forecasting("m5_items.txt", "local", 16, "m5_items", 13, 7, "arima")


# Running base models for horizontal stability experiments
do_stl_forecasting("m3_monthly_dataset.txt", "global", 6, "m3_monthly", 13, 12, "pooled_regression", 15)
do_stl_forecasting("m3_monthly_dataset.txt", "global", 6, "m3_monthly", 13, 12, "lightgbm", 15)
do_stl_forecasting("m3_monthly_dataset.txt", "local", 6, "m3_monthly", 13, 12, "ets")
do_stl_forecasting("m3_monthly_dataset.txt", "local", 6, "m3_monthly", 13, 12, "arima")

do_stl_forecasting("m4_monthly_dataset.txt", "global", 6, "m4_monthly", 13, 12, "pooled_regression", 15)
do_stl_forecasting("m4_monthly_dataset.txt", "global", 6, "m4_monthly", 13, 12, "lightgbm", 15)
do_stl_forecasting("m4_monthly_dataset.txt", "local", 6, "m4_monthly", 13, 12, "ets")
do_stl_forecasting("m4_monthly_dataset.txt", "local", 6, "m4_monthly", 13, 12, "arima")

do_stl_forecasting("favorita_dataset.txt", "global", 6, "favorita", 11, 7, "pooled_regression", 9)
do_stl_forecasting("favorita_dataset.txt", "global", 6, "favorita", 11, 7, "lightgbm", 9)
do_stl_forecasting("favorita_dataset.txt", "local", 6, "favorita", 11, 7, "ets")
do_stl_forecasting("favorita_dataset.txt", "local", 6, "favorita", 11, 7, "arima")

do_stl_forecasting("m5_items.txt", "global", 16, "m5_items", 13, 7, "pooled_regression", 9)
do_stl_forecasting("m5_items.txt", "global", 16, "m5_items", 13, 7, "lightgbm", 9)
do_stl_forecasting("m5_items.txt", "local", 16, "m5_items", 13, 7, "ets")
do_stl_forecasting("m5_items.txt", "local", 16, "m5_items", 13, 7, "arima")


############## Before running the following interpolation experiments, execute the base models using "do_forecasting" or "do_stl_forecasting" functions and get base model forecasts

# Partial Interpolation experiments 

# Vertical Stability

vertical_partial_interpolate("results/forecasts/m3_monthly_pooled_regression_forecasts.csv", "m3_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)
vertical_partial_interpolate("results/forecasts/m3_monthly_lightgbm_forecasts.csv", "m3_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)
vertical_partial_interpolate("results/forecasts/m3_monthly_ets_forecasts.csv", "m3_monthly_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)
vertical_partial_interpolate("results/forecasts/m3_monthly_arima_forecasts.csv", "m3_monthly_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)

vertical_partial_interpolate("results/forecasts/m4_monthly_pooled_regression_forecasts.csv", "m4_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12) 
vertical_partial_interpolate("results/forecasts/m4_monthly_lightgbm_forecasts.csv", "m4_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)
vertical_partial_interpolate("results/forecasts/m4_monthly_ets_forecasts.csv", "m4_monthly_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)
vertical_partial_interpolate("results/forecasts/m4_monthly_arima_forecasts.csv", "m4_monthly_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)

vertical_partial_interpolate("results/forecasts/favorita_pooled_regression_forecasts.csv", "favorita_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7) 
vertical_partial_interpolate("results/forecasts/favorita_lightgbm_forecasts.csv", "favorita_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7)
vertical_partial_interpolate("results/forecasts/favorita_ets_forecasts.csv", "favorita_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7)
vertical_partial_interpolate("results/forecasts/favorita_arima_forecasts.csv", "favorita_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7)

vertical_partial_interpolate("results/forecasts/m5_items_pooled_regression_forecasts.csv", "m5_items_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7) 
vertical_partial_interpolate("results/forecasts/m5_items_lightgbm_forecasts.csv", "m5_items_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7)
vertical_partial_interpolate("results/forecasts/m5_items_ets_forecasts.csv", "m5_items_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7)
vertical_partial_interpolate("results/forecasts/m5_items_arima_forecasts.csv", "m5_items_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7)



# Horizontal Stability

horizontal_partial_interpolate("results/forecasts/m3_monthly_stl_pooled_regression_forecasts.csv", "m3_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)
horizontal_partial_interpolate("results/forecasts/m3_monthly_stl_lightgbm_forecasts.csv", "m3_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)
horizontal_partial_interpolate("results/forecasts/m3_monthly_stl_ets_forecasts.csv", "m3_monthly_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)
horizontal_partial_interpolate("results/forecasts/m3_monthly_stl_arima_forecasts.csv", "m3_monthly_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)

horizontal_partial_interpolate("results/forecasts/m4_monthly_stl_pooled_regression_forecasts.csv", "m4_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)
horizontal_partial_interpolate("results/forecasts/m4_monthly_stl_lightgbm_forecasts.csv", "m4_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)
horizontal_partial_interpolate("results/forecasts/m4_monthly_stl_ets_forecasts.csv", "m4_monthly_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)
horizontal_partial_interpolate("results/forecasts/m4_monthly_stl_arima_forecasts.csv", "m4_monthly_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)

horizontal_partial_interpolate("results/forecasts/favorita_stl_pooled_regression_forecasts.csv", "favorita_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7)
horizontal_partial_interpolate("results/forecasts/favorita_stl_lightgbm_forecasts.csv", "favorita_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7)
horizontal_partial_interpolate("results/forecasts/favorita_stl_ets_forecasts.csv", "favorita_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7)
horizontal_partial_interpolate("results/forecasts/favorita_stl_arima_forecasts.csv", "favorita_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7)

horizontal_partial_interpolate("results/forecasts/m5_items_stl_pooled_regression_forecasts.csv", "m5_items_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7)
horizontal_partial_interpolate("results/forecasts/m5_items_stl_lightgbm_forecasts.csv", "m5_items_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7)
horizontal_partial_interpolate("results/forecasts/m5_items_stl_ets_forecasts.csv", "m5_items_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7)
horizontal_partial_interpolate("results/forecasts/m5_items_stl_arima_forecasts.csv", "m5_items_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7)



# Full Interpolation experiments

# Vertical Stability

vertical_full_interpolate("results/forecasts/m3_monthly_pooled_regression_forecasts.csv", "m3_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12) 
vertical_full_interpolate("results/forecasts/m3_monthly_lightgbm_forecasts.csv", "m3_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)
vertical_full_interpolate("results/forecasts/m3_monthly_ets_forecasts.csv", "m3_monthly_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)
vertical_full_interpolate("results/forecasts/m3_monthly_arima_forecasts.csv", "m3_monthly_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)

vertical_full_interpolate("results/forecasts/m4_monthly_pooled_regression_forecasts.csv", "m4_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12) 
vertical_full_interpolate("results/forecasts/m4_monthly_lightgbm_forecasts.csv", "m4_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)
vertical_full_interpolate("results/forecasts/m4_monthly_ets_forecasts.csv", "m4_monthly_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)
vertical_full_interpolate("results/forecasts/m4_monthly_arima_forecasts.csv", "m4_monthly_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)

vertical_full_interpolate("results/forecasts/favorita_pooled_regression_forecasts.csv", "favorita_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7) 
vertical_full_interpolate("results/forecasts/favorita_lightgbm_forecasts.csv", "favorita_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7)
vertical_full_interpolate("results/forecasts/favorita_ets_forecasts.csv", "favorita_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7)
vertical_full_interpolate("results/forecasts/favorita_arima_forecasts.csv", "favorita_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7)

vertical_full_interpolate("results/forecasts/m5_items_pooled_regression_forecasts.csv", "m5_items_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7) 
vertical_full_interpolate("results/forecasts/m5_items_lightgbm_forecasts.csv", "m5_items_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7)
vertical_full_interpolate("results/forecasts/m5_items_ets_forecasts.csv", "m5_items_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7)
vertical_full_interpolate("results/forecasts/m5_items_arima_forecasts.csv", "m5_items_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7)



# Horizontal Stability

horizontal_full_interpolate("results/forecasts/m3_monthly_stl_pooled_regression_forecasts.csv", "m3_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)
horizontal_full_interpolate("results/forecasts/m3_monthly_stl_lightgbm_forecasts.csv", "m3_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)
horizontal_full_interpolate("results/forecasts/m3_monthly_stl_ets_forecasts.csv", "m3_monthly_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)
horizontal_full_interpolate("results/forecasts/m3_monthly_stl_arima_forecasts.csv", "m3_monthly_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)

horizontal_full_interpolate("results/forecasts/m4_monthly_stl_pooled_regression_forecasts.csv", "m4_monthly_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)
horizontal_full_interpolate("results/forecasts/m4_monthly_stl_lightgbm_forecasts.csv", "m4_monthly_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)
horizontal_full_interpolate("results/forecasts/m4_monthly_stl_ets_forecasts.csv", "m4_monthly_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)
horizontal_full_interpolate("results/forecasts/m4_monthly_stl_arima_forecasts.csv", "m4_monthly_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)

horizontal_full_interpolate("results/forecasts/favorita_stl_pooled_regression_forecasts.csv", "favorita_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7)
horizontal_full_interpolate("results/forecasts/favorita_stl_lightgbm_forecasts.csv", "favorita_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7)
horizontal_full_interpolate("results/forecasts/favorita_stl_ets_forecasts.csv", "favorita_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7)
horizontal_full_interpolate("results/forecasts/favorita_stl_arima_forecasts.csv", "favorita_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "favorita_dataset.txt", 6, 11, 7)

horizontal_full_interpolate("results/forecasts/m5_items_stl_pooled_regression_forecasts.csv", "m5_items_pr", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7)
horizontal_full_interpolate("results/forecasts/m5_items_stl_lightgbm_forecasts.csv", "m5_items_lightgbm", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7)
horizontal_full_interpolate("results/forecasts/m5_items_stl_ets_forecasts.csv", "m5_items_ets", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7)
horizontal_full_interpolate("results/forecasts/m5_items_stl_arima_forecasts.csv", "m5_items_arima", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m5_items.txt", 16, 13, 7)






