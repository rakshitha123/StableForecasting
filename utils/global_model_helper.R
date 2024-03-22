# Global model helper functions



# A function to create training and test sets
create_train_test_sets <- function(input_file_name, forecast_horizon, num_cut_last_points, apply_stl = FALSE, seasonality = NULL){
  dataset <- readLines(file.path(BASE_DIR, "datasets", input_file_name))
  dataset <- strsplit(dataset, ",")
  
  training_set <- list()
  test_set <- matrix(0, nrow = length(dataset), ncol = forecast_horizon)
  
  if(apply_stl){
    remainder_list <- list()
    trend_forecasts <- matrix(0, nrow = length(dataset), ncol = forecast_horizon)
    seasonal_forecasts <- matrix(0, nrow = length(dataset), ncol = forecast_horizon)
  }
  
  for(i in 1:length(dataset)){
    time_series <- as.numeric(dataset[[i]])
    time_series <- time_series[1:(length(time_series) - num_cut_last_points)]
    train_series <- time_series[1:(length(time_series) - forecast_horizon)]
    test_series <- time_series[(length(time_series) - forecast_horizon + 1):length(time_series)]
    
    training_set[[i]] <- train_series
    test_set[i,] <- test_series
    
    if(apply_stl){
      sstl = stl(ts(train_series, frequency = seasonality), "periodic")
      seasonal_comp = as.numeric(sstl$time.series[, 1])
      trend_comp = as.numeric(sstl$time.series[, 2])
      
      remainder_list[[i]] <- as.numeric(sstl$time.series[, 3])
      trend_forecasts[i,] <- as.numeric(forecast(forecast:::ets(trend_comp, model = "ZZN"), h = forecast_horizon)$mean)
      seasonal_forecasts[i,] <- as.numeric(stlf(ts(seasonal_comp , frequency = seasonality), "period", h = forecast_horizon)$mean)
    }
  }
  
  if(apply_stl)
    list(training_set, data.frame(test_set), remainder_list, data.frame(trend_forecasts), data.frame(seasonal_forecasts))
  else
    list(training_set, data.frame(test_set))
}


# A function to create training set
create_training_set <- function(input_file_name, forecast_horizon, num_cut_last_points){
  dataset <- readLines(file.path(BASE_DIR, "datasets", input_file_name))
  dataset <- strsplit(dataset, ",")
  
  training_set <- list()

  for(i in 1:length(dataset)){
    time_series <- as.numeric(dataset[[i]])
    time_series <- time_series[1:(length(time_series) - num_cut_last_points)]
    train_series <- time_series[1:(length(time_series) - forecast_horizon)]
    
    training_set[[i]] <- train_series
  }
  
  training_set
}


# A function to create embedded matrices and final lags to train the global models
create_input_matrices <- function(dataset, lag){
  embedded_series <- NULL
  final_lags <- NULL

  for (i in 1:length(dataset)) {
    if(i %% 100 == 0)
      print(i)
    
    time_series <- as.numeric(dataset[[i]])

    # Embed the series
    embedded <- embed(time_series, lag + 1)
    
    if (!is.null(embedded_series)) {
      embedded_series <- as.matrix(embedded_series)
    }
    embedded_series <- rbind(embedded_series, embedded)
    
    # Creating the test set
    if (!is.null(final_lags)) {
      final_lags <- as.matrix(final_lags)
    }
    
    current_series_final_lags <- t(as.matrix(rev(tail(time_series, lag))))
    
    final_lags <- rbind(final_lags, current_series_final_lags)
  }
  
  # Adding proper column names for embedded_series and final_lags
  embedded_series <- as.data.frame(embedded_series)
  colnames(embedded_series)[1] <- "y"
  colnames(embedded_series)[2:(lag + 1)] <- paste("Lag", 1:lag, sep = "")
  
  final_lags <- as.data.frame(final_lags)
  colnames(final_lags)[1:lag] <- paste("Lag", 1:lag, sep = "")
  
  list(embedded_series, final_lags)
}


# A function to create a formula to train a model
create_formula <- function(data){
  formula <- "y ~ "
  for(predictor in 2:ncol(data)){
    if(predictor != ncol(data)){
      formula <- paste0(formula, colnames(data)[predictor], " + ")
    }else{
      formula <- paste0(formula, colnames(data)[predictor])
    }
  }
  
  as.formula(paste(formula, "+ 0", sep=""))
}


# A function to get training set for each origin
get_training_set <- function(input_file_name, forecast_horizon, num_origins){
  
  all_training <- list()
  
  for(origin in 1:num_origins){
    print(paste0("Creating training set origin = ", origin))
    
    # Creating training set for each origin
    num_cut_last_points <- num_origins - origin
    all_training[[origin]] <- create_training_set(input_file_name, forecast_horizon, num_cut_last_points)
  }
  
  all_training
}

