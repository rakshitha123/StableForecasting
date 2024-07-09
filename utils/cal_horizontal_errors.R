library(data.table)
library(forecast)


BASE_DIR <- "StableForecasting"


source(file.path(BASE_DIR, "utils", "error_calculator.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "global_model_helper.R", fsep = "/"))


cal_errors <- function(dataset_names, origins, input_file_names, forecast_horizons, seasonality){
  weights = c(0.2, 0.4, 0.5, 0.6, 0.8, 1)
  error_types <- c("smapc", "masc", "rmssc", "initial_smapc", "initial_masc", "initial_rmssc")
  models <- "arima"
  
  
  for(i in 1:length(dataset_names)){
    
    # get training set
    all_training <- list()
    
    for(origin in 1:origins[i]){
      
      print(paste0(dataset_names[i], " Origin = ", origin))
      
      num_cut_last_points <- origins[i] - origin
      loaded_data <- create_train_test_sets(input_file_names[i], forecast_horizons[i], num_cut_last_points, TRUE, seasonality[i])
      training_set <- loaded_data[[3]]
      all_training[[origin]] <- training_set
    }
    
    for(type in error_types)
      assign(paste0(dataset_names[i], "_", type), NULL) 
    
    for(model in models){
      
        # calculating base errors
        path_name <- paste0(dataset_names[i], "_stl_", model, "_forecasts.csv")
        
        
        all_data <- fread(file.path(BASE_DIR, "results", "forecasts", path_name))
        
        all_actuals <- all_data[all_data$type == "actual",]
        all_actuals <- all_actuals[,1:(ncol(all_actuals)-1)]
        
        all_trends <- all_data[all_data$type == "trend",]
        all_trends <- as.data.frame(all_trends[,1:(ncol(all_trends)-1)])
        
        all_seasonal <- all_data[all_data$type == "seasonal",]
        all_seasonal <- as.data.frame(all_seasonal[,1:(ncol(all_seasonal)-1)])
        
        all_forecasts <- all_data[all_data$type == "remainder",]
        all_forecasts <- all_forecasts[,1:(ncol(all_forecasts)-1)]
        
        result   <- calculate_horizontal_stability(all_forecasts, all_training, seasonality[i], "", FALSE, FALSE)
        
        for(t in 1:3)
          assign(paste0(dataset_names[i], "_", error_types[t]), c(eval(parse(text = paste0(dataset_names[i], "_", error_types[t]))), result[[t]]))
        
        result   <- calculate_horizontal_stability(all_forecasts, all_training, seasonality[i], "", FALSE, TRUE)
        
        for(t in 4:6)
          assign(paste0(dataset_names[i], "_", error_types[t]), c(eval(parse(text = paste0(dataset_names[i], "_", error_types[t]))), result[[t-3]]))
        
        
        # calculating interpolation errors
        # partial
        for(j in 1:length(weights)){
          print(weights[j])
          interpolation_forecasts <- read.csv(file.path(BASE_DIR, "results", "forecasts", paste0(dataset_names[i], "_", model, "_", "horizontal_interpolate", "_ws_", weights[j], "_forecasts.txt")), sep = " ", header = F)
          interpolation_forecasts[,3:ncol(interpolation_forecasts)] <- interpolation_forecasts[,3:ncol(interpolation_forecasts)] - all_trends[,3:ncol(all_trends)] - all_seasonal[,3:ncol(all_seasonal)]
          
          colnames(interpolation_forecasts)[1:2] = c("item_id", "fc_origin")
          interpolation_result <- calculate_horizontal_stability(interpolation_forecasts, all_training, seasonality[i], "", FALSE, FALSE)
          
          for(t in 1:3)
            assign(paste0(dataset_names[i], "_", error_types[t]), c(eval(parse(text = paste0(dataset_names[i], "_", error_types[t]))), interpolation_result[[t]]))
          
          interpolation_result <- calculate_horizontal_stability(interpolation_forecasts, all_training, seasonality[i], "", FALSE, TRUE)
          
          for(t in 4:6)
            assign(paste0(dataset_names[i], "_", error_types[t]), c(eval(parse(text = paste0(dataset_names[i], "_", error_types[t]))), interpolation_result[[t-3]]))
        }
        
        # full
        for(j in 1:length(weights)){
          print(weights[j])
          interpolation_forecasts <- read.csv(file.path(BASE_DIR, "results", "forecasts", paste0(dataset_names[i], "_", model, "_", "horizontal_full_interpolate", "_ws_", weights[j], "_forecasts.txt")), sep = " ", header = F)
          interpolation_forecasts[,3:ncol(interpolation_forecasts)] <- interpolation_forecasts[,3:ncol(interpolation_forecasts)] - all_trends[,3:ncol(all_trends)] - all_seasonal[,3:ncol(all_seasonal)]
          
          colnames(interpolation_forecasts)[1:2] = c("item_id", "fc_origin")
          interpolation_result <- calculate_horizontal_stability(interpolation_forecasts, all_training, seasonality[i], "", FALSE, FALSE)
          
          for(t in 1:3)
            assign(paste0(dataset_names[i], "_", error_types[t]), c(eval(parse(text = paste0(dataset_names[i], "_", error_types[t]))), interpolation_result[[t]])) 
          
          interpolation_result <- calculate_horizontal_stability(interpolation_forecasts, all_training, seasonality[i], "", FALSE, TRUE)
          
          for(t in 4:6)
            assign(paste0(dataset_names[i], "_", error_types[t]), c(eval(parse(text = paste0(dataset_names[i], "_", error_types[t]))), interpolation_result[[t-3]])) 
          
        }
      
    }
    
    for(t in 1:6)
      write.csv(eval(parse(text = paste0(dataset_names[i], "_", error_types[t]))), file.path(BASE_DIR, "results", paste0(dataset_names[i], "_", error_types[t],".csv")))  
    
  }
  
  smapc <- NULL
  masc <- NULL
  rmssc <- NULL
  initial_smapc <- NULL
  initial_masc <- NULL
  initial_rmssc <- NULL
  
  for(i in 1:length(dataset_names)){
    smapc <- cbind(smapc, eval(parse(text = paste0(dataset_names[i], "_", error_types[1]))))
    masc <- cbind(masc, eval(parse(text = paste0(dataset_names[i], "_", error_types[2]))))
    rmssc <- cbind(rmssc, eval(parse(text = paste0(dataset_names[i], "_", error_types[3]))))
    
    initial_smapc <- cbind(initial_smapc, eval(parse(text = paste0(dataset_names[i], "_", error_types[4]))))
    initial_masc <- cbind(initial_masc, eval(parse(text = paste0(dataset_names[i], "_", error_types[5]))))
    initial_rmssc <- cbind(initial_rmssc, eval(parse(text = paste0(dataset_names[i], "_", error_types[6]))))
  }
  
  colnames(smapc) <- dataset_names
  colnames(masc) <- dataset_names
  colnames(rmssc) <- dataset_names
  colnames(initial_smapc) <- dataset_names
  colnames(initial_masc) <- dataset_names
  colnames(initial_rmssc) <- dataset_names
  
  write.csv(smapc, file.path(BASE_DIR, "results", paste0(error_types[1],".csv")))  
  write.csv(masc, file.path(BASE_DIR, "results", paste0(error_types[2],".csv")))  
  write.csv(rmssc, file.path(BASE_DIR, "results", paste0(error_types[3],".csv")))  
  
  write.csv(initial_smapc, file.path(BASE_DIR, "results", paste0(error_types[4],".csv")))  
  write.csv(initial_masc, file.path(BASE_DIR, "results", paste0(error_types[5],".csv")))  
  write.csv(initial_rmssc, file.path(BASE_DIR, "results", paste0(error_types[6],".csv")))  
}



cal_errors(c("m4_monthly", "m3_monthly", "favorita", "m5_items"), c(13, 13, 11, 13), c("m4_monthly_dataset.txt", "m3_monthly_dataset.txt", "favorita_dataset.txt", "m5_items.txt"), c(6, 6, 6, 16), c(12, 12, 7, 7))




