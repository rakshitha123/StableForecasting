library(smooth)

# Error calculation Functions


# Function to calculate series wise sMAPE values
calculate_smape <- function(forecasts, test_set){
  smape <- 200 * abs(forecasts - test_set) / (abs(forecasts) + abs(test_set)) 
  smape_per_series <- rowMeans(smape, na.rm = TRUE)
  smape_per_series
}


# Function to calculate series wise MASE values
calculate_mase <- function(forecasts, test_set, training_set, seasonality){
  mase_per_series <- NULL
  
  for(k in 1:nrow(forecasts))
    mase_per_series[k] <- MASE(as.numeric(test_set[k,]), as.numeric(forecasts[k,]), mean(abs(diff(as.numeric(training_set[[k]]), lag = seasonality, differences = 1))))
  
  mase_per_series <- mase_per_series[!is.infinite(mase_per_series) & !is.na(mase_per_series)]
  mase_per_series
}


# Function to calculate series wise RMSSE values
calculate_rmsse <- function(forecasts, test_set, training_set, seasonality){
  rmsse_per_series <- NULL
  
  for(k in 1:nrow(forecasts))
    rmsse_per_series[k] <- RMSSE(as.numeric(test_set[k,]), as.numeric(forecasts[k,]), mean((diff(as.numeric(training_set[[k]]), lag = seasonality, differences = 1))^2))
  
  rmsse_per_series <- rmsse_per_series[!is.infinite(rmsse_per_series) & !is.na(rmsse_per_series)]
  rmsse_per_series
}


# A wrapper function to calculate sMAPE, MASE and RMSSE
calculate_errors <- function(forecasts, test_set, training_set, seasonality, file_name, write_files = TRUE){
  #calculating sMAPE
  smape_per_series <- calculate_smape(forecasts, test_set)
  
  #calculating MASE
  mase_per_series <- calculate_mase(forecasts, test_set, training_set, seasonality)
  
  #calculating RMSSE
  rmsse_per_series <- calculate_rmsse(forecasts, test_set, training_set, seasonality)
 
  dir.create(file.path(BASE_DIR, "results", "errors", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  
  if(write_files){
    write.table(smape_per_series, file.path(BASE_DIR, "results", "errors", paste0(file_name, "_smape_errors.txt")), row.names = F, col.names = F, quote = F)
    write.table(mase_per_series, file.path(BASE_DIR, "results", "errors", paste0(file_name, "_mase_errors.txt")), row.names = F, col.names = F, quote = F)
    write.table(rmsse_per_series, file.path(BASE_DIR, "results", "errors", paste0(file_name, "_rmsse_errors.txt")), row.names = F, col.names = F, quote = F)
  }
    
  mean_smape <- mean(smape_per_series, na.rm = TRUE)
  mean_mase <- mean(mase_per_series, na.rm = TRUE)
  mean_rmsse <- mean(rmsse_per_series, na.rm = TRUE)

  print(paste0("Mean SMAPE: ", mean_smape))
  print(paste0("Mean MASE: ", mean_mase))
  print(paste0("Mean RMSSE: ", mean_rmsse))

  if(write_files){
    write(c(paste0("Mean SMAPE: ", mean_smape), 
            paste0("Mean MASE: ", mean_mase), 
            paste0("Mean RMSSE: ", mean_rmsse)), 
          file = file.path(BASE_DIR, "results", "errors", paste0(file_name, ".txt")))
  }
  
  list("mean_smape" = mean_smape, "mean_mase" = mean_mase, "mean_rmsse" = mean_rmsse)
}


# A wrapper function to calculate sMAPC, MASC and RMSSC for vertical stability
calculate_vertical_stability <- function(all_forecasts, all_training, seasonality, file_name, write_files = TRUE, compare_to_initial = FALSE){
  series_nums <- unique(all_forecasts$item_id)
  
  if(compare_to_initial)
    file_name <- paste0(file_name, "_initial") # Calculates sMAPC_I, MASC_I and RMSSC_I
  
  old_forecasts <- NULL
  new_forecasts <- NULL
  required_training <- list()
  
  for(s in 1:length(series_nums)){
    if(s %% 1000 == 0)
      print(s)
    
    current <- all_forecasts[all_forecasts$item_id == series_nums[s],]
    old <- current[1:(nrow(current)-1), 4:ncol(current)]
    new <- current[2:nrow(current), 3:(ncol(current)-1)]
    new_forecasts <- rbind(new_forecasts, new)
    
    if(compare_to_initial){
      for(f in 2:nrow(old))
         old[f, 1:(ncol(old)-1)] <- old[f-1, 2:ncol(old)]
    }
    
    old_forecasts <- rbind(old_forecasts, old)
    
    for(t in 2:length(all_training))
      required_training[[length(required_training)+1]] <- all_training[[t]][[s]]
  }
  
  
  #calculating sMAPC
  smapc_per_series <- calculate_smape(old_forecasts, new_forecasts)
  
  #calculating MASC
  masc_per_series <- calculate_mase(old_forecasts, new_forecasts, required_training, seasonality)
  
  #calculating RMSSC
  rmssc_per_series <- calculate_rmsse(old_forecasts, new_forecasts, required_training, seasonality)
  
  dir.create(file.path(BASE_DIR, "results", "errors", "stability", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  
  if(write_files){
    write.table(smapc_per_series, file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, "_smapc_errors.txt")), row.names = F, col.names = F, quote = F)
    write.table(masc_per_series, file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, "_masc_errors.txt")), row.names = F, col.names = F, quote = F)
    write.table(rmssc_per_series, file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, "_rmssc_errors.txt")), row.names = F, col.names = F, quote = F)
  }
  
  mean_smapc <- mean(smapc_per_series, na.rm = TRUE)
  mean_masc <- mean(masc_per_series, na.rm = TRUE)
  mean_rmssc <- mean(rmssc_per_series, na.rm = TRUE)

  print(paste0("Mean SMAPC: ", mean_smapc))
  print(paste0("Mean MASC: ", mean_masc))
  print(paste0("Mean RMSSC: ", mean_rmssc))

  if(write_files){
    write(c(paste0("Mean SMAPC: ", mean_smapc), 
            paste0("Mean MASC: ", mean_masc), 
            paste0("Mean RMSSC: ", mean_rmssc)), 
            file = file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, ".txt")))
  }
  
  list("mean_smapc" = mean_smapc, "mean_masc" = mean_masc, "mean_rmssc" = mean_rmssc)
}


# A wrapper function to calculate sMAPC, MASC and RMSSC for horizontal stability
calculate_horizontal_stability <- function(all_forecasts, all_training, seasonality, file_name, write_files = TRUE, compare_to_initial = FALSE){
  series_nums <- unique(all_forecasts$item_id)
  
  if(compare_to_initial)
    file_name <- paste0(file_name, "_initial") # Calculates sMAPC_I, MASC_I and RMSSC_I
  
  old_forecasts <- NULL
  new_forecasts <- NULL
  required_training <- list()
  
  for(s in 1:length(series_nums)){
    if(s %% 1000 == 0)
      print(s)
    
    current <- all_forecasts[all_forecasts$item_id == series_nums[s],]
    old <- current[,3:(ncol(current)-1)]
    new <- current[,4:ncol(current)]
    new_forecasts <- rbind(new_forecasts, new)
    
    if(compare_to_initial){
      for(f in 2:ncol(old))
        old[,f] <- old[,1]
    }
    
    old_forecasts <- rbind(old_forecasts, old)
    
    for(t in 1:length(all_training))
      required_training[[length(required_training)+1]] <- all_training[[t]][[s]]
  }
  
  
  #calculating sMAPC
  smapc_per_series <- calculate_smape(old_forecasts, new_forecasts)
  
  #calculating MASC
  masc_per_series <- calculate_mase(old_forecasts, new_forecasts, required_training, seasonality)
  
  #calculating RMSSC
  rmssc_per_series <- calculate_rmsse(old_forecasts, new_forecasts, required_training, seasonality)
  
  dir.create(file.path(BASE_DIR, "results", "errors", "stability", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  
  if(write_files){
    write.table(smapc_per_series, file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, "_smapc_errors.txt")), row.names = F, col.names = F, quote = F)
    write.table(masc_per_series, file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, "_masc_errors.txt")), row.names = F, col.names = F, quote = F)
    write.table(rmssc_per_series, file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, "_rmssc_errors.txt")), row.names = F, col.names = F, quote = F)
  }
  
  mean_smapc <- mean(smapc_per_series, na.rm = TRUE)
  mean_masc <- mean(masc_per_series, na.rm = TRUE)
  mean_rmssc <- mean(rmssc_per_series, na.rm = TRUE)
  
  print(paste0("Mean SMAPC: ", mean_smapc))
  print(paste0("Mean MASC: ", mean_masc))
  print(paste0("Mean RMSSC: ", mean_rmssc))
  
  if(write_files){
    write(c(paste0("Mean SMAPC: ", mean_smapc), 
            paste0("Mean MASC: ", mean_masc), 
            paste0("Mean RMSSC: ", mean_rmssc)), 
          file = file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, ".txt")))
  }
  
  list("mean_smapc" = mean_smapc, "mean_masc" = mean_masc, "mean_rmssc" = mean_rmssc)
}







