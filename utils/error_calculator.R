# Error calculation Functions


# Function to calculate series wise sMAPE values
calculate_smape <- function(forecasts, test_set){
  smape <- 200 * abs(forecasts - test_set) / (abs(forecasts) + abs(test_set)) 
  smape_per_series <- rowMeans(smape, na.rm = TRUE)
  smape_per_series
}


# Function to calculate series wise MAE values
calculate_mae <- function(forecasts, test_set){
  mae <- abs(forecasts-test_set)
  mae_per_series <- rowMeans(mae, na.rm=TRUE)
  
  mae_per_series
}


# Function to calculate series wise RMSE values
calculate_rmse <- function(forecasts, test_set){
  squared_errors <- (forecasts-test_set)^2
  rmse_per_series <- sqrt(rowMeans(squared_errors, na.rm=TRUE))
  
  rmse_per_series
}


# A wrapper function to calculate sMAPE, MAE and RMSE
calculate_errors <- function(forecasts, test_set, file_name, write_files = TRUE){
  #calculating sMAPE
  smape_per_series <- calculate_smape(forecasts, test_set)
  
  #calculating MAE
  mae_per_series <- calculate_mae(forecasts, test_set)
  
  #calculating RMSE
  rmse_per_series <- calculate_rmse(forecasts, test_set)
 
  dir.create(file.path(BASE_DIR, "results", "errors", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  
  if(write_files){
    write.table(smape_per_series, file.path(BASE_DIR, "results", "errors", paste0(file_name, "_smape_errors.txt")), row.names = F, col.names = F, quote = F)
    write.table(mae_per_series, file.path(BASE_DIR, "results", "errors", paste0(file_name, "_mae_errors.txt")), row.names = F, col.names = F, quote = F)
    write.table(rmse_per_series, file.path(BASE_DIR, "results", "errors", paste0(file_name, "_rmse_errors.txt")), row.names = F, col.names = F, quote = F)
  }
    
  mean_smape <- mean(smape_per_series, na.rm = TRUE)
  median_smape <- median(smape_per_series, na.rm = TRUE)
  mean_mae <- mean(mae_per_series, na.rm = TRUE)
  median_mae <- median(mae_per_series, na.rm = TRUE)
  mean_rmse <- mean(rmse_per_series, na.rm = TRUE)
  median_rmse <- median(rmse_per_series, na.rm = TRUE)
  
  print(paste0("Mean SMAPE: ", mean_smape))
  print(paste0("Median SMAPE: ", median_smape))
  print(paste0("Mean MAE: ", mean_mae))
  print(paste0("Median MAE: ", median_mae))
  print(paste0("Mean RMSE: ", mean_rmse))
  print(paste0("Median RMSE: ", median_rmse))
  
  if(write_files){
    write(c(paste0("Mean SMAPE: ", mean_smape), 
            paste0("Median SMAPE: ", median_smape), 
            paste0("Mean MAE: ", mean_mae), 
            paste0("Median MAE: ", median_mae), 
            paste0("Mean RMSE: ", mean_rmse), 
            paste0("Median RMSE: ", median_rmse), "\n"), 
          file = file.path(BASE_DIR, "results", "errors", paste0(file_name, ".txt")))
  }
  
  list("mean_smape" = mean_smape, "mean_mae" = mean_mae, "mean_rmse" = mean_rmse, "median_smape" = median_smape, "median_mae" = median_mae, "median_rmse" = median_rmse)
}


# A wrapper function to calculate sMAPC, MAC and RMSC for vertical stability
calculate_vertical_stability <- function(all_forecasts, file_name, write_files = TRUE, compare_to_initial = FALSE){
  series_nums <- unique(all_forecasts$item_id)
  
  if(compare_to_initial)
    file_name <- paste0(file_name, "_initial") # Calculates sMAPC_I, MAC_I and RMSC_I
  
  old_forecasts <- NULL
  new_forecasts <- NULL
  
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
  }
  
  
  #calculating sMAPC
  smapc_per_series <- calculate_smape(old_forecasts, new_forecasts)
  
  #calculating MAC
  mac_per_series <- calculate_mae(old_forecasts, new_forecasts)
  
  #calculating RMSC
  rmsc_per_series <- calculate_rmse(old_forecasts, new_forecasts)
  
  dir.create(file.path(BASE_DIR, "results", "errors", "stability", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  
  if(write_files){
    write.table(smapc_per_series, file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, "_smapc_errors.txt")), row.names = F, col.names = F, quote = F)
    write.table(mac_per_series, file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, "_mac_errors.txt")), row.names = F, col.names = F, quote = F)
    write.table(rmsc_per_series, file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, "_rmsc_errors.txt")), row.names = F, col.names = F, quote = F)
  }
  
  mean_smapc <- mean(smapc_per_series, na.rm = TRUE)
  mean_mac <- mean(mac_per_series, na.rm = TRUE)
  mean_rmsc <- mean(rmsc_per_series, na.rm = TRUE)

  print(paste0("Mean SMAPC: ", mean_smapc))
  print(paste0("Mean MAC: ", mean_mac))
  print(paste0("Mean RMSC: ", mean_rmsc))

  if(write_files){
    write(c(paste0("Mean SMAPC: ", mean_smapc), 
            paste0("Mean MAC: ", mean_mac), 
            paste0("Mean RMSC: ", mean_rmsc)), 
            file = file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, ".txt")))
  }
  
  list("mean_smapc" = mean_smapc, "mean_mac" = mean_mac, "mean_rmsc" = mean_rmsc)
}


# A wrapper function to calculate sMAPC, MAC and RMSC for horizontal stability
calculate_horizontal_stability <- function(all_forecasts, file_name, write_files = TRUE, compare_to_initial = FALSE){
  series_nums <- unique(all_forecasts$item_id)
  
  if(compare_to_initial)
    file_name <- paste0(file_name, "_initial") # Calculates sMAPC_I, MAC_I and RMSC_I
  
  old_forecasts <- NULL
  new_forecasts <- NULL
  
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
  }
  
  
  #calculating sMAPC
  smapc_per_series <- calculate_smape(old_forecasts, new_forecasts)
  
  #calculating MAC
  mac_per_series <- calculate_mae(old_forecasts, new_forecasts)
  
  #calculating RMSC
  rmsc_per_series <- calculate_rmse(old_forecasts, new_forecasts)
  
  dir.create(file.path(BASE_DIR, "results", "errors", "stability", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
  
  if(write_files){
    write.table(smapc_per_series, file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, "_smapc_errors.txt")), row.names = F, col.names = F, quote = F)
    write.table(mac_per_series, file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, "_mac_errors.txt")), row.names = F, col.names = F, quote = F)
    write.table(rmsc_per_series, file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, "_rmsc_errors.txt")), row.names = F, col.names = F, quote = F)
  }
  
  mean_smapc <- mean(smapc_per_series, na.rm = TRUE)
  mean_mac <- mean(mac_per_series, na.rm = TRUE)
  mean_rmsc <- mean(rmsc_per_series, na.rm = TRUE)
  
  print(paste0("Mean SMAPC: ", mean_smapc))
  print(paste0("Mean MAC: ", mean_mac))
  print(paste0("Mean RMSC: ", mean_rmsc))
  
  if(write_files){
    write(c(paste0("Mean SMAPC: ", mean_smapc), 
            paste0("Mean MAC: ", mean_mac), 
            paste0("Mean RMSC: ", mean_rmsc)), 
          file = file.path(BASE_DIR, "results", "errors", "stability", paste0(file_name, ".txt")))
  }
  
  list("mean_smapc" = mean_smapc, "mean_mac" = mean_mac, "mean_rmsc" = mean_rmsc)
}







