library(data.table)


# A function to perform partial interpolation for vertical stability
vertical_partial_interpolate <- function(path, file_name, w_s){
  output <- fread(file.path(BASE_DIR, path))
  forecasts <- output[output$type == "forecast",]
  series_nums <- unique(forecasts$item_id)  
  
  # Applying linear interpolation to stabilise forecasts
  for(i in 1:length(w_s))
    assign(paste0("forecasts_ws_", w_s[i]), NULL)
  
  for(s in 1:length(series_nums)){  
    if(s %% 100 == 0)
      print(s)
    
    current_series_forecasts <- as.data.frame(forecasts[forecasts$item_id == series_nums[s],])
    current_series_forecasts <- current_series_forecasts[,3:(ncol(forecasts)-1)]
    
    old_forecasts <- current_series_forecasts[1:(nrow(current_series_forecasts)-1),2:ncol(current_series_forecasts)]
    new_forecasts <- current_series_forecasts[2:nrow(current_series_forecasts), 1:(ncol(current_series_forecasts)-1)]
    
    for(i in 1:length(w_s)){
      interpolated_forecasts <- w_s[i]*old_forecasts + (1 - w_s[i])*new_forecasts
      interpolated_forecasts <- cbind(interpolated_forecasts, current_series_forecasts[2:nrow(current_series_forecasts),ncol(current_series_forecasts)])
      colnames(interpolated_forecasts) <- colnames(current_series_forecasts)
      interpolated_forecasts <- rbind(current_series_forecasts[1,], interpolated_forecasts)
      assign(paste0("forecasts_ws_", w_s[i]), rbind(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), interpolated_forecasts))
    }
  }
  
  test_set <- output[output$type == "actual", 3:(ncol(output)-1)]
  
  # Error calculations 
  for(i in 1:length(w_s)){
    print(paste0("w_s = ", w_s[i]))
    calculate_errors(as.matrix(eval(parse(text=paste0("forecasts_ws_", w_s[i])))), as.matrix(test_set), paste0(file_name, "_interpolate_ws_", w_s[i]))
    assign(paste0("forecasts_ws_", w_s[i]), cbind(forecasts[,1:2], eval(parse(text=paste0("forecasts_ws_", w_s[i])))))
    dir.create(file.path(BASE_DIR, "results", "forecasts", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
    write.table(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), file.path(BASE_DIR, "results", "forecasts", paste0(file_name, "_interpolate_ws_", w_s[i], "_forecasts.txt")), row.names = FALSE, col.names = FALSE, quote = FALSE)
    calculate_vertical_stability(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), paste0(file_name, "_interpolate_ws_", w_s[i]))
    calculate_vertical_stability(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), paste0(file_name, "_interpolate_ws_", w_s[i]), TRUE, TRUE)
  }
}


# A function to perform partial interpolation for horizontal stability
horizontal_partial_interpolate <- function(path, file_name, w_s){
  output <- fread(file.path(BASE_DIR, path))
  forecasts <- output[output$type == "forecast",]
  series_nums <- unique(forecasts$item_id)  
  
  # Applying linear interpolation to stabilise forecasts
  for(i in 1:length(w_s))
    assign(paste0("forecasts_ws_", w_s[i]), NULL)
  
  for(s in 1:length(series_nums)){  
    if(s %% 100 == 0)
      print(s)
    
    current_series_forecasts <- as.data.frame(forecasts[forecasts$item_id == series_nums[s],])
    current_series_forecasts <- current_series_forecasts[,3:(ncol(forecasts)-1)]
    
    old_forecasts <- current_series_forecasts[,1:(ncol(current_series_forecasts)-1)]
    new_forecasts <- current_series_forecasts[, 2:ncol(current_series_forecasts)]
    
    for(i in 1:length(w_s)){
      interpolated_forecasts <- w_s[i]*old_forecasts + (1 - w_s[i])*new_forecasts
      interpolated_forecasts <- cbind(current_series_forecasts[,1], interpolated_forecasts)
      colnames(interpolated_forecasts) <- colnames(current_series_forecasts)
      assign(paste0("forecasts_ws_", w_s[i]), rbind(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), interpolated_forecasts))
    }
  }
  
  test_set <- output[output$type == "actual", 3:(ncol(output)-1)]
  
  # Error calculations 
  for(i in 1:length(w_s)){
    print(paste0("w_s = ", w_s[i]))
    calculate_errors(as.matrix(eval(parse(text=paste0("forecasts_ws_", w_s[i])))), as.matrix(test_set), paste0(file_name, "_horizontal_interpolate_ws_", w_s[i]))
    assign(paste0("forecasts_ws_", w_s[i]), cbind(forecasts[,1:2], eval(parse(text=paste0("forecasts_ws_", w_s[i])))))
    dir.create(file.path(BASE_DIR, "results", "forecasts", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
    write.table(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), file.path(BASE_DIR, "results", "forecasts", paste0(file_name, "_horizontal_interpolate_ws_", w_s[i], "_forecasts.txt")), row.names = FALSE, col.names = FALSE, quote = FALSE)
    calculate_horizontal_stability(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), paste0(file_name, "_horizontal_interpolate_ws_", w_s[i]))
    calculate_horizontal_stability(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), paste0(file_name, "_horizontal_interpolate_ws_", w_s[i]), TRUE, TRUE)
  }
}


# A function to perform full interpolation (uses previously interpolated forecasts) for vertical stability
vertical_full_interpolate <- function(path, file_name, w_s){
  output <- fread(file.path(BASE_DIR, path))
  forecasts <- output[output$type == "forecast",]
  series_nums <- unique(forecasts$item_id)  
  
  # Applying linear interpolation to stabilise forecasts
  for(i in 1:length(w_s))
    assign(paste0("forecasts_ws_", w_s[i]), NULL)
  
  for(s in 1:length(series_nums)){  
    if(s %% 100 == 0)
      print(s)
    
    current_series_forecasts <- as.data.frame(forecasts[forecasts$item_id == series_nums[s],])
    current_series_forecasts <- current_series_forecasts[,3:(ncol(forecasts)-1)]
    
    for(i in 1:length(w_s)){
      interpolated_forecasts <- current_series_forecasts[1,]
      
      for(r in 2:nrow(current_series_forecasts)){
        old_forecasts <- interpolated_forecasts[(r-1), 2:ncol(interpolated_forecasts)]
        current_forecasts <- current_series_forecasts[r, 1:(ncol(current_series_forecasts) - 1)]
        int_forecasts <- w_s[i]*old_forecasts + (1 - w_s[i])*current_forecasts
        int_forecasts <- cbind(int_forecasts, current_series_forecasts[r,ncol(current_series_forecasts)])
        colnames(int_forecasts) <- colnames(interpolated_forecasts)
        interpolated_forecasts <- rbind(interpolated_forecasts, int_forecasts)
      }
      
      assign(paste0("forecasts_ws_", w_s[i]), rbind(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), interpolated_forecasts))
    }
  }
  
  test_set <- output[output$type == "actual", 3:(ncol(output)-1)]
  
  # Error calculations 
  for(i in 1:length(w_s)){
    print(paste0("w_s = ", w_s[i]))
    calculate_errors(as.matrix(eval(parse(text=paste0("forecasts_ws_", w_s[i])))), as.matrix(test_set), paste0(file_name, "_full_interpolate_ws_", w_s[i]))
    assign(paste0("forecasts_ws_", w_s[i]), cbind(forecasts[,1:2], eval(parse(text=paste0("forecasts_ws_", w_s[i])))))
    dir.create(file.path(BASE_DIR, "results", "forecasts", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
    write.table(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), file.path(BASE_DIR, "results", "forecasts", paste0(file_name, "_full_interpolate_ws_", w_s[i], "_forecasts.txt")), row.names = FALSE, col.names = FALSE, quote = FALSE)
    calculate_vertical_stability(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), paste0(file_name, "_full_interpolate_ws_", w_s[i]))
    calculate_vertical_stability(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), paste0(file_name, "_full_interpolate_ws_", w_s[i]), TRUE, TRUE)
  }
}


# A function to perform full interpolation (uses previously interpolated forecasts) for horizontal stability
horizontal_full_interpolate <- function(path, file_name, w_s){
  output <- fread(file.path(BASE_DIR, path))
  forecasts <- output[output$type == "forecast",]
  series_nums <- unique(forecasts$item_id)  
  
  # Applying linear interpolation to stabilise forecasts
  for(i in 1:length(w_s))
    assign(paste0("forecasts_ws_", w_s[i]), NULL)
  
  for(s in 1:length(series_nums)){  
    if(s %% 100 == 0)
      print(s)
    
    current_series_forecasts <- as.data.frame(forecasts[forecasts$item_id == series_nums[s],])
    current_series_forecasts <- current_series_forecasts[,3:(ncol(forecasts)-1)]
    
    for(i in 1:length(w_s)){
      interpolated_forecasts <- as.data.frame(current_series_forecasts[,1])
      
      for(r in 2:ncol(current_series_forecasts)){
        old_forecasts <- interpolated_forecasts[,(r-1)]
        current_forecasts <- current_series_forecasts[,r]
        int_forecasts <- w_s[i]*old_forecasts + (1 - w_s[i])*current_forecasts
        interpolated_forecasts <- cbind(interpolated_forecasts, int_forecasts)
      }
      
      colnames(interpolated_forecasts) <- colnames(current_series_forecasts)
      assign(paste0("forecasts_ws_", w_s[i]), rbind(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), interpolated_forecasts))
    }
  }
  
  test_set <- output[output$type == "actual", 3:(ncol(output)-1)]
  
  # Error calculations 
  for(i in 1:length(w_s)){
    print(paste0("w_s = ", w_s[i]))
    calculate_errors(as.matrix(eval(parse(text=paste0("forecasts_ws_", w_s[i])))), as.matrix(test_set), paste0(file_name, "_horizontal_full_interpolate_ws_", w_s[i]))
    assign(paste0("forecasts_ws_", w_s[i]), cbind(forecasts[,1:2], eval(parse(text=paste0("forecasts_ws_", w_s[i])))))
    dir.create(file.path(BASE_DIR, "results", "forecasts", fsep = "/"), showWarnings = FALSE, recursive = TRUE)
    write.table(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), file.path(BASE_DIR, "results", "forecasts", paste0(file_name, "_horizontal_full_interpolate_ws_", w_s[i], "_forecasts.txt")), row.names = FALSE, col.names = FALSE, quote = FALSE)
    calculate_horizontal_stability(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), paste0(file_name, "_horizontal_full_interpolate_ws_", w_s[i]))
    calculate_horizontal_stability(eval(parse(text=paste0("forecasts_ws_", w_s[i]))), paste0(file_name, "_horizontal_full_interpolate_ws_", w_s[i]), TRUE, TRUE)
  }
}
