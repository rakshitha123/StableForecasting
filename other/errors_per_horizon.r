options("width" = 200)

# Function to calculate series wise sMAPE values
calculate_smape_per_hor <- function(forecasts, test_set) {
  smape <- 200 * abs(forecasts - test_set) / (abs(forecasts) + abs(test_set))
  smape_per_horizon <- colMeans(smape, na.rm = TRUE)
  smape_per_horizon
}



BASE_DIR <- "./StableForecasting/Vertical_Stability_Forecasts"
# DATA_DIR = file.path(BASE_DIR, "datasets")
DATA_DIR <- BASE_DIR

all_results <- list()

# dataset_name = "favorita"
# model_name = "ets"
for (dataset_name in c("favorita", "m3_monthly", "m5_items", "m4_monthly")) {
  for (model_name in c("ets", "pr", "lightgbm")) {
   
  tryCatch({

    all_forecasts <- list()

    curr_case <- paste0(dataset_name, "_", model_name)
    base_mod_data <- read.csv(file.path(DATA_DIR, paste0(curr_case, "_forecasts.csv")))

    act <- base_mod_data[base_mod_data[, "type"] == "actual", -ncol(base_mod_data)]
    all_forecasts[[curr_case]] <- base_mod_data[base_mod_data[, "type"] == "forecast", -ncol(base_mod_data)]

    int_type <- "full_interpolate"
    ws <- 0.2

    for (int_type in c("interpolate", "full_interpolate")) {
      for (ws in c(0.2, 0.4, 0.5, 0.6, 0.8, 1)) { 
        curr_case <- paste0(dataset_name, "_", model_name, "_", int_type, "_ws_", ws)
        all_forecasts[[curr_case]] <- read.table(file.path(DATA_DIR, paste0(curr_case, "_forecasts.txt")), sep = " ")
        colnames(all_forecasts[[curr_case]])[c(1, 2)] <- c("item_id", "fc_origin") 
      }
    }

    str(all_forecasts)

    names(all_forecasts)

    smape_per_hor <- do.call(rbind, lapply(all_forecasts, function(x) {
      calculate_smape_per_hor(x[, c(-1, -2)], act[, c(-1, -2)])
    }))

    smape_per_hor <- round(cbind(smape_per_hor, rowMeans(smape_per_hor)), digits = 3)

    smape_per_hor



    #--------------------------------------------------------------
    #--------------------------------------------------------------

    smapc_per_hor <- lapply(all_forecasts, function(x) {
      
      curr_forecasts <- x
      compare_to_initial <- FALSE

      head(curr_forecasts)
      head(base_mod_fc)

      series_nums <- unique(curr_forecasts$item_id)

      if (compare_to_initial) {
        file_name <- paste0(file_name, "_initial")
      } # Calculates sMAPC_I, MASC_I and RMSSC_I

      old_forecasts <- NULL
      new_forecasts <- NULL
      # required_training <- list()

      for (s in 1:length(series_nums)) {
        if (s %% 1000 == 0) {
          print(s)
        }

        current <- curr_forecasts[curr_forecasts$item_id == series_nums[s], ]
        old <- current[1:(nrow(current) - 1), 4:ncol(current)]
        new <- current[2:nrow(current), 3:(ncol(current) - 1)]
        new_forecasts <- rbind(new_forecasts, new)

        if (compare_to_initial) {
          for (f in 2:nrow(old)) {
            old[f, 1:(ncol(old) - 1)] <- old[f - 1, 2:ncol(old)]
          }
        }

        old_forecasts <- rbind(old_forecasts, old)

        #    for(t in 2:length(all_training))
        #      required_training[[length(required_training)+1]] <- all_training[[t]][[s]]
      }


      # calculating sMAPC
      # smapc_per_horizon <-
      calculate_smape_per_hor(old_forecasts, new_forecasts)
    })


    smapc_per_hor <- do.call(rbind, smapc_per_hor)

    smapc_per_hor <- round(cbind(smapc_per_hor, rowMeans(smapc_per_hor)), digits = 3)

    smapc_per_hor

    all_results[[paste0(dataset_name, "_", model_name)]] <- cbind(smape_per_hor, smapc_per_hor)

    saveRDS(all_results, file.path(BASE_DIR, "all_results.rds"))
  }, error= function(e){})

  }
  }






all_results

all_res <- all_results
#all_res <- readRDS("./StableForecasting/Vertical_Stability_Forecasts/all_results.rds")



# Load necessary libraries
library(ggplot2)
library(reshape2)
library(patchwork)

# Create a PDF to write the combined plots
pdf(file.path(BASE_DIR, "aa_stab_per_hor_%02d.pdf"), width = 10, height = 10, onefile=FALSE)

# Loop through each element in all_res
for (curr_res in names(all_res)) {
  
  # Split accuracy and stability data based on column indices
  hor <- ((ncol(all_res[[curr_res]]) - 1) / 2)
  acc_res_per_hor <- all_res[[curr_res]][, 1:hor]
  stab_res_per_hor <- all_res[[curr_res]][, (hor + 2):(hor * 2)]
  
  # Create a data frame for accuracy and reshape it for ggplot
  acc_df <- as.data.frame(t(acc_res_per_hor))
  colnames(acc_df) <- rownames(acc_res_per_hor)
  acc_df$Horizon <- 1:ncol(acc_res_per_hor)
  
  acc_long <- melt(acc_df, id.vars = "Horizon", variable.name = "Model", value.name = "Accuracy")
  
  # Plot accuracy
  acc_plot <- ggplot(acc_long, aes(x = Horizon, y = Accuracy, color = Model, group = Model)) +
    geom_line() +
    labs(title = paste0("Accuracy: ", curr_res), x = "Horizon", y = "sMAPE") +
    theme_minimal()
  
  # Create a data frame for stability and reshape it for ggplot
  stab_df <- as.data.frame(t(stab_res_per_hor))
  colnames(stab_df) <- rownames(stab_res_per_hor)
  stab_df$Horizon <- 1:ncol(stab_res_per_hor)
  
  stab_long <- melt(stab_df, id.vars = "Horizon", variable.name = "Model", value.name = "Stability")
  
  # Plot stability
  stab_plot <- ggplot(stab_long, aes(x = Horizon, y = Stability, color = Model, group = Model)) +
    geom_line() +
    labs(title = paste0("Stability: ", curr_res), x = "Horizon", y = "sMAPC(V)") +
    theme_minimal()
  
  # Combine the two plots side by side
  combined_plot <- acc_plot + stab_plot + plot_layout(nrow = 2)
  
  # Print the combined plot to the PDF
  print(combined_plot)
}

# Close the PDF device
dev.off()


str(all_forecasts)

head(all_forecasts[[1]])
