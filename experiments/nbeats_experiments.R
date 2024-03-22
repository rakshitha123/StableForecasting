library(data.table)

BASE_DIR <- "StableForecasting"


# Execution of helper scripts
source(file.path(BASE_DIR, "utils", "error_calculator.R", fsep = "/"))
source(file.path(BASE_DIR, "utils", "interpolate.R", fsep = "/"))


# Experiments

# Run the stable N-BEATS framework and obtain forecasts before trying the following experiments. 
# Update the "path" variable used by all functions with the path of the output file obtained from the stable N-BEATS framework.


# Partial interpolation experiments

# Vertical Stability
vertical_partial_interpolate("n-beats-s-main/m4m_nbeats_test_smart-sweep-1.csv", "nbeats_m4_monthly", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12) 
vertical_partial_interpolate("n-beats-s-main/m3m_nbeats_test_resilient-sweep-1.csv", "nbeats_m3_monthly", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)

# Horizontal Stability
horizontal_partial_interpolate("n-beats-s-main/m3m_nbeats_stl_test_resilient-sweep-1.csv", "nbeats_m3_monthly", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)
horizontal_partial_interpolate("n-beats-s-main/m4m_nbeats_stl_test_smart-sweep-1.csv", "nbeats_m4_monthly", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12) 



# Full interpolation experiments

# Vertical Stability
vertical_full_interpolate("n-beats-s-main/m4m_nbeats_test_smart-sweep-1.csv", "nbeats_m4_monthly", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)
vertical_full_interpolate("n-beats-s-main/m3m_nbeats_test_resilient-sweep-1.csv", "nbeats_m3_monthly", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)

# Horizontal Stability
horizontal_full_interpolate("n-beats-s-main/m3m_nbeats_stl_test_resilient-sweep-1.csv", "nbeats_m3_monthly", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m3_monthly_dataset.txt", 6, 13, 12)
horizontal_full_interpolate("n-beats-s-main/m4m_nbeats_stl_test_smart-sweep-1.csv", "nbeats_m4_monthly", c(0.2, 0.4, 0.5, 0.6, 0.8, 1), "m4_monthly_dataset.txt", 6, 13, 12)
