# StableForecasting

This repository contains the experiments related to a simple linear interpolation based framework that can be used to stabilise the forecasts obtained from any base model. 

We explore two types of forecast stability, vertical stability and horizontal stability. Our framework is applicable to stabilise the forecasts provided
by any base model either vertically or horizontally up to any extent by simply changing a parameter (w_s) used during the interpolation. Furthermore, the framework can produce both accurate and stable forecasts for certain types of datasets. For more details of our proposed framework, please refer to our [paper](https://arxiv.org/pdf/2310.17332.pdf).

# How to cite our work

```{r} 
@article{godahewa2025forecast,
  title={On Forecast Stability},
  author={Godahewa, Rakshitha and Bergmeir, Christoph and Baz, Zeynep Erkin and Zhu, Chengjun and Song, Zhangdi and Garc{\'\i}a, Salvador and Benavides, Dario},
  journal={International Journal of Forecasting},
  volume = {(forthcoming)}
  url={https://arxiv.org/abs/2310.17332}
  year={2025}
}
```

# Instructions for Execution

## Executing the Base Models
For the experiments, five base models are used: N-BEATS, Pooled Regression (PR), LightGBM, Exponential Smoothing (ETS) and Autoregressive Integrated Moving Average (ARIMA). The first three models are executed as global forecasting models where a single forecasting model is built across all series in a dataset whereas ETS and ARIMA are executed as local forecasting models. 

All base models except N-BEATS can be executed using the functions, "do_forecasting" (for vertical stability) and "do_stl_forecasting" (for horizontal stability) implemented in ./experiments/other_model_experiments.R script.
The function parameters are explained in detail in the script. 
The forecasts, accuracy metrics, stability metrics and execution times of the models will be stored in "./results/forecasts", "./results/errors", "./results/errors/stability" and "./results/execution_times" folders, respectively. 
See the examples provided in ./experiments/other_model_experiments.R script under the heading "Running base models" for more details.

The N-BEATS model is executed using the implementation available at https://github.com/KU-Leuven-LIRIS/n-beats-s. 
The N-BEATS model is executed in two ways: the original N-BEATS version and the stable N-BEATS version.
The original N-BEATS model is executed by setting the parameter lambda in the above implementation to zero. 
The stable N-BEATS model is executed as a benchmark by setting lambda to the corresponding optimal values provided in Van Belle et al. 2023 (https://www.sciencedirect.com/science/article/abs/pii/S016920702200098X). 

## Executing the Interpolation Experiments
After obtaining the base model forecasts, you can directly execute the interpolation experiments using the functions implemented in 
./utils/interpolate.R script.
Each function takes seven parameters: path (file path of the base model forecasts), file_name (output file name), w_s (a vector of numerical values to be used as w_s during interpolation), input_file_name (file name of the dataset), forecast_horizon (number of required forecasts for each origin), num_origins (number of origins) and seasonality (for MASE and RMSSE calculations).
The interpolation experiments of PR, LightGBM, ETS and ARIMA models are written in ./experiments/other_model_experiments.R.
The interpolation experiments of N-BEATS model are written in ./experiments/nbeats_experiments.R.
The forecasts, accuracy metrics and stability metrics of the framework will be stored in "./results/forecasts", "./results/errors" and "./results/errors/stability" folders, respectively. 

Note: run the script, "./utils/cal_horizontal_errors.R" to obtain the stability measures of horizontal stability experiments that are reported in the revised paper.


# Experimental Datasets
The experimental datasets are available in the datasets folder.

# Reproducibility of Results in the Paper

The repository should be checked out into a directory called "StableForecasting".

The programming language is R version 4.3.3 (2024-02-29)

Required packages:
experiments/nbeats_experiments.R:library(data.table)
experiments/other_model_experiments.R:library(tidyverse)
experiments/other_model_experiments.R:library(forecast)
experiments/other_model_experiments.R:library(parallel)
experiments/other_model_experiments.R:library(doParallel)
experiments/other_model_experiments.R:library(foreach)
models/global_models.R:library(glmnet)
models/global_models.R:library(lightgbm)
models/local_univariate_models.R:library(forecast)
other/sdevs_across_datasets.r:library(xtable)
other/errors_per_horizon.r:library(ggplot2)
other/errors_per_horizon.r:library(reshape2)
other/errors_per_horizon.r:library(patchwork)
pareto_ranking/pareto_ranking.R:library(cobs)
pareto_ranking/pareto_ranking.R:library("readxl")
pareto_ranking/pareto_ranking.R:library(xtable)
utils/error_calculator.R:library(smooth)
utils/cal_horizontal_errors.R:library(data.table)
utils/cal_horizontal_errors.R:library(forecast)
utils/interpolate.R:library(data.table)
"RhpcBLASctl"

The script ./experiments/other_model_experiments.R can be run to perform all forecasting for all models from the paper except NBEATS. It expects the working directory to be set to the parent directory of the repository, with the repository in a folder called "StableForecasting". The script will use parallelisation across all cores available, and will take days to completely run on a modern computer. However, it will produce intermediary results while running, in the form of files written to disk. If for some of the methods the results are not exactly as reported in the paper, this is due to the fact that the script only sets a seed value at the beginning, so that if the script is re-run and some methods are commented out, they effectively receive different random numbers. The results reported in the paper do not stem from a single end-to-end run of the script, but from various sub-runs.

The script can be run by:
Rscript ./StableForecasting/experiments/other_model_experiments.R

NBEATS can be run by following the instructions in ./experiments/nbeats_experiments.R
This involves the use of the N-BEATS-S code that is available from here: https://github.com/VerbekeLab/n-beats-s

These scripts save their results in the "results" subdirectory. There will be subfolders "forecasts", "errors", and "execution_times" be generated. The files in the errors subfolder are the results from the paper. For example, the result reported in Table 3 in the paper of the base model for MASE for PR, "Base", M3 of 0.755 can be found in the file results/errors/m3_monthly_pooled_regression.txt Analogously, the results reported for MASC and MASC_I can be found in 

./results/errors/stability/m3_monthly_pooled_regression.txt

and 

./results/errors/stability/m3_monthly_pooled_regression_initial.txt

For the results for the partial and full interpolation methods, for example "PR", PI_0.2, M3 of 0.718  will be stored in a file called "m3_monthly_pr_horizontal_interpolate_ws_0.2.txt" . Here, PI is "interpolate" and FI is "full_interpolate" in the file names. In this way, the main results have been assembled that can be found in the repository in the intermediary file ./pareto_ranking/revision_results2.xlsx and that are presented in the main paper in Tables 2 and 3.

From this xlsx file, Figures 7-10 and Table 4 can be obtained by running the script ./pareto_ranking/pareto_ranking.R
The script expects the working directory to be set to ./pareto_ranking , so best to run the script via Rscript from that directory.



