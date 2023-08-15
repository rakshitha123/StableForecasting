# StableForecasting

This repository contains the experiments related to a simple linear interpolation based framework that can be used to stabilise the forecasts obtained from any base model. 

We explore two types of forecast stability, vertical stability and horizontal stability. Our framework is applicable to stabilise the forecasts provided
by any base model either vertically or horizontally up to any extent by simply changing a parameter (w_s) used during the interpolation. Furthermore, the framework can produce both accurate and stable forecasts for certain types of datasets. For more details of our proposed framework, please refer to our paper.


# Instructions for Execution

## Executing the Base Models
For the experiments, three base models are used: N-BEATS, Pooled Regression (PR) and LightGBM. All three base models are executed as global forecasting models where a single forecasting model is built across all series in a dataset. 

The PR and LightGBM models can be executed using the "do_global_forecasting" function implemented in [./experiments/other_model_experiments.R](https://github.com/rakshitha123/StableForecasting/blob/master/experiments/other_model_experiments.R) script.
The function parameters are explained in detail in the script. 
The forecasts, accuracy metrics, stability metrics and execution times of the models will be stored in "./results/forecasts", "./results/errors", "./results/errors/stability" and "./results/execution_times" folders, respectively. 
See the examples provided in [./experiments/other_model_experiments.R](https://github.com/rakshitha123/StableForecasting/blob/master/experiments/other_model_experiments.R) script under the heading "Running base models" for more details.

The N-BEATS model is executed using the implementation available at [https://github.com/KU-Leuven-LIRIS/n-beats-s](https://github.com/KU-Leuven-LIRIS/n-beats-s). 
The N-BEATS model is executed in two ways: the original N-BEATS version and the stable N-BEATS version.
The original N-BEATS model is executed by setting the parameter λ in the above [implementation]((https://github.com/KU-Leuven-LIRIS/n-beats-s)) to zero. 
The stable N-BEATS model is executed as a benchmark by setting λ to the corresponding optimal values provided in [Van Belle et al. (2023)](https://www.sciencedirect.com/science/article/abs/pii/S016920702200098X). 

## Executing the Interpolation Experiments
After obtaining the base model forecasts, you can directly execute the interpolation experiments using the functions implemented in 
[./utils/interpolate.R](https://github.com/rakshitha123/StableForecasting/blob/master/utils/interpolate.R) script.
Each function takes three parameters: path (file path of the base model forecasts), file_name (output file name) and w_s (a vector of numerical values to be used as w_s during interpolation).
The interpolation experiments of PR and LightGBM models are written in [./experiments/other_model_experiments.R](https://github.com/rakshitha123/StableForecasting/blob/master/experiments/other_model_experiments.R).
The interpolation experiments of N-BEATS model are written in [./experiments/nbeats_experiments.R](https://github.com/rakshitha123/StableForecasting/blob/master/experiments/nbeats_experiments.R).
The forecasts, accuracy metrics and stability metrics of the framework will be stored in "./results/forecasts", "./results/errors" and "./results/errors/stability" folders, respectively. 


# Experimental Datasets
The experimental datasets are available in the [datasets](https://github.com/rakshitha123/StableForecasting/tree/master/datasets) folder.


# Citing Our Work
When using this repository, please cite:

```{r} 
@article{godahewa2023stable,
  title = {On Forecast Stability},
  author = {Godahewa, Rakshitha and Bergmeir, Christoph and Song, Zhangdi},
  year = {2023}
}
```
