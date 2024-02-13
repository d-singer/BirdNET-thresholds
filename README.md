# Aggregated time-series features boost species-specific differentiation of true and false positives in passive acoustic monitoring of bird assemblages

Data and R code related to the paper Singer et al. 2024

Please cite any use of this work: 

Singer et al. (2024): Aggregated time-series features boost species-specific differentiation of true and false positives in passive acoustic monitoring of bird assemblages, Remote Sensing in Ecology and Conservation

# R scripts

The provided R-code can be used to reproduce the results of the paper, however it can also be adapted for use with own data. 

## 01_select_random_sample_for_human_validation.R

This script selects the random sample of BirdNET detections per species for human validation and writes the output to a csv-file. The csv-file can be opened and edited throughout the human validation in a spreadsheet application. Additionally, the script also extracts the audio-snippets belonging to the random samples for human validation. 

## 02_calc_aggregated_timeseries_features.R

This script calculates the aggregated time-series features for each detection of the human validated random sample. The function calculate_atf() can be found in the script "functions.R"

## 03_threshold_modelling.R

This script models the species-specific thresholds by using conditional inference trees (CIT) of type 1 and 2 and saves all models as rds.-files. 

## 03b_Barré_et_al_logistic_thresholds.R

This script was adapted from the scripts provided by Barré et al. 2019 (https://github.com/KevBarre/Semi-automated-method-to-account-for-identification-errors-in-biological-acoustic-surveys) to calculate thresholds from logistic regression as comparision to the thresholds from CIT. 

## 04_model_selection.R

This script merges all CIT models and filters models with maximum performance as candidate models. Furthermore, the basic models (BirdNET confidence score only) are filtered. 

## 05_set_cover_optimization.R

This script identifies the minimal set of model to receive at least one model with maximum performance per species through a set cover optimisation algorithm. 

## 06_performance_evaluation.R

This script compares the performance of the used threshold approaches (universal thresholds, logistic thresholds, CIT thresholds) graphically and by statistical tests.

## 07_bootstrapping_order_ATF.R

This script evaluates the importance of the aggreated time-series features for the average model performance by bootstrapping. 

## 08_reduction_of_predictors.R

This script evaluates the effects of a step-wise reduction of the included aggregated time-series features on the average model performance. 

