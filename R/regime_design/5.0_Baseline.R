# 0. Setup ----

rm(list=ls())

## Libraries and functions

library(LightFitR)
source('R/functions/regime_functions.R')

## Paths

wd = getwd()
functions = 'R/functions/'
calib_dir = 'data/heliospectra_measurements/calibration/Apollo_Calib_20240827/'
out_dir = 'data/regimes/fig5_Refinement/'

## Functions
library(LightFitR)

## Import data

load('data/algorithm_testing/fig4_algorithm_comparisons/4_algorithmsTest.Rda')

# 1. Get high, mid and low starting MSEs from fig 4 ----

message('5.0.1. Get high, mid and low starting MSEs')

## Find event with min, median and max MSE
criteria = (mse_event$complexity==4) &  (mse_event$stage=='tidied') & (mse_event$algorithm_type=='multidimensional') & (mse_event$algorithm=='nnls') & (mse_event$calibration_processing=='none')
mse_subset = mse_event[criteria,]

MSEs = sort(unique(mse_subset$MSE))
min_med_max = MSEs[c(1, length(MSEs)/2, length(MSEs))] # median() doesn't work because length is even
refine_events = mse_subset[mse_subset$MSE %in% min_med_max, 'event']

## Subset algo_test_results to these events

criteria = (algo_test_results$event %in% refine_events) & (algo_test_results$algorithm_type=='multidimensional') & (algo_test_results$algorithm=='nnls') & (algo_test_results$stage=='tidied') & (algo_test_results$calibration_processing=='none')
refinement_subset = algo_test_results[criteria,]
refinement_subset

## Format df
refinement_subset$on = refinement_subset$true_intensity != 0
refinement_subset$stage = paste(refinement_subset$algorithm_type, refinement_subset$algorithm, sep='.')
colnames(refinement_subset)

baseline_targets= data.frame(calibration_processing=refinement_subset$calibration_processing, 
                       stage=refinement_subset$stage, treat=refinement_subset$event, 
                       LED=refinement_subset$LED, wavelength=refinement_subset$wavelength, 
                       target=refinement_subset$target_irradiance, 
                       on=refinement_subset$on, intensity_used=refinement_subset$predicted_intensity)

## Set intensity_used to 0 of on==FaLSe
baseline_targets[baseline_targets$on==FALSE, 'intensity_used'] = 0

## Tidy
rm(criteria, mse_subset, refine_events, min_med_max, MSEs, refinement_subset, mse_combinations, mse_led)

# 2. Make light recipe ----

message("5.0.2. Make light recipe")

treats = unique(baseline_targets$treat)

light_recipe = sapply(treats, function(i){
  baseline_targets[baseline_targets$treat==i, 'intensity_used']
})

light_recipe = rbind(light_recipe, rep(0, ncol(light_recipe)))

# 3. Make time recipe ----

message("5.0.3. Make time recipe")


nEvents = ncol(light_recipe)

time_vec = seq(from=lubridate::hm('00:00'), by=lubridate::minutes(10), length.out=nEvents)
time_vec = as.POSIXct(time_vec, origin=lubridate::origin, tz='GMT')
time_mat = LightFitR::internal.makeTimes(time_vec)

rm(time_vec)

# 4. Make regime ----

message("5.0.4. Make regime")

regime = rbind(time_mat, light_recipe)

# 5. Export -----
message("5.0.5. Export")

setwd(out_dir)

helio.writeSchedule(regime, '5_Baseline.csv', format='csv')
helio.writeSchedule(regime, '5_Baseline.txt', format='json')
write.csv(regime, '5_Baseline_intensities.csv', col.names=F, row.names=T)

setwd(wd)

save(baseline_targets, file='data/algorithm_testing/fig5_refinement/5.0_BaselineForRefinement_20250215.Rda')