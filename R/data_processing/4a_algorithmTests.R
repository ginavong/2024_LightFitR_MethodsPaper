# 0. Setup ----

## Define filepaths

wd = getwd()
functions = 'R/functions/'
fig4 = 'figures/fig4'
S2 = 'figures/S2'

## Libraries & functions
library(LightFitR)
library(tidyr)

setwd(functions)
source('processing_functions.R')
setwd(wd)

## Import data

load('data/light_testing/4a_20240905/4a_targetIrradiances_20240905.Rda')
target_watts = df
rm(df)

regime = read.csv('data/regimes/4a_ComplexityTest/4a_ComplexityTest_intensities.csv', row.names=1)

load('data/calibration/Apollo_Calib_20240827/Apollo_calibration_annotated_20240827.Rda')
calib_measurements = df
rm(df)

load('data/calibration/Apollo_Calib_20240827/Apollo_calibration_rollingAverage_20240827.Rda')
calib_rolling = df
rm(df)

load('data/calibration/Apollo_Calib_20240827/Apollo_calibration_medianPeaks_20240827.Rda')
peaks = df
rm(df)

# 1. Filter & format data ----
#Cuts down on what we need to store in RAM & prevents confusion with too many columns / units

## Calibration measurements
criteria = (calib_measurements$middle_time==T)
calib = calib_measurements[criteria,]
calib = LightFitR::internal.calibCombine(calib$LED, calib$wavelength, calib$intensity, calib$watts) #Format it in the way that the package can take it

## calibration rolling
criteria = (calib_rolling$middle_time==T)
calib_rolling = calib_rolling[criteria, ]
calib_rolling = LightFitR::internal.calibCombine(calib_rolling$LED, calib_rolling$wavelength, calib_rolling$intensity, calib_rolling$watts) ##Format it in the way that the package can take it

## Regime
regime = as.matrix(regime[c(5:12), -150])
class(regime) = 'numeric'

## Tidy up 
rm(calib_measurements, criteria)

# 2. Predict regime that was used ----

## Setup
nEvents = ncol(target_watts)

## 2-stage algorithm with calib

closest_mat_calib = LightFitR::internal.closestIntensities(target_watts, calib, peaks=peaks$median_peak_wl)

predicted_regime_calib = LightFitR::nnls_intensities(target_watts, closest_mat_calib, calib$led, calib$wavelength, calib$intensity, calib$irradiance, peaks=peaks$median_peak_wl)

tidied_predicted_regime_calib = LightFitR::internal.tidyIntensities(predicted_regime_calib, calib$intensity) #Turns everything into an integer and caps intensities to 1000

## 2-stage algorithm with rolling calib

closest_mat_rolling = LightFitR::internal.closestIntensities(target_watts, calib_rolling, peaks=peaks$median_peak_wl)

predicted_regime_rolling = LightFitR::nnls_intensities(target_watts, closest_mat_rolling, calib_rolling$led, calib_rolling$wavelength, calib_rolling$intensity, calib_rolling$irradiance, peaks=peaks$median_peak_wl)

tidied_predicted_regime_rolling = LightFitR::internal.tidyIntensities(predicted_regime_rolling, calib_rolling$intensity)

## Linear regression with calib

predicted_regime_lm = t(sapply(1:8, function(l){
  
  # Define variables
  led = LightFitR::helio.dyna.leds[l, 'wavelength']
  peak = peaks[l, 'median_peak_wl']
  led_target = data.frame(irradiance = target_watts[l,])
  
  # Subset calibration data
  criteria = (calib$led==led) & (calib$wavelength==peak)
  calib_subset = calib[criteria,]
  
  # Make the model
  mod = lm(intensity~irradiance, data=calib_subset)
  
  # Predict the intensities
  predict(mod, led_target)
}))


tidied_predicted_regime_lm = LightFitR::internal.tidyIntensities(predicted_regime_lm, calib$intensity)
under0 = which(tidied_predicted_regime_lm < 0)
tidied_predicted_regime_lm[under0] = 0

rm(under0)

# 3. Tidy up ----

## Remove dfs we don't need anymore
rm(calib, calib_rolling)

# 4. Calculate differences ----

diff_closest_mat_calib = regime - closest_mat_calib
diff_closest_mat_rolling = regime - closest_mat_rolling

diff_predicted_regime_calib = regime - predicted_regime_calib
diff_predicted_regime_lm = regime - predicted_regime_lm
diff_predicted_regime_rolling = regime - predicted_regime_rolling

diff_tidied_predicted_regime_calib = regime - tidied_predicted_regime_calib
diff_tidied_predicted_regime_lm = regime - tidied_predicted_regime_lm
diff_tidied_predicted_regime_rolling = regime - tidied_predicted_regime_rolling

# 5. Compile dataframes ----

## Setup

na_mat = matrix(data=NA, nrow=nrow(regime), ncol=ncol(regime))
target_mat = target_watts[-9,]

format_df = function(calib_processing, algorithm, target, true_intensities, peaks, closest_mat, predicted_mat, tidied_mat, diff_closest, diff_predicted, diff_tidied){
  
  # Checks
  checkDims = all((dim(true_intensities) == dim(target)), 
               (dim(true_intensities) == dim(closest_mat)), 
               (dim(true_intensities) == dim(predicted_mat)),
               (dim(true_intensities) == dim(tidied_mat)), 
               (dim(true_intensities) == dim(diff_closest)), 
               (dim(true_intensities) == dim(diff_predicted)), 
               (dim(true_intensities) == dim(diff_tidied))
  )
  stopifnot(checkDims)
  
  # Make df
  temp_df = lapply(1:ncol(true_intensities), function(i){
    
    event_vec = rep(i, nrow(true_intensities))
    calib_vec = rep(calib_processing, nrow(true_intensities))
    algorithm_vec = rep(algorithm, nrow(true_intensities))
    
    cbind(calib_vec, algorithm_vec, event_vec, peaks, target[,i], true_intensities[,i], closest_mat[,i], predicted_mat[,i], tidied_mat[,i], diff_closest[,i], diff_predicted[,i], diff_tidied[,i])
  })
  
  # Format df
  
  ## Basic df
  temp_df = do.call(rbind, temp_df)
  temp_df = as.data.frame(temp_df)
  colnames(temp_df) = c('calibration_processing', 'algorithm', 'event', 'LED', 'wavelength', 'target_irradiance', 'true_intensity', 'intensity_closest', 'intensity_predicted', 'intensity_tidied', 'diff_closest', 'diff_predicted', 'diff_tidied')
  
  ## Fancy pivot stuff
  out_df = temp_df %>%
    pivot_longer(cols=intensity_closest:diff_tidied, names_to=c('type', 'stage'), names_sep='_') %>%
    pivot_wider(names_from='type', values_from='value')
  
  out_df = as.data.frame(out_df)
  
  return(out_df)
}


## Format dfs

calib_df = format_df('none', 'nnls', target_mat, regime, peaks, closest_mat_calib, predicted_regime_calib, tidied_predicted_regime_calib, diff_closest_mat_calib, diff_predicted_regime_calib, diff_tidied_predicted_regime_calib)

rolling_df = format_df('rolling', 'nnls', target_mat, regime, peaks, closest_mat_rolling, predicted_regime_rolling, tidied_predicted_regime_rolling, diff_closest_mat_rolling, diff_predicted_regime_rolling, diff_tidied_predicted_regime_rolling)

lm_df = format_df('none', 'lm', target_mat, regime, peaks, na_mat, predicted_regime_lm, tidied_predicted_regime_lm, na_mat, diff_predicted_regime_lm, diff_tidied_predicted_regime_lm)

## Combine into one df
algo_test_results = rbind(calib_df, rolling_df, lm_df)

# 6. Big tidy up ----

rm(calib_df, rolling_df, lm_df, na_mat, 
   closest_mat_calib, closest_mat_rolling,
   diff_closest_mat_calib, diff_closest_mat_rolling, 
   diff_predicted_regime_calib, diff_predicted_regime_lm, diff_predicted_regime_rolling,
   diff_tidied_predicted_regime_calib, diff_tidied_predicted_regime_lm, diff_tidied_predicted_regime_rolling, 
   predicted_regime_calib, predicted_regime_lm, predicted_regime_rolling,
   target_mat, 
   tidied_predicted_regime_calib, tidied_predicted_regime_lm, tidied_predicted_regime_rolling)

# 7. Squared error ----

algo_test_results$diff_squared = algo_test_results$diff ^2

# 8. Mean squared error ----

## Define variables
events = unique(algo_test_results$event)
algos = unique(algo_test_results$algorithm)
process = unique(algo_test_results$calibration_processing)
stages = unique(algo_test_results$stage)

## Calculate mse

mse = lapply(process, function(p){
  
  process_mse = lapply(algos, function(a){
    
    algo_mse = lapply(stages, function(s){
      
      stages_mse = t(sapply(events, function(i){
        
        criteria = (algo_test_results$calibration_processing==p) & (algo_test_results$algorithm==a) & 
          (algo_test_results$stage==s) & (algo_test_results$event==i)
        data_subset = algo_test_results[criteria,]
        
        event_mse = mean(data_subset$diff_squared)
        
        c(i, event_mse)
      }))
      
      stage_vec = rep(s, nrow(stages_mse))
      cbind(stage_vec, stages_mse)
    })
    
    algo_mse = do.call(rbind, algo_mse)
    algo_vec = rep(a, nrow(algo_mse))
    cbind(algo_vec, algo_mse)
  })
  
  process_mse = do.call(rbind, process_mse)
  proc_vec = rep(p, nrow(process_mse))
  cbind(proc_vec, process_mse)
})
mse = as.data.frame(do.call(rbind, mse))
colnames(mse) = c('calibration_processing', 'algorithm', 'stage', 'event', 'MSE')

## Tidy up
rm(algos, events, stages, process)

# 9. Export data
save(algo_test_results, mse, file='data/light_testing/4a_20240905/4a_algorithmsTest.Rda')

write.csv(algo_test_results, file='data/light_testing/4a_20240905/4a_algo_test_results.csv')
write.csv(mse, file='data/light_testing/4a_20240905/4a_mse.csv')
