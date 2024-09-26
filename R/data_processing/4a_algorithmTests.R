# 0. Setup ----

## Define filepaths

wd = getwd()
functions = 'R/functions/'

## Libraries & functions
library(LightFitR)
library(nnls)
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

## Closest matricies needed for the multidimensional algoritms

closest_mat_calib = LightFitR::internal.closestIntensities(target_watts, calib, peaks=peaks$median_peak_wl)

closest_mat_rolling = LightFitR::internal.closestIntensities(target_watts, calib_rolling, peaks=peaks$median_peak_wl)

## multidim NNLS with calib

nnls_multidim_calib= LightFitR::nnls_intensities(target_watts, closest_mat_calib, calib$led, calib$wavelength, calib$intensity, calib$irradiance, peaks=peaks$median_peak_wl)

tidied_nnls_multidim_calib= LightFitR::internal.tidyIntensities(nnls_multidim_calib, calib$intensity) #Turns everything into an integer and caps intensities to 1000

## multidimensional NNLS with rolling calib

nnls_multidim_rolling = LightFitR::nnls_intensities(target_watts, closest_mat_rolling, calib_rolling$led, calib_rolling$wavelength, calib_rolling$intensity, calib_rolling$irradiance, peaks=peaks$median_peak_wl)

tidied_nnls_multidim_rolling = LightFitR::internal.tidyIntensities(nnls_multidim_rolling, calib_rolling$intensity)

## multidim SLE with calib

sle_multidim_calib = LightFitR::sle_intensities(target_watts, closest_mat_calib, calib$led, calib$wavelength, calib$intensity, calib$irradiance, peaks=peaks$median_peak_wl)

tidied_sle_multidim_calib = LightFitR::internal.tidyIntensities(sle_multidim_calib, calib$intensity)

## multidim SLE with rolling

sle_multidim_rolling = LightFitR::sle_intensities(target_watts, closest_mat_rolling, calib_rolling$led, calib_rolling$wavelength, calib_rolling$intensity, calib_rolling$irradiance, peaks=peaks$median_peak_wl)

tidied_sle_multidim_rolling = LightFitR::internal.tidyIntensities(sle_multidim_rolling, calib_rolling$intensity)


## Linear regression with calib

lm_calib = t(sapply(1:8, function(l){
  
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


tidied_lm_calib = LightFitR::internal.tidyIntensities(lm_calib, calib$intensity)

## Linear regression with rolling

lm_rolling = t(sapply(1:8, function(l){
  
  # Define variables
  led = LightFitR::helio.dyna.leds[l, 'wavelength']
  peak = peaks[l, 'median_peak_wl']
  led_target = data.frame(irradiance = target_watts[l,])
  
  # Subset calibration data
  criteria = (calib_rolling$led==led) & (calib_rolling$wavelength==peak)
  calib_subset = calib_rolling[criteria,]
  
  # Make the model
  mod = lm(intensity~irradiance, data=calib_subset)
  
  # Predict the intensities
  predict(mod, led_target)
}))


tidied_lm_rolling = LightFitR::internal.tidyIntensities(lm_rolling, calib_rolling$intensity)

## Individual NNLS

nnls_calib = t(sapply(1:8, function(l){
  
  # Define variables
  led = LightFitR::helio.dyna.leds[l, 'wavelength']
  peak = peaks[l, 'median_peak_wl']
  led_target = data.frame(irradiance = target_watts[l,])
  
  # Subset calibration data
  criteria = (calib$led==led) & (calib$wavelength==peak)
  calib_subset = calib[criteria,]
  
  # Make model
  mod = nnls::nnls(as.matrix(calib_subset$irradiance), calib_subset$intensity)
  
  int = mod$x * led_target
  int$irradiance
}))

tidied_nnls_calib = LightFitR::internal.tidyIntensities(nnls_calib, calib$intensity)

## Individual NNLS with rolling

nnls_rolling = t(sapply(1:8, function(l){
  
  # Define variables
  led = LightFitR::helio.dyna.leds[l, 'wavelength']
  peak = peaks[l, 'median_peak_wl']
  led_target = data.frame(irradiance = target_watts[l,])
  
  # Subset calibration data
  criteria = (calib_rolling$led==led) & (calib_rolling$wavelength==peak)
  calib_subset = calib_rolling[criteria,]
  
  # Make model
  mod = nnls::nnls(as.matrix(calib_subset$irradiance), calib_subset$intensity)
  
  int = mod$x * led_target
  int$irradiance
}))

tidied_nnls_rolling = LightFitR::internal.tidyIntensities(nnls_rolling, calib_rolling$intensity)


# 3. Tidy up ----

## Remove dfs we don't need anymore
rm(calib, calib_rolling)

# 4. Compile dataframes ----

## Setup

target_mat = target_watts[-9,]

format_df = function(calib_processing, algorithm_type, algorithm, peaks, target, true_intensities, predicted_mat, tidied_mat){
  
  # Checks
  checkDims = all((dim(true_intensities) == dim(target)), 
               (dim(true_intensities) == dim(predicted_mat)),
               (dim(true_intensities) == dim(tidied_mat)))
  stopifnot(checkDims)
  
  # Make df
  temp_df = lapply(1:ncol(true_intensities), function(i){
    
    event_vec = rep(i, nrow(true_intensities))
    calib_vec = rep(calib_processing, nrow(true_intensities))
    type_vec = rep(algorithm_type, nrow(true_intensities))
    algorithm_vec = rep(algorithm, nrow(true_intensities))
    
    cbind(calib_vec, type_vec, algorithm_vec, event_vec, peaks, target[,i], true_intensities[,i], predicted_mat[,i], tidied_mat[,i])
  })
  
  # Format df
  
  ## Basic df
  temp_df = do.call(rbind, temp_df)
  temp_df = as.data.frame(temp_df)
  colnames(temp_df) = c('calibration_processing', 'algorithm_type', 'algorithm', 'event', 'LED', 'wavelength', 'target_irradiance', 'true_intensity', 'intensity_predicted', 'intensity_tidied')
  
  ## Fancy pivot stuff
  out_df = temp_df %>%
    pivot_longer(cols=starts_with('intensity_'), names_to='stage', values_to='predicted_intensity',
                 names_prefix = 'intensity_')

  out_df = as.data.frame(out_df)
  
  return(out_df)
}


## Format dfs

df_closest_mat_calib = format_df('none', 'individual', 'closest', peaks, target_mat, regime, closest_mat_calib, closest_mat_calib)
df_closest_mat_rolling = format_df('rolling', 'individual', 'closest', peaks, target_mat, regime, closest_mat_rolling, closest_mat_rolling)

df_lm_calib = format_df('none', 'individual', 'lm', peaks, target_mat, regime, lm_calib, tidied_lm_calib)
df_lm_rolling = format_df('rolling', 'individual', 'lm', peaks, target_mat, regime, lm_rolling, tidied_lm_rolling)

df_nnls_calib = format_df('none', 'individual', 'nnls', peaks, target_mat, regime, nnls_calib, tidied_nnls_calib)
df_nnls_rolling = format_df('roling', 'individual', 'nnls', peaks, target_mat, regime, nnls_rolling, tidied_nnls_rolling)

df_nnls_multidim_calib = format_df('none', 'multidimensional', 'nnls', peaks, target_mat, regime, nnls_multidim_calib, tidied_nnls_multidim_calib)
df_nnls_multidim_rolling = format_df('rolling', 'multidimensional', 'nnls', peaks, target_mat, regime, nnls_multidim_rolling, tidied_nnls_multidim_rolling)

df_sle_multidim_calib = format_df('none', 'multidimensional', 'sle', peaks, target_mat, regime, sle_multidim_calib, tidied_sle_multidim_calib)
df_sle_multidim_rolling = format_df('rolling', 'multidimensional', 'sle', peaks, target_mat, regime, sle_multidim_rolling, tidied_sle_multidim_rolling)

## Combine into one df
algo_test_results = rbind(df_closest_mat_calib, df_closest_mat_rolling, 
                          df_lm_calib, df_lm_rolling, 
                          df_nnls_calib, df_nnls_rolling,
                          df_nnls_multidim_calib, df_nnls_multidim_rolling,
                          df_sle_multidim_calib, df_sle_multidim_rolling)

# 5. Big tidy up ----

rm(target_mat, target_watts, 
   closest_mat_calib, closest_mat_rolling,
   lm_calib, lm_rolling,
   nnls_calib, nnls_rolling,
   nnls_multidim_calib, nnls_multidim_rolling,
   sle_multidim_calib, sle_multidim_rolling,
   tidied_lm_calib, tidied_lm_rolling,
   tidied_nnls_calib, tidied_nnls_rolling,
   tidied_nnls_multidim_calib, tidied_nnls_multidim_rolling,
   tidied_sle_multidim_calib, tidied_sle_multidim_rolling,
  df_closest_mat_calib, df_closest_mat_rolling, 
  df_lm_calib, df_lm_rolling, 
  df_nnls_calib, df_nnls_rolling,
  df_nnls_multidim_calib, df_nnls_multidim_rolling,
  df_sle_multidim_calib, df_sle_multidim_rolling)

# 6. Calculate differences ----

algo_test_results$diff = algo_test_results$predicted_intensity - algo_test_results$true_intensity

# 7. Squared error ----

algo_test_results$diff_squared = algo_test_results$diff ^2

# 8. Mean squared error per event ----

## Define variables
events = unique(algo_test_results$event)
types = unique(algo_test_results$algorithm_type)
process = unique(algo_test_results$calibration_processing)
stages = unique(algo_test_results$stage)

## Calculate mse

mse = lapply(process, function(p){
  
  process_mse = lapply(types, function(ty){
    
    algos = unique(algo_test_results[algo_test_results$algorithm_type==ty, 'algorithm'])
    
    type_mse = lapply(algos, function(a){
    
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
    
    type_mse = do.call(rbind, type_mse)
    type_vec = rep(ty, nrow(type_mse))
    cbind(type_vec, type_mse)
  
  })
  
  process_mse = do.call(rbind, process_mse)
  proc_vec = rep(p, nrow(process_mse))
  cbind(proc_vec, process_mse)
})

## Formatting

mse = as.data.frame(do.call(rbind, mse))
colnames(mse) = c('calibration_processing', 'algorithm_type', 'algorithm', 'stage', 'event', 'MSE')
mse$event = as.integer(mse$event)
mse$MSE = as.numeric(mse$MSE)

## Tidy up
rm(types, stages, process)

# 9. Assigning segments based on no. LEDS active ----

## Make dictionary
complexity_dict = t(sapply(events, function(i){
  segment = length(which(regime[,i]>0))
  c(i, segment)
}))
complexity_dict = as.data.frame(complexity_dict)
colnames(complexity_dict) = c('event', 'complexity')

## Algo_test_results
algo_test_results$complexity = sapply(algo_test_results$event, function(i){
  complexity_dict[complexity_dict$event==i, 'complexity']
})

## MSE
mse$complexity = sapply(mse$event, function(i){
  complexity_dict[complexity_dict$event==i, 'complexity']
})

rm(complexity_dict)

# 10. MSE per combination of LEDs ----


# # 11. Find lowest MsE and export for refinement ----
# #TODO to figure out how to decide for later
# 
# ## Subset data
# criteria = (mse$complexity==8) & (mse$algorithm=='nnls')
# mse_subset = mse[criteria,]
# 
# mse_subset[which.min(mse_subset$MSE), ]

# 12. Export data ----

## Rearrange columns
algo_test_results = data.frame(algo_test_results$calibration_processing, algo_test_results$algorithm_type, algo_test_results$algorithm, algo_test_results$stage, 
                               algo_test_results$event, algo_test_results$complexity, algo_test_results$LED, 
                               algo_test_results$wavelength, algo_test_results$target_irradiance, algo_test_results$true_intensity, 
                               algo_test_results$predicted_intensity, algo_test_results$diff, algo_test_results$diff_squared)
colnames(algo_test_results) = c('calibration_processing', 'algorithm_type', 'algorithm', 'stage', 'event', 'complexity', 'LED', 'wavelength', 'target_irradiance', 'true_intensity', 'predicted_intensity', 'diff', 'diff_squared')

mse = data.frame(mse$calibration_processing, mse$algorithm_type, mse$algorithm, mse$stage, mse$event, mse$complexity, mse$MSE)
colnames(mse) = c('calibration_processing', 'algorithm_type', 'algorithm', 'stage', 'event', 'complexity', 'MSE')

## Export
save(algo_test_results, mse, file='data/light_testing/4a_20240905/4a_algorithmsTest.Rda')

write.csv(algo_test_results, file='data/light_testing/4a_20240905/4a_algo_test_results.csv')
write.csv(mse, file='data/light_testing/4a_20240905/4a_mse.csv')
