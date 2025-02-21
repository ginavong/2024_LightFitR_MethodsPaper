# 0. Setup ----

rm(list=ls())

## Define filepaths

wd = getwd()
functions = 'R/functions/'
calib_dir = 'data/heliospectra_measurements/calibration/Apollo_Calib_20240827/'
out_dir = 'data/algorithm_testing/fig4_algorithm_comparisons/'

## Libraries & functions
library(LightFitR)
library(nnls)
library(tidyr)
library(dplyr)

setwd(functions)
source('processing_functions.R')
setwd(wd)

## Import data

load('data/algorithm_testing/fig4_algorithm_comparisons/4_targetIrradiances_20240905.Rda')
target = df
rm(df)

regime = read.csv('data/regimes/fig4_ComplexityTest/4_ComplexityTest_intensities.csv', row.names=1)

### Calibration data

setwd(calib_dir)

load('Apollo_calibration_annotated_20240827.Rda')
calib_measurements = df
rm(df)

load('Apollo_calibration_medianPeaks_20240827.Rda')
peaks = df
rm(df)

load('Apollo_calibration_bleedthrough_20240827.Rda')
bleedthrough = df
rm(df)

setwd(wd)

# 1. Filter & format data ----
#Cuts down on what we need to store in RAM & prevents confusion with too many columns / units

message('4.2.1. Filter dataframe')

## Calibration measurements
criteria = (calib_measurements$middle_time==T)
calib = calib_measurements[criteria,]
calib = LightFitR::internal.calibCombine(calib$LED, calib$wavelength, calib$intensity, calib$umol) #Format it in the way that the package can take it

## Regime
regime = as.matrix(regime[c(5:12), ])
class(regime) = 'numeric'

## Tidy up 
rm(calib_measurements, criteria)

# 2. Predict regime that was used ----

message('4.2.2. Running algorithms')

## Setup
nEvents = ncol(target)

## Closest matricies needed for the multidimensional algoritms

closest_mat_calib = LightFitR::internal.closestIntensities(target, calib, peaks=peaks$median_peak_wl)

## multidim NNLS with calib

nnls_multidim_calib= LightFitR::nnls_intensities(target, closest_mat_calib, calib$led, calib$wavelength, calib$intensity, calib$irradiance, peaks=peaks$median_peak_wl)

tidied_nnls_multidim_calib= LightFitR::internal.tidyIntensities(nnls_multidim_calib, calib$intensity) #Turns everything into an integer and caps intensities to 1000

## multidim SLE with calib

sle_multidim_calib = LightFitR::sle_intensities(target, closest_mat_calib, calib$led, calib$wavelength, calib$intensity, calib$irradiance, peaks=peaks$median_peak_wl)

tidied_sle_multidim_calib = LightFitR::internal.tidyIntensities(sle_multidim_calib, calib$intensity)

## Linear regression with calib

lm_calib = t(sapply(1:8, function(l){
  
  # Define variables
  led = LightFitR::helio.dyna.leds[l, 'wavelength']
  peak = peaks[l, 'median_peak_wl']
  led_target = data.frame(irradiance = target[l,])
  
  # Subset calibration data
  criteria = (calib$led==led) & (calib$wavelength==peak)
  calib_subset = calib[criteria,]
  
  # Make the model
  mod = lm(intensity~irradiance, data=calib_subset)
  
  # Predict the intensities
  predict(mod, led_target)
}))


tidied_lm_calib = LightFitR::internal.tidyIntensities(lm_calib, calib$intensity)

## Individual NNLS

nnls_calib = t(sapply(1:8, function(l){
  
  # Define variables
  led = LightFitR::helio.dyna.leds[l, 'wavelength']
  peak = peaks[l, 'median_peak_wl']
  led_target = data.frame(irradiance = target[l,])
  
  # Subset calibration data
  criteria = (calib$led==led) & (calib$wavelength==peak)
  calib_subset = calib[criteria,]
  
  # Make model
  mod = nnls::nnls(as.matrix(calib_subset$irradiance), calib_subset$intensity)
  
  int = mod$x * led_target
  int$irradiance
}))

tidied_nnls_calib = LightFitR::internal.tidyIntensities(nnls_calib, calib$intensity)

# 3. Tidy up ----

## Remove dfs we don't need anymore
rm(calib)

# 4. Compile dataframes ----

message('4.2.4. Formatting dataframe')

## Setup

target_mat = target[-9,]

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

df_lm_calib = format_df('none', 'individual', 'lm', peaks, target_mat, regime, lm_calib, tidied_lm_calib)

df_nnls_calib = format_df('none', 'individual', 'nnls', peaks, target_mat, regime, nnls_calib, tidied_nnls_calib)

df_nnls_multidim_calib = format_df('none', 'multidimensional', 'nnls', peaks, target_mat, regime, nnls_multidim_calib, tidied_nnls_multidim_calib)

df_sle_multidim_calib = format_df('none', 'multidimensional', 'sle', peaks, target_mat, regime, sle_multidim_calib, tidied_sle_multidim_calib)

## Combine into one df
algo_test_results = rbind(df_closest_mat_calib,  
                          df_lm_calib,
                          df_nnls_calib,
                          df_nnls_multidim_calib, 
                          df_sle_multidim_calib)

# 5. Big tidy up ----

rm(target_mat, target, 
   closest_mat_calib, 
   lm_calib, 
   nnls_calib, 
   nnls_multidim_calib, 
   sle_multidim_calib, 
   tidied_lm_calib, 
   tidied_nnls_calib,
   tidied_nnls_multidim_calib, 
   tidied_sle_multidim_calib, 
  df_closest_mat_calib, 
  df_lm_calib, 
  df_nnls_calib, 
  df_nnls_multidim_calib, 
  df_sle_multidim_calib)

# 6. Calculate differences ----

message('4.2.6. Calculate differences')

algo_test_results$diff = algo_test_results$predicted_intensity - algo_test_results$true_intensity

# 7. Squared error ----

algo_test_results$diff_squared = algo_test_results$diff ^2

# 8. Mean squared error ----

message('4.2.8. Calculating mean squared error')

## MSE function

### Define variables

process = unique(algo_test_results$calibration_processing)
types = unique(algo_test_results$algorithm_type)
stages = unique(algo_test_results$stage)


### Define function

calculate_mse = function(data, by, process, types, stages){
  
  # Calculate
  
  mse = lapply(process, function(p){
    
    process_mse = lapply(types, function(ty){
      
      algos = unique(data[data$algorithm_type==ty, 'algorithm'])
      
      type_mse = lapply(algos, function(a){
        
        algo_mse = lapply(stages, function(s){
          
          bys = unique(data[,by])
          
          stages_mse = t(sapply(bys, function(i){
            
            criteria = (data$calibration_processing==p) & 
              (data$algorithm_type==ty) & (data$algorithm==a) & 
              (data$stage==s) & (data[, by]==i)
            data_subset = data[criteria,]
            
            by_mse = mean(data_subset$diff_squared)
            
            c(i, by_mse)
          }))
          
          stage_vec = rep(s, nrow(stages_mse))
          stages_mse = cbind(stage_vec, stages_mse)
          
          stages_mse
        })
        
        
        algo_mse = do.call(rbind, algo_mse)
        algo_vec = rep(a, nrow(algo_mse))
        algo_mse = cbind(algo_vec, algo_mse)
        
        algo_mse
      })
      
      type_mse = do.call(rbind, type_mse)
      type_vec = rep(ty, nrow(type_mse))
      type_mse = cbind(type_vec, type_mse)
      
      type_mse
    })
    
    process_mse = do.call(rbind, process_mse)
    proc_vec = rep(p, nrow(process_mse))
    process_mse = cbind(proc_vec, process_mse)
    
    process_mse
  })
  
  # Formatting
  
  mse = data.frame(do.call(rbind, mse))
  colnames(mse) = c('calibration_processing', 'algorithm_type', 'algorithm', 'stage', by, 'MSE')
  mse$MSE = as.numeric(mse$MSE)
  
  return(mse)
}

## MSE per event

mse_event = calculate_mse(algo_test_results, 'event', process, types, stages)
mse_event$event = as.integer(mse_event$event)


## MSE per LED

mse_led = calculate_mse(algo_test_results, 'LED', process, types, stages)
  

# 9. Assigning segments based on no. LEDS active ----

message('4.2.9 Assign segments')

## Make dictionary

events = unique(algo_test_results$event)

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
mse_event$complexity = sapply(mse_event$event, function(i){
  complexity_dict[complexity_dict$event==i, 'complexity']
})

rm(complexity_dict)

# 10. MSE per combination of LEDs ----

message('4.2.10. MSE by combination of LED (experimental)')

## Make the combinations

combinations = expand.grid(peaks$LED_name, peaks$LED_name, stringsAsFactors = FALSE)
colnames(combinations) = c('LED1', 'LED2')

## Calculate MSE

mse_combinations = lapply(1:nrow(combinations), function(i){
  
  # Set LEDs
  led1 = combinations[i, 1]
  led2 = combinations[i, 2]
  
  mse = lapply(process, function(p){
    
    process_mse = lapply(types, function(ty){
      
      algos = unique(algo_test_results[algo_test_results$algorithm_type==ty, 'algorithm'])
      
      type_mse = lapply(algos, function(a){
        
        algo_mse = lapply(stages, function(s){
            
          criteria = (algo_test_results$calibration_processing==p) & 
            (algo_test_results$algorithm_type==ty) & (algo_test_results$algorithm==a) & 
            (algo_test_results$stage==s) & (algo_test_results$LED == led1 | algo_test_results$LED == led2)
          data_subset = algo_test_results[criteria,]
          
          stage_mse = mean(data_subset$diff_squared)
          
          vec = c(s, led1, led2, stage_mse)
          vec
        })

        algo_mse = do.call(rbind, algo_mse)
        algo_vec = rep(a, nrow(algo_mse))
        algo_mse = cbind(algo_vec, algo_mse)
        
        algo_mse
      })
      
      type_mse = do.call(rbind, type_mse)
      type_vec = rep(ty, nrow(type_mse))
      type_mse = cbind(type_vec, type_mse)
      
      type_mse
    })
    
    process_mse = do.call(rbind, process_mse)
    proc_vec = rep(p, nrow(process_mse))
    process_mse = cbind(proc_vec, process_mse)
    
    process_mse
  })
  
  # Formatting
  
  mse = data.frame(do.call(rbind, mse))
  return(mse)
})

### Tidy the dataframe

mse_combinations = data.frame(do.call(rbind, mse_combinations))
colnames(mse_combinations) = c('calibration_processing', 'algorithm_type', 'algorithm', 'stage', 'LED1', 'LED2', 'MSE')

str(mse_combinations)
mse_combinations$MSE = as.numeric(mse_combinations$MSE)

## Column to indicate if LED1 == LED2
same = mse_combinations$LED1 == mse_combinations$LED2
mse_combinations$same = same

## Add bleedthrough stats

mse_combinations = left_join(mse_combinations, bleedthrough[, -c(3,4)], by=c('LED1'='LED1', 'LED2'='LED2', 'same'='same'))


## Alternative MSE calculation

mse_combinations2 = lapply(1:nrow(peaks), function(i){
  
  # Define variables
  led1 = peaks[i, 'LED_name']
  
  criteria = (algo_test_results$LED ==led1) & (algo_test_results$true_intensity >0)
  events = unique(algo_test_results[criteria, 'event'])
  print(length(events))
  
  # Subset data
  criteria = algo_test_results$event %in% events
  results_subset = algo_test_results[criteria,]
  
  # Calculate MSE
  mse = calculate_mse(data=results_subset, by='LED', process, types, stages)
  
  # Format df
  
  led1_vec = rep(led1, nrow(mse))
  mse_without_single = mse$MSE - mse_led$MSE
  
  mse_df = cbind(led1_vec, mse_without_single, mse)
  
  mse_df
  
})

mse_combinations2 = do.call(rbind, mse_combinations2)
colnames(mse_combinations2) = c('LED1', 'mse_without_single', 'calibration_processing', 'algorithm_type', 'algorithm', 'stage', 'LED2', 'MSE')

mse_combinations2$same = mse_combinations2$LED1 == mse_combinations2$LED2

mse_combinations2 = left_join(mse_combinations2, bleedthrough[, -c(3,4)], by=c('LED1'='LED1', 'LED2'='LED2', 'same'='same'))

## Tidy

rm(process, types, stages)

# 11. Export data ----

message('4.2.11. Exporting data')

## Rearrange columns

algo_test_results = data.frame(algo_test_results$calibration_processing, algo_test_results$algorithm_type, algo_test_results$algorithm, algo_test_results$stage, 
                               algo_test_results$event, algo_test_results$complexity, algo_test_results$LED, 
                               algo_test_results$wavelength, algo_test_results$target_irradiance, algo_test_results$true_intensity, 
                               algo_test_results$predicted_intensity, algo_test_results$diff, algo_test_results$diff_squared)
colnames(algo_test_results) = c('calibration_processing', 'algorithm_type', 'algorithm', 'stage', 'event', 'complexity', 'LED', 'wavelength', 'target_irradiance', 'true_intensity', 'predicted_intensity', 'diff', 'diff_squared')

mse_event = data.frame(mse_event$calibration_processing, mse_event$algorithm_type, mse_event$algorithm, mse_event$stage, mse_event$event, mse_event$complexity, mse_event$MSE)
colnames(mse_event) = c('calibration_processing', 'algorithm_type', 'algorithm', 'stage', 'event', 'complexity', 'MSE')

mse_led = data.frame(mse_led$calibration_processing, mse_led$algorithm_type, mse_led$algorithm, mse_led$stage, mse_led$LED, mse_led$MSE)
colnames(mse_led) = c('calibration_processing', 'algorithm_type', 'algorithm', 'stage', 'LED', 'MSE')

mse_combinations = data.frame(mse_combinations$calibration_processing, mse_combinations$algorithm_type, mse_combinations$algorithm, mse_combinations$stage, mse_combinations$LED1, mse_combinations$LED2, mse_combinations$same, mse_combinations$MSE, mse_combinations$irradiance, mse_combinations$irradiance, mse_combinations$watts, mse_combinations$mol, mse_combinations$umol)
colnames(mse_combinations) = c('calibration_processing', 'algorithm_type', 'algorithm', 'stage', 'LED1', 'LED2', 'same', 'MSE', 'bleedthrough_irradiance', 'bleedthrough_watts', 'bleedthrough_mol', 'bleedthrough_umol')

## Export

setwd(out_dir)

save(algo_test_results, mse_event, mse_led, mse_combinations, file='4_algorithmsTest.Rda')

write.csv(algo_test_results, file='4_algo_test_results.csv')
write.csv(mse_event, file='4_mse_event.csv')
write.csv(mse_led, file='4_mse_led.csv')
write.csv(mse_combinations, file='4_mse_combinations.csv')

setwd(wd)