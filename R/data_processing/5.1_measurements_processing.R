# 0. Setup----

rm(list=ls())

## Set file directories
out_dir = 'data/heliospectra_measurements/fig5_20250114/random_search/'
date_measured = '20250114'

wd = getwd()

## Functions / libraries
library(LightFitR)
library(lubridate)
library(dplyr)

setwd('R/functions')
source('OceanView_functions.R')
source('unit_conversion_functions.R')
source('processing_functions.R')

setwd(wd)

## Load data

regime = read.csv('data/regimes/fig5_Refinement/ 5a_RandomSearch_intensities.csv', row.names=1)

load('data/heliospectra_measurements/calibration/Apollo_Calib_20240827/Apollo_calibration_medianPeaks_20240827.Rda')
peaks = df
rm(df)

load('data/algorithm_testing/fig5_refinement/5a_SubsetForRefinement.Rda')

# 1. Import raw measurements ----
message("1. Import raw data")

measurements = read_many.OceanView('data/heliospectra_measurements/fig5/random_search_20250114/raw/')

raw = measurements

## Export raw data

fn = paste(out_dir, 'raw/', '5a_raw_', date_measured, sep='')
save_data(measurements, fn)

rm(fn)

# 2. Trimming ----
message('2. Trimming data')

## Trim wavelengths
measurements = trim_wavelengths(measurements)

## Trim times

start = regime[1,1]
end = '12:30:00'
measurements = trim_times(start, end, measurements)

rm(start, end)

# 3. Annotate ----
message("3. Annotate")

## Assign event numbers

events = event_nos_timestamp(regime, measurements)
measurements$event = events

rm(events)

## Find middle timepoint

measurements$middle_time = is.middle(measurements$event, measurements$time)

## Unit cOnversions
measurements$watts = oceanViewUnits_to_watts(measurements$irradiance)
measurements$mol = watts_to_moles(measurements$wavelength, measurements$watts)
measurements$umol = moles_to_umol(measurements$mol)

## Peaks
measurements$peak = is.peak(measurements$wavelength, peaks$median_peak_wl)

## Assign treatments

# 4. Format & Export ----

message("4. Export annotated")

## Format
measurements = data.frame(filename = measurements$filename, 
                          integration_time=measurements$integration_time, scans = measurements$scans, 
                          time=measurements$time, event=measurements$event, middle_time=measurements$middle_time,
                          wavelength=measurements$wavelength, peak=measurements$peak,
                          irradiance=measurements$irradiance, watts=measurements$watts,
                          mol=measurements$mol, umol = measurements$umol)

## ExpoRt

fn = paste(out_dir, '5a_annotated_', date_measured, sep='')
save_data(measurements, fn)

rm(fn)

# 5.Format refinement df ----

message("5. Formatting")

## Format refinement to keep columns we want

refinement = data.frame(calibration_processing=refinement_subset$calibration_processing,
                        stage=paste(refinement_subset$algorithm_type, refinement_subset$algorithm, sep='.'), 
                        treat=refinement_subset$event, 
                        LED=refinement_subset$LED, wavelength=refinement_subset$wavelength, 
                        target=refinement_subset$target_irradiance, 
                        intensity_used=refinement_subset$predicted_intensity)

## Filter for middle points and peaks

criteria = (measurements$middle_time==T) & (measurements$peak==T)
measurements2 = measurements[criteria,]
rm(criteria)

## Add treatment column

treats = unique(refinement$treat)

measurements2$treat = sapply(measurements2$event, function(i){
  
  if((i >=1) & (i <=50)){
    treat = treats[1]
  } 
  if((i>=51) & (i<=100)){
    treat = treats[2]
  } 
  if((i>=101) & (i<=150)){
    treat=treats[3]
  }
  
  return(treat)
})

rm(treats)

## Add target column

measurements2 = left_join(measurements2, 
                           (refinement |> select(wavelength, target, treat)), 
                           join_by(wavelength, treat))

## Add intensity_used column

events = unique(measurements2$event)
intensity_used = sapply(events, function(i){
  regime[-c(1:4, 13), i]
})

intensity_used = as.numeric(as.vector(intensity_used))

measurements2$intensity_used = intensity_used

rm(events, intensity_used)

## Add additional columns to measurements 2

measurements2$calibration_processing = rep('none', nrow(measurements2))
measurements2$stage = rep('random.search', nrow(measurements2))

measurements2$LED = sapply(measurements2$wavelength, function(wl){
  peaks[peaks$median_peak_wl==wl, 'LED_name']
})

## Add additional columNs to refinement
refinement$event = rep(NA, nrow(refinement))
refinement$measured = rep(NA, nrow(refinement))

## Format measurements2 df

colnames(measurements2)

measurements2 = data.frame(calibration_processing = measurements2$calibration_processing,
                           stage = measurements2$stage, 
                           treat = measurements2$treat, event=measurements2$event,
                           LED = measurements2$LED, wavelength = measurements2$wavelength, 
                           intensity_used = measurements2$intensity_used,
                           target=measurements2$target, measured=measurements2$watts)

## Combine dfs

refinement = rbind(refinement, measurements2)

# 6. Calculate diff ----

message("6. Calculate errors")

refinement$diff = refinement$measured - refinement$target
refinement$diff_squared = refinement$diff^2

# 7. Calculate MSE ----

## Filter algorithm raw output for now

refinement = refinement[refinement$stage=='random.search',]

events = unique(refinement$event)
mse_refinement = sapply(events, function(i){
  
  data_subset = refinement[refinement$event==i,]
  
  discard_cols = which(colnames(data_subset) %in% c('LED', 'wavelength', 'target', 'intensity_used', 'measured', 'diff', 'diff_squared'))
  
  # Calculate mse
  
  mse = mean(data_subset$diff_squared)
  
  # Format df
  
  row = c(data_subset[1, -discard_cols], mse)
  
  setNames(row, c(colnames(data_subset)[-discard_cols], 'MSE'))
  
  row
})

## Formatting
mse_refinement = as.data.frame(t(mse_refinement))
colnames(mse_refinement)[ncol(mse_refinement)] = 'MSE'

mse_refinement$calibration_processing = as.character(mse_refinement$calibration_processing)
mse_refinement$stage = as.character(mse_refinement$stage)
mse_refinement$treat = as.numeric(mse_refinement$treat)
mse_refinement$event = as.numeric(mse_refinement$event)
mse_refinement$MSE = as.numeric(mse_refinement$MSE)

rm(events)

# 8. Lowest MSEs for grid search ----

message("8. Lowest MSE for grid search")

## Find lowest events

treats = unique(refinement$treat)
lowest = t(sapply(treats, function(i){
  data_subset = mse_refinement[mse_refinement$treat ==i,]
  min_event = data_subset[which.min(data_subset$MSE), 'event']
  c(i, min_event)
}))
lowest = as.data.frame(lowest)
colnames(lowest) = c('treat', 'lowest_event')

## Label dfs with this info

mse_refinement$status = 'none'
lowest_index = which(mse_refinement$event %in% lowest$lowest_event)
mse_refinement[lowest_index, 'status'] = 'best'

refinement$status = 'none'
lowest_index = which(refinement$event %in% lowest$lowest_event)
refinement[lowest_index, 'status'] = 'best'

rm(treats, lowest, second, lowest_index, second_index)

# 9. Export ----

out_dir = 'data/algorithm_testing/fig5_refinement/'
setwd(out_dir)

save(refinement, mse_refinement, file='5_refinement.Rda')
save(for_gridSearch, file='5b_forGridSearch.Rda')

setwd(wd)
