# 0. Setup----

rm(list=ls())

## Set file directories
out_dir = 'data/heliospectra_measurements/fig5/5.1_RandomSearch_20250127/'
date_measured = 20250127

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

regime = read.csv('data/regimes/fig5_Refinement/5_RandomSearch_intensities.csv', row.names=1)

load('data/heliospectra_measurements/calibration/Apollo_Calib_20240827/Apollo_calibration_medianPeaks_20240827.Rda')
peaks = df
rm(df)

load('data/algorithm_testing/fig5_refinement/5.0_BaselineForRefinement_20250213.Rda')

# 1. Import raw measurements ----
message("5.3.1. Import raw data")

measurements = read_many.OceanView('data/heliospectra_measurements/fig5/5.1_RandomSearch_20250127/raw/')

raw = measurements

## Export raw data

fn = paste(out_dir, 'raw/', '5.1_raw_', date_measured, sep='')
save_data(measurements, fn)

rm(fn)

# 2. Trimming ----
message('5.3.2. Trimming data')

## Trim wavelengths
measurements = trim_wavelengths(measurements)

## Trim times

start = regime[1,1]
end = '12:30:00'
measurements = trim_times(start, end, measurements)

rm(start)

# 3. Annotate ----
message("5.3.3. Annotate")

## Assign event numbers

events = event_nos_timestamp(regime, measurements, end)
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

message("5.3.4. Export annotated")

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

message("5.3.5. Formatting")

## Filter for middle points and peaks

criteria = (measurements$middle_time==T) & (measurements$peak==T)
measurements2 = measurements[criteria,]
rm(criteria)

## Add treatment column

treats = unique(baseline_targets$treat)

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

## Add columns from refinement

measurements2 = left_join(measurements2, 
                           (baseline_targets |> select(wavelength, target, treat, calibration_processing, on)), 
                           join_by(wavelength, treat))

## Add intensity_used column

events = unique(measurements2$event)

intensity_used = sapply(events, function(i){
  regime[-c(1:4, 13), i]
})
intensity_used = as.numeric(as.vector(intensity_used))

measurements2$intensity_used = intensity_used

rm(events, intensity_used)

## Relative event

relative_event = sapply(1:nrow(measurements2), function(i){
  treat = measurements2[i, 'treat']
  
  rel_event = switch(as.character(treat),
                          '43' = measurements2[i, 'event'],
                          '51' = measurements2[i, 'event'] -50,
                          '58' = measurements2[i, 'event'] - 100)
  
  rel_event
})
measurements2$relative_event = relative_event
rm(relative_event)


## Add additional columns to measurements 2

measurements2$stage = rep('random.search', nrow(measurements2))

measurements2$LED = sapply(measurements2$wavelength, function(wl){
  peaks[peaks$median_peak_wl==wl, 'LED_name']
})


## Format measurements2 df

colnames(measurements2)

measurements2 = data.frame(calibration_processing = measurements2$calibration_processing,
                           stage = measurements2$stage, 
                           treat = measurements2$treat, event=measurements2$event,
                           relative_event = measurements2$relative_event,
                           LED = measurements2$LED, wavelength = measurements2$wavelength,
                           on = measurements2$on,
                           intensity_used = measurements2$intensity_used,
                           target=measurements2$target, 
                           irradiance=measurements2$irradiance, watts=measurements2$watts,
                           mol=measurements2$mol, umol=measurements2$umol)

# 6. Calculate diff ----

message("5.3.6. Calculate errors")

measurements2$diff = measurements2$umol - measurements2$target
measurements2$diff_squared = measurements2$diff^2

# 7. Calculate MSE ----

message("5.3.7. Calculate MSE")

events = unique(measurements2$event)
mse_refinement = sapply(events, function(i){
  
  data_subset = measurements2[measurements2$event==i,]
  
  discard_cols = which(colnames(data_subset) %in% c('LED', 'wavelength', 'target', 'intensity_used', 'irradiance', 'watts', 'mol', 'umol', 'diff', 'diff_squared', 'on'))
  
  # Calculate mse
  
  mse = mean(data_subset$diff_squared)
  
  # Format df
  
  row = c(data_subset[1, -discard_cols], mse)
  
  setNames(row, c(colnames(data_subset)[-discard_cols], 'MSE'))
  
  row
})

## Formatting
mse_refinement = as.data.frame(t(mse_refinement))

colnames(mse_refinement)
str(mse_refinement)
mse_refinement$calibration_processing = c(as.character(mse_refinement$calibration_processing))
mse_refinement$stage = c(as.character(mse_refinement$stage))
mse_refinement$treat = c(as.numeric(mse_refinement$treat))
mse_refinement$event = c(as.numeric(mse_refinement$event))
mse_refinement$relative_event = c(as.numeric(mse_refinement$relative_event))
mse_refinement$MSE = c(as.numeric(mse_refinement$V6))
mse_refinement = mse_refinement[,-6]
str(mse_refinement)

# 8. Lowest MSEs ----

message("5.3.8. Lowest MSE")

## Find lowest events

treats = unique(baseline_targets$treat)
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

measurements2$status = 'none'
lowest_index = which(measurements2$event %in% lowest$lowest_event)
measurements2[lowest_index, 'status'] = 'best'

rm(treats, lowest,lowest_index)

# 9. Intensity distance from best ----

message("5.3.9. Euclidian distance")

## Distance individual

best = measurements2[measurements2$status=='best',]

dist = c(sapply(events, function(i){
  data_subset = measurements2[measurements2$event==i,]
  
  # Best subset
  treat = unique(data_subset$treat)
  best_intensity = best[best$treat==treat, 'intensity_used']
  
  # Distance
  data_subset$intensity_used - best_intensity
  
}))

measurements2$dist = dist
measurements2$dist_squared = dist^2

rm(dist)

## Euclidian distance

euc_dist = sapply(1:nrow(mse_refinement), function(i){
  event = mse_refinement[i, 'event']
  dist_squared = measurements2[measurements2$event==event, 'dist_squared']
  euclidian = sqrt(sum(dist_squared))
})

mse_refinement$euc_dist = euc_dist

rm(euc_dist)

# 10. Export ----

message("5.3.10. Export")

refinement_random = measurements2
mse_refinement_random = mse_refinement

out_dir = 'data/algorithm_testing/fig5_refinement/'
setwd(out_dir)

save(refinement_random, mse_refinement_random, file='5_RandomRefinement.Rda')

setwd(wd)
