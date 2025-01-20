# 0. Setup----

rm(list=ls())

## Set file directories
out_dir = 'data/heliospectra_measurements/fig5_20250114/baseline/'
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

regime = read.csv('data/regimes/fig5_Refinement/ 5.0_Baseline_intensities.csv', row.names=1)

load('data/heliospectra_measurements/calibration/Apollo_Calib_20240827/Apollo_calibration_medianPeaks_20240827.Rda')
peaks = df
rm(df)

load('data/algorithm_testing/fig5_refinement/5a_SubsetForRefinement.Rda')

# 1. Import raw measurements ----
message("1. Import raw data")

measurements = read_many.OceanView('data/heliospectra_measurements/fig5_20250114/baseline/raw/')

raw = measurements

## Export raw data

fn = paste(out_dir, 'raw/', '5.0_raw_', date_measured, sep='')
save_data(measurements, fn)

rm(fn)

# 2. Trimming ----
message('2. Trimming data')

## Trim wavelengths
measurements = trim_wavelengths(measurements)

## Trim times

start = regime[1,1]
end = '00:40:00'
measurements = trim_times(start, end, measurements)

rm(start, end)

# 3. Annotate ----
message("3. Annotate")

## Assign event numbers

events = event_nos_timestamp(regime, measurements)
events[which(is.na(events))] = 3 # This is crude placeholder until we get the bug in the function fixed.
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

fn = paste(out_dir, '5.0_annotated_', date_measured, sep='')
save_data(measurements, fn)

rm(fn)

# 5.Format refinement df (copied from 5a for now until we tidy up this section of code) ----

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
  treats[i]
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
measurements2$stage = rep('multidimensinal.nnls', nrow(measurements2))

measurements2$LED = sapply(measurements2$wavelength, function(wl){
  peaks[peaks$median_peak_wl==wl, 'LED_name']
})

## Format measurements2 df

colnames(measurements2)

measurements2 = data.frame(calibration_processing = measurements2$calibration_processing,
                           stage = measurements2$stage, 
                           treat = measurements2$treat, event=measurements2$event,
                           LED = measurements2$LED, wavelength = measurements2$wavelength, 
                           intensity_used = measurements2$intensity_used,
                           target=measurements2$target, measured=measurements2$watts)

## Refinement
refinement = measurements2

# 6. Calculate diff ----

message("6. Calculate errors")

refinement$diff = refinement$measured - refinement$target
refinement$diff_squared = refinement$diff^2

# 7. MSE ----

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

# 8. Export ----
message("8. Export")

refinement_baseline = refinement
mse_refinement_baseline = mse_refinement

out_dir = 'data/algorithm_testing/fig5_refinement/'
setwd(out_dir)

save(refinement_baseline, mse_refinement_baseline, file='5.0_baseline.Rda')

setwd(wd)
