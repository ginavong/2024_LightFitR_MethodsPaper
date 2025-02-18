# 0. Setup----

rm(list=ls())

## Set file directories
out_dir = 'data/heliospectra_measurements/fig5/baseline_20250218/'
date_measured = '20250218'

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

regime = read.csv('data/regimes/fig5_Refinement/5_Baseline_intensities.csv', row.names=1)

load('data/heliospectra_measurements/calibration/Apollo_Calib_20240827/Apollo_calibration_medianPeaks_20240827.Rda')
peaks = df
rm(df)

load('data/algorithm_testing/fig5_refinement/5.0_BaselineForRefinement_20250215.Rda')


# 1. Import raw measurements ----
message("5.1.1. Import raw data")

measurements = read_many.OceanView('data/heliospectra_measurements/fig5/baseline_20250218/raw/')

raw = measurements

## Export raw data

fn = paste(out_dir, 'raw/', '5_baseline_raw_', date_measured, sep='')
save_data(measurements, fn)

rm(fn)

# 2. Trimming ----
message('5.1.2. Trimming data')

## Trim wavelengths
measurements = trim_wavelengths(measurements)

## Trim times

start = regime[1,1]
end = '00:40:00'
measurements = trim_times(start, end, measurements)

rm(start)

# 3. Annotate ----
message("5.1.3. Annotate")

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

message("5.1.4. Export annotated")

## Format
measurements = data.frame(filename = measurements$filename, 
                          integration_time=measurements$integration_time, scans = measurements$scans, 
                          time=measurements$time, event=measurements$event, middle_time=measurements$middle_time,
                          wavelength=measurements$wavelength, peak=measurements$peak,
                          irradiance=measurements$irradiance, watts=measurements$watts,
                          mol=measurements$mol, umol = measurements$umol)

## ExpoRt

fn = paste(out_dir, '5_baseline_annotated_', date_measured, sep='')
save_data(measurements, fn)

rm(fn)

# 5.Format refinement df (copied from 5a for now until we tidy up this section of code) ----

message("5.1.5. Formatting")


## Filter for middle points and peaks

criteria = (measurements$middle_time==T) & (measurements$peak==T)
measurements2 = measurements[criteria,]
rm(criteria)

## Add treatment column

treats = unique(baseline_targets$treat)
events = sort(unique(measurements2$event))
dict = cbind(events, treats)

measurements2$treat = sapply(measurements2$event, function(i){
  dict[events==i, 'treats']
})

rm(treats, events, dict)

## Add intensity_used column
events = unique(measurements2$event)

intensities = c(sapply(events, function(i){
  regime[-c(1:4, 13),i]
}))

measurements2$intensity_used = as.numeric(intensities)

rm(intensities, events)

## Add measurements to refinement

refinement = left_join(baseline_targets, 
                          (measurements2 |> select(wavelength, irradiance, watts, mol, umol, treat, event, intensity_used)), 
                          join_by(wavelength, treat, intensity_used))


## Add status column
refinement$status = rep('start', nrow(refinement))
refinement$relative_event = rep(25, nrow(refinement))

## Checks and final adjustments
str(refinement)
colnames(refinement) #=

# 6. Calculate diff ----

message("5.1.6. Calculate errors")

refinement$diff = refinement$umol - refinement$target
refinement$diff_squared = refinement$diff^2

summary(refinement)

# 7. MSE ----

message("5.1.7. Calculate MSE")

events = unique(refinement$event)

mse_refinement = sapply(events, function(i){
  
  data_subset = refinement[refinement$event==i,]
  
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
colnames(mse_refinement)[ncol(mse_refinement)] = 'MSE'

mse_refinement$calibration_processing = as.character(mse_refinement$calibration_processing)
mse_refinement$stage = as.character(mse_refinement$stage)
mse_refinement$status = as.character(mse_refinement$status)
mse_refinement$treat = as.numeric(mse_refinement$treat)
mse_refinement$event = as.numeric(mse_refinement$event)
mse_refinement$relative_event = as.numeric(mse_refinement$relative_event)
mse_refinement$MSE = as.numeric(mse_refinement$MSE)

## Tidy

rm(events)
summary(mse_refinement)


# 8. Export ----
message("5.1.8. Export")

refinement_baseline = refinement
mse_refinement_baseline = mse_refinement

out_dir = 'data/algorithm_testing/fig5_refinement/'
setwd(out_dir)

save(refinement_baseline, mse_refinement_baseline, file='5_baseline_mse.Rda')

setwd(wd)
