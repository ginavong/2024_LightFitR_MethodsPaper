# 0. Setup----

rm(list=ls())

## Set file directories
out_dir = 'data/heliospectra_measurements/fig5_20250114/random_search/'
date_measured = '20250114'

wd = getwd()

## Functions / libraries
library(LightFitR)
library(lubridate)

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

measurements = read_many.OceanView('data/heliospectra_measurements/fig5_20250114/random_search/raw/')

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

# 5. Calculate MSE ----


# 6. Find lowest MSEs for random search