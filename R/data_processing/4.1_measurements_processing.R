# Setup ----

rm(list=ls())

## Define paths
wd = getwd()
out_dir = "data/algorithm_testing/fig4_algorithm_comparisons/"

raw_dir = "data/heliospectra_measurements/fig4_20240905/raw"
measurements_dir = "data/heliospectra_measurements/fig4_20240905/"
regime_dir = "data/regimes/fig4_ComplexityTest/4_ComplexityTest_intensities.csv"
peaks_dir = 'data/heliospectra_measurements/calibration/Apollo_Calib_20240827/Apollo_calibration_medianPeaks_20240827.Rda'

## Define variables
date_measured = "20240905"

## Libraries & functions
library(LightFitR)
library(lubridate)
library(stringr)

setwd("R/functions")
source("OceanView_functions.R")
source("processing_functions.R")
source("unit_conversion_functions.R")
setwd(wd)

# 1. Data import ----
message("1. Import data")

measurements = read_many.OceanView(raw_dir)
regime = read.csv(regime_dir, row.names=1)

load(peaks_dir)
peaks = df
rm(df, raw_dir)

## Export Raw data

fn = paste(measurements_dir, "4_raw_", date_measured, sep='')
save_data(measurements, fn)

rm(fn)

# 2. Trimming ----
message("2. Trimming data")

## Trim wavelengths
measurements = trim_wavelengths(measurements)

## Trim times
start = regime[1,1]
end = "12:30:00"
measurements = trim_times(start, end, measurements)

rm(start)

# 3. Annotate with useful columns ----
message("3. Annotate with useful columns")

## Assign event numbers
events = event_nos_timestamp(regime, measurements, end)
measurements$event = events

rm(events)

## Find middle timepoint of the event

middle = is.middle(measurements$event, measurements$time)
measurements$middle_time = middle

rm(middle)

## Unit conversion
measurements$watts = oceanViewUnits_to_watts(measurements$irradiance)
measurements$mol = watts_to_moles(measurements$wavelength, measurements$watts)
measurements$umol = moles_to_umol(measurements$mol)

## Peaks (problem for another day)
measurements$peak = is.peak(measurements$wavelength, peaks$median_peak_wl)

# 4. Format & export ----
message("4. Format & export")

## Format
measurements = data.frame(filename = measurements$filename, 
                          integration_time=measurements$integration_time, scans = measurements$scans, 
                          time=measurements$time, event=measurements$event, middle_time=measurements$middle_time,
                          wavelength=measurements$wavelength, peak=measurements$peak,
                          irradiance=measurements$irradiance, watts=measurements$watts,
                          mol=measurements$mol, umol = measurements$umol)

## Export
fn = paste(measurements_dir, "4_annotated_", date_measured, sep='')
save_data(measurements, fn)

rm(fn, peaks)

# 5. Process measurements into target irradiances ----

message('5. Process into target irradiances')

## Filter data
criteria = (measurements$middle_time==T) & (measurements$peak==T)
measurements_filtered = measurements[criteria,]

rm(criteria)

## Check all events are represented
events = sort(unique(measurements_filtered$event))

length(events)
which(!(1:150 %in% events)) # This is the event we need to exclude from the regime dataset

## Format into matrix

target = sapply(events, function(i){
  print(i)
  
  dataSubset = measurements_filtered[measurements_filtered$event==i, ]
  
  ledOrder = order(dataSubset$wavelength)
  
  event_irradiances = dataSubset[ledOrder, 'watts']
  event_irradiances
})

## Add row of 0s for whitéchannel

target = rbind(target, rep(0, ncol(target)))

## Export

fn = paste(out_dir, "4_targetIrradiances_", date_measured, sep='')
save_data(target, fn)
rm(fn)