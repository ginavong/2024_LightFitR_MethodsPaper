# 0. Setup ====

rm(list=ls())

## Paths

wd = getwd()

raw_path = './data_raw/fig6_RFR_raw/'
out_path = './data/heliospectra_measurements/fig6/'

regime_dir = "data/regimes/fig6_R_FR_demo/6_RFR_intensities.csv"
peaks_dir = 'data/heliospectra_measurements/calibration/Apollo_Calib_20240827/Apollo_calibration_medianPeaks_20240827.Rda'
target_dir = './data/algorithm_testing/fig6_R_FR_demo/6_targets.csv'

functions_path = './R/functions/'

## Variables
date_measured = '20251215'

## Functions

library(dplyr)
library(tidyr)
library(lubridate)
library(LightFitR)

setwd(functions_path)
source('OceanView_functions.R')
source('unit_conversion_functions.R')
setwd(wd)

# 1. Import data ====

message("6.1.1. Import raw data")


measurements = read_many.OceanView(raw_path)
regime = read.csv(regime_dir, row.names=1)
target = read.csv(target_dir)

load(peaks_dir)
peaks = df
rm(df, raw_path)


## Export raw data

fig6_raw = measurements
fn = paste0(out_path, '6_raw_', date_measured, '.Rda')
save(fig6_raw, file=fn)
rm(fig6_raw)

# 2. Trimming ====

message("6.1.2 Trimming")

## Trim wavelengths

measurements = trim_wavelengths(measurements)

## Trim times
start = regime[1,1]
end = "21:50:00"
measurements = trim_times(start, end, measurements)

rm(start)

# 3. Annotate with useful columns ----
message("6.1.3. Annotate with useful columns")

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


## Peaks
measurements$peak = is.peak(measurements$wavelength, peaks$median_peak_wl)

# 4. Calculate R:FR ratio ====

message("6.1.4 Calculate R:FR ratio")

## Red and FR measured

criteria = (measurements$wavelength==peaks[7, 'median_peak_wl']) & (measurements$middle_time==TRUE)
measured_R = measurements[criteria, ]
measured_R$measured_R = measured_R$umol
measured_R$measured_time = measured_R$time

criteria = (measurements$wavelength==peaks[8, 'median_peak_wl']) & (measurements$middle_time==TRUE)
measured_FR = measurements[criteria,]
measured_FR$measured_FR = measured_FR$umol
measured_FR$measured_time = measured_FR$time

rm(criteria)

## Format target
target$event = 1:nrow(target)

## Combine with target

fig6_RFR_ratio = target |>
  full_join(select(measured_R, filename, measured_time, event, measured_R), join_by(event)) |>
  full_join(select(measured_FR, filename, measured_time, event, measured_FR), join_by(filename, measured_time, event))

## Calculate ratio

fig6_RFR_ratio$measured_RFR = fig6_RFR_ratio$measured_R / fig6_RFR_ratio$measured_FR

# 5. Make measurements df ====

message("6.1.5 Make measurements df")

cols_interest = c('filename', 
                  'event', 'time_char', 'middle_time', 'time', 'solar_elevation_angle',
                  'type', 
                  'colour', 'wavelength', 'umol')

## Filter & format spectrum data
wls_interest = peaks[c(7,8), 'median_peak_wl']
colour_dict = data.frame(wavelength=wls_interest, colour=c('red', 'far-red'))

measurements_filtered = measurements |> filter(wavelength %in% wls_interest) |> #Filter for red / far-red only
  full_join(select(target, event, solar_elevation_angle), by='event') |> # Add necesSary columns from target
  full_join(colour_dict, by='wavelength', relationship='many-to-one') |> #Add colour column
  mutate(type='measured', time_char = time, time=lubridate::hms(time)) |> # Add other missing columns
  select(all_of(cols_interest)) #Re-arrange columns in right order

## Format target df
R_FR_dict = data.frame(target=c('target_R_peak', 'target_FR_peak'), type=c('target', 'target'), 
                       wavelength=peaks[c(7,8), 'median_peak_wl'], colour=c('red', 'far-red'))

target_long = target |> pivot_longer(cols=starts_with('target'), names_to='target', values_to='umol') |>
  filter(target != 'target_R_FR') |> #Remove rows with 'target_R_FR' that got included with the starts_with
  #Add columns: type and wavelength
  full_join(R_FR_dict, by='target', relationship='many-to-one') |>
  #Add relevant columns from measurements
  full_join(select(measurements_filtered, event, time_char, middle_time, time, wavelength), 
            by=join_by(event, wavelength), relationship='one-to-many') |>
  mutate(filename='6_targets.csv') |> #Add missing columns
  #Re-arrange columns in right order
  select(all_of(cols_interest))

# Rbind
measurements_summary = rbind(measurements_filtered, target_long)
  

rm(cols_interest, wls_interest, colour_dict, measurements_filtered, R_FR_dict, target_long)

# 6. Format & Export ====

message("6.1.6 Format & export")

## Arrange columns sensibly

fig6_spectrum = measurements |> select(filename, integration_time, scans, 
                                   time, event, middle_time,
                                   wavelength, peak,
                                   irradiance, watts, mol, umol)

fig6_RFR_ratio = fig6_RFR_ratio |> select(event, solar_elevation_angle, time_approx, measured_time,
                                          filename,
                                          Kotilainen_target_R_FR_approx,
                                          target_R_peak, target_FR_peak, target_R_FR,
                                          measured_R, measured_FR, measured_RFR)



## Export

fn = paste0(out_path, 'fig6_data_', date_measured, '.Rda')
save(fig6_spectrum, fig6_RFR_ratio, measurements_summary, file=fn)