# 0. Setup

rm(list=ls())

## Directories
fun_dir = "R/functions/"
raw_dir = "data/heliospectra_measurements/calibration/Apollo_calib_20240827/raw"
calib_regime_dir = "data/regimes/calibration/calibration_intensities.csv"
out_dir = "data/heliospectra_measurements/calibration/Apollo_Calib_20240827/"

## Variables
light_name = "Apollo"
date_measured = '20240827'


## Libraries
library(LightFitR)
library(stringr)
library(lubridate)

source(paste(fun_dir, "OceanView_functions.R", sep=''))
source(paste(fun_dir, "calibration_functions.R", sep=''))
source(paste(fun_dir, "unit_conversion_functions.R", sep=''))
source(paste(fun_dir, "processing_functions.R", sep=''))


# 1. Imports ----

message('Imports')

calib_regime = read.csv(calib_regime_dir, row.names=1)
raw = read_many.OceanView(raw_dir)

## Export raw data
fn = paste(out_dir, light_name, '_calibration_raw_', date_measured, sep='')

save_data(raw, fn)

rm(fn, calib_regime_dir)

# 2. Remove unneeded bits ----

message('Trimming')

calib = na.omit(raw)

## Trim wavelengths: keep only wavelengths from 300-800nm
calib = trim_wavelengths(calib)


## Trim times: Remove values outside the calibration time
start = calib_regime[1,1]
end = "12:00:00"

calib = trim_times(start, end, calib)

rm(start)

## Checks
stopifnot(nrow(calib) < nrow(raw))
stopifnot(colnames(calib) == colnames(raw))

# 3. Add useful columns ----

message('Add useful columns')

## Assign event numbers

events = event_nos_timestamp(calib_regime, calib, end)

calib$event = events
rm(events)

## Add LED info, using calib_regime

led_cols = calib_info(calib_regime, calib, helio.dyna.leds$wavelength)
calib = cbind(calib, led_cols)

rm(led_cols)

## Find middle timepoint per event (each event is 5mins long as buffer for clocks which are out of sync by several secs)

middle = is.middle(calib$event, calib$time)
calib$middle_time = middle

rm(middle)

## Unit conversion
calib$watts = oceanViewUnits_to_watts(calib$irradiance)
calib$mol = watts_to_moles(calib$wavelength, calib$watts)
calib$umol = moles_to_umol(calib$mol)

## Save dataframe so we can go back to it

## Find peaks
peaks = find_peaks(calib, by='time')
calib$peak = peaks

rm(peaks)

## Column rearrangement
calib = data.frame(filename = calib$filename, integration_time = calib$integration_time, scans = calib$scans,
                   time=calib$time, event = calib$event, middle_time = calib$middle_time,
                   LED = calib$LED, intensity = calib$intensity, 
                   wavelength = calib$wavelength, peak = calib$peak,
                   irradiance = calib$irradiance, 
                   watts = calib$watts, mol = calib$mol, umol = calib$umol)

# 4. Exports ----
fn = paste(out_dir, light_name, '_calibration_annotated_', date_measured, sep='')
save_data(calib, fn)

# 5. Total irradiance ----

message('Total irradiance')

calib_total = get_total_irradiance(calib, by='time')

## Formatting
calib_total$integration_time = as.numeric(calib_total$integration_time)
calib_total$scans = as.numeric(calib_total$scans)
calib_total$event = as.numeric(calib_total$event)
calib_total$middle_time = as.logical(calib_total$middle_time)
calib_total$LED = as.numeric(calib_total$LED)
calib_total$intensity = as.numeric(calib_total$intensity)
calib_total$total_irradiance = as.numeric(calib_total$total_irradiance)

## Unit conversions
calib_total$watts = oceanViewUnits_to_watts(calib_total$total_irradiance)

## Export
fn = paste(out_dir, light_name, '_calibration_total_', date_measured, sep='')
save_data(calib_total, fn)

# 6. Rolling average ----

message('Rolling averages')

calib_rolling = rolling_average(calib)

## Calculate peaks
calib_rolling$peak = find_peaks(calib_rolling, by='time')

## Unit conversions
calib_rolling$watts = oceanViewUnits_to_watts(calib_rolling$irradiance)
calib_rolling$mol = watts_to_moles(calib_rolling$wavelength, calib_rolling$watts)
calib_rolling$umol = moles_to_umol(calib_rolling$mol)

## Export
fn = paste(out_dir, light_name, '_calibration_rollingAverage_', date_measured, sep='')
save_data(calib_rolling, fn)

# 7. Find median peak wavelength ----
criteria = calib$peak==T & calib$LED!=5700
calib_subset = calib[criteria,]

peaks = t(sapply(1:8, function(i){
  
  led = LightFitR::helio.dyna.leds[i, 'wavelength']
  name = LightFitR::helio.dyna.leds[i, 'name']
  
  wls_per_led = calib_subset[calib_subset$LED==led, 'wavelength']
  medianPeak = median(wls_per_led, na.rm=T)
  
  c(name, medianPeak)
}))

peaks = as.data.frame(peaks)
colnames(peaks) = c('LED_name', 'median_peak_wl')
peaks$median_peak_wl = as.numeric(peaks$median_peak_wl)

fn = paste(out_dir, light_name, '_calibration_medianPeaks_', date_measured, sep='')
save_data(peaks, fn)

rm(fn)

# 8. Calculate bleedthrough ----

## Get wls of peaks
criteria = (calib$intensity==1000) & (calib$peak==TRUE) & (calib$LED != 5700) & (calib$middle_time==TRUE)
wls = calib[criteria, 'wavelength']

## Filter data using wls
criteria = (calib$intensity==1000) & (calib$wavelength %in% wls) & (calib$LED != 5700) & (calib$middle_time==TRUE)
bleedthrough = calib[criteria,]

## Set the peaks to NA since those are difinitionally not bleedthrough
bleedthrough[bleedthrough$peak==TRUE, 'irradiance'] = NA 
bleedthrough[bleedthrough$peak==TRUE, 'watts'] = NA 
bleedthrough[bleedthrough$peak==TRUE, 'mol'] = NA 
bleedthrough[bleedthrough$peak==TRUE, 'umol'] = NA 

## structure dataframe

peaks$max_intensity_wls = wls
bleedthrough$LED2 = sapply(1:nrow(bleedthrough), function(i){
  wl = bleedthrough[i, 'wavelength']
  name = peaks[peaks$max_intensity_wls==wl, 'LED_name']
  name
})

bleedthrough$LED2 = as.factor(bleedthrough$LED2)
bleedthrough$LED1 = as.factor(str_c(bleedthrough$LED, 'nm'))
bleedthrough$wavelength = as.factor(bleedthrough$wavelength)


bleedthrough = data.frame('LED1'=bleedthrough$LED1, 'LED2'=bleedthrough$LED2, 'wavelength'=bleedthrough$wavelength, 'peak'=bleedthrough$peak, 'same' = bleedthrough$peak, 'irradiance'=bleedthrough$irradiance, 'watts'=bleedthrough$watts, 'mol'=bleedthrough$mol, 'umol'=bleedthrough$umol)

## Export

fn = paste(out_dir, light_name, '_calibration_bleedthrough_', date_measured, sep='')
save_data(bleedthrough, fn)

rm(criteria, wls, fn)