# 0. Setup ----

rm(list=ls())

## Libraries and functions

library(LightFitR)
source('R/functions/regime_functions.R')

## Import data

load('data/algorithm_testing/fig5_refinement/5.0_BaselineForRefinement_20250213.Rda')
load('data/heliospectra_measurements/calibration/Apollo_Calib_20240827/Apollo_calibration_annotated_20240827.Rda')
calib = df
load('data/heliospectra_measurements/calibration/Apollo_Calib_20240827/Apollo_calibration_medianPeaks_20240827.Rda')
peaks = df

rm(df)

## Format calib
calib = calib[calib$middle_time==T,]

# 1. Make Target ----

treat = unique(baseline_targets$treat)

target = sapply(treat, function(i){
  baseline_subset = baseline_targets[baseline_targets$treat==i, ]
  tar = baseline_subset$target # get target
  
  # Set not on to 0
  off = which(baseline_subset$on==FALSE)
  tar[off] = 0
  
  # Return
  tar
})

target = rbind(target, rep(0, ncol(target)))




# 2. Make Times ----

nEvents = ncol(target)

time_vec = seq(from=lubridate::hm('00:00'), by=lubridate::minutes(10), length.out=nEvents)
time_vec = as.POSIXct(time_vec, origin=lubridate::origin, tz='GMT')

# 3. Regime ----

regime = makeRegime(time_vec, target, calib$LED, calib$wavelength, calib$intensity, calib$umol, peaks=peaks$median_peak_wl, method='nnls')

# 4. Export

fp = 'data/regimes/fig5_Refinement/'


helio.writeSchedule(regime, paste(fp, '5_Baseline.csv', sep=''), format='csv')
helio.writeSchedule(regime, paste(fp, '5_Baseline.txt', sep=''), format='json')
write.csv(regime, paste(fp, '5_Baseline_intensities.csv', sep=''), col.names=F, row.names=T)