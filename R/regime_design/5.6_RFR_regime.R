# 0. Setup =====

message('5.6.0. Setup')

rm(list=ls())

## Directories

wd = getwd()
calib_dir = './data/heliospectra_measurements/calibration/Apollo_Calib_20240827/'
out_dir = './data/regimes/fig5b_R_FR_demo/'

## Functions

library(LightFitR)
library(lubridate)
source('./R/functions/regime_functions.R')

## Data

targets = read.csv('./data/algorithm_testing/fig5b_R_FR_demo/5b_targets.csv')

setwd(calib_dir)

load('Apollo_calibration_annotated_20240827.Rda')
calib = df

load('Apollo_calibration_medianPeaks_20240827.Rda')
medianPeaks=df

rm(df)
setwd(wd)

# 1. Create target matrix ====

message('5.6.1. Create target matrix')

## Matrix of 0s
target_mat = matrix(data=0, nrow=nrow(helio.dyna.leds), ncol=nrow(targets))
colnames(target_mat) = targets$time_approx
rownames(target_mat) = helio.dyna.leds$name

## Add red LED

target_mat['660nm',] = targets$target_R_peak
target_mat['735nm',] = targets$target_FR_peak

## Get timevector

time_vector = lubridate::as_datetime(lubridate::hm(targets$time_approx))

# 2. Filter calib data ====

message('5.6.2. Filter calib data')

criteria = (calib$middle_time==TRUE)
calib = calib[criteria,]

# 3. Make regime ====

message('5.6.3. Make regime')

RFR_regime = LightFitR::makeRegime(time_vector, target_mat, 
                                   calib$LED, calib$wavelength, calib$intensity, calib$umol,
                                   peaks=medianPeaks$median_peak_wl)

# 4. Export ====

message('5.6.4. Export regime')

fn = paste0(out_dir, '5b_RFR_demo.txt')
LightFitR::helio.writeSchedule(RFR_regime, filename=fn, format='json')

fn = paste0(out_dir, '5b_RFR_intensities.csv')
write.csv(RFR_regime, file=fn, col.names=FALSE, row.names=TRUE)
