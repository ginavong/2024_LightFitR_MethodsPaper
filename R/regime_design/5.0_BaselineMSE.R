# 0. Setup ----

rm(list=ls())

## Libraries and functions

library(LightFitR)
source('R/functions/regime_functions.R')

## Import data

load('data/algorithm_testing/fig5_refinement/5.0_BaselineForRefinement.Rda')


# 1. Make light recipe----

treat = unique(refinement$treat)

light_recipe = sapply(treat, function(i){
  
  data_subset = refinement[refinement$treat==i,]
  
  recipe = data_subset$intensity_used
  
  recipe
})

# 2. Time recipe ----
#This is sligtly different to the typical time recipe. BC there are so few points, we can have 10mins per event.

nEvents = ncol(light_recipe)
time_vec = seq(from=lubridate::hm('00:00'), by=lubridate::minutes(10), length.out=nEvents)
time_vec = as.POSIXct(time_vec, origin=lubridate::origin, tz='GMT')
time_mat = LightFitR::internal.makeTimes(time_vec)

rm(time_vec)

# 3. Regime ----

regime = rbind(time_mat, light_recipe, rep(0, ncol(light_recipe)))
rownames(regime) = c(rownames(time_mat), LightFitR::helio.dyna.leds$name)

## Export

fp = 'data/regimes/fig5_Refinement/'


helio.writeSchedule(regime, paste(fp, '5.0_Baseline.csv', sep=''), format='csv')
helio.writeSchedule(regime, paste(fp, '5.0_Baseline.txt', sep=''), format='json')
write.csv(regime, paste(fp, '5.0_Baseline_intensities.csv'), col.names=F, row.names=T)