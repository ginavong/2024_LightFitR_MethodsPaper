# 0. Setup ----

rm(list=ls())

## Libraries and functions

library(LightFitR)
source('R/functions/regime_functions.R')

## Import data

target = 
measurement = # or residual 
intensity = 
  
## Define variables

nEvents = LightFitR::helio.eventLimit

# 1. Define search range ----

## Make ranges proportionate to the residuals of individual LEDs

bound1 = intensity - (sign(residuals) * (residuals^2)) # We want bound1 to be in the opposite direction of the residual, and we want it to be a big range
bound2 = intensity + residuals # We want this to be in the same direction as the residual, but smaller
bounds = sort(c(bound1, bound2))
bounds

# 2. Random intensities into range ----

search_recipe = apply(search_bounds, 2, function(i){
  possibilities = seq(i, by=1)
  sample(possibilities, size=nEvents, replace=TRUE)
})

# 3. Make regime ----

## Add 0s to LEDs that are not on

light_recipe = matrix(rep(0, nEvents * nrow(LightFitR::helio.dyna.leds)), nrow=nrow(LightFitR::helio.dyna.leds), ncol=nEvents)

#Go through each column and put it in the right place in the matrix

## Time recipe

time_recipe = test_times(nEvents)

## Combine to regime

regime = rbind(time_recipe, light_recipe)
rownames(regime) = c(rownames(times), helio.dyna.leds$name)

# 4. Export ----

fp = 'data/regimes/fig5_Refinement/RandomSearch/'

helio.writeSchedule(regime, paste(fp, '5_RandomSearch.csv'), format='csv')
helio.writeSchedule(regime, paste(fp, '5_RandomSearch.txt'), format='json')
write.csv(regime, paste(fp, '5_RandomSearch_intensities.csv'), col.names=F, row.names=T)