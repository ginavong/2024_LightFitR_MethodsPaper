# 0. Setup ----

rm(list=ls())

## Libraries and functions

library(LightFitR)
source('R/functions/regime_functions.R')

## Import data

load('data/light_testing/fig5_refinement/5a_BestSubset_forSimulation.Rda')
  
## Define variables

nEvents = 20 ^ unique(best_subset$complexity)

# 1. Define search range ----

## Make ranges proportionate to the residuals of individual LEDs

bound1 = best_subset$predicted_intensity - (2 * best_subset$diff) # We want bound1 to be in the opposite direction of the residual, and we want it to be a big range
bound2 = best_subset$predicted_intensity + (0.5 * best_subset$diff) # We want this to be in the same direction as the residual, but smaller

bound1
bound2

bounds = rbind(bound1, bound2)
bounds

## Tidy up

### Sort each pair so that they're in a sensible order

bounds = apply(bounds, 2, function(i){
  sort(i)
})

### Set negatives to 0

bounds[which(bounds<0)] =0

bounds

### Remove excess
rm(bond1, bound2)

# 2. Random intensities into range ----

search_recipe = apply(bounds, 2, function(i){
  possibilities = seq(i[1], i[2], by=1)
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