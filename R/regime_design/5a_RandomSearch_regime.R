# 0. Setup ----

rm(list=ls())

## Libraries and functions

library(LightFitR)
source('R/functions/regime_functions.R')

## Import data

load('data/light_testing/fig5_refinement/5a_BestSubset_forSimulation.Rda')

best_subset

## Define variables

LEDs_of_interest = c(3,4,7,8)
LightFitR::helio.dyna.leds[LEDs_of_interest,]

nEvents = LightFitR::helio.eventLimit

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

### Round

bounds = round(bounds)

### Remove excess
rm(bound1, bound2)

# 2. Random intensities into range ----

search_recipe = apply(bounds, 2, function(i){
  possibilities = seq(i[1], i[2], by=1)
  sample(possibilities, size=nEvents, replace=TRUE)
})

# 3. Make regime ----

## Time recipe

time_recipe = test_times(nEvents)

## Light recipe

light_recipe = rbind(t(search_recipe), rep(0, nEvents)) # Transpose search_recipe and add 9th LED as off

## Combine to regime

regime = rbind(time_recipe, light_recipe)
rownames(regime) = c(rownames(time_recipe), helio.dyna.leds$name)

# 4. Export ----

fp = 'data/regimes/fig5_Refinement/'

helio.writeSchedule(regime, paste(fp, '5a_RandomSearch.csv', sep=''), format='csv')
helio.writeSchedule(regime, paste(fp, '5a_RandomSearch.txt', sep=''), format='json')
write.csv(regime, paste(fp, '5a_RandomSearch_intensities.csv'), col.names=F, row.names=T)