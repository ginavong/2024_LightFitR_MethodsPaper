# 0. Setup ----

rm(list=ls())

## Libraries and functions

library(LightFitR)
source('R/functions/regime_functions.R')

## Import data

load('data/algorithm_testing/fig5_refinement/5_baseline_mse.Rda')

refinement_baseline

## Define variables

nEvents = LightFitR::helio.eventLimit

## Set random seed

set.seed(148)

# 1. Make random_search function ----

message("5.2.1 Random Search Function")

random_search = function(data, leds_of_interest, nrow_output){

  ## Define search range - Make ranges proportionate to the residuals of individual LEDs
  
  bound1 = data$intensity_used - (10^4 * abs(data$diff)) # We want bound1 to be in the opposite direction of the residual, and we want it to be a big range
  bound2 = data$intensity_used + (10^2 * abs(data$diff)) # We want this to be in the same direction as the residual, but smaller
  
  bound1
  bound2
  
  bounds = rbind(bound1, bound2)
  bounds
  
  print(bounds)
  
  ## Tidy up
  
  ### Sort each pair so that they're in a sensible order
  
  bounds = apply(bounds, 2, function(i){
    sort(i)
  })
  
  ### Set non leds_of_interest to 0
  
  not_interest = setdiff(1:ncol(bounds), leds_of_interest)
  bounds[, not_interest] = c(0,0)
  
  
  ### Set negatives to 0
  
  bounds[which(bounds<0)] =0
  
  ### Limit to 1000
  bounds[which(bounds>1000)] = 1000
  
  ### Round
  
  bounds = round(bounds)
  
  ### Remove excess variables
  rm(bound1, bound2)
  
  ## Generate random search recipe based on the range provided
  
  search_recipe = apply(bounds, 2, function(i){
    possibilities = seq(i[1], i[2], by=1)
    sample(possibilities, size=nrow_output, replace=TRUE)
  })
  
  return(search_recipe)
}

# 2. Make random search recipe ----

message("5.2.2 Random search recipe")

treats = unique(refinement_baseline$treat)

search_recipe = lapply(unique(refinement_baseline$treat), function(i){
  print(i)
  
  # Define variables
  data_subset = refinement_baseline[refinement_baseline$treat==i,]
  
  LEDs_of_interest = which(data_subset$on==TRUE)
  
  # Run function
  random_search(data_subset, LEDs_of_interest, nEvents/length(treats))
  
})

search_recipe = do.call(rbind, search_recipe)


# 3. Make regime ----

message("5.2.3 Make regime")

## Time recipe

time_recipe = test_times(nEvents)

## Light recipe

light_recipe = rbind(t(search_recipe), rep(0, nEvents)) # Transpose search_recipe and add 9th LED as off

## Combine to regime

regime = rbind(time_recipe, light_recipe)
rownames(regime) = c(rownames(time_recipe), helio.dyna.leds$name)

# 4. Export ----

message("5.2.4 Export")

fp = 'data/regimes/fig5_Refinement/'

helio.writeSchedule(regime, paste(fp, '5_RandomSearch.csv', sep=''), format='csv')
helio.writeSchedule(regime, paste(fp, '5_RandomSearch.txt', sep=''), format='json')
write.csv(regime, paste(fp, '5_RandomSearch_intensities.csv', sep=''), col.names=F, row.names=T)
