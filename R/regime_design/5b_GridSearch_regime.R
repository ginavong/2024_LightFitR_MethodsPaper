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

LEDs_of_interest = c(3,4,7,8)
LightFitR::helio.dyna.leds[LEDs_of_interest,]

nEvents = 20 ^ unique(best_subset$complexity)

# 1. Define search range ----
  
# 2. Make Combinatorial grid ----


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