# 0. Setup ----

rm(list=ls())

## Libraries and functions

library(LightFitR)
source('R/functions/regime_functions.R')

## Import data

load('data/algorithm_testing/fig5_refinement/5b_forGridSearch.Rda')
random_search = for_gridSearch
rm(for_gridSearch)
  
## Define variables
treats = unique(random_search$treat)
LEDs = unique(random_search$LED)

# 1. Define search range ----

search_bounds = apply(treats, function(i){
  sapply(LEDs, function(l){
    
    # Subset data
    criteria = (random_search$treat==i) & (random_search$LED==l)
    data_subset = random_search[criteria,]
    
    diff = data_subset$diff
    sign = sign(diff)
    
    if(sign[1] != sign[2]){
      bounds = data_subset$intensity_used
    }
    else{
      first = which.min(abs(diff))
      intensities = data_subset$intensity_used
      magnitude = (intensities[first] - intensities[-first]) * data_subset[data_subset$status=='best', 'diff_squared']
    }
    
  })
})
  
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