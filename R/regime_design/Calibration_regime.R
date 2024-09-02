# Setup
library(lubridate)
library(LightFitR)
source("R/functions/regime_functions.R")

# Get intensities
intensities_head = c(0, 1, 5, 10, 20, 50)
intensities_main = seq(100, 1000, by=100)

intensities = c(intensities_head, intensities_main)

## Checks
length(intensities)
length(intensities) * 9

## Cleanup
rm(intensities_head, intensities_main)

# Do for all lights
light_recipe = lapply(1:nrow(helio.dyna.leds), function(l){
  temp_matrix = matrix(0, nrow = nrow(helio.dyna.leds), ncol=length(intensities))
  temp_matrix[l,] = intensities
  temp_matrix
})
light_recipe = do.call(cbind, light_recipe)

## Checks
dim(light_recipe)

# Times
time_mat = test_times(ncol(light_recipe))

# Make regime

regime = rbind(time_mat, light_recipe)
rownames(regime) = c(rownames(time_mat), helio.dyna.leds$name)

# Export

helio.writeSchedule(regime, 'data/regimes/calibration.csv', format='csv')
helio.writeSchedule(regime, 'data/regimes/calibration.txt', format='json')
write.csv(regime, 'data/regimes/calibration_intensities.csv', row.names=T, col.names=F)

rm(list=ls())