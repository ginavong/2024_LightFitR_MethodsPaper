# Setup
library(lubridate)
library(LightFitR)

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
time_vec = seq(from=lubridate::hm('00:00'), by=lubridate::minutes(5), length.out=ncol(light_recipe))
time_vec = as.POSIXct(time_vec, origin=origin, tz='GMT')
time_mat = LightFitR::internal.makeTimes(time_vec)

# Make regime

regime = rbind(time_mat, light_recipe)
rownames(regime) = c(rownames(time_mat), helio.dyna.leds$name)

# Export

helio.writeSchedule(regime, 'data/regimes/calibration.csv', format='csv')
helio.writeSchedule(regime, 'data/regimes/calibration.txt', format='json')
write.csv(regime, 'data/regimes/calibration_intensities.csv', row.names=T, col.names=F)

rm(list=ls())