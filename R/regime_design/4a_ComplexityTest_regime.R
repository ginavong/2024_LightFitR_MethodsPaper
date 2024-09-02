# Setup

## Libraries & functions
library(LightFitR)
source("R/functions/regime_functions.R")

## Random seed
set.seed(367)

#---
# Make light recipe

## Make dataframe to break 150 events up into segments
nLED = 2:8
nOff = 8 - nLED

### Increasing no. LEDS every 20 events, for 8 LEDS

mins = c(seq(from=1, by=20, length.out=7))
maxs = c(seq(from=20, by=20, length.out=7))

### Add to dataframe
segments = data.frame(nLED = nLED, nOff = nOff, min = mins, max = maxs)

rm(nLED, nOff, mins, maxs)

## Make matrix of random light intensities

random_mat = sapply(1:helio.eventLimit, function(i){
  sample(1:1000, size=8, replace=T)
}) # All 8 LEDS will be on simultaneously like this. So in next steps, we need to overwrite some of them with 0s

## Add 0s to random LEDs such that more LEDs are on through the regime
complex_mat = lapply(1:nrow(segments), function(i){

  nOff = segments[i, 'nOff']
  
  refinedRandom = sapply(segments[i, 'min']:segments[i, 'max'], function(event){
    whichLEDoff = sample(c(1:8), size=nOff) # Randomly select which LEDs are off
    random_mat[whichLEDoff, event] = rep(0, nOff) #Replace those LEDs with 0
    random_mat[, event] #Return vector for random_mat
  })
  
  refinedRandom
})
complex_mat = do.call(cbind, complex_mat)

## Add the final few events at max complexity

complex_mat = cbind(complex_mat, random_mat[ , (ncol(complex_mat)+1):ncol(random_mat)])

#---
# Add the white LED as 0
complex_mat = rbind(complex_mat, rep(0, helio.eventLimit))

## Checks
image(complex_mat)

rm(segments, random_mat)

#---
# Make the time recipe

times = test_times(helio.eventLimit)

#---
# Make regime

regime = rbind(times, complex_mat)
rownames(regime) = c(rownames(times), helio.dyna.leds$name)

rm(times, complex_mat)

#---
# Export
helio.writeSchedule(regime, 'data/regimes/4a_ComplexityTest/4a_ComplexityTest.csv', format='csv')
helio.writeSchedule(regime, 'data/regimes/4a_ComplexityTest/4a_ComplexityTest.txt', format='json')
write.csv(regime, 'data/regimes/4a_ComplexityTest/4a_ComplexityTest_intensities.csv', col.names=F, row.names=T)
