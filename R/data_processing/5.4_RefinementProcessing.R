# 0. Setup
rm(list=ls())

## File directories

wd = getwd()

data_dir = 'data/algorithm_testing/fig5_refinement/'

## Libraries

library(dplyr)
source('R/functions/processing_functions.R')

## Import data

setwd(data_dir)
load('5_baseline_mse.Rda')
load('5_RandomRefinement.Rda')
setwd(wd)

# 1. Calculate Euclidian distance of baseline ----
message('5.4.1. Euclidian distance of baseline')

## Dist
best = refinement_random[refinement_random$status=='best',]
treats = unique(refinement_baseline$treat)

refinement_baseline$dist = as.numeric(sapply(treats, function(i){
  best_intensity = best[best$treat==i, 'intensity_used']
  baseline_intensity = refinement_baseline[refinement_baseline$treat==i, 'intensity_used']
  
  dist = baseline_intensity - best_intensity
  dist
}))

refinement_baseline$dist_squared = as.numeric(refinement_baseline$dist ^2)

## Euclidian distance

treats = unique(mse_refinement_baseline$treat)

mse_refinement_baseline$euc_dist = as.numeric(sapply(treats, function(i){
  dist_squared = refinement_baseline[refinement_baseline$treat==i, 'dist_squared']
  mean(dist_squared)
}))

## Tidy
rm(best, treats)

# 2. Combine dataframes ----

message("5.4.2. Combine dataframes")

## Refinement

colnames(refinement_baseline)
colnames(refinement_random)

str(refinement_baseline)
str(refinement_random)

refinement = rbind(refinement_baseline, refinement_random)
str(refinement)

## MSE

colnames(mse_refinement_baseline)
colnames(mse_refinement_random)

str(mse_refinement_baseline)
str(mse_refinement_random)

mse_refinement = rbind(mse_refinement_baseline, mse_refinement_random)
str(mse_refinement)

# 2. Add starting_MSE column ----

message("5.4.2. Add columns")

treat_dict = data.frame(treat = c(56, 57, 60), label = c('low', 'mid', 'high'))

treats = refinement$treat
start_MSE = sapply(treats, function(i){
  treat_dict[treat_dict$treat==i, 'label']
})
start_MSE
refinement$start_MSE = c(as.character(start_MSE))

treats = mse_refinement$treat
start_MSE = sapply(treats, function(i){
  treat_dict[treat_dict$treat==i, 'label']
})
mse_refinement$start_MSE = c(as.character(start_MSE))

rm(treats, start_MSE)

# 3. Export ----

message("5.4.3. Export")

fn = paste(data_dir, '5_refinementCollated.Rda', sep='')
save(refinement, mse_refinement, file=fn)
