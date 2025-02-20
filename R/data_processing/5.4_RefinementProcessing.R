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

# 1. Combine dataframes ----

message("5.4.1. Combine dataframes")

## Refinement

colnames(refinement_baseline)
colnames(refinement_random)

refinement_baseline$dist = as.numeric(NA)
refinement_baseline$dist_squared = as.numeric(NA)

str(refinement_baseline)
str(refinement_random)

### Combine

refinement = rbind(refinement_baseline, refinement_random)
str(refinement)

## MSE

colnames(mse_refinement_baseline)
colnames(mse_refinement_random)

mse_refinement_baseline$euc_dist = as.numeric(NA)

str(mse_refinement_baseline)
str(mse_refinement_random)

### Combine
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
