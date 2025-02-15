# 0. Setup
rm(list=ls())

## File directories

wd = getwd()

data_dir = 'data/algorithm_testing/fig5_refinement/'

## Libraries

library(dplyr)
source('R/functions/processing_functions.R')

## Import data

fn = paste(data_dir, '5_baseline_mse.Rda', sep='')
load(fn)

fn = paste(data_dir, '5_RandomRefinement.Rda', sep='')
load(fn)

rm(fn)

# 1. Combine dataframes ----

## Refinement

colnames(refinement_baseline)
colnames(refinement_random)

refinement_baseline$dist = NA
refinement_baseline$dist_squared = NA

### Match col_order with refinement_random

#Look into dplyr::select and dplyr::relocate

colnames(refinement_baseline)
colnames(refinement_random)

### Combine

refinement = rbind(refinement_baseline, refinement_random)
str(refinement)

## MSE

colnames(mse_refinement_baseline)
colnames(mse_refinement_random)

mse_refinement_baseline$euc_dist = NA

str(mse_refinement_baseline)
str(mse_refinement_random)

### Coltypes

mse_refinement_baseline$relative_event = as.numeric(mse_refinement_baseline$relative_event)
mse_refinement_baseline$status = as.character(mse_refinement_baseline$status)

### Want colnames = c('calibration_processing', 'stage', 'treat', 'event', 'relative_event', 'status', 'MSE', 'euc_dist')

#Look into dplyr::select and dplyr::relocate

### Combine
mse_refinement = rbind(mse_refinement_baseline, mse_refinement_random)
str(mse_refinement)


# 2. Add starting_MSE column ----

treat_dict = data.frame(treat = c(43, 51, 58), label = c('high', 'low', 'mid'))

treats = refinement$treat
start_MSE = sapply(treats, function(i){
  treat_dict[treat_dict$treat==i, 'label']
})
start_MSE
refinement$start_MSE = start_MSE

treats = mse_refinement$treat
start_MSE = sapply(treats, function(i){
  treat_dict[treat_dict$treat==i, 'label']
})
mse_refinement$start_MSE = start_MSE

rm(treats, start_MSE)

# 3. Export ----

fn = paste(data_dir, '5_refinementCollated.Rda', sep='')
save(refinement, mse_refinement, file=fn)
