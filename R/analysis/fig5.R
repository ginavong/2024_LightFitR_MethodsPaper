# 0. Setup ----

rm(list=ls())

## Set file directories

wd = getwd()

fun_dir = 'R/functions/'

data_dir = 'data/algorithm_testing/fig5_refinement/'
main_dir = 'figures/fig5/'
sup_dir = 'figures/S4'

## Functions / Libraries

library(ggplot2)
library(ggbeeswarm)

setwd(fun_dir)
source('ggplot_functions.R')
setwd(wd)

## Load data

setwd(data_dir)
load('5.0_baseline.Rda')
load('5_refinement.Rda')
setwd(wd)

# 1. Format df

## Combine dataframes (Temporary until I sort my codebase out) ----

mse_refinement_baseline$status = rep('none', nrow(mse_refinement_baseline))
refinement_baseline$status = rep('none', nrow(refinement_baseline))

refinement = rbind(refinement_baseline, refinement)
mse_refinement = rbind(mse_refinement_baseline, mse_refinement)

summary(refinement)
summary(mse_refinement)
str(refinement)
str(mse_refinement)

rm(mse_refinement_baseline, refinement_baseline)

## datatypes

ref_types = c('character', 'character', 'character', 'numeric', 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'character')
for(i in 1:length(ref_types)){
  class(refinement[,i]) = ref_types[i]
}

mse_types = c('character', 'character', 'character', 'numeric', 'numeric', 'character')
for(i in 1:length(mse_types)){
  class(mse_refinement[,i]) = mse_types[i]
}
rm(i, mse_types, ref_types)

mse_refinement$treat = as.factor(mse_refinement$treat)
refinement$treat = as.factor(refinement$treat)

# 2. MSE plot ----

best_mse = mse_refinement[mse_refinement$status=='best', -6]

mse_plot = ggplot(mse_refinement, aes(x=stage, y=MSE, colour=treat)) + 
  geom_violin(fill='transparent') + geom_quasirandom(dodge.width=0.9, aes(colour=treat, shape=status, size=status)) +
  #geom_point(data=best_mse, colour='black', aes(x=stage, y=MSE, group=treat)) +
  scale_shape_manual(values=c(8, 16, 15), guide='none') + scale_size_manual(values=c(5, 2, 2), guide='none') +
  theme_classic()
mse_plot

# 3. Refinement plot ----

## Messing with df

### new column

relative_event = sapply(1:nrow(refinement), function(i){
  treat = refinement[i, 'treat']
  
  if(refinement[i, 'stage'] == 'multidimensional.nnls'){
    rel_event=refinement[i, 'event']
  }
  else{rel_event = switch(as.character(treat),
         '43' = refinement[i, 'event'],
         '51' = refinement[i, 'event'] -50,
         '58' = refinement[i, 'event'] - 100)
  }
  
  rel_event
})
refinement$relative_event = relative_event
rm(relative_event)

### Subset df

refinement_subset = refinement[refinement$intensity_used!=0, ]

baseline = refinement_subset[refinement_subset$stage=='multidimensional.nnls',]
baseline$relative_event = 25

leds_used = which(LightFitR::helio.dyna.leds$name %in% unique(refinement_subset$LED))

## Plotting!


  refinement_intensity_plot = ggplot(refinement_subset, aes(x=relative_event, y=intensity_used, colour=LED)) + facet_wrap(~treat) +
    geom_hline(data=baseline, linetype='longdash', linewidth=0.8, aes(yintercept=intensity_used, colour=LED)) +
    geom_point(aes(shape=status, size=status)) + 
    scale_colour_manual(values=led_colours[leds_used]) +
    scale_size_manual(values=c(4, 1, 1), guide='none') + scale_shape_manual(values=c(8, 16, 15), guide='none') +
    theme_classic() 
refinement_intensity_plot

refinement_target_plot = ggplot(refinement_subset, aes(x=relative_event, y=measured, colour=LED)) + facet_wrap(~treat) +
  geom_point(data=baseline, size=3, shape=24, colour='black', aes(x=relative_event, y=measured, fill=LED)) +
  geom_hline(data=baseline, aes(yintercept=target, colour=LED)) +
  geom_point(aes(shape=status, size=status)) + 
  scale_colour_manual(values=led_colours[leds_used]) + scale_fill_manual(values=led_colours[leds_used]) +
  scale_size_manual(values=c(4, 1, 1), guide='none') + scale_shape_manual(values=c(8, 16, 15), guide='none') +
  theme_classic() 
refinement_target_plot

refinement_residual_plot = ggplot(refinement_subset, aes(x=relative_event, y=diff, colour=LED)) +
  facet_wrap(~treat) + geom_hline(yintercept=0, colour='black') +
  geom_hline(data=baseline, linetype='longdash', linewidth=0.8, aes(yintercept=diff, colour=LED)) +
  geom_point(aes(shape=status, size=status)) + 
  scale_colour_manual(values=led_colours[leds_used]) +
  scale_size_manual(values=c(4, 1, 1), guide='none') + scale_shape_manual(values=c(8, 16, 15), guide='none') +
  theme_classic()
refinement_residual_plot