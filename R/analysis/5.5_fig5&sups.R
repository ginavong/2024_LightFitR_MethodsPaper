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
load('5_refinementCollated.Rda')
setwd(wd)

# 1. Format df ----

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

mse_plot = ggplot(mse_refinement, aes(x=start_MSE, y=MSE, colour=start_MSE)) + 
  geom_violin(fill='transparent') + geom_quasirandom(aes(colour=start_MSE, shape=status, size=status)) +
  scale_shape_manual(values=c(8, 16, 17)) + scale_size_manual(values=c(5, 2, 4), guide='none') +
  theme_classic()
mse_plot

# 3. Refinement plot ----

### Subset df

refinement_subset = refinement[refinement$intensity_used!=0, ]

baseline = refinement_subset[refinement_subset$stage=='multidimensional.nnls',]

leds_used = which(LightFitR::helio.dyna.leds$name %in% unique(refinement_subset$LED))

## Plotting!

refinement_target_plot = ggplot(refinement_subset, aes(x=relative_event, y=measured, colour=LED)) + facet_wrap(~start_MSE) +
  geom_point(data=baseline, size=3, shape=24, colour='black', aes(x=relative_event, y=measured, fill=LED)) +
  geom_hline(data=baseline, aes(yintercept=target, colour=LED)) +
  geom_point(aes(shape=status, size=status)) + 
  scale_colour_manual(values=led_colours[leds_used]) + scale_fill_manual(values=led_colours[leds_used]) +
  scale_size_manual(values=c(4, 1, 1), guide='none') + scale_shape_manual(values=c(8, 16, 17)) +
  theme_classic() 
refinement_target_plot

# 4. Euclidian distance plot ----

euclidian_plot = ggplot(mse_refinement, aes(x=euc_dist, y=MSE, colour=start_MSE)) + 
  geom_smooth(se=F, na.rm=T, method='lm', linewidth=0.6, aes(group=start_MSE)) +
  geom_point(aes(shape=status)) + 
  scale_size_manual(values=c(4, 1, 1), guide='none') + scale_shape_manual(values=c(8, 16, 17)) +
  theme_classic()
euclidian_plot
