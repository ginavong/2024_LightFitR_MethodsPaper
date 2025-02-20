# 0. Setup ----

rm(list=ls())

## Set file directories

wd = getwd()

fun_dir = 'R/functions/'

data_dir = 'data/algorithm_testing/fig5_refinement/'
main_dir = 'figures/fig5/'
sup_dir = 'figures/S4/'

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

message("5.5.1 Formatting")

str(refinement)
str(mse_refinement)

mse_refinement$treat = as.factor(mse_refinement$treat)
refinement$treat = as.factor(refinement$treat)

# 2. MSE plot ----

message("fig5")

mse_plot = ggplot(mse_refinement, aes(x=start_MSE, y=MSE, colour=start_MSE)) + 
  geom_violin(fill='transparent') + geom_quasirandom(aes(colour=start_MSE, shape=status, size=status)) +
  scale_shape_manual(values=c(8, 16, 17)) + scale_size_manual(values=c(5, 2, 4), guide='none') +
  theme_classic()
mse_plot

fn = paste(main_dir, 'fig5.png', sep='')
ggsave(fn, mse_plot)

# 3. Refinement plot ----

message("figS4a")

### Subset df

refinement_subset = refinement[refinement$intensity_used!=0, ]
refinement_subset$LED = as.factor(refinement_subset$LED)

baseline = refinement_subset[refinement_subset$stage=='multidimensional.nnls',]
baseline$LED = as.factor(baseline$LED)

leds_used = which(LightFitR::helio.dyna.leds$name %in% unique(refinement_subset$LED))

## Plotting!

refinement_target_plot = ggplot(refinement_subset, aes(x=relative_event, y=umol, colour=LED)) + facet_wrap(~start_MSE) +
  geom_point(data=baseline, size=3, shape=24, colour='black', aes(x=relative_event, y=umol, fill=LED)) +
  geom_hline(data=baseline, aes(yintercept=target, colour=LED)) +
  geom_point(aes(shape=status, size=status)) + 
  scale_colour_manual(values=led_colours[leds_used]) + scale_fill_manual(values=led_colours[leds_used]) +
  scale_size_manual(values=c(4, 1, 1), guide='none') + scale_shape_manual(values=c(8, 16, 17)) +
  labs(x='event', y=irr_umol_lab) +
  theme_classic() 
refinement_target_plot

## Export

fn = paste(sup_dir, 'S4a.png', sep='')
ggsave(fn, refinement_target_plot)

rm(refinement_subset, baseline, leds_used)

# 4. Euclidian distance plot ----

message("figS4b")

euclidian_plot = ggplot(mse_refinement, aes(x=euc_dist, y=MSE, colour=start_MSE)) + 
  geom_smooth(se=F, na.rm=T, method='lm', linewidth=0.6, aes(group=start_MSE)) +
  geom_point(aes(shape=status)) + 
  scale_size_manual(values=c(4, 1, 1), guide='none') + scale_shape_manual(values=c(8, 16, 17)) +
  theme_classic()
euclidian_plot

fn = paste(sup_dir, 'S4b.png', sep='')
ggsave(fn, euclidian_plot)

# 5. Residuals per LED

criteria = refinement$on==TRUE
refinement_subset = refinement[criteria,]

resid_plot = ggplot(refinement_subset, aes(x=LED, y=diff)) +
  geom_violin(fill='transparent') + geom_quasirandom(aes(, colour=LED, shape=status, size=status)) +
  facet_wrap(~start_MSE) +
  scale_colour_manual(values=led_colours[-1]) + 
  scale_shape_manual(values=c(8, 16, 17)) + scale_size_manual(values=c(5, 1, 4), guide='none') +
  geom_hline(yintercept=0) +
  theme_classic()
resid_plot
