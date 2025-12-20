# 0. Setup ----

rm(list=ls())

## Set file directories

wd = getwd()

fun_dir = 'R/functions/'

data_dir = 'data/algorithm_testing/fig5_refinement/'
main_dir = 'results/fig5/'
sup_dir = 'results/S5/'

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

## ggplot defaults
OkabeIto = palette.colors(palette = "Okabe-Ito")[2:4]
shapes = scale_shape_manual(values=c(17, 8, 16))
col_lab = 'algorithm MSE'

# 1. Format df ----

message("5.5.1 Formatting")

str(refinement)
str(mse_refinement)

mse_refinement$treat = as.factor(mse_refinement$treat)
refinement$treat = as.factor(refinement$treat)

mse_refinement$start_MSE = factor(mse_refinement$start_MSE, levels=c('low', 'mid', 'high'))
refinement$start_MSE = factor(refinement$start_MSE, levels=c('low', 'mid', 'high'))

# 2. MSE plot ----

message("fig5")

mse_plot = ggplot(mse_refinement, aes(x=start_MSE, y=MSE, colour=start_MSE)) + 
  geom_violin(fill='transparent') + geom_quasirandom(aes(colour=start_MSE, shape=status, size=status)) +
  scale_color_manual(values=OkabeIto, guide='none') +
  shapes + scale_size_manual(values=c(4, 5, 2), guide='none') +
  labs(x='mean squared error of algorithm intensities', y='mean squared error', colour=col_lab) +
  theme_manuscript()
mse_plot

fn = paste(main_dir, 'fig5', sep='')
save_fig(fn, mse_plot)

# 3. Refinement plot ----

message("figS5a")

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
  scale_colour_manual(values=led_colours[leds_used], guide='none') + scale_fill_manual(values=led_colours[leds_used], guide='none') +
  scale_size_manual(values=c(1, 4, 1), guide='none') + shapes + guides(shape='none') +
  labs(x='event', y=irr_umol_peak_lab) +
  theme_manuscript() 
refinement_target_plot

## Export

fn = paste(sup_dir, 'S5a', sep='')
save_fig(fn, refinement_target_plot)

# 4. Euclidian distance plot ----

message("figS5b")

## Plot

euclidian_plot = ggplot(mse_refinement, aes(x=euc_dist, y=MSE, colour=start_MSE)) + 
  geom_smooth(se=F, na.rm=T, method='lm', linewidth=0.6, aes(group=start_MSE)) +
  geom_point(aes(shape=status, size=status)) +
  scale_color_manual(values=OkabeIto, guide='none') + 
  scale_size_manual(values=c(3, 3, 1), guide='none') + shapes + guides(shape='none') +
  labs(x='euclidian distance to best intensities', y='mean squared error', colour=col_lab) +
  theme_manuscript()
euclidian_plot

fn = paste(sup_dir, 'S5b', sep='')
save_fig(fn, euclidian_plot)

## Stats

criteria = complete.cases(mse_refinement)
mse_subset = mse_refinement[criteria,]
treats = as.character(unique(mse_refinement$start_MSE))

test_results = t(sapply(treats, function(i){
  
  print(i)
  
  test_subset = mse_subset[mse_subset$start_MSE==i, ]
  test = cor.test(test_subset$euc_dist, test_subset$MSE, method='spearman')
  
  print(test)
  print(test$p.value)
  
  c(i, test$estimate, test$statistic, test$p.value)
}))
colnames(test_results) = c('start_MSE', 'rho', 'S', 'p.value')

fn = paste(sup_dir, 'S5b_SpearmanRank.csv', sep='')
write.csv(test_results, file=fn)

rm(fn, criteria, mse_subset, treats)