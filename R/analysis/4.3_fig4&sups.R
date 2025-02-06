# Setup ----
rm(list=ls())

## Directories
wd = getwd()
fun_dir = 'R/functions/'
fig4_dir = 'figures/fig4/'
S2_dir = 'figures/S2/'
S3_dir = 'figures/S3/'


## Libraries & functions
library(ggplot2)
library(ggbeeswarm)

library(stringr)
library(dplyr)

library(FSA)

setwd(fun_dir)
source('ggplot_functions.R')
setwd(wd)

## Import data
load('data/algorithm_testing/fig4_algorithm_comparisons/4_algorithmsTest.Rda')

## Format data

## Plot settings 

algo_colours = c('#FF49AE', '#B55000', '#1E7DB3', '#8D1AFB', '#00543D')

pd = 0.9 #position_dodge
ps = 1 #point_size
twoPanel_ps = 0.8 #point size for 2 panel plots
multi_ps = 0.5

resid_lab = '(predicted intensity) - (true intensity)'
target_irr_lab = expression('target irradiance (W m'^-2 * nm^-1*')')

# 4a MSE after all the steps of the algorithm ----

## Data subset

criteria = (mse_event$calibration_processing=='none') & (mse_event$stage=='tidied') & complete.cases(mse_event) &  (mse_event$algorithm!='closest')
mse_subset = mse_event[criteria,]

mse_subset$algorithm_comb = as.factor(paste(mse_subset$algorithm_type, mse_subset$algorithm, sep='.'))
mse_subset$complexity = as.factor(mse_subset$complexity)
mse_subset$algorithm_type = as.factor(mse_subset$algorithm_type)
mse_subset$algorithm = as.factor(mse_subset$algorithm)

## Initial plot with tidied

fig4a = ggplot(data=mse_subset, aes(x=as.factor(complexity), y=MSE, colour=interaction(algorithm_type, algorithm))) +
  geom_violin(fill='transparent') + geom_quasirandom(dodge.width=pd, size=ps) +
  stat_summary(geom='point', fun.y='mean', shape=17, size=2, col='black', position=position_dodge(width=pd), aes(group = interaction(algorithm_type, algorithm))) +
  scale_colour_manual(values=algo_colours[-1]) +
  labs(x='number of LED channels active', y='mean squared error') +
  guides(colour=guide_legend(title='algorithm')) +
  theme_classic()
fig4a

## Stats - Kruskal test with Dunn correction

complexity = unique(mse_subset$complexity)

stats_test = lapply(complexity, function(i){
  criteria = mse_subset$complexity==i
  complexity_subset = mse_subset[criteria,]
  
  #Stats test
  mod = FSA::dunnTest(MSE~algorithm_comb, data=complexity_subset,
                method='holm', two.sided = F)
  
  # Format df
  complex = rep(i, nrow(mod$res))
  
  cbind(complex, mod$res)
  
})

stats_test = do.call(rbind, stats_test)
colnames(stats_test)[1] = 'complexity'

stats_test$signif = symnum(stats_test$P.adj,
                           corr = FALSE, na = FALSE, 
                           cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                           symbols = c("***", "**", "*", ".", " "))


## Save

fn = paste(fig4_dir, '4a_algorithmMSE', sep='')
save_fig(fn, fig4a)

fn = paste(fig4_dir, '4a_statsTests.csv', sep='')
write.csv(stats_test, file=fn)

rm(criteria, mse_subset, fn)

# S2  ----

## S2a Tidied with closest

criteria = (mse_event$calibration_processing=='none') & (mse_event$stage=='tidied') & complete.cases(mse_event)
mse_subset = mse_event[criteria,]

mse_subset$algorithm_comb = as.factor(paste(mse_subset$algorithm_type, mse_subset$algorithm, sep='.'))
mse_subset$complexity = as.factor(mse_subset$complexity)
mse_subset$algorithm_type = as.factor(mse_subset$algorithm_type)
mse_subset$algorithm = as.factor(mse_subset$algorithm)

S2a = ggplot(data=mse_subset, aes(x=as.factor(complexity), y=MSE, colour=interaction(algorithm_type, algorithm))) +
  geom_violin(fill='transparent') + geom_quasirandom(dodge.width=pd, size=ps) +
  stat_summary(geom='point', fun.y='mean', shape=17, size=2, col='black', position=position_dodge(width=pd), aes(group = interaction(algorithm_type, algorithm))) +
  scale_colour_manual(values=algo_colours) +
  labs(x='number of LED channels active', y='mean squared error') +
  guides(colour=guide_legend(title='algorithm')) +
  theme_classic()
S2a

fn = paste(S2_dir, 'S2a_inclClosest', sep='')
save_fig(fn, S2a)

rm(criteria, mse_subset, fn)


## Predicted

predicted = ggplot(data=mse_event, aes(x=as.factor(complexity), y=MSE, colour=interaction(algorithm_type, algorithm))) +
  geom_violin(fill='transparent') + geom_quasirandom(dodge.width=pd, size=multi_ps) +
  facet_wrap(~stage) +
  scale_colour_manual(values=algo_colours) +
  labs(x='number of LED channels active', y='mean squared error') +
  guides(colour=guide_legend(title='algorithm')) +
  theme_classic()
predicted

fn = paste(S2_dir, 'S2c_stage', sep='')
save_fig(fn, predicted)


# ## Calibration Processing
# 
# processing = ggplot(data=mse_event, aes(x=as.factor(complexity), y=MSE, colour=calibration_processing)) +
#   geom_violin(fill='transparent') + geom_quasirandom(dodge.width=1, size=multi_ps) +
#   facet_wrap(~interaction(algorithm_type, algorithm)) +
#   labs(x='number of LED channels active', y='mean squared error') +
#   guides(colour=guide_legend(title='calibration processing')) +
#   theme_classic()
# processing
# 
# fn = paste(S2_dir, 'S2b_calibProcessing', sep='')
# save_fig(fn, processing)


# 4b Error by LED ----

criteria = (algo_test_results$calibration_processing != 'rolling') & (algo_test_results$stage=='predicted') & ((algo_test_results$algorithm=='lm') | (algo_test_results$algorithm_type=='multidimensional' & algo_test_results$algorithm=='nnls'))
algo_subset = algo_test_results[criteria,]

fig4b = ggplot(data=algo_subset, aes(x=as.factor(LED), y=diff, colour=LED)) +
  geom_violin(colour='black') + geom_quasirandom(size=twoPanel_ps, dodge.width=1) +
  stat_summary(geom='point', fun.y='mean', shape=17, size=2, col='black') +
  facet_wrap(~interaction(algorithm_type, algorithm)) +
  scale_colour_manual(values=led_colours) +
  labs(x='LED channel', y=resid_lab) +
  theme_classic()
fig4b

fn = paste(fig4_dir, '4b_LEDs', sep='')
save_fig(fn, fig4b)

rm(criteria, algo_subset, fn)


# S3 ----

## S3a Distribution of all LEDS

criteria = (algo_test_results$calibration_processing != 'rolling') & (algo_test_results$stage=='predicted')
algo_subset = algo_test_results[criteria,]

led = ggplot(data=algo_subset, aes(x=as.factor(LED), y=diff, colour=LED)) +
  geom_violin(colour='black') + geom_quasirandom(size=multi_ps, dodge.width=1) +
  stat_summary(geom='point', fun.y='mean', shape=17, size=2, col='black') +
  facet_wrap(~interaction(algorithm_type, algorithm)) +
  scale_colour_manual(values=led_colours) +
  labs(x='LED channel', y=resid_lab) +
  theme_classic()
led

fn = paste(S3_dir, 'S3a_LEDs', sep='')
save_fig(fn, led)

rm(criteria, algo_subset, fn)

## S3b Residuals at different irradiances

criteria = (algo_test_results$stage=='tidied')
algo_subset = algo_test_results[criteria,]

irradiances = ggplot(data=algo_subset, aes(x=target_irradiance, y=diff, colour=LED)) +
  geom_point(size=multi_ps) + facet_wrap(~interaction(algorithm_type, algorithm)) +
  geom_hline(yintercept=0) +
  scale_colour_manual(values=led_colours) +
  labs(x=target_irr_lab, y=resid_lab) +
  theme_classic()
irradiances

fn = paste(S3_dir, 'S3b_residuals', sep='')
save_fig(fn, irradiances)

rm(criteria, algo_subset, fn)


# CombinatioNs of LEDs [Work in progress] ----

## Heatmaps

criteria = mse_combinations$stage=='tidied' & mse_combinations$calibration_processing=='none'
combs_subset = mse_combinations[criteria,]
#combs_subset[combs_subset$same==T, 'MSE'] = NA

ggplot(data=combs_subset, aes(x=LED1, y=LED2, fill=MSE)) +
  geom_tile() +
  facet_wrap(~interaction(algorithm_type, algorithm)) +
  scale_fill_gradient(low='white', high='#060038', na.value='#fa9900') +
  theme_classic()


## Correlation with bleedthrough

criteria = complete.cases(mse_combinations) & mse_combinations$stage=='tidied' & mse_combinations$calibration_processing=='none'
combs_subset = mse_combinations[criteria,]

ggplot(data=combs_subset, aes(x=bleedthrough_irradiance, y=MSE, colour=interaction(algorithm_type, algorithm), shape=algorithm_type)) + 
  geom_point() +
  geom_smooth(se=F)

#It's clear here that I haven't define mse_combination very well. Most of the high MSE is just driven bY the problematic LEDs. Need to define as "when LEDx is on, the error of LEDy is this".