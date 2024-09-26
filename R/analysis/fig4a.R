# Setup ----

## Directories
wd = getwd()
fun_dir = 'R/functions/'
fig4_dir = 'figures/fig4/'
S2_dir = 'figures/S2/'


## Libraries & functions
library(ggplot2)
library(ggbeeswarm)

setwd(fun_dir)
source('ggplot_functions.R')
setwd(wd)

## Import data
load('data/light_testing/4a_20240905/4a_algorithmsTest.Rda')

## Format data

# 4a MSE after all the steps of the algorithm ----

criteria = (mse$calibration_processing=='none') & (mse$stage=='tidied') & complete.cases(mse)
mse_subset = mse[criteria,]

fig4a = ggplot(data=mse_subset, aes(x=as.factor(complexity), y=MSE, colour=interaction(algorithm_type, algorithm))) +
  geom_violin(fill='transparent') + geom_quasirandom(dodge.width=1) +
  labs(x='number of LED channels active', y='mean squared error') +
  guides(colour=guide_legend(title='algorithm')) +
  theme_classic()
fig4a

rm(mse_subset, criteria)

# Test design
criteria = (algo_test_results$algorithm=='lm')
lm_subset = algo_test_results[criteria,]
rm(criteria)

ggplot(data=lm_subset, aes(x=as.factor(event), y=LED, fill=true_intensity)) +
  geom_tile() + scale_fill_gradient(low='black', high='white') +
  theme_classic()

# Pridected
ggplot(data=lm_subset, aes(x=as.factor(event), y=LED, fill=predicted_intensity)) +
  geom_tile() + scale_fill_gradient(low='black', high='white') +
  theme_classic() + labs(title='lm')

criteria = (algo_test_results$algorithm=='nnls') & (algo_test_results$calibration_processing=='none')
nnls_subset = algo_test_results[criteria,]
ggplot(nnls_subset, aes(x=as.factor(event), y=LED, fill=predicted_intensity)) +
  geom_tile() + scale_fill_gradient(low='black', high='white') +
  theme_classic() + labs(title='nnls')

criteria = (algo_test_results$algorithm=='nnls') & (algo_test_results$calibration_processing=='rolling')
nnls_subset = algo_test_results[criteria,]
ggplot(nnls_subset, aes(x=as.factor(event), y=LED, fill=predicted_intensity)) +
  geom_tile() + scale_fill_gradient(low='black', high='white') +
  theme_classic() + labs(title='nnls rolling')

rm(criteria, lm_subset, nnls_subset)
# MSE at individual steps of the algorithm

ggplot(data=mse, aes(x=as.factor(complexity), y=MSE, colour=stage)) +
  geom_violin(fill='transparent') + geom_quasirandom(dodge.width=1) +
  facet_wrap(facets=interaction(mse$calibration_processing, mse$algorithm)) +
  labs(x='number of LED channels active', y='mean squared error') +
  guides(colour=guide_legend(title='stage')) +
  theme_classic()

# Error at different intensities
criteria = algo_test_results$calibration_processing != 'rolling'
algo_subset = algo_test_results[criteria,]
rm(criteria)

ggplot(data=algo_subset, aes(x=target_irradiance, y=diff, colour=LED, shape=algorithm)) +
  geom_point() + facet_wrap(~algorithm) +
  scale_colour_manual(values=led_colours) +
  geom_hline(yintercept=0) +
  theme_classic()

# Error by complexity at specific LEDS

ggplot(data=algo_subset, aes(x=complexity, y=LED, fill=diff)) +
  geom_tile() + facet_wrap(~algorithm) +
  scale_fill_gradient2(low='blue', mid='white', high='red') +
  theme_classic()

# Error by LED
ggplot(data=algo_subset, aes(x=as.factor(LED), y=diff, colour=LED)) +
  geom_violin(colour='black') + geom_quasirandom(size=0.8, dodge.width=1) +
  facet_wrap(~algorithm) +
  scale_colour_manual(values=led_colours) +
  theme_classic()

