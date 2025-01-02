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

## Tidied

criteria = (mse_event$calibration_processing=='none') & (mse_event$stage=='tidied') & complete.cases(mse_event)
mse_subset = mse_event[criteria,]

fig4a = ggplot(data=mse_subset, aes(x=as.factor(complexity), y=MSE, colour=interaction(algorithm_type, algorithm))) +
  geom_violin(fill='transparent') + geom_quasirandom(dodge.width=0.9) +
  labs(x='number of LED channels active', y='mean squared error') +
  guides(colour=guide_legend(title='algorithm')) +
  theme_classic()
fig4a

rm(criteria, mse_subset)

## Tidied without closest

criteria = (mse_event$calibration_processing=='none') & (mse_event$stage=='tidied') & complete.cases(mse_event) & (mse_event$algorithm!='closest')
mse_subset = mse_event[criteria,]

ggplot(data=mse_subset, aes(x=as.factor(complexity), y=MSE, colour=interaction(algorithm_type, algorithm))) +
  geom_violin(fill='transparent') + geom_quasirandom(dodge.width=0.9) +
  labs(x='number of LED channels active', y='mean squared error') +
  guides(colour=guide_legend(title='algorithm')) +
  theme_classic()

rm(criteria, mse_subset)

## Predicted

predicted = ggplot(data=mse_event, aes(x=as.factor(complexity), y=MSE, colour=interaction(algorithm_type, algorithm))) +
  geom_violin(fill='transparent') + geom_quasirandom(dodge.width=0.9) +
  facet_wrap(~stage) +
  labs(x='number of LED channels active', y='mean squared error') +
  guides(colour=guide_legend(title='algorithm')) +
  theme_classic()
predicted


## Processing

processing = ggplot(data=mse_event, aes(x=as.factor(complexity), y=MSE, colour=calibration_processing)) +
  geom_violin(fill='transparent') + geom_quasirandom(dodge.width=1) +
  facet_wrap(~interaction(algorithm_type, algorithm)) +
  labs(x='number of LED channels active', y='mean squared error') +
  guides(colour=guide_legend(title='calibration processing')) +
  theme_classic()
processing

## Residuals at different irradiances

criteria = (algo_test_results$stage=='tidied')
algo_subset = algo_test_results[criteria,]

irradiances = ggplot(data=algo_subset, aes(x=target_irradiance, y=diff, colour=LED)) +
  geom_point() + facet_wrap(~interaction(algorithm_type, algorithm)) +
  scale_colour_manual(values=led_colours) +
  geom_hline(yintercept=0) +
  theme_classic()
irradiances

rm(criteria, algo_subset)

# 4b Error by LED ----

criteria = (algo_test_results$calibration_processing != 'rolling') & (algo_test_results$stage=='predicted') & ((algo_test_results$algorithm=='lm') | (algo_test_results$algorithm_type=='multidimensional' & algo_test_results$algorithm=='nnls'))
algo_subset = algo_test_results[criteria,]

fig4b = ggplot(data=algo_subset, aes(x=as.factor(LED), y=diff, colour=LED)) +
  geom_violin(colour='black') + geom_quasirandom(size=0.8, dodge.width=1) +
  facet_wrap(~interaction(algorithm_type, algorithm)) +
  scale_colour_manual(values=led_colours) +
  theme_classic()
fig4b

rm(criteria, algo_subset)

## Distribution of all LEDS

criteria = (algo_test_results$calibration_processing != 'rolling') & (algo_test_results$stage=='predicted')
algo_subset = algo_test_results[criteria,]

led = ggplot(data=algo_subset, aes(x=as.factor(LED), y=diff, colour=LED)) +
  geom_violin(colour='black') + geom_quasirandom(size=0.8, dodge.width=1) +
  facet_wrap(~interaction(algorithm_type, algorithm)) +
  scale_colour_manual(values=led_colours) +
  theme_classic()
led

rm(criteria, algo_subset)

## MSE by LED

criteria = (mse_led$calibration_processing=='none') & (mse_led$stage=='tidied') & complete.cases(mse_led)
mse_subset = mse_led[criteria,]

ggplot(data=mse_subset, aes(x=LED, y=MSE, colour=LED, shape=interaction(algorithm_type, algorithm))) +
  geom_quasirandom() +
  labs(y='mean squared error') + scale_colour_manual(values=led_colours) +
  theme_classic()

rm(criteria, mse_subset)


