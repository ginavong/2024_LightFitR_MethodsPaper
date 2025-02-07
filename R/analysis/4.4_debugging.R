# 0. Setup ----

rm(list=ls())

## Packages
library(ggplot2)

## Data

load('data/algorithm_testing/fig4_algorithm_comparisons/4_algorithmsTest.Rda')
load('data/heliospectra_measurements/fig4_20240905/4_annotated_20240905.Rda')
measurements = df
rm(df)

## Plot settings

algo_colours = c('#FF49AE', '#B55000', '#1E7DB3', '#8D1AFB', '#00543D')

# 1. Find event of bug ----

## Plot

predicted_plot = ggplot(data=mse_event, aes(x=as.factor(complexity), y=MSE, colour=interaction(algorithm_type, algorithm))) +
  geom_violin(fill='transparent') + geom_quasirandom(dodge.width=0.9, size=0.5) +
  facet_wrap(~stage) +
  scale_colour_manual(values=algo_colours) +
  labs(x='number of LED channels active', y='mean squared error') +
  guides(colour=guide_legend(title='algorithm')) +
  theme_classic()
predicted_plot

## Find events for outliers

### Events at complexity 3

criteria = mse_event$stage=='predicted' & mse_event$algorithm_type=='multidimensional' & mse_event$algorithm=='sle' & (mse_event$complexity==3)
mse_subset = mse_event[criteria, ]

event1 = mse_subset[which.max(mse_subset$MSE), 'event']
event1
mse_subset[mse_subset$event==event1,] #Sanity check that we got the right thing. Interesting the difference between no calibration processing and rolling average

mses = sort(unique(mse_subset$MSE))
criteria = mse_subset$MSE == mses[length(mses)-1]
event2 = mse_subset[criteria, 'event']
event2
mse_subset[mse_subset$event==event2,] #That one is also an issue with the rolling average

### Event at complexity 2

criteria = mse_event$stage=='predicted' & mse_event$algorithm_type=='multidimensional' & mse_event$algorithm=='sle' & (mse_event$complexity==2)
mse_subset = mse_event[criteria,]

event3 = mse_subset[which.max(mse_subset$MSE), 'event']
event3
mse_subset[mse_subset$event==event3,]

# 2. Plot without rolling averages

criteria = mse_event$calibration_processing=='none'
mse_subset = mse_event[criteria,]

predicted_plot2 = ggplot(data=mse_subset, aes(x=as.factor(complexity), y=MSE, colour=interaction(algorithm_type, algorithm))) +
  geom_violin(fill='transparent') + geom_quasirandom(dodge.width=0.9, size=0.5) +
  facet_wrap(~stage) +
  scale_colour_manual(values=algo_colours) +
  labs(x='number of LED channels active', y='mean squared error') +
  guides(colour=guide_legend(title='algorithm')) +
  theme_classic()
predicted_plot2

# Conclusion: There is an issue with rolling averages. Probably a bug in how I've calculated rolling averages, but since we're removing the rolling averages part of the manuscript anyway, this can be excluded from the figure.
