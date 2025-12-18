# 0. Setup ====

rm(list=ls())

## Paths
wd = getwd()

data_path = './data/heliospectra_measurements/fig6/fig6_data_20251215.Rda'
out_path = './results/fig6/'

functions_path = './R/functions/'

## Functions

library(ggplot2)
library(dplyr)
library(lubridate)

source(paste0(functions_path, 'ggplot_functions.R'))

## Load data
load(data_path)

# 1. Plot ratios ====

fig6_ratios_plot = ggplot(data=fig6_RFR_ratio, aes(x=event, y=measured_RFR)) +
  #Add measured points
  geom_line() + geom_point() +
  
  #Make pretty
  scale_x_continuous(breaks=fig6_RFR_ratio$event,
    #Add second x axis to translate back to the original paper                 
    sec.axis=dup_axis(breaks=fig6_RFR_ratio$event,
                                       labels=fig6_RFR_ratio$solar_elevation_angle,
                                       name='solar elevation angle')) +
  labs(y='R:FR') +
  theme_manuscript()
fig6_ratios_plot

fn = paste0(out_path, 'fig6_ratios')
save_fig(fn, fig6_ratios_plot)

# 2. Plot measurements ====

