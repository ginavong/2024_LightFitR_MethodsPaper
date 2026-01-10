# 0. Setup ====

rm(list=ls())

## Paths
wd = getwd()

data_path = './data/heliospectra_measurements/fig5b/fig5b_data_20260105.Rda'
fig5_path = './results/fig5/'

functions_path = './R/functions/'

## Functions

library(ggplot2)
library(dplyr)
library(lubridate)

source(paste0(functions_path, 'ggplot_functions.R'))

## Load data
load(data_path)

# 1. Plot ratios ====

message('5.8.1 Plot ratios')

# Events on bottom
fig5b_ratios_plot = ggplot(data=fig5b_RFR_ratio, aes(x=event, y=measured_RFR)) +
  #Add measured points
  geom_line() + geom_point() +
  # Add target points
  geom_point(colour='red', shape=3, aes(y=target_R_FR)) +
  #Make pretty
  scale_x_continuous(breaks=fig5b_RFR_ratio$event,
    #Add second x axis to translate back to the original paper                 
    sec.axis=dup_axis(breaks=fig5b_RFR_ratio$event,
                                       labels=fig5b_RFR_ratio$solar_elevation_angle,
                                       name='solar elevation angle')) +
  labs(y='R:FR') +
  theme_manuscript()
fig5b_ratios_plot

fn = paste0(fig5_path, 'fig5b_ratios')
save_fig(fn, fig5b_ratios_plot)

# Solar elevation on bottom
fig5b_ratios_plotb = ggplot(data=fig5b_RFR_ratio, aes(x=solar_elevation_angle, y=measured_RFR)) +
  #Add measured points
  geom_line() + geom_point() +
  # Add target points
  geom_point(colour='red', shape=3, aes(y=target_R_FR)) +
  #Make pretty
  scale_x_reverse(
                     #Add second x axis to translate back to the original paper                 
                     sec.axis=dup_axis(breaks=fig5b_RFR_ratio$solar_elevation_angle,
                                       labels=fig5b_RFR_ratio$event,
                                       name='event')) +
  labs(y='R:FR', x='solar elevation angle') +
  theme_manuscript()

fn = paste0(fig5_path, 'fig5b_ratios_alternative')
save_fig(fn, fig5b_ratios_plotb)