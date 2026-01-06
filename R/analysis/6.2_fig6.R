# 0. Setup ====

rm(list=ls())

## Paths
wd = getwd()

data_path = './data/heliospectra_measurements/fig6/fig6_data_20251215.Rda'
fig6_path = './results/fig6/'

functions_path = './R/functions/'

## Functions

library(ggplot2)
library(dplyr)
library(lubridate)

source(paste0(functions_path, 'ggplot_functions.R'))

## Load data
load(data_path)

# 1. Plot ratios ====

message('6.2.1 Plot ratios')

# Events on bottom
fig6_ratios_plot = ggplot(data=fig6_RFR_ratio, aes(x=event, y=measured_RFR)) +
  #Add measured points
  geom_line() + geom_point() +
  # Add target points
  geom_point(colour='red', shape=3, aes(y=target_R_FR)) +
  #Make pretty
  scale_x_continuous(breaks=fig6_RFR_ratio$event,
    #Add second x axis to translate back to the original paper                 
    sec.axis=dup_axis(breaks=fig6_RFR_ratio$event,
                                       labels=fig6_RFR_ratio$solar_elevation_angle,
                                       name='solar elevation angle')) +
  labs(y='R:FR') +
  theme_manuscript()
fig6_ratios_plot

fn = paste0(fig6_path, 'fig6_ratios')
save_fig(fn, fig6_ratios_plot)

# Solar elevation on bottom
fig6_ratios_plotb = ggplot(data=fig6_RFR_ratio, aes(x=solar_elevation_angle, y=measured_RFR)) +
  #Add measured points
  geom_line() + geom_point() +
  # Add target points
  geom_point(colour='red', shape=3, aes(y=target_R_FR)) +
  #Make pretty
  scale_x_reverse(
                     #Add second x axis to translate back to the original paper                 
                     sec.axis=dup_axis(breaks=fig6_RFR_ratio$solar_elevation_angle,
                                       labels=fig6_RFR_ratio$event,
                                       name='event')) +
  labs(y='R:FR', x='solar elevation angle') +
  theme_manuscript()

fn = paste0(fig6_path, 'fig6_ratios_b')
save_fig(fn, fig6_ratios_plotb)