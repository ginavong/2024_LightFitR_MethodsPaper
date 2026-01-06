# 0. Setup ====

rm(list=ls())

## Paths
wd = getwd()

data_path = './data/heliospectra_measurements/fig6/fig6_data_20251215.Rda'
fig6_path = './results/fig6/'
S5_path = './results/S6/'

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

# 2. Plot measurements ====

message('6.2.2 Plot measurements')

time_dict = fig6_RFR_ratio |> select(time_approx, event, solar_elevation_angle) |> distinct() |>
  mutate(time=hm(time_approx))

plot_measurements = measurements_summary |> filter(middle_time==TRUE) |>
  ggplot(aes(x=solar_elevation_angle, y=umol, colour=colour, shape=type, linetype=type)) +
  geom_line() + geom_point()+
  # Make x axis go large -> small and add second event axis on top
  scale_x_reverse(sec.axis=dup_axis(breaks=time_dict$solar_elevation_angle,
                                 labels=time_dict$event,
                                 name='event')) +
  # Make pretty
  scale_colour_manual(values=led_colours[c(8,7)]) + 
  scale_shape_manual(values=c(16, 3)) +
  labs(x='solar elevation angle', y=irr_umol_peak_lab) +
  theme_manuscript()
plot_measurements

fn = paste0(S5_path, 'S5_measurements')
save_fig(fn, plot_measurements)
