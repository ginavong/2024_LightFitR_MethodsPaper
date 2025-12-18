# 0. Setup ====

rm(list=ls())

## Paths

wd = getwd()

out_path = './results/'
data_path = './data/heliospectra_measurements/fig4_20240905/4_annotated_20240905.Rda'

functions_path = './R/functions/'

## Functions

library(ggplot2)
library(ggspectra)
library(dplyr)

source(paste0(functions_path, 'ggplot_functions.R'))

## Data

load(data_path)
measurements = df
rm(df)

# 1. Filter df for last event ====

last_event = max(measurements$event)

last_measurements = measurements[measurements$event==last_event,]

peaks = data.frame(wavelength=unique(measurements[measurements$peak==T, 'wavelength']), LED=LightFitR::helio.dyna.leds$name[-9])

# 2. Spectrum of middle time ====

title = paste('event:', last_event, ', time:', 
              unique(last_measurements[last_measurements$middle_time==TRUE, 'time']))

plot_spectrum = last_measurements |> filter(middle_time==TRUE) |>
  ggplot(aes(x=wavelength, y=umol)) + geom_point(size=1) +
  #Spectrum bar
  stat_wl_strip(ymax=-0.01, ymin=-0.1) + scale_fill_identity() +
  #Add channel peaks
  geom_vline(aes(xintercept=wavelength, colour=LED), peaks, linewidth=1) + 
  scale_colour_manual(values=led_colours) +
  # Make pretty
  labs(title=title, y=irr_umol_lab) +
  theme_manuscript(LED.guide=FALSE)
plot_spectrum

fn = paste0(out_path, 'event150_spectrum')
save_fig(fn, plot_spectrum)

rm(title)

# 3. Peak irradiances over time

plot_peaks = last_measurements |> filter(peak==TRUE) |> 
  mutate(time=lubridate::hms(time), wavelength=as.factor(round(wavelength))) |>
  ggplot(aes(x=time, y=umol, colour=wavelength)) + scale_x_time() +
    geom_line() + geom_point() + 
  scale_colour_manual(values=led_colours) +
    labs(y=irr_umol_peak_lab, title=paste('event:', last_event), colour='channel peak') + theme_manuscript()
plot_peaks

fn = paste0(out_path, 'event150_peaks_time')
save_fig(fn, plot_peaks)