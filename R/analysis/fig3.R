# Setup -------

rm(list=ls())

## Libraries & functions
library(ggplot2)
library(ggbeeswarm)
library(ggspectra)

source("R/functions/ggplot_functions.R")

## Directories
wd = getwd()
fig3_out = "figures/fig3/"
sup_out = "figures/S1/"
data_in = "data/calibration/Apollo_Calib_20240827/"

## Import data
setwd(data_in)
load("Apollo_calibration_annotated_20240827.Rda")
calib = df
load("Apollo_calibration_rollingAverage_20240827.Rda")
rolling = df
load("Apollo_calibration_total_20240827.Rda")
total = df
setwd(wd)
rm(df)

## Format dfs

calib$LED = as.factor(calib$LED)
rolling$LED = as.factor(rolling$LED)
total$LED = as.factor(total$LED)

## Set variables
led_wls = LightFitR::helio.dyna.leds[-9, 'wavelength'] # Supposed wavelengths of the LED channels

# Calibration spectrum ------

criteria = calib$middle_time==T

spectrum_light = ggplot(data=calib[criteria,], aes(x=wavelength, y=watts, colour=LED)) +
  geom_point(size=0.8) + scale_color_manual(values=led_colours) +
  geom_vline(xintercept = led_wls, colour='darkgrey') +
  labs(x = wl_lab, y=irr_w_lab) +
  theme_classic()

fn = paste(sup_out, 'S1/S1a', sep='')
save_fig(fn, spectrum_light)

rm(criteria, spectrum_light, fn)

# Calibration heatmap ----

heatmap_light = ggplot(data=calib, aes(x=event, y=wavelength, fill=irradiance)) +
  geom_tile() + labs(x='timepoint', y=wl_lab) +
  theme_classic()

fn = paste(sup_out, 'S1b', sep='')
save_fig(fn, heatmap_light)

rm(heatmap_light, fn)

# Total irradiance line ----

criteria = total$middle_time == T

total_line_light = ggplot(data=total[criteria,], aes(x=intensity, y=watts, colour=LED)) +
  geom_point() + geom_smooth(se=F, linewidth=0.5) +
  scale_colour_manual(values = led_colours) + labs(y=expression('total irradiance (W m'^-2 * nm^-1*')')) +
  theme_classic()

fn = paste(sup_out, 'S1c', sep='')
save_fig(fn, total_line_light)

rm(criteria, total_line_light, fn)

# Irradiance at peak ----

criteria = (calib$peak==T) & (calib$LED != 5700) & (calib$middle_time==T)

peak_line_light = ggplot(data=calib[criteria,], aes(x=intensity, y=watts, colour=LED)) +
  geom_smooth(se=F, linewidth=0.6) + geom_point() + 
  scale_colour_manual(values = led_colours) + labs(y=irr_w_lab) +
  theme_classic()

fn = paste(fig3_out, '3A_line', sep='')
save_fig(fn, peak_line_light)

rm(criteria, peak_line_light, fn)

# Bleedthrough heatmap ----

## df setup

### Get wls of peaks
criteria = (calib$intensity==1000) & (calib$peak==TRUE) & (calib$LED != 5700) & (calib$middle_time==TRUE)
wls = calib[criteria, 'wavelength']

### Filter data using wls
criteria = (calib$intensity==1000) & (calib$wavelength %in% wls) & (calib$LED != 5700) & (calib$middle_time==TRUE)
bleedthrough = calib[criteria,]

bleedthrough[bleedthrough$peak==TRUE, 'irradiance'] = NA #Set the peaks to NA since those are difinitionally not bleedthrough

### structure data
bleedthrough$LED = as.factor(bleedthrough$LED)
bleedthrough$wavelength = as.factor(bleedthrough$wavelength)

rm(criteria, wls)

## Heatmap light
bleed_heatmap_light = ggplot(bleedthrough, aes(x=LED, y=wavelength, fill=irradiance)) +
  geom_tile() + labs(x='LED which is on', y="Irradiance of wavelengths at other channels") +
  scale_fill_gradient(low='white', high='#060038', na.value='#fa9900') + #060038 is a dark blue option
  theme_classic()

fn = paste(fig3_out, '3b_bleedthrough', sep='')
save_fig(fn, bleed_heatmap_light)

rm(bleed_heatmap_light, fn)

## Heatmap dark

bleed_heatmap_dark = ggplot(bleedthrough, aes(x=LED, y=wavelength, fill=irradiance)) +
  geom_tile() + labs(x='LED which is on', y="Irradiance of wavelengths at other channels") +
  scale_fill_gradient(low='#060038', high='white', na.value='#fa9900') + 
  theme_presentation()
bleed_heatmap_dark

fn = paste(fig3_out, '3b_bleedthrough_dark', sep='')
save_fig(fn, bleed_heatmap_dark)

rm(bleed_heatmap_dark, fn, bleedthrough)

# Peaks move ----

## Main panel
criteria = (calib$peak==TRUE) & (calib$LED != 5700) & (calib$intensity > 0) & complete.cases(calib)

peak_wls_light = ggplot(data=calib[criteria,], aes(x=intensity, y=wavelength, colour=LED)) +
  geom_point() +
  geom_hline(yintercept = led_wls, linetype='dashed') +
  scale_colour_manual(values=led_colours) + labs(y=wl_lab) + theme_classic()

fn = paste(fig3_out, '3c_main', sep='')
save_fig(fn, peak_wls_light)

rm(criteria, peak_wls_light, fn)

## Sub panels

### Processing
criteria = (calib$peak==TRUE) & (calib$LED %in% c(530, 620)) & (calib$intensity > 0) & complete.cases(calib)
calib_subset = calib[criteria,]

diff = calib_subset$wavelength - as.numeric(as.character(calib_subset$LED))
calib_subset$diff = diff

rm(diff, criteria)

### Plotting

diff_plot_light = ggplot(calib_subset, aes(x=LED, y=diff, colour=LED)) +
  geom_violin() + geom_quasirandom(aes(alpha=intensity)) +
  ylim(-15, 15) + geom_hline(yintercept=0) +
  scale_colour_manual(values=led_colours[5:6]) +
  labs(y="measured peak wavelength - purported peak wavelength (nm)") + 
  theme_classic()

fn = paste(fig3_out, '3c_sub', sep='')
save_fig(fn, diff_plot_light, c(10,15))

rm(diff_plot_light, fn)

### Dark plot

diff_plot_dark = ggplot(calib_subset, aes(x=LED, y=diff, colour=LED)) +
  geom_violin(fill='transparent') + geom_quasirandom(aes(alpha=intensity)) +
  ylim(-15, 15) + geom_hline(yintercept=0) +
  scale_colour_manual(values=led_colours[5:6]) +
  labs(y="measured peak wavelength - purported peak wavelength (nm)") + 
  theme_presentation()

fn = paste(fig3_out, '3c_sub_dark', sep='')
save_fig(fn, diff_plot_dark, c(24,36))

rm(calib_subset, diff_plot_dark, fn)
