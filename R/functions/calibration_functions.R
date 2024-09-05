# Add LED and intensity columns

calib_info = function(calib_regime, spectrophotometer_df, LED_wavelengths_vec){
  
  # Dictionary of which events correspond with which settings
  settings_dict = t(sapply(1:ncol(calib_regime), function(event){
    
    intensities = as.numeric(c(calib_regime[-c(1:4), event])) #remove the timestamp stuff
    
    if((length(unique(intensities)) ==1) & (0 %in% unique(intensities))){
      #if all the lights are off (i.e. everything 0), the LED being tested is the one where the next event has a non-zero.
      intensities = calib_regime[-c(1:4), (event+1)] 
      led = LED_wavelengths_vec[which(intensities != 0)]
      intensity = 0
    }
    else{ # The LED is whichever one has intensity > 0
      led = LED_wavelengths_vec[which(intensities != 0)]
      intensity = intensities[intensities != 0]
    }
    
    c(led, intensity)
  }))
  
  LED_cols = t(sapply(spectrophotometer_df$event, function(event){
    settings_dict[event,]
  }))
  colnames(LED_cols) = c('LED', 'intensity')
  
  
  if(nrow(LED_cols) != nrow(spectrophotometer_df)){
    warning("nrow(calib_info) does not match nrow(spectrophotometer_df)")
  }
  
  return(LED_cols)
}

#---
# Calculate the peaks from the calibration data

find_peaks = function(calib_df, by=c('event', 'time')){
  
  instances = unique(calib_df[, by])

  # Go through each event and find the wavelength with max irradiance
  bool_vec = c(sapply(instances, function(i){
    criteria = calib_df[, by]==i
    calib_subset = calib_df[criteria, ]
    
    m = max(calib_subset$irradiance)
    vec = calib_subset$irradiance == m
    vec
  }))
  
  #checks
  checkNPeaks = length(which(bool_vec)) == (nrow(LightFitR::helio.dyna.leds) * length(unique(calib_df$intensities)))
  checkRows = nrow(calib_df) == length(bool_vec)
  
  checks = c(checkNPeaks, checkRows)
  if(any(!checks)){
    warning(paste("Checks failed: ", paste(which(!checks), collapse=', ')))
  }
  
  return(bool_vec)
}
