
wls = unique(calib$wavelength)

rolling = lapply(helio.leds$wavelength, function(l){ #For each led
  
  led_subset = calib[calib$LED==l,]
  intensities = unique(led_subset$intensity)
  
  led_rolling = lapply(intensities, function(i){ #For each intensity
    
    message(paste('\n', 'LED = ', l, ',', 'intensity = ', i, sep=' '))
    
    # Subset calibration data by led and intensity
    criteria = (led_subset$LED==l) & (led_subset$intensity==i)
    intensity_subset = led_subset[criteria,]
    intensity_subset = intensity_subset[complete.cases(intensity_subset), ]
    
    # Checks
    test = (nrow(intensity_subset) == length(unique(intensity_subset$wavelength))) #Each row should represent a unique wavelength
    if(test==F){
      warning('There is a problem with the raw calibration data for this channel at this intensity ^. Excluded from rolling average')
      return()
    }
    
    else{ # Get the rolling average
      
      average = t(sapply(3:(length(wls)-2), function(a){
        wl = wls[a]
        
        av = mean(intensity_subset[((a-2):(a+2)), 'irradiance'])
        
        c(wl, av)
      }))
      
      # Formatting
      fn_vec = rep(unique(intensity_subset$filename), nrow(average))
      integration_vec = rep(unique(intensity_subset$integration_time), nrow(average))
      scans_vec = rep(unique(intensity_subset$scans), nrow(average))
      time_vec = rep(unique(intensity_subset$time), nrow(average))
      event_vec = rep(unique(intensity_subset$event), nrow(average))
      led_vec = rep(l, nrow(average))
      int_vec = rep(i, nrow(average))
      
      
      data.frame(filename=fn_vec, integration_time=integration_vec, scans=scans_vec,
                 time=time_vec, event=event_vec,
                 LED=led_vec, intensity=int_vec, wavelength=average[,1], irradiance=average[,2])
    }
  })
  
  led_rolling = do.call(rbind, led_rolling)
  return(led_rolling)
})

rolling = do.call(rbind, rolling)

rm(wls)