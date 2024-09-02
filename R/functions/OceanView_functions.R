# Depends: stringr and lubridate packages

# Read a single OceanView ASCII file
read.OceanView = function(file, skip=14){

  # Get the irradiance data
  measures = read.table(file, skip=skip)
  
  # Parse header metadata
  header = read.table(file, sep='\n', nrow=skip)

  ## Metadata extract variables
  folder_depth = 1 + stringr::str_count(file, pattern='/')
  fn = stringr::str_split_i(file, pattern = '/', i=folder_depth) #This allows us to isolate the filename for the filename column in the dataframe, without the full filepath.
  
  time = stringr::str_extract(header[2,], pattern='\\d{2}\\:\\d{2}\\:\\d{2}')
  integration = as.numeric(stringr::str_extract(header[6,], pattern='(\\d+\\.\\d+E-\\d*)|(\\d+\\.\\d+)|(\\d+)')) #Integration time setting when running the spectrometer
  scans = stringr::str_extract(header[7,], pattern='\\d+') #How many scans to average
  
  rm(folder_depth)

  # Format metadata - use nrows of measures to make big vectors of the metadata
  filename = rep(fn, nrow(measures))
  time_vec = rep(time, nrow(measures))
  int_vec = rep(integration, nrow(measures))
  scans_vec = rep(scans, nrow(measures))
  
  metadata = cbind(filename, time_vec, int_vec, scans_vec)

  # Make dataframe
  combined = data.frame(cbind(metadata, measures))
  colnames(combined) = c('filename', 'time', 'integration_time', 'scans', 'wavelength', 'irradiance')
  return(combined)
}

#---
# Read a folder of OceanView ASCII files

read_many.OceanView = function(filepath){
  
  filenames = list.files(filepath, pattern='.txt') #Make a list of filenames
  message(paste(length(filenames), ' ASCII files found.'))
  
  # Go through each file and extract the data into a list
  measurements = lapply(filenames, function(f){
    print(f) # Reassures user when importing lots of files
    full_path = paste(filepath, f, sep='/') #Reconstruct the full path to pass into read.OceanView
    data = read.OceanView(full_path)
    return(data)
  })

  # Formatting
  measurements = as.data.frame(do.call(rbind, measurements)) #Converts from list to dataframe
  print(summary(measurements)) #Gives user info about their data
  return(measurements)
}

#---
# Trim wavelengths
trim_wavelengths = function(df){
  wlRange = c(300, 800)
  criteria = (df$wavelength > wlRange[1]) & (df$wavelength < wlRange[2])
  dfOut = df[criteria,]
  return(dfOut)
}

#---
# Trim timepoints to keep only the ones we care about

trim_times = function(start, end, timepoints_vec, df){
  
  # Formatting variables
  timepoints = lubridate::as_datetime(hms(timepoints_vec))
  timeRange = lubridate::interval(start=lubridate::as_datetime(hms(start)), end = (lubridate::as_datetime(hms(end))))
  
  # Decide which timepoints to keep
  timestamps = unique(timepoints) #Formatting and uniquing
  keepTimes = timestamps[timestamps %within% timeRange]
  
  # Checks
  if(length(keepTimes) >= length(timestamps)){
    warning("All of the timepoints seem to fall within range. Please double check this.")
  }
  
  # Generate new df with only rows with times we need
  keepVec = timepoints %in% keepTimes # Boolean of whether the timepoint is on the keep list
    
  dfOut = df[keepVec,]
  
  stopifnot(nrow(dfOut) < nrow(df))
  
  return(dfOut)
}

#---
# Match the timestamp with the event number from the light schedule

event_nos_timestamp = function(intensities_matrix, OceanView_dataframe, n_test_scripts = 1){ 
  
  # Get times of events and group them into bins
  
  events = lubridate::as_datetime(lubridate::hms(intensities_matrix[1,]))
  time_bins = lubridate::interval(events[-length(events)], events[-1]) # Find time intervals between each event
  
  # Get times of measurements
  times_char = unique(OceanView_dataframe$time)
  measurements_times = lubridate::as_datetime(hms(times_char))
  
  times = data.frame(char=times_char, datetime = measurements_times)
  rm(times_char, measurements_times)
  
  # Make dictionary of measurement times and their corresponding event number
  
  time_dictionary = sapply(times$datetime, function(time){
    
    event = which(time %within% time_bins) # The positions will correspond with the event number
    
    if(length(event) < n_test_scripts+1){ # ? accounts for the measurements taken outside of the script time
      # Add NAs to make the lengths of event the same
      diff = n_test_scripts +1 - length(event)
      event = c(event, rep_len(NA, diff))
    }
    
    #print(length(event))
    event
  })
  time_dictionary = data.frame(t(time_dictionary), row.names=times$char)
  
  nEvents = (max(time_dictionary, na.rm=T)-min(time_dictionary, na.rm=T))
  message(paste(nEvents, ' events out of ', ncol(intensities_matrix), ' found.')) 
  rm(time_bins, nEvents)
  
  # Make vector of event numbers
  
  filenames = unique(OceanView_dataframe$filename)
  
  events_vec = c(sapply(filenames, function(fn){
    
    # # Figure out which segment of the script it's from 
    # # TODO This is useful when analysing data from multiple scripts, but look into how to work this into the fuction
    # type = stringr::str_extract(fn, pattern='test22[:alpha:]') #Get test20 part
    # type = stringr::str_extract(type, pattern='[:alpha:]$') #Get last letter on test20 part
    # segment = which(letters == type) #Determine (numerically) which segment it belongs to, corresponds with colnumber of time_dictionary
    # rm(type)
    
    if(n_test_scripts==1){
      segment = 1
    }
    
    # Get the timepoint for that file
    fn_times = c(OceanView_dataframe[OceanView_dataframe$filename==fn,'time'])
    
    if(length(unique(fn_times))==1){ # There should be 1 unique time
      time = (unique(fn_times))
      event = time_dictionary[as.character(time), segment]
    }
    else{
      warning(paste(fn, 'failed to get allocated to an event', sep=' '))
      event = NA
    }
    
    len = length(fn_times)
    
    fn_events = rep(event, len)
  }))
  
  # Count NAs
  message(paste('NAs fOund: ', length(which(is.na(events_vec)))))
  
  # Checks
  lenMatch = length(events_vec) == nrow(OceanView_dataframe)
  rangeMatch = range(events_vec, na.rm=T) == range(events)
  uniqMatch = length(unique(events_vec)) == (ncol(intensities_matrix) + 1) #+1 for any NAs
  
  checks = c(lenMatch, rangeMatch, uniqMatch)
# 
#   sapply(1:ncol(intensities), function(i){
#     print(length(unique(measurements[measurements$event==i, 'filename'])))
#   })
  
  rm(time_dictionary)
  
  if(all(checks)){
    message(paste('Checks passed: ', paste(which(checks), collapse=', '), '\n'))
    return(events_vec)
  } else{
    warning(paste('Checks failed: ', paste(which(!checks), collapse=', ')))
    return(events_vec)
  }
}

#---
# Keep only middle timepoint in each event

is.middle = function(events_vec, time_vec){
  
  # Checks
  if(length(events_vec) != length(time_vec)){
    stop("length(events_vec) != length(time_vec) \n 
         Please point to the events and time columns in your dataframe")
  }
  
  # Define variables
  eventsUnique = na.omit(unique(events_vec))
  
  eventDict = data.frame(event = events_vec, time = time_vec)
  
  # Pick the middle times
  middle_times = sapply(eventsUnique, function(i){ # For each event
    
    # Get times corresponding to the event
    times = unique(eventDict[eventDict$event==i, 'time'])
    times = na.omit(times)
    times = sort(lubridate::as_datetime(lubridate::hms(times), origin=lubridate::origin)) #format as datetime
    
    # Find the middle timepoint
    if((length(times) %% 2) ==1){ # If it's an odd length of times, find the straightforward median
      med = median(times)
    } 
    else{ # If it's an even length of times, randomly pick either side of the median
      halfway = length(times)/2
      rand = sample(c(halfway, halfway+1), size=1)
      med = times[rand]
    }
    
    # Format median timepoint as characters that can be matched
    med_char = as.character(format(med, '%H:%M:%S'))
    
    med_char
  })
  
  boolOut = time_vec %in% middle_times
  
  if(length(boolOut) == length(events_vec)){
    return(boolOut)
  } else{
    warning("Something went wrong with calculating middle timepoints; length of output doesn't match input")
    return(boolOut)
  }
}

#---
# Get the total irradiance per event

get_total_irradiance = function(spectrophotometer_df, by = c('event', 'time')){
  
  # Get columns we need to keep, for formatting later
  discard_cols = c('wavelength', 'irradiance', 'watts', 'mol', 'umol', 'peak')
  discard_nos = which(colnames(spectrophotometer_df) %in% discard_cols)
  keep_cols = colnames(spectrophotometer_df)[-discard_nos]
  
  # Define variables
  instances = unique(spectrophotometer_df[, by])
  
  dfOut = t(sapply(instances, function(i){
    criteria = spectrophotometer_df[, by] ==i
    data_subset = spectrophotometer_df[criteria,]
    
    # Values to keep from other columns
    other_cols = sapply(keep_cols, function(k){
      uni = unique(data_subset[, k])
      uni
    })
    
    # Calculate total
    total = sum(data_subset$irradiance)
    
    c(other_cols, total)
  }))
  
  # Formatting
  dfOut = as.data.frame(dfOut)
  colnames(dfOut) = c(keep_cols, 'total_irradiance')
  
  dfOut$event = as.numeric(dfOut$event)
  dfOut$total_irradiance = as.numeric(dfOut$total_irradiance)
  
  return(dfOut)
}
