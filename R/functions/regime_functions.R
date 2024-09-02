# Make the timepoints for testing at 5mins apart
test_times = function(nEvents){
  time_vec = seq(from=lubridate::hm('00:00'), by=lubridate::minutes(5), length.out=nEvents)
  time_vec = as.POSIXct(time_vec, origin=lubridate::origin, tz='GMT')
  time_mat = LightFitR::internal.makeTimes(time_vec)
  return(time_mat)
}
