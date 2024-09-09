oceanViewUnits_to_watts = function(irradiance_vec){
  return(irradiance_vec * 10^-2)
}

watts_to_moles = function(wavelength_vec, watts_vec){
  return(photobiology::as_quantum_mol(wavelength_vec, watts_vec))
}

moles_to_umol = function(moles_vec){
  return(moles_vec * 10^6)
}

