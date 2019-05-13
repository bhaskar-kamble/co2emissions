getCO2Emissions <- function(co2_coeff,energy_shares_absolute) {
  if (!setequal(names(energy_shares_absolute),names(co2_coeff))) {
    stop("give same names")
  }
  energy_shares_absolute <- energy_shares_absolute[,names(co2_coeff)]
  co2_emissions <- co2_coeff*energy_shares_absolute
  co2_emissions$abrechnungsjahr <- 2002:2018
  return(co2_emissions)
}