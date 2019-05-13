getAbsoluteEnergyShares <- function(totalConsumption , energy_prop_table) {
  energy_shares_absolute <- 0.01*totalConsumption$gesamtVerbrauch * energy_prop_table
  energy_shares_absolute$abrechnungsjahr <- 2002:2018
  return(energy_shares_absolute)
}