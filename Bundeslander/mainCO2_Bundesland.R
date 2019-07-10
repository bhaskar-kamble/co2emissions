source("D:/GITHUB_REPOS/co2emissions/Bundeslander/getBundeslandData.R")
#source("D:/GITHUB_REPOS/co2emissions/Bundeslander/getBundeslandAreas.R")
source("D:/GITHUB_REPOS/co2emissions/Bundeslander/getSpecificConsumptionBundesland.R")
source("D:/GITHUB_REPOS/co2emissions/Bundeslander/getCO2CoeffBundesland.R")

source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/energy_proportions_by_et.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/area_proportions_by_et.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/find_proportions.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getTotalConsumption.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getAbsoluteEnergyShares.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getCO2Emissions.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getRowSums.R")

main_function <- function(gtype , et_list , bundesland) {
  
  return_data <- list()
  bundesland_data <- getBundeslandData(gtype,bundesland)
  bundesland_data$verbrauch_gesamt_kwh_spez <- bundesland_data$verbrauch_gesamt_kwh_spez/1.2
  return_data$bundesland_data <- bundesland_data
  
  energy_prop_table <- energy_proportions_by_et(bundesland_data,et_list)
  return_data$energy_prop_table <- energy_prop_table
  
  area_prop_table <- area_proportions_by_et(bundesland_data,et_list)
  return_data$area_prop_table <- area_prop_table
  
  #totalArea <- getBundeslandAreas()
  #return_data$totalArea <- totalArea
  
  spz_verbrauch_mean <- getSpecificConsumptionBundesland(bundesland_data , TRUE , bundesland)
  return_data$spz_verbrauch_mean <- spz_verbrauch_mean
  
  #totalConsumption <- getTotalConsumption(totalArea,spz_verbrauch_mean,gtype)
  #return_data$totalConsumption <- totalConsumption
  
  #energy_shares_absolute <- getAbsoluteEnergyShares(totalConsumption , energy_prop_table)
  #return_data$energy_shares_absolute <- energy_shares_absolute
  
  co2_coeff <- getCO2CoeffBundesland(bundesland)
  return_data$co2_coeff <- co2_coeff
  
  #co2_emissions <- getCO2Emissions(co2_coeff , energy_shares_absolute)
  #co2_emissions <- getRowSums(co2_emissions , dropCols = "abrechnungsjahr")
  #return_data$co2_emissions <- co2_emissions
  
  
  return(return_data)

}
