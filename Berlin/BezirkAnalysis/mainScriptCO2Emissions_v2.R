#setwd("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis")

source("D:/GITHUB_REPOS/visualization-project2-smurfs/cleanData.R")

source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getBerlinData.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getBezirkData.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/energy_proportions_by_et.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/area_proportions_by_et.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/find_proportions.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getBezirkAreas.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getSpecificConsumption.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getTotalConsumption.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getAbsoluteEnergyShares.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getCO2Coeff.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getCO2Emissions.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getRowSums.R")

#gtype <- "SFH"   # SFH or MFH
#bezirk <- "charlottenburg_wilmersdorf"

main_function <- function(gtype,bezirk,et_list) {
  
  return_data <- list()
  
  berlin_data <- getBerlinData(gtype)
  berlin_data$verbrauch_gesamt_kwh_spez <- berlin_data$verbrauch_gesamt_kwh_spez/1.2
  berlin_data <- cleanData(berlin_data , gtype)
  
  bezirk_data <- getBezirkData(bezirk,berlin_data)
  return_data$bezirk_data <- bezirk_data
  
  energy_prop_table <- energy_proportions_by_et(bezirk_data,et_list)
  return_data$energy_prop_table <- energy_prop_table
  
  area_prop_table <- area_proportions_by_et(bezirk_data,et_list)
  return_data$area_prop_table <- area_prop_table
  
  totalArea <- getBezirkAreas(bezirk)
  return_data$totalArea <- totalArea
  
  spz_verbrauch_mean <- getSpecificConsumption(bezirk_data , TRUE)
  return_data$spz_verbrauch_mean <- spz_verbrauch_mean
  
  totalConsumption <- getTotalConsumption(totalArea,spz_verbrauch_mean,gtype)
  return_data$totalConsumption <- totalConsumption
  
  energy_shares_absolute <- getAbsoluteEnergyShares(totalConsumption , energy_prop_table)
  return_data$energy_shares_absolute <- energy_shares_absolute
  
  co2_coeff <- getCO2Coeff()
  return_data$co2_coeff <- co2_coeff
  
  co2_emissions <- getCO2Emissions(co2_coeff , energy_shares_absolute)
  co2_emissions <- getRowSums(co2_emissions , "abrechnungsjahr")
  return_data$co2_emissions <- co2_emissions
  
  return(return_data)
  
}