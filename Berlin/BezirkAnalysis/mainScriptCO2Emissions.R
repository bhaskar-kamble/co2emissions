#setwd("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis")


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
  
  berlin_data <- getBerlinData(gtype)
  
  bezirk_data <- getBezirkData(bezirk,berlin_data)
  
  energy_prop_table <- energy_proportions_by_et(bezirk_data,et_list)
  
  area_prop_table <- area_proportions_by_et(bezirk_data,et_list)
  
  totalArea <- getBezirkAreas(bezirk)
  
  spz_verbrauch_mean <- getSpecificConsumption(bezirk_data , TRUE)
  
  totalConsumption <- getTotalConsumption(totalArea,spz_verbrauch_mean,gtype)
  
  energy_shares_absolute <- getAbsoluteEnergyShares(totalConsumption , energy_prop_table)
  
  co2_coeff <- getCO2Coeff()
  
  co2_emissions <- getCO2Emissions(co2_coeff , energy_shares_absolute)
  
  co2_emissions <- getRowSums(co2_emissions , "abrechnungsjahr")
  
  return(co2_emissions)
  
}