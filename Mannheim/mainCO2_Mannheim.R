source("D:/GITHUB_REPOS/co2emissions/Mannheim/getMannheimData.R")
source("D:/GITHUB_REPOS/co2emissions/Mannheim/getMannheimAreas.R")
source("D:/GITHUB_REPOS/co2emissions/Mannheim/getSpecificConsumptionMannheim.R")
source("D:/GITHUB_REPOS/co2emissions/Mannheim/getCO2CoeffMannheim.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/energy_proportions_by_et.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/area_proportions_by_et.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/find_proportions.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getTotalConsumption.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getAbsoluteEnergyShares.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getCO2Emissions.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getRowSums.R")


main_function <- function(gtype , et_list) {
  
  return_data <- list()
  
  mannheim_data <- getMannheimData(gtype)
  mannheim_data$verbrauch_gesamt_kwh_spez <- mannheim_data$verbrauch_gesamt_kwh_spez/1.2
  if (gtype=="SFH") {
    cap_value <- 400.0
  }
  if (gtype=="MFH") {
    cap_value <- 350.0
  }
  mannheim_data <- mannheim_data[(mannheim_data$verbrauch_gesamt_kwh_spez < cap_value)&(mannheim_data$verbrauch_gesamt_kwh_spez > 15.0) , ]
  return_data$mannheim_data <- mannheim_data
  
  
  
  energy_prop_table <- energy_proportions_by_et(mannheim_data,et_list)
  return_data$energy_prop_table <- energy_prop_table
  
  
  
  area_prop_table <- area_proportions_by_et(mannheim_data,et_list)
  return_data$area_prop_table <- area_prop_table
  
  
  
  totalArea <- getMannheimAreas()
  return_data$totalArea <- totalArea
  
  
  
  spz_verbrauch_mean <- getSpecificConsumptionMannheim(mannheim_data , TRUE)
  return_data$spz_verbrauch_mean <- spz_verbrauch_mean
  
  
  
  totalConsumption <- getTotalConsumption(totalArea,spz_verbrauch_mean,gtype)
  return_data$totalConsumption <- totalConsumption
  
  
  
  energy_shares_absolute <- getAbsoluteEnergyShares(totalConsumption , energy_prop_table)
  return_data$energy_shares_absolute <- energy_shares_absolute
  
  
  
  co2_coeff <- getCO2CoeffMannheim()
  return_data$co2_coeff <- co2_coeff
  
  
  
  co2_emissions <- getCO2Emissions(co2_coeff , energy_shares_absolute)
  co2_emissions <- getRowSums(co2_emissions , "abrechnungsjahr")
  return_data$co2_emissions <- co2_emissions
  
  
  return(return_data)
  
}