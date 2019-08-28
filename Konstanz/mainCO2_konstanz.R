source("D:/GITHUB_REPOS/co2emissions/RheinNeckarKreis/getRegionData.R")

source("D:/GITHUB_REPOS/co2emissions/Konstanz/get_Konstanz_Areas.R")
source("D:/GITHUB_REPOS/co2emissions/Konstanz/getSpecificConsumptionKonstanz.R")
source("D:/GITHUB_REPOS/co2emissions/Konstanz/getCO2CoeffKonstanz.R")

source("D:/GITHUB_REPOS/visualization-project2-smurfs/cleanData.R")

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
  
  region <- "Konstanz"
  region_data <- getRegionData(gtype,region)
  #region_data$verbrauch_gesamt_kwh_spez <- region_data$verbrauch_gesamt_kwh_spez/1.2
  if (gtype=="SFH") {
    cap_value <- 400.0
  }
  if (gtype=="MFH") {
    cap_value <- 350.0
  }
  region_data <- region_data[(region_data$verbrauch_gesamt_kwh_spez < cap_value)&(region_data$verbrauch_gesamt_kwh_spez > 15.0) , ]
  region_data <- cleanData(region_data , gtype)
  return_data$region_data <- region_data
  
  
  
  energy_prop_table <- energy_proportions_by_et(region_data,et_list)
  return_data$energy_prop_table <- energy_prop_table
  
  
  
  area_prop_table <- area_proportions_by_et(region_data,et_list)
  return_data$area_prop_table <- area_prop_table
  
  totalArea <- get_Konstanz_Areas()
  return_data$totalArea <- totalArea
  
  
  spz_verbrauch_mean <- getSpecificConsumptionKonstanz(region_data , TRUE)
  return_data$spz_verbrauch_mean <- spz_verbrauch_mean
  
  
  
  totalConsumption <- getTotalConsumption(totalArea,spz_verbrauch_mean,gtype)
  return_data$totalConsumption <- totalConsumption
  
  
  
  energy_shares_absolute <- getAbsoluteEnergyShares(totalConsumption , energy_prop_table)
  return_data$energy_shares_absolute <- energy_shares_absolute
  
  
  
  co2_coeff <- getCO2CoeffKonstanz()
  return_data$co2_coeff <- co2_coeff
  
  
  
  co2_emissions <- getCO2Emissions(co2_coeff , energy_shares_absolute)
  co2_emissions <- getRowSums(co2_emissions , dropCols = "abrechnungsjahr")
  return_data$co2_emissions <- co2_emissions
  
  
  return(return_data)
  
}