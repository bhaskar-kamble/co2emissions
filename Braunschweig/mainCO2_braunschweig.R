source("/home/kbhaskar/Github_Repos/co2emissions/RheinNeckarKreis/getRegionData.R")

# Here Braunschweig specific---
#---
source("/home/kbhaskar/Github_Repos/co2emissions/Braunschweig/get_Braunschweig_Areas.R")
source("/home/kbhaskar/Github_Repos/co2emissions/Braunschweig/getSpecificConsumptionBraunschweig.R")
#---

source("/home/kbhaskar/Github_Repos/visualization-project2-smurfs/cleanData.R")

source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/energy_proportions_by_et.R")
#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")
source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/area_proportions_by_et.R")
#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/find_proportions.R")
source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/getTotalConsumption.R")
source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/getAbsoluteEnergyShares.R")
#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getCO2Emissions.R")
#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getRowSums.R")

main_function <- function(gtype , et_list) {
  
  return_data <- list()
  
  region <- "Braunschweig, Stadt"
  region_data <- getRegionData(gtype,region)
  
  #if (gtype=="SFH") {
    #cap_value <- 400.0
  #}
  #if (gtype=="MFH") {
    #cap_value <- 350.0
  #}
  #region_data <- region_data[(region_data$verbrauch_gesamt_kwh_spez < cap_value)&(region_data$verbrauch_gesamt_kwh_spez > 15.0) , ]
  region_data <- cleanData(region_data , gtype)
  return_data$region_data <- region_data
  
  energy_prop_table <- energy_proportions_by_et(region_data,et_list)
  #replace by linear trend temporarily-----------
  source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/linearizer.R")
  energy_prop_table <- linearizer(obj = energy_prop_table , dropCols = NULL , xVar = "abrechnungsjahr")
  return_data$energy_prop_table <- energy_prop_table
  
  area_prop_table <- area_proportions_by_et(region_data,et_list)
  return_data$area_prop_table <- area_prop_table
  
  totalArea <- get_Braunschweig_Areas()
  return_data$totalArea <- totalArea
  
  spz_verbrauch_mean <- getSpecificConsumptionBraunschweig(region_data , FALSE)
  #replace by linear trend temporarily-----------
  source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/linearizer.R")
  spz_verbrauch_mean <- linearizer(obj = spz_verbrauch_mean , dropCols = NULL , xVar = "abrechnungsjahr")
  return_data$spz_verbrauch_mean <- spz_verbrauch_mean
  
  totalConsumption <- getTotalConsumption(totalArea,spz_verbrauch_mean,gtype)
  return_data$totalConsumption <- totalConsumption
  
  energy_shares_absolute <- getAbsoluteEnergyShares(totalConsumption , energy_prop_table)
  return_data$energy_shares_absolute <- energy_shares_absolute
  
  return(return_data)
  
}