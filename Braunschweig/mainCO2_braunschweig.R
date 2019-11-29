source("/home/kbhaskar/Github_Repos/co2emissions/RheinNeckarKreis/getRegionData.R")

# Here Braunschweig specific---
#---
#---

source("/home/kbhaskar/Github_Repos/visualization-project2-smurfs/cleanData.R")

source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/energy_proportions_by_et.R")
#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")
#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/area_proportions_by_et.R")
#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/find_proportions.R")
#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getTotalConsumption.R")
#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getAbsoluteEnergyShares.R")
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
  return_data$energy_prop_table <- energy_prop_table
  
  return(return_data)
  
}