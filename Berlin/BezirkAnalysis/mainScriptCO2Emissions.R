setwd("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis")


source("getBerlinData.R"           )
source("getBezirkData.R"           )
source("energy_proportions_by_et.R" )
source("appendLinearTrend.R"       )
source("area_proportions_by_et.R"  )
source("find_proportions.R"        )
source("getBezirkAreas.R"        )
source("getSpecificConsumption.R"   )
source("getTotalConsumption.R"     )
source("getAbsoluteEnergyShares.R"  )
source("getCO2Coeff.R"             )
source("getCO2Emissions.R"         )
source("getRowSums.R")

gtype <- "SFH"   # SFH or MFH
bezirk <- "charlottenburg_wilmersdorf"

main_function <- function(gtype,bezirk) {
  
  berlin_data <- getBerlinData(gtype)
  
  bezirk_data <- getBezirkData(bezirk,berlin_data)
  
  energy_prop_table <- energy_proportions_by_et(bezirk_data)
  
  area_prop_table <- area_proportions_by_et(bezirk_data)
  
  totalArea <- getBezirkAreas(bezirk)
  
  spz_verbrauch_mean <- getSpecificConsumption(bezirk_data , TRUE)
  
  totalConsumption <- getTotalConsumption(totalArea,spz_verbrauch_mean,gtype)
  
  energy_shares_absolute <- getAbsoluteEnergyShares(totalConsumption , energy_prop_table)
  
  co2_coeff <- getCO2Coeff()
  
  co2_emissions <- getCO2Emissions(co2_coeff , energy_shares_absolute)
  
  co2_emissions <- getRowSums(co2_emissions , "abrechnungsjahr")
  
  return(co2_emissions)
  
}