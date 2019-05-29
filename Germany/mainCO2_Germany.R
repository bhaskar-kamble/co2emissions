#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getBerlinData.R")
#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getBezirkData.R")
source("D:/GITHUB_REPOS/co2emissions/Germany/getGermanyData.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/energy_proportions_by_et.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/area_proportions_by_et.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/find_proportions.R")
##---------------------------------------------------------------------------
#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getBezirkAreas.R")
source("D:/GITHUB_REPOS/co2emissions/Germany/getGermanyAreas.R")
##---------------------------------------------------------------------------
source("D:/GITHUB_REPOS/co2emissions/Germany/getSpecificConsumptionGermany.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getTotalConsumption.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getAbsoluteEnergyShares.R")
source("D:/GITHUB_REPOS/co2emissions/Germany/getCO2CoeffGermany.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getCO2Emissions.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getRowSums.R")

main_function <- function(gtype,et_list) {
  
  germany_data <- getGermanyData(gtype)
  germany_data$verbrauch_gesamt_kwh_spez <- germany_data$verbrauch_gesamt_kwh_spez/1.2
  if (gtype=="SFH") {
    cap_value <- 400.0
  }
  if (gtype=="MFH") {
    cap_value <- 350.0
  }
  germany_data <- germany_data[(germany_data$verbrauch_gesamt_kwh_spez < cap_value)&(germany_data$verbrauch_gesamt_kwh_spez > 15.0) , ]
  
  energy_prop_table <- energy_proportions_by_et(germany_data,et_list)
  #energy_prop_table <- appendLinearTrend(energy_prop_table , "abrechnungsjahr" , NULL , 2015:2018)
  
  area_prop_table <- area_proportions_by_et(germany_data,et_list)
  #area_prop_table <- appendLinearTrend(area_prop_table , "abrechnungsjahr" , NULL , 2015:2018)
  
  totalArea <- getGermanyAreas() # THIS IS THE STEP WHERE I NEED DATA!!!
  
  spz_verbrauch_mean <- getSpecificConsumptionGermany(germany_data , FALSE)
  #spz_verbrauch_mean <- appendLinearTrend(spz_verbrauch_mean , "abrechnungsjahr" , NULL , 2015:2018)
  
  totalConsumption <- getTotalConsumption(totalArea,spz_verbrauch_mean,gtype)
  #totalConsumption <- appendLinearTrend(totalConsumption , "abrechnungsjahr" , NULL , 2015:2018)
  
  energy_shares_absolute <- getAbsoluteEnergyShares(totalConsumption , energy_prop_table)
  #energy_shares_absolute <- appendLinearTrend(energy_shares_absolute , "abrechnungsjahr" , NULL , 2015:2018)
  
  co2_coeff <- getCO2CoeffGermany()
  
  co2_emissions <- getCO2Emissions(co2_coeff , energy_shares_absolute)
  
  co2_emissions <- getRowSums(co2_emissions , "abrechnungsjahr")
  #co2_emissions <- appendLinearTrend(co2_emissions , "abrechnungsjahr" , NULL , 2015:2018)
  
  return(co2_emissions)
  
  
  
}