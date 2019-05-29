source("D:/GITHUB_REPOS/co2emissions/Mannheim/getMannheimData.R")

main_function <- function(gtype , et_list) {
  
  mannheim_data <- getMannheimData(gtype)
  mannheim_data$verbrauch_gesamt_kwh_spez <- mannheim_data$verbrauch_gesamt_kwh_spez/1.2
  if (gtype=="SFH") {
    cap_value <- 400.0
  }
  if (gtype=="MFH") {
    cap_value <- 350.0
  }
  mannheim_data <- mannheim_data[(mannheim_data$verbrauch_gesamt_kwh_spez < cap_value)&(mannheim_data$verbrauch_gesamt_kwh_spez > 15.0) , ]
  
  energy_prop_table <- energy_proportions_by_et(mannheim_data,et_list)
  
  area_prop_table <- area_proportions_by_et(mannheim_data,et_list)
  
}