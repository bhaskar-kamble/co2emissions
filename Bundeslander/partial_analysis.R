source("D:/GITHUB_REPOS/co2emissions/Bundeslander/mainCO2_Bundesland.R")
et_list <- c("erdgas","fluessiggas","heizoel","holzpellets","strom","waerme")

BadWurt_SFH <- main_function("SFH" , et_list , "Baden-Württemberg")
BadWurt_MFH <- main_function("MFH" , et_list , "Baden-Württemberg")

#> attributes(BadWurt_SFH)
#$`names`
#[1] "bundesland_data"    "energy_prop_table"  "area_prop_table"    "spz_verbrauch_mean" "co2_coeff"         

write.csv2(
  BadWurt_SFH$energy_prop_table , 
  file="D:/GITHUB_REPOS/co2emissions/Bundeslander/BadenWurtemmberg/badenWurtemberg_energy_proportions_SFH.csv" , 
  row.names = FALSE
)

write.csv2(
  BadWurt_SFH$area_prop_table , 
  file="D:/GITHUB_REPOS/co2emissions/Bundeslander/BadenWurtemmberg/badenWurtemberg_area_proportions_SFH.csv" , 
  row.names = FALSE
)

write.csv2(
  BadWurt_SFH$spz_verbrauch_mean , 
  file="D:/GITHUB_REPOS/co2emissions/Bundeslander/BadenWurtemmberg/badenWurtemberg_spz_verbrauch_SFH.csv" , 
  row.names = FALSE
)






write.csv2(
  BadWurt_MFH$energy_prop_table , 
  file="D:/GITHUB_REPOS/co2emissions/Bundeslander/BadenWurtemmberg/badenWurtemberg_energy_proportions_MFH.csv" , 
  row.names = FALSE
)

write.csv2(
  BadWurt_MFH$area_prop_table , 
  file="D:/GITHUB_REPOS/co2emissions/Bundeslander/BadenWurtemmberg/badenWurtemberg_area_proportions_MFH.csv" , 
  row.names = FALSE
)

write.csv2(
  BadWurt_MFH$spz_verbrauch_mean , 
  file="D:/GITHUB_REPOS/co2emissions/Bundeslander/BadenWurtemmberg/badenWurtemberg_spz_verbrauch_MFH.csv" , 
  row.names = FALSE
)

