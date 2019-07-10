source("D:/GITHUB_REPOS/co2emissions/Berlin/mainCO2_Berlin.R")
et_list <- c("erdgas","fluessiggas","heizoel","holzpellets","strom","waerme")

Berlin_SFH <- main_function("SFH" , et_list)
Berlin_MFH <- main_function("MFH" , et_list)



write.csv2(
  Berlin_SFH$energy_prop_table , 
  file="D:/GITHUB_REPOS/co2emissions/Berlin/berlin_energy_proportions_SFH.csv" , 
  row.names = FALSE
)

write.csv2(
  Berlin_SFH$area_prop_table , 
  file="D:/GITHUB_REPOS/co2emissions/Berlin/berlin_area_proportions_SFH.csv" , 
  row.names = FALSE
)

write.csv2(
  Berlin_SFH$spz_verbrauch_mean , 
  file="D:/GITHUB_REPOS/co2emissions/Berlin/berlin_spz_verbrauch_SFH.csv" , 
  row.names = FALSE
)





write.csv2(
  Berlin_MFH$energy_prop_table , 
  file="D:/GITHUB_REPOS/co2emissions/Berlin/berlin_energy_proportions_MFH.csv" , 
  row.names = FALSE
)

write.csv2(
  Berlin_MFH$area_prop_table , 
  file="D:/GITHUB_REPOS/co2emissions/Berlin/berlin_area_proportions_MFH.csv" , 
  row.names = FALSE
)

write.csv2(
  Berlin_MFH$spz_verbrauch_mean , 
  file="D:/GITHUB_REPOS/co2emissions/Berlin/berlin_spz_verbrauch_MFH.csv" , 
  row.names = FALSE
)
