setwd("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis")
source("getBerlinData.R"           )
source("getBezirkData.R")
source("energy_proportions_by_et.R" )

#do for sfh first
berlin_sfh <- getBerlinData("SFH")
bezirk_list <- unique(berlin_sfh$bezirk)

for (bezirk in bezirk_list) {
  bezirk_sfh <- getBezirkData(bezirk,berlin_sfh)
  energy_prop_table <- energy_proportions_by_et(bezirk_sfh)
  energy_prop_table <- energy_prop_table[, c("abrechnungsjahr" , 
              names(energy_prop_table)[names(energy_prop_table)!="abrechnungsjahr"])]
  filename <- paste0(bezirk , "_" , "SFH.csv")
  write.csv2(energy_prop_table , file = filename , row.names = FALSE)
}


#do for mfh now
berlin_sfh <- getBerlinData("MFH")
bezirk_list <- unique(berlin_sfh$bezirk)

for (bezirk in bezirk_list) {
  bezirk_sfh <- getBezirkData(bezirk,berlin_sfh)
  energy_prop_table <- energy_proportions_by_et(bezirk_sfh)
  energy_prop_table <- energy_prop_table[, c("abrechnungsjahr" , 
                                             names(energy_prop_table)[names(energy_prop_table)!="abrechnungsjahr"])]
  filename <- paste0(bezirk , "_" , "MFH.csv")
  write.csv2(energy_prop_table , file = filename , row.names = FALSE)
}