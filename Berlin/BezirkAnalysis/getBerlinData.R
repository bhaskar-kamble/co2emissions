subset_data_by_region <- function(inputdata , region) {
  outputdata <- inputdata[inputdata$Landkreis_von_GS == region , ]
  return(outputdata)
}


getBerlinData <- function(gtype) {
  #setwd("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis") 
  #plz_bezirk <- read.csv2("../Postleitzahl/plz_bezirk_long.csv" , stringsAsFactors = FALSE)
  plz_bezirk <- read.csv2("D:/GITHUB_REPOS/co2emissions/Berlin/Postleitzahl/plz_bezirk_long.csv" , stringsAsFactors = FALSE)
  if (gtype=="SFH") {
    #load("../../SFH20022018_v2.RData")
    load("D:/GITHUB_REPOS/co2emissions/SFH20022018_v2.RData")
    SFH20022018$abrechnungsjahr <- as.integer(SFH20022018$abrechnungsjahr)
    SFH20022018$verbrauch_gesamt_kwh <- gsub("," , "." , SFH20022018$verbrauch_gesamt_kwh)
    SFH20022018$verbrauch_gesamt_kwh <- as.numeric(SFH20022018$verbrauch_gesamt_kwh)
    SFH20022018$gtype <- "SFH"
    berlin_sfh <- subset_data_by_region(SFH20022018,"Berlin, Stadt")
    berlin_sfh$energietraeger[berlin_sfh$energietraeger=="fernwaerme"] <- "waerme"
    berlin_sfh$energietraeger[berlin_sfh$energietraeger=="nahwaerme"] <- "waerme"
    berlin_sfh$energietraeger[berlin_sfh$energietraeger=="waermepumpe"] <- "strom"
    berlin_sfh$energietraeger[berlin_sfh$energietraeger=="waermepumpe_luft"] <- "strom"
    berlin_sfh$energietraeger[berlin_sfh$energietraeger=="waermepumpe_wasser"] <- "strom"
    berlin_sfh$sto_plz <- as.integer(berlin_sfh$sto_plz)
    berlin_sfh <- merge(berlin_sfh , plz_bezirk , by.x="sto_plz" , by.y = "plz")
    berlin_return_data <- berlin_sfh
  }
  
  if (gtype=="MFH") {
    #load("../../MFH20022018_v2.RData")
    load("D:/GITHUB_REPOS/co2emissions/MFH20022018_v2.RData")
    MFH20022018$gtype <- "MFH"
    berlin_mfh <- subset_data_by_region(MFH20022018,"Berlin, Stadt")
    berlin_mfh <- berlin_mfh[(berlin_mfh$verbrauch_gesamt_kwh_spez > 15)&(berlin_mfh$verbrauch_gesamt_kwh_spez < 400) , ]
    berlin_mfh$energietraeger[berlin_mfh$energietraeger=="fernwaerme"] <- "waerme"
    berlin_mfh$energietraeger[berlin_mfh$energietraeger=="nahwaerme"] <- "waerme"
    berlin_mfh$energietraeger[berlin_mfh$energietraeger=="waermepumpe"] <- "strom"
    berlin_mfh$energietraeger[berlin_mfh$energietraeger=="waermepumpe_luft"] <- "strom"
    berlin_mfh$energietraeger[berlin_mfh$energietraeger=="waermepumpe_wasser"] <- "strom"
    berlin_mfh$sto_plz <- as.integer(berlin_mfh$sto_plz)
    berlin_mfh <- merge(berlin_mfh , plz_bezirk , by.x="sto_plz" , by.y = "plz")
    berlin_return_data <- berlin_mfh
  }
  return(berlin_return_data)
}

