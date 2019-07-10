subset_data_by_region <- function(inputdata , region) {
  outputdata <- inputdata[inputdata$Landkreis_von_GS == region , ]
  return(outputdata)
}


getBerlinData <- function(gtype) { #gtype can be either SFH or MFH
  
  
  if (gtype == "SFH") {
    
    load("D:/GITHUB_REPOS/co2emissions/SFH20022018_v3.RData")
    
    SFH20022018$abrechnungsjahr <- as.integer(SFH20022018$abrechnungsjahr)
    SFH20022018$verbrauch_gesamt_kwh <- gsub("," , "." , SFH20022018$verbrauch_gesamt_kwh)
    SFH20022018$verbrauch_gesamt_kwh <- as.numeric(SFH20022018$verbrauch_gesamt_kwh)
    SFH20022018$gtype <- "SFH"
    
    #subset data by region here---
    berlin_sfh <- subset_data_by_region(SFH20022018 , "Berlin, Stadt")
    
    berlin_sfh$energietraeger[berlin_sfh$energietraeger=="fernwaerme"] <- "waerme"
    berlin_sfh$energietraeger[berlin_sfh$energietraeger=="nahwaerme"] <- "waerme"
    berlin_sfh$energietraeger[berlin_sfh$energietraeger=="waermepumpe"] <- "strom"
    berlin_sfh$energietraeger[berlin_sfh$energietraeger=="waermepumpenstrom"] <- "strom"
    berlin_sfh$energietraeger[berlin_sfh$energietraeger=="waermepumpe_luft"] <- "strom"
    berlin_sfh$energietraeger[berlin_sfh$energietraeger=="waermepumpe_wasser"] <- "strom"
    
    berlin_sfh$sto_plz <- as.integer(berlin_sfh$sto_plz)
    
    return_data <- berlin_sfh
    
  }
  
  if (gtype == "MFH") {
    
    load("D:/GITHUB_REPOS/co2emissions/MFH20022018_v3.RData")
    MFH20022018$gtype <- "MFH"
    
    #subset data by region here---
    berlin_mfh <- subset_data_by_region(MFH20022018 , "Berlin, Stadt")
    
    berlin_mfh <- berlin_mfh[berlin_mfh$energietraeger != "braunkohle" , ]
    berlin_mfh$energietraeger[berlin_mfh$energietraeger=="fernwaerme"] <- "waerme"
    berlin_mfh$energietraeger[berlin_mfh$energietraeger=="nahwaerme"] <- "waerme"
    berlin_mfh$energietraeger[berlin_mfh$energietraeger=="waermepumpe"] <- "strom"
    berlin_mfh$energietraeger[berlin_mfh$energietraeger=="waermepumpenstrom"] <- "strom"
    berlin_mfh$energietraeger[berlin_mfh$energietraeger=="waermepumpe_luft"] <- "strom"
    berlin_mfh$energietraeger[berlin_mfh$energietraeger=="waermepumpe_wasser"] <- "strom"
    
    berlin_mfh$sto_plz <- as.integer(berlin_mfh$sto_plz)
    return_data <- berlin_mfh
    
    
  }
  
  return(return_data)
}