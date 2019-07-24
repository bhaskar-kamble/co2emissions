subset_data_by_region <- function(inputdata , region) {
  outputdata <- inputdata[inputdata$Landkreis_von_GS == region , ]
  return(outputdata)
}


getRegionData <- function(gtype,region) { #gtype can be either SFH or MFH
  
  
  if (gtype == "SFH") {
    
    load("D:/GITHUB_REPOS/co2emissions/SFH20022018_v3.RData")
    
    SFH20022018$abrechnungsjahr <- as.integer(SFH20022018$abrechnungsjahr)
    SFH20022018$verbrauch_gesamt_kwh <- gsub("," , "." , SFH20022018$verbrauch_gesamt_kwh)
    SFH20022018$verbrauch_gesamt_kwh <- as.numeric(SFH20022018$verbrauch_gesamt_kwh)
    SFH20022018$gtype <- "SFH"
    
    #subset data by region here---
    region_sfh <- subset_data_by_region(SFH20022018 , region)
    
    region_sfh$energietraeger[region_sfh$energietraeger=="fernwaerme"] <- "waerme"
    region_sfh$energietraeger[region_sfh$energietraeger=="nahwaerme"] <- "waerme"
    region_sfh$energietraeger[region_sfh$energietraeger=="waermepumpe"] <- "strom"
    region_sfh$energietraeger[region_sfh$energietraeger=="waermepumpenstrom"] <- "strom"
    region_sfh$energietraeger[region_sfh$energietraeger=="waermepumpe_luft"] <- "strom"
    region_sfh$energietraeger[region_sfh$energietraeger=="waermepumpe_wasser"] <- "strom"
    
    region_sfh$sto_plz <- as.integer(region_sfh$sto_plz)
    
    return_data <- region_sfh
    
  }
  
  if (gtype == "MFH") {
    
    load("D:/GITHUB_REPOS/co2emissions/MFH20022018_v3.RData")
    MFH20022018$gtype <- "MFH"
    
    #subset data by region here---
    region_mfh <- subset_data_by_region(MFH20022018 , region)
    
    region_mfh <- region_mfh[region_mfh$energietraeger != "braunkohle" , ]
    region_mfh$energietraeger[region_mfh$energietraeger=="fernwaerme"] <- "waerme"
    region_mfh$energietraeger[region_mfh$energietraeger=="nahwaerme"] <- "waerme"
    region_mfh$energietraeger[region_mfh$energietraeger=="waermepumpe"] <- "strom"
    region_mfh$energietraeger[region_mfh$energietraeger=="waermepumpenstrom"] <- "strom"
    region_mfh$energietraeger[region_mfh$energietraeger=="waermepumpe_luft"] <- "strom"
    region_mfh$energietraeger[region_mfh$energietraeger=="waermepumpe_wasser"] <- "strom"
    
    region_mfh$sto_plz <- as.integer(region_mfh$sto_plz)
    return_data <- region_mfh
    
    
  }
  
  return(return_data)
}