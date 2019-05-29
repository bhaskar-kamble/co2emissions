subset_data_by_region <- function(inputdata , region) {
  outputdata <- inputdata[inputdata$Landkreis_von_GS == region , ]
  return(outputdata)
}


getMannheimData <- function(gtype) { #gtype can be either SFH or MFH
  
  
  if (gtype == "SFH") {

    load("D:/GITHUB_REPOS/co2emissions/SFH20022018_v2.RData")
    
    SFH20022018$abrechnungsjahr <- as.integer(SFH20022018$abrechnungsjahr)
    SFH20022018$verbrauch_gesamt_kwh <- gsub("," , "." , SFH20022018$verbrauch_gesamt_kwh)
    SFH20022018$verbrauch_gesamt_kwh <- as.numeric(SFH20022018$verbrauch_gesamt_kwh)
    SFH20022018$gtype <- "SFH"
    
    #subset data by region here---
    mannheim_sfh <- subset_data_by_region(SFH20022018 , "Mannheim")
    
    mannheim_sfh$energietraeger[mannheim_sfh$energietraeger=="fernwaerme"] <- "waerme"
    mannheim_sfh$energietraeger[mannheim_sfh$energietraeger=="nahwaerme"] <- "waerme"
    mannheim_sfh$energietraeger[mannheim_sfh$energietraeger=="waermepumpe"] <- "strom"
    mannheim_sfh$energietraeger[mannheim_sfh$energietraeger=="waermepumpenstrom"] <- "strom"
    mannheim_sfh$energietraeger[mannheim_sfh$energietraeger=="waermepumpe_luft"] <- "strom"
    mannheim_sfh$energietraeger[mannheim_sfh$energietraeger=="waermepumpe_wasser"] <- "strom"
    
    mannheim_sfh$sto_plz <- as.integer(mannheim_sfh$sto_plz)

    return_data <- mannheim_sfh

  }

  if (gtype == "MFH") {

    load("D:/GITHUB_REPOS/co2emissions/MFH20022018_v2.RData")
    MFH20022018$gtype <- "MFH"
    
    #subset data by region here---
    mannheim_mfh <- subset_data_by_region(MFH20022018 , "Mannheim")
    
    mannheim_mfh <- mannheim_mfh[mannheim_mfh$energietraeger != "braunkohle" , ]
    mannheim_mfh$energietraeger[mannheim_mfh$energietraeger=="fernwaerme"] <- "waerme"
    mannheim_mfh$energietraeger[mannheim_mfh$energietraeger=="nahwaerme"] <- "waerme"
    mannheim_mfh$energietraeger[mannheim_mfh$energietraeger=="waermepumpe"] <- "strom"
    mannheim_mfh$energietraeger[mannheim_mfh$energietraeger=="waermepumpenstrom"] <- "strom"
    mannheim_mfh$energietraeger[mannheim_mfh$energietraeger=="waermepumpe_luft"] <- "strom"
    mannheim_mfh$energietraeger[mannheim_mfh$energietraeger=="waermepumpe_wasser"] <- "strom"
    
    mannheim_mfh$sto_plz <- as.integer(mannheim_mfh$sto_plz)
    return_data <- mannheim_mfh
    
    
  }

  return(return_data)
}