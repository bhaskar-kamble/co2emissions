subset_data_by_region <- function(inputdata , region) {
  outputdata <- inputdata[inputdata$Landkreis_von_GS == region , ]
  return(outputdata)
}


getRegionData <- function(gtype,region) { #gtype can be either SFH or MFH
  
  
  if (gtype == "SFH") {
    
    load("/home/kbhaskar/Github_Repos/co2emissions/SFH20022018_v3.RData")
    SFH20022018$Landkreis_von_GS <- iconv(SFH20022018$Landkreis_von_GS, to = "UTF-8", sub = "X")
    
    SFH20022018$abrechnungsjahr <- as.integer(SFH20022018$abrechnungsjahr)
    SFH20022018$verbrauch_gesamt_kwh <- gsub("," , "." , SFH20022018$verbrauch_gesamt_kwh)
    SFH20022018$verbrauch_gesamt_kwh <- as.numeric(SFH20022018$verbrauch_gesamt_kwh)
    SFH20022018$gtype <- "SFH"
    
    
    
    #subset data by region here---
    region_sfh <- subset_data_by_region(SFH20022018 , region)
    
    #waermepumpe: divide by 3.5
    region_sfh$verbrauch_gesamt_kwh[region_sfh$energietraeger=="waermepumpe"] <- 
      region_sfh$verbrauch_gesamt_kwh[region_sfh$energietraeger=="waermepumpe"]/3.5
    region_sfh$verbrauch_gesamt_kwh_spz[region_sfh$energietraeger=="waermepumpe"] <- 
      region_sfh$verbrauch_gesamt_kwh_spz[region_sfh$energietraeger=="waermepumpe"]/3.5
    
    
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
    
    load("/home/kbhaskar/Github_Repos/co2emissions/MFH20022018_v3.RData")
    MFH20022018$Landkreis_von_GS <- iconv(MFH20022018$Landkreis_von_GS, to = "UTF-8", sub = "X")
    MFH20022018$gtype <- "MFH"
    
    #subset data by region here---
    region_mfh <- subset_data_by_region(MFH20022018 , region)

    #waermepumpe: divide by 3.5
    region_mfh$verbrauch_gesamt_kwh[region_mfh$energietraeger=="waermepumpe"] <- 
      region_mfh$verbrauch_gesamt_kwh[region_mfh$energietraeger=="waermepumpe"]/3.5
    region_mfh$verbrauch_gesamt_kwh_spz[region_mfh$energietraeger=="waermepumpe"] <- 
      region_mfh$verbrauch_gesamt_kwh_spz[region_mfh$energietraeger=="waermepumpe"]/3.5
    
        
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
