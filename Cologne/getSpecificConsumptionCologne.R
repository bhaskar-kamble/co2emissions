source("/home/kbhaskar/Github_Repos//co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")

createRegData01 <- function(hev_data) {#no multivariate analysis for weather, based on BerlinPresentationCO2BalanceSFH_v6.R, line 530
  #hev_data is the same as bezirk_data
  require(dplyr)
  by_AbJahr <- group_by(hev_data , abrechnungsjahr)  
  #RegData_temp <- as.data.frame(summarize(by_AbJahr,mean(gebaeude_baujahr,na.rm=TRUE),mean(gebaeude_nutzflaeche,na.rm=TRUE),mean(verbrauch_gesamt_kwh_spez,na.rm=TRUE)))
  RegData_temp <- as.data.frame(summarize(by_AbJahr,mean(verbrauch_gesamt_kwh_spez,na.rm=TRUE)))
  names(RegData_temp) <- c("abrechnungsjahr","mean_spzverbrauch")
  years_absent <- (2002:2018)[!((2002:2018) %in% RegData_temp$abrechnungsjahr)]
  RegData_temp <- appendLinearTrend(RegData_temp , "abrechnungsjahr" , NULL , years_absent )
  RegDatafunc <- RegData_temp
  detach("package:dplyr")
  return(RegDatafunc)
}







getSpecificConsumptionCologne <- function(bezirk_data , wetter_ja_nein) {
  
  if (wetter_ja_nein) {
    stop("Get Cologne weather data before proceeding.")
  }
  
  if (!wetter_ja_nein) {
    spz_verbrauch_mean <- createRegData01(bezirk_data)
  }
  return(spz_verbrauch_mean)
}