#source("appendLinearTrend.R")
source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")

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

createRegData02 <- function(hev_data) {#with multivariate analysis for weather, based on BerlinPresentationCO2BalanceSFH_v6.R, line 530
  ##hev_data is the same as bezirk_data
  #weather_data <- read.csv2("D:/GITHUB_REPOS/co2emissions/Mannheim/mannheinWeather.csv")
  ##append linear trend
  #names(weather_data) <- c("abrechnungsjahr","wind","sun","bedeckung","temperatur")
  #not_in_2002_2018 <- (2002:2018)[!((2002:2018) %in% weather_data$abrechnungsjahr)]
  #weather_data <- appendLinearTrend(weather_data , "abrechnungsjahr" , NULL , not_in_2002_2018)
  #weather_data$abrechnungsjahr <- 2002:2018
  weather_data <- read.table("D:/GITHUB_REPOS/co2emissions/Berlin/berlin_wetter_tegel.txt",header=TRUE)
  names(weather_data) <- c("wind","sun","bedeckung","temperatur")
  weather_data$abrechnungsjahr <- 2002:2018  
  require(dplyr)
  by_AbJahr <- group_by(hev_data , abrechnungsjahr)  
  RegData_temp <- as.data.frame(summarize(by_AbJahr,
                                          mean(gebaeude_baujahr,na.rm=TRUE),
                                          mean(gebaeude_nutzflaeche,na.rm=TRUE),
                                          mean(verbrauch_gesamt_kwh_spez,na.rm=TRUE)))
  names(RegData_temp) <- c("abrechnungsjahr","mean_baujahr","mean_nutzflaeche","mean_spzverbrauch")
  years_absent <- (2002:2018)[!((2002:2018) %in% RegData_temp$abrechnungsjahr)]
  RegData_temp <- appendLinearTrend(RegData_temp , "abrechnungsjahr" , NULL , years_absent )
  RegData_temp <- merge(RegData_temp,weather_data,by.x="abrechnungsjahr",by.y="abrechnungsjahr")
  abjahr_temp <- data.frame(abrechnungsjahr=as.numeric(c(2002:2018)) , abjahr_index = as.numeric(c(0:16)))
  RegDatafunc <- merge(RegData_temp , abjahr_temp , by.x = "abrechnungsjahr" , by.y = "abrechnungsjahr")
  #RegDatafunc <- RegData_temp
  detach("package:dplyr")
  return(RegDatafunc)
}

RegDataBereinigung <- function(RD) {
  FitRegData <- lm(mean_spzverbrauch ~ abjahr_index + mean_baujahr + mean_nutzflaeche + 
                     wind + sun + temperatur + bedeckung  ,  data = RD)
  RD <- cbind(RD,predict(FitRegData, newdata = RD))
  names(RD) <- c("abrechnungsjahr","mean_baujahr","mean_nutzflaeche",
                 "mean_spzverbrauch","wind","sun","bedeckung","temperatur","abjahr_index","pred1")
  
  FitRegData2 <- lm(mean_spzverbrauch ~ abjahr_index + wind + sun + temperatur + bedeckung , data = RD)
  RD <- cbind(RD,predict(FitRegData2, newdata = RD))
  names(RD) <- c("abrechnungsjahr","mean_baujahr","mean_nutzflaeche",
                 "mean_spzverbrauch","wind","sun","bedeckung","temperatur","abjahr_index","pred1","pred2")
  
  RD$pred1minuspred2 <- RD$pred1 - RD$pred2
  
  RD$verbrauch_bereinigt <- RD$mean_spzverbrauch - RD$pred1minuspred2
  
  FitRegData3 <- lm(verbrauch_bereinigt ~ abjahr_index + wind + sun + temperatur + bedeckung ,  data = RD)
  
  RD <- cbind(RD,predict(FitRegData3, newdata = RD))
  names(RD) <- c("abrechnungsjahr","mean_baujahr","mean_nutzflaeche",
                 "mean_spzverbrauch","wind","sun","bedeckung","temperatur","abjahr_index",
                 "pred1","pred2","pred1minuspred2","verbrauch_bereinigt","pred3")
  
  RD$jahrsq <- RD$abjahr_index*RD$abjahr_index
  FitRegData4 <- lm(verbrauch_bereinigt ~ abjahr_index + jahrsq + wind + sun + temperatur + bedeckung ,  data = RD)
  RD <- cbind(RD,predict(FitRegData4, newdata = RD))
  names(RD) <- c("abrechnungsjahr","mean_baujahr","mean_nutzflaeche",
                 "mean_spzverbrauch","wind","sun","bedeckung","temperatur","abjahr_index",
                 "pred1","pred2","pred1minuspred2","verbrauch_bereinigt","pred3","jahrsq","pred4")
  RD <- RD[ , c("abrechnungsjahr","pred4")]
  names(RD) <- c("abrechnungsjahr","mean_spzverbrauch")
  return(RD)
}


getSpecificConsumptionMannheim <- function(bezirk_data , wetter_ja_nein) {
  
  if (wetter_ja_nein) {
    #weather_data <- read.csv2("D:/GITHUB_REPOS/co2emissions/Germany/wetterGermany.csv",header=TRUE)
    #names(weather_data) <- c("wind","sun","bedeckung","temperatur")
    
    weather_data <- read.csv2("D:/GITHUB_REPOS/co2emissions/Mannheim/mannheinWeather.csv")
    #append linear trend
    names(weather_data) <- c("abrechnungsjahr","wind","sun","bedeckung","temperatur")
    not_in_2002_2018 <- (2002:2018)[!((2002:2018) %in% weather_data$abrechnungsjahr)]
    weather_data <- appendLinearTrend(weather_data , "abrechnungsjahr" , NULL , not_in_2002_2018)
    #weather_data <- weather_data[ , names(weather_data)!="abrechnungsjahr"]
    
    RegData <- createRegData02(bezirk_data)
    RegData <- cbind(RegData[ , c("abrechnungsjahr", "mean_baujahr", "mean_nutzflaeche", "mean_spzverbrauch")] , 
                     weather_data[ , c("wind","sun","bedeckung","temperatur")])
    RegData$abjahr_index <- 0:16
    spz_verbrauch_mean <- RegDataBereinigung(RegData)
  }
  
  if (!wetter_ja_nein) {
    spz_verbrauch_mean <- createRegData01(bezirk_data)
  }
  return(spz_verbrauch_mean)
}