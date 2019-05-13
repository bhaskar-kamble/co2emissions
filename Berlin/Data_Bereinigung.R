spez consumption : upper limit is 400 kwh, lower limit is 15kwh
berlinmfh_noout <- berlin_mfh[(berlin_mfh$verbrauch_gesamt_kwh_spez > 15)&(berlin_mfh$verbrauch_gesamt_kwh_spez < 400) , ]
#######################################################################

subset_data_by_region <- function(inputdata , region) {
  outputdata <- inputdata[inputdata$Landkreis_von_GS == region , ]
  return(outputdata)
}

setwd("D:/GITHUB_REPOS/co2emissions/Berlin/")
load("../MFH20022018_v2.RData")
MFH20022018$gtype <- "MFH"

weather_data <- read.table("berlin_wetter_tegel.txt",header=TRUE)
names(weather_data) <- c("wind","sun","bedeckung","temperatur")
weather_data$abrechnungsjahr <- 2002:2018

berlin_mfh <- subset_data_by_region(MFH20022018,"Berlin, Stadt")

berlin_mfh$energietraeger[berlin_mfh$energietraeger=="fernwaerme"] <- "waerme"
berlin_mfh$energietraeger[berlin_mfh$energietraeger=="nahwaerme"] <- "waerme"
berlin_mfh$energietraeger[berlin_mfh$energietraeger=="waermepumpe"] <- "strom"
berlin_mfh$energietraeger[berlin_mfh$energietraeger=="waermepumpe_luft"] <- "strom"
berlin_mfh$energietraeger[berlin_mfh$energietraeger=="waermepumpe_wasser"] <- "strom"
et_list <- find_et_list(berlin_mfh)

berlin_mfh <- berlin_mfh[(berlin_mfh$verbrauch_gesamt_kwh_spez > 15)&(berlin_mfh$verbrauch_gesamt_kwh_spez < 400) , ]
berlin2016 <- berlin_mfh[berlin_mfh$abrechnungsjahr == 2016 , ]
berlin2017 <- berlin_mfh[berlin_mfh$abrechnungsjahr == 2017 , ]
berlin2018 <- berlin_mfh[berlin_mfh$abrechnungsjahr == 2018 , ]

#replace2018 by trend
replaceall2018trend <- function(obj) {
  obj_temp <- obj[obj$abrechnungsjahr!=2018 , ]
  var_list <- names(obj)[names(obj)!="abrechnungsjahr"]
  storage <- list()
  data2018 <- obj[obj$abrechnungsjahr==2018 , ]
  #obj_new <- obj
  for (varname in var_list) {
    storage[[varname]] <- lm(get(varname) ~ abrechnungsjahr , data = obj_temp)
    data2018[[varname]] <- as.numeric(predict(storage[[varname]] , newdata = data2018))
  }
  obj_new <- rbind(obj_temp , data2018)
  return(obj_new)
}









createRegData <- function(hev_data,weather_data) {
  library(dplyr)
  #averages according to abrechnungsjahr:
  by_AbJahr <- group_by(hev_data,abrechnungsjahr)
  RegData_temp <- as.data.frame(summarize(by_AbJahr,mean(gebaeude_baujahr),mean(gebaeude_nutzflaeche),mean(verbrauch_gesamt_kwh_spez)))
  names(RegData_temp) <- c("abrechnungsjahr","mean_baujahr","mean_nutzflaeche","mean_spzverbrauch")
  
  RegData_temp <- merge(RegData_temp,weather_data,by.x="abrechnungsjahr",by.y="abrechnungsjahr") #not using cbind
  #as we dont know if all the years are present in RegData_temp
  #
  #now u hv 2 introduce abjahr_index
  abjahr_temp <- data.frame(abrechnungsjahr=as.numeric(c(2002:2018)) , abjahr_index = as.numeric(c(0:16)))
  RegDatafunc <- merge(RegData_temp , abjahr_temp , by.x = "abrechnungsjahr" , by.y = "abrechnungsjahr")
  detach("package:dplyr")
  return(RegDatafunc)
}



RegDataBereinigung_v2 <- function(RD) {
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
  
  
  return(RD)
  
}





RegData <- createRegData(berlin_mfh , weather_data)

RegData <- replaceall2018trend(RegData)

RegData <- cbind(RegData[ , c("abrechnungsjahr", "mean_baujahr", "mean_nutzflaeche", "mean_spzverbrauch")] , weather_data[ , c(
  "wind","sun","bedeckung","temperatur")])

RegData$abjahr_index <- 0:16

RegData <- RegDataBereinigung_v2(RegData)