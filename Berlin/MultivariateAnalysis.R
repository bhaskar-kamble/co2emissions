#Based on the file DL_SFH.R from the folder D:/R/ForDropbox/BhaskarBerechnung/01Deutschland/1.1.1.SFH/1.1.1.HEV
weatherDL <- readweather()
RegData <- createRegData(sfh_subset,weatherDL)
RegData <- RegDataBereinigung(RegData)

#########

readweather <- function() {
  weatherDL <- read.csv("DeutschlandWeatherData.csv",header=TRUE , sep=";" , dec="," )
  names(weatherDL) <- c("wind","sun","bedeckung","temperatur")
  no_of_years <- 16
  if (dim(weatherDL)[1] != no_of_years) {
    stop("check number of years in weather data")
  }
  return(weatherDL)
}

#########

createRegData <- function(hev_data,weather_data) {
  library(dplyr)
  #averages according to abrechnungsjahr:
  by_AbJahr <- group_by(hev_data,abrechnungsjahr)
  RegData_temp <- as.data.frame(summarize(by_AbJahr,mean(gebaeude_baujahr),mean(gebaeude_nutzflaeche),mean(Verbrauch)))
  names(RegData_temp) <- c("abrechnungsjahr","mean_baujahr","mean_nutzflaeche","mean_verbrauch")
  
  weather_data$year <- as.numeric(c(2002:2017))
  RegData_temp <- merge(RegData_temp,weather_data,by.x="abrechnungsjahr",by.y="year") #not using cbind
  #as we dont know if all the years are present in RegData_temp
  #
  #now u hv 2 introduce abjahr_index
  abjahr_temp <- data.frame(year=as.numeric(c(2002:2017)) , abjahr_index = as.numeric(c(0:15)))
  RegDatafunc <- merge(RegData_temp , abjahr_temp , by.x = "abrechnungsjahr" , by.y = "year")
  detach("package:dplyr")
  return(RegDatafunc)
}


##########

RegDataBereinigung <- function(RD) {
  ## Step 1.
  ### 1a. Creating the regression model
  FitRegData <- lm(RD$mean_verbrauch ~ RD$abjahr_index+RD$mean_baujahr+RD$mean_nutzflaeche+RD$wind+RD$sun+RD$temperatur)
  
  ### 1b. Predict using the regression model
  RD <- cbind(RD,predict(FitRegData, newdata = RD))
  names(RD) <- c("abrechnungsjahr","mean_baujahr","mean_nutzflaeche","mean_verbrauch","wind","sun","bedeckung","temperatur","abjahr_index","pred1")
  #The predictions aer stored in the `pred1` column.
  
  ### 1c. Difference between the predicted and the actual SV
  RD$diff1 <- RD$pred1 - RD$mean_verbrauch
  
  ## Step 2. 
  ### 2a. Regression model after replacing the `baujahr` and `nutzflaeche` by the average
  FitRegData2 <- lm(RD$mean_verbrauch ~ RD$abjahr_index+RD$wind+RD$sun+RD$temperatur)
  
  ### 2b. Predict using this model
  RD <- cbind(RD,predict(FitRegData2, newdata = RD))
  names(RD) <- c("abrechnungsjahr","mean_baujahr","mean_nutzflaeche","mean_verbrauch","wind","sun","bedeckung","temperatur","abjahr_index","pred1","diff1","pred2")
  
  ### 2c. Difference between this predicted and the actual SV
  RD$diff2 <- RD$pred2 - RD$mean_verbrauch
  
  ## Step 3. Bereinigung
  ### 3a. Prediction of model 1 minus prediction of model 2
  RD$pred1minuspred2 <- RD$pred1 - RD$pred2
  
  ### 3b. Bereinigung
  RD$verbrauch_bereinigt <- RD$mean_verbrauch - RD$pred1minuspred2
  #names(RegData)[4] <- "Rohdaten"                 #mean_verbrauch
  names(RD)[names(RD)=="mean_verbrauch"] <- "Rohdaten"
  #names(RegData)[15]   <- "BereinigteDaten"       #verbrauch_bereinigt
  names(RD)[names(RD)=="verbrauch_bereinigt"] <- "BereinigteDaten"
  RD$Rohdaten <- RD$Rohdaten/1.2
  RD$BereinigteDaten <- RD$BereinigteDaten/1.2
  return(RD)
}
