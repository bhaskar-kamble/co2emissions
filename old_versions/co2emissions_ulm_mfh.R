setwd("/home/kbhaskar/CO2ONLINE/co2emissions_github")
load("MFH20022018.RData")
MFH20022018$gtype <- "MFH"



find_ET_shares_by_year <- function(inputdata) {
  require(dplyr)
  by_year_ET <- group_by(inputdata,abrechnungsjahr,energietraeger)
  return(as.data.frame(summarize(by_year_ET,sum(verbrauch_gesamt_kwh))))
}

find_et_list <- function(inputdata) {
  ET_list <- unique(inputdata$energietraeger)  
  return(ET_list)
}
subset_data_by_region <- function(inputdata , region) {
  outputdata <- inputdata[inputdata$Landkreis_von_GS == region , ]
  return(outputdata)
}
readweather <- function(filename) {
  weather_data <- read.csv(filename,header=TRUE , sep=";" , dec="," )
  names(weather_data) <- c("Year","wind","sun","bedeckung","temperatur")
  no_of_years <- 17
  if (dim(weather_data)[1] != no_of_years) {
    stop("check number of years in weather data")
  }
  return(weather_data)
}



weather_ulm <- readweather("ulm_weather.csv")
ulm_mfh <- subset_data_by_region(MFH20022018,"Ulm")
ulm_mfh$energietraeger[ulm_mfh$energietraeger=="fernwaerme"] <- "waerme"
ulm_mfh$energietraeger[ulm_mfh$energietraeger=="nahwaerme"] <- "waerme"
et_list <- find_et_list(ulm_mfh)


ET_shares_by_year <- find_ET_shares_by_year(ulm_mfh)
names(ET_shares_by_year) <- c("abrechnungsjahr" ,"energietraeger" ,"sum_verbrauch_gesamt_kwh")
detach(package:dplyr)
library(reshape2)
ET_shares_by_year_wide <- dcast(ET_shares_by_year , abrechnungsjahr ~ energietraeger , value.var = "sum_verbrauch_gesamt_kwh")
ET_shares_by_year_wide[is.na(ET_shares_by_year_wide)]=0.0
drop_cols <- "abrechnungsjahr"
ET_shares_by_year_wide$Yearlysum <- rowSums(ET_shares_by_year_wide[ , !(names(ET_shares_by_year_wide) %in% drop_cols)])
ET_shares_by_year_wide
# here is the problem: there is no data for 2017. How will you make the graph? Just skip it? or fill 
# it with means?



drop_cols <- c("abrechnungsjahr","Yearlysum")
final_prop_table <- as.data.frame(sapply(ET_shares_by_year_wide[ , !(names(ET_shares_by_year_wide) %in% drop_cols)],function(obj) obj/ET_shares_by_year_wide$Yearlysum))
final_prop_table$abrechnungsjahr <- 2002:2018
#above gives error - change it to ET_shares_by_year_wide$abrechnungsjahr, see Rmd file

xtabs(~abrechnungsjahr+energietraeger , data=ulm_mfh)

df1 <- data.frame(cat1=rep(c("a","b"),times=2) , cat2 = rep(c("c","d"),each=2))


