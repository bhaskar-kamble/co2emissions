setwd("D:/GITHUB_REPOS/co2emissions/Berlin/FindArea")
extracted_data <- read.csv("GebaeudeWohnungenExtracted.csv",sep=";")
extracted_data <- as.data.frame(t(extracted_data))
extracted_data <- extracted_data[ 2:nrow(extracted_data) , ]
rownames(extracted_data) <- NULL
extracted_data$abrechnungsjahr <- 2002:2018
names(extracted_data) <- c("anzahl_sfh_gebaeude","anzahl_mfh_gebaeude",
                           "anzahl_sfh_wohnungen","anzahl_mfh_wohnungen",
                           "Wohnflaeche","abrechnungsjahr")
extracted_data
##### THIS DOES NOT WORK!!! WHY???? #######################################################
extracted_data <- as.data.frame(sapply(extracted_data , function(obj) as.character(obj)))
###########################################################################################

for (colname in names(extracted_data)) {
  extracted_data[[colname]] <- as.character(extracted_data[[colname]])
}

extracted_data
str(extracted_data)

extracted_data <- as.data.frame(sapply(extracted_data , function(obj) gsub("\\.","",obj)))
extracted_data <- as.data.frame(sapply(extracted_data , function(obj) gsub(" ","",obj)))
extracted_data
for (colname in names(extracted_data)) {
  extracted_data[[colname]] <- as.character(extracted_data[[colname]])
}

extracted_data <- as.data.frame(sapply(extracted_data , function(obj) as.numeric(obj)))

extracted_data <- extracted_data[ , c("abrechnungsjahr" , names(extracted_data)[names(extracted_data)!="abrechnungsjahr"])]


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

extracted_data <- replaceall2018trend(extracted_data)
require(ggplot2)
ggplot() + geom_point(data=extracted_data , aes(x=abrechnungsjahr,y=anzahl_mfh_gebaeude)
)+ylim(0,max(extracted_data$anzahl_mfh_gebaeude))+geom_smooth(method="lm",data=
extracted_data,aes(x=abrechnungsjahr,y=anzahl_mfh_gebaeude))

ggplot() + geom_point(data=extracted_data , aes(x=abrechnungsjahr,y=anzahl_sfh_gebaeude)
)+ylim(0,max(extracted_data$anzahl_sfh_gebaeude))+geom_smooth(method="lm",data=
extracted_data,aes(x=abrechnungsjahr,y=anzahl_sfh_gebaeude))

ggplot() + geom_point(data=extracted_data , aes(x=abrechnungsjahr,y=anzahl_mfh_wohnungen)
)+ylim(0,max(extracted_data$anzahl_mfh_wohnungen))+geom_smooth(method="lm",data=
extracted_data,aes(x=abrechnungsjahr,y=anzahl_mfh_wohnungen))

ggplot() + geom_point(data=extracted_data , aes(x=abrechnungsjahr,y=anzahl_sfh_wohnungen)
)+ylim(0,max(extracted_data$anzahl_sfh_wohnungen))+geom_smooth(method="lm",data=
extracted_data,aes(x=abrechnungsjahr,y=anzahl_sfh_wohnungen))

ggplot() + geom_point(data=extracted_data , aes(x=abrechnungsjahr,y=Wohnflaeche)
)+ylim(0,max(extracted_data$Wohnflaeche))+geom_smooth(method="lm",data=
extracted_data,aes(x=abrechnungsjahr,y=Wohnflaeche))










#########################################################################
setwd("D:/GITHUB_REPOS/co2emissions/Berlin/")
#load("../MFH20022018_v2.RData")
load("../SFH20022018_v2.RData")
#In SFH20022018, abrechnungsjahr and verbrauch_gesamt_kwh are character - change then to int and numeric
SFH20022018$abrechnungsjahr <- as.integer(SFH20022018$abrechnungsjahr)
SFH20022018$verbrauch_gesamt_kwh <- gsub("," , "." , SFH20022018$verbrauch_gesamt_kwh)
SFH20022018$verbrauch_gesamt_kwh <- as.numeric(SFH20022018$verbrauch_gesamt_kwh)
SFH20022018$gtype <- "SFH"

subset_data_by_region <- function(inputdata , region) {
  outputdata <- inputdata[inputdata$Landkreis_von_GS == region , ]
  return(outputdata)
}

berlin_sfh <- subset_data_by_region(SFH20022018,"Berlin, Stadt")

berlin_sfh$energietraeger[berlin_sfh$energietraeger=="fernwaerme"] <- "waerme"
berlin_sfh$energietraeger[berlin_sfh$energietraeger=="nahwaerme"] <- "waerme"
berlin_sfh$energietraeger[berlin_sfh$energietraeger=="waermepumpe"] <- "strom"
berlin_sfh$energietraeger[berlin_sfh$energietraeger=="waermepumpe_luft"] <- "strom"
berlin_sfh$energietraeger[berlin_sfh$energietraeger=="waermepumpe_wasser"] <- "strom"

#########################################################################
setwd("D:/GITHUB_REPOS/co2emissions/Berlin/")
load("../MFH20022018_v2.RData")
MFH20022018$gtype <- "MFH"
subset_data_by_region <- function(inputdata , region) {
  outputdata <- inputdata[inputdata$Landkreis_von_GS == region , ]
  return(outputdata)
}
berlin_mfh <- subset_data_by_region(MFH20022018,"Berlin, Stadt")
berlin_mfh <- berlin_mfh[(berlin_mfh$verbrauch_gesamt_kwh_spez > 15)&(berlin_mfh$verbrauch_gesamt_kwh_spez < 400) , ]
berlin_mfh$energietraeger[berlin_mfh$energietraeger=="fernwaerme"] <- "waerme"
berlin_mfh$energietraeger[berlin_mfh$energietraeger=="nahwaerme"] <- "waerme"
berlin_mfh$energietraeger[berlin_mfh$energietraeger=="waermepumpe"] <- "strom"
berlin_mfh$energietraeger[berlin_mfh$energietraeger=="waermepumpe_luft"] <- "strom"
berlin_mfh$energietraeger[berlin_mfh$energietraeger=="waermepumpe_wasser"] <- "strom"
##########################################################################



library(dplyr)
by_year <- group_by(berlin_mfh , abrechnungsjahr)
mfh_by_year <- as.data.frame(summarize(by_year , mean(gebaeude_nutzflaeche)))
names(mfh_by_year) <- c("abrechnungsjahr","nutzflaeche")
by_year <- group_by(berlin_sfh , abrechnungsjahr)
sfh_by_year <- as.data.frame(summarize(by_year , mean(gebaeude_nutzflaeche)))
names(sfh_by_year) <- c("abrechnungsjahr","nutzflaeche")

mfh_by_year <- replaceall2018trend(mfh_by_year)
sfh_by_year <- replaceall2018trend(sfh_by_year)

ggplot() + geom_point(data=mfh_by_year , aes(x=abrechnungsjahr,y=nutzflaeche)
)+ylim(0,max(mfh_by_year$nutzflaeche))+geom_smooth(method="lm",data=
mfh_by_year,aes(x=abrechnungsjahr,y=nutzflaeche))

ggplot() + geom_point(data=sfh_by_year , aes(x=abrechnungsjahr,y=nutzflaeche)
)+ylim(0,max(sfh_by_year$nutzflaeche))+geom_smooth(method="lm",data=
sfh_by_year,aes(x=abrechnungsjahr,y=nutzflaeche))

##################################################################

# there are two methods of finding the area breakup of sfh andmfh buildings
# method1: find avg area of sfh and mfh buildings and multiply with anzahl of buildings (or wohnungen?)
# method2: find the percentage of areas for each year and multiply with the total areas which you have from the 
# official figures
#Method1
names(mfh_by_year) <- c("abrechnungsjahr","mfh_nutzflaeche")
names(sfh_by_year) <- c("abrechnungsjahr","sfh_nutzflaeche")
cbind(mfh_by_year , sfh_by_year)
mean_nutzflaeche <- as.data.frame(cbind(mfh_by_year$abrechnungsjahr , mfh_by_year$mfh_nutzflaeche , sfh_by_year$sfh_nutzflaeche))
names(mean_nutzflaeche) <- c("abrechnungsjahr","mfh","sfh")


area_estimates <- data.frame(abrechnungsjahr=2002:2018 , 
                             SFH_geb = mean_nutzflaeche$sfh*extracted_data$anzahl_sfh_gebaeude ,
                             MFH_geb = mean_nutzflaeche$mfh*extracted_data$anzahl_mfh_gebaeude ,
                             SFH_woh = mean_nutzflaeche$sfh*extracted_data$anzahl_sfh_wohnungen,
                             MFH_woh = mean_nutzflaeche$mfh*extracted_data$anzahl_mfh_wohnungen)

area_estimates