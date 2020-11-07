#wik_2018_stk_bericht.pdf, Seite 2:

years <- 2008:2018
Ewohner <- c(1019328,
             1020303,
             1027504,
             1036117,
             1044555,
             1044070,
             1053528,
             1069192,
             1081701,
             1084795,
             1089984)



haushalte <- c(530429,
               531197,
               537017,
               542261,
               547221,
               545050,
               551024,
               557090,
               560298,
               561071,
               564260)

BvollkInHaushalten <- c(997594,
                        999035,
                        1007567,
                        1017190,
                        1025549,
                        1024202,
                        1033477,
                        1043249,
                        1052264,
                        1054612,
                        1060596)

HhaltsGrosse <- 1.88*rep(1,11)
HhaltsGrosse[c(5,8)] <- 1.87

Wohnungen <- c(535391,
               537666,
               539935,
               541890,
               544630,
               547553,
               550645,
               554018,
               556009,
               558038,
               561514)

WEIn12FH <- c(94205,
              94835,
              90779,
              91475,
              92136,
              92788,
              93563,
              94171,
              94575,
              94791,
              95220)

WflacheProEWohner <- c(37.55,
                       37.75,
                       39.54,
                       39.45,
                       39.40,
                       39.72,
                       39.67,
                       39.42,
                       39.14,
                       39.18,
                       39.25)

WflacheProWE <- c(71.48,
                  71.63,
                  75.25,
                  75.42,
                  75.57,
                  75.73,
                  75.90,
                  76.07,
                  76.14,
                  76.17,
                  76.19)


areas = data.frame(
  years = years,
  Ewohner = Ewohner,
  haushalte = haushalte,
  BvollkInHaushalten = BvollkInHaushalten,
  Wohnungen = Wohnungen,
  WEIn12FH = WEIn12FH,
  WflacheProEWohner = WflacheProEWohner,
  WflacheProWE = WflacheProWE
)

xVars <- c("Ewohner",
           "haushalte",
           "BvollkInHaushalten",
           "Wohnungen",
           "WEIn12FH",
           "WflacheProEWohner",
           "WflacheProWE")



require(ggplot2)
#yVar <- "Ewohner"
xVar <- xVars[2]
plotAreas <- ggplot()+geom_point(data=areas,aes(x=years,y=get(xVar)))+scale_y_continuous(lim=c(0,1.25*max(get(xVar))))  
plotAreas



#next you have to find the areas of MFH and 1-2FH flats.
#from esk data, find area of each flat, 1-2FH and MFH
#from above data, you have number of MFH and 1-2FH.

source("/home/kbhaskar/Github_Repos/co2emissions/RheinNeckarKreis/getRegionData.R")
source("/home/kbhaskar/Github_Repos/visualization-project2-smurfs/cleanData.R")
gtype <- "SFH"
region <- "KXln, Stadt"
region_data_sfh <- getRegionData(gtype,region)

gtype <- "MFH"
region <- "KXln, Stadt"
region_data_mfh <- getRegionData(gtype,region)

region_data_sfh <- region_data_sfh[region_data_sfh$abrechnungsjahr > 2000 , ]
region_data_sfh <- region_data_sfh[region_data_sfh$abrechnungsjahr <= 2018 , ]
# 1. Find the area per flat in each year
#dplyr, group_by, summarize
require(dplyr)
by_year <- group_by(region_data_sfh,abrechnungsjahr)
avgAreaSFH <- as.data.frame(summarize(by_year,mean(gebaeude_nutzflaeche)))
names(avgAreaSFH) <- c("abrechnungsjahr","meanArea")
#2019 is an outlier - remove it
ggplot()+geom_point(data=avgAreaSFH,aes(x=abrechnungsjahr,y=meanArea))+scale_y_continuous(lim=c(0,200))




table(region_data_mfh$abrechnungsjahr)
by_year <- group_by(region_data_mfh,abrechnungsjahr)
avgAreaMFH <- as.data.frame(summarize(by_year,mean(gebaeude_nutzflaeche)))
names(avgAreaMFH) <- c("abrechnungsjahr","meanArea")
ggplot()+geom_point(data=avgAreaMFH,aes(x=abrechnungsjahr,y=meanArea))+geom_smooth(method="lm")+scale_y_continuous(lim=c(0,3000))

ggplot(avgAreaMFH,aes(x=abrechnungsjahr,y=meanArea))+geom_point()+geom_smooth(method="lm")+scale_y_continuous(lim=c(0,3000))
#So now you can find the total areas of Köln. 

#make linear regression for the areas
lmMFH <- lm(meanArea~abrechnungsjahr , data = avgAreaMFH)
lmSFH <- lm(meanArea~abrechnungsjahr , data = avgAreaSFH)

avgAreaMFH$pred <- predict(lmMFH , avgAreaMFH)
avgAreaSFH$pred <- predict(lmSFH , avgAreaSFH)
require(reshape2)
avgAreaMFHLong <- melt(avgAreaMFH,id.vars="abrechnungsjahr")
ggplot(avgAreaMFHLong,aes(x=abrechnungsjahr,y=value,color=variable))+geom_point()+scale_y_continuous(lim=c(0,3000))

avgAreaSFHLong <- melt(avgAreaSFH,id.vars="abrechnungsjahr")
ggplot(avgAreaSFHLong,aes(x=abrechnungsjahr,y=value,color=variable))+geom_point()+scale_y_continuous(lim=c(0,200))

#so now you have area per flat. From the above data, you have the number of flats. It is then a simple matter of
#multiplying area per flat with the number of flats.

#when i take the average area from the co2online data, I guess I do it for each WE (Wohneigentum? Please confirm).
#So do I mutiply with the number of flats or with the number of eigentums? I also need the shares of 
#the MFH and 1-2FH.
#but form the official data I have the following: 
#1. no. Haushalte 
#2. no. wohnungen
#3. no. WE in 1-2FH
#4. Wohnfläche pro WE

#seems like the information is just not there