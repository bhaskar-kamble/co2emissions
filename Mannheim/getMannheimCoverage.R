source("D:/GITHUB_REPOS/co2emissions/Mannheim/getMannheimData.R")

gtype <- "MFH"
mannheim_data <- getMannheimData(gtype)
mannheim_data$verbrauch_gesamt_kwh_spez <- mannheim_data$verbrauch_gesamt_kwh_spez/1.2
if (gtype=="SFH") {
  cap_value <- 400.0
}
if (gtype=="MFH") {
  cap_value <- 350.0
}
mannheim_data_mfh <- mannheim_data[(mannheim_data$verbrauch_gesamt_kwh_spez < cap_value)&(mannheim_data$verbrauch_gesamt_kwh_spez > 15.0) , ]


gtype <- "SFH"
mannheim_data <- getMannheimData(gtype)
mannheim_data$verbrauch_gesamt_kwh_spez <- mannheim_data$verbrauch_gesamt_kwh_spez/1.2
if (gtype=="SFH") {
  cap_value <- 400.0
}
if (gtype=="MFH") {
  cap_value <- 350.0
}
mannheim_data_sfh <- mannheim_data[(mannheim_data$verbrauch_gesamt_kwh_spez < cap_value)&(mannheim_data$verbrauch_gesamt_kwh_spez > 15.0) , ]


mannheim_data_mfh <- mannheim_data_mfh[, c("abrechnungsjahr" , "gebaeude_nutzflaeche")]
mannheim_data_sfh <- mannheim_data_sfh[, c("abrechnungsjahr" , "gebaeude_nutzflaeche")]

summary(mannheim_data_mfh$gebaeude_nutzflaeche)
summary(mannheim_data_sfh$gebaeude_nutzflaeche)

par(mar=c(1.8,1.8,1.8,1.8))
boxplot(mannheim_data_mfh$gebaeude_nutzflaeche)


######################

# Seem to be many outliers in Mannheim areas
# upper limits for area:
mfh_upper_limit <- 1500
sfh_upper_limit <- 300

mannheim_data_mfh <- mannheim_data_mfh[mannheim_data_mfh$gebaeude_nutzflaeche < mfh_upper_limit , ]
mannheim_data_sfh <- mannheim_data_sfh[mannheim_data_sfh$gebaeude_nutzflaeche < sfh_upper_limit , ]

library(dplyr)
by_jahr <- group_by(mannheim_data_mfh , abrechnungsjahr)
sum_area_mfh <- as.data.frame(summarize(by_jahr , sum(gebaeude_nutzflaeche)))
by_jahr <- group_by(mannheim_data_sfh , abrechnungsjahr)
sum_area_sfh <- as.data.frame(summarize(by_jahr , sum(gebaeude_nutzflaeche)))
names(sum_area_mfh) <- c("abrechnungsjahr" , "sum_area")
names(sum_area_sfh) <- c("abrechnungsjahr" , "sum_area")

mfh_count <- as.data.frame(table(mannheim_data_mfh$abrechnungsjahr))
sfh_count <- as.data.frame(table(mannheim_data_sfh$abrechnungsjahr))
names(mfh_count) <- c("abrechnungsjahr" , "counts")
names(sfh_count) <- c("abrechnungsjahr" , "counts")

mannheim_area_counts <- data.frame(abrechnungsjahr=2002:2018 , mfh_area = sum_area_mfh$sum_area , mfh_count = mfh_count$counts , 
                                       sfh_area = sum_area_sfh$sum_area , sfh_count = sfh_count$counts)

write.csv2(mannheim_area_counts , file="mannheim_coverage_counts.csv",row.names = FALSE)
