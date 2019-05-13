setwd("D:/GITHUB_REPOS/co2emissions")
load("MFH20022018_v2.RData")
load("SFH20022018_v2.RData")
#In SFH20022018, abrechnungsjahr and verbrauch_gesamt_kwh are character - change then to int and numeric
SFH20022018$abrechnungsjahr <- as.integer(SFH20022018$abrechnungsjahr)
SFH20022018$verbrauch_gesamt_kwh <- gsub("," , "." , SFH20022018$verbrauch_gesamt_kwh)
SFH20022018$verbrauch_gesamt_kwh <- as.numeric(SFH20022018$verbrauch_gesamt_kwh)
SFH20022018$gtype <- "SFH"
MFH20022018$gtype <- "MFH"

berlinSFH <- SFH20022018[SFH20022018$Landkreis_von_GS=="Berlin, Stadt"     ,  ]
berlinSFH <- berlinSFH[!is.na(berlinSFH$gebaeude_baujahr)   ,  ]
berlinSFH <- berlinSFH[!is.na(berlinSFH$gebaeude_nutzflaeche)   ,  ]
berlinSFH <- berlinSFH[!is.na(berlinSFH$verbrauch_gesamt_kwh_spez)   ,  ]


berlinMFH <- MFH20022018[MFH20022018$Landkreis_von_GS=="Berlin, Stadt"     ,  ]
berlinMFH <- berlinMFH[!is.na(berlinMFH$gebaeude_baujahr)   ,  ]
berlinMFH <- berlinMFH[!is.na(berlinMFH$gebaeude_nutzflaeche)   ,  ]
berlinMFH <- berlinMFH[!is.na(berlinMFH$verbrauch_gesamt_kwh_spez)   ,  ]


names(berlinMFH)
table(berlinMFH$abrechnungsjahr)




library(dplyr)
by_jahr_mfh <- group_by(berlinMFH , abrechnungsjahr)
berlin_stats_mfh <- as.data.frame(summarize(by_jahr_mfh,mean(gebaeude_baujahr),mean(gebaeude_nutzflaeche),mean(verbrauch_gesamt_kwh_spez)))
names(berlin_stats_mfh) <- c("abrechnungsjahr" , "baujahr" , "nutzflaeche" , "SV")
write.csv2(berlin_stats_mfh , file="./Berlin/berlin_MFH.csv",row.names = FALSE)

by_jahr_sfh <- group_by(berlinSFH , abrechnungsjahr)
berlin_stats_sfh <- as.data.frame(summarize(by_jahr_sfh,mean(gebaeude_baujahr),mean(gebaeude_nutzflaeche),mean(verbrauch_gesamt_kwh_spez)))
names(berlin_stats_sfh) <- c("abrechnungsjahr" , "baujahr" , "nutzflaeche" , "SV")
write.csv2(berlin_stats_sfh , file="./Berlin/berlin_SFH.csv",row.names = FALSE)