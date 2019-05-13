berlin_mfh_counts <- read.csv2("./data/presentations/mfh/berlin_mfh_counts.csv")

berlin_sfh_counts <- read.csv2("./data/presentations/sfh/berlin_sfh_counts.csv")

berlin_mfh_areas <- read.csv2("./data/presentations/mfh/berlin_mfh_area_sums.csv")


berlin_sfh_areas <- read.csv2("./data/presentations/sfh/berlin_sfh_area_sums.csv")

is_not_year <- names(berlin_mfh_counts)!= "abrechnungsjahr"

anzahl_flaeche_berlin <- berlin_mfh_counts
anzahl_flaeche_berlin$sfh_counts <- berlin_sfh_counts$sfh_counts
anzahl_flaeche_berlin$mfh_area <- berlin_mfh_areas$sum_nutzflaeche
anzahl_flaeche_berlin$sfh_area <- berlin_sfh_areas$sum_nutzflaeche
anzahl_flaeche_berlin <- anzahl_flaeche_berlin[,names(anzahl_flaeche_berlin)!="X"]
write.csv2(anzahl_flaeche_berlin , "./data/presentations/anzahl_flaeche_berlin.csv",row.names=FALSE)