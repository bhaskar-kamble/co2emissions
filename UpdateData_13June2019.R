Data_13June2019 <- read.csv2("D:/GITHUB_REPOS/co2emissions/Johannes Export ab 01.08.2018 - Update 02.05.2019_20190502_075013.csv",
                             stringsAsFactors = FALSE)

names(Data_13June2019)

unique(Data_13June2019$gdb_abrechnung.abrech_jahr)

gsub("a","",c("!awe","tzcaw","faaaag"))

names(Data_13June2019) <- gsub("gdb_abrechnung.","",names(Data_13June2019)) 
names(Data_13June2019) <- gsub("gdb_berechnung.","",names(Data_13June2019))
names(Data_13June2019) <- gsub("gdb_adresse.","",names(Data_13June2019))

#gebaeude_baujahr
#abrech_jahr
#sto_plz
#gebaeude_typ
#verbrauch_gesamt_kwh_spez
#Landkreis
#gebaeude_flaeche
#gebaeude_nutzflaeche
#energietraeger
#verbrauch_gesamt_kwh
#gdb_adresse.gemeindeschluessel_komplett

#load the earlier version of the MFH and SFH data:
load("D:/GITHUB_REPOS/co2emissions/MFH20022018_v2.RData")
load("D:/GITHUB_REPOS/co2emissions/SFH20022018_v2.RData")

#from the latest data, extract the relevant columns:
#> names(MFH20022018)
#"sto_plz"                -     called the same                
#"bundesland"             -     called the same
#"gebaeude_baujahr"       -     called the same
#"energietraeger"         -     called the same  
#"abrechnungsjahr"        -     called abrech_jahr
#"gebaeude_nutzflaeche"   -     called the same
#"verbrauch_gesamt_kwh"   -     called the same
#"verbrauch_gesamt_kwh_spez" -  called the same
#"Landkreis_von_GS"       -     have to extract from gemeindeschluessel
# Also, the gemeindeschluessel is called "gemeindeschluessel_komplett", from which the Landkreis has to be extracted.
#gebaeude_typ

reqd_features <- c( "sto_plz",
                    "bundesland",
                    "gebaeude_baujahr",
                    "energietraeger",           
                    "abrech_jahr",
                    "gebaeude_nutzflaeche",
                    "verbrauch_gesamt_kwh",
                    "verbrauch_gesamt_kwh_spez",
                    "gemeindeschluessel_komplett",
                    "gebaeude_typ")
new_data <- Data_13June2019[, reqd_features]
names(new_data)[names(new_data)=="gemeindeschluessel_komplett"] <- "gemeindeschluessel"
names(new_data)[names(new_data)=="abrech_jahr"] <- "abrechnungsjahr"



getLK <- function(obj) {
  #remove all those which are not valid integers:
  obj$gemeindeschluessel <- as.integer(obj$gemeindeschluessel)
  obj <- obj[!is.na(obj$gemeindeschluessel) , ]
  #convert to character:
  obj$gemeindeschluessel <- as.character(obj$gemeindeschluessel)
  #retain only those with 7 or 8 nchar
  is_less_than_7 <- nchar(obj$gemeindeschluessel) < 7
  obj <- obj[!is_less_than_7 , ]
  #append 0 to those with 7:
  is7 <- nchar(obj$gemeindeschluessel) == 7
  obj$GS8 <- obj$gemeindeschluessel
  obj$GS8[is7] <- paste0("0",obj$gemeindeschluessel[is7])
  obj$GS5 <- substr(obj$GS8,1,5)
  
  gslk <- read.csv("D:/GITHUB_REPOS/co2emissions/GS_LK_IMP.csv",header=TRUE , sep=";" , dec="," , stringsAsFactors=FALSE)
  gslk$Gemeindeschluessel <- as.character(gslk$Gemeindeschluessel)
  gslk$G8 <- gslk$Gemeindeschluessel
  is7 <- nchar(gslk$Gemeindeschluessel)==7
  gslk$G8[is7] <- paste("0",gslk$Gemeindeschluessel[is7],sep="")
  #leave off the last 3 digits of gemneindeschlussel
  gslk$G5 <- substr(gslk$G8,1,5)
  gslk <- gslk[,c("Landkreis","G5")]
  names(gslk) <- c("Landkreis_von_GS" , "G5")
  
  obj_new <- merge(obj , gslk , by.x="GS5" , by.y="G5")
  return(obj_new)
}

new_data <- getLK(new_data)

new_data$gebaeude_typ <- as.character(new_data$gebaeude_typ)

isMFH <- new_data$gebaeude_typ == "mehrfamilienhaus"
isSFH <- new_data$gebaeude_typ == "1-2familienhaus"

new_data$gebaeude_typ[isMFH] <- "MFH"
new_data$gebaeude_typ[isSFH] <- "SFH"


######## change data types ------------
new_data$gebaeude_nutzflaeche <- as.numeric(new_data$gebaeude_nutzflaeche)
new_data$verbrauch_gesamt_kwh <- as.numeric(new_data$verbrauch_gesamt_kwh)
new_data$verbrauch_gesamt_kwh_spez <- as.numeric(new_data$verbrauch_gesamt_kwh_spez)

sum(is.na(new_data$gebaeude_nutzflaeche))      #none
sum(is.na(new_data$verbrauch_gesamt_kwh))      #none
sum(is.na(new_data$verbrauch_gesamt_kwh_spez)) #none
sum(is.na(new_data$abrechnungsjahr))           #114

new_data <- new_data[!is.na(new_data$abrechnungsjahr) , ]

new_data_mfh <- new_data[new_data$gebaeude_typ=="MFH"   ,   ]
new_data_sfh <- new_data[new_data$gebaeude_typ=="SFH"   ,   ]

#now append to MFH2002018 and SFH2002018 data
#> names(SFH20022018)
#[1] "sto_plz"                   "bundesland"                "gebaeude_baujahr"          "energietraeger"           
#[5] "abrechnungsjahr"           "gebaeude_nutzflaeche"      "verbrauch_gesamt_kwh"      "verbrauch_gesamt_kwh_spez"
#[9] "Landkreis_von_GS"         
#> names(MFH20022018)
#[1] "sto_plz"                   "bundesland"                "gebaeude_baujahr"          "energietraeger"           
#[5] "abrechnungsjahr"           "gebaeude_nutzflaeche"      "verbrauch_gesamt_kwh"      "verbrauch_gesamt_kwh_spez"
#[9] "Landkreis_von_GS" 
##> names(new_data_mfh)
#[1] "GS5"                       "sto_plz"                   "bundesland"               
#[4] "gebaeude_baujahr"          "energietraeger"            "abrechnungsjahr"          
#[7] "gebaeude_nutzflaeche"      "verbrauch_gesamt_kwh"      "verbrauch_gesamt_kwh_spez"
#[10] "gemeindeschluessel"        "gebaeude_typ"              "GS8"                      
#[13] "Landkreis_von_GS" 
features_sfh_v2 <- names(SFH20022018)
features_mfh_v2 <- names(MFH20022018)
new_data_mfh <- new_data_mfh[ , features_mfh_v2]
new_data_sfh <- new_data_sfh[ , features_sfh_v2]

#> as.data.frame(sapply(MFH20022018 , class))
#sapply(MFH20022018, class)
#sto_plz                                    character
#bundesland                                 character
#gebaeude_baujahr                             integer
#energietraeger                             character
#abrechnungsjahr                              integer
#gebaeude_nutzflaeche                         numeric
#verbrauch_gesamt_kwh                         numeric
#verbrauch_gesamt_kwh_spez                    numeric
#Landkreis_von_GS                           character
#> as.data.frame(sapply(new_data_mfh , class))
#sapply(new_data_mfh, class)
#sto_plz                                     character
#bundesland                                    integer
#gebaeude_baujahr                              integer
#energietraeger                              character
#abrechnungsjahr                               integer
#gebaeude_nutzflaeche                          numeric
#verbrauch_gesamt_kwh                          numeric
#verbrauch_gesamt_kwh_spez                     numeric
#Landkreis_von_GS                            character
new_data_mfh$bundesland <- as.character(new_data_mfh$bundesland)



#> as.data.frame(sapply(SFH20022018 , class))
#sapply(SFH20022018, class)
#sto_plz                                    character
#bundesland                                 character
#gebaeude_baujahr                             integer
#energietraeger                             character
#abrechnungsjahr                            character
#gebaeude_nutzflaeche                         numeric
#verbrauch_gesamt_kwh                       character
#verbrauch_gesamt_kwh_spez                    numeric
#Landkreis_von_GS                           character
#> as.data.frame(sapply(new_data_sfh , class))
#sapply(new_data_sfh, class)
#sto_plz                                     character
#bundesland                                    integer
#gebaeude_baujahr                              integer
#energietraeger                              character
#abrechnungsjahr                               integer
#gebaeude_nutzflaeche                          numeric
#verbrauch_gesamt_kwh                          numeric
#verbrauch_gesamt_kwh_spez                     numeric
#Landkreis_von_GS                            character
new_data_sfh$bundesland <- as.character(new_data_sfh$bundesland)
new_data_sfh$abrechnungsjahr <- as.character(new_data_sfh$abrechnungsjahr)
new_data_sfh$verbrauch_gesamt_kwh <- as.character(new_data_sfh$verbrauch_gesamt_kwh)

MFH20022018 <- rbind(MFH20022018 , new_data_mfh)
SFH20022018 <- rbind(SFH20022018 , new_data_sfh)

# AND NOW, REMOVE DUPLICATES ------------------------------------
# THEN SAVE AS VERSION_3
remove_duplicates_by <- c("gebaeude_baujahr",
                          "energietraeger",
                          "abrechnungsjahr",
                          "gebaeude_nutzflaeche",
                          "verbrauch_gesamt_kwh",
                          "verbrauch_gesamt_kwh_spez",
                          "Landkreis_von_GS")


is_remove_duplicates_by <- which(names(MFH20022018) %in% remove_duplicates_by)


#file2018 <- both2018_v4[!duplicated(both2018_v4[ , is_remove_duplicates_by]) , ]