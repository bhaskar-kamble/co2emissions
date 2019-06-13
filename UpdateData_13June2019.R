Data_13June2019 <- read.csv2("D:/GITHUB_REPOS/co2emissions/Johannes Export ab 01.08.2018 - Update 02.05.2019_20190502_075013.csv")

names(Data_13June2019)

unique(Data_13June2019$gdb_abrechnung.abrech_jahr)

gsub("a","",c("!awe","tzcaw","faaaag"))

names(Data_13June2019) <- gsub("gdb_abrechnung.","",names(Data_13June2019)) 
names(Data_13June2019) <- gsub("gdb_berechnung.","",names(Data_13June2019))
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