getBerlinBezirkPopulation <- function() {
  bezirk_population <- read.csv2("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/PopulationBezirke/BerlinBezirkPopulation.csv",stringsAsFactors = FALSE,fileEncoding="WINDOWS-1252")
  names(bezirk_population) <- c("bezirk",2002:2018)
  #converting data from wide to long: http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
  # look at the reshape2 option. id.vars has to be bezirk
  require(reshape2)
  bezirk_population <- melt(bezirk_population,id.vars = "bezirk")
  #convert $variable and $value to numeric
  bezirk_population$variable <- as.character(bezirk_population$variable)
  bezirk_population$variable <- as.numeric(bezirk_population$variable)
  bezirk_population$value <- gsub("\\.","",bezirk_population$value)
  bezirk_population$value <- as.numeric(bezirk_population$value)
  names(bezirk_population) <- c("bezirk","abrechnungsjahr","population")
  
  bezirk_population$bezirk[ bezirk_population$bezirk=="Berlin-Mitte"] <- "mitte"
  bezirk_population$bezirk[ bezirk_population$bezirk=="Charlottenburg Wilmersdorf"] <- "charlottenburg_wilmersdorf"
  bezirk_population$bezirk[ bezirk_population$bezirk=="Friedrichshain - Kreuzberg"] <- "friedrichshain_kreuzberg"
  bezirk_population$bezirk[ bezirk_population$bezirk=="Lichtenberg"] <- "lichtenberg"
  bezirk_population$bezirk[ bezirk_population$bezirk=="Marzahn-Hellersdorf"] <- "marzahn_hellersdorf"
  bezirk_population$bezirk[ bezirk_population$bezirk=="Neukölln"] <- "neukoelln"
  bezirk_population$bezirk[ bezirk_population$bezirk=="Pankow"] <- "pankow"
  bezirk_population$bezirk[ bezirk_population$bezirk=="Reinickendorf"] <- "reinickendorf"
  bezirk_population$bezirk[ bezirk_population$bezirk=="Spandau"] <- "spandau"
  bezirk_population$bezirk[ bezirk_population$bezirk=="Steglitz Zehlendorf"] <- "steglitz_zehlendorf"
  bezirk_population$bezirk[ bezirk_population$bezirk=="Tempelhof-Schöneberg"] <- "tempelhof_schoeneberg"
  bezirk_population$bezirk[ bezirk_population$bezirk=="Treptow-Köpenick"] <- "treptow_koepenick"
  return(dcast(bezirk_population , abrechnungsjahr~bezirk,value.var = "population"))
}
