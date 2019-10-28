#source("appendLinearTrend.R")
source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")
# Bezirk Areas are in 100m-square.
getBezirkAreas <- function(bezirk) {
  #bezirk_areas <- read.csv2("../FindArea/areas_berlin_bezirke.csv")
  bezirk_areas <- read.csv2("/home/kbhaskar/Github_Repos/co2emissions/Berlin/FindArea/areas_berlin_bezirke.csv")
  bezirk_areas <- bezirk_areas[ , c("bezirk",
                                    "wohnflaeche_gebaeude_insgesamt",
                                    "wohnflaeche_gebaeude_1FH",
                                    "wohnflaeche_gebaeude_2FH",
                                    "wohnflaeche_gebaeude_MFH",
                                    "abrechnungsjahr")]
  
  bezirk_areas <- bezirk_areas[ , c("bezirk" , "wohnflaeche_gebaeude_1FH" , "wohnflaeche_gebaeude_2FH" , "wohnflaeche_gebaeude_MFH" , "abrechnungsjahr")]
  bezirk_areas$wohnflaeche_gebaeude_SFH <- bezirk_areas$wohnflaeche_gebaeude_1FH + bezirk_areas$wohnflaeche_gebaeude_2FH
  bezirk_areas <- bezirk_areas[ , c("abrechnungsjahr" , "bezirk" , "wohnflaeche_gebaeude_SFH" , "wohnflaeche_gebaeude_MFH")]
  names(bezirk_areas) <- c("abrechnungsjahr","bezirk","areaSFH","areaMFH")
  
  bezirk_areas <- bezirk_areas[bezirk_areas$bezirk==bezirk  ,  ]
  
  not_in_2002_2018 <- (2002:2018)[!((2002:2018) %in% bezirk_areas$abrechnungsjahr)]
  
  bezirk_areas <- appendLinearTrend(bezirk_areas , "abrechnungsjahr" , "bezirk" , not_in_2002_2018)
  return(bezirk_areas)
  
}
