source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")

getMannheimAreas <-function() {

  mannheim_area <- read.table("D:/GITHUB_REPOS/co2emissions/Mannheim/Mannhein_Areas.txt.txt")
  
  names(mannheim_area) <- c("gebaeude_type" , "area_units" , "area" , "abrechnungsjahr")
  
  # https://www.datacamp.com/community/tutorials/long-wide-data-R
  
  library(reshape2)
  
  mannheim_area_wide <- dcast(mannheim_area , abrechnungsjahr ~ gebaeude_type , value.var = "area")
  
  names(mannheim_area_wide) <- c("abrechnungsjahr" , "eine_wohnung_area" , "zwei_wohnung_area" , "areaMFH")
  
  mannheim_area_wide$areaSFH <- mannheim_area_wide$eine_wohnung_area + mannheim_area_wide$zwei_wohnung_area
  
  mannheim_area_wide <- mannheim_area_wide[ , c("abrechnungsjahr" , "areaSFH" , "areaMFH")]
  
  mannheim_areas <- mannheim_area_wide
  
  not_in_2002_2018 <- (2002:2018)[!((2002:2018) %in% mannheim_areas$abrechnungsjahr)]
  
  mannheim_areas <- appendLinearTrend(mannheim_areas , "abrechnungsjahr" , NULL , not_in_2002_2018)
  
  # areas are in square meters
  
  return(mannheim_areas)
  
}