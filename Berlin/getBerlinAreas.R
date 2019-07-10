source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")

getBerlinAreas <-function() {
  
  #mannheim_area <- read.table("D:/GITHUB_REPOS/co2emissions/Mannheim/Mannhein_Areas.txt.txt")
  berlin_area <- read.table("D:/GITHUB_REPOS/co2emissions/Berlin/berlin_wohnflaeche.txt",header=TRUE)
  
  names(berlin_area) <- c("areaSFH","areaMFH")
  berlin_area <- 1000000*berlin_area
  berlin_area$abrechnungsjahr <- 2002:2018
  berlin_areas <- berlin_area[,c("abrechnungsjahr" , "areaSFH" , "areaMFH")]
  
  
  # areas are in square meters (i guess!)
  
  return(berlin_areas)
  
}