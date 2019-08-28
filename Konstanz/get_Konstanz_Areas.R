source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")

get_Konstanz_Areas <- function() {
  area_Konstanz <- read.table(
    "D:/GITHUB_REPOS/co2emissions/Konstanz/CO2 Konstanz 2002-2017_ExtractAreas.txt",
    header=TRUE)
  area_Konstanz$areaSFH <- area_Konstanz$Area1FH + area_Konstanz$Area2FH
  area_Konstanz <- area_Konstanz[ , c("Year" , "areaSFH" , "AreaMFH")]
  names(area_Konstanz) <- c("abrechnungsjahr" , "areaSFH" , "areaMFH")
  area_Konstanz <- appendLinearTrend(obj = area_Konstanz,
                                xVar = "abrechnungsjahr",
                                dropCols = NULL,
                                missingValues = (2002:2018)[!(2002:2018 %in% 2010:2017)])
  #areas are in meters-squared
  return(area_Konstanz)
}


