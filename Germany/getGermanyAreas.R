source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")
getGermanyAreas <- function() {
  germany_areas <- read.csv2("D:/GITHUB_REPOS/co2emissions/Germany/Areas_SFH_MFH.csv") 
  names(germany_areas) <- c("abrechnungsjahr" , "areaSFH" , "areaMFH")
  not_in_2002_2018 <- (2002:2018)[!((2002:2018) %in% germany_areas$abrechnungsjahr)]
  germany_areas <- appendLinearTrend(germany_areas , "abrechnungsjahr" , NULL , not_in_2002_2018)
  germany_areas <- 0.01 * germany_areas
  germany_areas$abrechnungsjahr <- 2002:2018
  return(germany_areas)
}

# The areas in the csv file in the green column are in SQUARE METERS!!!
# The areas in the Berlin area files were in 100 square meters....
# to convert to units of 100 sq. m, multiply with 0.01