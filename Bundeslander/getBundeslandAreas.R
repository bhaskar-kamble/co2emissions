source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")

getBundeslandAreas <- function(b_land) {
  path <- "D:/GITHUB_REPOS/co2emissions/Bundeslander/"
  sfh_areas <- read.csv2(paste0(path,"SFHAreas_bundeslands.csv"),stringsAsFactors = FALSE)
  mfh_areas <- read.csv2(paste0(path,"MFHAreas_bundeslands.csv"),stringsAsFactors = FALSE)
  names(sfh_areas) <- c("abrechnungsjahr",
                        "Baden-Württemberg",
                        "Bayern",
                        "Berlin",
                        "Brandenburg",
                        "Bremen",
                        "Hamburg",
                        "Hessen",
                        "Mecklenburg-Vorpommern",
                        "Niedersachsen",
                        "Nordrhein-Westfalen",
                        "Rheinland-Pfalz",
                        "Saarland",
                        "Sachsen",
                        "Sachsen-Anhalt",
                        "Schleswig-Holstein",
                        "Thüringen")
  names(mfh_areas) <- names(sfh_areas)
  sfh_areas$abrechnungsjahr <- 2002:2018
  mfh_areas$abrechnungsjahr <- 2002:2018
  
  sfh_areas <- as.data.frame( sapply(sfh_areas , function(obj) gsub("\\." , "" , obj))  )
  for (Var in names(sfh_areas)) {
    sfh_areas[[Var]] <- as.numeric( as.character(sfh_areas[[Var]]) )
  }
  
  mfh_areas <- as.data.frame( sapply(mfh_areas , function(obj) gsub("\\." , "" , obj))  )
  for (Var in names(mfh_areas)) {
    mfh_areas[[Var]] <- as.numeric( as.character(mfh_areas[[Var]]) )
  }
  
  bland_mfh <- mfh_areas[ , c("abrechnungsjahr" , b_land)]
  names(bland_mfh) <- c("abrechnungsjahr" , "areaMFH")
  bland_sfh <- sfh_areas[ , c("abrechnungsjahr" , b_land)]
  names(bland_sfh) <- c("abrechnungsjahr" , "areaSFH")
  
  area_bland <- data.frame(abrechnungsjahr=2002:2018)
  area_bland$areaSFH <- bland_sfh$areaSFH
  area_bland$areaMFH <- bland_mfh$areaMFH
  
  area_bland <- 1000*area_bland
  area_bland$abrechnungsjahr <- 2002:2018
  
  return(area_bland)
}