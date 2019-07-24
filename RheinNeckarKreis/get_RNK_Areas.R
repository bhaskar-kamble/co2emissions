source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")

get_RNK_Areas <- function() {
  
  RNK_area <- read.csv2("D:/GITHUB_REPOS/co2emissions/RheinNeckarKreis/RheinNeckarAreas.csv",stringsAsFactors = FALSE)
  
  #> unique(RNK_area$Merkmal)
  #[1] "Wohnfläche"
  #> unique(RNK_area$Attribut)
  #[1] "Wohnfläche insgesamt" "Wohngebäude mit 3 und mehr Wohnungen"
  
  year_names <- paste0("X",2010:2017)
  
  for (year in year_names) {
    RNK_area[[year]] <- gsub("\\." , "" , RNK_area[[year]])
    RNK_area[[year]] <- as.numeric(RNK_area[[year]])
  }
  
  
  
  area_total <- RNK_area[RNK_area$Attribut=="Wohnfläche insgesamt" , ]
  area_MFH <- RNK_area[RNK_area$Attribut=="Wohngebäude mit 3 und mehr Wohnungen" , ]
  area_total <- area_total[ , year_names]
  area_MFH <- area_MFH[ , year_names]
  area_total <- as.numeric(colSums(area_total))
  area_MFH <- as.numeric(colSums(area_MFH))
  
  area_RNK <- data.frame(abrechnungsjahr = 2010:2017 , area_total = area_total , areaMFH = area_MFH)
  area_RNK$areaSFH <- area_RNK$area_total - area_RNK$areaMFH
  
  area_RNK <- area_RNK[ , c("abrechnungsjahr" , "areaSFH" , "areaMFH")]
  
  area_RNK <- appendLinearTrend(obj = area_RNK,
                                xVar = "abrechningsjahr",
                                dropCols = NULL,
                                missingValues = (2002:2018)[!(2002:2018 %in% 2010:2017)])
  
  return(area_RNK)
  
}