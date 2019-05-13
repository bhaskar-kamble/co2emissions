source("find_proportions.R")
area_proportions_by_et <- function(bezirk_data) {
  require(dplyr)
  by_year_ET <- group_by(bezirk_data,abrechnungsjahr,energietraeger)
  return_data <- as.data.frame(summarize(by_year_ET,sum(gebaeude_nutzflaeche)))
  names(return_data) <- c("abrechnungsjahr" ,"energietraeger" ,"sum_gebaeude_nutzflaeche")
  detach(package:dplyr)
  require(reshape2)
  return_data <- dcast(return_data , abrechnungsjahr ~ energietraeger , value.var = "sum_gebaeude_nutzflaeche")
  return_data[is.na(return_data)]=0.0
  return_data <- find_proportions(return_data , "abrechnungsjahr")
  return(return_data)
}