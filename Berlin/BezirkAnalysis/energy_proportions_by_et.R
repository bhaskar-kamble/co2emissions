source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/find_proportions.R")
source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")
energy_proportions_by_et <- function(bezirk_data,et_list) {
  require(dplyr)
  by_year_ET <- group_by(bezirk_data,abrechnungsjahr,energietraeger)
  return_data <- as.data.frame(summarize(by_year_ET,sum(verbrauch_gesamt_kwh)))
  names(return_data) <- c("abrechnungsjahr" ,"energietraeger" ,"sum_verbrauch_gesamt_kwh")
  detach(package:dplyr)
  require(reshape2)
  return_data <- dcast(return_data , abrechnungsjahr ~ energietraeger , value.var = "sum_verbrauch_gesamt_kwh")
  return_data[is.na(return_data)]=0.0
  not_in_2002_2018 <- (2002:2018)[!((2002:2018) %in% return_data$abrechnungsjahr)]
  return_data <- appendLinearTrend(return_data , "abrechnungsjahr" , NULL , not_in_2002_2018)
  return_data <- find_proportions(return_data , "abrechnungsjahr")
  # Add the missing energietraegers:
  et_present <- names(return_data)[names(return_data) != "abrechnungsjahr"]
  et_absent <- et_list[!(et_list %in% et_present)]
  for (et_abs in et_absent) {
    return_data[[et_abs]] <- 0.0
  }
  return_data <- return_data[ , c(et_list , "abrechnungsjahr")]
  detach(package:reshape2)
  return(return_data)
}
