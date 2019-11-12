convert_kilo_to_megaton <- function(obj , colsToDrop) {
  colsToRetain = names(obj)[ !(names(obj) %in%  colsToDrop)]
  for (varName in colsToRetain) {
    obj[[varName]] <- 1e-3*obj[[varName]]
  }
  return(obj)
}

