getRowSums <- function(obj , dropCols) {
  obj_temp <- obj[ , !(names(obj) %in% dropCols)]
  obj_temp$total <- rowSums(obj_temp)
  for (colName in dropCols) {
    obj_temp[[colName]] <- obj[[colName]]
  }
  obj_temp <- obj_temp[ , c(names(obj) , "total")]
  return(obj_temp)
}