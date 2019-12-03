linearizer <- function(obj,dropCols,xVar) {
  obj_new <- obj[ , !(names(obj) %in% dropCols)]
  feature_list <- names(obj_new)[ !(names(obj_new) %in% c(dropCols,xVar)) ]
  storage <- list()
  for (feature in feature_list) {
    storage[[feature]] <- lm(get(feature) ~ get(xVar) , data = obj)
    obj_new[[feature]] <- as.numeric(predict(storage[[feature]] , newdata = obj))
  }
  return(obj_new)
}
