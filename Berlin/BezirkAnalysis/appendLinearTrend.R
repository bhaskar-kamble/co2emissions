appendLinearTrend <- function(obj , xVar , dropCols , missingValues) {
  # A function to put missing values by linear trend - does it for all columns except xVar and dropCols
  # obj is the data frame, and "missingValues" is the list of x-values for which you calculate the y-values
  # xVar is the name of the x-variable in the data frame
  
  obj <- obj[ !(obj[[xVar]] %in% missingValues) , !(names(obj) %in% dropCols)]
  
  newData <- data.frame(placeholder_name = missingValues)
  names(newData) <- xVar
  
  yvars <- names(obj)[ !(names(obj) %in% c(xVar,dropCols))]
  for (yVar in yvars) {
    linMod <- lm(get(yVar) ~ get(xVar) , data = obj)
    newData[[yVar]] <- as.numeric(predict(linMod , newdata = newData))
  }
  
  newData <- newData[, names(obj)]
  newData <- rbind(obj,newData)
  newData <- newData[order(newData[[xVar]])  ,  ]
  return(newData)
}