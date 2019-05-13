remove_outliers <- function(datframe , colums) {
  #takes in a dataframe <datframe> and returns the dataframe after removing rows containing outliers in the 
  #all the columns specifed in the vector "colums"
  for (colum in colums) {
    q25 <- as.numeric(quantile(datframe[[colum]] , probs = 0.25 ))
    q75 <- as.numeric(quantile(datframe[[colum]] , probs = 0.75 ))
    IQR <- q75 - q25
    left_fence  <- q25 - 1.5*IQR
    right_fence <- q75 + 1.5*IQR
    is_outlier <- (datframe[[colum]] < left_fence)|(datframe[[colum]] > right_fence)
    datframe <- datframe[!is_outlier   ,  ]
  }
  return(datframe)  
}