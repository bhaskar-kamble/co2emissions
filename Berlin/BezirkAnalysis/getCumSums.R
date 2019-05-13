getCumSums <- function(obj , dropCols) {
  obj_temp <- obj [ , !(names(obj) %in% dropCols)  ]
  obj_temp <- as.data.frame(t(apply(obj_temp , 1 , cumsum)))
  obj_temp$abrechnungsjahr <- 2002:2018
  return(obj_temp)
}