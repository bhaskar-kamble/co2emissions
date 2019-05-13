find_proportions <- function(obj,drop_cols) {# from the file BerlinPresentationCO2BalanceUnified_v6.Rmd
  obj <- obj[ , !(names(obj) %in% drop_cols)]
  obj <- obj / rowSums(obj)
  obj <- 100*obj
  obj$abrechnungsjahr <- 2002:2018
  return(obj)
} 