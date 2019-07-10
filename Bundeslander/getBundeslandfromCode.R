getBundeslandfromCode <- function(obj) { #obj is either DL_MFH, or DL_SFH, or a subset or a union of these, which in turn are created by getGermanyData.R
  #assigns bundesland to those cases where only the digit(s) for the bundesland is given.
  obj$bundesland[obj$bundesland=="1"]  <- "Baden-Württemberg"
  obj$bundesland[obj$bundesland=="2"]  <- "Bayern"
  obj$bundesland[obj$bundesland=="3"]  <- "Berlin"
  obj$bundesland[obj$bundesland=="4"]  <- "Brandenburg"
  obj$bundesland[obj$bundesland=="5"]  <- "Bremen"
  obj$bundesland[obj$bundesland=="6"]  <- "Hamburg"
  obj$bundesland[obj$bundesland=="7"]  <- "Hessen"
  obj$bundesland[obj$bundesland=="8"]  <- "Mecklenburg-Vorpommern"
  obj$bundesland[obj$bundesland=="9"]  <- "Niedersachsen"
  obj$bundesland[obj$bundesland=="10"]  <- "Nordrhein-Westfalen"
  obj$bundesland[obj$bundesland=="11"]  <- "Rheinland-Pfalz"
  obj$bundesland[obj$bundesland=="12"]  <- "Saarland"
  obj$bundesland[obj$bundesland=="13"]  <- "Sachsen"
  obj$bundesland[obj$bundesland=="14"]  <- "Sachsen-Anhalt"
  obj$bundesland[obj$bundesland=="15"]  <- "Schleswig-Holstein"
  obj$bundesland[obj$bundesland=="16"]  <- "Thüringen"

  
  return(obj)
}