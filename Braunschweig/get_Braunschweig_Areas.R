get_Braunschweig_Areas <- function() {
  area_Braunschweig <- data.frame(abrechnungsjahr = 2002:2018)
  area_Braunschweig$areaSFH <- 39932*100
  area_Braunschweig$areaMFH <- 68652*100
  return(area_Braunschweig)
}