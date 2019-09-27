changeCO2_to_CO_2 <- function(obj) {
  objBefore <- strsplit(obj,"CO2")[[1]][1]
  objAfter <- strsplit(obj,"CO2")[[1]][2]
  return(bquote(.(objBefore)*CO[2] * .(objAfter)))
}