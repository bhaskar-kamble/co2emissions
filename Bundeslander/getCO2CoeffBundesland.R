getCO2CoeffBundesland <- function(bundesland) {

  warning("using Berlin co2 coefficients for now")
    
  co2_coeff <- read.table("D:/GITHUB_REPOS/co2emissions/Berlin/berlin_co2_coefficients.txt",header=TRUE)
  
  #co2_coeff <- co2_coeff[ , c("abrechnungsjahr","erdgas","fernwaerme","fluessiggas","heizoel","holzpellets","strom")]
  names(co2_coeff) <- c("abrechnungsjahr","erdgas","waerme","fluessiggas","heizoel","holzpellets","strom")
  co2_coeff <- co2_coeff[co2_coeff$abrechnungsjahr %in% 2002:2018 , ]
  
  return(co2_coeff)
}