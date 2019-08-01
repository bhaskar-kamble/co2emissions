getCO2CoeffRNK <- function() {
  
  co2_coeff <- read.csv2("D:/GITHUB_REPOS/co2emissions/Mannheim/Mannheim_CO2_coefficients.csv")
  co2_coeff <- co2_coeff[co2_coeff$abrechnungsjahr %in% 2002:2018 , ]
  co2_coeff <- co2_coeff[ , c("abrechnungsjahr","erdgas","fernwaerme","fluessiggas","heizoel","holzpellets","strom")]
  names(co2_coeff) <- c("abrechnungsjahr","erdgas","waerme","fluessiggas","heizoel","holzpellets","strom")
  
  return(co2_coeff)
}