getCO2Coeff <- function() {
  #co2_coeff <- read.table("../berlin_co2_coefficients.txt",header=TRUE)
  co2_coeff <- read.table("D:/GITHUB_REPOS/co2emissions/Berlin/berlin_co2_coefficients.txt",header=TRUE)
  names(co2_coeff) <- c("abrechnungsjahr","erdgas","waerme","fluessiggas","heizoel","holzpellets","strom")
  return(co2_coeff)
}