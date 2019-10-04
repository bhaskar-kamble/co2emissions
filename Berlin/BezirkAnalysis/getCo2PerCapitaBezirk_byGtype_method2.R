getBerlinWohnungen <- function() {
  
  source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/appendLinearTrend.R")
  source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getRowSums.R")
  
  bezirk_wohnungen <- read.csv2("D:/GITHUB_REPOS/co2emissions/Berlin/FindArea/areas_berlin_bezirke.csv")
  
  bezirk_wohnungen_all <- bezirk_wohnungen[ , c(    "abrechnungsjahr",
                                                    "bezirk",
                                                    "anzahl_wohnungen_insgesamt")]
  bezirk_wohnungen_1FH <- bezirk_wohnungen[ , c(    "abrechnungsjahr",
                                                    "bezirk",
                                                    "anzahl_gebaeude_1FH")]
  bezirk_wohnungen_2FH <- bezirk_wohnungen[ , c(    "abrechnungsjahr",
                                                    "bezirk",
                                                    "anzahl_wohnungen_2FH")]
  bezirk_wohnungen_MFH <- bezirk_wohnungen[ , c(    "abrechnungsjahr",
                                                    "bezirk",
                                                    "anzahl_wohnungen_MFH")]
  
  
  
  return_data <- list()
  return_data$ALL <- bezirk_wohnungen_all
  return_data$IFH   <- bezirk_wohnungen_1FH
  return_data$IIFH  <- bezirk_wohnungen_2FH
  return_data$MFH   <- bezirk_wohnungen_MFH
  
  require(reshape2)
  return_data$ALL <- dcast(return_data$ALL, abrechnungsjahr ~ bezirk , value.var = "anzahl_wohnungen_insgesamt")
  return_data$IFH   <- dcast(return_data$IFH, abrechnungsjahr ~ bezirk , value.var = "anzahl_gebaeude_1FH")
  return_data$IIFH <- dcast(return_data$IIFH, abrechnungsjahr ~ bezirk , value.var = "anzahl_wohnungen_2FH")
  return_data$MFH <- dcast(return_data$MFH, abrechnungsjahr ~ bezirk , value.var = "anzahl_wohnungen_MFH")
  
  not_in_2002_2018 <- (2002:2018)[!((2002:2018) %in% bezirk_wohnungen$abrechnungsjahr)]
  return_data$ALL <- appendLinearTrend(return_data$ALL , 
                                       xVar = "abrechnungsjahr",
                                       dropCols = NULL,
                                       missingValues = not_in_2002_2018)
  return_data$IFH <- appendLinearTrend(return_data$IFH , 
                                       xVar = "abrechnungsjahr",
                                       dropCols = NULL,
                                       missingValues = not_in_2002_2018)
  return_data$IIFH <- appendLinearTrend(return_data$IIFH , 
                                        xVar = "abrechnungsjahr",
                                        dropCols = NULL,
                                        missingValues = not_in_2002_2018)
  return_data$MFH <- appendLinearTrend(return_data$MFH , 
                                       xVar = "abrechnungsjahr",
                                       dropCols = NULL,
                                       missingValues = not_in_2002_2018)
  
  return_data$ALL <- getRowSums(return_data$ALL , dropCols = "abrechnungsjahr")
  return_data$IFH <- getRowSums(return_data$IFH , dropCols = "abrechnungsjahr")
  return_data$IIFH <- getRowSums(return_data$IIFH , dropCols = "abrechnungsjahr")
  return_data$MFH <- getRowSums(return_data$MFH , dropCols = "abrechnungsjahr")
  
  
  return(return_data)
}




getPopulationFromHaushalt2014 <- function(p1=5.5,
                                          p2=5.5,
                                          p3=5.5,
                                          p4=5.5) {
  haushalt2014 <- read.csv2("D:/GITHUB_REPOS/co2emissions/Berlin/berlin_haushalte_2014.csv" , stringsAsFactors = FALSE)
  haushalt2014 <- haushalt2014[haushalt2014$Haushalte !=  "haushalte_total" ,]
  # multiply with 1000 all the numbers
  haushalt2014[,c("ALL","IFH","IIFH","MFH")] <- 1000*haushalt2014[,c("ALL","IFH","IIFH","MFH")]
  persons_number <- as.data.frame(matrix(rep(1:5,4),ncol=4))
  persons_number[5 , ] <- c(p1,p2,p3,p4)
  names(persons_number) <- c("ALL","IFH","IIFH","MFH")
  
  haushalt2014[,c("ALL","IFH","IIFH","MFH")] <- persons_number*haushalt2014[,c("ALL","IFH","IIFH","MFH")]
  haushalt2014$ALL <- haushalt2014$IFH + haushalt2014$IIFH + haushalt2014$MFH
  
  berlin_population_2014 <- haushalt2014
  names(berlin_population_2014) <- c("population","ALL","IFH","IIFH","MFH")
  berlin_population_2014$population <-  paste0("population_in_" , berlin_population_2014$population)
  berlin_population_2014$SFH <- berlin_population_2014$IFH + berlin_population_2014$IIFH
  
  return(berlin_population_2014)
}





getPopulationBezirk_byGtype <- function(gtype) {

  berlin_wohnungen_bygtype <- getBerlinWohnungen()
  berlin_wohnungen_bygtype$SFH <- berlin_wohnungen_bygtype$IFH + berlin_wohnungen_bygtype$IIFH
  berlin_wohnungen_bygtype$SFH$abrechnungsjahr <- 2002:2018

  #get number of gtype-flats in berlin in 2014:
  berlin_wohnungen_gtype <- berlin_wohnungen_bygtype[[gtype]]
  berlin_wohnungen_gtype <- berlin_wohnungen_gtype[ , c("abrechnungsjahr","total")]
  # above is the total number of flats of gtype in berlin for all years
  berlin_wohnungen_gtype_2014 <- berlin_wohnungen_gtype[berlin_wohnungen_gtype$abrechnungsjahr==2014 , ]
  berlin_wohnungen_gtype_2014 <- berlin_wohnungen_gtype_2014$total
  # the above gives the 2014 value of number of flats in berlin of type gtype
  
  #Find gtype-population of berlin in 2014:
  berlin_population_gtype_2014 <- getPopulationFromHaushalt2014()
  berlin_population_gtype_2014 <- berlin_population_gtype_2014[ , c("population",gtype)]
  berlin_population_gtype_2014 <- sum(berlin_population_gtype_2014[[gtype]])
  # the above gives the 2014 population of berlin in building type gtype.
  
  ppl_per_flat_gtype <- berlin_population_gtype_2014/berlin_wohnungen_gtype_2014
  
  population_bybezirk_gtype <- ppl_per_flat_gtype * berlin_wohnungen_bygtype[[gtype]]
  population_bybezirk_gtype$abrechnungsjahr <- 2002:2018
  return(population_bybezirk_gtype)
}

getCo2PerCapitaBezirk_byGtype_method2 <- function(obj,gtype) {#obj is data frame of co2 emissions 
                                                            #of all bezirke in Berlin and also their sum
  if (!(gtype %in% c("SFH","MFH"))) {
    stop("give valid gtype. options are: MFH or SFH")
  } 
  
  pop_bybezirk_gtype <- getPopulationBezirk_byGtype(gtype)
  co2_perhead_bybezirk_gtype <- 1e6*obj/pop_bybezirk_gtype #this is what you want...
  co2_perhead_bybezirk_gtype$abrechnungsjahr <- 2002:2018
  return(co2_perhead_bybezirk_gtype)
}