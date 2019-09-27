# Line 1901 of Berlin_LeistungsbildTeil1_v7.Rmd.
# Line 2439 of Berlin_LeistungsbildTeil1_v7.Rmd.

#get the 2014 values of no. of SFH and MFH wohnungen in Berlin. Extract the 2014 value.
#D:/GITHUB_REPOS/co2emissions/Berlin/FindArea/areas_berlin_bezirke.csv. 
#D:/GITHUB_REPOS/co2emissions /Berlin/BezirkAnalysis/getBezirkAreas.R
#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getBezirkPopulations_by_gtype.R")

getBezirkPopulations_by_gtype <- function() {
  
  getBerlinWohnungen2014 <- function() {
    
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
  
  # 2014 Haushalte split by the building type in Berlin. This gives the population in berlin living in SFH and MFH flats.
  # D:/GITHUB_REPOS/co2emissions/Berlin/SB_F01-02-00_2014j04_BE.pdf
  # This I have saved as file:///D:/GITHUB_REPOS/co2emissions/Berlin/berlin_haushalte_2014.csv
  getPopulationFromHaushalt2014 <- function(p1,p2,p3,p4) {
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
    
    return(berlin_population_2014)
  }
  
  #From the above you get what fraction lives in 1-2FH and what fraction lives in MFH.
  berlin_pops_2014 <- getPopulationFromHaushalt2014(6,8,7,6)
  berlin_pops_2014 <- berlin_pops_2014[ , c("IFH","IIFH","MFH")]
  berlin_pops_2014 <- as.data.frame(lapply(berlin_pops_2014, sum))
  #rowSums(berlin_pops_2014) is the total berlin population according to this
  
  # The fraction of people living in SFH and MFH:
  frac_SFH <-   (berlin_pops_2014$IFH + berlin_pops_2014$IIFH) /rowSums(berlin_pops_2014)
  frac_MFH <-   berlin_pops_2014$MFH /rowSums(berlin_pops_2014)
  #c(frac_SFH , frac_MFH)
  #Actual value of berlin population in 2014: 3565998
  
  
  
  # total population
  # D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/PopulationBezirke/BerlinBezirkPopulation.csv
  # The population you can find as follows:
  # -----------------
  source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getBerlinBezirkPopulation.R")
  source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getRowSums.R")
  bezirk_population <- getBerlinBezirkPopulation()
  bezirk_population <- getRowSums(bezirk_population , dropCols = "abrechnungsjahr")
  #Extract the 2014 value from the above.
  # -----------------
  
  bezirk_population_SFH <- frac_SFH * bezirk_population
  bezirk_population_SFH$abrechnungsjahr <- 2002:2018
  
  bezirk_population_MFH <- frac_MFH * bezirk_population
  bezirk_population_MFH$abrechnungsjahr <- 2002:2018
  
  bezirk_population_ALL <- bezirk_population
  
  return_data <- list()
  return_data$ALL <- bezirk_population_ALL
  return_data$SFH <- bezirk_population_SFH
  return_data$MFH <- bezirk_population_MFH
  
  return(return_data)
  
  
}

