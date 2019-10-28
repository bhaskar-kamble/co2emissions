source("/home/kbhaskar/Github_Repos/visualization-project2-smurfs/cleanData.R")
source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/getBerlinData.R")
source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/getRowSums.R")

getBezirkSampleSizeByET <- function(bezirk) {
  
  return_object <- list()
  
  createTable <- function(gtype,bezirk) {
    berlin_data <- getBerlinData(gtype)
    berlin_data <- cleanData(berlin_data , gtype)
    bezirk_data <- berlin_data[ berlin_data$bezirk == bezirk , ]
    table_jahr_et <- as.data.frame(xtabs(~abrechnungsjahr+energietraeger,data=bezirk_data))
    require(reshape2)
    table_jahr_et <- dcast(table_jahr_et , abrechnungsjahr ~ energietraeger , value.var="Freq")
    table_jahr_et <- getRowSums(table_jahr_et , dropCols = "abrechnungsjahr")
    table_jahr_et$abrechnungsjahr <- as.integer(as.character(table_jahr_et$abrechnungsjahr))
    
    allYears <- 2002:2018
    missingYears <- allYears[!(allYears %in% table_jahr_et$abrechnungsjahr)]
    for (missingYear in missingYears) {
      rowTemp <- data.frame(abrechnungsjahr = missingYear)
      for (varName in names(table_jahr_et)[names(table_jahr_et)!="abrechnungsjahr"]) {
        rowTemp[[varName]] <- 0
      }
      table_jahr_et <- rbind(table_jahr_et , rowTemp)
    }
    
    table_jahr_et <- table_jahr_et[order(table_jahr_et$abrechnungsjahr) , ] #sort years
    
    list_of_et <- c("erdgas","waerme","fluessiggas","heizoel","holzpellets","strom")
    missingETs  <- list_of_et[!(list_of_et %in% names(table_jahr_et))]
    for (missingET in missingETs) {
      table_jahr_et[[missingET]] <- rep(0 , nrow(table_jahr_et))
    }
    
    table_jahr_et <- table_jahr_et[ , c("abrechnungsjahr" , list_of_et , "total")]
    
    return(table_jahr_et)
  }
  
  gtype <- "MFH"
  
  return_object$MFH <- createTable("MFH",bezirk)
  
  gtype <- "SFH"
  
  return_object$SFH <- createTable("SFH",bezirk)
  
  return_object$ALL <- return_object$MFH + return_object$SFH
  return_object$ALL$abrechnungsjahr <- 2002:2018
  
  return(return_object)
  
}
