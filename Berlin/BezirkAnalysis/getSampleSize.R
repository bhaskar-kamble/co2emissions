source("/home/kbhaskar/Github_Repos/visualization-project2-smurfs/cleanData.R")
source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/getBerlinData.R")
source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/getRowSums.R")

getSampleSize <- function() {
  
  return_object <- list()
  
  createTable <- function(gtype) {
    berlin_data <- getBerlinData(gtype)
    berlin_data <- cleanData(berlin_data , gtype)
    table_jahr_bezirk <- as.data.frame(xtabs(~abrechnungsjahr+bezirk,data=berlin_data))
    require(reshape2)
    table_jahr_bezirk <- dcast(table_jahr_bezirk , abrechnungsjahr ~ bezirk , value.var="Freq")
    table_jahr_bezirk <- getRowSums(table_jahr_bezirk , dropCols = "abrechnungsjahr")
    table_jahr_bezirk$abrechnungsjahr <- as.integer(as.character(table_jahr_bezirk$abrechnungsjahr))
    return(table_jahr_bezirk)
  }

  gtype <- "MFH"

  return_object$MFH <- createTable("MFH")
  
  gtype <- "SFH"
  
  return_object$SFH <- createTable("SFH")
  
  return_object$ALL <- return_object$MFH + return_object$SFH
  return_object$ALL$abrechnungsjahr <- 2002:2018
  
  return(return_object)
  
}
