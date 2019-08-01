source("D:/GITHUB_REPOS/visualization-project2-smurfs/cleanData.R")
source("D:/GITHUB_REPOS/visualization-project2-smurfs/getGermanyData_v2.R")
source("D:/GITHUB_REPOS/co2emissions/Bundeslander/getBundeslandfromCode.R")
source("D:/GITHUB_REPOS/visualization-project2-smurfs/getBundeslandfromLK.R")

subset_data_by_region <- function(inputdata , region) {
  outputdata <- inputdata[inputdata$bundesland == region , ]
  return(outputdata)
}


getBundeslandData <- function(gtype,bundesland) {

  if (gtype=="MFH") {  
    DL_MFH <- getGermanyData(gtype = "MFH")
    #remove outliers:
    DL_MFH <- cleanData(DL_MFH , "MFH")
    #find bundesland
    DL_data <- getBundeslandfromCode(DL_MFH)
  }
  
  
  
  if (gtype=="SFH") {    
    DL_SFH <- getGermanyData(gtype = "SFH")
    DL_SFH <- cleanData(DL_SFH , "SFH")
    DL_SFH <- getBundeslandfromCode(DL_SFH)
    DL_SFH <- DL_SFH[!is.na(DL_SFH$bundesland) , ]
    DL_data <- getBundeslandfromLK(DL_SFH)
  }  
  
  return_data <- subset_data_by_region(DL_data,bundesland)
  
}