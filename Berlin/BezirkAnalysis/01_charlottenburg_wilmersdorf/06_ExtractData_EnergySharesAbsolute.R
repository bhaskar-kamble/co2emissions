#Calculates EnergyProportions

bezirk_number_list <- c(
  "01",
  "02",
  "03",
  "04",
  "05",
  "06",
  "07",
  "08",
  "09",
  "10",
  "11",
  "12"
)

bezirk_list  <- c(
  "charlottenburg_wilmersdorf",
  "friedrichshain_kreuzberg",
  "lichtenberg",
  "marzahn_hellersdorf",
  "mitte",
  "neukoelln",
  "pankow"    ,             
  "reinickendorf",
  "spandau",
  "steglitz_zehlendorf" ,
  "tempelhof_schoeneberg",
  "treptow_koepenick"                              
)


path_to_file <- "D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/CO2LinearTrendForBezirke/06_EnergySharesAbsolute/"

################################################################################

################################################################################


for (i in 1:12) {
  
  
  
  bezirk_number <- bezirk_number_list[i]
  bezirk <- bezirk_list[i]
  #bezirk_path <- bezirk_path_list[i]
  
  
  et_list <- c("erdgas","waerme","fluessiggas","heizoel","holzpellets","strom")
  
  
  source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/mainScriptCO2Emissions_v2.R")
  return_SFH <- main_function("SFH" , bezirk , et_list)
  return_MFH <- main_function("MFH" , bezirk , et_list)

  linearizer <- function(obj,dropCols,xVar) {
    obj_new <- obj[ , !(names(obj) %in% dropCols)]
    feature_list <- names(obj_new)[ !(names(obj_new) %in% c(dropCols,xVar)) ]
    storage <- list()
    for (feature in feature_list) {
      storage[[feature]] <- lm(get(feature) ~ get(xVar) , data = obj)
      obj_new[[feature]] <- as.numeric(predict(storage[[feature]] , newdata = obj))
    }
    return(obj_new)
  }
  
  #replace negative numbers with zero
  capZero <- function(obj,dropCols) {
    varNames <- names(obj)[!(names(obj) %in% dropCols)]
    for (var in varNames) {
      obj[[var]][obj[[var]] < 0] <- 0
    }
    return(obj)
  }
  
  ESA_SFH <- return_SFH$energy_shares_absolute
  ESA_MFH <- return_MFH$energy_shares_absolute
  ESA_SFH <- ESA_SFH[,c("abrechnungsjahr","erdgas","waerme","fluessiggas","heizoel","holzpellets","strom")]
  ESA_MFH <- ESA_MFH[,c("abrechnungsjahr","erdgas","waerme","fluessiggas","heizoel","holzpellets","strom")]
  
  ESA_SFH_lin <- linearizer(ESA_SFH , dropCols = "total" , xVar = "abrechnungsjahr")
  ESA_SFH_lin <- capZero(ESA_SFH_lin , dropCols = "abrechnungsjahr")
  ESA_MFH_lin <- linearizer(ESA_MFH , dropCols = "total" , xVar = "abrechnungsjahr")
  ESA_MFH_lin <- capZero(ESA_MFH_lin , dropCols = "abrechnungsjahr")
  
  file_name_sfh <- paste0("EnergySharesAbsolute_",bezirk_number,"_",bezirk,"SFH.csv")
  file_name_mfh <- paste0("EnergySharesAbsolute_",bezirk_number,"_",bezirk,"MFH.csv")
  
  write.csv2(ESA_SFH_lin,file=paste0(path_to_file , file_name_sfh) , row.names = FALSE)
  write.csv2(ESA_MFH_lin,file=paste0(path_to_file , file_name_mfh) , row.names = FALSE)
  
  
  
  
}