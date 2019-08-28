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

#bezirk_path_list <- paste0("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/",bezirk_number_list,"_",bezirk_list,"/")

################################################################################
areas_mfh_allebezirke <- data.frame(abrechnungsjahr = 2002:2018)
areas_sfh_allebezirke <- data.frame(abrechnungsjahr = 2002:2018)
co2_mfh_allebezirke <- data.frame(abrechnungsjahr = 2002:2018)
co2_sfh_allebezirke <- data.frame(abrechnungsjahr = 2002:2018)
################################################################################


for (i in 1:12) {



  #bezirk_number <- bezirk_number_list[i]
  bezirk <- bezirk_list[i]
  #bezirk_path <- bezirk_path_list[i]
  
  
  et_list <- c("erdgas","waerme","fluessiggas","heizoel","holzpellets","strom")
  
  
  source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/mainScriptCO2Emissions_v2.R")
  return_SFH <- main_function("SFH" , bezirk , et_list)
  return_MFH <- main_function("MFH" , bezirk , et_list)
  co2_SFH <- return_SFH$co2_emissions 
  co2_MFH <- return_MFH$co2_emissions 
  co2_SFH$strom[co2_SFH$abrechnungsjahr < 2010] <- mean(co2_SFH$strom[co2_SFH$abrechnungsjahr >2009])
  co2_MFH$strom[co2_MFH$abrechnungsjahr < 2010] <- mean(co2_MFH$strom[co2_MFH$abrechnungsjahr >2009])
  co2_ALL <- co2_SFH + co2_MFH
  co2_ALL$abrechnungsjahr <- 2002:2018
  co2_SFH <- co2_SFH/1e7
  co2_MFH <- co2_MFH/1e7
  co2_ALL <- co2_ALL/1e7
  co2_SFH$abrechnungsjahr <- 2002:2018
  co2_MFH$abrechnungsjahr <- 2002:2018
  co2_ALL$abrechnungsjahr <- 2002:2018
  
  et_list <- names(co2_SFH)[!(names(co2_SFH) %in% c("abrechnungsjahr","total"))]
  
  
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
  
  
  
  co2_SFH_lin <- linearizer(co2_SFH , dropCols = "total" , xVar = "abrechnungsjahr")
  co2_SFH_lin <- capZero(co2_SFH_lin , dropCols = "abrechnungsjahr")
  co2_MFH_lin <- linearizer(co2_MFH , dropCols = "total" , xVar = "abrechnungsjahr")
  co2_MFH_lin <- capZero(co2_MFH_lin , dropCols = "abrechnungsjahr")
  co2_ALL_lin <- linearizer(co2_ALL , dropCols = "total" , xVar = "abrechnungsjahr")
  co2_ALL_lin <- capZero(co2_ALL_lin , dropCols = "abrechnungsjahr")

  
  ##########################################################################  
  areas_mfh_allebezirke[[bezirk]] <- return_MFH$totalArea$areaMFH
  areas_sfh_allebezirke[[bezirk]] <- return_SFH$totalArea$areaSFH
  co2_mfh_allebezirke[[bezirk]] <- co2_MFH$total
  co2_sfh_allebezirke[[bezirk]] <- co2_SFH$total
  ##########################################################################
  
  
  

  
}