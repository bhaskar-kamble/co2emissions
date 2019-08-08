source("D:/GITHUB_REPOS/co2emissions/RheinNeckarKreis/getRegionData.R")

source("D:/GITHUB_REPOS/visualization-project2-smurfs/cleanData.R")

source("D:/GITHUB_REPOS/co2emissions/Konstanz/get_Konstanz_Areas.R")

area_konstanz <- get_Konstanz_Areas()
areaSFH2018 <- area_konstanz$areaSFH[area_konstanz$abrechnungsjahr==2018]
areaMFH2018 <- area_konstanz$areaMFH[area_konstanz$abrechnungsjahr==2018]

region <- "Konstanz"

#coverage for MFH
gtype <- "MFH"
region_data <- getRegionData(gtype,region)
region_data <- cleanData(region_data , gtype)
coverage_mfh <- sum(region_data$gebaeude_nutzflaeche)/areaMFH2018


#coverage for SFH
gtype <- "SFH"
region_data <- getRegionData(gtype,region)
region_data <- cleanData(region_data , gtype)
coverage_sfh <- sum(region_data$gebaeude_nutzflaeche)/areaSFH2018

coverage_mfh

coverage_sfh