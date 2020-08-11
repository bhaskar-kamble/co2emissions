load("/home/kbhaskar/Github_Repos/co2emissions/SFH20022018_v3.RData")
SFH20022018$Landkreis_von_GS <- iconv(SFH20022018$Landkreis_von_GS, to = "UTF-8", sub = "X")


load("/home/kbhaskar/Github_Repos/co2emissions/MFH20022018_v3.RData")
MFH20022018$Landkreis_von_GS <- iconv(MFH20022018$Landkreis_von_GS, to = "UTF-8", sub = "X")


sort(unique(SFH20022018$Landkreis_von_GS))[grep("onn" , sort(unique(SFH20022018$Landkreis_von_GS)))]

sort(unique(MFH20022018$Landkreis_von_GS))[grep("onn" , sort(unique(MFH20022018$Landkreis_von_GS)))]

#region is "Bonn,Stadt"

#############################################

source("/home/kbhaskar/Github_Repos/co2emissions/RheinNeckarKreis/getRegionData.R")
source("/home/kbhaskar/Github_Repos/visualization-project2-smurfs/cleanData.R")

gtype <- "SFH"
region <- "Bonn, Stadt"
region_data_sfh <- getRegionData(gtype,region)


gtype <- "MFH"
region <- "Bonn, Stadt"
region_data_mfh <- getRegionData(gtype,region)

dim(region_data_sfh)
dim(region_data_mfh)
