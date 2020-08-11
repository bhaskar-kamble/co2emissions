source("/home/kbhaskar/Github_Repos/co2emissions/RheinNeckarKreis/getRegionData.R")
source("/home/kbhaskar/Github_Repos/visualization-project2-smurfs/cleanData.R")

gtype <- "SFH"
region <- "KXln, Stadt"
region_data_sfh <- getRegionData(gtype,region)


gtype <- "MFH"
region <- "KXln, Stadt"
region_data_mfh <- getRegionData(gtype,region)


  
#> dim(region_data_mfh)
#[1] 7451   10
#> dim(region_data_sfh)
#[1] 15135    10