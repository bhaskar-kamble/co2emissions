setwd("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/")

source("getBerlinData.R"           )
#source("getBezirkData.R"           )
#source("energy_proportions_by_et.R" )
#source("appendLinearTrend.R"       )
#source("area_proportions_by_et.R"  )
#source("find_proportions.R"        )
#source("getBezirkAreas.R"        )
#source("getSpecificConsumption.R"   )
#source("getTotalConsumption.R"     )
#source("getAbsoluteEnergyShares.R"  )
#source("getCO2Coeff.R"             )
#source("getCO2Emissions.R"         )

berlin_sfh <- getBerlinData("SFH")
berlin_mfh <- getBerlinData("MFH")
plz_list_berlin <- unique(c(berlin_sfh$sto_plz  ,  berlin_mfh$sto_plz))

berlin_sfh <- berlin_sfh[ , c("sto_plz" , "energietraeger")]
berlin_mfh <- berlin_mfh[ , c("sto_plz" , "energietraeger")]
berlin_all <- rbind(berlin_sfh , berlin_mfh)

plz_et_sfh <- as.data.frame(xtabs(~ sto_plz + energietraeger , data = berlin_sfh))
plz_et_mfh <- as.data.frame(xtabs(~ sto_plz + energietraeger , data = berlin_mfh))
plz_et_all <- as.data.frame(xtabs(~ sto_plz + energietraeger , data = berlin_all))
plz_et_sfh$sto_plz <- as.integer(as.character(plz_et_sfh$sto_plz))
plz_et_mfh$sto_plz <- as.integer(as.character(plz_et_mfh$sto_plz))
plz_et_all$sto_plz <- as.integer(as.character(plz_et_all$sto_plz))

require(reshape2)
plz_et_sfh <- dcast(plz_et_sfh , sto_plz ~ energietraeger , value.var = "Freq")
plz_et_mfh <- dcast(plz_et_mfh , sto_plz ~ energietraeger , value.var = "Freq")
plz_et_all <- dcast(plz_et_all , sto_plz ~ energietraeger , value.var = "Freq")
detach(package:reshape2)

find_proportions <- function(obj,drop_cols) {# from the file BerlinPresentationCO2BalanceUnified_v6.Rmd
  obj_temp <- obj[ , !(names(obj) %in% drop_cols)]
  obj_temp <- obj_temp / rowSums(obj_temp)
  obj_temp <- 100*obj_temp
  for (colum in drop_cols) {
    obj_temp[[colum]] <- obj[[colum]]
  }
  obj_temp <- obj_temp[, names(obj)]
  return(obj_temp)
} 

plz_et_sfh_prop <- find_proportions(plz_et_sfh , "sto_plz")
plz_et_mfh_prop <- find_proportions(plz_et_mfh , "sto_plz")
plz_et_all_prop <- find_proportions(plz_et_all , "sto_plz")

names(plz_et_sfh_prop)[names(plz_et_sfh_prop)=="sto_plz"] <- "plz"
names(plz_et_mfh_prop)[names(plz_et_mfh_prop)=="sto_plz"] <- "plz"
names(plz_et_all_prop)[names(plz_et_all_prop)=="sto_plz"] <- "plz"

## Visualize with the help of a map
library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(ggplot2)

#plz_daten_DL <- read.csv("plz-5stellig-daten.csv")
#in_berlin <- plz_daten_DL$plz %in% plz_list_berlin
#plz_daten_berlin <- plz_daten_DL[ in_berlin , ]

#plz_et_sfh_prop <- merge(plz_et_sfh_prop , plz_daten_berlin , by.x = "plz" , by.y = "plz")
#plz_et_mfh_prop <- merge(plz_et_mfh_prop , plz_daten_berlin , by.x = "plz" , by.y = "plz")
#plz_et_all_prop <- merge(plz_et_all_prop , plz_daten_berlin , by.x = "plz" , by.y = "plz")

mymap <- st_read("D:/GITHUB_REPOS/co2emissions/Berlin/Postleitzahl/plz-5stellig.shp/plz-5stellig.shp",
                 stringsAsFactors = FALSE)
mymap$plz <- as.integer(mymap$plz)
mymap_berlin <- mymap[mymap$plz %in% plz_list_berlin , ]

# for ALL buildings
map_plz_et_all <- inner_join(mymap_berlin , plz_et_all_prop , by = "plz")
map_plz_et_mfh <- inner_join(mymap_berlin , plz_et_mfh_prop , by = "plz")
map_plz_et_sfh <- inner_join(mymap_berlin , plz_et_sfh_prop , by = "plz")

ggplot(map_plz_et_all) + theme(axis.ticks =  element_blank()) + geom_sf(aes(fill=waerme
                  )) + scale_fill_gradient(low="red" , high = "green")

#library(ggthemes)                        
#ggplot(map_plz_et_all)+ theme_map()  + geom_sf(aes(fill=waerme
#)) + scale_fill_gradient(low="red" , high = "green")


ggplot(map_plz_et_all) + geom_sf(aes(fill=waerme
)) + scale_fill_gradient(low="red" , high = "green"
) + theme(axis.text.x = element_blank(),
axis.text.y = element_blank(),
axis.ticks = element_blank())+coord_sf(datum=NA)
#https://stackoverflow.com/questions/49836184/cant-remove-gridlines-when-plotting-with-geom-sf

##############

ggplot(map_plz_et_all) + geom_sf(aes(fill=waerme
)) + scale_fill_gradient(low="red" , high = "green"
) + theme(axis.text.x = element_blank(),
axis.text.y = element_blank(),
axis.ticks = element_blank(),
legend.text = element_text(size=25),
legend.title = element_text(size=25))+coord_sf(datum=NA)

ggsave("berlin.pdf",width=20,height=20)

  #coord_sf()+theme(axis.ticks =  element_blank())


