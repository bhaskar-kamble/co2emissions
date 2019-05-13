setwd("D:/GITHUB_REPOS/co2emissions/Berlin/Postleitzahl/plz-5stellig.shp")

library(sf)
#library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(ggplot2)

options(scipen = 999)

plz_daten <- read.csv("plz-5stellig-daten.csv")
plz_berlin <- read.csv2("plz_waerme_dist.csv")

dim(plz_daten)
dim(plz_berlin)

sum(plz_daten$plz %in% plz_berlin$plz)

in_berlin <- plz_daten$plz %in% plz_berlin$plz

plz_daten_in_berlin <- plz_daten[in_berlin , ]

plz_berlin_merged <- merge(plz_daten_in_berlin , plz_berlin , by.x = "plz" , by.y = "plz")
# in the above, the column "prop" contains the proportion in percentage of the waerme ET
write.csv2(plz_berlin_merged , file = "plz_berlin_merged.csv" , row.names = FALSE)

mymap <- st_read("plz-5stellig.shp" , stringsAsFactors = FALSE)
mymap$plz <- as.integer(mymap$plz)
mymap_berlin <- mymap[mymap$plz %in% plz_berlin$plz , ]

map_and_data <- inner_join(mymap_berlin , plz_berlin_merged , by = "plz")

ggplot(map_and_data) + geom_sf(aes(fill=prop))

#https://de.wikipedia.org/wiki/Datei:Berlin_Postleitzahlen.svg
#https://upload.wikimedia.org/wikipedia/commons/thumb/5/56/Berlin_Postleitzahlen.svg/1731px-Berlin_Postleitzahlen.svg.png
