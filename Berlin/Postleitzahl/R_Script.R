library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)

options(scipen = 999)

setwd("D:/GITHUB_REPOS/co2emissions/Berlin/Postleitzahl/Bhaskar-Berlin_Bezirksgrenzen.shp")

mydata <- read.csv("bezirklist.csv" , ,sep=";" , stringsAsFactors = FALSE)

mymap <- st_read("bezirksgrenzen.shp" , stringsAsFactors = FALSE)

str(mymap)



names(mymap)[names(mymap) == "Gemeinde_n"] <- "bezirk"

map_and_data <- inner_join(mymap,mydata)