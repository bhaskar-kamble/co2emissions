setwd("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis")



DL_PLZ <- read.csv2("FW_ja_nein.csv")
str(DL_PLZ)

library(sf)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(ggplot2)

mymap <- st_read("D:/GITHUB_REPOS/co2emissions/Berlin/Postleitzahl/plz-5stellig.shp/plz-5stellig.shp",
                 stringsAsFactors = FALSE)
mymap$plz <- as.integer(mymap$plz)

str(mymap)

map_data <- inner_join(mymap , DL_PLZ , by = "plz")

ggplot(map_data) + geom_sf(aes(fill=FW.ja.nein.)
) + scale_fill_manual(values=c("green","red"),name="Fernwärme",labels=c("Ja","Nein")
)+ theme(plot.title=element_text(size=22),
axis.text.x = element_blank(),
axis.text.y = element_blank(),
axis.ticks = element_blank(),
legend.text = element_text(size=65),
legend.title = element_text(size=65),
legend.key.size = unit(4,"cm"),
legend.key.width = unit(4,"cm"))+coord_sf(datum=NA)# 55 was 15
#https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/
ggsave("Germany_districtheat_test.pdf" , 
       width= 55 , height = 55 , limitsize = FALSE)


#########################################################################################



ggplot(map_data) + geom_sf(aes(fill=FW.ja.nein. , size = 0.001)
) + scale_fill_manual(values=c("green","red"),name="Fernwärme",labels=c("Ja","Nein")
)+ theme(plot.title=element_text(size=22) ,
         legend.text = element_text(size=15), legend.title = element_text(size=15))
ggsave("Germany_districtheat_widthpt001.pdf" , width= 15 , height = 15)

#https://ggplot2.tidyverse.org/reference/ggsf.html