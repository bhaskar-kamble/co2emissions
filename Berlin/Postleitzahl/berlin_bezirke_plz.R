setwd("D:/GITHUB_REPOS/co2emissions/Berlin/Postleitzahl")

plz_bezirk <- read.csv2("Berlin_Bezirke_PLZ.csv")
names(plz_bezirk) <- c(
  
  "charlottenburg_wilmersdorf",
  "friedrichshain_kreuzberg",
  "lichtenberg",
  "marzahn_hellersdorf",
  "mitte",                     
  "neukoelln",
  "pankow",
  "reinickendorf",
  "spandau",
  "steglitz_zehlendorf",       
  "tempelhof_schoeneberg",
  "treptow_koepenick" 
  
)

library(reshape)

plz_bezirk_long <- melt(plz_bezirk)
plz_bezirk_long <- plz_bezirk_long[!is.na(plz_bezirk_long$value)  ,  ]
plz_bezirk_long$variable <- as.character(plz_bezirk_long$variable)
str(plz_bezirk_long)
names(plz_bezirk_long) <- c("bezirk","plz")
write.csv2(plz_bezirk_long,file="plz_bezirk_long.csv",row.names = FALSE)


# TEST DATA FOR ARCGIS ------------------------
plz_list <- plz_bezirk_long$value
set.seed(3072)
plz_value <- sample(1:length(plz_list))
test_data <- data.frame(plz = plz_list , value = plz_value)

# TEST DATA FOR ARCGIS ------------------------
unique(plz_bezirk_long$variable)
test_data <- data.frame(bezirk = unique(plz_bezirk_long$variable) ,
                        wert = sample(1:length(unique(plz_bezirk_long$variable))))
write.csv2(test_data , file = "test_data.csv" , row.names = FALSE)