#use the files year2010.csv, year2011.csv etc.

setwd("D:/GITHUB_REPOS/co2emissions/Berlin/FindArea")

##################################################################
############                  ####################################
############  2   0   1   0   ####################################
############                  ####################################
##################################################################

data2010 <- read.csv2("year2010.csv",stringsAsFactors = FALSE)
data2010 <- data2010[, !(names(data2010) %in% c("notreqd","notreqd.1","notreqd.2"))]

bezirk_list <- c(
  "mitte",
  "friedrichshain_kreuzberg", 
  "pankow",
  "charlottenburg_wilmersdorf", 
  "spandau",  
  "steglitz_zehlendorf",
  "tempelhof_schoeneberg",
  "neukoelln",
  "treptow_koepenick",
  "marzahn_hellersdorf", 
  "lichtenberg",  
  "reinickendorf"   
   )

data2010$Bezirk <- bezirk_list
names(data2010)[names(data2010)=="Bezirk"] <- "bezirk"

not_bezirk <- names(data2010)[names(data2010) != "bezirk"]

for (item in not_bezirk) {
  data2010[[item]] <- gsub(" " , ""  , data2010[[item]])
  data2010[[item]] <- as.numeric(data2010[[item]])
}
data2010$abrechnungsjahr <- 2010




##################################################################
############                  ####################################
############  2   0   1   1   ####################################
############                  ####################################
##################################################################

data2011 <- read.csv2("year2011.csv",stringsAsFactors = FALSE)
data2011 <- data2011[, !(names(data2011) %in% c("notreqd","notreqd.1","notreqd.2"))]

data2011$Bezirk <- bezirk_list
names(data2011)[names(data2011)=="Bezirk"] <- "bezirk"

not_bezirk <- names(data2011)[names(data2011) != "bezirk"]

for (item in not_bezirk) {
  data2011[[item]] <- as.numeric(data2011[[item]])
}
data2011$abrechnungsjahr <- 2011



##################################################################
############                  ####################################
############  2   0   1   2   ####################################
############                  ####################################
##################################################################

data2012 <- read.csv2("year2012.csv",stringsAsFactors = FALSE)
data2012 <- data2012[, !(names(data2012) %in% c("notreqd","notreqd.1","notreqd.2"))]

data2012$Bezirk <- bezirk_list
names(data2012)[names(data2012)=="Bezirk"] <- "bezirk"

not_bezirk <- names(data2012)[names(data2012) != "bezirk"]

for (item in not_bezirk) {
  data2012[[item]] <- as.numeric(data2012[[item]])
}
data2012$abrechnungsjahr <- 2012



##################################################################
############                  ####################################
############  2   0   1   3   ####################################
############                  ####################################
##################################################################
data2013 <- read.csv2("year2013.csv",stringsAsFactors = FALSE)
data2013 <- data2013[, !(names(data2013) %in% c("notreqd","notreqd.1","notreqd.2"))]

data2013$Bezirk <- bezirk_list
names(data2013)[names(data2013)=="Bezirk"] <- "bezirk"

not_bezirk <- names(data2013)[names(data2013) != "bezirk"]

for (item in not_bezirk) {
  data2013[[item]] <- as.numeric(data2013[[item]])
}
data2013$abrechnungsjahr <- 2013


##################################################################
############                  ####################################
############  2   0   1   4   ####################################
############                  ####################################
##################################################################
data2014 <- read.csv2("year2014.csv",stringsAsFactors = FALSE)
data2014 <- data2014[, !(names(data2014) %in% c("notreqd","notreqd.1","notreqd.2"))]

data2014$Bezirk <- bezirk_list
names(data2014)[names(data2014)=="Bezirk"] <- "bezirk"

not_bezirk <- names(data2014)[names(data2014) != "bezirk"]

for (item in not_bezirk) {
  data2014[[item]] <- gsub("\\." , ""  , data2014[[item]])
  data2014[[item]] <- as.numeric(data2014[[item]])
}
data2014$abrechnungsjahr <- 2014


##################################################################
############                  ####################################
############  2   0   1   5   ####################################
############                  ####################################
##################################################################

data2015 <- read.csv2("year2015.csv",stringsAsFactors = FALSE)
data2015 <- data2015[, !(names(data2015) %in% c("notreqd","notreqd.1","notreqd.2"))]

data2015$Bezirk <- bezirk_list
names(data2015)[names(data2015)=="Bezirk"] <- "bezirk"

not_bezirk <- names(data2015)[names(data2015) != "bezirk"]

for (item in not_bezirk) {
  data2015[[item]] <- as.numeric(data2015[[item]])
}
data2015$abrechnungsjahr <- 2015



##################################################################
############                  ####################################
############  2   0   1   6   ####################################
############                  ####################################
##################################################################

data2016 <- read.csv2("year2016.csv",stringsAsFactors = FALSE)
data2016 <- data2016[, !(names(data2016) %in% c("notreqd","notreqd.1","notreqd.2"))]

data2016$Bezirk <- bezirk_list
names(data2016)[names(data2016)=="Bezirk"] <- "bezirk"

not_bezirk <- names(data2016)[names(data2016) != "bezirk"]

for (item in not_bezirk) {
  data2016[[item]] <- as.numeric(data2016[[item]])
}
data2016$abrechnungsjahr <- 2016


##################################################################
############                  ####################################
############  2   0   1   7   ####################################
############                  ####################################
##################################################################

data2017 <- read.csv2("year2017.csv",stringsAsFactors = FALSE)
data2017 <- data2017[, !(names(data2017) %in% c("notreqd","notreqd.1","notreqd.2"))]

data2017$Bezirk <- bezirk_list
names(data2017)[names(data2017)=="Bezirk"] <- "bezirk"

not_bezirk <- names(data2017)[names(data2017) != "bezirk"]

for (item in not_bezirk) {
  data2017[[item]] <- gsub("\\." , ""  , data2017[[item]])
  data2017[[item]] <- as.numeric(data2017[[item]])
}
data2017$abrechnungsjahr <- 2017


##################################################################
data_allyears <- rbind(data2010,data2011,data2012,data2013,data2014,data2015,data2016,data2017)

write.csv2(data_allyears , file = "areas_berlin_bezirke.csv", row.names = FALSE)