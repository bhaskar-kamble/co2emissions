# Braunschweig
#This program finds the sample size for Braunschweig
source("/home/kbhaskar/Github_Repos/co2emissions/RheinNeckarKreis/getRegionData.R")
source("/home/kbhaskar/Github_Repos/visualization-project2-smurfs/cleanData.R")
source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/getRowSums.R")

load("/home/kbhaskar/Github_Repos/co2emissions/SFH20022018_v3.RData")
load("/home/kbhaskar/Github_Repos/co2emissions/MFH20022018_v3.RData")

######################################################################################################
# Damn, here have to change the strings to the correct format (umlauts and other special characters)
LK <- sort(unique(SFH20022018$Landkreis_von_GS))
Encoding(LK)
LK2 <- iconv(LK, to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE)
LK3 <- enc2utf8(LK)
######################################################################################################

LK <- sort(unique(SFH20022018$Landkreis_von_GS))
grep("Braunschweig" , LK)

#> LK[grep("Braunschweig" , LK)]
#[1] "Braunschweig, Stadt"

braunschweig_sfh <- getRegionData("SFH" , "Braunschweig, Stadt")
braunschweig_mfh <- getRegionData("MFH" , "Braunschweig, Stadt")

table_jahr_sfh <- as.data.frame(xtabs(~abrechnungsjahr,data=braunschweig_sfh))
table_jahr_mfh <- as.data.frame(xtabs(~abrechnungsjahr,data=braunschweig_mfh))
table_jahr_sfh$abrechnungsjahr <- as.numeric(as.character(table_jahr_sfh$abrechnungsjahr))
table_jahr_mfh$abrechnungsjahr <- as.numeric(as.character(table_jahr_mfh$abrechnungsjahr))
require(ggplot2)
ggplot(table_jahr_sfh , aes(x=abrechnungsjahr,y=Freq)) + geom_point() + labs(x="Jahr",y="N",title="Braunschweig, Stichprobengröße, 1-2 FH")+scale_x_continuous(breaks=seq(2002,2018,2))
ggsave("/home/kbhaskar/Github_Repos/co2emissions/Braunschweig/StichprobeBraunschweig_1-2FH.jpg")
ggplot(table_jahr_mfh , aes(x=abrechnungsjahr,y=Freq)) + geom_point() + labs(x="Jahr",y="N",title="Braunschweig, Stichprobengröße, MFH")+scale_x_continuous(breaks=seq(2002,2018,2))
ggsave("/home/kbhaskar/Github_Repos/co2emissions/Braunschweig/StichprobeBraunschweig_MFH.jpg")

# Breakup according to energietraeger:
table_sfh <- as.data.frame(xtabs(~abrechnungsjahr+energietraeger,data=braunschweig_sfh))
table_mfh <- as.data.frame(xtabs(~abrechnungsjahr+energietraeger,data=braunschweig_mfh))
table_sfh$abrechnungsjahr <- as.numeric(as.character(table_sfh$abrechnungsjahr))
table_mfh$abrechnungsjahr <- as.numeric(as.character(table_mfh$abrechnungsjahr))
table_sfh$energietraeger <- as.character(table_sfh$energietraeger)
table_mfh$energietraeger <- as.character(table_mfh$energietraeger)

require(reshape2)
sample_sfh <- dcast(table_sfh , abrechnungsjahr ~ energietraeger , value.var = "Freq")
sample_sfh <- getRowSums(sample_sfh , dropCols = "abrechnungsjahr")
sample_mfh <- dcast(table_mfh , abrechnungsjahr ~ energietraeger , value.var = "Freq")
sample_mfh <- getRowSums(sample_mfh , dropCols = "abrechnungsjahr")

#########################

getETNames <- function(obj) {
  names(obj)[names(obj)=="erdgas"] <- "Erdgas"
  names(obj)[names(obj)=="fluessiggas"] <- "Flüssiggas"
  names(obj)[names(obj)=="heizoel"] <- "Heizöl"
  names(obj)[names(obj)=="holzpellets"] <- "Holzpellets"
  names(obj)[names(obj)=="strom"] <- "Strom"
  names(obj)[names(obj)=="waerme"] <- "Wärme"
  return(names(obj))
}

plot_reqdColumns <- function(input_data,   # data frame
                             xVar,         # column name of "input_data" to be plotted on the x-axis
                             cols_to_plot, #column names of "input_data" to be plotted on the y-axis
                             yColsName,    # for eg. if cols_to_plot = c("Bonn","Berlin"), then this can be City
                             yVar,        # for eg. "population" if the columns for "Berlin" etc. show population
                             plot_title = NULL,
                             xlabel = NULL,
                             ylabel = NULL,
                             regLine = TRUE,
                             kt_to_mt = FALSE
) {
  
  if (kt_to_mt) {
    source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/convert_kilo_to_megaton.R")
    input_data <- convert_kilo_to_megaton(input_data , "abrechnungsjahr")
  }
  
  input_data <- input_data[ , c(xVar , cols_to_plot)]
  #names(input_data) <- getETNames(names(input_data))
  
  #convert data to long format
  require(reshape2)
  input_data <- melt(input_data , id.vars = xVar )
  names(input_data) <- c(xVar , yColsName , yVar)
  
  require(ggplot2)
  return_object <- 
    ggplot(input_data
    )+geom_point(aes(x=get(xVar),y=get(yVar),col=get(yColsName))
    ) + geom_smooth(method="lm",aes(x=get(xVar),y=get(yVar),col=get(yColsName)),se=FALSE
    )+scale_color_discrete(name = yColsName
    )+labs(x=xlabel,y=ylabel,title=plot_title)+ylim(0,max(input_data[[yVar]]))
  
  if (regLine==FALSE) {
    return_object <- 
      ggplot(input_data
      )+geom_point(aes(x=get(xVar),y=get(yVar),col=get(yColsName))
      )+geom_line(aes(x=get(xVar),y=get(yVar),col=get(yColsName))
      )+scale_color_discrete(name = yColsName
      )+labs(x=xlabel,y=ylabel,title=plot_title)+ylim(0,max(input_data[[yVar]]))
  }
  
  detach("package:reshape2")
  #detach("package:ggplot2")
  
  return(return_object) 
}


names(sample_sfh) <- getETNames(sample_sfh)
names(sample_mfh) <- getETNames(sample_mfh)
plot_reqdColumns(sample_sfh,
                 xVar = "abrechnungsjahr",
                 cols_to_plot = names(sample_sfh)[!(names(sample_sfh) %in% c("abrechnungsjahr","total"))],
                 yColsName = "Energieträger",
                 yVar = "N",
                 plot_title = "1-2 FH Anzahl, Braunschweig",
                 xlabel = "Jahr",
                 ylabel = "N",
                 regLine = FALSE
                 )
ggsave("/home/kbhaskar/Github_Repos/co2emissions/Braunschweig/Anzahl_SFH_nachET.jpg")
plot_reqdColumns(sample_mfh,
                 xVar = "abrechnungsjahr",
                 cols_to_plot = names(sample_mfh)[!(names(sample_mfh) %in% c("abrechnungsjahr","total"))],
                 yColsName = "Energieträger",
                 yVar = "N",
                 plot_title = "MFH Anzahl, Braunschweig",
                 xlabel = "Jahr",
                 ylabel = "N",
                 regLine = FALSE
)
ggsave("/home/kbhaskar/Github_Repos/co2emissions/Braunschweig/Anzahl_MFH_nachET.jpg")
######################################### 
sample_sfh_save <- sample_sfh
sample_mfh_save <- sample_mfh
names(sample_sfh_save)[names(sample_sfh_save)=="abrechnungsjahr"] <- "Jahr"
names(sample_sfh_save)[names(sample_sfh_save)=="total"] <- "Gesamt"
names(sample_mfh_save)[names(sample_mfh_save)=="abrechnungsjahr"] <- "Jahr"
names(sample_mfh_save)[names(sample_mfh_save)=="total"] <- "Gesamt"
write.csv2(sample_sfh_save , file = "/home/kbhaskar/Github_Repos/co2emissions/Braunschweig//Anzahl_SFH.csv",row.names = FALSE)
write.csv2(sample_mfh_save , file = "/home/kbhaskar/Github_Repos/co2emissions/Braunschweig//Anzahl_MFH.csv",row.names = FALSE)


names(read.csv2(file = "/home/kbhaskar/Github_Repos/co2emissions/Braunschweig//Anzahl_MFH.csv",stringsAsFactors  = FALSE))