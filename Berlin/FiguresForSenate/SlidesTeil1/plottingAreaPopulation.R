plot_reqdColumns <- function(input_data,   # data frame
                             xVar,         # column name of "input_data" to be plotted on the x-axis
                             cols_to_plot, #column names of "input_data" to be plotted on the y-axis
                             yColsName,    # for eg. if cols_to_plot = c("Bonn","Berlin"), then this can be City
                             yVar,        # for eg. "population" if the columns for "Berlin" etc. show population
                             plot_title = NULL,
                             xlabel = NULL,
                             ylabel = NULL,
                             regLine = TRUE
) {
  
  input_data <- input_data[ , c(xVar , cols_to_plot)]
  names(input_data) <- get_BezirkNames(names(input_data))
  
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
      )+scale_color_discrete(name = yColsName
      )+labs(x=xlabel,y=ylabel,title=plot_title)+ylim(0,max(input_data[[yVar]]))
  }
  
  detach("package:reshape2")
  #detach("package:ggplot2")
  
  return(return_object) 
}

####

#######


get_BezirkNames <- function(obj) {
  obj[obj=="charlottenburg_wilmersdorf"] <- "Charlottenburg-Wilmersdorf"
  obj[obj=="friedrichshain_kreuzberg"]   <- "Friedrichshain-Kreuzberg"
  obj[obj=="lichtenberg"]                <- "Lichtenberg"
  obj[obj=="marzahn_hellersdorf"]        <- "Marzahn-Hellersdorf"
  obj[obj=="mitte"]                      <- "Mitte"
  obj[obj=="neukoelln"]                  <- "Neukölln"
  obj[obj=="pankow"]                     <- "Pankow"
  obj[obj=="reinickendorf"]              <- "Reinickendorf"
  obj[obj=="spandau"]                    <- "Spandau"
  obj[obj=="steglitz_zehlendorf"]        <- "Steglitz-Zehlendorf"
  obj[obj=="tempelhof_schoeneberg"]      <- "Tempelhof-Schöneberg"
  obj[obj=="treptow_koepenick"]          <- "Treptow-Köpenick"
  return(obj)
}

#######################################

bezirk_areas <- read.csv2("/home/kbhaskar/Github_Repos/co2emissions/Berlin/FindArea/areas_berlin_bezirke.csv",stringsAsFactors = FALSE)
#str(bezirk_areas)

source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/getBezirkAreas.R")

bezirk_list <- sort(unique(bezirk_areas$bezirk))

getBezirkAreas(bezirk_list[1])

bezirke_areas_sfh <- data.frame(abrechnungsjahr = 2002:2018 )
bezirke_areas_mfh <- data.frame(abrechnungsjahr = 2002:2018 )
for (i in 1:length(bezirk_list)) {
  bezirke_areas_sfh[[bezirk_list[i] ]] <- getBezirkAreas(bezirk_list[i])$areaSFH
  bezirke_areas_mfh[[bezirk_list[i] ]] <- getBezirkAreas(bezirk_list[i])$areaMFH
}
bezirke_areas_all <- bezirke_areas_mfh + bezirke_areas_sfh
#area originally in 100 sq.m. Convert to million sq. m.
bezirke_areas_sfh <- 1e-4*bezirke_areas_sfh
bezirke_areas_mfh <- 1e-4*bezirke_areas_mfh
bezirke_areas_all <- 1e-4*bezirke_areas_all
bezirke_areas_sfh$abrechnungsjahr <- 2002:2018
bezirke_areas_mfh$abrechnungsjahr <- 2002:2018
bezirke_areas_all$abrechnungsjahr <- 2002:2018

plot_reqdColumns(bezirke_areas_sfh,
                 xVar = "abrechnungsjahr",
                 cols_to_plot = names(bezirke_areas_sfh)[names(bezirke_areas_sfh) !="abrechnungsjahr"],
                 yColsName = "Bezirk",
                 yVar = "Mio. q.m.",
                 plot_title = "Wohnfläche, 1-2 Familiengebäude",
                 xlabel = "Jahr",
                 ylabel = "Mio. q.m.",
                 regLine = FALSE)


plot_reqdColumns(bezirke_areas_mfh,
                 xVar = "abrechnungsjahr",
                 cols_to_plot = names(bezirke_areas_mfh)[names(bezirke_areas_mfh) !="abrechnungsjahr"],
                 yColsName = "Bezirk",
                 yVar = "Mio. q.m.",
                 plot_title = "Wohnfläche, Mehrfamiliengebäude",
                 xlabel = "Jahr",
                 ylabel = "Mio. q.m.",
                 regLine = FALSE)

plot_reqdColumns(bezirke_areas_all,
                 xVar = "abrechnungsjahr",
                 cols_to_plot = names(bezirke_areas_all)[names(bezirke_areas_all) !="abrechnungsjahr"],
                 yColsName = "Bezirk",
                 yVar = "Mio. q.m.",
                 plot_title = "Wohnfläche, alle Wohngebäude",
                 xlabel = "Jahr",
                 ylabel = "Mio. q.m.",
                 regLine = FALSE)

ggsave("/home/kbhaskar/Github_Repos/co2emissions/Berlin/FiguresForSenate/SlidesTeil1/areas_all.png")

source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/FiguresForSenate/saveFigure.R")


#######################################
#######################################

source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/getBerlinBezirkPopulation.R")
bezirk_population <- getBerlinBezirkPopulation()

#######################################
#population:
source("/home/kbhaskar/Github_Repos/co2emissions/Berlin/BezirkAnalysis/getCo2PerCapitaBezirk_byGtype_method2.R")
pop_bybezirk_mfh <- getPopulationBezirk_byGtype("MFH")
pop_bybezirk_sfh <- getPopulationBezirk_byGtype("SFH")
pop_bybezirk_all <- getPopulationBezirk_byGtype("ALL")
pop_bybezirk_mfh <- pop_bybezirk_mfh[names(pop_bybezirk_mfh)!="total"]
pop_bybezirk_sfh <- pop_bybezirk_sfh[names(pop_bybezirk_sfh)!="total"]
pop_bybezirk_all <- pop_bybezirk_all[names(pop_bybezirk_all)!="total"]
pop_bybezirk_mfh <- 1e-4*pop_bybezirk_mfh
pop_bybezirk_sfh <- 1e-4*pop_bybezirk_sfh
pop_bybezirk_all <- 1e-4*pop_bybezirk_all
pop_bybezirk_all$abrechnungsjahr <- 2002:2018
pop_bybezirk_mfh$abrechnungsjahr <- 2002:2018
pop_bybezirk_sfh$abrechnungsjahr <- 2002:2018
plot_reqdColumns(pop_bybezirk_all,
                 xVar = "abrechnungsjahr",
                 cols_to_plot = names(pop_bybezirk_all)[names(pop_bybezirk_all) !="abrechnungsjahr"],
                 yColsName = "Bezirk",
                 yVar = "population",
                 plot_title = "Einwohnerzahl, alle Wohngebäude",
                 xlabel = "Jahr",
                 ylabel = "Einwohnerzahl (10k)",
                 regLine = FALSE)

plot_reqdColumns(pop_bybezirk_sfh,
                 xVar = "abrechnungsjahr",
                 cols_to_plot = names(pop_bybezirk_sfh)[names(pop_bybezirk_sfh) !="abrechnungsjahr"],
                 yColsName = "Bezirk",
                 yVar = "population",
                 plot_title = "Einwohnerzahl, 1-2 Familiengebäude",
                 xlabel = "Jahr",
                 ylabel = "Einwohnerzahl (10k)",
                 regLine = FALSE)

plot_reqdColumns(pop_bybezirk_mfh,
                 xVar = "abrechnungsjahr",
                 cols_to_plot = names(pop_bybezirk_mfh)[names(pop_bybezirk_mfh) !="abrechnungsjahr"],
                 yColsName = "Bezirk",
                 yVar = "population",
                 plot_title = "Einwohnerzahl, Mehrfamiliengebäude",
                 xlabel = "Jahr",
                 ylabel = "Einwohnerzahl (10k)",
                 regLine = FALSE)


#######################################
#anzahl wohnungen:
berlin_wohnungen_bygtype <- getBerlinWohnungen()
berlin_wohnungen_bygtype$SFH <- berlin_wohnungen_bygtype$IFH + berlin_wohnungen_bygtype$IIFH
berlin_wohnungen_bygtype$SFH$abrechnungsjahr <- 2002:2018