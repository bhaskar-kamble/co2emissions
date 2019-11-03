
###################################################################################################################

Teil <- 1
gtypeForFig <- "SFH"
figIndex <- 1


figIndex <- saveFigure(gtypeForFig=gtypeForFig , Teil=Teil , figIndex=figIndex , whatToSave = whatToSave)


####################################################################################################################


figIndex <- 1
figIndex <- saveFigure(gtypeForFig=gtypeForFig , 
                       section = i_section,
                       subsection = i_subsection,
                       subsubsection = i_subsubsection, 
                       figIndex=figIndex,
#                       whatToSave = whatToSave)

#####################################################################################################################

figIndex <- 1
figIndex <- saveFigureTeil2(gtypeForFig=gtypeForFig , 
                       section = i_section,
                       subsection = i_subsection,
                       subsubsection = i_subsubsection, 
                       figIndex=figIndex,
                       bezirk_index = i_bezirk,
#                       whatToSave = whatToSave)
                       
######################################################################################################################                       
plot_gridAllETs(spz_vrbrch_all_byET, geb_type = "ALL")
whatToSave <- grob_gridAllETs(spz_vrbrch_all_byET, geb_type = "ALL")
figIndex <- 1
figIndex <- saveFigureTeil2(gtypeForFig=gtypeForFig , 
                            section = i_section,
                            subsection = i_subsection,
                            subsubsection = i_subsubsection, 
                            figIndex=figIndex,
                            bezirk_index = i_bezirk,
                            whatToSave = whatToSave)
