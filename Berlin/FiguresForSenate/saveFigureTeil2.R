saveFigureTeil2 <- function(plotFig=TRUE,
                       gtypeForFig,
                       section=NULL,
                       subsection=NULL,
                       subsubsection=NULL,
                       figIndex,
                       figWidth=NULL,
                       figHeight=NULL,
                       bezirk_index,
                       whatToSave = last_plot()) {
  
  bezirk_list  <- c(
    "charlottenburg_wilmersdorf",
    "friedrichshain_kreuzberg",
    "lichtenberg",
    "marzahn_hellersdorf",
    "mitte",
    "neukoelln",
    "pankow"    ,             
    "reinickendorf",
    "spandau",
    "steglitz_zehlendorf" ,
    "tempelhof_schoeneberg",
    "treptow_koepenick"                              
  )

  pathToFile <- "/home/kbhaskar/Github_Repos/co2emissions/Berlin/FiguresForSenate/Teil2/"
  pathToFile <- paste0(pathToFile , bezirk_list[bezirk_index] , "/")
  
  figName <- paste0(gtypeForFig,"_Section_",section,"_",subsection,"_",subsubsection,"_fig_",figIndex,".svg")
  figName <- paste0(pathToFile , figName)
  if (!is.null(figWidth) & !is.null(figHeight)) {
    ggsave(figName , 
           plot = whatToSave,
           width = figWidth , 
           height=figHeight)
  }
  if (is.null(figWidth) & is.null(figHeight)) {
    ggsave(figName , plot = whatToSave)
  }
  if ((is.null(figWidth) & !is.null(figHeight))|((!is.null(figWidth) & is.null(figHeight))))  {
    stop("either specify BOTH height and width, or specify none")
  }
  
  return(figIndex+1)
}