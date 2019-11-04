saveFigure <- function(plotFig=TRUE,
                       gtypeForFig,
                       section=NULL,
                       subsection=NULL,
                       subsubsection=NULL,
                       Teil=1, #either 1 or 2
                       figIndex,
                       figWidth=NULL,
                       figHeight=NULL,
                       whatToSave = last_plot()) {
  if (Teil==1) {
    pathToFile <- "/home/kbhaskar/Github_Repos/co2emissions/Berlin/FiguresForSenate/Teil1/"
  }
  if (Teil==2) {
    pathToFile <- "/home/kbhaskar/Github_Repos/co2emissions/Berlin/FiguresForSenate/Teil2/"
  }
  figName <- paste0(gtypeForFig,"_Section_",section,"_",subsection,"_",subsubsection,"_fig_",figIndex,".png")
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