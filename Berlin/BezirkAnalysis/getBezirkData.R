getBezirkData <- function(bezirk,berlin_data) {
  #setwd("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis") 
  return(berlin_data[berlin_data$bezirk == bezirk ,])
}