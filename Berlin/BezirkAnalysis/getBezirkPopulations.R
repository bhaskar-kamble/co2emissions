#get the 2014 values of no. of SFH and MFH wohnungen
#D:/GITHUB_REPOS/co2emissions/Berlin/FindArea/areas_berlin_bezirke.csv. 
#D:/GITHUB_REPOS/co2emissions /Berlin/BezirkAnalysis/getBezirkAreas.R


# 2014 Haushalte split by the building type in Berlin.
# D:/GITHUB_REPOS/co2emissions/Berlin/SB_F01-02-00_2014j04_BE.pdf

# total population
# D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/PopulationBezirke/BerlinBezirkPopulation.csv
# The population you can find as follows:
# -----------------
#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getBerlinBezirkPopulation.R")
#bezirk_population <- getBerlinBezirkPopulation()
#source("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/getRowSums.R")
#bezirk_population <- getRowSums(bezirk_population , dropCols = "abrechnungsjahr")
#Extract the 2014 value from the above.
# -----------------

# As a final check: 
# The total population in the Bezirks is given here: 
# D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/PopulationBezirke/BerlinBezirkPopulation.csv.