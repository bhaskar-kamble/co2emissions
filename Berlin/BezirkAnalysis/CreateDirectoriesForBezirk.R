setwd("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis")
source("getBerlinData.R"           )
gtype <- "SFH"   # SFH or MFH
berlin_data <- getBerlinData(gtype)

bezirk_list <- sort(unique(berlin_data$bezirk))

n_bezirk <- as.character(1:length(bezirk_list))
n_bezirk[nchar(n_bezirk)==1] <- paste0("0",n_bezirk[nchar(n_bezirk)==1])

dir_names <- paste0(n_bezirk , "_", bezirk_list)

for (dir_name in dir_names) {
  dir.create(dir_name)
}