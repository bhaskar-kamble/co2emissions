setwd("D:/GITHUB_REPOS/co2emissions/Berlin/BezirkAnalysis/")

source("getBerlinData.R"           )
#source("getBezirkData.R"           )
#source("energy_proportions_by_et.R" )
#source("appendLinearTrend.R"       )
#source("area_proportions_by_et.R"  )
#source("find_proportions.R"        )
#source("getBezirkAreas.R"        )
#source("getSpecificConsumption.R"   )
#source("getTotalConsumption.R"     )
#source("getAbsoluteEnergyShares.R"  )
#source("getCO2Coeff.R"             )
#source("getCO2Emissions.R"         )

berlin_sfh <- getBerlinData("SFH")
berlin_mfh <- getBerlinData("MFH")
plz_list <- unique(c(berlin_sfh$sto_plz  ,  berlin_mfh$sto_plz))

unique(berlin_mfh$energietraeger)
unique(berlin_sfh$energietraeger)

berlin_sfh_waerme <- berlin_sfh[berlin_sfh$energietraeger=="waerme"   ,   ]
berlin_mfh_waerme <- berlin_mfh[berlin_mfh$energietraeger=="waerme"   ,   ]

plz_list_with_waerme <- c(berlin_sfh_waerme$sto_plz , berlin_mfh_waerme$sto_plz)

plz_waerme_dist <- as.data.frame(table(plz_list_with_waerme))
plz_waerme_dist$prop <- 100.0*plz_waerme_dist$Freq / sum(plz_waerme_dist$Freq)
names(plz_waerme_dist) <- c("plz","Freq","prop")
plz_waerme_dist$plz <- as.integer(as.character(plz_waerme_dist$plz))
write.csv2(plz_waerme_dist , file="plz_waerme_dist.csv" , row.names = FALSE)



unique(berlin_mfh$bezirk)