table01 <- round_off(table01 , "bezirk")

tab01 <- tableGrob(table01,theme=ttheme_default(base_size = 8),rows=NULL,cols=NULL)

header01 <- tableGrob(table01[1, 1:7],rows=NULL, cols=c("Stadtbezirk:",
                                                        "Gesamt",
                                                        "Fern \n wärme",
                                                        "Erdgas",
                                                        "Heizöl",
                                                        "WP-Strom \n und Direkt- \n heizung",
                                                        "Pellets"))


jn <- gtable_combine(header01[1,],tab01,along=2)
jn$layout[1:7 , c("r","l")] <- list(1:7,1:7)



t2 <- ttheme_default(base_size = 12,
                     colhead=list(fg_params=list(hjust=0,x=0.05))) 
header02 <- tableGrob(table01[1, 1:2],
                      rows=NULL, 
                      cols=c(" " , "CO2 Emission in 1.000 t"),
                      theme = t2)
jn <- gtable_combine(header02[1,],jn,along=2)
jn$layout[1:4 , c("r","l")] <- list(c(1,2),c(1,7)) # Wtf is the logic for 1:4 ???



grid.newpage()
grid.draw(jn)




header03 <- tableGrob(table01[1, 1],rows=NULL, 
                      cols="1-2 Familiengebäude",theme=t2) 
jn <- gtable_combine(header03[1,],jn,along=2)
#jn$widths <- rep(max(jn$widths), length(jn$widths))
#jn$widths <- rep(5,7)
jn$layout[1:2 , c("r","l")] <- list(1, 7)


header04 <- tableGrob(table01[1, 1],rows=NULL, 
        cols="CO2 Emissionen aus Beheizung von Wohngebäuden, Berliner Stadtbezirke, 2018",
        theme=t2) 
jn <- gtable_combine(header04[1,],jn,along=2)
jn$widths <- rep(5,7)
jn$layout[1:2 , c("r","l")] <- list(1, 7)

grid.newpage()
grid.draw(jn)
