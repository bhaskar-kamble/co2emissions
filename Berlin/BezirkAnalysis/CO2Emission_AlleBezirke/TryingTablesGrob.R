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

#grid.draw(jn)
ttt <- ttheme_default(core=list(fg_params=list(hjust=-4.9, x=0.95)))

header02 <- tableGrob(table01[1, 1:2],rows=NULL, cols=c(" " , "CO2 Emission in 1.000 t"),
                      theme=ttt) 
jn <- gtable_combine(header02[1,],jn,along=2)
jn$layout[1:4 , c("r","l")] <- list(c(1,2),c(1,7)) # What is the logic for 1:4 ???
grid.newpage()
grid.arrange(jn)#,theme=ttt)




header03 <- tableGrob(table01[1, 1],rows=NULL, cols=c("1-2 Familiengebäude")) 
jn <- gtable_combine(header03[1,],jn,along=2)
jn$layout[1:2 , c("r","l")] <- list(1, 7)


header04 <- tableGrob(table01[1, 1],rows=NULL, cols=c("CO2 Emissionen aus Beheizung von Wohngebäuden, Berliner Stadtbezirke, 2018")) 
jn <- gtable_combine(header04[1,],jn,along=2)
jn$widths <- rep(5,7)
jn$layout[1:2 , c("r","l")] <- list(1, 7)


#ttt <- ttheme_default(header=list(fg_params=list(hjust=0,x=0)))


grid.newpage()
grid.arrange(jn)#,theme=ttt)






##################################################################################

daf <- data.frame(x=1:5,y=6:10)
jn <- tableGrob(daf,rows=NULL,cols=NULL)
header01 <- tableGrob(daf[1,1:2])
jn <- gtable_combine(header01[1,],jn,along=2)


##################################################################################

library(gridExtra)
library(grid)
d <- head(iris[,1:3])
grid.table(d)

d[2,3] <- "this is very wwwwwide"
d[1,2] <- "this\nis\ntall"
colnames(d) <- c("alpha*integral(xdx,a,infinity)",
                 "this text\nis high", 'alpha/beta')

tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
grid.table(d, theme=tt)

###

tt1 <- ttheme_default()
tt2 <- ttheme_minimal()
tt3 <- ttheme_minimal(
  core=list(bg_params = list(fill = blues9[1:4], col=NA),
            fg_params=list(fontface=3)),
  colhead=list(fg_params=list(col="navyblue", fontface=4L)),
  rowhead=list(fg_params=list(col="orange", fontface=3L)))

grid.arrange(
  tableGrob(iris[1:4, 1:2], theme=tt1),
  tableGrob(iris[1:4, 1:2], theme=tt2),
  tableGrob(iris[1:4, 1:2], theme=tt3),
  nrow=1)

###

tt1 <- ttheme_default()
tt2 <- ttheme_default(core=list(fg_params=list(hjust=1, x=0.9)),
                      rowhead=list(fg_params=list(hjust=1, x=0.95)))
tt3 <- ttheme_default(core=list(fg_params=list(hjust=0, x=0.1)),
                      rowhead=list(fg_params=list(hjust=0, x=0)))
grid.arrange(
  tableGrob(mtcars[1:4, 1:2], theme=tt1),
  tableGrob(mtcars[1:4, 1:2], theme=tt2),
  tableGrob(mtcars[1:4, 1:2], theme=tt3),
  nrow=1)