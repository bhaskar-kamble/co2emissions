library(gridExtra)
library(grid)

table01 <- data.frame(aa=sample(10) , bb = sample(10) , cc = sample(10) , dd = sample(10))
tab01 <- tableGrob(table01,theme=ttheme_default(base_size = 8),rows=NULL)

#header <- tableGrob(table01[1,1] , theme=ttheme_default(base_size = 14), 
#                    rows=NULL, cols="Tabelle: Berlin, Wohngebäude,flächenbezogene CO2-Emission und #beheizte Wohnfläche 2002 - 2018") 
#header0 <- tableGrob(mtcars[1, 1:2], rows=NULL, cols=c("head01", "head02")) 
header <- tableGrob(table01[1, 1:2], rows=NULL, cols=c("head1", "head2")) 

jn <- gtable_combine(header[1,],tab01,along=2)
jn$widths <- rep(max(jn$widths), length(jn$widths))
jn$layout[1:4 , c("r","l")] <- list(c(1, 3), c(2, 4))

header0 <- tableGrob(table01[1, 1:2], rows=NULL, cols=c("head01", "head02")) 
jn <- gtable_combine(header0[1,],jn,along=2)
jn$widths <- rep(max(jn$widths), length(jn$widths))
jn$layout[1:4 , c("r","l")] <- list(c(1, 3), c(2, 4))

grid.newpage()
grid.draw(jn)


#############################
#############################

table01 <- data.frame(aa=sample(10) , bb = sample(10) , cc = sample(10) , dd = sample(10))
tab01 <- tableGrob(table01,theme=ttheme_default(base_size = 8),rows=NULL)

header <- tableGrob(table01[1, 1],rows=NULL, cols=c("head1")) 
jn <- gtable_combine(header[1,],tab01,along=2)
#jn$widths <- rep(max(jn$widths), length(jn$widths))
#jn$widths <- rep(5,4)
jn$layout[1:2 , c("r","l")] <- list(1, 4)

header2 <- tableGrob(table01[1, 1],rows=NULL, cols=c("head2")) 
jn <- gtable_combine(header2[1,],jn,along=2)
#jn$widths <- rep(max(jn$widths), length(jn$widths))
#jn$widths <- rep(5,4)
jn$layout[1:2 , c("r","l")] <- list(1, 4)

header3 <- tableGrob(table01[1, 1:2],rows=NULL, cols=c("head3","head4")) 
jn <- gtable_combine(header3[1,],jn,along=2)
#jn$widths <- rep(max(jn$widths), length(jn$widths))
#jn$widths <- rep(5,4)
#jn$layout[1:4 , c("r","l")] <- list(c(1,3),c(2,4))
jn$widths <- c(2,5,8,11)#7:10#rep(5,4)
jn$layout[1:4 , c("r","l")] <- list(c(1,2),c(1,4))

grid.newpage()
grid.draw(jn)

############################
############################

table01 <- data.frame(aa=sample(10) , bb = sample(10) , cc = sample(10) , dd = sample(10) , 
                      ee=sample(10) , ff=sample(10) , gg=sample(10) )
tab01 <- tableGrob(table01,theme=ttheme_default(base_size = 8),rows=NULL)

header <- tableGrob(table01[1, 1],rows=NULL, cols=c("head1")) 
jn <- gtable_combine(header[1,],tab01,along=2)
#jn$widths <- rep(max(jn$widths), length(jn$widths))
jn$widths <- c(8:14)
jn$layout[1:2 , c("r","l")] <- list(1, 7)

grid.newpage()
grid.draw(jn)
