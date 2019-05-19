library(gridExtra)
library(grid)
n <- 5
d <- data.frame(x=rnorm(n),y=rnorm(n),z=sample(letters[1:2],n,replace=T))

m <- format(d, digits = 1, scientific=F,big.mark = ",")

mytheme <- ttheme_default(core = list(fg_params = list(hjust=0, x=0.1, 
                                                       fontsize=8)),
                          colhead = list(fg_params = list(fontsize=9, 
                                                          fontface="bold"))
)
g1 <- tableGrob(m, theme = mytheme, rows=NULL)
grid.newpage()
grid.draw(g1)


####################

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
jn$widths <- rep(3,4) #c(2,5,8,11)#7:10#
jn$layout[1:4 , c("r","l")] <- list(c(1,2),c(1,4))

grid.newpage()
grid.draw(jn)


######################
