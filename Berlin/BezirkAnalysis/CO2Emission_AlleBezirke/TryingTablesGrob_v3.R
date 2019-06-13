daf <- data.frame(SomeLetters = paste0(letters[sample(1:26 , 5)],letters[sample(1:26 , 5)],letters[sample(1:26 , 5)]), 
                  SomeNumbers1 = sample(1:26,5) , 
                  SomeNumbers2 = sample(1:26,5) , 
                  SomeNumbers3 = sample(1:26,5))



library(gridExtra)
library(grid)

tab01 <- tableGrob(daf,theme=ttheme_default(base_size = 8),rows=NULL, cols = NULL)

#grid.draw(tab01)

#create header
t1 <- ttheme_default(base_size = 8, colhead=list(fg_params=list(hjust=0,x=0.01)))
header01 <- tableGrob(daf[1,1:4],theme=t1,
                      rows=NULL,cols= c("1. Some Letters",
                                        "2. Some Numbers",
                                        "3. Some Numbers",
                                        "4. Some Numbers"))

#grid.draw(header01)

jn <- gtable_combine(header01[1,],tab01,along=2)
jn$widths <- 1:4
jn$layout[1:4 , c("r","l")] <- list(1:4,1:4)
grid.newpage()
grid.draw(jn)

# Now I want to left-justify only the name of the first column, and the first column
#itself. All others have to be center justified.
#see this: https://stackoverflow.com/questions/34962951/gridextra-align-text-to-right

# Let me left justify the first column, center justify the second column, and 
#right justify the third and fourth columns.
t0 <- ttheme_default(base_size = 8)
t1 <- ttheme_default(base_size = 8,core=list(fg_params=list(hjust=1,x=0.9)))
t2 <- ttheme_default(base_size = 8,core=list(fg_params=list(hjust=0,x=0.02)))
t3 <- ttheme_default(base_size = 8,core=list(fg_params=list(hjust=0,x=0.3)))
tab01 <- tableGrob(daf[, 1],theme=t0,rows=NULL, cols = NULL)
#tab01 <- grid.table(daf[, 1],theme=t0,rows=NULL, cols = NULL)

tab02 <- tableGrob(daf[, 2],theme=t0,rows=NULL, cols = NULL)
#tab02 <- grid.table(daf[, 2],theme=t0,rows=NULL, cols = NULL)

tab34 <- tableGrob(daf[, 3:4],theme=t0,rows=NULL, cols = NULL)
#tab34 <- grid.table(daf[, 3:4],theme=t0,rows=NULL, cols = NULL)


#tab_1234 <- tableGrob(gtable_combine(tab01,tab02,tab34))
tab_1234 <- gtable_combine(tab01,tab02,tab34)

tb01 <- tableGrob(daf,theme=ttheme_default(base_size = 8),rows=NULL, cols = NULL)

# https://stackoverflow.com/questions/44418647/tablegrob-formatting
# https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html

header01 <- tableGrob(daf[1,1:4],theme=t0,
                      rows=NULL,cols= c("1. Some Letters",
                                        "2. Some Numbers",
                                        "3. Some Numbers",
                                        "4. Some Numbers"))

#jn <- gtable_combine(header01[1,],tab01,along=2)
jn <- gtable_combine(header01[1,],tab_1234,along=2)
#jn$layout[1:8 , c("r","l")] <- list(1:4,1:4)
jn2 <- gtable_combine(header01[1,],tb01,along=2)
#jn2$layout[1:8 , c("r","l")] <- list(1:4,1:4)


grid.newpage()
grid.draw(jn)


grid.newpage()
grid.draw(jn2)

###











#works:
grid.newpage()
grid.draw(gtable_combine(header01[1,], tb01 , along = 2))
 
##
#check:
grid.newpage()
grid.draw(header01[1,])

grid.newpage()
grid.draw(tb01)

grid.newpage()
grid.draw(tab_1234)


grid.newpage()
grid.draw(gtable_combine(header01[1,], tab_1234 , along = 2))
