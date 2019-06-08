daf <- data.frame(SomeLetters = letters[sample(1:26 , 5)], 
                  SomeNumbers1 = sample(1:26,5) , 
                  SomeNumbers2 = sample(1:26,5) , 
                  SomeNumbers3 = sample(1:26,5))



library(gridExtra)
library(grid)

tab01 <- tableGrob(daf,theme=ttheme_default(base_size = 8),rows=NULL,cols=NULL)