x <- rep(0:4,3)
xx <- rep(0:4,3)
b <- rep(c(-1,1,2),each=5)     #slope
a <- rep(c(-0.5,0.5,1),each=5) #intercept
y <- b*x+a
g <- rep(c("a","b","c"),each=5)

df <- data.frame(x1=x , x2=xx , y=y , g=g)

require(ggplot2)

ggplot(df)+geom_point(aes(x=x1,y=y,col=g))+xlim(0,10)+ylim(-10,10) + geom_smooth(method="lm",aes(x=x1,y=y,col=g))

###########

#convert df to wide - this is how data will usually be
require(reshape2)
df_wide <- dcast(df , x1 + x2  ~ g , value.var = "y")

#convert to long:
df_long <- melt(df_wide , id.vars = c("x1","x2"))

###########

# You have data usually in the wide format, and the aim is to plot certain 
# certain columns as line plot with the regression line.
# Write a function that acchieves this aim

plot_ReqdColumns <- function(input_data,   # data frame
                             xVar,         # column name of "input_data" to be plotted on the x-axis
                             cols_to_plot, #column names of "input_data" to be plotted on the y-axis
                             yColsName,    # for eg. if cols_to_plot = c("Bonn","Berlin"), then this can be City
                             yVar          # for eg. "population" if the columns for "Berlin" etc. show population
) {
  
  input_data <- input_data[ , c(xVar , cols_to_plot)]
  
  #convert data to long format
  require(reshape2)
  input_data <- melt(input_data , id.vars = xVar )
  names(input_data) <- c(xVar , yColsName , yVar)
    
  require(ggplot2)
  return_object <- 
    ggplot(input_data
    )+geom_point(aes(x=get(xVar),y=get(yVar),col=get(yColsName))
    ) + geom_smooth(method="lm",aes(x=get(xVar),y=get(yVar),col=get(yColsName))
    )+scale_color_discrete(name = yColsName
    )+labs(x=xVar,y=yVar)
  
  detach("package:reshape2")
  detach("package:ggplot2")
 
  return(return_object) 
}
