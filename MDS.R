# What function does:
##The function creates the 3D visualization of countries ##
# the idea is very simple, 360 plots are made and combined into one gif.



## Copy the data and prepare for visualization - MDS
data <- cleaned
data.s <- scale(data[, c(-1,-2)])
d <- dist(data.s)       
cmd <- cmdscale(d, k=3)


## Add colors for continents
colors <- c("#999999", "#E69F00", "#56B4E9", "#483D8B", "#8B0000", "#C71585", "#3CB371")
colors_data <- colors[as.numeric(factor(data$continent))]

## Create pictures of the plot from different agels
frames <- 360
rename <- function(x){
  if (x < 10) {
    return(name <- paste('000',i,'plot.png',sep=''))
  }
  if (x < 100 && i >= 10) {
    return(name <- paste('00',i,'plot.png', sep=''))
  }
  if (x >= 100) {
    return(name <- paste('0', i,'plot.png', sep=''))
  }
}

# Run the function to save imagies
for(i in 1:frames){
  name <- rename(i)
  
  png(name)
  scatterplot3d(cmd,
                main="MDS Continents",
                angle=i,
                pch=16,
                cex.axis = 1,
                color=colors_data)
  legend("bottomleft", fill=colors, legend=levels(factor(data$continent)), col=colors, cex=0.8)
  
  dev.off()
}

##Convert picutres into .gif
# my_command <- 'convert *.png -delay 1 -loop 0 3d.gif'
# system(my_command)
# my_command <- 'rm *.png'
# system(my_command)

#convert 3d.gif -fuzz 30% -layers Optimize result.gif





## Additionaly.
## Create 3D plot and add a text for Quatar
# library(scatterplot3d)
s3d <- scatterplot3d(cmd, pch=16, cex.axis = 1)
#Add the label for the biggest outlier in Asia continent -Quatar
s3d.coords <- s3d$xyz.convert(cmd)
text(s3d.coords$x[141], s3d.coords$y[141],
     labels=data$country[141],
     cex=.5, pos=4)
