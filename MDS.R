
#apply MDS to all data
data <- cleaned
data.s <- scale(data[, c(-1,-2)])
d <- dist(data.s)       
cmd <- cmdscale(d, k=5, eig = T)
cumsum(cmd$eig[1:5])/sum(cmd$eig[1:5]) 
cmd <- cmdscale(d, k=3)
library(scatterplot3d)
s3d <- scatterplot3d(cmd, pch=16, cex.axis = 1)
s3d.coords <- s3d$xyz.convert(cmd)
text(s3d.coords$x[141], s3d.coords$y[141],           # x and y coordinates
     labels=data$country[141],               # text to plot
     cex=.5, pos=4)       


#True clusters: continents
colors <- c("#999999", "#E69F00", "#56B4E9", "#483D8B", "#8B0000", "#C71585", "#3CB371")
colors_data <- colors[as.numeric(factor(data$continent))]
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

my_command <- 'convert *.png -delay 1 -loop 0 3d.gif'
system(my_command)
my_command <- 'rm *.png'
system(my_command)

#convert 3d.gif -density 50 -quality 50 mds2.gif


#group by continent and apply MDS
by_continent <- aggregate(data[, 3:17], list(data$continent), mean)
cmd_continent <- cmdscale(dist(scale(by_continent[, c(-1,-2)])), k=5, eig = T)
cumsum(cmd_continent$eig[1:5])/sum(cmd_continent$eig[1:5]) 
cmd_continent <- cmdscale(dist(scale(by_continent[, c(-1,-2)])), k=2)
plot(cmd_continent, xlab = "coordinate 1", ylab = "coordinate 2", pch=16, col=colors) 
legend(-2.5, -1, fill=colors, legend=by_continent$Group.1, col=colors, cex=0.8)


