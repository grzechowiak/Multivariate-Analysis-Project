
#apply MDS to all data 
data.s <- scale(data[, c(-1,-2)])
d <- dist(data.s)       
cmd <- cmdscale(d, k=5, eig = T)
cumsum(cmd$eig[1:5])/sum(cmd$eig[1:5]) 
cmd <- cmdscale(d, k=3)
library(scatterplot3d)
s3d <- scatterplot3d(cmd, pch=16, cex.axis = 1)

#True clusters: continents
colors_data <- colors[as.numeric(factor(data$continent))]
s3d <- scatterplot3d(cmd, pch=16, cex.axis = 1, color = colors_data)
legend(3.9,6.1, fill=colors_data, legend=levels(factor(data$continent)), col=colors_data, cex=0.8)


#group by continent and apply MD5
by_continent <- aggregate(data[, 3:17], list(data$continent), mean)
cmd_continent <- cmdscale(dist(scale(by_continent[, c(-1,-2)])), k=5, eig = T)
cumsum(cmd_continent$eig[1:5])/sum(cmd_continent$eig[1:5]) 
cmd_continent <- cmdscale(dist(scale(by_continent[, c(-1,-2)])), k=2)
colors <- c("#999999", "#E69F00", "#56B4E9", "#483D8B", "#8B0000", "#C71585", "#3CB371")
plot(cmd_continent, xlab = "coordinate 1", ylab = "coordinate 2", pch=16, col=colors) 
legend(-2.5, -1, fill=colors, legend=by_continent$Group.1, col=colors, cex=0.8)
