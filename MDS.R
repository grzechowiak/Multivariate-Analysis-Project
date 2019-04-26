#MDS
mydata <- data[, c(-1,-2)]
data.s <- scale(mydata)
d <- dist(data.s)
cmd <- cmdscale(d, k= 5, eig = T)
cumsum(cmd$eig[1:5])/sum(cmd$eig[1:5]) #we choose 3 because cover 78%

cmd <- cmdscale(dist(data.s), k= 3)
cmd

library("scatterplot3d")
s3d <- scatterplot3d(cmd, pch = "", cex.axis = 1)

by_continent <- aggregate(data[, 3:17], list(data$continent), mean)
cmd_continent <- cmdscale(dist(scale(by_continent[, c(-1,-2)])), k=5, eig = T)
cumsum(cmd_continent$eig[1:5])/sum(cmd_continent$eig[1:5]) 
cmd_continent <- cmdscale(dist(scale(by_continent[, c(-1,-2)])), k=2)
colors <- c("#999999", "#E69F00", "#56B4E9", "#483D8B", "#8B0000", "#C71585", "#3CB371")
plot(cmd_continent, xlab = "coordinate 1", ylab = "coordinate 2", pch=19, col=colors) 
legend(-2.5, -2, fill=colors, legend=by_continent$Group.1, col=colors, cex=0.8)
