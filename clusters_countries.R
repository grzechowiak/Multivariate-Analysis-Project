########################################################
#### COMPUTING THE DISTANCE BETWEEN COUNTRIES ####
########################################################

#Copy data
clus_coun <- data

#Save without names and scale
clus_coun.s <- scale(clus_coun[,c(-1,-2)])

#Function to check # of clusters in a data
plot.wgss = function(mydata, maxc) {
  wss = numeric(maxc)
  for (i in 1:maxc) 
    wss[i] = kmeans(mydata, centers=i, nstart = 10)$tot.withinss 
  plot(1:maxc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", main="Scree Plot") 
}

#Check # of cluster
plot.wgss(clus_coun.s, 20) 
# Plots shows around 7 clausters

km <- kmeans(clus_coun.s, centers=7)

table(km$cluster)

subset(clus_coun.s, km$cluster == 1)
