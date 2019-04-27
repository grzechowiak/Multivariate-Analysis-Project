########################################################
#### COMPUTING THE DISTANCE BETWEEN COUNTRIES ####
########################################################

#Copy data
clus_coun <- data
row.names(clus_coun) <- clus_coun$country
#delete columns: country, continent
clus_coun[c(1,2)] <- NULL

#Scale data
clus_coun <- scale(clus_coun)

#Function to check # of clusters in a data
plot.wgss = function(mydata, maxc) {
  wss = numeric(maxc)
  for (i in 1:maxc) 
    wss[i] = kmeans(mydata, centers=i, nstart = 10)$tot.withinss 
  plot(1:maxc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", main="Scree Plot") 
}

#Check # of cluster
plot.wgss(clus_coun, 20)
# Plots shows around 7 clausters

#Perform clustering
km <- kmeans(clus_coun, centers=6, nstart = 20)
table(km$cluster)

#### GROUP 1 #### 
gr1 <- as.data.frame(subset(clus_coun, km$cluster == 1))
gr1$country <- rownames(gr1)
gr1 <- as.vector(unlist(gr1$country))

#Plot group 1
library(maptools)
data(wrld_simpl)
myCountries = wrld_simpl@data$NAME %in% gr1
plot(wrld_simpl, col = c(gray(.80), "red")[myCountries+1], main='Group 1')


