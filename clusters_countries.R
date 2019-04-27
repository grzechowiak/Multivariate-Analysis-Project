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

#### GROUP 2 #### 
gr2 <- as.data.frame(subset(clus_coun, km$cluster == 2))
gr2$country <- rownames(gr2)
gr2 <- as.vector(unlist(gr2$country))

#### GROUP 3 #### 
gr3 <- as.data.frame(subset(clus_coun, km$cluster == 3))
gr3$country <- rownames(gr3)
gr3 <- as.vector(unlist(gr3$country))

#### GROUP 4 #### 
gr4 <- as.data.frame(subset(clus_coun, km$cluster == 4))
gr4$country <- rownames(gr4)
gr4 <- as.vector(unlist(gr4$country))

#### GROUP 5 #### 
gr5 <- as.data.frame(subset(clus_coun, km$cluster == 5))
gr5$country <- rownames(gr5)
gr5 <- as.vector(unlist(gr5$country))

#### GROUP 6 #### 
gr6 <- as.data.frame(subset(clus_coun, km$cluster == 6))
gr6$country <- rownames(gr6)
gr6 <- as.vector(unlist(gr6$country))



library(maptools)
data(wrld_simpl)

country_colors <- setNames(rep(gray(.80), length(wrld_simpl@data$NAME)), wrld_simpl@data$NAME)
country_colors[wrld_simpl@data$NAME %in% gr1] <- "#d53e4f"
country_colors[wrld_simpl@data$NAME %in% gr2] <- "#fc8d59"
country_colors[wrld_simpl@data$NAME %in% gr3] <- "#fee08b"
country_colors[wrld_simpl@data$NAME %in% gr4] <- "#e6f598"
country_colors[wrld_simpl@data$NAME %in% gr5] <- "#99d594"
country_colors[wrld_simpl@data$NAME %in% gr6] <- "#3288bd"

group <- c('1','2','3','4','5','6')
color <- c("#d53e4f", "#fc8d59", "#fee08b","#e6f598","#99d594","#3288bd")
col_gr <- data.frame(group, color)


sort(wrld_simpl@data$NAME)
plot(wrld_simpl, col = country_colors)
title(main=paste("Clusters of Countries"))
legend("topleft", inset=.1, title="Clusters",
       c("1", "2","3", "4","5", "6"), 
       fill=c("#d53e4f", "#fc8d59", "#fee08b","#e6f598","#99d594","#3288bd"), 
       horiz=FALSE, cex=0.5)

library(maptools)
data(wrld_simpl)
myCountries = wrld_simpl@data$NAME %in% c("Australia", "United Kingdom", "Germany", "United States", "Sweden", "Netherlands", "New Zealand")
plot(wrld_simpl, col = c(gray(.80), "red")[myCountries+1])