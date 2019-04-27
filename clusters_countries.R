#What is inside:
#The function receives cleaned data and k-means 
#clustering between countries.

clusters_countries <- function(data_input=NULL) {
  

########################################################
################ K- MEANS CLUSTERS  ####################
########################################################

#Copy data
clus_coun <- cleaned
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
scree_plot2 %<a-%plot.wgss(clus_coun, 20)
# Plots shows around 7 clausters

#Perform clustering
set.seed(123)
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


# Plot all groups on the one map
#library(maptools)
data(wrld_simpl)

#Specify Columns per group
country_colors <- setNames(rep(gray(.80), length(wrld_simpl@data$NAME)), wrld_simpl@data$NAME)
country_colors[wrld_simpl@data$NAME %in% gr1] <- "#d53e4f" #red
country_colors[wrld_simpl@data$NAME %in% gr2] <- "#fee08b" #yellow
country_colors[wrld_simpl@data$NAME %in% gr3] <- "#e6f598" #light green 
country_colors[wrld_simpl@data$NAME %in% gr4] <- "#fc8d59" #orange
country_colors[wrld_simpl@data$NAME %in% gr5] <- "#91cf60" #green 
country_colors[wrld_simpl@data$NAME %in% gr6] <- "#3288bd" #blue 

#Plot the map
# sort(wrld_simpl@data$NAME)
Cl_countries <-  plot(wrld_simpl, col = country_colors) 
Cl_countries <- Cl_countries + title(main=paste("Clusters of Countries")) 
Cl_countries <- Cl_countries <- legend(x=-180,y=15, inset=.09, title="",
            c("Poor, Corrupted", "Income/Gender","Low birthrate", "Crowded","Inequality","Developed", "NoData"), 
            fill=c("#d53e4f","#fee08b", "#e6f598", "#fc8d59","#91cf60","#3288bd", gray(.80)), 
            horiz=FALSE, cex=1.5, bg="transparent",bty = "n")

centers <- km$centers

ToReturn <- list(Cl_countries,centers) # Return two objects: Plot and Centers to interpret the plot

return(ToReturn)
}