#What is inside:
#The function receives cleaned data and compute hierarchical 
#clustering between continents.

cluster_continents <- function(data_input=NULL) {

########################################################
#### COMPUTING THE DISTANCE BETWEEN CONTINENTS ####
########################################################
data_clus <-  cleaned
data_clus <- data_clus[,-2] # delete country column

#Take median for each continent for each column
grouped_by_con <- aggregate(. ~ continent, data=data_clus, FUN=median)

#Set continents as a rownames
row.names(grouped_by_con) <- grouped_by_con$continent
grouped_by_con[1] <- NULL

#transpose data
test <- t(grouped_by_con)

#Make a distance matrix
distance <- 1-cor(test)
#class(distance)
distance <- as.dist(distance)

#Make clusters
hc <- hclust(distance, "single")
#plot(hc, main="Distance between Continents")

#Plot Clusters
cluster_plot %<a-% plot(as.phylo(hc), type = "cladogram", cex = 0.8,
     edge.color = "steelblue", edge.width = 2, edge.lty = 2,
     tip.color = "steelblue")

# Check clusters by scree plot
scree_plot %<a-%plot(rev(hc$height))

cutree(hc,3)
#Scree plot support the idea of 3 clusters
#We have 3 clusters in data:
#1: Africa & Oceania
#2: Asia & Central Ameica
#3: South America, Europe, North America

return(cluster_plot)
}