data_clus <-  data
data_clus <- data_clus[,-2]

#Take median for each continent for each column
grouped_by_con <- aggregate(. ~ continent, data=data_clus, FUN=median)

#transpose
test <- t(grouped_by_con)

#Make a distance matrix
distance <- 1-cor(test)
class(distance)
distance <- as.dist(distance)

#Make cluster
#1
hc <- hclust(distance, "single")
plot(hc, main="Distance between Continents")

#2
library(ape)
plot(as.phylo(hc), type = "unrooted", cex = 1,
     no.margin = TRUE)

#3
plot(as.phylo(hc), type = "cladogram", cex = 0.8,
     edge.color = "steelblue", edge.width = 2, edge.lty = 2,
     tip.color = "steelblue")

# More: http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
## Interesting is Oceania & Africa are together!!
# Make sense! 
#Invstigate it here:
#https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)_per_capita#/media/File:GDP_per_capita_(nominal)_2015.png
#https://en.wikipedia.org/wiki/List_of_Oceanian_countries_by_GDP_(nominal)