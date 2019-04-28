#What is inside:
#The function receives cleaned data perform k-means & mode based
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
#scree_plot2 <- plot.wgss(clus_coun, 20)
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
country_colors[wrld_simpl@data$NAME %in% gr1] <- "#80cdc1" #green "Income/Gender"
country_colors[wrld_simpl@data$NAME %in% gr2] <- "#d53e4f" #red Poor/Corrupted
country_colors[wrld_simpl@data$NAME %in% gr3] <- "#91cf60" #light green  "Developed"
country_colors[wrld_simpl@data$NAME %in% gr4] <- "#fc8d59" #orange "Low birthrate
country_colors[wrld_simpl@data$NAME %in% gr5] <- "#fee08b"  #yellow  "Inequality"
country_colors[wrld_simpl@data$NAME %in% gr6] <- "#3288bd" #blue  "Crowded"

#Plot the map
Cl_countries <-  plot(wrld_simpl, col = country_colors) 
Cl_countries <- Cl_countries + title(main=paste("K-Means Clustering")) 
Cl_countries <- Cl_countries <- legend(x=-180,y=15, inset=.09, title="",
            c("Income/Gender","Poor/Corrupted","Developed", "Low birthrate","Inequality","Crowded","NoData"),
            fill=c("#80cdc1","#d53e4f", "#91cf60", "#fc8d59","#fee08b","#3288bd", gray(.80)),
            horiz=FALSE, cex=1.5, bg="transparent",bty = "n")


centers <- km$centers





########################################################
################ MODEL BASED CLUSTERS  #################
########################################################

#library(mclust)

#Perform Model based Clustering
set.seed(123)
mc <- Mclust(cleaned[,c(-1,-2)])

cat("\014") ## just a trick to clean a console. In order to avoid annoying R Console info
# in Rmarkdown information.

#Check summary
summary(mc)

#Match clusters with country names and transfer into df
mc_groups <- cbind(mc$classification, cleaned$country)
mc_groups <- as.data.frame(mc_groups)
colnames(mc_groups) <- c("group", "country")

#Create new df for each group and save as a list 
gr1.mc <- filter(mc_groups, group==1)
gr1.mc <- as.vector(unlist(gr1.mc$country))

gr2.mc <- filter(mc_groups, group==2)
gr2.mc <- as.vector(unlist(gr2.mc$country))

gr3.mc <- filter(mc_groups, group==3)
gr3.mc <- as.vector(unlist(gr3.mc$country))

gr4.mc <- filter(mc_groups, group==4)
gr4.mc <- as.vector(unlist(gr4.mc$country))

gr5.mc <- filter(mc_groups, group==5)
gr5.mc <- as.vector(unlist(gr5.mc$country))

gr6.mc <- filter(mc_groups, group==6)
gr6.mc <- as.vector(unlist(gr6.mc$country))

gr7.mc <- filter(mc_groups, group==7)
gr7.mc <- as.vector(unlist(gr7.mc$country))

cleaned[cleaned[,2] %in% gr7.mc]

filter(cleaned[,2] %in% gr7.mc)

#library(maptools)
data(wrld_simpl)
#Specify Columns per group
country_colors <- setNames(rep(gray(.80), length(wrld_simpl@data$NAME)), wrld_simpl@data$NAME)
country_colors[wrld_simpl@data$NAME %in% gr1.mc] <- "#d53e4f" #red "Poor/Corrupted"
country_colors[wrld_simpl@data$NAME %in% gr2.mc] <- "#91cf60" #green "Developed"
country_colors[wrld_simpl@data$NAME %in% gr3.mc] <- "#d53e4f" #roseeeeee   "Semi-Developed"
country_colors[wrld_simpl@data$NAME %in% gr4.mc] <- "#fc8d59" #orange "Not Crowded"
country_colors[wrld_simpl@data$NAME %in% gr5.mc] <- "#fee08b" #yellow  "Highly Developed"
country_colors[wrld_simpl@data$NAME %in% gr6.mc] <- "#3288bd" #blue  "Unequal/Corrupted"
country_colors[wrld_simpl@data$NAME %in% gr7.mc] <- "#f1a340" #orange  "Income/Gender"

#Plot the map
Cl_countries.mc <-  plot(wrld_simpl, col = country_colors) 
Cl_countries.mc <- Cl_countries.mc + title(main=paste("Model Based Clustering")) 
Cl_countries.mc <- Cl_countries.mc <- legend(x=-180,y=15, inset=.09, title="",
                                       c("Poor/Corrupted","Developed","Semi-Developed","Low population","Highly Developed","Unequal/Corrupted","Population/Income/Gender","NoData"), 
                                       fill=c("#d53e4f","#91cf60", "#d53e4f", "#fc8d59","#91cf60","#3288bd" ,"#f1a340", gray(.80)), 
                                       horiz=FALSE, cex=1.5, bg="transparent",bty = "n")




## Centroids to interprete the clusters
round(t(mc$parameters[["mean"]]),3)


#COMPARE
#model based
chisq.test(table(cleaned[,1], mc$classification))
table(cleaned[,1], mc$classification)

table(cleaned[,1])
table(mc$classification)

#k-means
chisq.test(table(cleaned[,1], km$cluster))
table(cleaned[,1], km$cluster)

table(cleaned[,1])
table(km$cluster)

ToReturn <- list(Cl_countries, Cl_countries.mc) # Return two objects: Plot and Centers to interpret the plot

return()
}