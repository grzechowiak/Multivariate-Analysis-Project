###Roger Valdez
###Project
library(dplyr)
filt.data <- select(data, 'murder_pp', 'phones_p100', 'children_p_woman', 'life_exp_yrs', 'suicide_pp', 'corruption_CPI', 'internet_%of_pop', 'child_mort_p1000', 'income_per_person', 'gini')
pca <- princomp(filt.data, cor = T)
             
cor(filt.data)    

filt.data$suicide_pp <- max(filt.data$suicide_pp)-filt.data$suicide_pp
filt.data$murder_pp <- max(filt.data$murder_pp)-filt.data$murder_pp
filt.data$gini <- max(filt.data$gini)-filt.data$gini

summary(pca,loadings = T)

biplot(pca, xlabs = abbreviate(data$continent))




########################
##K-Means Clustering####
########################

plot.wgss = function(mydata, maxc) {
  wss = numeric(maxc)
  for (i in 1:maxc)
    wss[i] = kmeans(mydata, centers=i, nstart = 10)$tot.withinss
  plot(1:maxc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", main="Scree Plot")
}
plot.wgss(filt.data, 20)


#install.packages("ggplot2")
library("ggplot2")
km <- kmeans(filt.data, centers = 7, nstart = 20)
table(km$cluster)
km$cluster

km$tot.withinss

#pca <- princomp(filt.data)
#plot(pca$scores[,1:3], col = km$cluster)

plot(filt.data[,c(9,5)], col = km$cluster , main = "K-Means Clusters" )
text(filt.data[,c(9,5)], abbreviate(data$country),pch=16,cex=.6)

km$centers


########################
#Model-Based Clustering#
########################
library(mclust)
mc <- Mclust(filt.data[,c(3:10)],7)
table(mc$classification, data$continent)
plot(mc, what = "classification")

###Bivariate Boxplots####
library(MVA)
par(mfrow=c(1,3))
###GINI/Income_per_person###
plot(gini ~ income_per_person, data = cleaned,
     cex.lab = 1,
     xlab = "Income Per Person",
     ylab = "GINI Inequality Index",
     main = "GINI/Income")
text(gini ~ income_per_person, data = cleaned,
     labels=(cleaned$country))
x <- cleaned[,c("income_per_person", "gini")]
bvbox(x, add = T)

###Life Expectancy Years/Children Per Woman###
plot(life_exp_yrs ~ children_p_woman, data = cleaned,
     cex.lab = 1,
     xlab = "Children Per Woman",
     ylab = "Life Expectancy Years",
     main = "Life Expectancy/\nChildren Per Woman")
text(life_exp_yrs ~ children_p_woman, data = cleaned,
     labels=(cleaned$country))
x <- cleaned[,c("children_p_woman", "life_exp_yrs")]
bvbox(x, add = T)

###Suicide Per Person/Phone per 100 People###
plot(suicide_pp ~ phones_p100, data = cleaned, 
     cex.lab = 1,
     xlab = "Phone per 100 People",
     ylab = "Suicide Per Person",
     main = "Suicide/Phone per 100")
text(suicide_pp ~ phones_p100, data = cleaned,
     labels=(cleaned$country))
x <- cleaned[,c("phones_p100", "suicide_pp")]
bvbox(x, add = T)

colnames(cleaned)
































