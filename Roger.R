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