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

###############################
########Data Cleaning##########
###############################

library(car)

par(mfrow=c(5,3))
#Pop_Total Column
with(cleaned, Boxplot(pop_total, id=list(labels=cleaned$country),main="Population Total"))

#Murder_PP Column
with(cleaned, Boxplot(murder_pp, id=list(labels=cleaned$country),main="Murder Per Person"))

#Armed_PP Column
with(cleaned, Boxplot(armed_pp, id=list(labels=cleaned$country),main="Armed Per Person"))

#Phones_p100 Column
with(cleaned, Boxplot(phones_p100, id=list(labels=cleaned$country),main="Phones Per 100 People"))

#children_p_woman Column
with(cleaned, Boxplot(children_p_woman, id=list(labels=cleaned$country),main="Children Per Woman"))

#Life_Exp_Yrs Column
with(cleaned, Boxplot(life_exp_yrs, id=list(labels=cleaned$country),main="Life Expectancy in Years"))

#Suicide_PP Column
with(cleaned, Boxplot(suicide_pp, id=list(labels=cleaned$country),main="Suicide Per Person"))

#Urban_Pop_Tot Column
with(cleaned, Boxplot(urban_pop_tot, id=list(labels=cleaned$country),main="Urban Population Total"))

#Sex_Ratio_p100
with(cleaned, Boxplot(sex_ratio_p100, id=list(labels=cleaned$country),main="Sex Ratio Per 100 People"))

#Corruption_CPI
with(cleaned, Boxplot(corruption_CPI, id=list(labels=cleaned$country),main="Corruption CPI Index"))

#Internet_%0f_pop
with(cleaned, Boxplot(`internet_%of_pop`, id=list(labels=cleaned$country),main="Internet Usage Perceantage of Population"))

#child_mort_p1000
with(cleaned, Boxplot(child_mort_p1000, id=list(labels=cleaned$country),main="Child Mortality Rate per 1000"))

#income_per_person
with(cleaned, Boxplot(income_per_person, id=list(labels=cleaned$country),main="Income Per Person"))

#investments_per_ofGDP
with(cleaned, Boxplot(investments_per_ofGDP, id=list(labels=cleaned$country),main="Investments Percentage of GDP"))

#Gini
with(cleaned, Boxplot(gini, id=list(labels=cleaned$country),main="GINI Ratio"))

####Histograms######
hist(cleaned$pop_total)
hist(cleaned$murder_pp)
hist(cleaned$armed_pp)
hist(cleaned$phones_p100)
hist(cleaned$children_p_woman)
hist(cleaned$life_exp_yrs)
hist(cleaned$suicide_pp)
hist(cleaned$urban_pop_tot)
hist(cleaned$sex_ratio_p100)
hist(cleaned$corruption_CPI)
hist(cleaned$`internet_%of_pop`)
hist(cleaned$child_mort_p1000)
hist(cleaned$income_per_person)
hist(cleaned$investments_per_ofGDP)
hist(cleaned$gini)


