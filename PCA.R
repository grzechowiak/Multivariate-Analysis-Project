data2 <- data
colnames(data2)
data2 <- select(data,'continent', 'country', #'armed_pp',
                'phones_p100','children_p_woman','life_exp_yrs',
                'suicide_pp',
                #'sex_ratio_p100',
                'corruption_CPI','internet_%of_pop','child_mort_p1000','income_per_person'
                #,'gini'
                )

cor(data2[,c(-1,-2)])
## Get data in onedirection
# data2$suicide_pp <- max(data2$suicide_pp)-data2$suicide_pp
# data2$child_mort_p1000 <- max(data2$child_mort_p1000)-data2$child_mort_p1000
# data2$gini <- max(data2$gini) - data2$gini

#data <- data[,c(3:17)]
pca <- princomp(data2[,c(-1,-2)], cor=T)
summary(pca, loadings=T)
biplot(pca,xlabs=abbreviate(data2$continent))

biplot(prcomp(data2[,c(-1,-2)]), choices=c(1,2),xlabs=(data2$country))     ## plots ax1 and ax2 



#Country with low comp2 



######## TO DO:###########!!!!!!!!!!!!!!!!!!!!!!!!!!
#Filter what to display on biplot


#library(devtools)
#install_github("vqv/ggbiplot")




library(ggbiplot)
wine.pca <- prcomp(data2[,c(-1,-2)], scale. = TRUE)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         groups = data2$continent, ellipse = TRUE) +
  scale_color_discrete(name = 'Continents:') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

#https://github.com/vqv/ggbiplot


data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)
print(ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = wine.class, ellipse = TRUE))
