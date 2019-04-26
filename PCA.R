data2 <- data
colnames(data2)
data2 <- select(data,'continent', 'country', #'armed_pp',
                'phones_p100','children_p_woman','life_exp_yrs',
                'suicide_pp',
                #'sex_ratio_p100',
                'corruption_CPI','internet_%of_pop','child_mort_p1000','income_per_person'
                #,'gini'
                )

## Scale only column 3 to 10
data2[,c(3:10)] <- lapply(data2[,c(3:10)], function(x) c(scale(x)))

pca <- princomp(data2[,c(-1,-2)], cor=T)
summary(pca, loadings=T)

#Normal biplot, from class
#biplot(pca,xlabs=abbreviate(data2$continent))
#biplot(prcomp(data2[,c(-1,-2)]), choices=c(1,2),xlabs=(data2$country))     ## plots ax1 and ax2 


#library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
wine.pca <- prcomp(data2[,c(-1,-2)], scale. = TRUE)
ggbiplot(wine.pca, obs.scale = 1, var.scale = 1,
         groups = data2$continent, ellipse = TRUE) +
  scale_color_discrete(name = 'Continents:') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

#https://github.com/vqv/ggbiplot