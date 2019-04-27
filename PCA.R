#What is inside:
#The function receives cleaned data and compute PCA. The result is PCA plot.

PCA <- function(data_input=NULL) {

#Copy the dataset
data2 <- cleaned

#Select interesting columns
data2 <- select(data2,'continent', 'country', #'armed_pp',
                'phones_p100','children_p_woman','life_exp_yrs',
                'suicide_pp',
                'sex_ratio_p100',
                'corruption_CPI','internet_%of_pop','child_mort_p1000','income_per_person'
                ,'gini'
                )

#Change colnames
colnames(data2)[which(colnames(data2) %in% c(
  "phones_p100", "children_p_woman","life_exp_yrs","suicide_pp","sex_ratio_p100",
  "corruption_CPI","internet_%of_pop","child_mort_p1000","income_per_person",
  "gini") )] <- c("PHONES","CHILDREN","LIFE EXP","SUICIDE","SEX RATIO","LESS CORRUPTION", 
                  "INTERNET","CHILD MORT.","INCOME","INEQUALITY")

## Scale only column 3 to 10 (exclude columns with names)
data2[,c(3:10)] <- lapply(data2[,c(3:10)], function(x) c(scale(x)))

#Perform PCA
pca <- princomp(data2[,c(-1,-2)], cor=T)
summary(pca, loadings=T)

#Plot PCA (before check if you need 'ggibiplot')
#library(devtools)
#install_github("vqv/ggbiplot")
#library(ggbiplot)
plot.pca <- prcomp(data2[,c(-1,-2)], scale. = TRUE)
PCA_plot %<a-% ggbiplot(plot.pca, obs.scale = 1, var.scale = 1,
         groups = data2$continent, ellipse = TRUE) +
  scale_color_discrete(name = 'Continents:') +
  theme(legend.direction = 'horizontal', legend.position = 'top')

#Source of biplot:
#https://github.com/vqv/ggbiplot

return(PCA_plot)
}