data2 <- data
library(dplyr)
colnames(data2)
data2 <- select(data,'continent', 'country', #'armed_pp',
                'phones_p100','children_p_woman','life_exp_yrs','suicide_pp',
                #'sex_ratio_p100',
                'corruption_CPI','internet_%of_pop','child_mort_p1000','income_per_person','gini')

## Get data in onedirection
data2$suicide_pp <- max(data2$suicide_pp)-data2$suicide_pp
data2$child_mort_p1000 <- max(data2$child_mort_p1000)-data2$child_mort_p1000
data2$gini <- max(data2$gini) - data2$gini

#data <- data[,c(3:17)]
pca <- princomp(data2[,c(-1,-2)], cor=T)
summary(pca, loadings=T)


## PC1 can be high develope countries
# because not many children_p_woman, suicide is high
### REMEMBER suicide_pp, child_mort_p1000 and gini are REVERSED

## PC2 Arabic country:
#more inequality
# Suicide is very low
#gini is high (more inequality)

#Country with low comp2 


Africa <- data2[,c(1,2)]
biplot(pca,xlabs=abbreviate(Africa$continent))
pca$scores[,1:2]



f <- function(Africa) {
  ifelse(Africa$continent == "Africa", 1, 0)
}

apply(Africa, 1, f)


Africa$continent <- apply(Africa,1,function(x) 
  ifelse(Africa$continent == "Africa", "Africa", ""))




## I wanna rownames as country
rownames(data2) <- data2[,2]

index_africa <- which(data2[,1] == 'Africa')
index_africa <- as.data.frame(index_africa)

table(data2$continent)

      
pca$scores[pca$scores %in% index_africa]

row.names(pca$scores)


rows <- pca$scores
pc_scores <- as.data.frame(rows)
library(data.table)
setDT(pc_scores, keep.rownames = "row_index")

rownames(pc_scores)


Africa_scores <- subset(pc_scores, pc_scores$row_index %in% index_africa$index_africa)
biplot(Africa_scores,xlabs=abbreviate(data2$country))
biplot(Africa_scores[,2:3])
plot(Africa_scores[,2:3])

biplot(pca)



## TO DO
