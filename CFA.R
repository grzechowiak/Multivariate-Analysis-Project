
#CFA <- function(data_input=NULL) {
#}
#CFA(cleaned)

#####with 2 factors
library(sem)
cfa_data <- cleaned
cfa_data <- cleaned
cfa_data <- cfa_data[,c(-1,-2,-4,-5,-9,-11,-15,-16,-17)]
cfa_model <- specifyModel(file = "2_factors.txt")
colnames(cfa_data)
cfa_sem <- sem(cfa_model, cor(cfa_data), nrow(cfa_data))
summary(cfa_sem)
##########

