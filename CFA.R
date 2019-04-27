
#CFA <- function(data_input=NULL) {
#}
#CFA(cleaned)

#####with 2 factors
library(sem)
cfa_data <- cleaned
colnames(cfa_data)
cfa_data <- cfa_data[,c(-1,-2,-4,-5,-9,-11,-15,-16,-17)]

cfa_data <- cfa_data[,c(-1,-2,-9,-17)]
cfa_model <- specifyModel(file = "cfa_model.txt")
colnames(cfa_data)
cfa_sem <- sem(cfa_model, cor(cfa_data), nrow(cfa_data))
summary(cfa_sem)

cfa_result <- cfa(file = "cfa_model.txt")
fit <- cfa(file = "cfa_model.txt", sample.cov=cor(cfa_data), sample.nobs=nrow(cfa_data),  std.lv=FALSE)


summary(cfa_result)