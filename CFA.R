
#with sem error: Iteration limit exceeded.  Algorithm failed.
CFA <- function(data_input=NULL) {
library(lavaan)
d <- cleaned
d <- d[,c(-1,-2,-5,-9,-16)]
colnames(d)[which(colnames(d) %in% c("internet_%of_pop") )] <- c("internet")

model4 <- ' Development_Level =~ 1*phones_p100 + (-1)*children_p_woman + 1*life_exp_yrs + 1*corruption_CPI + 1*internet + (-1)*child_mort_p1000 
Population_Level =~ 1*urban_pop_tot + 1*pop_total
Inequality_Level =~ 1*murder_pp + 1*gini 
Gender_Income =~ 1*sex_ratio_p100 + 1*income_per_person '


model4 <- ' Development_Level =~ start(1)*phones_p100 + start(-1)*children_p_woman + start(1)*life_exp_yrs + start(1)*corruption_CPI + start(1)*internet + start(-1)*child_mort_p1000 
Population_Level =~ start(1)*urban_pop_tot + start(1)*pop_total
Inequality_Level =~ start(1)*murder_pp + start(1)*gini 
Gender_Income =~ start(1)*sex_ratio_p100 + start(1)*income_per_person '

fit4 <- lavaan::cfa(model4, data = log(d))
summary(fit4)
parTable(fit4)
lavInspect(fit4, "optim.gradient")


}
CFA(cleaned)