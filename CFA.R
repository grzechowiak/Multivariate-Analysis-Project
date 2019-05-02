
#with sem error: Iteration limit exceeded.  Algorithm failed.
CFA <- function(data_input=NULL) {
library(lavaan)
d <- cleaned
d <- d[,c(-1,-2,-5,-9,-16)]
colnames(d)[which(colnames(d) %in% c("internet_%of_pop") )] <- c("internet")

model4 <- 'Development_Level =~ phones_p100 + children_p_woman + life_exp_yrs + corruption_CPI + internet + child_mort_p1000 
Population_Level =~ urban_pop_tot + pop_total
Inequality_Level =~ murder_pp + gini 
Gender_Income =~ sex_ratio_p100 + income_per_person'

fit4 <- lavaan::cfa(model4, data = d)
#ERROR: lavaan WARNING: some observed variances are (at least) a factor 1000 times larger than others; use varTable(fit) to investigate
fit4 <- lavaan::cfa(model4, data = scale(d))

fit4 <- lavaan::cfa(model4, data = log(d))
#ERROR: lavaan WARNING: the optimizer warns that a solution has NOT been found!

summary(fit4)
parTable(fit4)
lavInspect(fit4, "optim.gradient")


}
CFA(cleaned)