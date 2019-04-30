
#http://www.statpower.net/Content/312/Handout/Confirmatory%20Factor%20Analysis%20with%20R.pdf
#with sem error: Iteration limit exceeded.  Algorithm failed.
library(sem)
cfa_data <- cleaned
cfa_data <- cfa_data[,c(-1,-2,-4,-5,-9,-11,-15,-16,-17)]
colnames(cfa_data)[which(colnames(cfa_data) %in% c("internet_%of_pop") )] <- c("internet")

cfa_model <- specifyModel(text = "
                          Factor1          -> phones_p100, lambda1, NA              
                          Factor1          -> life_exp_yrs, lambda2, NA  
                          Factor1          -> corruption_CPI, lambda3, NA                
                          Factor1          -> internet, lambda4, NA                           
                          Factor1          -> children_p_woman, lambda5, NA                        
                          Factor1          -> child_mort_p1000, lambda6, NA              
                          Factor2          -> urban_pop_tot, lambda7, NA   
                          Factor2          -> pop_total, lambda8, NA
                          Factor1          <-> Factor2, rho0, NA
                          phones_p100      <-> phones_p100, theta1, NA
                          life_exp_yrs     <-> life_exp_yrs, theta2, NA
                          corruption_CPI   <-> corruption_CPI, theta3, NA
                          internet <-> internet, theta4, NA
                          children_p_woman <-> children_p_woman, theta5, NA
                          child_mort_p1000 <-> child_mort_p1000, theta6, NA
                          urban_pop_tot    <-> urban_pop_tot, theta7, NA
                          pop_total        <-> pop_total, theta8, NA
                          Factor1          <-> Factor1, NA, 1
                          Factor2          <-> Factor2, NA, 1")
cfa_sem <- sem(cfa_model, cor(cfa_data), nrow(cfa_data), debug = T)
summary(cfa_sem)

#with lavaan
#warning: lavaan WARNING: some observed variances are (at least) a factor 1000 times larger than others
#so, we did the log if the data

library(lavaan)
#Factor 1 and 2
model1 <- ' Development_Level =~ phones_p100 + children_p_woman + life_exp_yrs + corruption_CPI + internet + child_mort_p1000 
Population_Level =~ urban_pop_tot + pop_total '
d1 <- cleaned
d1 <- d1[,c(-1,-2,-4,-5,-9,-11,-15,-16,-17)]
colnames(d1)[which(colnames(d1) %in% c("internet_%of_pop") )] <- c("internet")

fit <- lavaan::cfa(model1, data = log(d1))
summary(fit)


#Factor 1 and Factor 4
d2 <- cleaned
colnames(d2)
d2 <- d2[,c(-1,-2,-3,-4,-5,-9,-10,-16,-17)]
colnames(d2)[which(colnames(d2) %in% c("internet_%of_pop") )] <- c("internet")
model2 <- ' Development_Level =~ phones_p100 + children_p_woman + life_exp_yrs + corruption_CPI + internet + child_mort_p1000 
Gender_Income =~ sex_ratio_p100 + income_per_person '

fit2 <- lavaan::cfa(model2, data = log(d2))
summary(fit2)

#Factor 1 and 3
d3 <- cleaned
colnames(d3)
d3 <- d3[,c(-1,-2,-3,-5,-9,-10,-11,-16)]
colnames(d3)[which(colnames(d3) %in% c("internet_%of_pop") )] <- c("internet")
model3 <- ' Development_Level =~ phones_p100 + children_p_woman + life_exp_yrs + corruption_CPI + internet + child_mort_p1000 
Inequality_Level =~ murder_pp + gini  '

fit3 <- lavaan::cfa(model3, data = log(d3))
summary(fit3)


#4 Factors
d <- cleaned
d <- d[,c(-1,-2,-5,-9,-16)]
colnames(d)[which(colnames(d) %in% c("internet_%of_pop") )] <- c("internet")

model4 <- ' Development_Level =~ phones_p100 + children_p_woman + life_exp_yrs + corruption_CPI + internet + child_mort_p1000 
Population_Level =~ urban_pop_tot + pop_total
Inequality_Level =~ murder_pp + gini 
Gender_Income =~ sex_ratio_p100 + income_per_person '

fit <- lavaan::cfa(model4, data = log(d))
varTable(fit)
summary(fit)

