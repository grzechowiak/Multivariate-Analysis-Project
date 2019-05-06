library(sem)
library(dplyr)

data <- cleaned
cfa_data <- subset(data, select=-c(continent,country, investments_per_ofGDP, armed_pp))

colnames(cfa_data)[which(colnames(cfa_data) %in% c("internet_%of_pop") )] <- c("internet")

cfa_model <- specifyModel(text = "
                          Factor1 -> phones_p100, lambda1, NA
                          Factor1 -> life_exp_yrs, lambda2, NA  
                          Factor1 -> corruption_CPI, lambda3, NA                
                          Factor1 -> internet, lambda4, NA                           
                          Factor1 -> children_p_woman, lambda5, NA                        
                          Factor1 -> child_mort_p1000, lambda6, NA              
                          Factor2 -> murder_pp, lambda7, NA   
                          Factor2 -> gini, lambda8, NA
                          Factor3 -> suicide_pp, lambda9, NA               
                          Factor4 -> sex_ratio_p100, lambda10, NA  
                          Factor4 -> income_per_person, lambda11, NA
                          Factor1 <-> Factor2, rho0, NA
                          Factor1 <-> Factor3, rho1, NA
                          Factor1 <-> Factor4, rho2, NA
                          Factor2 <-> Factor3, rho3, NA
                          Factor2 <-> Factor4, rho4, NA
                          Factor3 <-> Factor4, rho5, NA
                          phones_p100 <-> phones_p100, theta1, NA
                          life_exp_yrs <-> life_exp_yrs, theta2, NA
                          corruption_CPI <-> corruption_CPI, theta3, NA
                          internet <-> internet, theta4, NA
                          children_p_woman <-> children_p_woman, theta5, NA
                          child_mort_p1000 <-> child_mort_p1000, theta6, NA
                          sex_ratio_p100 <-> sex_ratio_p100, theta7, NA
                          income_per_person <-> income_per_person, theta8, NA
                          murder_pp <-> murder_pp, theta9, NA
                          gini <-> gini, theta10, NA,
                          suicide_pp <-> suicide_pp, theta11, NA
                          Factor1 <-> Factor1, NA, 1
                          Factor2 <-> Factor2, NA, 1
                          Factor3 <-> Factor3, NA, 1
                          Factor4 <-> Factor4, NA, 1")
cfa_sem <- sem::sem(cfa_model, cor(cfa_data), nrow(cfa_data), debug = T)
#ERROR: coefficient covariances cannot be computed

summary(cfa_sem)

library(lavaan)
model <- 'Development_Level =~ phones_p100 + children_p_woman + life_exp_yrs + corruption_CPI + internet + child_mort_p1000 + income_per_person
Suicide_Level =~ suicide_pp
Inequality_Level =~ murder_pp + gini 
Gender_Income =~ sex_ratio_p100 + income_per_person'
fit <- lavaan::cfa(model, data = cfa_data)
#ERROR: some observed variances are (at least) a factor 1000 times larger than others; use varTable(fit)

fit <- lavaan::cfa(model, data = scale(cfa_data))
summary(fit)
fitMeasures(fit, c("GFI", "AGFI", "SRMR"))

fit2 <- lavaan::cfa(model, data = log(cfa_data))
summary(fit2)
fitMeasures(fit2, c("GFI", "AGFI", "SRMR"))




