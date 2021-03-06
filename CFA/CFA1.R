library(sem)
library(dplyr)

data <- cleaned
cfa_data <- select(data, 'phones_p100', 'life_exp_yrs', 'corruption_CPI', 
                   'internet_%of_pop', 'children_p_woman', 'child_mort_p1000', 'urban_pop_tot', 'pop_total')
colnames(cfa_data)[which(colnames(cfa_data) %in% c("internet_%of_pop") )] <- c("internet")

cfa_model <- specifyModel(text = "
                        Factor1 -> phones_p100, lambda1, NA
                        Factor1 -> life_exp_yrs, lambda2, NA  
                          Factor1 -> corruption_CPI, lambda3, NA                
                          Factor1 -> internet, lambda4, NA                           
                          Factor1 -> children_p_woman, lambda5, NA                        
                          Factor1 -> child_mort_p1000, lambda6, NA              
                          Factor2 -> urban_pop_tot, lambda7, NA   
                          Factor2 -> pop_total, lambda8, NA
                          Factor3 -> sex_ratio_p100, lambda9, NA               
                          Factor3 -> income_per_person, lambda10, NA    
                          Factor4 -> murder_pp, lambda11, NA  
                          Factor4 -> gini, lambda12, NA
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
                          urban_pop_tot <-> urban_pop_tot, theta7, NA
                          pop_total <-> pop_total, theta8, NA
                          sex_ratio_p100 <-> sex_ratio_p100, theta9, NA
                          income_per_person <-> income_per_person, theta10, NA
                          murder_pp <-> murder_pp, theta11, NA
                          gini <-> gini, theta12, NA
                          Factor1 <-> Factor1, NA, 1
                          Factor2 <-> Factor2, NA, 1
                          Factor3 <-> Factor3, NA, 1
                          Factor4 <-> Factor4, NA, 1")
cfa_sem <- sem(cfa_model, cor(cfa_data), nrow(cfa_data), debug = T)
#ERROR:   maximum iterations exceeded
