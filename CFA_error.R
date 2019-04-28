
library(sem)
library(dplyr)
cfa_data <- readRDS("my_cor.rds")
cfa_data <- cfa_data[,c(-1,-2,-4,-5,-9,-11,-15,-16,-17)]
cfa_data <- select(cfa_data, 'phones_p100', 'life_exp_yrs', 'corruption_CPI', 
                   'internet_%of_pop', 'children_p_woman', 'child_mort_p1000', 'urban_pop_tot', 'pop_total')
as.data.frame(cfa_data)
cfa_model <- specifyModel(text = "
                          Factor1          -> phones_p100, lambda1, NA              
                          Factor1          -> life_exp_yrs, lambda2, NA  
                          Factor1          -> corruption_CPI, lambda3, NA                
                          Factor1          -> internet_%of_pop, lambda4, NA                           
                          Factor1          -> children_p_woman, lambda5, NA                        
                          Factor1          -> child_mort_p1000, lambda6, NA              
                          Factor2          -> urban_pop_tot, lambda7, NA   
                          Factor2          -> pop_total, lambda8, NA
                          Factor1          <-> Factor2, rho0, NA
                          phones_p100      <-> phones_p100, theta1, NA
                          life_exp_yrs     <-> life_exp_yrs, theta2, NA
                          corruption_CPI   <-> corruption_CPI, theta3, NA
                          internet_%of_pop <-> internet_%of_pop, theta4, NA
                          children_p_woman <-> children_p_woman, theta5, NA
                          child_mort_p1000 <-> child_mort_p1000, theta6, NA
                          urban_pop_tot    <-> urban_pop_tot, theta7, NA
                          pop_total        <-> pop_total, theta8, NA
                          Factor1          <-> Factor1, NA, 1
                          Factor2          <-> Factor2, NA, 1")
cfa_sem <- sem(cfa_model, cfa_data, 195, debug = T)
summary(cfa_sem)




cor_data <- cor(cfa_data[,c(-1,-2)])

