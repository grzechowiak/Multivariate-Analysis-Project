
library(sem)
library(readr)

cfa_data <- read_csv("cor_data.csv")
cfa_data <- cfa_data[,c(-1)]
class(cfa_data[1,1])
row.names(cfa_data) <- colnames(cfa_data) 
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
saveRDS(cor_data, file = "my_cor.rds")
