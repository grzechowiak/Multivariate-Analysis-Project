###CFA Attempt 2
################
library("sem")
data1 <- cleaned[,c(-1,-2,-4,-5, -9, -11, -16, -17)]
x <- cor(data1)
model1 <- specifyModel(text = "
Factor1           -> phones_p100,       lambda1, NA
Factor1           -> children_p_woman,  lambda2, NA
Factor1           -> life_exp_yrs,      lambda3, NA
Factor1           -> corruption_CPI,    lambda4, NA
Factor1           -> internet_%of_pop,  lambda5, NA
Factor1           -> child_mort_p1000,  lambda6, NA
Factor2           -> pop_total,         lambda7, NA
Factor2           -> urban_pop_tot,     lambda8, NA
Factor1           -> income_per_person,     lambda9, NA
Factor1           <-> Factor2,          rho, NA
phones_p100       <-> phones_p100,      theta1, NA
children_p_woman  <-> children_p_woman, theta2, NA
life_exp_yrs      <-> life_exp_yrs,     theta3, NA
corruption_CPI    <-> corruption_CPI,   theta4, NA
internet_%of_pop  <-> internet_%of_pop, theta5, NA
child_mort_p1000  <-> child_mort_p1000, theta6, NA
pop_total         <-> pop_total,        theta7, NA
urban_pop_tot     <-> urban_pop_tot,    theta8, NA
income_per_person     <-> income_per_person,    theta9, NA
Factor1           <-> Factor1, NA, 1
Factor2           <-> Factor2, NA, 1
" )
colnames(cleaned)
model1_sem <- sem(model1, x, 195)



data2 <- select(cleaned,"murder_pp","sex_ratio_p100","income_per_person", "gini")
model2 <- specifyModel(text = "
Factor3           -> murder_pp,         lambda1, NA
Factor3           -> gini,              lambda2, NA
Factor4           -> sex_ratio_p100,    lambda3, NA
Factor4           -> income_per_person, lambda4, NA
Factor3           <-> Factor4,          rho, NA
murder_pp         <-> murder_pp,        theta1, NA
gini              <-> gini,             theta2, NA
sex_ratio_p100    <-> sex_ratio_p100,     theta3, NA
income_per_person <-> income_per_person,   theta4, NA
Factor3           <-> Factor3, NA, 1
Factor4           <-> Factor4, NA, 1
                       " )
model2_sem <- sem(model2, cor(data2), nrow(mydata2))





fit = cfa(text = "
Factor3           -> murder_pp,       lambda1, NA
          Factor3           -> gini,  lambda2, NA
          Factor4           -> sex_ratio_p100,      lambda3, NA
          Factor4           -> income_per_person,    lambda4, NA
          Factor3           <-> Factor4,          rho, NA
          murder_pp       <-> murder_pp,      theta1, NA
          gini  <-> gini, theta2, NA
          sex_ratio_p100      <-> sex_ratio_p100,     theta3, NA
          income_per_person    <-> income_per_person,   theta4, NA
          
          Factor3           <-> Factor3, NA, 1
          Factor4           <-> Factor4, NA, 1
          ", data = data2)






