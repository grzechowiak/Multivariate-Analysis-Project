factor_rms <- function(data, fact, corr) {
  data.fa <- factanal(data, factors = fact)
  f.loadings <- data.fa$loadings[,1:fact]
  corHat <- f.loadings %*% t(f.loadings) + diag(data.fa$uniquenesses)
  rmse = sqrt(mean((corHat - corr)^2))
  print(rmse)
}	

data <- cleaned
num_data <- data[, c(-1,-2,-3,-4,-5,-10,-16,-17)]
colnames(num_data)
colnames(num_data)[which(colnames(num_data) %in% c("internet_%of_pop") )] <- c("internet")

corr <- cor(num_data)
factor_rms(num_data, 1, corr)
factor_rms(num_data, 2, corr)
factor_rms(num_data, 3, corr)

#3 factors 
fa <- factanal(num_data, factors = 3)
print(fa$loadings, cut=0.5)

cfa_model <- specifyModel(text = "
Factor1 -> phones_p100, l1, NA
Factor1 -> children_p_woman, l2, NA
Factor1 -> life_exp_yrs, l3, NA
Factor1 -> corruption_CPI, l4, NA
Factor1 -> internet, l5, NA
Factor1 -> child_mort_p1000, l6, NA
Factor2 -> sex_ratio_p100, l7, NA
Factor2 -> income_per_person, l8, NA
Factor3 -> suicide_pp, l9, NA
Factor1 <-> Factor2, rho1, NA
Factor1 <-> Factor3, rho2, NA
Factor2 <-> Factor3, rho3, NA
phones_p100 <-> phones_p100, t1, NA
children_p_woman <-> children_p_woman, t2, NA
life_exp_yrs <-> life_exp_yrs, t3, NA
corruption_CPI <-> corruption_CPI, t4, NA
internet <-> internet, t5, NA
child_mort_p1000 <-> child_mort_p1000, t6, NA
sex_ratio_p100 <-> sex_ratio_p100, t7, NA
income_per_person <-> income_per_person, t8, NA
suicide_pp <-> suicide_pp, t9, NA
Factor1 <-> Factor1, NA, 1
Factor2 <-> Factor2, NA, 1
Factor3 <-> Factor3, NA, 1")
library(sem)
# Negative parameter variances.
c <- sem::sem(cfa_model, cor(num_data), nrow(num_data))
summary(c)

library(lavaan)
model <- 'F1 =~ phones_p100 + children_p_woman + life_exp_yrs + corruption_CPI + internet + child_mort_p1000
          F2 =~ sex_ratio_p100 + income_per_person
          F3 =~ suicide_pp'

#lavaan WARNING: some observed variances are larger than 1000000
fit <- lavaan::cfa(model , data = num_data)
varTable(fit)

#lavaan WARNING: the optimizer warns that a solution has NOT been found!
fit2 <- lavaan::cfa(model , data = log(num_data))
varTable(fit2)

#2 factors 
fa2 <- factanal(num_data, factors = 2)
print(fa2$loadings, cut=0.5)
num_data2 <- num_data[,c(-4)]
cfa_model2 <- specifyModel(text = "
                          Factor1 -> phones_p100, l1, NA
                          Factor1 -> children_p_woman, l2, NA
                          Factor1 -> life_exp_yrs, l3, NA
                          Factor1 -> corruption_CPI, l4, NA
                          Factor1 -> internet, l5, NA
                          Factor1 -> child_mort_p1000, l6, NA
                          Factor2 -> sex_ratio_p100, l7, NA
                          Factor2 -> income_per_person, l8, NA
                          Factor1 <-> Factor2, rho1, NA
                          phones_p100 <-> phones_p100, t1, NA
                          children_p_woman <-> children_p_woman, t2, NA
                          life_exp_yrs <-> life_exp_yrs, t3, NA
                          corruption_CPI <-> corruption_CPI, t4, NA
                          internet <-> internet, t5, NA
                          child_mort_p1000 <-> child_mort_p1000, t6, NA
                          sex_ratio_p100 <-> sex_ratio_p100, t7, NA
                          income_per_person <-> income_per_person, t8, NA
                          Factor1 <-> Factor1, NA, 1
                          Factor2 <-> Factor2, NA, 1")

c2 <- sem::sem(cfa_model2, cor(num_data2), nrow(num_data2))

options(fit.indices=c("GFI", "AGFI", "SRMR"))
criteria <- summary(c2)
criteria$GFI
criteria$AGFI
criteria$SRMR
criteria
