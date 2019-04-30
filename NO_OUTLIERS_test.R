remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

colnames(cleaned)
no_out <-  NULL
no_out$pop_total <- remove_outliers(cleaned[,3])
no_out <- as.data.frame(no_out)
no_out$murder_pp <- remove_outliers(cleaned[,4])
no_out$armed_pp <- remove_outliers(cleaned[,5])
no_out$phones_p100 <- remove_outliers(cleaned[,6])
no_out$children_p_woman <- remove_outliers(cleaned[,7])
no_out$life_exp_yrs <- remove_outliers(cleaned[,8])
no_out$suicide_pp <- remove_outliers(cleaned[,9])
no_out$urban_pop_tot <- remove_outliers(cleaned[,10])
no_out$sex_ratio_p100 <- remove_outliers(cleaned[,11])
no_out$corruption_CPI <- remove_outliers(cleaned[,12])
no_out$internet <- remove_outliers(cleaned[,13])
no_out$child_mort_p1000 <- remove_outliers(cleaned[,14])
no_out$income_per_person <- remove_outliers(cleaned[,15])
no_out$investments_per_ofGDP <- remove_outliers(cleaned[,16])
no_out$gini <- remove_outliers(cleaned[,17])


#missing values
for(q in 1:ncol(no_out)){
  no_out[is.na(no_out[, q]), q] <- median(no_out[, q], na.rm = TRUE) 
}
summary(no_out)

saveRDS(no_out, file = "forMika.rds")
