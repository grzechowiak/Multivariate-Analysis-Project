#EFA analysis

factor_rms <- function(data, fact, corr) {
  data.fa <- factanal(data, factors = fact)
  f.loadings <- data.fa$loadings[,1:fact]
  corHat <- f.loadings %*% t(f.loadings) + diag(data.fa$uniquenesses)
  rmse = sqrt(mean((corHat - corr)^2))
  print(rmse)
}	

num_data <- data[, c(-1,-2)]
corr <- cor(num_data)
factor_rms(num_data, 1, corr)
factor_rms(num_data, 2, corr)
factor_rms(num_data, 3, corr)
factor_rms(num_data, 4, corr)

data.fa <- factanal(num_data, factors = 4, scores = "regression")
data.fa$loadings
print(data.fa$loadings, cut=0.4)
scores <- data.fa$scores
scores <- as.data.frame(scores)
row.names(scores) <- data$country

### GROUP 1 #### 
gr1 <- tail(scores[order(scores$Factor1),],5)
gr1$country <- rownames(gr1)
gr1 <- as.vector(unlist(gr1$country))
gr2 <- tail(scores[order(scores$Factor2),],5)
gr2$country <- rownames(gr2)
gr2 <- as.vector(unlist(gr2$country))

#Plot group 1
library(maptools)
data(wrld_simpl)

country_colors <- setNames(rep("white", length(wrld_simpl@data$NAME)), wrld_simpl@data$NAME)
country_colors[wrld_simpl@data$NAME %in% gr1] <- "red"
country_colors[wrld_simpl@data$NAME %in% gr2] <- "blue"
country_colors[wrld_simpl@data$NAME %in% gr3] <- "green"
country_colors[wrld_simpl@data$NAME %in% gr4] <- "yellow"

sort(wrld_simpl@data$NAME)
plot(wrld_simpl, col = country_colors)


