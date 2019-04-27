#EFA analysis

factor_rms <- function(data, fact, corr) {
  data.fa <- factanal(data, factors = fact)
  f.loadings <- data.fa$loadings[,1:fact]
  corHat <- f.loadings %*% t(f.loadings) + diag(data.fa$uniquenesses)
  rmse = sqrt(mean((corHat - corr)^2))
  print(rmse)
}	

find_EFA <- function(data_input=NULL) {
  data <- cleaned
  num_data <- data[, c(-1,-2)]
  corr <- cor(num_data)
  print(factor_rms(num_data, 1, corr))
  print(factor_rms(num_data, 2, corr))
  print(factor_rms(num_data, 3, corr))
  print(factor_rms(num_data, 4, corr))
}

EFA <- function(data_input=NULL) {
  data <- cleaned
  num_data <- data[, c(-1,-2)]
  data.fa <- factanal(num_data, factors = 4, scores = "regression")
  print(data.fa$loadings, cut=0.4)
  scores <- data.fa$scores
  scores <- as.data.frame(scores)
  row.names(scores) <- data$country
  
  gr1 <- tail(scores[order(scores$Factor1),],5)
  gr1$country <- rownames(gr1)
  gr1 <- as.vector(gr1$country)
  gr2 <- tail(scores[order(scores$Factor2),],5)
  gr2$country <- rownames(gr2)
  gr2 <- as.vector(gr2$country)
  gr3 <- tail(scores[order(scores$Factor3),],5)
  gr3$country <- rownames(gr3)
  gr3$country[1]='Brunei Darussalam' #change Brunei for Brunei Darussalam because in world data is with that name
  gr3 <- as.vector(gr3$country)
  gr4 <- tail(scores[order(scores$Factor4),],5)
  gr4$country <- rownames(gr4)
  gr4 <- as.vector(gr4$country)
  
  #Plot group 1
  library(maptools)
  data(wrld_simpl)
  
  country_colors <- setNames(rep("white", length(wrld_simpl@data$NAME)), wrld_simpl@data$NAME)
  country_colors[wrld_simpl@data$NAME %in% gr1] <- "red"
  country_colors[wrld_simpl@data$NAME %in% gr2] <- "blue"
  country_colors[wrld_simpl@data$NAME %in% gr3] <- "green" #Singapore, Qatar, Luxembourg & Bunei are too small to observe in graph
  country_colors[wrld_simpl@data$NAME %in% gr4] <- "yellow"
  
  sort(wrld_simpl@data$NAME)
  plot(wrld_simpl, col = country_colors)
  legend(3.9,6.1, fill=colors, legend=), col=colors, cex=0.5)
  
}  
find_EFA(cleaned)
EFA(cleaned)