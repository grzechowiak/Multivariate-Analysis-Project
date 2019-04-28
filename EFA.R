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
  factor_rms(num_data, 1, corr)
  factor_rms(num_data, 2, corr)
  factor_rms(num_data, 3, corr)
  factor_rms(num_data, 4, corr)
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
  country_colors[wrld_simpl@data$NAME %in% gr1] <- "#3288bd"
  country_colors[wrld_simpl@data$NAME %in% gr2] <- "#fc8d59"
  country_colors[wrld_simpl@data$NAME %in% gr3] <- "#91cf60" #Singapore, Qatar, Luxembourg & Bunei are too small to observe in graph
  country_colors[wrld_simpl@data$NAME %in% gr4] <- "#fee08b"
  colors <- c("#3288bd", "#fc8d59", "#91cf60", "#fee08b")
  sort(wrld_simpl@data$NAME)
  plot(wrld_simpl, col = country_colors)
  legend("bottomleft", fill=colors, legend=c("Developed", "Crowded", "Inequality", "Gender/Income"), col=colors, cex=0.5)
  
}  

