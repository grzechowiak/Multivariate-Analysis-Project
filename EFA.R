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

data.fa <- factanal(num_data, factors = 4)
data.fa$loadings
print(data.fa$loadings, cut=0.4)



