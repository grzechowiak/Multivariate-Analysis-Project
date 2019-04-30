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
EFA_loadings <- function(data_input=NULL) {
  data <- cleaned
  num_data <- data[, c(-1,-2)]
  #Change colnames
  colnames(num_data)[which(colnames(num_data) %in% c("pop_total", "murder_pp","armed_pp",
    "phones_p100", "children_p_woman","life_exp_yrs","suicide_pp","sex_ratio_p100",
    "corruption_CPI","internet_%of_pop","child_mort_p1000","income_per_person",
    "gini", "investments_per_ofGDP", "urban_pop_tot") )] <- c("POPULATION", "MURDER", "ARMED","PHONES","CHILDREN","LIFE EXP.","SUICIDE","SEX RATIO","LESS CORRUPTION", 
                    "INTERNET","CHILD MORT.","INCOME","INEQUALITY", "INVESTMENT", "URBAN POPULATION")
  data.fa <- factanal(num_data, factors = 4, scores = "regression")
  print(data.fa$loadings, cut=0.4)
}

EFA_plot <- function(data_input=NULL) {
  data <- cleaned
  num_data <- data[, c(-1,-2)]
  data.fa <- factanal(num_data, factors = 4, scores = "regression")
  scores <- data.fa$scores
  scores <- as.data.frame(scores)
  row.names(scores) <- data$country
  
  gr1 <- tail(scores[order(scores$Factor1),],10)
  gr1$country <- rownames(gr1)
  gr1 <- as.vector(gr1$country)
  gr2 <- tail(scores[order(scores$Factor2),],10)
  gr2$country <- rownames(gr2)
  gr2 <- as.vector(gr2$country)
  gr3 <- tail(scores[order(scores$Factor3),],10)
  gr3$country <- rownames(gr3)
  gr3$country[1]='Brunei Darussalam' #change Brunei for Brunei Darussalam because in world data is with that name
  gr3 <- as.vector(gr3$country)
  gr4 <- tail(scores[order(scores$Factor4),],10)
  gr4$country <- rownames(gr4)
  gr4 <- as.vector(gr4$country)
  
  groups <- list(gr1, gr2, gr3, gr4)

  #Plot group 1
  library(maptools)
  data(wrld_simpl)
  
  country_colors <- setNames(rep(gray(.80), length(wrld_simpl@data$NAME)), wrld_simpl@data$NAME)
  country_colors[wrld_simpl@data$NAME %in% gr1] <- "#91cf60"
  country_colors[wrld_simpl@data$NAME %in% gr2] <- "#3288bd"
  country_colors[wrld_simpl@data$NAME %in% gr3] <- "#fee08b" #Singapore, Qatar, Luxembourg & Bunei are too small to observe in graph
  country_colors[wrld_simpl@data$NAME %in% gr4] <- "#80cdc1"
  colors <- c("#91cf60", "#3288bd", "#fee08b", "#80cdc1", gray(.80))
  sort(wrld_simpl@data$NAME)
  EFA_plot <- plot(wrld_simpl, col = country_colors)
  EFA_plot <-  legend(x=-180,y=15, inset=.09, title="",
                       fill=colors, legend=c("Developed", "Crowded", "High Inequality", "Gender/Income", 'NoData'), 
                      horiz=FALSE, cex=1.5, bg="transparent",bty = "n")
  return(groups)
  
}  
  

