


col_names <- as.vector(names(cleaned))
x <- aggregate(. ~ continent, data=cleaned[,c(-2)], FUN=median)

'%ni%' <- Negate('%in%')

for(i in 1:length(col_names)){
  if (col_names[i] %ni% c("continent","country")) {

    for (j in 1:nrow(cleaned)){
      if(is.na(cleaned[j,i])){
        cleaned[j,i] <- x[x$continent==cleaned$continent[j],i-1]
      }
    }
  }
}

