


col_names <- as.vector(names(cleaned))
x <- aggregate(. ~ continent, data=cleaned[,c(-2)], FUN=median)

'%ni%' <- Negate('%in%')

for(i in 1:length(col_names)){
  if (col_names[i] %ni% c("continent","country")) {

    for (j in 1:nrow(cleaned)){
      if(is.na(cleaned[j,i])){
        cleaned[j,i] <- x[x$continent==cleaned$continent[j],2]
      }
    }
  }
}



#1
df.new = as.data.frame(lapply(cleaned, function(x) ifelse(is.na(x), 0, print(x))))

#2
lapply(cleaned,print(cleaned[1,1]))  
