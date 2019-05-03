
#What is inside:
# The function, read all csv files and merge them together. Later a new column 
# "continent" is created based on the column "country". On the end that is cleaned.

Read_Clean <- function(data_input=NULL){


  

######## ######## ######## ######## ######## 
########## READ ALL DATA AND MERGE ########## 
######## ######## ######## ######## ######## 

#Read data
population_tot <- read_csv("data/population_total.csv")
murder_total_deaths <- read_csv("data/murder_total_deaths edited.csv")
armed_forces_personnel_total <- read_csv("data/armed_forces_personnel_total.csv")
cell_phones <- read_csv("data/cell_phones_per_100_people.csv")
children_per_woman <- read_csv("data/children_per_woman_total_fertility.csv")
life_expectancy_years <- read_csv("data/life_expectancy_years.csv")
suicide_total_deaths <- read_csv("data/suicide_total_deaths.csv")
#urban_population <- read_csv("data/urban_population.csv")
sex_ratio <- read_csv("data/sex_ratio_all_age_groups.csv")
corruption <- read_csv("data/corruption_perception_index_cpi.csv")
internet_users <- read_csv("data/internet_users.csv")
child_mortality <- read_csv("data/child_mortality_0_5_year_olds_dying_per_1000_born.csv")
income_per_person <- read_csv("data/income_per_person_gdppercapita_ppp_inflation_adjusted.csv")
investments_per_ofGDP <- read_csv("data/investments_percent_of_gdp.csv")
gini <- read_csv("data/gini.csv")
#popdensity <- read_csv("data/popdensity_sqkm_mod.csv")



#####DATA#####
#POPULATION TOTAL
population_tot <- select(population_tot, "country", "2016")
colnames(population_tot) <- c("country", "pop_total")


#MURDER per person (from total)
murder_total_deaths <- select(murder_total_deaths, "Country", "2016")
colnames(murder_total_deaths) <- c("country", "murder_total")
murder_total_deaths <- merge(population_tot, murder_total_deaths , by="country", all = TRUE)
murder_total_deaths$divided <- murder_total_deaths$murder_total/murder_total_deaths$pop_total
murder_total_deaths <- select(murder_total_deaths,"country",'divided')
colnames(murder_total_deaths) <- c("country","murder_pp")

#ARMED per person (from total)
armed_forces_personnel_total <- select(armed_forces_personnel_total, "country", "2016")
colnames(armed_forces_personnel_total) <- c("country", "armed_total")
armed_forces_personnel_total <- merge(population_tot, armed_forces_personnel_total , by="country", all = TRUE)
armed_forces_personnel_total$divided <- armed_forces_personnel_total$armed_total/armed_forces_personnel_total$pop_total
armed_forces_personnel_total <- select(armed_forces_personnel_total,"country",'divided')
colnames(armed_forces_personnel_total) <- c("country","armed_pp")


#CELL PHONES per 100
cell_phones <- select(cell_phones, "country", "2016")
colnames(cell_phones) <- c("country", "phones_p100")

#URBAN POPULATION
# urban_population<- select(urban_population, "country", "2016")
# colnames(urban_population) <- c("country", "urban_pop_tot")

#CHILDREN PER WOMAN
children_per_woman <- select(children_per_woman, "country", "2016")
colnames(children_per_woman) <- c("country", "children_p_woman")


#LIFE EXPECTANCY
life_expectancy_years <- select(life_expectancy_years, "country", "2016")
colnames(life_expectancy_years) <- c("country", "life_exp_yrs")


#SUICIDE DEATHS TOTAL
suicide_total_deaths <- select(suicide_total_deaths, "country", "2016")
colnames(suicide_total_deaths) <- c("country", "suicide_total")
suicide_total_deaths <- merge(population_tot, suicide_total_deaths , by="country", all = TRUE)
suicide_total_deaths$divided <- suicide_total_deaths$suicide_total/suicide_total_deaths$pop_total
suicide_total_deaths <- select(suicide_total_deaths,"country",'divided')
colnames(suicide_total_deaths) <- c("country","suicide_pp")


#CORRUPTION
corruption <- select(corruption, "country", "2016")
colnames(corruption) <- c("country", "corruption_CPI")


#INTERNET USERS (%of pop)
internet_users <- select(internet_users, "country", "2016")
colnames(internet_users) <- c("country", "internet_%of_pop")

#SEX RATIO IT IS 2015!!!!!! #man/women per 100
sex_ratio <- select(sex_ratio, "country", "2015")
colnames(sex_ratio) <- c("country", "sex_ratio_p100")

#CHILDREN MORTALITY
# 0-5 year olds dying per 1000 born
child_mortality <- select(child_mortality, "country", "2016")
colnames(child_mortality) <- c("country", "child_mort_p1000")

#INCOME PER PERSON
income_per_person <- select(income_per_person, "country", "2016")
colnames(income_per_person) <- c("country", "income_per_person")

#INVESTMENT AS A % OF DGP
investments_per_ofGDP <- select(investments_per_ofGDP, "country", "2016")
colnames(investments_per_ofGDP) <- c("country", "investments_per_ofGDP")

#GINI
gini <- select(gini, "country", "2016")
colnames(gini) <- c("country", "gini")

# #Population Density (sqkm)
# popdensity <- select(popdensity, 'Country Name', '2016')
# colnames(popdensity) <- c('country','Pop_Dens')



#Merge all
data <- merge(population_tot, murder_total_deaths, by="country", all = TRUE)
data <- merge(data, armed_forces_personnel_total , by="country", all = TRUE)
data <- merge(data, cell_phones , by="country", all = TRUE)
data <- merge(data, children_per_woman , by="country", all = TRUE)
data <- merge(data, life_expectancy_years , by="country", all = TRUE)
data <- merge(data, suicide_total_deaths , by="country", all = TRUE)
# data <- merge(data, urban_population , by="country", all = TRUE)
data <- merge(data, sex_ratio , by="country", all = TRUE)
data <- merge(data, corruption , by="country", all = TRUE)
data <- merge(data, internet_users , by="country", all = TRUE)
data <- merge(data, child_mortality, by="country", all = TRUE)
data <- merge(data, income_per_person, by="country", all = TRUE)
data <- merge(data, investments_per_ofGDP, by= "country", all=TRUE)
data <- merge(data, gini, by="country", all=TRUE)
# data <- merge(x=data, y=popdensity, by="country", all.x=TRUE)
#pop_density does not come from GapMinder and country names are a little bit
#different. Since all data but popdensity comes from GapMiner
#it is assumed that GapMinder is a main source, so we left join to
#GapMinder dataset.


#After all computation we get rid of pop_total, and from now use pop_density only:
data <- subset(data, select=-c(pop_total))


######## ######## ######## ######## ######## 
######## NEW COLUMN - CONTINENT ######## 
######## ######## ######## ######## ######## 

# Create new a column continent and divide countires by continent

#library(countrycode)
data$continent <- countrycode(sourcevar = data[, "country"],
                              origin = "country.name",
                              destination = "continent")

# Just rearanage columns to get continent before country
data <- data[,c(ncol(data),seq(1,ncol(data)-1,by=1))]

#The function above did not divide coutries into America North, Central, South so I did this by hand
subset(data, continent=='Americas') %>% select(country) %>% as.list()

America_North <- c("Canada","United States","Mexico")
America_South <- c("Peru","Ecuador","Chile","Bolivia","Venezuela","Paraguay","Brazil", 
                   "Colombia",  "Guyana", "Suriname", "Uruguay", "Argentina")

#Change the values from "Americas" into more specific part of America

for (i in 1:nrow(data)){
  ifelse(data$continent[i] == "Americas", 
         ifelse(data$country[i] %in% America_North,data$continent[i] <- 'North America',
                ifelse(data$country[i] %in% America_South,data$continent[i] <- 'South America',
                       data$continent[i] <- 'Central America')), data$country[i])
}





######## ######## ######## ######## ######## 
######## DATA CLEANING INSERT MEDIAN ######## 
######## ######## ######## ######## ######## 

#Each NA value is replaced by the median value for the continent. 

#make a list of column names
col_names <- as.vector(names(data))

#Group by continent and find median
x <- aggregate(. ~ continent, data=subset(data, select=-c(country)), FUN=median)

#Create a sign for opposite of %in%
'%ni%' <- Negate('%in%')

#Iterate thru columns and row. If NA spotted insert median, depending on continent.
for(i in 1:length(col_names)){
  if (col_names[i] %ni% c("continent","country")) {
    
    for (j in 1:nrow(data)){
      if(is.na(data[j,i])){
        data[j,i] <- x[x$continent==data$continent[j],i-1]
      }
    }
  }
}





######## ######## ######## ######## ######## 
############# CHECKING OUTLIERS ############# 
######## ######## ######## ######## ######## 


## This plot in report
 par(mfrow=c(1,3))
 #income_per_person
with(data, Boxplot(income_per_person, id=list(labels=data$country,cex=2),main="Income Per Person", cex.main=2.5))

#Sex_Ratio_p100
with(data, Boxplot(sex_ratio_p100, id=list(labels=data$country,cex=2),main="Sex Ratio Per 100 People", cex.main=2.5))

#Gini
with(data, Boxplot(gini, id=list(labels=data$country,cex=2),main="GINI Ratio", cex.main=2.5))




## This saved in the Appendix (commented here)
#library(car)
# uncomment in order to get a matrix with all boxplots
layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), nrow = 3, ncol = 4, byrow = TRUE))

# #income_per_person
with(data, Boxplot(income_per_person, id=list(labels=data$country,cex=1.3),main="Income Per Person", cex.main=2))

#Sex_Ratio_p100
with(data, Boxplot(sex_ratio_p100, id=list(labels=data$country,cex=1.3),main="Sex Ratio Per 100 People", cex.main=2))

# #investments_per_ofGDP
with(data, Boxplot(investments_per_ofGDP, id=list(labels=data$country,cex=1.5),main="Investments Percentage of GDP", cex.main=2))

#Gini
with(data, Boxplot(gini, id=list(labels=data$country,cex=1.3),main="GINI Ratio", cex.main=2))



# Below are rest of the boxplot, but we just want to present an example

#child_mort_p1000
with(data, Boxplot(child_mort_p1000, id=list(labels=data$country,cex=1.3),main="Child Mortality Rate per 1000", cex.main=2))
# 
# #Murder_PP Column
# with(data, Boxplot(murder_pp, id=list(labels=data$country,cex=1.3),main="Murder Per Person", cex.main=2))
# 
# #Armed_PP Column
with(data, Boxplot(armed_pp, id=list(labels=data$country,cex=1.3),main="Armed Per Person", cex.main=2))
# 
# #Phones_p100 Column
with(data, Boxplot(phones_p100, id=list(labels=data$country,cex=1.3),main="Phones Per 100 People", cex.main=2))
# 
# #children_p_woman Column
with(data, Boxplot(children_p_woman, id=list(labels=data$country,cex=1.3),main="Children Per Woman", cex.main=2))
# 

# #Life_Exp_Yrs Column
with(data, Boxplot(life_exp_yrs, id=list(labels=data$country,cex=1.3),main="Life Expectancy in Years", cex.main=2))
# 
# #Suicide_PP Column
with(data, Boxplot(suicide_pp, id=list(labels=data$country,cex=1.3),main="Suicide Per Person", cex.main=2))
# 

# #Internet_%0f_pop
with(data, Boxplot(`internet_%of_pop`, id=list(labels=data$country,cex=1.3),main="Internet Usage Perceantage of Population", cex.main=2))
# 
# #Corruption_CPI
with(data, Boxplot(corruption_CPI, id=list(labels=data$country,cex=1.3),main="Corruption CPI Index", cex.main=2))
# 
# 


## Check by Histograms

# uncomment to check histograms:


# hist(data$murder_pp)
# hist(data$armed_pp)
# hist(data$phones_p100)
# hist(data$children_p_woman)
# hist(data$life_exp_yrs)
# hist(data$suicide_pp)

# hist(data$sex_ratio_p100)
# hist(data$corruption_CPI)
# hist(data$`internet_%of_pop`)
# hist(data$child_mort_p1000)
# hist(data$income_per_person)
# hist(data$investments_per_ofGDP)
# hist(data$gini)




return(data)
} #end of the function