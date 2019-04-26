#Read Libraries
library(readr)
library(readxl)
library(dplyr)

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
urban_population <- read_csv("data/urban_population.csv")
sex_ratio <- read_csv("data/sex_ratio_all_age_groups.csv")
corruption <- read_csv("data/corruption_perception_index_cpi.csv")
internet_users <- read_csv("data/internet_users.csv")
internet_users <- read_csv("data/internet_users.csv")
child_mortality <- read_csv("data/child_mortality_0_5_year_olds_dying_per_1000_born.csv")
income_per_person <- read_csv("data/income_per_person_gdppercapita_ppp_inflation_adjusted.csv")
investments_per_ofGDP <- read_csv("data/investments_percent_of_gdp.csv")
gini <- read_csv("data/gini.csv")



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
urban_population<- select(urban_population, "country", "2016")
colnames(urban_population) <- c("country", "urban_pop_tot")

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



#Merge all
data <- merge(population_tot, murder_total_deaths, by="country", all = TRUE)
data <- merge(data, armed_forces_personnel_total , by="country", all = TRUE)
data <- merge(data, cell_phones , by="country", all = TRUE)
data <- merge(data, children_per_woman , by="country", all = TRUE)
data <- merge(data, life_expectancy_years , by="country", all = TRUE)
data <- merge(data, suicide_total_deaths , by="country", all = TRUE)
data <- merge(data, urban_population , by="country", all = TRUE)
data <- merge(data, sex_ratio , by="country", all = TRUE)
data <- merge(data, corruption , by="country", all = TRUE)
data <- merge(data, internet_users , by="country", all = TRUE)
data <- merge(data, child_mortality, by="country", all = TRUE)
data <- merge(data, income_per_person, by="country", all = TRUE)
data <- merge(data, investments_per_ofGDP, by= "country", all=TRUE)
data <- merge(data, gini, by="country", all=TRUE)





######## ######## ######## ######## ######## 
######## NEW COLUMN - CONTINENT ######## 
######## ######## ######## ######## ######## 

# Create new a column continent and divide countires by continent

library(countrycode)
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
######## INSERT MEDIAN ######## 
######## ######## ######## ######## ######## 


for(q in 1:ncol(data)){
  data[is.na(data[, q]), q] <- median(data[, q], na.rm = TRUE) 
}