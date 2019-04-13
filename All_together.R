#Read Libraries
library(readr)
library(readxl)
library(dplyr)

#Read data
population_tot <- read_csv("population_total.csv")
murder_total_deaths <- read_csv("murder_total_deaths edited.csv")
armed_forces_personnel_total <- read_csv("armed_forces_personnel_total.csv")
cell_phones <- read_csv("cell_phones_per_100_people.csv")
children_per_woman <- read_csv("children_per_woman_total_fertility.csv")
life_expectancy_years <- read_csv("life_expectancy_years.csv")
suicide_total_deaths <- read_csv("suicide_total_deaths.csv")
urban_population <- read_csv("urban_population.csv")
sex_ratio <- read_csv("sex_ratio_all_age_groups.csv")
corruption <- read_csv("corruption_perception_index_cpi.csv")
internet_users <- read_csv("internet_users.csv")
internet_users <- read_csv("internet_users.csv")
child_mortality <- read_csv("child_mortality_0_5_year_olds_dying_per_1000_born.csv")
income_per_person <- read_csv("income_per_person_gdppercapita_ppp_inflation_adjusted.csv")
investments_per_ofGDP <- read_csv("investments_percent_of_gdp.csv")
gini <- read_csv("gini.csv")



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



########################

######NA'S
#Check numbers of NA's
summary(data)

###### CORRELATIONS
# simple matrix correlation plot
plot(data[,-1])

# more fency correlation matrix plot
pairs(data[,-1],
      panel = function (x, y, ...) {
        points(x, y, ...)
        abline(lm(y ~ x), col = "red")
      }, pch = ".", cex = 1.5)

# super fency correlation
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(data[,-1], histogram=TRUE, pch=42)

# correlation matrix (numbers)
cor(data[,-1], use = "complete.obs")
cor(data[,-1], use = "pairwise.complete.obs")


#Check PC
data_test <- na.omit(data)
data
a <- princomp(data_test[,-1], cor=T)
summary(a, loadings=T)





# HEre is a test where I deleted few columns.
# See how PC increases/descreases while deleting less correlated columns
## TEST PCA WITHOUT FEW COLUMNS
test <- data2[,-c(2,4,9,10,15)] ## optioaly delete murder_pp
test2 <- na.omit(test)
b <- princomp(test2[,-1], cor=T)
summary(b, loadings=T)

chart.Correlation(test[,-1], histogram=TRUE, pch=42)
