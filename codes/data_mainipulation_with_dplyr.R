
# Installation ------------------------------------------------------------

install.packages("dplyr")
install.packages("gapminder")
install.packages("rio")


# Load packages -----------------------------------------------------------

library(dplyr)
library(gapminder)
gapminder ; ?gapminder
gapminder <- gapminder
glimpse(gapminder) # same as str(), get a glimpse of the data

# Import and export data --------------------------------------------------

library(rio)
library(haven)
# Use rio::import to load data/ import data
# Use rio::export to write/ export data
# Use haven::read_ to load/ import data
# Use haven::write_ write/ export data
lowbwt <- import("C:/docs/R_teaching/data/lowbwt_csv3.csv")
lowbwt <- read_dta("C:/docs/R_teaching/data/lowbwt_dta2.dta")

# Selecting column --------------------------------------------------------

# We can use %>% to pipeline the data frame/ tibble.
# %>% (pipeline) gives us the tibble and this makes it easy for us to manipulate data.
# We can use select() function to select the columns we want to use

gapminder %>% select(country, lifeExp) # select country and life expectancy
gapminder %>% select(1,4)
gapminder[,c(1,4)]

gapminder %>% select(-continent) # select all except continent
gapminder %>% select(-2)
gapminder[,-2]

gapminder %>% select(starts_with("c")) #select all start with "c"
# We can get helper functions with tidyselect::
gapminder %>% select(tidyselect::everything())
# Homework: Test the helper functions how they work

# Positioning the columns -------------------------------------------------

#We use relocate() function to position and arrange the columns
gapminder %>% relocate(year, .before = NULL) # in front of all columns
gapminder %>% relocate(year, .after = last_col()) # behind all columns
gapminder %>% 
  relocate(year, .before = NULL) %>% 
  relocate(pop, .after = gdpPercap)

# Renaming the columns ------------------------------------------------------

# We use rename() function to rename the variables with the following syntax
# data %>% rename(<new_name> = <old_name>)
gapminder %>% rename(life_expectency = lifeExp, population_size = pop)
names(gapminder)[5] <- "population_size" ; gapminder

# Making new variable -----------------------------------------------------

gapminder <- gapminder::gapminder
# We use mutate() function to create new column based on existing ones with the syntax
# data %>% mutate(<new_var> = <expression>, .keep = "<var>", .before = <var>/.after = <var>)

# Here we will create gdp variable from gdp per capital and population size
gapminder %>% mutate(gdp = gdpPercap*pop)

# Then we will relocate it before population size variable
gapminder %>% mutate(gdp = gdpPercap*pop, .before = pop)

# Last we will drop population and gdp per capital 
gapminder %>% mutate(gdp = gdpPercap*pop, .before = pop, .keep = "unused")

# transmute() only keep the generated new column
gapminder %>% transmute(gdp = gdpPercap*pop) 

# Please note that we need to save every result tibble so that we can use later
gdp <- gapminder %>% transmute(gdp = gdpPercap*pop)
mean(gdp$gdp)

# We can also use conditional expression to mutate variables
signif(median(gapminder$lifeExp),2)
gapminder %>% mutate(le_gp = if_else(lifeExp > 61, "good","poor"))
gapminder %>% mutate(le_gp = case_when(lifeExp > 61 ~ 1,
                                       lifeExp <= 61 & lifeExp > 50 ~ 2,
                                       TRUE ~ 8))



# Filtering cases/row -----------------------------------------------------

# To select the cases/row we can use filter() with a condition
gapminder %>% filter(continent == "Asia", year > 2000)
# It is better to use logical operators: AND(&), OR(|),NOT(!)
gapminder %>% filter(continent == "Asia" & year > 2000)
gapminder %>% filter(continent == "Asia" | year >= 2000)
gapminder %>% filter(continent != "Asia" & lifeExp <= 30)
gapminder %>% filter(continent %in% c("Asia","Africa") & year > 1990)
gapminder %>% filter(!(continent %in% c("Asia","Africa")) & year > 1990 & between(lifeExp,50,60))

# While filter() selects row by condition, slice() select the rows by position
gapminder %>% filter(continent == "Europe") %>% slice(301:310) #row no. 301 to 310
gapminder %>% slice_tail(n=5) # select last 5 rows
gapminder %>% slice_head(n=10) # select first 10 rows
gapminder %>% slice_sample(n=20) # sampling 20 rows
gapminder %>% filter(year == 2002) %>% slice_max(gdpPercap,n = 10) # select top 10 gdp per cap
gapminder %>% filter(year == 2007) %>% slice_min(lifeExp, prop = 0.1) # select lowest life exp 10%
gapminder %>% filter(year == 2007) %>% slice_min(lifeExp, prop = 0.1) %>% arrange(desc(lifeExp))

# distinct() removes duplicate rows
gapminder %>% distinct(country, .keep_all = T) # if .keep_all is F, the other variable will be dropped
gapminder %>% distinct(continent, .keep_all = F) 


# Changing the value  --------------------------------------------

# We can use with mutate() and replace() to re-code the cases' values with the syntax:
# mutate(<var> = replace(<var>, <condition>, <value>))

gapminder %>% select(continent,lifeExp) %>%
  distinct(continent, .keep_all = T) %>% 
  mutate(continent = replace(continent, continent == "Asia", "Asian Region"))
# We got NA because the continent is a factor

gapminder %>% select(continent,lifeExp) %>%
  distinct(continent, .keep_all = T) %>% 
  mutate(continent = as.character(continent)) %>% 
  mutate(continent = replace(continent, continent == "Asia", "Asian Region"),
         continent = replace(continent, continent == "Europe", "European Region"),
         continent = replace(continent, continent == "Africa", "African Region"),
         continent = replace(continent, continent == "Americas", "American Region"),
         continent = replace(continent, continent == "Oceania", "Oceania Region")) %>% 
  mutate(continent = as.factor(continent))

# We can also use recode() for changing the value
# Syntax of recode() is inverse to rename(): recode(<var>,<old code> = <new code>)
gapminder %>% select(continent,lifeExp) %>%
  distinct(continent, .keep_all = T) %>% 
  mutate(continent = recode(continent,  "Asia" = "Asian Region"),
         continent = recode(continent,  "Europe" = "European Region"),
         continent = recode(continent,  "Africa" = "African Region"),
         continent = recode(continent,  "Americas" = "American Region"),
         continent = recode(continent,  "Oceania" = "Oceania Region"))

# Summarizing the data with summarize() ----------------------------------------------------

# Count categorical variables/values

gapminder %>% count(continent)
table(gapminder$continent) #base R

gapminder %>% count(continent) %>% mutate(Percent = prop.table(n)*100)
prop.table(table(gapminder$continent))*100 #base R

# Counting distinct values

n_distinct(gapminder$continent, gapminder$country)
gapminder %>% select(continent,country) %>% n_distinct()

# Counting NA
na <- sample(c(rep("a",5), rep(NA,3)), size = 100, replace = T)
sum(is.na(na))
sum(!is.na(na))

# Measure of Central tendency
gapminder %>% summarise(Mean = mean(lifeExp), Median = median(lifeExp))

# Logical vector summary
logi <- sample(c(rep(T,3), rep(F,3)), size =  100, replace = T)
mean(logi) # Proportion of TRUE
sum(logi) # Number of TRUE

# Order
gapminder %>% summarise(First = first(country), Last = last(country),
                        Seventy_seventh = nth(country,77))

# Ranking
gapminder %>% 
  summarise(Max = max(gdpPercap),
            Min = min(gdpPercap))
gapminder %>% summarise(
  Quartile = quantile(gdpPercap, probs = seq(0,1,0.25)))

quantile(gapminder$gdpPercap, probs = seq(0,1,0.25)) # base

broom::tidy(quantile(gapminder$gdpPercap, probs = seq(0,1,0.25)))

# Measure of dispersion
gapminder %>% summarize(
  IQR = IQR(pop),
  SD = sd(pop),
  Variance = var(pop)
)

# Grouping summary with group_by()
gapminder %>% filter(year ==2002) %>% 
  group_by(continent) %>% 
  summarise(Number_of_country_in_each_zone = n())

gapminder %>% filter(year ==2002) %>% 
  group_by(continent) %>% 
  summarise(Average_GDP = mean(gdpPercap)) %>% 
  arrange(desc(Average_GDP))

gapminder %>% filter(continent == "Oceania") %>% 
  group_by(country) %>% 
  summarise(Avg_lifeExp = mean(lifeExp))


# Joining data tables -----------------------------------------------------

# Load necessary data
rm(list = ls())
install.packages("nycflights13")
library(nycflights13)
library(dplyr)

flights # flight data departed NYC in 2013
airlines # Airline names and carrier code
weather # hourly weather data at NYC
planes # data of the planes organized by tailed number


# left_join(x/left, y/right) joins the two data sets by keeping the observations in the left data set.
# Note the data set before the pipeline " %>% " is left data set
flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2 %>% 
  left_join(airlines)
# Here the join_by/by argument is not defined, so the function use the column name common in both data set, which is "carrier"
# It is okay when there is only one key variable common in both data set.


flights2
weather
flights2 %>% left_join(weather)
# It joins matching all the common variables, but this is still okay. 

flights2 # here, year column means flight date year (2013)
planes # here, year column means the year the plane is manufactured.
# So we can't join the two tables using the year column common in both of them
# We see that the tailnum is the key variable for joining the two tables

flights2 %>% left_join(planes, 
                       by = "tailnum")
# Note the two year columns are suffixed with x and y to disambiguate them

# When the two data sets have different variable names, we can use our key variables
# The syntax is left_join(x, y, by = c("x" = "y"))
# This will match "x" variable in table x to "y" variable in table y

flights2 # Airport codes in both origin and destination
airports # Join with airport codes "faa" column as key
flights2 %>% left_join(airports, c("dest" = "faa")) # Use destination airport as key
flights2 %>% left_join(airports, c("origin" = "faa")) # Use departing airport as key

# Types of mutating join adding new columns
# 1. left_join(x,y) includes all observation in x
# 2. right_join(x,y) includes all observation in y
# 3. inner_join(x,y) includes only matching observation in x and y
# 4. full_join(x,y) includes all observation from both x and y

x <- tibble(A=c("a","b","c"),B=c("t","u","v"),C=c(1,2,3)) ;x
y <- tibble(D=c("a","b","d"),E=c(3,2,1)); y

left_join(x,y, by=join_by(A == D)) # NA in column E 
right_join(x,y, by = join_by(A == D)) # NA in column B and C 
right_join(y,x, by = join_by(D == A)) # same as left_join(x,y) but col order is different
inner_join(x,y, by = join_by(A == D)) # only matching row
full_join(x,y, by = join_by(A==D)) # all rows from both data sets


# Filtering join ----------------------------------------------------------

semi_join(x,y, by = join_by(A == D)) # return only row of x matching with y
semi_join(y,x, by = join_by(D == A)) # return only row of y matching with x

anti_join(x,y, by = c("A" ="D")) #return only row of x not matching with y
anti_join(y,x, by = c("D" ="A")) #return only row of y not matching with x


# Simple binding ----------------------------------------------------------

# Binding columns
# Columns will not be matched by ID or key.
# Columns must have same length.
# Rows must be checked before binding

x <- tibble(A=c("a","b","c"),B=c("t","u","v")) ;x
y <- tibble(C=c("a","b","d"),D=c(3,2,1)); y

bind_cols(x,y)

# Binding rows
# Columns must have same name and same data type
x <- tibble(A=c("a","b","c"),B=c("t","u","v")) ;x
y <- tibble(A=c("d","e","f"),B=c("x","y","z")); y
bind_rows(x,y)


rm(list=ls())
df1 <- tibble(x = c(1, 1, 2), y = 1:3)
df2 <- tibble(x = c(1, 1, 2), z = c("a", "b", "a"))

df1 ; df2

df1 %>% left_join(df2) # extra rows in third and fourth row
bind_cols(df1, df2) %>% select(1,2,4) # what we really want


