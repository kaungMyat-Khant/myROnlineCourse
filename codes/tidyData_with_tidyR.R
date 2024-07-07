
# Require set up ----------------------------------------------------------

rm(list = ls())
library(tidyr)
library(dplyr)


# Wide to Long data -------------------------------------------------------

# When the values of observation are in the column names, we need to change the shape of data to long Format


# Categorical data in column name -----------------------------------------

# We will use Pew religion and income survey where annual income range are in column names

relig_income
# Religions are stored in rows. 
# Income are across the column names.
# Count of income ranges of each religion in the cell values.

relig_income %>% 
  pivot_longer(
    cols = !religion,
    names_to = "income",
    values_to = "count"
  )

# cols argument means columns which need to be reshaped.
# names_to argument means variable to which the reshaped column names go as value.
# values_to argument means variable to which values of the reshaped columns go as value.
# Note we put quotes("") around the new variable names which are not in data set


# Numerical data in column names ------------------------------------------

# We will use billboard data set for top 100 song ranking in 2000 where the week number are in column names 

billboard

# Artist's name, song track name and entry date into billboard in each row
# Week number after entry in each column 
# Note there are NA which mean the song get out of the billboard top 100

billboard %>% 
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank"
  )
# `8,785 missing values are generated for each song kicked out of top 100`

billboard %>% 
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  )
# Now NA are okay, but week numbers are in character format with "wk" and integer

billboard %>% 
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    names_transform = list(week = as.integer),
    values_to = "rank",
    values_drop_na = TRUE
  )

# Many variables in column names ------------------------------------------

# Multiple variables are crammed into a column name for display purpose such as
# WHO data sets

who 
# country, iso2, iso3 and year are already tidy variables.
# new_sp_m014 and so on have four variables inside them
# new is new case of TB

# sp* is diagnosis__
# rel is relapse; 
# sn is negative pulmonary smear; 
# sp is positive pulmonary smear; and
# ep is extrapulmonary. 
  
# m/f is male/female

# 014 is age group of 1-14, 1524 is 15-24 etc and 65 is 65 or older

# We have to use regular expression and name pattern = argument

who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = c("diagnosis", "gender", "age"), 
    names_pattern = "new_?(.*)_(.)(.*)",
    values_to = "count"
  )
# Explanation of regular expression
# ? means optional__ either new_ is in the column name or not
# () expression takes the column value from the string
# "." means single character from the string
# ".*" means a sequence of characters

# It's usable. Also it is a good practice to convert the variables with known set of value
# But here we need readr package function parse_factor()

who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = c("diagnosis", "gender", "age"), 
    names_pattern = "new_?(.*)_(.)(.*)",
    names_transform = list(
      gender = ~readr::parse_factor(.x, levels = c("f", "m")),
      age = ~readr::parse_factor(
        .x,
        levels = c("014", "1524", "2534", "3544", "4554", "5564", "65"),
        ordered = TRUE
      )
    ),
    values_to = "count"
  )


# Multiple observation per row --------------------------------------------

# Some data sets have more than one observation per row. Common examples are:
# child data of a household surveys
# cluster analysis of dental studies
# cluster analysis of physician follow-up studies
household
# Here in this household data set, each row has data of two children:
#  date of birth and name

household %>% 
  pivot_longer(
    cols = !family,
    names_to = c(".value", "child"),
    names_sep = "_",
    values_drop_na = TRUE
  )

# Long to wide data -------------------------------------------------------

fish_encounters ; ?fish_encounters
# We wanna see each fish species identified by numeric code encountered in each river station.
fish_encounters %>% 
  pivot_wider(
    names_from = station,
    values_from = seen
  )
# There are many missing value as every river station does not see all fish species, 
# so we will replace the missing value with "0" coding "not seen"

fish_encounters %>% 
  pivot_wider(
    names_from = station,
    values_from = seen,
    values_fill = 0
  )

# We can also aggregate data by using pivot_wider() like Excel function

warpbreaks ; ? warpbreaks
# This data set gives number of breaks of warp in weaving experiment, type of wool and tension

# We reorder the column first before aggregation
warpbreaks <- warpbreaks %>% 
  as_tibble() %>% 
  select(wool, tension, breaks) 
# We can see nine experiments for each wool (A,B) and tension (L,M,H)
# We can summarize  to see this as followed:
warpbreaks %>% 
  count(wool, tension)

warpbreaks %>% 
  pivot_wider(names_from = wool,
              values_from = breaks)
# But we can only get list for each tension and wool because
# wool and tension pairs are not unique
# So we will use functions to summarize the values in the list
# Here we can see that the knowledge we previously learn from base-R come into value

# Mean breaks
warpbreaks %>% 
  pivot_wider(
    names_from = wool,
    values_from = breaks,
    values_fn = mean
  )

# Total breaks
warpbreaks %>% 
  pivot_wider(
    names_from = wool,
    values_from = breaks,
    values_fn = sum
  )

# We can also use more than one column for aggregates
warpbreaks %>% 
  pivot_wider(
    names_from = c(wool, tension),
    values_from = breaks, 
    values_fn = sum
  )

# We can use combination of the functions for aggregates
library(ggplot2)
warpbreaks %>% 
  pivot_wider(
    names_from = c(wool, tension),
    values_from = breaks, 
    values_fn = sum
  ) %>% 
  pivot_longer(cols = everything(),
               names_to = "wool_tension",
               values_to = "number_of_breaks") %>% 
  ggplot(aes(x = wool_tension, y = number_of_breaks)) +
  geom_col(aes(x = reorder(wool_tension, -number_of_breaks),
               y = number_of_breaks,
               fill = number_of_breaks))+
  scale_fill_continuous()+
  theme(legend.position = "")


# Multiple values in one cell ---------------------------------------------

# We can use separate() to split the value in a column
table3
table3 %>% 
  separate(col = rate,
           into = c("case","population"),
           sep = "/",
           convert = T)

