

# Setup -------------------------------------------------------------------
# install.packages("labelled")
rm(list = ls())
library(labelled)
library(dplyr)

# Variable labels ---------------------------------------------------------

# Let's use the iris data to learn to set variable label
data <- as_tibble(iris)
# Get the variable label with var_label()
# Syntax is var_label(x) where x is a vector
var_label(data$Sepal.Length)
# Since the variable is not labelled, it gives NULL value.

# We can simply use the same function to set the variable label.
# Syntax is var_label(x) <- "variable label"
var_label(data$Sepal.Length) <- "Length of Sepal"

# Now we can see the variable is labelled with the same function.
var_label(data$Sepal.Length)

# We can also remove the label by assigning NULL value
var_label(data$Sepal.Length) <- NULL

# Now the label is gone
var_label(data$Sepal.Length)

# We can use list() function to label multiple variable with one code
# Syntax is var_label(<data.frame>) <- list(var1 = "lab1", var2 = "lab2")
var_label(data) <- list(
  Petal.Length = "Length of Petal",
  Petal.Width = "Width of Petal"
)
# We can use look_for() function to check the labels
look_for(data)
var_label(data) <- NULL


# We can also use dplyr pipeline together with set_variable_labels
data %>% 
  set_variable_labels(
    Sepal.Length = "Length of the Sepal",
    Sepal.Width = "Width of the Sepal",
    Petal.Length = "Length of the Petal",
    Petal.Width = "Width of the Petal",
    Species = "Species of flower"
  )

# Note that we need to save the data by assignment when we use pipeline
data <- data %>% 
  set_variable_labels(
    Sepal.Length = "Length of the Sepal",
    Sepal.Width = "Width of the Sepal",
    Petal.Length = "Length of the Petal",
    Petal.Width = "Width of the Petal",
    Species = "Species of flower"
  )
look_for(data)

# Value labels ------------------------------------------------------------
# Let us create another variable to our iris data.
# It will be scientific nomenclature of the flowers.

str(data$Species)
levels(data$Species)

data <- data %>% mutate(
  full_name = case_when(
    Species == "setosa" ~ 1,
    Species == "versicolor" ~ 2,
    Species == "virginica" ~ 3,
    .ptype = as.integer(Species)
  )
) %>% set_variable_labels(full_name = "Scientific nomenclature")

#  Let us see the value labels of our newly created variable

# Get the value label 
val_label(data$full_name,1)
val_label(data$full_name,2)
val_label(data$full_name,3)
look_for(data)

# Set the value labels
# We can use two functions val_label() and val_labels()

# val_label() set only one value label
# Syntax is val_label(<x>,<value>) <- <"label">
val_label(data$full_name,1) <- "Iris setosa"

# val_labels() set multiple value labels
# Syntax is val_labels(<x>) <- c(<"label"> = <value>)
# Note syntax is reverse of variable label.
# Label  on LHS, value on RHS in value label.
# Value on LHS, label on RHS in variable label.

val_labels(data$full_name) <- c("Iris setosa" = 1, "Iris versicolor" = 2, "Iris virginica" = 3)
look_for(data)

# Similarly we can remove the value label by assigning NULL value
val_labels(data$full_name) <- NULL
look_for(data)

# We can use pipeline to set value labels
data <- data %>% 
  set_value_labels(
    full_name = c("Iris setosa" = 1, 
                  "Iris versicolor" = 2, 
                  "Iris virginica" = 3)
  )
look_for(data)

# We can also add or remove value labels.
# Add another value label
data <- data %>% 
  add_value_labels(
    full_name = c("Unknown" = 9)
  )
look_for(data)

# Remove value label
data <- data %>% 
  remove_value_labels(
    full_name = 9
  )
look_for(data)

# Remove all value labels
data %>% 
  set_value_labels(
    full_name = NULL
  ) %>% look_for()

# Handling NA values ------------------------------------------------------------

# There are generally three types pf missing values according to file type
# 1. Simple missing values in R
# 2. User defined missing values in SPSS
# 3. Tagged missing values in Stata and SAS


# User defined missing values from SPSS -----------------------------------

# Let's create a labelled numeric vector with some user defined missing values

library(labelled)
v <- labelled(
  c(1,2,2,2,3,8,9,1,3,2,NA),
  c( yes = 1, no = 2, "don't know" = 9, "refuse to answer" = 8)
)

v

sort_val_labels(v)
v <- sort_val_labels(v)

is.na(v)  #only NA is the missing value

# Here we want to set 8 and 9 as missing values
na_values(v) #We haven't set the NA yet, so it gives us NULL

na_values(v) <- c(8,9)
# Now we set values, 8 and 9 as missing values/ NA. They are called user define missing values

is.na(v) 
# Now 8 and 9 become missing value
na_values(v)

#  Now you can change them as NA if you need to
v <- user_na_to_na(v) ; v

# User defined missing values can be removed by assigning NULL
v <- labelled(
  c(1,2,2,2,3,8,9,1,3,2,NA),
  c( yes = 1, no = 2, "don't know" = 9, "refuse to answer" = 8)
)
na_values(v) <- c(8,9)
is.na(v)
na_values(v) <- NULL
is.na(v)

# Tagged NAs from Stata and SAS -------------------------------------------

# Tagged missing values are used in Stata and SAS
# Tagged missing values are usually a letter ("a" : "z")
# Please note tagged NA can be only used in variable with double data type

# Let change the values 8 and 9 from created vector as tagged NA
v <- labelled(
  c(1,2,2,2,3,8,9,1,3,2,NA),
  c( yes = 1, no = 2, "don't know" = 9, "refuse to answer" = 8)
)
v
v <- replace(v, v == 8, tagged_na("z"))
v <- replace(v, v == 9, tagged_na("a"))
v

is.na(v)

# Let's create a vector with tagged NAs
x <- c(1:5, tagged_na("a"), tagged_na("z"), NA)
x #tagged NAs appeared as NAs
is.na(x) 
# Tagged NAs work identically to regular NAs
# Here we see three NAs but don't know which is tagged
# Thus is_tagged_na() is used
is_tagged_na(x) # Only tagged NAs return TRUE

na_tag(x) # See the tags from tagged NA
na_values(x)

# We can use val_labels to label the tagged NAs

val_labels(x) <- c(
  "Straight" = 1,
  "Lesbian" = 2,
  "Gay" = 3,
  "Bisexual" = 4,
  "Transgender" = 5,
  "Don't know" = tagged_na("a"), 
  "Refuse to answer" = tagged_na("z")
)
x


# Conversion --------------------------------------------------------------

# We can use to_factor() to convert labelled vectors to factor

data
data %>% to_factor() %>% print(n = Inf)
data <- data %>% to_factor() 

glimpse(data)
str(data)

v <- sort_val_labels(v)
v
to_factor(v)
