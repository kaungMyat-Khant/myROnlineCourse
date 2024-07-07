Sex <- c("Male","Female","Female","Male","Male") ; str(Sex)
# c() is concatenate function
Age <- c(40,45,43,60,35) ; str(Age)
# str() is function to show the type or class of vector
Hypertension <- c(TRUE,TRUE,TRUE,TRUE,FALSE) ; str(Hypertension)
data <- data.frame(cbind(Age, Sex, Hypertension)) ; str(data)
# data.frame() is a function to create data frame
# cbind is a function to bind the column
View(data)
data <- data.frame(Age, Sex, Hypertension) ; str(data)
##### Type of object  End #####

library(tidyverse)
mpg
str(mpg)
# to get a scatter plot with engine size(displ) and highway mileage(hwy)
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ,y = hwy))
#Function ggplot
## ggplot(data = <Data>)+
      #<GEOM FUNCTION>(mapping = aes(<MAPPING>))
## data and mapping typed before equal sign inside bracket are called arguments
## data argument tells what dataset we want to use
## mapping argument tells which data we will use to create the graph
## aes() is a function which tell the appearance we want
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ,y = hwy))+
  geom_smooth(mapping = aes(x = displ,y = hwy),method = "lm", se=F)
# geom_point() function gets scatter plot
# geom_smooth() function gets fitted line
# method argument tells which method of fitting we will use, here lm is linear model
# se argument tells if we would include standard error in the plot.

## Assignment 1 ##
#1 Run ggplot(data = mpg). Waht do you  see?
#2 How many rows are in mtcars? How many columns?
#3 What does the drv variable describe? Read the help for ?mpg to find out.
#4 Make a scatterplot of hwy vs cyl
#5 What happens if you make a scatterplot of class vs drv? 
####Is the plot useful? Why?
ggplot(data = mpg)
mtcars
str(mtcars)
?mpg
ggplot(data = mpg)+
  geom_point(mapping = aes(x = cyl, y= hwy))
ggplot(data = mpg)+
  geom_point(mapping = aes(x = class, y = drv))
