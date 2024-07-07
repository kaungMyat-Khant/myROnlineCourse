##Aesthetic Mapping
###The greatest value of a picture is when it forces us to notice what we never expected to see  _ John Tukey
library(tidyverse)
mpg
ggplot(mpg)+
  geom_point(aes(displ,hwy, color= hwy>20 & displ > 5))
#To know the car model with larger engine displacement and higher mileage
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, size = class))  
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
#aes() function controls the appearance of graph depending on variable
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy), shape = 1)
ggplot(data = mpg)+
  geom_point(mapping = aes(x = displ, y = hwy), size = 2.5)
#To change geom aesthetic manually,aesthetic argument were typed outside the aes() function

# Assignment2
#1. ggplot(data = mpg)+geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))
###What is wrong with the code?Why are the points not blue?
#2. Which variables in mpg are categorical?
###Which variables are continuous? How can you see this information when you run mpg?
#3. Map a continuous variable to color, size and shape. 
####How do these aesthetics behave differenttly for categorical vs continuous variable.
#4. What happened if you map the same variable to multiple aesthetics?
#5. What does the stroke aesthetic do? What shapes does it work with?
#6. What happens if you map an aesthetic to something other than variable name,
####like aes(color = displ < 5)