
# Introduction to ggplot2 -------------------------------------------------

# ggplot means grammar of graphics for the plots in R.
# ggplot at least need three inputs to produce a plot:
# 1) data 2) mapping and 3) layer
# The components of ggplot from topmost layer to bottom are:
# 7. Theme
# 6. Coordinates
# 5. Facets
# 4. Scales
# 3. Layers
# 2. Mapping
# 1. Data
# When you write the codes, you will need to start from No. 1

# Set up ------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(forcats)


# 1.Data ------------------------------------------------------------------

# Data must be in tidy format
# Here, we prefer using the pipeline to putting data inside ggplot()

mpg
# mpg is the fuel economy data from 1999 to 2008 for 38 popular models of cars
# It has 11 columns with one variable in each column; 
# 234 rows with one observation in each row;
# and one value in each cell.
# Thus the data is in tidy format


# 2.Mapping ---------------------------------------------------------------

# We will use mapping inside the function ggplot() with syntax
# ggplot(mapping = aes(x = <var>, y = <var>))

mpg %>% 
  ggplot(mapping = aes(x = displ, y = hwy))

# 3. Layer ----------------------------------------------------------------

# There are three types of layers: 
# 1)geometry 
# 2)statistical transformation
# 3)position adjustment
# Then we put the geom layers using the "+" sign on the mapping

mpg %>% 
  ggplot(mapping = aes(x = displ, y = hwy))+
  geom_point()
# Please note that we can also add mappings in the geom layers

mpg %>% 
  ggplot(mapping = aes(x = displ, y = hwy))+
  geom_point(aes(size = hwy, color = class))+
  geom_smooth(aes(x = displ, y = hwy), method = "lm")

# 4. Scales ------------------------------------------------------------------
# Scales are using in pair with the aesthetic attributes
# For example, 
# scale_x_continuous; 
# scale_y_discrete
# scale_color_manual

mpg %>% 
  ggplot(mapping = aes(x = displ, y = hwy))+
  geom_point(aes(size = hwy, color = class))+
  scale_color_viridis_d()

# 5. Facets ------------------------------------------------------------------

# Facets are used to create small multiples
# They split up the data into smaller panel based on one or more variables

mpg %>% 
  ggplot(mapping = aes(x = displ, y = hwy))+
  geom_point(aes(size = hwy, color = class))+
  geom_smooth(aes(x = displ, y = hwy), method = "lm")+
  facet_grid(year ~ drv)

mpg %>% 
  ggplot(mapping = aes(x = displ, y = hwy))+
  geom_point(aes(size = hwy, color = class))+
  geom_smooth(aes(x = displ, y = hwy), method = "lm")+
  facet_wrap(vars(drv))

mpg %>% 
  ggplot(mapping = aes(x = displ, y = hwy))+
  geom_point(aes(size = hwy, color = class))+
  geom_smooth(aes(x = displ, y = hwy), method = "lm")+
  facet_wrap(year ~ drv)

# 6. Coordinates -------------------------------------------------------------

# Coordinates interpret and change the position aesthetics
# For example, they change horizontal bar graph into vertical bar graph
# They are very useful in map projections and polar plots
# For example, projecting bubble map and changing stacked bar into pie
# Here coord_fixed() is used to change the aspect ratio of X and Y axes
mpg %>% 
  ggplot(mapping = aes(cty, hwy)) +
  geom_point()

mpg %>% 
  ggplot(mapping = aes(cty, hwy)) +
  geom_point() +
  coord_fixed()

# 7. Themes ------------------------------------------------------------------

# Themes control almost any visuals of the plot that are not control by the data
# theme() function is use to change
#   fonts
#   legend position
#   background colour etc

mpg %>% 
  ggplot(mapping = aes(x = displ, y = hwy))+
  geom_point(aes(size = hwy, color = class))+
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.line = element_line(linewidth = 0.75),
    axis.line.x.bottom = element_line(colour = "blue")
  )

mpg %>% 
  ggplot(mapping = aes(displ, hwy)) +
  geom_point(mapping = aes(colour = class)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  scale_colour_viridis_d() +
  facet_grid(year ~ drv) +
  coord_fixed() +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

mpg %>% 
  ggplot(mapping = aes(displ, hwy)) +
  geom_point(mapping = aes(colour = class)) +
  geom_smooth(formula = y ~ x, method = "lm") +
  scale_colour_viridis_d() +
  facet_wrap(year ~ drv) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# Exploratory data analysis -----------------------------------------------

# One variable - continuous -----------------------------------------------

# Area graph
# To get area graph, 
# we have to transform the continuous variable into bins
# We just use stat argument to transform it
mpg %>% 
  ggplot(aes(hwy))+
  geom_area(stat = "bin", 
            fill = "skyblue")

# Density plot for probability density function
mpg %>% 
  ggplot(aes(hwy))+
  geom_density(kernel = "gaussian", 
               color = "maroon",
               size = 1)

# Dotplot
mpg %>% 
  ggplot(aes(hwy))+
  geom_dotplot(fill = "skyblue", 
               color = "skyblue")

# Frequency polygon
mpg %>% 
  ggplot(aes(hwy))+
  geom_freqpoly(color = "maroon",
                size = 1)

# Histogram
mpg %>% 
  ggplot(aes(hwy))+
  geom_histogram(fill = "skyblue",
                 bins = 30)

mpg %>% 
  ggplot(aes(hwy))+
  geom_histogram(fill = "skyblue",
                 bins = 20)

mpg %>% 
  ggplot(aes(hwy))+
  geom_histogram(fill = "skyblue",
                 binwidth = 1.5)

# QQ plot
# QQ plot need sample aesthetic for mapping
mpg %>% 
  ggplot()+
  geom_qq(aes(sample = hwy),
          color = "maroon")


# One variable - discrete -------------------------------------------------

# Vertical bar graph
mpg %>% 
  ggplot(aes(fl)) +
  geom_bar(fill = "skyblue") 

# Horizontal bar graph using coord_flip()
mpg %>% 
  ggplot(aes(fl)) +
  geom_bar(fill = "skyblue") +
  coord_flip()

# Direct labelling of the bar graph
# We need to change the variable to factor
# Then we add geom_text() layer, 
# set the label aesthetic to ..count..
# and set the statistics to count

mpg %>% 
  ggplot(aes(x = factor(fl))) +
  geom_bar(fill = "skyblue") +
  geom_text(aes(label = ..count..),
            stat = "count",
            vjust = -0.5) + 
  scale_y_continuous(limits = c(0,200)) +
  labs(
    title = "Fuel type used in the cars",
    subtitle = "Source:Fuel economy data",
    x = "Fuel type",
    y = "Number of cars"
  )+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank())

# It is easier to use the geom_col()
# But we will need two aesthetic mapping in geom_col()

mpg %>% 
  mutate(
    fl = factor(fl,
                levels = c("e","d","r","p","c"),
                labels = c("Ethanol","Diesel","Regular","Premium","CNG"))
  ) %>% 
  count(fl) %>%
  mutate(percent = round(100*prop.table(n),1)) %>% 
  ggplot()+
  geom_col(mapping = aes(x = reorder(fl, -percent), y = percent),
           fill = "royalblue")+
  geom_text(mapping = aes(x = reorder(fl, -percent), y = percent,label = percent),
            vjust = -0.3) +  
  scale_y_continuous(limits = c(0,100))+
    labs(
    title = "Type of fuel used in the cars",
    subtitle = "Source: Fuel economy data",
    x = "Fuel type",
    y = "Percentage"
  )+
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
# Here, we need to overwrite the mapping in layer__
# geom_text(mapping = aes(x = reorder(fl, -percent), y = percent,label = percent),
# vjust = -0.3)

# We often need to overwrite the mapping in changing position

# Cluster bar
mpg %>% 
  ggplot() +
  geom_bar(aes(x = fl, fill = drv),
           position = "dodge")

# stacked bar
mpg %>% 
  ggplot() +
  geom_bar(aes(x = fl, fill = drv),
           position = "stack")

# 100% stacked bar
mpg %>% 
  ggplot() +
  geom_bar(aes(x = fl, fill = drv),
           position = "fill") 
  
mpg %>% 
  ggplot() +
  geom_bar(aes(x = fl, fill = drv),
           position = "fill") +
  facet_wrap(vars(manufacturer)) +
  coord_flip()+
  theme(legend.position = "bottom")
  


# Two variables - continuous x continuous y -------------------------------
# Scatter plot
mpg %>% 
  ggplot(mapping = aes(x = cty, y = hwy)) +
  geom_point(aes(color = drv))

# Change the position of the points to avoid over-plotting and overlapping point
# We can simply use the shortcut geom_jitter()
mpg %>% 
  ggplot(mapping = aes(x = cty, y = hwy)) +
  geom_jitter(size =1)

# Fitting line
mpg %>% 
  ggplot(mapping = aes(x = cty, y = hwy)) +
  geom_smooth(method = NULL)

mpg %>% 
  ggplot(mapping = aes(x = cty, y = hwy)) +
  geom_smooth(method = "lm")

mpg %>% 
  ggplot(mapping = aes(x = cty, y = hwy)) +
  geom_smooth(method = "loess")

mpg %>% 
  ggplot(mapping = aes(x = cty, y = hwy)) +
  geom_smooth(method = "glm")


# Two variables - discrete x continuous y ---------------------------------

# Column chart

mpg %>% 
  ggplot()+
  geom_col(aes(x = class, y = hwy))
# Since geom_col uses the position stack, 
# the value of hwy are stacked over each other
# which is not very informative

# Thus, change the position
mpg %>% 
  ggplot()+
  geom_col(aes(x = fct_reorder(class, hwy), y = hwy), position = "dodge")
# But it gives only maximum value
mpg %>% 
  ggplot()+
  geom_col(aes(x = fct_reorder(class, hwy, .fun = max, .desc=T), y = hwy), position = "dodge")

# Thus we'd better summarize the data before we use geom_col()
mpg %>% 
  group_by(class) %>% 
  summarise(mean=round(mean(hwy),1)) %>% 
  ggplot(aes(x = mean, y = fct_reorder(class,mean))) +
  geom_col(fill = "royalblue")+
  geom_text(aes(label = mean), nudge_x = -2)+
  labs(title = "Average highway mileage by class",
       subtitle = "Source: fuel economy data",
       x = "Average miles per gallon",
       y = "Class of cars") +
  theme_minimal()


# Dot-plots are alternative but graph literacy is required
mpg %>% 
  ggplot(aes(x = hwy, y = class)) +
  geom_dotplot(binaxis = "y", stackdir = "center")

mpg %>% 
  ggplot(aes(x = class,y = hwy)) +
  geom_dotplot(binaxis = "y", stackdir = "center")

# Box and whisker plot 
mpg %>% 
  ggplot(aes(drv,hwy)) +
  geom_boxplot(aes(fill = drv)) 

# Violin plot
mpg %>% 
  ggplot(aes(drv,hwy)) +
  geom_violin(aes(fill = drv)) 


# Two variables - discrete x discrete y -----------------------------------

# dot plot discrete
diamonds %>% 
  ggplot(aes(cut, color)) +
  geom_count(color = "skyblue")


#  Continuous function ----------------------------------------------------

# Area graph
economics %>% 
  ggplot(aes(date,unemploy)) +
  geom_area(fill = "maroon")
  
# Line graph
economics %>% 
  ggplot(aes(date,unemploy)) +
  geom_line(color = "red")



# Errors and spread ------------------------------------------------------------------

# To use error viz, we need the fitting (centre) and standard error (spread) columns

df <- tibble(grp = c("A","B"), fit = 4:5, se=1:2)

# Cross bar
df %>% 
  ggplot(aes(x = grp, y = fit, ymin = fit-se, ymax = fit+se)) +
  geom_crossbar()

# Error bar
df %>% 
  ggplot(aes(x = grp, y = fit, ymin = fit-se, ymax = fit+se)) +
  geom_errorbar()

# Line range
df %>% 
  ggplot(aes(x = grp, y = fit, ymin = fit-se, ymax = fit+se)) +
  geom_linerange()

# Point range
df %>% 
  ggplot(aes(x = grp, y = fit, ymin = fit-se, ymax = fit+se)) +
  geom_pointrange()


# Simple map projection ----------------------------------------------------------

# To project map, we can use:
# geom_map and geom_sf

# geom_map()
# geom_map() uses data frames with longitude and lattitude data
# We need 
# data frame with measured variable and 
# data frame with area variable
# For example here, we will use USArrest data where rownames are states
mdf <- data.frame(murder = USArrests$Murder,
                 state = tolower(rownames(USArrests)))
map <- map_data("state")
# If you don't have maps package installed, R will ask to install it.

mdf %>% 
  ggplot(aes(fill = murder)) +
  geom_map(
    aes(map_id = state),    #define the key id to connect
    map = map               #connect with the map
  ) +
  expand_limits(x = map$long, y = map$lat)  #Set longitude to X axis and latitude to Y axis

mdf %>% 
  ggplot(aes(fill = murder)) +
  geom_map(
    aes(map_id = state),    
    map = map               
  ) +
  expand_limits(x = map$long, y = map$lat) +
  scale_fill_gradient(low = "green",high = "red")

# geom_sf()
# geom_sf() needs simple feature file types like .json
# And we need to join the the layer data and our measured data

sr <- sf::st_read(dsn = "C:/docs/R_teaching/data/state_region.json")  
nrow(sr)
set.seed(123)
sr$temp <- as.double(sample(20:45, nrow(sr)))
ggplot(data = sr) +
  geom_sf(aes(fill = temp))+
  scale_fill_gradient(low = "yellow", high = "red")+theme_void()

sr <- sf::st_read(dsn = "C:/docs/R_teaching/data/state_region.json")  
srtemp <- read.csv("C:/docs/R_teaching/data/st_rg_temp.csv")

srtemp <- srtemp %>% select(!1)

sr %>% 
  left_join(srtemp) %>% 
  select(ST, temp) %>% 
  ggplot() +
  geom_sf(aes(fill = temp))+
  scale_fill_gradient(low = "yellow", high = "red")+
  labs(title = "Temperature around Myanmar in summer",
       subtitle = "Imaginary data") +
  theme_void()
